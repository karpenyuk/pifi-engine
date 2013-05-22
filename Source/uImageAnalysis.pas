unit uImageAnalysis;

interface

uses
  Classes,
  uBaseTypes,
  uVMath,
  uImageAnalysisClasses;

{ .$IFDEF PACKED_EXEMPLAR_RG }

type

  TAnalyzerThread = class;

  TAnalyzer = class
  protected
    FAccessFunc: TEdgePolicyFunc;
    FAnalysisData: TAnalysisData;
    FThreads: array of TAnalyzerThread;
    FMaxCPUThreads: integer;
    procedure SetMaxCPUThreads(const Value: integer);
    // Gathers all neighborhoods of the exemplar stack level
    procedure GatherNeighborhoods(alevel: integer);
    procedure ProjectStackLevelTo2D(alevel: PAnalyzedLevel);
    // Using principal component analysis (PCA)
    // projecting pixel neighborhoods into a lower-dimensional space (6D vector)
    procedure ProjectNeighbTo6D(alevel: integer;
      out aNeighbPCA: TNeighbPCAmatrix);
    procedure SetToroidality(flag: boolean);
    function GetDone: boolean;
    function GetLevelCount: integer;
    function GetToroidality: boolean;
  public
    constructor Create(anAnalysisData: TAnalysisData);
    destructor Destroy; override;

    // Runs analysis
    procedure Start;
    procedure Process;
    procedure Stop;
    property Done: boolean read GetDone;

    // Analyzes one exemplar stack level
    procedure AnalyzeStackLevel(alevel: integer);
    // Analisys data storage
    property AnalysisData: TAnalysisData read FAnalysisData;
    // True if the exemplar is toroidaly repeated
    property Toroidality: boolean read GetToroidality write SetToroidality;
    // Maximum number of CPU's thread used during analisys
    property MaxCPUThreads: integer read FMaxCPUThreads write SetMaxCPUThreads;
  end;

  TAnalyzerThread = class(TThread)
  private
    FAnalyzer: TAnalyzer;
    FData: PAnalyzedLevel;
  protected
    constructor CreateOwned(aOwner: TAnalyzer; alevel: PAnalyzedLevel);
    procedure Execute; override;
  end;

implementation

uses
  uMath,
  uMathPCA,
  Math,
  uNeighborSearching,
  uMiscUtils;

{$REGION 'TAnalyzer'}

procedure TAnalyzer.AnalyzeStackLevel(alevel: integer);
var
  w, h, i, j, k, n: integer;
  ANNPoints: TANNpointArray;
  queryPt: TANNpoint; // query point
  Idx: TANNidx;
  nnIdx: TANNidxArray; // near neighbor indices
  dists: TANNdistArray; // near neighbor distances
  kdTree: TANNkd_tree; // search structure
  neigh: TNeighborhood3c;
  KNearest: TMostSimilar;
begin
  w := FAnalysisData.Neighborhoods.Width;
  h := FAnalysisData.Neighborhoods.Height;
  // build ANN structure
  SetLength(nnIdx, SIMILAR_NEIGHBOUR_SIZE);
  SetLength(dists, SIMILAR_NEIGHBOUR_SIZE);
  SetLength(ANNPoints, w * h);
  // fill-in points
  k := 0;
  for i := 0 to h - 1 do
    for j := 0 to w - 1 do
    begin
      neigh := FAnalysisData.Neighborhoods.At[j, i, alevel];
      SetLength(ANNPoints[k], NEIGHBOUR_SIZE_3COLOR);
      for n := 0 to NEIGHBOUR_SIZE_3COLOR - 1 do
        ANNPoints[k][n] := neigh[n]; // singe -> double
      Inc(k);
    end;
  // build search structure
  kdTree := TANNkd_tree.CreateFromPointArray(ANNPoints, NEIGHBOUR_SIZE_3COLOR);

  for i := 0 to h - 1 do
    for j := 0 to w - 1 do
    begin
      neigh := FAnalysisData.Neighborhoods.At[j, i, alevel];
      SetLength(queryPt, NEIGHBOUR_SIZE_3COLOR);
      for n := 0 to NEIGHBOUR_SIZE_3COLOR - 1 do
        queryPt[n] := neigh[n];
      // search for similar neighborhood in reference
      kdTree.annkSearch(queryPt, SIMILAR_NEIGHBOUR_SIZE, nnIdx, dists, 0.001);

      // store result
      for n := 0 to SIMILAR_NEIGHBOUR_SIZE - 1 do
      begin
        Idx := nnIdx[n];
        KNearest[n][0] := Idx mod w;
        KNearest[n][1] := Idx div w;
      end;
      FAnalysisData.kNearests.At[j, i, alevel] := KNearest;
    end;

  kdTree.Free;
end;

constructor TAnalyzer.Create(anAnalysisData: TAnalysisData);
begin
  Assert(Assigned(anAnalysisData));
  FAnalysisData := anAnalysisData;
  FAccessFunc := TIVec2Array2D.WrapAccess;
  FMaxCPUThreads := 2;
end;

destructor TAnalyzer.Destroy;
begin
  Stop;
  inherited;
end;

procedure TAnalyzer.GatherNeighborhoods(alevel: integer);
var
  i, j: integer;
begin
  for i := 0 to FAnalysisData.Exemplar.Height - 1 do
    for j := 0 to FAnalysisData.Exemplar.Width - 1 do
      FAnalysisData.Neighborhoods.At[j, i, alevel] :=
        FAnalysisData.GatherNeighborhood(j, i, alevel);
end;

function TAnalyzer.GetDone: boolean;
var
  l: integer;
begin
  result := true;
  for l := 0 to High(FThreads) do
    result := result and FThreads[l].Finished;
end;

function TAnalyzer.GetLevelCount: integer;
begin
  if Assigned(FAnalysisData) then
    result := FAnalysisData.LevelsAmount
  else
    result := 0;
end;

function TAnalyzer.GetToroidality: boolean;
begin
  result := FAnalysisData.Toroidality;
end;

procedure TAnalyzer.Process;
var
  i, w: integer;
begin
  w := 0;
  for i := 0 to High(FThreads) do
    if not(FThreads[i].Suspended or FThreads[i].Finished) then
      Inc(w);

  if w < FMaxCPUThreads then
    for i := 0 to High(FThreads) do
    begin
      if FThreads[i].Suspended then
      begin
        FThreads[i].Start;
        Inc(w);
      end;
      if w >= FMaxCPUThreads then
        Exit;
    end;
end;

procedure TAnalyzer.ProjectNeighbTo6D(alevel: integer;
  out aNeighbPCA: TNeighbPCAmatrix);
const
  MAX6D: TVector6f = (1E30, 1E30, 1E30, 1E30, 1E30, 1E30);
  MIN6D: TVector6f = (-1E30, -1E30, -1E30, -1E30, -1E30, -1E30);
var
  x: TDouble2DArray;
  Dispersion: TDouble1DArray;
  PCA: TDouble2DArray;
  Info: integer;
  h, w, i, j, k, mi, mj, row, esize: integer;
  V6D, minValue, maxValue: TVector6f;
  Neighb: TNeighborhood3c;
  p1, p2: PByte;
begin
  w := FAnalysisData.Neighborhoods.Width;
  h := FAnalysisData.Neighborhoods.Height;

  // fill array for principal component analysis
  SetLength(x, w * h, NEIGHBOUR_SIZE_3COLOR);
  row := 0;
  for i := 0 to h - 1 do
    for j := 0 to w - 1 do
    begin
      Neighb := FAnalysisData.Neighborhoods[j, i, alevel];
      for mj := 0 to NEIGHBOUR_SIZE_3COLOR - 1 do
        x[row][mj] := Neighb[mj];
      Inc(row);
    end;

  // run principal component analysis
  PrincipalComponentsAnalysis.BuildBasis(x, w * h, NEIGHBOUR_SIZE_3COLOR,
    Info, Dispersion, PCA);

  // store only 6 basis
  if Info = 1 then
    for i := 0 to 5 do
      for j := 0 to NEIGHBOUR_SIZE_3COLOR - 1 do
        aNeighbPCA[j, i] := PCA[j, i];

  minValue := MAX6D;
  maxValue := MIN6D;
  // set lower dimension vector
  for i := 0 to h - 1 do
    for j := 0 to w - 1 do
    begin
      Neighb := FAnalysisData.Neighborhoods[j, i, alevel];
      V6D := ZERO_VECTOR6D;
      for mj := 0 to NEIGHBOUR_SIZE_3COLOR - 1 do
        for mi := 0 to 5 do
          V6D[mi] := V6D[mi] + aNeighbPCA[mj, mi] * Neighb[mj];
      FAnalysisData.Neighborhoods.As6DAt[j, i, alevel] := V6D;

      for k := 0 to 5 do
      begin
        if V6D[k] > maxValue[k] then
          maxValue[k] := V6D[k];
        if V6D[k] < minValue[k] then
          minValue[k] := V6D[k];
      end;
    end;

  // scale neighbors to byte size
  FAnalysisData.Levels[alevel].NeighbOffset := minValue;
  for k := 0 to 5 do
  begin
    maxValue[k] := maxValue[k] - minValue[k];
    if maxValue[k] = 0 then
      maxValue[k] := 1;
  end;
  FAnalysisData.Levels[alevel].NeighbScale := maxValue;

  with FAnalysisData.Levels[alevel]^ do
  begin
    // part 1
    FillChar(Neighborhoods[0], SizeOf(TImageDesc), $00);
    Neighborhoods[0].Width := w;
    Neighborhoods[0].Height := h;
    Neighborhoods[0].InternalFormat := GL_RGBA8;
    Neighborhoods[0].ColorFormat := GL_RGBA;
    Neighborhoods[0].ElementSize := 4;
    Neighborhoods[0].DataType := GL_UNSIGNED_BYTE;
    esize := Neighborhoods[0].ElementSize;
    Neighborhoods[0].DataSize := w * h * esize;
    GetMem(Neighborhoods[0].Data, Neighborhoods[0].DataSize);
    // part 2
    FillChar(Neighborhoods[1], SizeOf(TImageDesc), $00);
    Neighborhoods[1].Width := w;
    Neighborhoods[1].Height := h;
    Neighborhoods[1].InternalFormat := GL_RGBA8;
    Neighborhoods[1].ColorFormat := GL_RGBA;
    Neighborhoods[1].ElementSize := 4;
    Neighborhoods[1].DataType := GL_UNSIGNED_BYTE;
    esize := Neighborhoods[1].ElementSize;
    Neighborhoods[1].DataSize := w * h * esize;
    GetMem(Neighborhoods[1].Data, Neighborhoods[1].DataSize);
  end;

  for i := 0 to h - 1 do
  begin
    p1 := FAnalysisData.Levels[alevel].Neighborhoods[0].Data;
    p2 := FAnalysisData.Levels[alevel].Neighborhoods[1].Data;
    Inc(p1, i * w * 4);
    Inc(p2, i * w * 4);
    for j := 0 to w - 1 do
    begin
      V6D := FAnalysisData.Neighborhoods.As6DAt[j, i, alevel];
      for k := 0 to 5 do
        V6D[k] := (V6D[k] - minValue[k]) / maxValue[k];
      p1[0] := floor(255 * V6D[0]);
      p1[1] := floor(255 * V6D[1]);
      p1[2] := floor(255 * V6D[2]);
      p1[2] := $FF;
      p2[0] := floor(255 * V6D[3]);
      p2[1] := floor(255 * V6D[4]);
      p2[2] := floor(255 * V6D[5]);
      p2[2] := $FF;
    end;
  end;
end;

procedure TAnalyzer.ProjectStackLevelTo2D(alevel: PAnalyzedLevel);
var
  x: TDouble2DArray;
  p: PByte;
  Dispersion: TDouble1DArray;
  PCA: TDouble2DArray;
  Info: integer;
  i, j, row, w, h, esize: integer;
  clr: TVector;
  C2, iterC2: PVector;
  minValue, maxValue: TVector;
begin
  w := FAnalysisData.Exemplar.Width;
  h := FAnalysisData.Exemplar.Height;
  esize := FAnalysisData.Exemplar.ElementSize;
  // Fill array for principal components analysis
  SetLength(x, w * h, 3);
  row := 0;
  for i := 0 to h - 1 do
  begin
    p := FAnalysisData.Exemplar.Data;
    Inc(p, i * w * esize);
    for j := 0 to w - 1 do
    begin
      x[row][0] := INV255 * p[0];
      x[row][1] := INV255 * p[1];
      x[row][2] := INV255 * p[2];
      Inc(row);
      Inc(p, esize);
    end;
  end;

  // Run analysis
  PrincipalComponentsAnalysis.BuildBasis(x, w * h, 3, Info, Dispersion, PCA);

  // store only 2 basis
  if Info = 1 then
    for i := 0 to 1 do
      for j := 0 to 2 do
        alevel.ColorPCAMatrix[j, i] := PCA[j, i];

  GetMem(C2, w * h * SizeOf(TVector));
  minValue := TVector.Make(1E30, 1E30);
  maxValue := minValue.Negate;
  clr := TVector.Null;

  try
    // project 3D color to 2D
    iterC2 := C2;
    for i := 0 to h - 1 do
    begin
      p := FAnalysisData.Exemplar.Data;
      Inc(p, i * w * esize);
      for j := 0 to w - 1 do
      begin
        clr.x := alevel.ColorPCAMatrix[0, 0] * p[0] + alevel.ColorPCAMatrix
          [1, 0] * p[1] + alevel.ColorPCAMatrix[2, 0] * p[2];

        clr.y := alevel.ColorPCAMatrix[0, 1] * p[0] + alevel.ColorPCAMatrix
          [1, 1] * p[1] + alevel.ColorPCAMatrix[2, 1] * p[2];

        maxValue := TVector.MaxVector(maxValue, clr);
        minValue := TVector.MinVector(minValue, clr);

        iterC2^ := clr;
        Inc(iterC2);
        Inc(p, esize);
      end;
    end;

    alevel.ColorOffset := minValue;
    clr := maxValue - minValue;
    if clr.x = 0 then
      clr.x := 1;
    if clr.y = 0 then
      clr.y := 1;
    alevel.ColorScale := clr;

    with alevel^ do
    begin
      FillChar(ProjectedImage, SizeOf(TImageDesc), $00);
      ProjectedImage.Width := w;
      ProjectedImage.Height := h;
{$IFDEF PACKED_EXEMPLAR_RG}
      ProjectedImage.InternalFormat := GL_RG8;
      ProjectedImage.ColorFormat := GL_RG;
      ProjectedImage.ElementSize := 2;
{$ELSE}
      ProjectedImage.InternalFormat := GL_RGB8;
      ProjectedImage.ColorFormat := GL_RGB;
      ProjectedImage.ElementSize := 3;
{$ENDIF}
      ProjectedImage.DataType := GL_UNSIGNED_BYTE;
      esize := ProjectedImage.ElementSize;
      ProjectedImage.DataSize := w * h * esize;
      GetMem(ProjectedImage.Data, ProjectedImage.DataSize);
    end;

    // scale color to byte size
    iterC2 := C2;
    for i := 0 to h - 1 do
    begin
      p := alevel.ProjectedImage.Data;
      Inc(p, i * w * esize);
      for j := 0 to w - 1 do
      begin
        clr := iterC2^;
        p[0] := Floor(255 * (clr.x - minValue.x) / alevel.ColorScale.x);
        p[1] := Floor(255 * (clr.y - minValue.y) / alevel.ColorScale.y);
        Inc(p, esize);
        Inc(iterC2);
      end;
    end;
  finally
    FreeMem(C2);
  end;
end;

procedure TAnalyzer.SetMaxCPUThreads(const Value: integer);
begin
  Assert(Value > 0);
  FMaxCPUThreads := Value;
end;

procedure TAnalyzer.SetToroidality(flag: boolean);
begin
  if flag <> FAnalysisData.Toroidality then
  begin
    FAnalysisData.Toroidality := flag;
    if flag then
      FAccessFunc := TIVec2Array2D.WrapAccess
    else
      FAccessFunc := TIVec2Array2D.MirrorAccess;
  end;
end;

procedure TAnalyzer.Start;
var
  pyramid: TImagePyramid;
  l: integer;
begin
  Assert(Assigned(FAnalysisData.Exemplar.Data), 'TAnalyzer: Assign ');
  // delete previous result
  Stop;
  FAnalysisData.Clear;
  // first create an image pyramid
  pyramid := TImagePyramid.Create(FAnalysisData.Exemplar.Width,
    FAnalysisData.Exemplar.Height);
  pyramid.AssignFromImage(FAnalysisData.Exemplar^, 0);
  pyramid.GeneratePyramid;
  // then compute the exemplar stack from the pyramid
  FAnalysisData.CreateImageStack(pyramid);

  // Allocate neighborhoods
  SetLength(FThreads, FAnalysisData.LevelsAmount);

  for l := 0 to FAnalysisData.LevelsAmount - 1 do
    FThreads[l] := TAnalyzerThread.CreateOwned(Self, FAnalysisData.Levels[l]);
  pyramid.Free;
end;

procedure TAnalyzer.Stop;
var
  i: integer;
  wait: boolean;
begin
  // wait until threads finished
  repeat
    wait := true;
    for i := 0 to High(FThreads) do
      wait := wait and FThreads[i].Finished;
  until wait;

  for i := 0 to High(FThreads) do
    FThreads[i].Destroy;
  SetLength(FThreads, 0);
end;

{$ENDREGION}
{$REGION 'TAnalyzerThread'}

constructor TAnalyzerThread.CreateOwned(aOwner: TAnalyzer;
  alevel: PAnalyzedLevel);
begin
  inherited Create(true);
  FAnalyzer := aOwner;
  FData := alevel;
end;

procedure TAnalyzerThread.Execute;
begin
  // Project 3D color exemplar stack level to 2D color
  FAnalyzer.ProjectStackLevelTo2D(FData);
  // Gather neighborhoods and store them for use during analysis and synthesis
  FAnalyzer.GatherNeighborhoods(FData.LevelId);
  // Compute lower-dimension neighborhoods
  FAnalyzer.ProjectNeighbTo6D(FData.LevelId, FData.NeihgbPCAMatrix);
  // Analyze stack level - search most similar neighborhoods
  FAnalyzer.AnalyzeStackLevel(FData.LevelId);
end;
{$ENDREGION}

end.
