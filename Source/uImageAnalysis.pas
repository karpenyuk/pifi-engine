unit uImageAnalysis;

interface

uses
  Classes,
  uBaseTypes,
  uVMath,
  uImageAnalysisClasses;

{$POINTERMATH ON}
{.$IFDEF PACKED_EXEMPLAR_RG }

type

  TAnalyzerThread = class;

  TAnalyzer = class
  protected
    FAnalysisData: TAnalysisData;
    FThreads: array of TAnalyzerThread;
    FMaxCPUThreads: integer;
    // Gathers all neighborhoods of the exemplar stack level
    procedure GatherNeighborhoods(alevel: integer);
    procedure ProjectStackLevelTo2D(aLevel: integer);
    // Using principal component analysis (PCA)
    // projecting pixel neighborhoods into a lower-dimensional space (6D vector)
    procedure ProjectNeighbTo6D(alevel: integer);
    procedure PrepareKNearest(alevel: integer);
    function GetDone: boolean;
    function GetLevelCount: integer;
    function GetProgress: Single;
  public
    constructor Create(anAnalysisData: TAnalysisData);
    destructor Destroy; override;

    // Runs analysis
    procedure Start;
    procedure Process;
    function Stop: Boolean;
    property Progress: Single read GetProgress;

    // Analyzes one exemplar stack level
    procedure AnalyzeStackLevel(alevel: integer);
    // Analisys data storage
    property AnalysisData: TAnalysisData read FAnalysisData;
    // Maximum number of CPU's thread used during analisys
    property MaxCPUThreads: integer read FMaxCPUThreads write FMaxCPUThreads;
  end;

  TAnalyzerThread = class(TThread)
  private
    FAnalyzer: TAnalyzer;
    FData: PAnalyzedLevel;
    FTatalCycle: integer;
    function GetProgress: Single;
  protected
    constructor CreateOwned(aOwner: TAnalyzer; alevel: PAnalyzedLevel);
    procedure Execute; override;
    property Progress: Single read GetProgress;
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
  Level: PAnalyzedLevel;
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
  Level := FAnalysisData.Levels[aLevel];
  w := FAnalysisData.Neighborhoods.Width;
  h := FAnalysisData.Neighborhoods.Height;
  // build ANN structure
  SetLength(nnIdx, SIMILAR_NEIGHBOUR_SIZE);
  SetLength(dists, SIMILAR_NEIGHBOUR_SIZE);
  SetLength(ANNPoints, w * h);
  // fill-in points
  k := 0;
  if SizeOf(TANNpoint) = SizeOf(Single) then
  begin
    for i := 0 to h - 1 do
    begin
      for j := 0 to w - 1 do
      begin
        neigh := FAnalysisData.Neighborhoods.At[j, i, alevel];
        SetLength(ANNPoints[k], NEIGHBOUR_SIZE_3COLOR);
        Move(neigh[0], ANNPoints[k][0], SizeOf(TNeighborhood3c));
        Inc(k);
      end;
      Inc(Level.CycleCounter, w);
    end;
  end
  else  if SizeOf(TANNpoint) = SizeOf(Double) then
    for i := 0 to h - 1 do
    begin
      for j := 0 to w - 1 do
      begin
        neigh := FAnalysisData.Neighborhoods.At[j, i, alevel];
        SetLength(ANNPoints[k], NEIGHBOUR_SIZE_3COLOR);
        for n := 0 to NEIGHBOUR_SIZE_3COLOR - 1 do
          ANNPoints[k][n] := neigh[n]; // singe -> double
        Inc(k);
      end;
      Inc(Level.CycleCounter, w);
    end;
  // build search structure
  kdTree := TANNkd_tree.CreateFromPointArray(ANNPoints, NEIGHBOUR_SIZE_3COLOR);
  SetLength(queryPt, NEIGHBOUR_SIZE_3COLOR);
  for i := 0 to h - 1 do
  begin
    for j := 0 to w - 1 do
    begin
      neigh := FAnalysisData.Neighborhoods.At[j, i, alevel];
      if SizeOf(TANNpoint) = SizeOf(Single) then
          Move(neigh[0], queryPt[0], SizeOf(TNeighborhood3c))
      else if SizeOf(TANNpoint) = SizeOf(Double) then
        for n := 0 to NEIGHBOUR_SIZE_3COLOR - 1 do
          queryPt[n] := neigh[n];
      // search for similar neighborhood in reference
      kdTree.annkSearch(queryPt, SIMILAR_NEIGHBOUR_SIZE, nnIdx, dists, 1);

      // store result
      for n := 0 to SIMILAR_NEIGHBOUR_SIZE - 1 do
      begin
        Idx := nnIdx[n];
        KNearest[n][0] := Idx mod w;
        KNearest[n][1] := Idx div w;
      end;
      FAnalysisData.kNearests.At[j, i, alevel] := KNearest;
    end;
    Inc(Level.CycleCounter, w);
  end;
  kdTree.Free;
end;

{
// used ANN as DLL
procedure TAnalyzer.AnalyzeStackLevel(alevel: integer);
var
  Level: PAnalyzedLevel;
  i, j, n: integer;
  addres: ^pointer;
  dataPts: ANNpointArray; // data points
  tempPts: ^Double;
  queryPt: TANNpoint; // query point
  Idx: TANNidx;
  nnIdx: TANNidxArray; // near neighbor indices
  dists: TANNdistArray; // near neighbor distances
  kdTree: TANNkd_tree; // search structure
  neigh: TNeighborhood3c;
  Knearest: TMostSimilar;
  neighsSize: integer;
begin
  Level := FAnalysisData.Levels[alevel];
  // build ANN structure
  neighsSize := FAnalysisData.Neighborhoods.Width *
    FAnalysisData.Neighborhoods.Height;
  GetMem(nnIdx, SIMILAR_NEIGHBOUR_SIZE * SizeOf(TANNidx));
  GetMem(dists, SIMILAR_NEIGHBOUR_SIZE * SizeOf(TANNdist));
  // allocate query point
  queryPt := annAllocPt(NEIGHBOUR_SIZE_3COLOR);
  // allocate data points
  dataPts := annAllocPts(neighsSize, NEIGHBOUR_SIZE_3COLOR);
  // fill-in points
  addres := pointer(dataPts);
  for i := 0 to FAnalysisData.Neighborhoods.Height - 1 do
  begin
    for j := 0 to FAnalysisData.Neighborhoods.Width - 1 do begin
      neigh := FAnalysisData.Neighborhoods.At[j, i, alevel];
      tempPts := addres^;
      for n := 0 to NEIGHBOUR_SIZE_3COLOR - 1 do begin
        tempPts^ := neigh[n]; // singe -> double
        Inc(tempPts);
      end;
      Inc(addres);
    end;
    Inc(Level.CycleCounter, FAnalysisData.Neighborhoods.Width);
  end;
  // build search structure
  kdTree := TANNkd_tree.Create(dataPts, neighsSize, NEIGHBOUR_SIZE_3COLOR);

  for i := 0 to FAnalysisData.Neighborhoods.Height - 1 do
  begin
    for j := 0 to FAnalysisData.Neighborhoods.Width - 1 do
    begin
      neigh := FAnalysisData.Neighborhoods.At[j, i, alevel];
      // tempPts := pointer(queryPt);
      for n := 0 to NEIGHBOUR_SIZE_3COLOR - 1 do
      begin
        queryPt^ := neigh[n];
        Inc(queryPt);
      end;
      Dec(queryPt, NEIGHBOUR_SIZE_3COLOR);
      // search for similar neighborhood in reference
      kdTree.annkSearch( // search
        queryPt, // query point
        SIMILAR_NEIGHBOUR_SIZE, // number of near neighbors
        nnIdx, // nearest neighbors (returned)
        dists, // distance (returned)
        INV255); // error bound);
      // store result
      for n := 0 to SIMILAR_NEIGHBOUR_SIZE - 1 do begin
        Idx := nnIdx^;
        Inc(nnIdx);
        Knearest[n][0] := Idx mod FAnalysisData.Neighborhoods.Width;
        Knearest[n][1] := Idx div FAnalysisData.Neighborhoods.Width;
      end;
      FAnalysisData.KNearests.At[j, i, alevel] := Knearest;
      Dec(nnIdx, SIMILAR_NEIGHBOUR_SIZE);
    end;
    Inc(Level.CycleCounter, FAnalysisData.Neighborhoods.Width);
  end;

  kdTree.Free;
  annDeallocPt(queryPt);
  annDeallocPts(dataPts);
  FreeMem(nnIdx, SIMILAR_NEIGHBOUR_SIZE * SizeOf(TANNidx));
  FreeMem(dists, SIMILAR_NEIGHBOUR_SIZE * SizeOf(TANNdist));
end;
}
constructor TAnalyzer.Create(anAnalysisData: TAnalysisData);
begin
  Assert(Assigned(anAnalysisData));
  FAnalysisData := anAnalysisData;
  FMaxCPUThreads := 2;
end;

destructor TAnalyzer.Destroy;
begin
  Stop;
  inherited;
end;

procedure TAnalyzer.GatherNeighborhoods(alevel: integer);
var
  Level : PAnalyzedLevel;
  i, j: integer;
begin
  Level := FAnalysisData.Levels[aLevel];
  for i := 0 to FAnalysisData.Exemplar.Height - 1 do
  begin
    for j := 0 to FAnalysisData.Exemplar.Width - 1 do
    begin
      FAnalysisData.Neighborhoods.At[j, i, alevel] :=
        FAnalysisData.GatherNeighborhood(j, i, alevel);
    end;
    Inc(Level.CycleCounter, FAnalysisData.Exemplar.Width);
  end;
end;

function TAnalyzer.GetDone: boolean;
var
  l: integer;
begin
  result := true;
  if Length(FThreads) > 0 then
  begin
    for l := 0 to High(FThreads) do
      result := result and FThreads[l].Finished;
    FAnalysisData.IsValid := Result;
  end;
end;

function TAnalyzer.GetLevelCount: integer;
begin
  if Assigned(FAnalysisData) then
    result := FAnalysisData.LevelsAmount
  else
    result := 0;
end;

function TAnalyzer.GetProgress: Single;
var
  t: integer;
begin
  Result := 0;
  for t := 0 to High(FThreads) do
    result := result + FThreads[t].Progress;

  if Length(FThreads) > 0 then
    Result := Result / Length(FThreads);

  if Result >= 1.0 then
    FAnalysisData.IsValid := True;
end;

procedure TAnalyzer.PrepareKNearest(alevel: integer);
var
  Level: PAnalyzedLevel;
  p: PSmallInt;
  i, j: integer;
  ms: TMostSimilar;
begin
  Level := FAnalysisData.Levels[alevel];
  with Level^ do
  begin
    FillChar(kNearest, SizeOf(TImageDesc), $00);
    kNearest.Width := FAnalysisData.Exemplar.Width;
    kNearest.Height := FAnalysisData.Exemplar.Height;
    kNearest.InternalFormat := GL_RGBA16I;
    kNearest.ColorFormat := GL_RGBA_INTEGER;
    kNearest.ElementSize := 8;
    kNearest.DataType := GL_SHORT;
    kNearest.DataSize := kNearest.Width * kNearest.Height * 8;
    GetMem(kNearest.Data, kNearest.DataSize);
  end;

  // scale color to byte size
  p := Level.kNearest.Data;
  for i := 0 to Level.kNearest.Height - 1 do
  begin
    for j := 0 to Level.kNearest.Width  - 1 do
    begin
      ms := FAnalysisData.kNearests.At[j, i, alevel];
      p[0] := ms[1][0];
      p[1] := ms[1][1];
      p[2] := ms[SIMILAR_NEIGHBOUR_SIZE-1][0];
      p[3] := ms[SIMILAR_NEIGHBOUR_SIZE-1][1];
      Inc(p, 4);
    end;
    Inc(Level.CycleCounter, Level.kNearest.Width);
  end;
end;

procedure TAnalyzer.Process;
var
  i, w: integer;
begin
  if FMaxCPUThreads > 0 then
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
end;

procedure TAnalyzer.ProjectNeighbTo6D(alevel: integer);
const
  MAX6D: TVector6f = (1E30, 1E30, 1E30, 1E30, 1E30, 1E30);
  MIN6D: TVector6f = (-1E30, -1E30, -1E30, -1E30, -1E30, -1E30);
var
  Level: PAnalyzedLevel;
  x: TDouble2DArray;
  Dispersion: TDouble1DArray;
  PCA: TDouble2DArray;
  Info: integer;
  h, w, i, j, k, mi, mj, row, esize: integer;
  V6D, minValue, maxValue: TVector6f;
  Neighb: TNeighborhood3c;
  p1, p2: PByte;
begin
  Level := FAnalysisData.Levels[aLevel];
  w := FAnalysisData.Neighborhoods.Width;
  h := FAnalysisData.Neighborhoods.Height;

  // fill array for principal component analysis
  SetLength(x, w * h, NEIGHBOUR_SIZE_3COLOR);
  row := 0;
  for i := 0 to h - 1 do
  begin
    for j := 0 to w - 1 do
    begin
      Neighb := FAnalysisData.Neighborhoods[j, i, alevel];
      for mj := 0 to NEIGHBOUR_SIZE_3COLOR - 1 do
        x[row][mj] := Neighb[mj];
      Inc(row);
    end;
    Inc(Level.CycleCounter, w);
  end;

  // run principal component analysis
  PrincipalComponentsAnalysis.BuildBasis(x, w * h, NEIGHBOUR_SIZE_3COLOR,
    Info, Dispersion, PCA);

  // store only 6 basis
  if Info = 1 then
    for i := 0 to 5 do
    begin
      k := 0;
      for j := 0 to NEIGHBOUR_SIZE_1COLOR - 1 do
        with FAnalysisData.Levels[alevel]^ do
        begin
          k := j * 4;
          row := j * 3;
          NeihgbPCAMatrix[k, i] := PCA[row, i];
          NeihgbPCAMatrix[k+1, i] := PCA[row+1, i];
          NeihgbPCAMatrix[k+2, i] := PCA[row+2, i];
          NeihgbPCAMatrix[k+3, i] := 0;
        end;
    end;

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
          with FAnalysisData.Levels[alevel]^ do
              V6D[mi] := V6D[mi] + NeihgbPCAMatrix[4 * mj div 3, mi] * Neighb[mj];
      FAnalysisData.Neighborhoods.As6DAt[j, i, alevel] := V6D;

      for k := 0 to 5 do
      begin
        if V6D[k] > maxValue[k] then
          maxValue[k] := V6D[k];
        if V6D[k] < minValue[k] then
          minValue[k] := V6D[k];
      end;

      Inc(Level.CycleCounter);
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

  p1 := FAnalysisData.Levels[alevel].Neighborhoods[0].Data;
  p2 := FAnalysisData.Levels[alevel].Neighborhoods[1].Data;
  for i := 0 to h - 1 do
  begin
    for j := 0 to w - 1 do
    begin
      V6D := FAnalysisData.Neighborhoods.As6DAt[j, i, alevel];
      for k := 0 to 5 do
        V6D[k] := (V6D[k] - minValue[k]) / maxValue[k];
      p1[0] := floor(255 * V6D[0]);
      p1[1] := floor(255 * V6D[1]);
      p1[2] := floor(255 * V6D[2]);
      p1[3] := $FF;
      Inc(p1, 4);
      p2[0] := floor(255 * V6D[3]);
      p2[1] := floor(255 * V6D[4]);
      p2[2] := floor(255 * V6D[5]);
      p2[3] := $FF;
      Inc(p2, 4);
    end;
    Inc(Level.CycleCounter, w);
  end;
end;

procedure TAnalyzer.ProjectStackLevelTo2D(aLevel: integer);
var
  Level: PAnalyzedLevel;
  img: PImageDesc;
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
  Level := FAnalysisData.Levels[aLevel];
  img := FAnalysisData.Images[aLevel];
  w := img.Width;
  h := img.Height;
  esize := img.ElementSize;
  // Fill array for principal components analysis
  SetLength(x, w * h, 3);
  row := 0;
  for i := 0 to h - 1 do
  begin
    p := img.Data;
    Inc(p, i * w * esize);
    for j := 0 to w - 1 do
    begin
      x[row][0] := p[0];
      x[row][1] := p[1];
      x[row][2] := p[2];
      Inc(row);
      Inc(p, esize);
    end;
    Inc(Level.CycleCounter, w);
  end;

  // Run analysis
  PrincipalComponentsAnalysis.BuildBasis(x, w * h, 3, Info, Dispersion, PCA);

  // store only 2 basis
  if Info = 1 then
    for i := 0 to 1 do
      for j := 0 to 2 do
        Level.ColorPCAMatrix[j, i] := PCA[j, i];

  GetMem(C2, w * h * SizeOf(TVector));
  minValue := TVector.Make(1E30, 1E30);
  maxValue := minValue.Negate;
  clr := TVector.Null;

  try
    // project 3D color to 2D
    iterC2 := C2;
    for i := 0 to h - 1 do
    begin
      p := img.Data;
      Inc(p, i * w * esize);
      for j := 0 to w - 1 do
      begin
        clr.x := Level.ColorPCAMatrix[0, 0] * p[0] + Level.ColorPCAMatrix
          [1, 0] * p[1] + Level.ColorPCAMatrix[2, 0] * p[2];

        clr.y := Level.ColorPCAMatrix[0, 1] * p[0] + Level.ColorPCAMatrix
          [1, 1] * p[1] + Level.ColorPCAMatrix[2, 1] * p[2];

        maxValue := TVector.MaxVector(maxValue, clr);
        minValue := TVector.MinVector(minValue, clr);

        iterC2^ := clr;
        Inc(iterC2);
        Inc(p, esize);

        Inc(Level.CycleCounter);
      end;
    end;

    Level.ColorOffset := minValue;
    clr := maxValue - minValue;
    if clr.x = 0 then
      clr.x := 1;
    if clr.y = 0 then
      clr.y := 1;
    Level.ColorScale := clr;

    with Level^ do
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
      p := Level.ProjectedImage.Data;
      Inc(p, i * w * esize);
      for j := 0 to w - 1 do
      begin
        clr := iterC2^;
        p[0] := Floor(255 * (clr.x - minValue.x) / Level.ColorScale.x);
        p[1] := Floor(255 * (clr.y - minValue.y) / Level.ColorScale.y);
        Inc(p, esize);
        Inc(iterC2);

        Inc(Level.CycleCounter);
      end;
    end;
  finally
    FreeMem(C2);
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
  pyramid.Free;

  if FMaxCPUThreads > 0 then
  begin
    SetLength(FThreads, FAnalysisData.LevelsAmount);
    for l := 0 to FAnalysisData.LevelsAmount - 1 do
        FThreads[l] := TAnalyzerThread.CreateOwned(Self,
        FAnalysisData.Levels[l]);
  end
  else
  begin
    for l := 0 to FAnalysisData.LevelsAmount - 1 do
    begin
      ProjectStackLevelTo2D(l);
      // Gather neighborhoods and store them for use during analysis and synthesis
      GatherNeighborhoods(l);
      // Compute lower-dimension neighborhoods
      ProjectNeighbTo6D(l);
      // Analyze stack level - search most similar neighborhoods
      AnalyzeStackLevel(l);
    end;
    FAnalysisData.IsValid := True;
  end;
end;

function TAnalyzer.Stop: Boolean;
var
  i: integer;
begin
  // wait until threads finished
  Result := true;
  for i := 0 to High(FThreads) do
    Result := Result and (FThreads[i].Finished or FThreads[i].Suspended);

  if Result then
  begin
  for i := 0 to High(FThreads) do
    FThreads[i].Destroy;
  SetLength(FThreads, 0);
end;

  FAnalysisData.IsValid := False;
end;

{$ENDREGION}
{$REGION 'TAnalyzerThread'}

constructor TAnalyzerThread.CreateOwned(aOwner: TAnalyzer;
  alevel: PAnalyzedLevel);
begin
  inherited Create(true);
  FAnalyzer := aOwner;
  FData := alevel;
  FTatalCycle := 5 * FData.FloatImage.Width * FData.FloatImage.Height + 5 *
    aOwner.FAnalysisData.Exemplar.Width * aOwner.FAnalysisData.Exemplar.Height;
end;

procedure TAnalyzerThread.Execute;
begin
  // Project 3D color exemplar stack level to 2D color
  FAnalyzer.ProjectStackLevelTo2D(FData.LevelId);
  // Gather neighborhoods and store them for use during analysis and synthesis
  FAnalyzer.GatherNeighborhoods(FData.LevelId);
  // Compute lower-dimension neighborhoods
  FAnalyzer.ProjectNeighbTo6D(FData.LevelId);
  // Analyze stack level - search most similar neighborhoods
  FAnalyzer.AnalyzeStackLevel(FData.LevelId);
  // Prepare image for kNearest
  FAnalyzer.PrepareKNearest(FData.LevelId);
end;

function TAnalyzerThread.GetProgress: Single;
begin
  Result := FData.CycleCounter / FTatalCycle;
end;

{$ENDREGION}

end.
