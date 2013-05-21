unit uImageSynthesis;

interface

uses
  Classes,
  uImageAnalysisClasses,
  uBaseTypes;

{ .$IFDEF PACKED_EXEMPLAR_RG }

type

  TSynthesizerThread = class;

  TSynthesizer = class
  private
    FAnalysisData: TAnalysisData;
    FWidth, FHeight: integer;
    FSynthesized: array of TIVec2Array2D;
    FWriteBuffer: TIVec2Array2D;
    FProcessedLevel: integer;
    FKappa: single;
    FJitterStrength: single;
    FJitterPeriodX: integer;
    FJitterPeriodY: integer;

    FEdgePolicy: TEdgePolicy;
    FEdgePolicyFunc: TEdgePolicyFunc;

    // Threads
    FThreads: array of TSynthesizerThread;
    FMaxCPUThreads: integer;

    // Phase of synthesizer's work
    FPhase: integer;

    FCorrectionSubpassesCount: integer;

    // Upsamples previous level synthesis result
    procedure UpsampleToNextLevel;
    // Adds jitter
    procedure Jitter(strength: single; const aIndices: TIVec2Array2D);

    // Sub-pass mechanism

    // CorrectionSubpass processes pixels in an interleaved pattern aligned with ci,cj
    // TSynthesizerThread are created, each calling CorrectionSubpassInRegion
    procedure CorrectionSubpass(subPass: integer; aIndices: TIVec2Array2D);
    // CorrectionSubpassInRegion processes pixels of a sub-region of the synthesized image
    procedure CorrectionSubpassInRegion(subPass, rx, ry, rw, rh: integer);

    // Helper methods

    // Gather a neighborhood in the current synthesis result
    function GatherNeighborhood(j, i, step: integer): TNeighborhood3c;
    function GetLevelCount: integer;
    function GetDone: boolean;
    procedure SetMaxCPUThreads(const Value: integer);
    function GetSynthImage(aLevel: integer): TImageDesc;
    function GetPatchesImage(aLevel: integer): TImageDesc;
    procedure SetCorrectionSubpassesCount(const Value: integer);
    procedure DestroyThread;
  public
    constructor Create(anAnalysisData: TAnalysisData);
    destructor Destroy; override;

    // Runs synthesis.
    procedure Start;
    procedure Process;
    procedure Stop;
    property Done: boolean read GetDone;

    property CorrectionSubpassesCount: integer read FCorrectionSubpassesCount
      write SetCorrectionSubpassesCount;
    // Returns synthesis result
    property SynthImage[aLevel: integer]: TImageDesc read GetSynthImage;
    // Returns color-coded patches for the result
    property PatchesImage[aLevel: integer]: TImageDesc read GetPatchesImage;
    // Dimension of result image
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
    // Return numder of synthesyzed levels
    property LevelCount: integer read GetLevelCount;
    // Controls jitter strength.
    property JitterStrength: single read FJitterStrength write FJitterStrength;
    // Controls jitter horizontal and vertical periods
    property JitterPeriodX: integer read FJitterPeriodX write FJitterPeriodX;
    property JitterPeriodY: integer read FJitterPeriodY write FJitterPeriodY;
    // Controls whether coherent candidates are favored; 1.0 has no effect, 0.1 has strong effect, 0.0 is invalid.
    property Kappa: single read FKappa write FKappa;
    property AnalysisData: TAnalysisData read FAnalysisData;
    // Number of threads to be used for the correction step
    property MaxCPUThreads: integer read FMaxCPUThreads write SetMaxCPUThreads;
  end;

  TSynthesizerThread = class(TThread)
  private
    FSynthesizer: TSynthesizer;
    FSubPass, Frx, Fry, Frw, Frh: integer;
  protected
    constructor CreateOwned(aOwner: TSynthesizer);
    procedure Execute; override;
    property subPass: integer read FSubPass write FSubPass;
    property RegionLeft: integer read Frx write Frx;
    property RegionTop: integer read Fry write Fry;
    property RegionWidth: integer read Frw write Frw;
    property RegionHeight: integer read Frh write Frh;
  end;

implementation

uses
  uMath,
  Math;

constructor TSynthesizer.Create(anAnalysisData: TAnalysisData);
begin
  Assert(Assigned(anAnalysisData));
  FAnalysisData := anAnalysisData;
  FEdgePolicy := epWrap;
  FEdgePolicyFunc := TIVec2Array2D.WrapAccess;
  FWidth := 256;
  FHeight := 256;
  FCorrectionSubpassesCount := 2;
  FJitterStrength := 0;
  FJitterPeriodX := 0; // zero for non periodic jitter
  FJitterPeriodY := 0;
  FKappa := 1.0;
  FMaxCPUThreads := 4;
end;

destructor TSynthesizer.Destroy;
begin
  Stop;
  FWriteBuffer.Free;
  inherited;
end;

procedure TSynthesizer.DestroyThread;
var
  t: integer;
begin
  for t := 0 to High(FThreads) do
    FThreads[t].Destroy;
  SetLength(FThreads, 0);
end;

procedure TSynthesizer.SetCorrectionSubpassesCount(const Value: integer);
begin
  Assert(Value > 0);
  FCorrectionSubpassesCount := Value;
end;

procedure TSynthesizer.SetMaxCPUThreads(const Value: integer);
begin
  Assert(Value > 0);
  FMaxCPUThreads := Value;
end;

procedure TSynthesizer.Start;
var
  l: integer;
  nx, ny: integer;
  V: IVec2;
begin
  Stop;

  if FAnalysisData.Toroidality then
    FEdgePolicyFunc := TIVec2Array2D.WrapAccess
  else
    FEdgePolicyFunc := TIVec2Array2D.MirrorAccess;

  // initialize from coarsest level to finest obtain desired resolution at finest level
  SetLength(FSynthesized, FAnalysisData.LevelsAmount);
  nx := TMath.Ceil(FWidth / FAnalysisData.Exemplar.Width);
  ny := TMath.Ceil(FHeight / FAnalysisData.Exemplar.Height);
  for l := High(FSynthesized) downto 0 do
  begin
    FSynthesized[l] := TIVec2Array2D.Create(nx, ny);
    nx := 2 * nx;
    ny := 2 * ny;
  end;
  // start level is coarsest
  FProcessedLevel := High(FSynthesized);
  // with current algorithm it makes no sense to start at level 0
  Assert(FProcessedLevel > 0);

  // fill coarsest level with exemplar center coordinates (in abscence of jitter, produces tiling of the exemplar)
  V[0] := FAnalysisData.Exemplar.Width div 2;
  V[1] := FAnalysisData.Exemplar.Height div 2;
  FSynthesized[FProcessedLevel].Clear(V);
  Dec(FProcessedLevel);
  FPhase := 0;
end;

procedure TSynthesizer.Stop;
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

  DestroyThread;

  for i := 0 to High(FSynthesized) do
    FSynthesized[i].Free;
  SetLength(FSynthesized, 0);
end;

procedure TSynthesizer.Process;
var
  strength: single;
  t: integer;
  nextPass: boolean;
  tmp: TIVec2Array2D;
begin
  // Done if finest level has been reached.
  // This happens when the number of perfromed synthesis steps
  // equals the exemplar level at which synthesis started.
  nextPass := FProcessedLevel > -1;
  // Performs a synthesis step, producing the result at the next level.
  // The new result is added to fSynthesized
  if nextPass then
  begin
    if FPhase = 0 then
    begin
      /// 1. upsample
      UpsampleToNextLevel;
      Inc(FPhase);
    end
    else if FPhase = 1 then
    begin
      /// 2. jitter
      // adapt jitter strength per level - arbitrary, ideally should be per-level
      // user control. Overall it is often more desirable to add strong jitter at
      // coarser levels and let synthesis recover at finer resolution levels.
      if FProcessedLevel >= Length(FSynthesized) - 3 then
        strength := 0
      else
        strength := FJitterStrength * FProcessedLevel /
          FAnalysisData.LevelsAmount;
      // apply jitter
      Jitter(strength, FSynthesized[FProcessedLevel]);
      Inc(FPhase);
    end
    else if (FPhase >= 2) and (FPhase < 2 + 4 * FCorrectionSubpassesCount) then
    begin
      /// 3. correct
      nextPass := true;
      // cheking threads to next subPass
      for t := 0 to High(FThreads) do
        nextPass := nextPass and FThreads[t].Finished;

      if nextPass then
      begin
        DestroyThread;
        if FPhase > 2 then
        begin
          // Swap read buffer and write buffer
          tmp := FSynthesized[FProcessedLevel];
          FSynthesized[FProcessedLevel] := FWriteBuffer;
          FWriteBuffer := tmp;
          FWriteBuffer.Assign(FSynthesized[FProcessedLevel]);
        end
        else if not Assigned(FWriteBuffer) then
          FWriteBuffer := TIVec2Array2D.Create(
            FSynthesized[FProcessedLevel].Width,
            FSynthesized[FProcessedLevel].Height);

        CorrectionSubpass(FPhase - 2, FSynthesized[FProcessedLevel]);
        Inc(FPhase);
      end;
    end
    else
    begin
      nextPass := true;
      // cheking threads to end final subpass
      for t := 0 to High(FThreads) do
        nextPass := nextPass and FThreads[t].Finished;
      if nextPass then
      begin
        DestroyThread;

        if Assigned(FWriteBuffer) then
        begin
          tmp := FSynthesized[FProcessedLevel];
          FSynthesized[FProcessedLevel] := FWriteBuffer;
          FWriteBuffer := tmp;
        end;
        // store result of final subpass
        Dec(FProcessedLevel);
        FPhase := 0; // Next level
      end;
    end;
  end;
end;

procedure TSynthesizer.UpsampleToNextLevel;
var
  spacing, pi, pj: integer;
  V, detV: IVec2;
  UpLevel, DownLevel: TIVec2Array2D;
begin
  DownLevel := FSynthesized[FProcessedLevel + 1];
  UpLevel := FSynthesized[FProcessedLevel];
  // Upsampling of the previous synthesis step result

  spacing := 1 shl FProcessedLevel;
  for pi := 0 to DownLevel.Height - 1 do
    for pj := 0 to DownLevel.Width - 1 do
    begin
      // compute coordinates of children within exemplar stack
      // (0, 0)
      V := DownLevel.At[pj, pi];
      UpLevel.At[pj * 2 + 0, pi * 2 + 0] := V;
      // (1, 0)
      detV[0] := UpLevel.WrapAccess(V[0] + spacing,
        FAnalysisData.Exemplar.Width);
      detV[1] := V[1];
      UpLevel.At[pj * 2 + 1, pi * 2 + 0] := detV;
      // (0, 1)
      detV[0] := V[0];
      detV[1] := UpLevel.WrapAccess(V[1] + spacing,
        FAnalysisData.Exemplar.Height);
      UpLevel.At[pj * 2 + 0, pi * 2 + 1] := detV;
      // (1, 1)
      detV[0] := UpLevel.WrapAccess(V[0] + spacing,
        FAnalysisData.Exemplar.Width);
      detV[1] := UpLevel.WrapAccess(V[1] + spacing,
        FAnalysisData.Exemplar.Height);
      UpLevel.At[pj * 2 + 1, pi * 2 + 1] := detV;
    end;
end;

procedure TSynthesizer.Jitter(strength: single; const aIndices: TIVec2Array2D);
var
  spacing, i, j: integer;
  kx, ky, dx, dy: single;
  V: IVec2;
begin
  if FJitterPeriodX + FJitterPeriodY = 0 then
  begin
    // Perturbs synthesized coordinates
    for i := 0 to aIndices.Height - 1 do
      for j := 0 to aIndices.Width - 1 do
      begin
        // add a random offset, which size is controlled by 'strength'
        V := aIndices.At[j, i];
        V[0] := V[0] + floor(strength * random); // Almighty Random
        V[1] := V[1] + floor(strength * random);
        aIndices.At[j, i] := V;
      end;
  end
  else
  begin
    // maintain tiling periodicity by quantizing each jitter coordinate
    spacing := 1 shl FProcessedLevel;
    kx := FAnalysisData.Exemplar.Width / FJitterPeriodX;
    ky := FAnalysisData.Exemplar.Height / FJitterPeriodY;
    dx := 0.5;
    dy := dx;
    if kx < spacing then
    begin
      kx := 1;
      dx := 0;
    end;
    if ky < spacing then
    begin
      ky := 1;
      dy := 0;
    end;

    for i := 0 to aIndices.Height - 1 do
      for j := 0 to aIndices.Width - 1 do
      begin
        V := aIndices.At[j, i];
        V[0] := V[0] + floor(kx * floor(strength * random / kx + dx));
        V[1] := V[1] + floor(ky * floor(strength * random / ky + dy));
        aIndices.At[j, i] := V;
      end;
  end;
end;

procedure TSynthesizer.CorrectionSubpass(subPass: integer;
  aIndices: TIVec2Array2D);
var
  t, next: integer;
  h: integer;
  thread: TSynthesizerThread;
begin
  h := TMath.Ceil(aIndices.Height / FMaxCPUThreads);
  SetLength(FThreads, TMath.Min(TMath.Ceil(aIndices.Height / h),
    FMaxCPUThreads));
  next := 0;
  // Run threads
  for t := 0 to High(FThreads) do
  begin
    thread := TSynthesizerThread.CreateOwned(Self);
    thread.subPass := subPass and 3;
    thread.RegionLeft := 0;
    thread.RegionTop := next;
    thread.RegionWidth := aIndices.Width;
    thread.RegionHeight := TMath.Clamp(aIndices.Height - next, 0, h);
    thread.Start;
    next := next + h;
    FThreads[t] := thread;
  end;
end;

procedure TSynthesizer.CorrectionSubpassInRegion(subPass: integer; rx: integer;
  ry: integer; rw: integer; rh: integer);
const
  step: array [0 .. 3] of IVec2 = ((0, 0), (1, 0), (0, 1), (1, 1));
var
  Source: TIVec2Array2D;
  spacing, i, j, k, ni, nj, ci, cj, nk: integer;
  ms: TMostSimilar;
  n, c, best: IVec2;
  syN: TNeighborhood3c;
  NM: TNeighbPCAmatrix;
  syN_V6D, exN_V6D: TVector6f;
  minDis, Dis: single;
begin
  Source := FSynthesized[FProcessedLevel];
  // Applies correction to 1/4 of pixels within a given region.
  cj := step[subPass][0];
  ci := step[subPass][1];
  // In a multi-thread environment, the image is divided into regions,
  // each being processed by a thread calling this function.
  spacing := 1 shl FProcessedLevel;
  // apply neighborhood matching to region
  for i := ry to ry + rh - 1 do
    for j := rx to rx + rw - 1 do
    begin
      // only process one pixel in 4 (sub-pass mechanism)
      if ((i and 1) <> ci) or ((j and 1) <> cj) then
        continue;
      /// Gather current neighborhood in synthesized texture
      syN := GatherNeighborhood(j, i, FProcessedLevel);
      syN_V6D := ZERO_VECTOR6D;
      NM := FAnalysisData.Levels[FProcessedLevel].NeihgbPCAMatrix;
      // project it to 6D vector
      for nj := 0 to NEIGHBOUR_SIZE_3COLOR - 1 do
        for ni := 0 to 5 do
          syN_V6D[ni] := syN_V6D[ni] + NM[nj, ni] * syN[nj];
      /// Find best matching candidate
      minDis := 1E30;
      best := Source.At[j, i];
      /// Gather candidates
      // for each neighbor around the pixel (9 of them, including center)
      for ni := -1 to 1 do
        for nj := -1 to 1 do
        begin
          // n is a coordinate in exemplar stack
          n := Source.At[j + nj, i + ni];
          // delta must be multiplied by stack level offset
          ms := FAnalysisData.KNearests.At[n[0], n[1], FProcessedLevel];

          c[0] := ms[0][0] - nj * spacing;
          c[1] := ms[0][1] - ni * spacing;
          exN_V6D := FAnalysisData.Neighborhoods.As6DAt[c[0], c[1],
            FProcessedLevel];
          // compare
          Dis := 0;
          for nk := 0 to 5 do
            Dis := Dis + sqr(exN_V6D[nk] - syN_V6D[nk]);
          // favor (or defavor) coherent candidates
          Dis := Dis * FKappa;
          if Dis <= minDis then
          begin
            minDis := Dis;
            best := c;
          end;

          // cycle for non-coherent candidate
          for k := 1 to SIMILAR_NEIGHBOUR_SIZE - 1 do
          begin
            c[0] := ms[k][0] - nj * spacing;
            c[1] := ms[k][1] - ni * spacing;
            exN_V6D := FAnalysisData.Neighborhoods.As6DAt[c[0], c[1],
              FProcessedLevel];
            // compare
            Dis := 0;
            for nk := 0 to 5 do
              Dis := Dis + sqr(exN_V6D[nk] - syN_V6D[nk]);
            // favor (or defavor) coherent candidates
            if Dis <= minDis then
            begin
              minDis := Dis;
              best := c;
            end;
          end; // for k
        end; // for ninj

      // self as last -- VERY IMPORTANT to ensure identity in coherent patches --
      n := Source.At[j, i];
      exN_V6D := FAnalysisData.Neighborhoods.As6DAt[n[0], n[1],
        FProcessedLevel];
      Dis := 0;
      for nk := 0 to 5 do
        Dis := Dis + sqr(exN_V6D[nk] - syN_V6D[nk]);
      // favor (or defavor) coherent candidates
      Dis := Dis * FKappa;
      if Dis <= minDis then
        best := n;
      // replace in output
      FWriteBuffer.At[j, i] := best;
    end; // for ij
end;

function TSynthesizer.GatherNeighborhood(j: integer; i: integer; step: integer)
  : TNeighborhood3c;
var
  img: TIVec2Array2D;
  fimg: TFloatImage;
  fp: TFloatPixel;
  At: integer;
  ni, nj, di, dj, x, y: integer;
  s: IVec2;
begin
  // Gather a neighborhood in the current synthesis result
  img := FSynthesized[step];
  fimg := FAnalysisData.FloatImages[step];
  At := 0;
  for ni := 0 to NEIGHBOUR_DIM - 1 do
    for nj := 0 to NEIGHBOUR_DIM - 1 do
    begin
      di := ni - NEIGHBOUR_DIM div 2;
      dj := nj - NEIGHBOUR_DIM div 2;
      x := j + dj;
      y := i + di;
      s := img.At[x, y]; // S[p]  (coordinate in exemplar stack)
      x := FEdgePolicyFunc(s[0], fimg.Width);
      y := FEdgePolicyFunc(s[1], fimg.Height);
      fp := fimg.Pixel[x, y];
      result[At + 0] := fp.r; // E[S[p]] (RGB color)
      result[At + 1] := fp.g;
      result[At + 2] := fp.b;
      Inc(At, 3);
    end;
end;

function TSynthesizer.GetSynthImage(aLevel: integer): TImageDesc;
var
  src: PImageDesc;
  crd: TIVec2Array2D;
  i, j: integer;
  xy: IVec2;
  pb_Src, pb_Dst: PByte;
begin
  // Create color version of the synthesis result (which contains coordinates only)
  src := FAnalysisData.Images[aLevel];
  crd := FSynthesized[aLevel];
  FillChar(result, SizeOf(TImageDesc), $00);
  result.Width := crd.Width;
  result.Height := crd.Height;
  result.InternalFormat := GL_RGB8;
  result.ColorFormat := GL_RGB;
  result.ElementSize := 3;
  result.DataSize := result.Width * result.Height * result.ElementSize;
  GetMem(result.Data, result.DataSize);

  for i := 0 to crd.Height - 1 do
  begin
    pb_Dst := result.Data;
    Inc(pb_Dst, i * result.Width * result.ElementSize);
    for j := 0 to crd.Width - 1 do
    begin
      xy := crd.At[j, i];
      xy[0] := TIVec2Array2D.WrapAccess(xy[0], src.Width);
      xy[1] := TIVec2Array2D.WrapAccess(xy[1], src.Height);
      pb_Src := src.Data;
      Inc(pb_Src, (xy[0] + xy[1] * src.Width) * src.ElementSize);
      pb_Dst[0] := pb_Src[0];
      pb_Dst[1] := pb_Src[1];
      pb_Dst[2] := pb_Src[2];
      Inc(pb_Dst, result.ElementSize);
    end;
  end;
end;

function TSynthesizer.GetPatchesImage(aLevel: integer): TImageDesc;
var
  img: TIVec2Array2D;
  i, j: integer;
  xy: IVec2;
  p: PByte;
begin
  img := FSynthesized[aLevel];
  FillChar(result, SizeOf(TImageDesc), $00);
  result.Width := img.Width;
  result.Height := img.Height;
{$IFDEF PACKED_EXEMPLAR_RG}
  result.InternalFormat := GL_RG8;
  result.ColorFormat := GL_RG;
  result.ElementSize := 2;
{$ELSE}
  result.InternalFormat := GL_RGB8;
  result.ColorFormat := GL_RGB;
  result.ElementSize := 3;
{$ENDIF}
  result.DataSize := img.Width * img.Height * result.ElementSize;
  GetMem(result.Data, result.DataSize);

  for i := 0 to img.Height - 1 do
  begin
    p := result.Data;
    Inc(p, i * img.Width * result.ElementSize);
    for j := 0 to img.Width - 1 do
    begin
      xy := img.At[j, i];
      p[0] := 0;
      p[2] := floor(256 * TIVec2Array2D.WrapAccess(xy[0],
        FAnalysisData.Exemplar.Width) / FAnalysisData.Exemplar.Width);
      p[1] := floor(256 * TIVec2Array2D.WrapAccess(xy[1],
        FAnalysisData.Exemplar.Height) / FAnalysisData.Exemplar.Height);
      Inc(p, result.ElementSize);
    end;
  end;
end;

function TSynthesizer.GetDone: boolean;
var
  l: integer;
begin
  if FProcessedLevel > -1 then
    Exit(False);

  result := true;
  for l := 0 to High(FThreads) do
    result := result and FThreads[l].Finished;
end;

function TSynthesizer.GetLevelCount: integer;
begin
  result := Length(FSynthesized);
end;

{$REGION 'TSynthesizerThread' }

constructor TSynthesizerThread.CreateOwned(aOwner: TSynthesizer);
begin
  Create(true);
  FSynthesizer := aOwner;
end;

procedure TSynthesizerThread.Execute;
begin
  FSynthesizer.CorrectionSubpassInRegion(FSubPass, Frx, Fry, Frw, Frh);
end;
{$ENDREGION}

end.
