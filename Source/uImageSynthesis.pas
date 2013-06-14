unit uImageSynthesis;

interface

uses
  Classes,
  uPersistentClasses,
  SyncObjs,
  uImageAnalysisClasses,
  uBaseTypes;

{$DEFINE PACKED_EXEMPLAR_RG}

type

  TSynthesizerThread = class;

  TSynthesizer = class(TPersistentResource)
  private
    FAnalysisData: TAnalysisData;
    FWidth, FHeight: integer;
    FSynthesized: array of TVec2iArray2D;
    FReadBuffer: TVec2iArray2D;
    FProcessedLevel: integer;
    FKappa: single;
    FJitterStrength: single;
    FJitterPeriodX: integer;
    FJitterPeriodY: integer;
    FEdgePolicyFunc: TEdgePolicyFunc;

    // Threads
    FThreads: array of TSynthesizerThread;
    FLock: TCriticalSection;
    FMaxCPUThreads: integer;
    FTotalCycles: integer;

    // Phase of synthesizer's work
    FPhase: integer;
    FCorrectionSubpassesCount: integer;

    // Upsamples previous level synthesis result
    procedure UpsampleToNextLevel;
    // Adds jitter
    procedure Jitter(strength: single; const aIndices: TVec2iArray2D);

    // Sub-pass mechanism

    // CorrectionSubpass processes pixels in an interleaved pattern aligned with ci,cj
    // TSynthesizerThread are created, each calling CorrectionSubpassInRegion
    procedure CorrectionSubpass(subPass: integer; aIndices: TVec2iArray2D);
    // CorrectionSubpassInRegion processes pixels of a sub-region of the synthesized image
    procedure CorrectionSubpassInRegion(subPass, rx, ry, rw, rh: integer);

    // Helper methods

    // Gather a neighborhood in the current synthesis result
    function GatherNeighborhood(j, i, l: integer): TNeighborhood3c;
    function GetLevelCount: integer;
    function GetSynthImage(aLevel: integer): TImageDesc;
    function GetPatchesImage(aLevel: integer): TImageDesc;
    procedure SetCorrectionSubpassesCount(const Value: integer);
    procedure DestroyThread;
    function GetProgress: Single;
  public
    constructor CreateFrom(anAnalysisData: TAnalysisData);
    destructor Destroy; override;

    procedure Notify(Sender: TObject; Msg: Cardinal;
      Params: pointer = nil); override;

    // Runs synthesis.
    procedure Start;
    procedure Process;
    procedure Stop;
    property Progress: Single read GetProgress;

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
    // Controls whether coherent candidates are favored; 1.0 has no effect,
    // 0.1 has strong effect, 0.0 is invalid.
    property Kappa: single read FKappa write FKappa;
    property AnalysisData: TAnalysisData read FAnalysisData;
    // Number of threads to be used for the correction step
    property MaxCPUThreads: integer read FMaxCPUThreads write FMaxCPUThreads;
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
  uVMath,
  Math,
  uMiscUtils;

constructor TSynthesizer.CreateFrom(anAnalysisData: TAnalysisData);
begin
  Create;
  Assert(Assigned(anAnalysisData));
  FAnalysisData := anAnalysisData;
  FAnalysisData.Subscribe(Self);
  FEdgePolicyFunc := EdgePolicyFor.GetFuncN(anAnalysisData.EdgePolicy);
  FWidth := 256;
  FHeight := 256;
  FCorrectionSubpassesCount := 2;
  FJitterStrength := 0;
  FJitterPeriodX := 0; // zero for non periodic jitter
  FJitterPeriodY := 0;
  FKappa := 1.0;
  FMaxCPUThreads := 4;
  FTotalCycles := 1;
  FLock := TCriticalSection.Create;
end;

destructor TSynthesizer.Destroy;
begin
  Stop;
  FAnalysisData.UnSubscribe(Self);
  FReadBuffer.Free;
  FLock.Destroy;
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
  if Value <> FCorrectionSubpassesCount then
  begin
    FCorrectionSubpassesCount := Value;
    DispatchMessage(NM_ResourceChanged);
  end;
end;

procedure TSynthesizer.Start;
var
  l: integer;
  nx, ny: integer;
  V: Vec2i;
begin
  Stop;
  FEdgePolicyFunc := EdgePolicyFor.GetFuncN(FAnalysisData.EdgePolicy);

  // initialize from coarsest level to finest obtain desired resolution at finest level
  SetLength(FSynthesized, FAnalysisData.LevelsAmount);
  nx := TMath.Ceil(FWidth / FAnalysisData.Exemplar.Width);
  ny := TMath.Ceil(FHeight / FAnalysisData.Exemplar.Height);
  FTotalCycles := -nx * ny * (2 + 4 * FCorrectionSubpassesCount);
  for l := High(FSynthesized) downto 0 do
  begin
    FSynthesized[l] := TVec2iArray2D.Create(nx, ny);
    Inc(FTotalCycles, nx * ny * (2 + 4 * FCorrectionSubpassesCount));
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
      Dec(FProcessedLevel);
      if FProcessedLevel < 0 then
        Exit;
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
      if FProcessedLevel < 3 then
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
        if not Assigned(FReadBuffer) then
          FReadBuffer := TVec2iArray2D.Create(
            FSynthesized[FProcessedLevel].Width,
            FSynthesized[FProcessedLevel].Height);

        FReadBuffer.Assign(FSynthesized[FProcessedLevel]);
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
        FPhase := 0; // Next level
      end;
    end;
  end;
end;

procedure TSynthesizer.UpsampleToNextLevel;
var
  spacing, pi, pj, p2i, p2j: integer;
  V, detV: Vec2i;
  UpLevel, DownLevel: TVec2iArray2D;
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
      p2j := pj shl 1;
      p2i := pi shl 1;
      UpLevel.At[p2j, p2i] := V;
      // (1, 0)
      detV[0] := EdgePolicyFor.RepeatedImage(V[0] + spacing,
        FAnalysisData.Exemplar.Width);
      detV[1] := V[1];
      UpLevel.At[p2j + 1, p2i] := detV;
      // (0, 1)
      detV[0] := V[0];
      detV[1] := EdgePolicyFor.RepeatedImage(V[1] + spacing,
        FAnalysisData.Exemplar.Height);
      UpLevel.At[p2j, p2i + 1] := detV;
      // (1, 1)
      detV[0] := EdgePolicyFor.RepeatedImage(V[0] + spacing,
        FAnalysisData.Exemplar.Width);
      detV[1] := EdgePolicyFor.RepeatedImage(V[1] + spacing,
        FAnalysisData.Exemplar.Height);
      UpLevel.At[p2j + 1, p2i + 1] := detV;
    end;
  UpLevel.CycleCounter := UpLevel.CycleCounter + UpLevel.Width * UpLevel.Height;
end;

procedure TSynthesizer.Jitter(strength: single; const aIndices: TVec2iArray2D);
var
  spacing, i, j, w, h: integer;
  kx, ky, dx, dy: single;
  V: Vec2i;
begin
  w := aIndices.Width;
  h := aIndices.Height;

  if strength > 0 then
  begin
    if FJitterPeriodX + FJitterPeriodY = 0 then
    begin
      // Perturbs synthesized coordinates
      for i := 0 to h - 1 do
        for j := 0 to w - 1 do
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

      for i := 0 to h - 1 do
        for j := 0 to w - 1 do
        begin
          V := aIndices.At[j, i];
          V[0] := V[0] + floor(kx * floor(strength * random / kx + dx));
          V[1] := V[1] + floor(ky * floor(strength * random / ky + dy));
          aIndices.At[j, i] := V;
        end;
    end;
  end;

  aIndices.CycleCounter := aIndices.CycleCounter + w * h;
end;

procedure TSynthesizer.Notify(Sender: TObject; Msg: Cardinal; Params: pointer);
begin
  if Sender = FAnalysisData then
  begin
    if Msg = NM_ObjectDestroyed then
    begin
      Assert(False, 'Analysis data destroyed before class-user destruction');
      FAnalysisData := nil;
    end;
  end;
end;

procedure TSynthesizer.CorrectionSubpass(subPass: integer;
  aIndices: TVec2iArray2D);
var
  t, next: integer;
  h: integer;
  thread: TSynthesizerThread;
begin
  if FMaxCPUThreads > 0 then
  begin
    // Run threads
    h := TMath.Ceil(aIndices.Height / FMaxCPUThreads);
    SetLength(FThreads, TMath.Min(TMath.Ceil(aIndices.Height / h),
      FMaxCPUThreads));
    next := 0;
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
  end
  else begin
    CorrectionSubpassInRegion(subPass and 3, 0, 0, aIndices.Width, aIndices.Height);
  end;
end;

procedure TSynthesizer.CorrectionSubpassInRegion(subPass: integer; rx: integer;
  ry: integer; rw: integer; rh: integer);
const
  STEP: array [0 .. 3] of Vec2i = ((0, 0), (1, 1), (0, 1), (1, 0));
var
  Dest: TVec2iArray2D;
  spacing, i, j, k, ni, nj, ci, cj, nk: integer;
  ms: TMostSimilar;
  filter, n, c, best: Vec2i;
  syN: TNeighborhood3c;
  NM: TNeighbPCAmatrix;
  syN_V6D, exN_V6D: TVector6f;
  minDis, Dis: single;
begin
  Dest := FSynthesized[FProcessedLevel];

  // Applies correction to 1/4 of pixels within a given region.
  filter := step[subPass];
  // In a multi-thread environment, the image is divided into regions,
  // each being processed by a thread calling this function.
  spacing := 1 shl FProcessedLevel;
  // apply neighborhood matching to region
  for i := ry to ry + rh - 1 do
  begin
    for j := rx to rx + rw - 1 do
    begin
      // only process one pixel in 4 (sub-pass mechanism)
      if ((j and 1) <> filter[0]) or ((i and 1) <> filter[1]) then
        continue;
      /// Gather current neighborhood in synthesized texture
      syN := GatherNeighborhood(j, i, FProcessedLevel);
      syN_V6D := ZERO_VECTOR6D;
      NM := FAnalysisData.Levels[FProcessedLevel].NeihgbPCAMatrix;
      // project it to 6D vector
      for nj := 0 to NEIGHBOUR_SIZE_3COLOR - 1 do
        for ni := 0 to 5 do
          syN_V6D[ni] := syN_V6D[ni] + NM[4 * nj div 3, ni] * syN[nj];
      /// Find best matching candidate
      minDis := 1E30;
      best := FReadBuffer.At[j, i];
      /// Gather candidates
      // for each neighbor around the pixel (9 of them, including center)
      for ni := -1 to 1 do
      begin
        for nj := -1 to 1 do
        begin
          // n is a coordinate in exemplar stack
          n := FReadBuffer.At[j + nj, i + ni];

          ms := FAnalysisData.KNearests.At[n[0], n[1], FProcessedLevel];
          // delta must be multiplied by stack level offset
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
        end; // for nj
      end; // for ni

      // self as last -- VERY IMPORTANT to ensure identity in coherent patches
      n := FReadBuffer.At[j, i];
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
      Dest.At[j, i] := best;
    end; // for j

    // Atomic counter
    FLock.Enter;
    Dest.CycleCounter := Dest.CycleCounter + rw;
    FLock.Leave;
  end; // for i
end;

function TSynthesizer.GatherNeighborhood(j: integer; i: integer; l: integer)
  : TNeighborhood3c;
var
  img: PImageDesc;
  p: PByte;
  At: integer;
  x, y: integer;
  s: Vec2i;
begin
  // Gather a neighborhood in the current synthesis result
  At := 0;
  img := FAnalysisData.Images[l];
  for y := i-HALF_NEIGHBOUR_DIM to i+HALF_NEIGHBOUR_DIM do
    for x := j-HALF_NEIGHBOUR_DIM to j+HALF_NEIGHBOUR_DIM do
    begin
      s := FReadBuffer.At[x, y]; // S[p]  (coordinate in exemplar stack)
      s[0] := FEdgePolicyFunc(s[0], img.Width);
      s[1] := FEdgePolicyFunc(s[1], img.Height);
      p := img.Data;
      Inc(p, (s[0] + s[1] * img.Width) * img.ElementSize);
      Result[At + 0] := p[0]; // E[S[p]] (RGB color)
      Result[At + 1] := p[1];
      Result[At + 2] := p[2];
      Inc(At, 3);
    end;
end;

function TSynthesizer.GetSynthImage(aLevel: integer): TImageDesc;
var
  src: PImageDesc;
  crd: TVec2iArray2D;
  i, j: integer;
  xy: Vec2i;
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
  result.DataType := GL_UNSIGNED_BYTE;
  result.DataSize := result.Width * result.Height * result.ElementSize;
  GetMem(result.Data, result.DataSize);

  for i := 0 to crd.Height - 1 do
  begin
    pb_Dst := result.Data;
    Inc(pb_Dst, i * result.Width * result.ElementSize);
    for j := 0 to crd.Width - 1 do
    begin
      xy := crd.At[j, i];
      xy[0] := EdgePolicyFor.RepeatedImage(xy[0], src.Width);
      xy[1] := EdgePolicyFor.RepeatedImage(xy[1], src.Height);
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
  img: TVec2iArray2D;
  i, j: integer;
  xy: Vec2i;
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
  result.DataType := GL_UNSIGNED_BYTE;
  result.DataSize := img.Width * img.Height * result.ElementSize;
  GetMem(result.Data, result.DataSize);

  for i := 0 to img.Height - 1 do
  begin
    p := result.Data;
    Inc(p, i * img.Width * result.ElementSize);
    for j := 0 to img.Width - 1 do
    begin
      xy := img.At[j, i];
{$IFDEF PACKED_EXEMPLAR_RG}
      p[0] := floor(256 * EdgePolicyFor.RepeatedImage(xy[0],
        FAnalysisData.Exemplar.Width) / FAnalysisData.Exemplar.Width);
      p[1] := floor(256 * EdgePolicyFor.RepeatedImage(xy[1],
        FAnalysisData.Exemplar.Height) / FAnalysisData.Exemplar.Height);
{$ELSE}
      p[0] := 0;
      p[2] := floor(256 * EdgePolicyFor.RepeatedImage(xy[0],
        FAnalysisData.Exemplar.Width) / FAnalysisData.Exemplar.Width);
      p[1] := floor(256 * EdgePolicyFor.RepeatedImage(xy[1],
        FAnalysisData.Exemplar.Height) / FAnalysisData.Exemplar.Height);
{$ENDIF}
      Inc(p, result.ElementSize);
    end;
  end;
end;

function TSynthesizer.GetProgress: Single;
var
  l: integer;
begin
  Result := 0;
  for l := 0 to High(FSynthesized) do
    result := result + FSynthesized[l].CycleCounter;
  result := result / FTotalCycles;
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
