unit uGLImageSynthesis;

interface

uses
  uPersistentClasses,
  uBaseTypes,
  uImageAnalysisClasses,
  dglOpenGL,
  uBaseGL,
  uRenderResource,
  uVMath;

{$POINTERMATH ON}

const
  PADDING_BORDER = 16; // Pixels
  WORKGROUP_SIZE = 16; // Threads

type

  TCoverLimit = (clmWidthLimit, clmHeightLimit);
  TCoverLimits = set of TCoverLimit;

  TGLSynthLevel = record
    ExemplarTextureId: GLuint;
    PatchesTextureId: GLuint;
    kNearestTextureId: GLuint;
    NeihgbPart1TextureId: GLuint;
    NeihgbPart2TextureId: GLuint;
    NeihgbPCAMatrixBuffer: TGLBufferObject;
    JitterStrength: Vec2;
    Spacing: array [0 .. 2] of Vec4i;
    RandScaleOffset: Vec4i;
    ParentOffset: Vec2i;
    RelativeOffset: Vec2i;
    DestinationOffset: Vec2i;
    State: (stFullOutdated, stPartOutdated, stChanged, stCompleted);
    CoverLimit: TCoverLimits;
  end;

  TConstructPhase = (cphForward, cphUpRight, cphBack, cphUpLeft);

  TTileRec = record
    Coord: Vec2i;
    Completed: Boolean;
  end;

  TGLSynthesizer = class(TPersistentResource)
  private
    FAnalysisData: TAnalysisData;
    FSideSize: integer;
    FHalfSideSize: integer;
    FLevels: array of TGLSynthLevel;
    FUpsampleShaderSource: array [Boolean] of TShaderProgram;
    FCorrectionShaderSource: TShaderProgram;

    FInitialized: Boolean;
    FUpShadersCompiled: Boolean;
    FUpsampleShader: array [Boolean] of TGLSLShaderProgram;
    FCorrectionShader: TGLSLShaderProgram;
    FCopyImageShader: TGLSLShaderProgram;
    FImageConstructShader: TGLSLShaderProgram;
    FWorkGroupCount: vec3i;

    FReadSynthTextureId: GLuint;
    FRandomTextureId: GLuint;
    FDest: TGLTextureObject;
    FConstructState: (cstBeginning, cstInProgress, cstCompleted);
    FConstructPhase: TConstructPhase;
    FLODFromDownLevels: Boolean;
{    FCopyBuffer: TGLBufferObject;}

    FCorrectionSubpassesCount: integer;
    FKappa: single;
    FJitterModulation: Boolean;

    function CompileUpsampleJitterShaders: Boolean;
    function GetLevelCount: integer;
    function GetRelativeOffset(const aLevel: integer): Vec2i;
    procedure SetCorrectionShaderSource(const Value: TShaderProgram);
    procedure SetUpsampleShaderSource(const aStarter: Boolean;
      const Value: TShaderProgram);
    function GetTextureID(const Level: integer): GLuint;
    function GetExemplarTextureID(const Level: integer): GLuint;
    function GetUpsampleShaderSource(const Starter: Boolean): TShaderProgram;
    function GetJitter(const Level: integer): single;
    procedure SetJitter(const Level: integer; const Value: single);
    procedure SetKappa(const Value: single);
    procedure SetCorrectionSubpassesCount(const Value: integer);
    procedure SetDestinationTexture(const Value: TGLTextureObject);
    procedure SetLODFromDownLevels(const Value: Boolean);
    procedure SetJitterModulation(const Value: Boolean);
  protected
    procedure NotifyLevelChanged(const aLevel: integer);
    procedure DefineLevelStatus;
    procedure CreateRandomTexture;
    procedure ResetConstructionProgress;
    procedure DoUpsample(const aLevel: integer); overload;
    procedure DoUpsample(const aLevel: integer; const aRect: Vec4i); overload;
    procedure DoCorrection(const aLevel: integer; const aRect: Vec4i); overload;
    procedure DoCorrection(const aLevel: integer); overload;
    procedure DoShift(const aLevel: integer; const aStep: Vec2i);
    procedure DoConstruct(const aLevel: integer);
    procedure CopyToReadSynthTexture(const aLevel: integer);
    procedure DoScroll(const aLevel: integer; const aStep: Vec2i);
  public
    constructor CreateFrom(const anAnalysisData: TAnalysisData);
    destructor Destroy; override;

    procedure Notify(Sender: TObject; Msg: Cardinal;
      Params: pointer = nil); override;

    procedure Initialize;
    procedure Process;
    procedure Panning(const aDeltaX, aDeltaY: integer);
    procedure Finalize;
    class function Supported: Boolean;
    property Initialized: Boolean read FInitialized;

    property ExemplarTextureIDs[const Level: integer]: GLuint
      read GetExemplarTextureID;
    // GL name of synthesized pathces texture
    property PachesTextureIDs[const Level: integer]: GLuint read GetTextureID;
    // Return numder of synthesyzed levels
    property LevelCount: integer read GetLevelCount;
    // Return side size of synthesized texture
    property SideSize: integer read FSideSize;
    // External synthesis shaders
    property UpsampleShader[const Starter: Boolean]: TShaderProgram
      read GetUpsampleShaderSource write SetUpsampleShaderSource;
    property CorrectionShader: TShaderProgram read FCorrectionShaderSource
      write SetCorrectionShaderSource;
    // Controls whether coherent candidates are favored; 1.0 has no effect,
    // 0.1 has strong effect, 0.0 is invalid.
    property CoherenceWeight: single read FKappa write SetKappa;
    // Controls jitter strength.
    property JitterStrength[const Level: integer]: single read GetJitter
      write SetJitter;
    // Spatial modulation over source exemplar
    property JitterModulation: Boolean read FJitterModulation
      write SetJitterModulation;
    property CorrectionSubpassesCount: integer read FCorrectionSubpassesCount
      write SetCorrectionSubpassesCount;

    property DestinationTexture: TGLTextureObject read FDest
      write SetDestinationTexture;
    property LODFromDownLevels: Boolean read FLODFromDownLevels
      write SetLODFromDownLevels;
  end;

implementation

uses
  uMiscUtils,
  uImageSynthesisShaderGen,
  Math,
  uMath;

const
  ZERO_OFFSETS: Vec4i = (0, 0, 0, 0);
  STEPS: array [0 .. 7] of Vec2i = (
    (0, 0), (1, 1), (0, 1), (1, 0), (1, 1), (0, 0), (0, 1), (1, 0));

{$REGION 'TGLSynthesizer'}


function TGLSynthesizer.CompileUpsampleJitterShaders: Boolean;
var
  shader: TShaderProgram;
  randfunc: TRandomFunc;
begin
  if FJitterModulation then
    randfunc := rfRandomTextureModulation
  else
    randfunc := rfRandomTexture;

  FUpsampleShader[True].Free;
  if not Assigned(FUpsampleShaderSource[True]) then
  begin
    shader := SynthesisShaderGenerator.GenUpsampleJitterShader(Self, True, randfunc);
    FUpsampleShader[True] := TGLSLShaderProgram.CreateFrom(shader);
    shader.Destroy;
  end
  else
      FUpsampleShader[True] := TGLSLShaderProgram.CreateFrom
      (FUpsampleShaderSource[True]);
  FUpsampleShader[True].LinkShader;
  WriteLn(FUpsampleShader[True].Log);

  FUpsampleShader[False].Free;
  if not Assigned(FUpsampleShaderSource[False]) then
  begin
    shader := SynthesisShaderGenerator.GenUpsampleJitterShader(Self, False, randfunc);
    FUpsampleShader[False] := TGLSLShaderProgram.CreateFrom(shader);
    shader.Destroy;
  end
  else
      FUpsampleShader[False] := TGLSLShaderProgram.CreateFrom
      (FUpsampleShaderSource[False]);
  FUpsampleShader[False].LinkShader;
  WriteLn(FUpsampleShader[False].Log);

  FUpShadersCompiled :=
    not FUpsampleShader[True].Error and
    not FUpsampleShader[False].Error;
  Result := FUpShadersCompiled;
end;

procedure TGLSynthesizer.CopyToReadSynthTexture(const aLevel: integer);
begin
  if GL_ARB_copy_image then
  begin
    glCopyImageSubData(
      FLevels[aLevel].PatchesTextureId,
      GL_TEXTURE_2D,
      0, 0, 0, 0,
      FReadSynthTextureId,
      GL_TEXTURE_2D,
      0, 0, 0, 0,
      FSideSize, FSideSize, 1);
  end
  else
  begin
{   // Raw copy method, that requires addidition memory
    // do not know which one is faster
    FCopyBuffer.Bind;
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, FCopyBuffer.Id);
    glGetTextureImageEXT(FLevels[aLevel].PatchesTextureId, GL_TEXTURE_2D,
      0, GL_RG_INTEGER, GL_SHORT, nil);
    glTextureSubImage2DEXT(FReadSynthTextureId, GL_TEXTURE_2D,
      0, 0, 0, FSideSize, FSideSize, GL_RG_INTEGER, GL_SHORT, nil);
    FCopyBuffer.UnBindBuffer;
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);  }

    FCopyImageShader.Apply;
    FCopyImageShader.SetUniform('offsets', ZERO_OFFSETS);
    // Source
    glActiveTexture(GL_TEXTURE7);
    glBindTexture(GL_TEXTURE_2D, FLevels[aLevel].PatchesTextureId);
    // Destination
    glBindImageTexture(1, FReadSynthTextureId, 0, False, 0,
      GL_WRITE_ONLY, GL_RG16I);
    glDispatchCompute(FWorkGroupCount[0] * 2, FWorkGroupCount[1] * 2, 1);
    glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
    FCorrectionShader.Apply;
  end;
end;

constructor TGLSynthesizer.CreateFrom(const anAnalysisData: TAnalysisData);
begin
  Create;
  Assert(Assigned(anAnalysisData));
  FAnalysisData := anAnalysisData;
  FSideSize := 256 + 2 * PADDING_BORDER;
  FHalfSideSize := FSideSize div 2;
  FKappa := 1.0;
  FCorrectionSubpassesCount := 2;
  FLODFromDownLevels := True;
end;

procedure TGLSynthesizer.CreateRandomTexture;
var
  i: integer;
  p: PByte;
begin
  GetMem(p, FSideSize * FSideSize * 2);
  for i := 0 to FSideSize * FSideSize * 2 - 1 do
  begin
    p^ := round(255 * random);
    Inc(p);
  end;
  Dec(p, FSideSize * FSideSize * 2);
  glGenTextures(1, @FRandomTextureId);
  glBindTexture(GL_TEXTURE_2D, FRandomTextureId);
  glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP_SGIS, GL_FALSE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RG8, FSideSize, FSideSize, 0,
    GL_RG, GL_UNSIGNED_BYTE, p);
  FreeMem(p);
end;

procedure TGLSynthesizer.DefineLevelStatus;
var
  L, size: integer;
  offset, curr: vec2i;
begin
  // Determine levels status
  size := FSideSize - 2 * PADDING_BORDER;
  for L := High(FLevels) downto 0 do
  begin
    FLevels[L].CoverLimit := [];
    if L < FDest.ImageDescriptor.Levels then
    begin
      if FDest.ImageDescriptor.LODS[L].Width >= size then
      begin
        Include(FLevels[L].CoverLimit, clmWidthLimit);
        FLevels[L].DestinationOffset[0] := (FDest.ImageDescriptor.LODS[L].Width - FSideSize) div 2 + PADDING_BORDER;
      end;
      if FDest.ImageDescriptor.LODS[L].Height >= size then
      begin
        Include(FLevels[L].CoverLimit, clmHeightLimit);
        FLevels[L].DestinationOffset[1] := (FDest.ImageDescriptor.LODS[L].Height - FSideSize) div 2 + PADDING_BORDER;
      end;
    end;
  end;

  for L := High(FLevels) downto 0 do
  with FLevels[L] do
  begin
    if FLevels[L].CoverLimit <> [] then
    begin
      offset[0] := -TMath.Ceil(DestinationOffset[0] / PADDING_BORDER)
        * PADDING_BORDER div 2;
      offset[1] := -TMath.Ceil(DestinationOffset[1] / PADDING_BORDER)
        * PADDING_BORDER div 2;
      DoShift(L, offset);
      curr := GetRelativeOffset(L);
      offset[0] :=  RelativeOffset[0] - curr[0];
      offset[1] :=  RelativeOffset[1] - curr[1];
      DestinationOffset[0] := DestinationOffset[0] - offset[0];
      DestinationOffset[1] := DestinationOffset[1] - offset[1];
      RelativeOffset := curr;
      State := stFullOutdated;
    end;
  end;

  ResetConstructionProgress;
end;

destructor TGLSynthesizer.Destroy;
begin
  Assert(not FInitialized);
  if Assigned(FUpsampleShaderSource[False]) and
    (FUpsampleShaderSource[False].Owner = Self) then
      FreeAndNil(FUpsampleShaderSource[False]);
  if Assigned(FUpsampleShaderSource[True]) and
    (FUpsampleShaderSource[True].Owner = Self) then
      FreeAndNil(FUpsampleShaderSource[True]);
  if Assigned(FCorrectionShaderSource)
    and (FCorrectionShaderSource.Owner = Self) then
      FreeAndNil(FCorrectionShaderSource);
  DestinationTexture := nil;
  inherited;
end;

procedure TGLSynthesizer.DoConstruct(const aLevel: integer);
var
  w, h, gj, gi: integer;
  v: vec4i;
begin
  FImageConstructShader.Apply;
  glBindImageTexture(0, FDest.Id, aLevel, False, 0, GL_WRITE_ONLY, GL_RGBA8);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, FLevels[aLevel].ExemplarTextureId);
  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, FLevels[aLevel].PatchesTextureId);
  w := FDest.ImageDescriptor.LODS[aLevel].Width;
  h := FDest.ImageDescriptor.LODS[aLevel].Height;

  if clmWidthLimit in FLevels[aLevel].CoverLimit then
  begin
    v[0] := PADDING_BORDER;
    v[2] := FLevels[aLevel].DestinationOffset[0];
    gj := TMath.Max(1, (FSideSize - 2 * PADDING_BORDER) div WORKGROUP_SIZE);
  end
  else begin
    v[0] := TMath.Max((FSideSize - w) div 2, 0);
    v[2] := 0;
    gj := TMath.Max(1, w div WORKGROUP_SIZE);
  end;

  if clmHeightLimit in FLevels[aLevel].CoverLimit then
  begin
    v[1] := PADDING_BORDER;
    v[3] := FLevels[aLevel].DestinationOffset[1];
    gi := TMath.Max(1, (FSideSize - 2 * PADDING_BORDER) div WORKGROUP_SIZE);
  end
  else begin
    v[1] := TMath.Max((FSideSize - h) div 2, 0);
    v[3] := 0;
    gi := TMath.Max(1, h div WORKGROUP_SIZE);
  end;

  FImageConstructShader.SetUniform('offsets', v);

  glDispatchCompute(gj, gi, 1);
  glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
  FLevels[aLevel].State := stCompleted;
end;

procedure TGLSynthesizer.DoCorrection(const aLevel: integer;
  const aRect: Vec4i);
var
  v2i: Vec2i;
  v6: TVector6f;
  pv: ^Vec3;
  subPass: integer;
begin
  with FCorrectionShader do
  begin
    Apply;

    // Exemplar texture for gathering neighbor
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, FLevels[aLevel].ExemplarTextureId);

    // Source
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, FReadSynthTextureId);

    // Most similar
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, FLevels[aLevel].kNearestTextureId);

    // Neihgborhoods
    glActiveTexture(GL_TEXTURE3);
    glBindTexture(GL_TEXTURE_2D, FLevels[aLevel].NeihgbPart1TextureId);
    glActiveTexture(GL_TEXTURE4);
    glBindTexture(GL_TEXTURE_2D, FLevels[aLevel].NeihgbPart2TextureId);

    // Destination
    glBindImageTexture(0, FLevels[aLevel].PatchesTextureId, 0, False, 0,
      GL_WRITE_ONLY, GL_RG16I);

    FLevels[aLevel].NeihgbPCAMatrixBuffer.BindAllRange(0);

    // Spacing exemplar coordinates for level
    v2i[0] := 1 shl aLevel;
    v2i[1] := v2i[0];
    SetUniform('spacing', v2i);

    v6 := FAnalysisData.Levels[aLevel].NeighbScale;
    pv := @v6[0];
    SetUniform('NeighbScale1', pv^);
    pv := @v6[3];
    SetUniform('NeighbScale2', pv^);

    v6 := FAnalysisData.Levels[aLevel].NeighbOffset;
    pv := @v6[0];
    SetUniform('NeighbOffset1', pv^);
    pv := @v6[3];
    SetUniform('NeighbOffset2', pv^);

    SetUniform('Kappa', FKappa);

    case FAnalysisData.EdgePolicy of
      epNonRepeat: SetSubroutine('synthWrapping', 'wrapMirrorRepeat');
      epRepeat: SetSubroutine('synthWrapping', 'wrapRepeat');
      epNonRepeatDbl:;
      epRepeatDbl:;
    end;

    for subPass := 0 to FCorrectionSubpassesCount * 4 - 1 do
    begin
      CopyToReadSynthTexture(aLevel);
      v2i := STEPS[subPass and 7];
      v2i[0] := v2i[0] + aRect[0];
      v2i[1] := v2i[1] + aRect[1];
      SetUniform('subPassOffset', v2i);
      glDispatchCompute(
        TMath.Ceil(aRect[2] / WORKGROUP_SIZE),
        TMath.Ceil(aRect[3] / WORKGROUP_SIZE),
        1);
      glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
    end;

    FLevels[aLevel].NeihgbPCAMatrixBuffer.UnBindBuffer;
  end;
end;

procedure TGLSynthesizer.DoCorrection(const aLevel: integer);
var
  iv: Vec2i;
  v6: TVector6f;
  pv: ^Vec3;
  subPass: integer;
begin
  with FCorrectionShader do
  begin
    Apply;

    // Exemplar texture for gathering neighbor
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, FLevels[aLevel].ExemplarTextureId);

    // Source
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, FReadSynthTextureId);

    // Most similar
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, FLevels[aLevel].kNearestTextureId);

    // Neihgborhoods
    glActiveTexture(GL_TEXTURE3);
    glBindTexture(GL_TEXTURE_2D, FLevels[aLevel].NeihgbPart1TextureId);
    glActiveTexture(GL_TEXTURE4);
    glBindTexture(GL_TEXTURE_2D, FLevels[aLevel].NeihgbPart2TextureId);

    // Destination
    glBindImageTexture(0, FLevels[aLevel].PatchesTextureId, 0, False, 0,
      GL_WRITE_ONLY, GL_RG16I);

    FLevels[aLevel].NeihgbPCAMatrixBuffer.BindAllRange(0);

    // Spacing exemplar coordinates for level
    iv[0] := 1 shl aLevel;
    iv[1] := iv[0];
    SetUniform('spacing', iv);

    v6 := FAnalysisData.Levels[aLevel].NeighbScale;
    pv := @v6[0];
    SetUniform('NeighbScale1', pv^);
    pv := @v6[3];
    SetUniform('NeighbScale2', pv^);

    v6 := FAnalysisData.Levels[aLevel].NeighbOffset;
    pv := @v6[0];
    SetUniform('NeighbOffset1', pv^);
    pv := @v6[3];
    SetUniform('NeighbOffset2', pv^);

    SetUniform('Kappa', FKappa);

    case FAnalysisData.EdgePolicy of
      epNonRepeat: SetSubroutine('synthWrapping', 'wrapMirrorRepeat');
      epRepeat: SetSubroutine('synthWrapping', 'wrapRepeat');
      epNonRepeatDbl:;
      epRepeatDbl:;
    end;

    for subPass := 0 to FCorrectionSubpassesCount * 4 - 1 do
    begin
      CopyToReadSynthTexture(aLevel);
      SetUniform('subPassOffset', STEPS[subPass and 7]);
      glDispatchCompute(FWorkGroupCount[0], FWorkGroupCount[1], 1);
      glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
    end;

    FLevels[aLevel].NeihgbPCAMatrixBuffer.UnBindBuffer;
  end;
end;

procedure TGLSynthesizer.DoShift(const aLevel: integer; const aStep: Vec2i);

  function DivStep(A, S: integer): integer;
  begin
    Result := A div 2;
    if Sign(Result) <> S then
      Result := S;
  end;

var
  downSize, L: integer;
  newOffset, downStep, singStep, upStep: vec2i;
begin
  downSize := FSideSize div 2 + PADDING_BORDER;
  newOffset[0] := FLevels[aLevel].ParentOffset[0] + aStep[0];
  newOffset[1] := FLevels[aLevel].ParentOffset[1] + aStep[1];

  // checking approach to borders
  if (newOffset[0] < PADDING_BORDER) or (newOffset[0] + downSize > FSideSize) or
    (newOffset[1] < PADDING_BORDER) or (newOffset[1] + downSize > FSideSize) then
  begin
    singStep[0] := Sign(aStep[0]);
    singStep[1] := Sign(aStep[1]);
    downStep[0] := DivStep(aStep[0], singStep[0]);
    downStep[1] := DivStep(aStep[1], singStep[1]);
    upStep[0] := aStep[0] - 2 * downStep[0];
    upStep[1] := aStep[1] - 2 * downStep[1];
    if (upStep[0] <> 0) or (upStep[1] <> 0) then
    begin
      for L := aLevel downto 0 do
        with FLevels[L] do begin
          if State > stPartOutdated then
            State := stPartOutdated;
          ParentOffset[0] := ParentOffset[0] + upStep[0];
          ParentOffset[1] := ParentOffset[1] + upStep[1];
          upStep[0] := upStep[0] * 2;
          upStep[1] := upStep[1] * 2;
        end;
    end;

    if aLevel < High(FLevels) then
      DoShift(aLevel + 1, downStep);
  end
  else
    FLevels[aLevel].ParentOffset := newOffset;

  if FLevels[aLevel].State > stPartOutdated then
    FLevels[aLevel].State := stPartOutdated;
end;

procedure TGLSynthesizer.DoUpsample(const aLevel: integer; const aRect: Vec4i);
var
  first: Boolean;
  v2: Vec2;
  v2i: Vec2i;
  v4i: Vec4i;
begin
  first := aLevel = High(FLevels);
  with FUpsampleShader[first] do
  begin
    Apply;

    // Child level to write
    glBindImageTexture(0, FLevels[aLevel].PatchesTextureId, 0, False, 0,
      GL_WRITE_ONLY, GL_RG16I);

    // Random texture
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, FRandomTextureId);

    if FJitterModulation then
    begin
      glActiveTexture(GL_TEXTURE2);
      glBindTexture(GL_TEXTURE_2D, FLevels[aLevel].ExemplarTextureId);
    end;

    v4i[2] := 2 * aRect[0];
    v4i[3] := 2 * aRect[1];
    if first then
    begin
      v2i[0] := FAnalysisData.Exemplar.Width div 2;
      v2i[1] := FAnalysisData.Exemplar.Height div 2;
      SetUniform('baseCoords', v2i);
    end
    else
    begin
      // Parent level to read
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, FLevels[aLevel + 1].PatchesTextureId);

      // Parent level's quarter offset which upsampled to child
      v4i[0] := FLevels[aLevel].ParentOffset[0] + aRect[0];
      v4i[1] := FLevels[aLevel].ParentOffset[1] + aRect[1];
    end;
    SetUniform('offsets', v4i);

    // Coordinates shift for random texture
    v4i := FLevels[aLevel].RandScaleOffset;
    v2i := GetRelativeOffset(aLevel);
    v4i[2] := v4i[2] + v2i[0];
    v4i[3] := v4i[3] + v2i[1];
    SetUniform('randScaleOffset', v4i);

    // Jitter strength
    if FJitterModulation then
    begin
      v2[0] := FLevels[aLevel].JitterStrength[0] * INV255;
      v2[1] := FLevels[aLevel].JitterStrength[1] * INV255;
      SetUniform('strength', v2);
    end
    else SetUniform('strength', FLevels[aLevel].JitterStrength);

    // Spacing exemplar coordinates for level
    SetUniform('spacing', FLevels[aLevel].Spacing[0], 3);

    glDispatchCompute(
      TMath.Ceil(aRect[2] / WORKGROUP_SIZE),
      TMath.Ceil(aRect[3] / WORKGROUP_SIZE),
      1);
    glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
  end;
end;

procedure TGLSynthesizer.DoUpsample(const aLevel: integer);
var
  first: Boolean;
  v2: Vec2;
  v2i: Vec2i;
  v4i: Vec4i;
begin
  first := aLevel = High(FLevels);
  with FUpsampleShader[first] do
  begin
    Apply;

    // Child level to write
    glBindImageTexture(0, FLevels[aLevel].PatchesTextureId, 0, False, 0,
      GL_WRITE_ONLY, GL_RG16I);

    // Random texture
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, FRandomTextureId);

    if FJitterModulation then
    begin
      glActiveTexture(GL_TEXTURE2);
      glBindTexture(GL_TEXTURE_2D, FLevels[aLevel].ExemplarTextureId);
    end;

    if first then
    begin
      v2i[0] := FAnalysisData.Exemplar.Width div 2;
      v2i[1] := FAnalysisData.Exemplar.Height div 2;
      SetUniform('baseCoords', v2i);
      v4i := ZERO_OFFSETS;
    end
    else
    begin
      // Parent level to read
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, FLevels[aLevel + 1].PatchesTextureId);

      // Parent level's quarter offset which upsampled to child
      v4i[0] := FLevels[aLevel].ParentOffset[0];
      v4i[1] := FLevels[aLevel].ParentOffset[1];
      v4i[2] := 0;
      v4i[3] := 0;
    end;
    SetUniform('offsets', v4i);

    // Coordinates shift for random texture
    v4i := FLevels[aLevel].RandScaleOffset;
    v2i := GetRelativeOffset(aLevel);
    v4i[2] := v4i[2] + v2i[0];
    v4i[3] := v4i[3] + v2i[1];
    SetUniform('randScaleOffset', v4i);

    // Jitter strength
    if FJitterModulation then
    begin
      v2[0] := FLevels[aLevel].JitterStrength[0] * INV255;
      v2[1] := FLevels[aLevel].JitterStrength[1] * INV255;
      SetUniform('strength', v2);
    end
    else SetUniform('strength', FLevels[aLevel].JitterStrength);

    // Spacing exemplar coordinates for level
    SetUniform('spacing', FLevels[aLevel].Spacing[0], 3);

    glDispatchCompute(FWorkGroupCount[0], FWorkGroupCount[1], 1);
    glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
  end;
end;

procedure TGLSynthesizer.Finalize;
var
  L: integer;
begin
  Assert(FInitialized);
  glDeleteTextures(1, @FReadSynthTextureId);
  glDeleteTextures(1, @FRandomTextureId);
  for L := 0 to High(FLevels) do
  begin
    glDeleteTextures(5, @FLevels[L].ExemplarTextureId);
    FLevels[L].NeihgbPCAMatrixBuffer.Destroy;
  end;
  SetLength(FLevels, 0);
  FreeAndNil(FUpsampleShader[True]);
  FreeAndNil(FUpsampleShader[False]);
  FreeAndNil(FCorrectionShader);
  FreeAndNil(FCopyImageShader);
//  FreeAndNil(FCopyBuffer);
  FInitialized := False;
end;

function TGLSynthesizer.GetExemplarTextureID(const Level: integer): GLuint;
begin
  Result := FLevels[Level].ExemplarTextureId;
end;

function TGLSynthesizer.GetRelativeOffset(const aLevel: integer): Vec2i;
var
  L, R: integer;
begin
  Result[0] := 0;
  Result[1] := 0;
  R := 2;
  for L := aLevel to High(FLevels) do
  begin
    Result[0] := Result[0] + FLevels[L].ParentOffset[0] * R;
    Result[1] := Result[1] + FLevels[L].ParentOffset[1] * R;
    R := R shl 1;
  end;
end;

function TGLSynthesizer.GetJitter(const Level: integer): single;
begin
  Result := FLevels[Level].JitterStrength[0];
end;

function TGLSynthesizer.GetLevelCount: integer;
begin
  Result := Length(FLevels);
end;

function TGLSynthesizer.GetTextureID(const Level: integer): GLuint;
begin
  Result := FLevels[Level].PatchesTextureId;
end;

function TGLSynthesizer.GetUpsampleShaderSource(
  const Starter: Boolean): TShaderProgram;
begin
  Result := FUpsampleShaderSource[Starter];
end;

procedure TGLSynthesizer.Initialize;
var
  L, LL, R, Spacing, i, j, k, qsize: integer;
  img: TImageDesc;
  wgs, wgc: vec3i;
  M: pointer;
  pM: PSingle;
  shader: TShaderProgram;
begin
  Assert(Supported);
  if FInitialized then
      Finalize;

  if not FAnalysisData.IsValid then
  begin
    // There must be error message
    Exit;
  end;

  wgs := GetWorkgroupSize;
  if (wgs[0] < WORKGROUP_SIZE) or (wgs[1] < WORKGROUP_SIZE) then
  begin
    // There must be error message
    Exit;
  end;
  FWorkGroupCount[0] := FSideSize div (2 * WORKGROUP_SIZE);
  FWorkGroupCount[1] := FSideSize div (2 * WORKGROUP_SIZE);
  wgc := GetWorkgroupCount;
  if (2 * FWorkGroupCount[0] > wgc[0]) or (2 * FWorkGroupCount[1] > wgc[1]) then
  begin
    // There must be error message
    Exit;
  end;

  CreateRandomTexture;

  // Read buffer
  glGenTextures(1, @FReadSynthTextureId);
  glBindTexture(GL_TEXTURE_2D, FReadSynthTextureId);
  glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP_SGIS, GL_FALSE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RG16I, FSideSize, FSideSize, 0,
    GL_RG_INTEGER, GL_SHORT, nil);
  // glTextureStorage2DEXT(FReadSynthTextureId, GL_TEXTURE_2D, 1,
  // GL_RG16I, FSideSize, FSideSize);

  GetMem(M, SizeOf(TNeighbPCAmatrix));

  qsize := FSideSize div 4;

  SetLength(FLevels, FAnalysisData.LevelsAmount - 1);
  for L := 0 to High(FLevels) do
  begin
    // Spacing exemplar coordinates for level
    Spacing := 1 shl L;
    FLevels[L].Spacing[0][0] := Spacing;
    FLevels[L].Spacing[0][1] := 0;
    FLevels[L].Spacing[1][0] := 0;
    FLevels[L].Spacing[1][1] := Spacing;
    FLevels[L].Spacing[2][0] := Spacing;
    FLevels[L].Spacing[2][1] := Spacing;

    glGenTextures(5, @FLevels[L].ExemplarTextureId);
    // Gaussian level of exemplar
    img := FAnalysisData.Levels[L].Image;
    glBindTexture(GL_TEXTURE_2D, FLevels[L].ExemplarTextureId);
    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP_SGIS, GL_FALSE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexImage2D(GL_TEXTURE_2D, 0, img.InternalFormat, img.Width, img.Height, 0,
      img.ColorFormat, img.DataType, img.Data);

    // Empty patches level
    glBindTexture(GL_TEXTURE_2D, FLevels[L].PatchesTextureId);
    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP_SGIS, GL_FALSE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RG16I, FSideSize, FSideSize, 0,
      GL_RG_INTEGER, GL_SHORT, nil);
    // glTextureStorage2DEXT(FReadSynthTextureId, GL_TEXTURE_2D, 1,
    // GL_RG16I, FSideSize, FSideSize);

    // Most similar
    img := FAnalysisData.Levels[L].kNearest;
    glBindTexture(GL_TEXTURE_2D, FLevels[L].kNearestTextureId);
    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP_SGIS, GL_FALSE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, img.InternalFormat, img.Width, img.Height, 0,
      img.ColorFormat, img.DataType, img.Data);

    // Neihgborhoods in two texture
    img := FAnalysisData.Levels[L].Neighborhoods[0];
    glBindTexture(GL_TEXTURE_2D, FLevels[L].NeihgbPart1TextureId);
    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP_SGIS, GL_FALSE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, img.InternalFormat, img.Width, img.Height, 0,
      img.ColorFormat, img.DataType, img.Data);
    img := FAnalysisData.Levels[L].Neighborhoods[1];
    glBindTexture(GL_TEXTURE_2D, FLevels[L].NeihgbPart2TextureId);
    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP_SGIS, GL_FALSE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, img.InternalFormat, img.Width, img.Height, 0,
      img.ColorFormat, img.DataType, img.Data);

    // PCA matrix of neihgborhoods level
    pM := M;
    for i := 0 to 5 do
      for j := 0 to NEIGHBOUR_SIZE_1COLOR - 1 do
        with FAnalysisData.Levels[L]^ do
        begin
          k := 4 * j;
          pM[0] := NeihgbPCAMatrix[k + 0, i];
          pM[1] := NeihgbPCAMatrix[k + 1, i];
          pM[2] := NeihgbPCAMatrix[k + 2, i];
          pM[3] := 0;
          Inc(pM, 4);
        end;
    FLevels[L].NeihgbPCAMatrixBuffer := TGLBufferObject.Create(btUniform);
    FLevels[L].NeihgbPCAMatrixBuffer.Allocate(SizeOf(TNeighbPCAmatrix), M,
      GL_STATIC_READ);

    FLevels[L].RandScaleOffset[0] := round(random * FSideSize);
    FLevels[L].RandScaleOffset[1] := round(random * FSideSize);
    FLevels[L].RandScaleOffset[2] := round(random * FSideSize);
    FLevels[L].RandScaleOffset[3] := round(random * FSideSize);

    FLevels[L].ParentOffset[0] := qsize;
    FLevels[L].ParentOffset[1] := qsize;

    FLevels[L].RelativeOffset[0] := 0;
    FLevels[L].RelativeOffset[1] := 0;
    R := FHalfSideSize;
    for LL := L to High(FLevels) do
    begin
      FLevels[L].RelativeOffset[0] := FLevels[L].RelativeOffset[0] + R;
      FLevels[L].RelativeOffset[1] := FLevels[L].RelativeOffset[1] + R;
      R := R shl 1;
    end;

    FLevels[L].DestinationOffset[0] := 0;
    FLevels[L].DestinationOffset[1] := 0;

    FLevels[L].State := stFullOutdated;
    FLevels[L].CoverLimit := [];
  end;

  CompileUpsampleJitterShaders;

  if not Assigned(FCorrectionShaderSource) then
  begin
    shader := SynthesisShaderGenerator.GenCorrectionShader(Self);
    FCorrectionShader := TGLSLShaderProgram.CreateFrom(shader);
    shader.Destroy;
  end
  else
      FCorrectionShader := TGLSLShaderProgram.CreateFrom
      (FCorrectionShaderSource);
  FCorrectionShader.LinkShader;
  WriteLn(FCorrectionShader.Log);

  if not GL_ARB_copy_image then
  begin
    shader := SynthesisShaderGenerator.GenCopyImageShader(Self);
    FCopyImageShader := TGLSLShaderProgram.CreateFrom(shader);
    FCopyImageShader.LinkShader;
    WriteLn(FCopyImageShader.Log);
    shader.Destroy;
  end;

  shader := SynthesisShaderGenerator.GenImageConstructShader(Self);
  FImageConstructShader := TGLSLShaderProgram.CreateFrom(shader);
  FImageConstructShader.LinkShader;
  WriteLn(FImageConstructShader.Log);
  shader.Destroy;

{
  FCopyBuffer := TGLBufferObject.Create(btPixelPack);
  FCopyBuffer.Allocate(FSideSize*FSideSize*2*SizeOf(Word), nil, GL_DYNAMIC_COPY);
}

  FreeMem(M);

  if Assigned(FDest) then
    DefineLevelStatus;

  FInitialized := FUpShadersCompiled and not FCorrectionShader.Error and
    not FImageConstructShader.Error;
end;

procedure TGLSynthesizer.Notify(Sender: TObject; Msg: Cardinal;
  Params: pointer);
begin
  if Sender = FAnalysisData then
  begin
    if Msg = NM_ObjectDestroyed then
    begin
      FAnalysisData := nil;
      Assert(False, 'Analysis data destroyed before class-user destruction');
    end
    else if Msg = NM_ResourceChanged then
    begin
      NotifyLevelChanged(High(FLevels));
    end;
  end
  else if Sender = FDest then
  begin
    if Msg = NM_ObjectDestroyed then
    begin
      FDest := nil;
    end
    else if Msg = NM_ResourceChanged then
    begin
      NotifyLevelChanged(High(FLevels));
    end;
  end;
end;

procedure TGLSynthesizer.NotifyLevelChanged(const aLevel: integer);
var
  L: integer;
begin
  for L := 0 to aLevel do
      FLevels[L].State := stFullOutdated;
  ResetConstructionProgress;
  DispatchMessage(NM_ResourceChanged);
end;

procedure TGLSynthesizer.Process;
const
  CONSTR_STEPS: array[TConstructPhase] of Vec2i = (
    (4, 0), (0, 4), (-4, 0), (0, 4));
var
  L, LevelsNum: integer;
  changed: Boolean;
  curr, diff: Vec2i;
  rect: Vec4i;

  procedure ApplyOffset;
  begin
    with FLevels[L] do
    begin
      State := stChanged;
      curr := GetRelativeOffset(L);
      diff[0] :=  RelativeOffset[0] - curr[0];
      diff[1] :=  RelativeOffset[1] - curr[1];
      if clmWidthLimit in CoverLimit then
        DestinationOffset[0] := DestinationOffset[0] - diff[0];
      if clmHeightLimit in CoverLimit then
        DestinationOffset[1] := DestinationOffset[1] - diff[1];
      RelativeOffset := curr;
    end;
  end;

  procedure FullUpdate;
  begin
    DoUpsample(L);
    if FCorrectionSubpassesCount > 0 then
      DoCorrection(L);
    ApplyOffset;
    changed := True;
  end;

  procedure PartialUpdate;
  begin
    DoUpsample(L, rect);
    if FCorrectionSubpassesCount > 0 then
    begin
      rect[0] := 2 * rect[0];
      rect[1] := 2 * rect[1];
      DoCorrection(L, rect);
    end;
  end;

begin
  Assert(FInitialized);

  if not FUpShadersCompiled then
    if not CompileUpsampleJitterShaders then Exit;

  changed := False;

  for L := LevelCount - 1 downto 0 do
  begin
    if FLevels[L].State in [stFullOutdated] then
    begin
      FullUpdate;
    end
    else if FLevels[L].State = stPartOutdated then
    begin
      changed := True;
      ApplyOffset;
      if (Abs(diff[0]) >= FSideSize) or (Abs(diff[1]) >= FSideSize) then
      begin
        FullUpdate;
        continue;
      end;
      DoScroll(L, diff);

      diff[0] := diff[0] div 2;
      diff[1] := diff[1] div 2;

      if diff[0] <> 0 then
      begin
        // Horizontal stripe update
        if diff[0] > 0 then
        begin
          rect[0] := 0;
          rect[2] := diff[0];
        end else begin
          rect[0] := WORKGROUP_SIZE * ((FHalfSideSize + diff[0]) div WORKGROUP_SIZE);
          rect[2] := FHalfSideSize - rect[0];
        end;
        rect[1] := 0;
        rect[3] := WORKGROUP_SIZE * FWorkGroupCount[1];
        PartialUpdate;

        if diff[1] <> 0 then
        begin
          // Vertical remain stripe update
          if diff[1] > 0 then
          begin
            rect[1] := 0;
            rect[3] := diff[1];
          end else begin
            rect[1] := WORKGROUP_SIZE * ((FHalfSideSize + diff[1]) div WORKGROUP_SIZE);
            rect[3] := FHalfSideSize - rect[1];
          end;
          if diff[0] > 0 then
          begin
            rect[0] := WORKGROUP_SIZE * (diff[0] div WORKGROUP_SIZE);
            rect[2] := FHalfSideSize - rect[0];
          end else begin
            rect[0] := 0;
            rect[2] := FHalfSideSize + diff[0];
          end;
          PartialUpdate;
        end;
      end
      else if diff[1] <> 0 then
      begin
        // Vertical stripe update
        if diff[1] > 0 then
        begin
          rect[1] := 0;
          rect[3] := diff[1];
        end else begin
          rect[1] := WORKGROUP_SIZE * ((FHalfSideSize + diff[1]) div WORKGROUP_SIZE);
          rect[3] := FHalfSideSize - rect[1];
        end;
        rect[0] := 0;
        rect[2] := WORKGROUP_SIZE * FWorkGroupCount[0];
        PartialUpdate;
      end;
    end;
  end;

  // Construct sunthesized image
  changed := changed or (FConstructState = cstInProgress);
  if Assigned(FDest) and changed then
  begin
    if FLODFromDownLevels then
    begin
      LevelsNum := TMath.Min(LevelCount, FDest.ImageDescriptor.Levels);
      for L := 0 to LevelsNum - 1 do
        if FLevels[L].State = stChanged then
          DoConstruct(L);
    end
    else
    begin
      if FLevels[0].State = stChanged then
        DoConstruct(0);
    end;

    if FConstructState = cstInProgress then
    begin
      case FConstructPhase of
        cphForward: begin
          if FLevels[0].DestinationOffset[0] + FSideSize - 2 * PADDING_BORDER + 8 > FDest.ImageDescriptor.Width then
          begin
            Inc(FConstructPhase);
            if FLevels[0].DestinationOffset[1] + FSideSize - 2 * PADDING_BORDER + 8 > FDest.ImageDescriptor.Height then
              FConstructState := cstCompleted
          end
          else DoShift(0, CONSTR_STEPS[FConstructPhase]);
        end;
        cphUpRight: begin
          if FLevels[0].DestinationOffset[1] + FSideSize - 2 * PADDING_BORDER + 8 > FDest.ImageDescriptor.Height then
            Inc(FConstructPhase)
          else begin
            DoShift(0, CONSTR_STEPS[FConstructPhase]);
            if (FLevels[0].DestinationOffset[1] + 8) mod (FSideSize - 2 * PADDING_BORDER) = 0 then
              Inc(FConstructPhase);
          end;
        end;
        cphBack: begin
          if FLevels[0].DestinationOffset[0] - 8 < 0 then
          begin
            Inc(FConstructPhase);
            if FLevels[0].DestinationOffset[1] + FSideSize - 2 * PADDING_BORDER + 8 > FDest.ImageDescriptor.Height then
              FConstructState := cstCompleted;
          end
          else DoShift(0, CONSTR_STEPS[FConstructPhase]);
        end;
        cphUpLeft: begin
          if FLevels[0].DestinationOffset[1] + FSideSize - 2 * PADDING_BORDER + 8 > FDest.ImageDescriptor.Height then
            FConstructPhase := cphForward
          else begin
            DoShift(0, CONSTR_STEPS[FConstructPhase]);
            if (FLevels[0].DestinationOffset[1] + 8) mod (FSideSize - 2 * PADDING_BORDER) = 0 then
              FConstructPhase := cphForward;
          end;
        end;
      end;
      if (FConstructState = cstCompleted) and not FLODFromDownLevels then
        glGenerateTextureMipmapEXT(FDest.Id, GL_TEXTURE_2D);
    end;
  end;

  if FConstructState = cstBeginning then
  begin
    diff := FLevels[0].DestinationOffset;
    diff[0] := - diff[0] div 2;
    diff[1] := - diff[1] div 2;
    DoShift(0, diff);
    FConstructState := cstInProgress;
  end;
end;

procedure TGLSynthesizer.ResetConstructionProgress;
begin
  if (Length(FLevels) > 0) and (FLevels[0].CoverLimit <> []) then
  begin
    FConstructState := cstBeginning;
    FConstructPhase := cphForward;
  end
  else
    FConstructState := cstCompleted;
end;

procedure TGLSynthesizer.DoScroll(const aLevel: integer;
  const aStep: Vec2i);
var
  exId: GLuint;
  offsets: Vec4i;
  gw, gh: integer;
begin
  if aStep[0] < 0 then
  begin
    offsets[0] := -aStep[0];
    offsets[2] := 0;
  end
  else
  begin
    offsets[0] := 0;
    offsets[2] := aStep[0];
  end;

  if aStep[1] < 0 then
  begin
    offsets[1] := -aStep[1];
    offsets[3] := 0;
  end
  else
  begin
    offsets[1] := 0;
    offsets[3] := aStep[1];
  end;

  if GL_ARB_copy_image then
  begin
    glCopyImageSubData(
      FLevels[aLevel].PatchesTextureId,
      GL_TEXTURE_2D,
      0, offsets[0], offsets[1], 0,
      FReadSynthTextureId,
      GL_TEXTURE_2D,
      0, offsets[2], offsets[3], 0,
      FSideSize - Abs(aStep[0]), FSideSize - Abs(aStep[1]), 1);
  end
  else
  begin
    FCopyImageShader.Apply;
    FCopyImageShader.SetUniform('offsets', offsets);
    // Source
    glActiveTexture(GL_TEXTURE7);
    glBindTexture(GL_TEXTURE_2D, FLevels[aLevel].PatchesTextureId);
    // Destination
    glBindImageTexture(1, FReadSynthTextureId, 0, False, 0,
      GL_WRITE_ONLY, GL_RG16I);
    gw := TMath.Ceil( (FSideSize - Abs(aStep[0])) / WORKGROUP_SIZE );
    gh := TMath.Ceil( (FSideSize - Abs(aStep[1])) / WORKGROUP_SIZE );

    glDispatchCompute(gw, gh, 1);
    glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
  end;

  exId := FLevels[aLevel].PatchesTextureId;
  FLevels[aLevel].PatchesTextureId := FReadSynthTextureId;
  FReadSynthTextureId := exId;
end;

procedure TGLSynthesizer.SetCorrectionShaderSource(const Value: TShaderProgram);
begin
  if Assigned(FCorrectionShaderSource)
    and (FCorrectionShaderSource.Owner = Self) then
      FCorrectionShaderSource.Destroy;
  FCorrectionShaderSource := Value;
  NotifyLevelChanged(High(FLevels));
end;

procedure TGLSynthesizer.SetCorrectionSubpassesCount(const Value: integer);
begin
  if Value <> FCorrectionSubpassesCount then
  begin
    FCorrectionSubpassesCount := Value;
    NotifyLevelChanged(High(FLevels));
  end;
end;

procedure TGLSynthesizer.SetDestinationTexture(const Value: TGLTextureObject);
begin
  if Value <> FDest then
  begin
    if Assigned(FDest) then
        FDest.UnSubscribe(Self);
    FDest := Value;
    if Assigned(FDest) then
    begin
      FDest.Subscribe(Self);
      DefineLevelStatus;
    end;
  end;
end;

procedure TGLSynthesizer.SetJitter(const Level: integer; const Value: single);
begin
  if FLevels[Level].JitterStrength[0] <> Value then
  begin
    FLevels[Level].JitterStrength[0] := Value;
    FLevels[Level].JitterStrength[1] := Value;
    FLevels[Level].State := stFullOutdated;
    NotifyLevelChanged(Level);
  end;
end;

procedure TGLSynthesizer.SetJitterModulation(const Value: Boolean);
begin
  if FJitterModulation <> Value then
  begin
    FJitterModulation := Value;
    FUpShadersCompiled := False;
    NotifyLevelChanged(High(FLevels));
  end;
end;

procedure TGLSynthesizer.SetKappa(const Value: single);
begin
  if FKappa <> Value then
  begin
    FKappa := Value;
    NotifyLevelChanged(High(FLevels));
  end;
end;

procedure TGLSynthesizer.SetLODFromDownLevels(const Value: Boolean);
var
  L: integer;
begin
  if FLODFromDownLevels <> Value then
  begin
    FLODFromDownLevels := Value;
    for L := 0 to High(FLevels) do
      if FLevels[L].State  = stCompleted then
        FLevels[L].State := stChanged;
  end;
end;

procedure TGLSynthesizer.SetUpsampleShaderSource(const aStarter: Boolean;
  const Value: TShaderProgram);
begin
  if Assigned(FUpsampleShaderSource[aStarter])
    and (FUpsampleShaderSource[aStarter].Owner = Self) then
      FUpsampleShaderSource[aStarter].Destroy;
  FUpsampleShaderSource[aStarter] := Value;
  NotifyLevelChanged(High(FLevels));
end;

procedure TGLSynthesizer.Panning(const aDeltaX, aDeltaY: integer);
var
  step: vec2i;
begin
  if FInitialized then
  begin
    step[0] := TMath.Ceil(aDeltaX / 8) * 4;
    step[1] := TMath.Ceil(aDeltaY / 8) * 4;

    if Assigned(FDest) and (FLevels[0].CoverLimit <> []) then
    begin
      if clmWidthLimit in FLevels[0].CoverLimit then
        FLevels[0].DestinationOffset[0] := FLevels[0].DestinationOffset[0]  - 2 * step[0];

      if clmHeightLimit in FLevels[0].CoverLimit then
        FLevels[0].DestinationOffset[1] := FLevels[0].DestinationOffset[1]  - 2 * step[1];

      ResetConstructionProgress;
    end;

    DoShift(0, step);
  end;
end;

class function TGLSynthesizer.Supported: Boolean;
begin
  Result := GL_ARB_compute_shader and
    GL_ARB_shader_image_load_store and
    GL_ARB_shader_subroutine;
end;

{$ENDREGION}

end.
