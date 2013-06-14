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


type

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
    Changed: Boolean;
  end;

  TGLSynthesizer = class(TPersistentResource)
  private
    FAnalysisData: TAnalysisData;
    FSideSize: integer;
    FLevels: array of TGLSynthLevel;
    FUpsampleShaderSource: array [Boolean] of TShaderProgram;
    FCorrectionShaderSource: TShaderProgram;

    FInitialized: Boolean;
    FUpsampleShader: array [Boolean] of TGLSLShaderProgram;
    FCorrectionShader: TGLSLShaderProgram;
    FCopyImageShader: TGLSLShaderProgram;
    FGenImageShader: TGLSLShaderProgram;
    FWorkGroupCount: vec3i;

    FReadSynthTextureId: GLuint;
    FRandomTextureId: GLuint;
    FDestinationTextureId: GLuint;

    FCorrectionSubpassesCount: integer;
    FKappa: single;

    function GetLevelCount: integer;
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

  protected
    procedure NotifyLevelChanged(const aLevel: integer);
    procedure CreateRandomTexture;
    procedure DoUpsample(const aLevel: integer);
    procedure DoCorrection(const aLevel: integer);
    procedure CopyToReadSynthTexture(const aLevel: integer);
  public
    constructor CreateFrom(const anAnalysisData: TAnalysisData);
    destructor Destroy; override;

    procedure Notify(Sender: TObject; Msg: Cardinal;
      Params: pointer = nil); override;

    procedure Initialize;
    procedure Process;
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
    property CorrectionSubpassesCount: integer read FCorrectionSubpassesCount
      write SetCorrectionSubpassesCount;

    property DestinationTextureId: GLuint read FDestinationTextureId
      write FDestinationTextureId;
  end;

implementation

uses
  uMiscUtils,
  uImageSynthesisShaderGen;

{$REGION 'TGLSynthesizer'}


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
    FCopyImageShader.Apply;
    // Source
    glActiveTexture(GL_TEXTURE7);
    glBindTexture(GL_TEXTURE_2D, FLevels[aLevel].PatchesTextureId);
    // Destination
    glBindImageTexture(1, FReadSynthTextureId, 0, False, 0,
      GL_WRITE_ONLY, GL_RG16I);
    glDispatchCompute(FWorkGroupCount[0], FWorkGroupCount[1], 1);
    glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
    FCorrectionShader.Apply;
  end;
end;

constructor TGLSynthesizer.CreateFrom(const anAnalysisData: TAnalysisData);
begin
  Create;
  Assert(Assigned(anAnalysisData));
  FAnalysisData := anAnalysisData;
  FSideSize := 256;
  FKappa := 1.0;
  FCorrectionSubpassesCount := 2;
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
  inherited;
end;

procedure TGLSynthesizer.DoCorrection(const aLevel: integer);
const
  STEP: array [0 .. 7] of Vec2i = (
    (0, 0), (1, 1), (0, 1), (1, 0), (1, 1), (0, 0), (0, 1), (1, 0));
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
      SetUniform('subPassOffset', STEP[subPass and 7]);
      glDispatchCompute(FWorkGroupCount[0] div 2, FWorkGroupCount[1] div 2, 1);
      glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
    end;

    FLevels[aLevel].NeihgbPCAMatrixBuffer.UnBindBuffer;
  end;
end;

procedure TGLSynthesizer.DoUpsample(const aLevel: integer);
var
  first: Boolean;
  iv: Vec2i;
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

    if first then
    begin
      iv[0] := FAnalysisData.Exemplar.Width div 2;
      iv[1] := FAnalysisData.Exemplar.Height div 2;
      SetUniform('baseCoords', iv);
    end
    else
    begin
      // Parent level to read
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, FLevels[aLevel + 1].PatchesTextureId);

      // Parent quarter offset which upsampled to child
      iv[0] := FSideSize div 4;
      iv[1] := FSideSize div 4;
      SetUniform('quarter', iv);
    end;

    // Coordinates shift for random texture
    SetUniform('randScaleOffset', FLevels[aLevel].RandScaleOffset);

    // Jitter strength
    SetUniform('strength', FLevels[aLevel].JitterStrength);

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
  FInitialized := False;
end;

function TGLSynthesizer.GetExemplarTextureID(const Level: integer): GLuint;
begin
  Result := FLevels[Level].ExemplarTextureId;
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
  L, Spacing, i, j, k: integer;
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
  if (wgs[0] < 16) or (wgs[1] < 16) then
  begin
    // There must be error message
    Exit;
  end;
  FWorkGroupCount[0] := FSideSize div 16;
  FWorkGroupCount[1] := FSideSize div 16;
  wgc := GetWorkgroupCount;
  if (FWorkGroupCount[0] > wgc[0]) or (FWorkGroupCount[1] > wgc[1]) then
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

    FLevels[L].Changed := True;
  end;

  if not Assigned(FUpsampleShaderSource[True]) then
  begin
    shader := SynthesisShaderGenerator.GenUpsampleJitterShader(Self, True);
    FUpsampleShader[True] := TGLSLShaderProgram.CreateFrom(shader);
    shader.Destroy;
  end
  else
      FUpsampleShader[True] := TGLSLShaderProgram.CreateFrom
      (FUpsampleShaderSource[True]);
  FUpsampleShader[True].LinkShader;
  WriteLn(FUpsampleShader[True].Log);

  if not Assigned(FUpsampleShaderSource[False]) then
  begin
    shader := SynthesisShaderGenerator.GenUpsampleJitterShader(Self, False);
    FUpsampleShader[False] := TGLSLShaderProgram.CreateFrom(shader);
    shader.Destroy;
  end
  else
      FUpsampleShader[False] := TGLSLShaderProgram.CreateFrom
      (FUpsampleShaderSource[False]);
  FUpsampleShader[False].LinkShader;
  WriteLn(FUpsampleShader[False].Log);

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

  shader := SynthesisShaderGenerator.GenImageGenShader(Self);
  FGenImageShader := TGLSLShaderProgram.CreateFrom(shader);
  FGenImageShader.LinkShader;
  WriteLn(FCopyImageShader.Log);
  shader.Destroy;

  FreeMem(M);

  FInitialized := True;
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
  end;
end;

procedure TGLSynthesizer.NotifyLevelChanged(const aLevel: integer);
var
  L: integer;
begin
  for L := 0 to aLevel do
      FLevels[L].Changed := True;
  DispatchMessage(NM_ResourceChanged);
end;

procedure TGLSynthesizer.Process;
var
  i: integer;
begin
  Assert(FInitialized);
  for i := LevelCount - 1 downto 0 do
    if FLevels[i].Changed then
    begin
      DoUpsample(i);
      if FCorrectionSubpassesCount > 0 then
          DoCorrection(i);
      FLevels[i].Changed := False;
    end;
  glFinish;
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

procedure TGLSynthesizer.SetJitter(const Level: integer; const Value: single);
begin
  if FLevels[Level].JitterStrength[0] <> Value then
  begin
    FLevels[Level].JitterStrength[0] := Value;
    FLevels[Level].JitterStrength[1] := Value;
    FLevels[Level].Changed := True;
    NotifyLevelChanged(Level);
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

procedure TGLSynthesizer.SetUpsampleShaderSource(const aStarter: Boolean;
  const Value: TShaderProgram);
begin
  if Assigned(FUpsampleShaderSource[aStarter])
    and (FUpsampleShaderSource[aStarter].Owner = Self) then
      FUpsampleShaderSource[aStarter].Destroy;
  FUpsampleShaderSource[aStarter] := Value;
  NotifyLevelChanged(High(FLevels));
end;

class function TGLSynthesizer.Supported: Boolean;
begin
  Result := GL_ARB_compute_shader and
    GL_ARB_shader_image_load_store and
    GL_ARB_shader_subroutine;
end;

{$ENDREGION}

end.
