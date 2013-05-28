unit uGLImageSynthesis;

interface

uses
  uBaseTypes,
  uImageAnalysisClasses,
  dglOpenGL,
  uBaseGL,
  uRenderResource,
  uVMath;

type

  TGLSynthLevel = record
    ExemplarTextureId: GLuint;
    PatchesTextureId: GLuint;
    kNearestTextureId: GLuint;
    NeihgbPart1TextureId: GLuint;
    NeihgbPart2TextureId: GLuint;
    NeihgbPCAMatrixBuffer: TGLBufferObject;
    JitterStrength: single;
    Spacing: array [0 .. 2] of Vec2i;
  end;

  TGLSynthesizer = class
  private
    FAnalysisData: TAnalysisData;
    FSideSize: integer;
    FLevels: array of TGLSynthLevel;
    FUpsampleShaderSource: TShaderProgram;
    FCorrectionShaderSource: TShaderProgram;

    FInitialized: Boolean;
    FUpsampleShader: TGLSLShaderProgram;
    FCorrectionShader: TGLSLShaderProgram;
    FWorkGroupCount: vec3i;

    FRandomTextureId: GLuint;

    function GetLevelCount: integer;
    procedure SetCorrectionShaderSource(const Value: TShaderProgram);
    procedure SetUpsampleShaderSource(const Value: TShaderProgram);
    function GetTextureID(Level: integer): GLuint;

  protected
    procedure CreateRandomTexture;
    procedure DoUpsample(aLevel: integer);
  public
    constructor Create(anAnalysisData: TAnalysisData);
    destructor Destroy; override;

    procedure Initialize;
    procedure Process;
    procedure Finalize;
    class function Supported: Boolean;
    property Initialized: Boolean read FInitialized;

    // GL name of synthesized pathces texture
    property PachesTextureIDs[Level: integer]: GLuint read GetTextureID;
    // Return numder of synthesyzed levels
    property LevelCount: integer read GetLevelCount;
    // Return side size of synthesized texture
    property SideSize: integer read FSideSize;
    // External synthesis shaders
    property UpsampleShader: TShaderProgram read FUpsampleShaderSource
      write SetUpsampleShaderSource;
    property CorrectionShader: TShaderProgram read FCorrectionShaderSource
      write SetCorrectionShaderSource;
  end;

implementation

uses
  uMiscUtils,
  uImageSynthesisShaderGen;

{$REGION 'TGLSynthesizer'}


constructor TGLSynthesizer.Create(anAnalysisData: TAnalysisData);
begin
  Assert(Assigned(anAnalysisData));
  FAnalysisData := anAnalysisData;
  FSideSize := 256;
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
  if Assigned(FUpsampleShaderSource) and
    (FUpsampleShaderSource.Owner = Self) then
      FreeAndNil(FUpsampleShaderSource);
  if Assigned(FCorrectionShaderSource)
    and (FCorrectionShaderSource.Owner = Self) then
      FreeAndNil(FCorrectionShaderSource);
  inherited;
end;

procedure TGLSynthesizer.DoUpsample(aLevel: integer);
var
  iv: Vec2i;
begin
  with FUpsampleShader do
  begin
    Apply;
    // Parent level to read
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, FLevels[aLevel - 1].PatchesTextureId);

    // Child level to write
    glBindImageTexture(0, FLevels[aLevel].PatchesTextureId, 0, False, 0,
      GL_WRITE_ONLY, GL_RG16UI);

    // Random texture
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, FRandomTextureId);

    // Exemplar texture size
    iv[0] := FAnalysisData.Exemplar.Width;
    iv[1] := FAnalysisData.Exemplar.Height;
    SetUniform('exemplarSize', iv);

    // Size of synthesized texture
    iv[0] := FSideSize;
    iv[1] := FSideSize;
    SetUniform('synthSize', iv);

    // Parent quarter offset which upsampled to child
    iv[0] := FSideSize div 4;
    iv[1] := FSideSize div 4;
    SetUniform('quarter', iv);

    // Coordinates shift for random texture
    iv[0] := Round(Random * FSideSize);
    iv[1] := Round(Random * FSideSize);
    SetUniform('randOffeset', iv);

    // Jitter strength
    SetUniform('strength', FLevels[aLevel].JitterStrength);

    // Spacing exemplar coordinates for level
    SetUniform('spacing', FLevels[aLevel].Spacing[0], 3);

    glDispatchCompute(FWorkGroupCount[0], FWorkGroupCount[1], 1);
    glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
    UnApply;
  end;
end;

procedure TGLSynthesizer.Finalize;
var
  L: integer;
begin
  Assert(FInitialized);
  glDeleteTextures(1, @FRandomTextureId);
  for L := 0 to FAnalysisData.LevelsAmount - 1 do
  begin
    glDeleteTextures(5, @FLevels[L].ExemplarTextureId);
    FLevels[L].NeihgbPCAMatrixBuffer.Destroy;
  end;
  SetLength(FLevels, 0);
  FreeAndNil(FUpsampleShader);
  FreeAndNil(FCorrectionShader);
  FInitialized := False;
end;

function TGLSynthesizer.GetLevelCount: integer;
begin
  Result := Length(FLevels);
end;

function TGLSynthesizer.GetTextureID(Level: integer): GLuint;
begin
  Result := FLevels[Level].PatchesTextureId;
end;

procedure TGLSynthesizer.Initialize;
var
  L, spacing: integer;
  img: TImageDesc;
  tempFBO: GLuint;
  initCoords: Vec2i;
  wgs, wgc: vec3i;
begin
  Assert(not FInitialized);
  Assert(Supported);

  if not FAnalysisData.IsValid then
  begin
    // There must be error message
    Exit;
  end;

  wgs := GetWorkgroupSize;
  wgc := GetWorkgroupCount;
  FWorkGroupCount[0] := wgs[0] div FSideSize;
  FWorkGroupCount[1] := wgs[1] div FSideSize;
  if (FWorkGroupCount[0] > wgc[0]) or (FWorkGroupCount[1] > wgc[1]) then
  begin
    // There must be error message
    Exit;
  end;

  CreateRandomTexture;

  SetLength(FLevels, FAnalysisData.LevelsAmount);
  for L := 0 to FAnalysisData.LevelsAmount - 1 do
  begin
    // Spacing exemplar coordinates for level
    spacing := 1 shl L;
    FLevels[L].Spacing[0][0] := spacing;
    FLevels[L].Spacing[0][1] := 0;
    FLevels[L].Spacing[1][0] := 0;
    FLevels[L].Spacing[1][1] := spacing;
    FLevels[L].Spacing[2][0] := spacing;
    FLevels[L].Spacing[2][1] := spacing;

    glGenTextures(5, @FLevels[L].ExemplarTextureId);
    // Gaussian level of exemplar
    img := FAnalysisData.Levels[L].Image;
    glBindTexture(GL_TEXTURE_2D, FLevels[L].ExemplarTextureId);
    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP_SGIS, GL_FALSE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, img.InternalFormat, img.Width, img.Height, 0,
      img.ColorFormat, img.DataType, img.Data);
    // Empty patches level
    glBindTexture(GL_TEXTURE_2D, FLevels[L].PatchesTextureId);
    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP_SGIS, GL_FALSE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RG16I, FSideSize, FSideSize, 0,
      GL_RG, GL_UNSIGNED_BYTE, nil);
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
    glBindTexture(GL_TEXTURE_2D, FLevels[L].NeihgbPart1TextureId);
    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP_SGIS, GL_FALSE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, img.InternalFormat, img.Width, img.Height, 0,
      img.ColorFormat, img.DataType, img.Data);
    // PCA matrix of neihgborhoods level
    FLevels[L].NeihgbPCAMatrixBuffer := TGLBufferObject.Create(btUniform);
    FLevels[L].NeihgbPCAMatrixBuffer.Allocate(SizeOf(TNeighbPCAmatrix),
      @FAnalysisData.Levels[L].NeihgbPCAMatrix, GL_STATIC_READ);
  end;

  // Fill coarsest level with midle exemplar's coordinates
  glGenFramebuffers(1, @tempFBO);
  glFramebufferDrawBufferEXT(tempFBO, GL_COLOR_ATTACHMENT0);
  glNamedFramebufferTextureEXT(tempFBO, GL_COLOR_ATTACHMENT0,
    FLevels[FAnalysisData.LevelsAmount - 1].PatchesTextureId, 0);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, tempFBO);
  initCoords[0] := FAnalysisData.Exemplar.Width div 2;
  initCoords[1] := FAnalysisData.Exemplar.Height div 2;
  glClearBufferiv(GL_COLOR, 0, @initCoords[0]);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
  glDeleteFramebuffers(1, @tempFBO);

  if not Assigned(FUpsampleShaderSource) then
  begin
    FUpsampleShaderSource := TShaderProgram.CreateOwned(Self);
    FUpsampleShaderSource.ShaderText[stCompute] :=
      GLSL_SYNTH_HEADER +
      GLSL_SYNTH_UPSAMPLE_JITTER_INTERFACE +
      GLSL_SYNTH_TILINGSUB +
      GLSL_SYNTH_RANDSUB +
      GLSL_SYNTH_UPSAMPLE_JITTER_MAIN;
  end;
  FUpsampleShader := TGLSLShaderProgram.CreateFrom(FUpsampleShaderSource);
  if FUpsampleShader.LinkShader = 0 then
  begin
    WriteLn(FUpsampleShader.Log);
  end;

  if not Assigned(FCorrectionShaderSource) then
  begin
    FCorrectionShaderSource := TShaderProgram.CreateOwned(Self);
    FCorrectionShaderSource.ShaderText[stCompute] :=
      GLSL_SYNTH_HEADER +
      GLSL_SYNTH_CORRECTION_INTERFACE +
      GLSL_SYNTH_TILINGSUB +
      GLSL_SYNTH_CORRECTION_MAIN;
  end;
  FCorrectionShader := TGLSLShaderProgram.CreateFrom(FCorrectionShaderSource);
  FCorrectionShader.LinkShader;
  if FCorrectionShader.LinkShader = 0 then
  begin
    WriteLn(FCorrectionShader.Log);
  end;

  FInitialized := True;
end;

procedure TGLSynthesizer.Process;
var
  i: integer;
begin
  Assert(FInitialized);
  for i := LevelCount - 2 downto 0 do
  begin
    DoUpsample(i);
  end;
end;

procedure TGLSynthesizer.SetCorrectionShaderSource(const Value: TShaderProgram);
begin
  if Assigned(FCorrectionShaderSource)
    and (FCorrectionShaderSource.Owner = Self) then
      FCorrectionShaderSource.Destroy;
  FCorrectionShaderSource := Value;
end;

procedure TGLSynthesizer.SetUpsampleShaderSource(const Value: TShaderProgram);
begin
  if Assigned(FUpsampleShaderSource)
    and (FUpsampleShaderSource.Owner = Self) then
      FUpsampleShaderSource.Destroy;
  FUpsampleShaderSource := Value;
end;

class function TGLSynthesizer.Supported: Boolean;
begin
  Result := GL_ARB_compute_shader and
    GL_ARB_shader_image_load_store and
    GL_ARB_shader_storage_buffer_object and
    GL_ARB_clear_buffer_object;
end;

{$ENDREGION}

end.
