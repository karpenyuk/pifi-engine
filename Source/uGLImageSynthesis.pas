unit uGLImageSynthesis;

interface

uses
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
  end;

  TGLSynthesizer = class
  private
    FAnalysisData: TAnalysisData;
    FSideSize: integer;
    FLevels: array of TGLSynthLevel;
    FUpsampleShaderSource: TShaderProgram;
    FCorrectionShaderSource: TShaderProgram;
    FCopyImageShaderSource: TShaderProgram;

    FInitialized: Boolean;
    FUpsampleShader: TGLSLShaderProgram;
    FCorrectionShader: TGLSLShaderProgram;
    FCopyImageShader: TGLSLShaderProgram;
    FWorkGroupCount: vec3i;

    FFBOId: GLuint;
    FReadSynthTextureId: GLuint;
    FRandomTextureId: GLuint;

    FKappa: single;

    function GetLevelCount: integer;
    procedure SetCorrectionShaderSource(const Value: TShaderProgram);
    procedure SetUpsampleShaderSource(const Value: TShaderProgram);
    function GetTextureID(Level: integer): GLuint;
    function GetExemplarTextureID(Level: integer): GLuint;

  protected
    procedure CreateRandomTexture;
    procedure DoUpsample(aLevel: integer);
    procedure DoCorrection(aLevel: integer);
    procedure CopyToReadSynthTexture(aLevel: integer);
  public
    constructor Create(anAnalysisData: TAnalysisData);
    destructor Destroy; override;

    procedure Initialize;
    procedure Process;
    procedure Finalize;
    class function Supported: Boolean;
    property Initialized: Boolean read FInitialized;

    property ExemplarTextureIDs[Level: integer]: GLuint read GetExemplarTextureID;
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
    // Controls whether coherent candidates are favored; 1.0 has no effect,
    // 0.1 has strong effect, 0.0 is invalid.
    property Kappa: single read FKappa write FKappa;
  end;

implementation

uses
  uMiscUtils,
  uImageSynthesisShaderGen;

{$REGION 'TGLSynthesizer'}


procedure TGLSynthesizer.CopyToReadSynthTexture(aLevel: integer);
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
    glBindImageTexture(6, FReadSynthTextureId, 0, False, 0,
      GL_WRITE_ONLY, GL_RG16I);
    glDispatchCompute(FWorkGroupCount[0], FWorkGroupCount[1], 1);
    glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
    FCorrectionShader.Apply;
  end;
end;

constructor TGLSynthesizer.Create(anAnalysisData: TAnalysisData);
begin
  Assert(Assigned(anAnalysisData));
  FAnalysisData := anAnalysisData;
  FSideSize := 512;
  FKappa := 1;
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
  FCopyImageShaderSource.Free;
  inherited;
end;

procedure TGLSynthesizer.DoCorrection(aLevel: integer);
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
    glBindImageTexture(5, FLevels[aLevel].PatchesTextureId, 0, False, 0,
      GL_WRITE_ONLY, GL_RG16I);

    FLevels[aLevel].NeihgbPCAMatrixBuffer.BindAllRange(0);

    // Exemplar texture size
    iv[0] := FAnalysisData.Exemplar.Width;
    iv[1] := FAnalysisData.Exemplar.Height;
    SetUniform('exemplarSize', iv);

    // Spacing exemplar coordinates for level
    iv[0] := 1 shl aLevel;
    iv[1] := iv[0];
    SetUniform('spacing', iv);

    v6 := FAnalysisData.Levels[alevel].NeighbScale;
    pv := @v6[0];
    SetUniform('NeighbScale1', pv^);
    pv := @v6[3];
    SetUniform('NeighbScale2', pv^);

    v6 := FAnalysisData.Levels[alevel].NeighbOffset;
    pv := @v6[0];
    SetUniform('NeighbOffset1', pv^);
    pv := @v6[3];
    SetUniform('NeighbOffset2', pv^);

    SetUniform('Kappa', 1.0);

    for subPass := 0 to 7 do
    begin
      CopyToReadSynthTexture(aLevel);
      SetUniform('subPassOffset', STEP[subPass]);
      glDispatchCompute(FWorkGroupCount[0] div 2, FWorkGroupCount[1] div 2, 1);
      glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT);
    end;

    FLevels[aLevel].NeihgbPCAMatrixBuffer.UnBindBuffer;

    UnApply;
  end;
end;

procedure TGLSynthesizer.DoUpsample(aLevel: integer);
var
  iv: Vec2i;
  iv4: Vec4i;
begin
  with FUpsampleShader do
  begin
    Apply;
    // Parent level to read
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, FLevels[aLevel + 1].PatchesTextureId);

    // Child level to write
    glBindImageTexture(0, FLevels[aLevel].PatchesTextureId, 0, False, 0,
      GL_WRITE_ONLY, GL_RG16I);

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
    iv4[0] := Round(Random * FSideSize);
    iv4[1] := Round(Random * FSideSize);
    iv4[2] := Round(Random * FSideSize);
    iv4[3] := Round(Random * FSideSize);
    SetUniform('randScaleOffset', iv4);

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
  glDeleteFramebuffers(1, @FFBOId);
  glDeleteTextures(1, @FReadSynthTextureId);
  glDeleteTextures(1, @FRandomTextureId);
  for L := 0 to FAnalysisData.LevelsAmount - 1 do
  begin
    glDeleteTextures(5, @FLevels[L].ExemplarTextureId);
    FLevels[L].NeihgbPCAMatrixBuffer.Destroy;
  end;
  SetLength(FLevels, 0);
  FreeAndNil(FUpsampleShader);
  FreeAndNil(FCorrectionShader);
  FreeAndNil(FCopyImageShader);
  FInitialized := False;
end;

function TGLSynthesizer.GetExemplarTextureID(Level: integer): GLuint;
begin
  Result := FLevels[Level].ExemplarTextureId;
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
  L, spacing, i, j, k: integer;
  img: TImageDesc;
  initCoords: Vec2i;
  wgs, wgc: vec3i;
  M: Pointer;
  pM: PSingle;
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
  if FAnalysisData.EdgePolicy = epRepeat then
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  end
  else
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_MIRRORED_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_MIRRORED_REPEAT);
  end;
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RG16I, FSideSize, FSideSize, 0,
    GL_RG_INTEGER, GL_SHORT, nil);
  //  glTextureStorage2DEXT(FReadSynthTextureId, GL_TEXTURE_2D, 1,
//    GL_RG16I, FSideSize, FSideSize);

  GetMem(M, SizeOf(TNeighbPCAmatrix));

  SetLength(FLevels, FAnalysisData.LevelsAmount);
  for L := 0 to High(FLevels) do
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
//    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);
//    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_BASE_LEVEL, 0);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RG16I, FSideSize, FSideSize, 0, GL_RG_INTEGER, GL_SHORT, nil);
//    glTextureStorage2DEXT(FReadSynthTextureId, GL_TEXTURE_2D, 1,
//      GL_RG16I, FSideSize, FSideSize);

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

    if L < 3 then
      FLevels[L].JitterStrength := Vec2Make(0, 0)
    else
    begin
      FLevels[L].JitterStrength[0] := 25 * L / LevelCount;
      FLevels[L].JitterStrength[1] := FLevels[L].JitterStrength[0];
    end;
  end;

  // Fill coarsest level with midle exemplar's coordinates
  glGenFramebuffers(1, @FFBOID);
  glFramebufferDrawBufferEXT(FFBOID, GL_COLOR_ATTACHMENT0);
  glNamedFramebufferTextureEXT(FFBOID, GL_COLOR_ATTACHMENT0,
    FLevels[FAnalysisData.LevelsAmount - 1].PatchesTextureId, 0);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, FFBOID);
  initCoords[0] := FAnalysisData.Exemplar.Width div 2;
  initCoords[1] := FAnalysisData.Exemplar.Height div 2;
  glClearBufferiv(GL_COLOR, 0, @initCoords[0]);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);

  if not Assigned(FUpsampleShaderSource) then
  begin
    FUpsampleShaderSource := TShaderProgram.CreateOwned(Self);
    FUpsampleShaderSource.ShaderText[stCompute] :=
      GLSL_SYNTH_HEADER +
      GLSL_SYNTH_UPSAMPLE_JITTER_INTERFACE +
      GLSL_SYNTH_TILINGSUB +
      GLSL_SYNTH_RANDSUB_TEXTURE +
      GLSL_SYNTH_UPSAMPLE_JITTER_MAIN;
  end;
  FUpsampleShader := TGLSLShaderProgram.CreateFrom(FUpsampleShaderSource);
  WriteLn(FUpsampleShader.Log);

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
  WriteLn(FCorrectionShader.Log);

  if not GL_ARB_copy_image then
  begin
    FCopyImageShaderSource := TShaderProgram.CreateOwned(Self);
    FCopyImageShaderSource.ShaderText[stCompute] :=
      GLSL_SYNTH_HEADER +
      GLSL_SYNTH_COPY_IMAGE;
    FCopyImageShader := TGLSLShaderProgram.CreateFrom(FCopyImageShaderSource);
    FCopyImageShader.LinkShader;
    WriteLn(FCopyImageShader.Log);
  end;

  FreeMem(M);

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
    DoCorrection(i);
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
    GL_ARB_clear_buffer_object and
    GL_ARB_texture_storage;
end;

{$ENDREGION}

end.
