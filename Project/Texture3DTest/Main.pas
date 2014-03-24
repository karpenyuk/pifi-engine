unit Main;

interface

uses
  SysUtils, Classes, uBaseGL, uBaseTypes, uVMath, dglOpenGL,
  uPrimitives, uMiscUtils, uRenderResource, uBaseRenders, uGLRenders,
  uLists, uImageFormats, uImageLoader;

type
  TDemo = class
    procedure ContextReady(Sender: TObject);
    procedure SceneRender(Sender: TObject; Delta: double);
    procedure onResize(Sender: TObject; NewWidth, NewHeight: Integer);
    procedure onMouseDown(Sender: TObject; X, Y: Integer;
      Buttons: TCMouseButtons);
    procedure onMouseMove(Sender: TObject; X, Y: Integer;
      Buttons: TCMouseButtons);
    procedure onDebugMessage(const AMessage: string);
  private
    { Private declarations }
    MX, MY: Integer;
  public
    { Public declarations }
    destructor Destroy; override;
  end;

var
  Demo: TDemo;
  Shader1: TGLSLShaderProgram;
  cameraPos: TVector;
  Model, View, Proj: TMatrix;

  Box: TGLVertexObject;

  Render: TBaseRender;

  sampler: TTextureSampler;
  glSampler: TGLTextureSampler;

  VolumeImage: TImageLoader;
  texture: TTexture;
  glTexture: TGLTextureObject;

  Projection_length: integer;

implementation

procedure TDemo.onDebugMessage(const AMessage: string);
begin

end;

procedure TDemo.onMouseDown(Sender: TObject; X, Y: Integer;
  Buttons: TCMouseButtons);
begin
  MX := X;
  MY := Y;
end;

procedure TDemo.onMouseMove(Sender: TObject; X, Y: Integer;
  Buttons: TCMouseButtons);
begin
  if mbLeft in Buttons then
  begin
    cameraPos.RotateAround(VecNull, vecY, MY - Y, MX - X);
    View := TMatrix.LookAtMatrix(cameraPos, VecNull, vecY);
  end;
  MX := X;
  MY := Y;
end;

procedure TDemo.onResize(Sender: TObject; NewWidth, NewHeight: Integer);
begin
  Proj := TMatrix.PerspectiveMatrix(60, NewWidth / NewHeight, 0.1, 100);
end;

procedure TDemo.ContextReady(Sender: TObject);
var
  ver: TApiVersion;
  path: string;
begin
  // Checking OpenGL Version
  ver.GAPI := avGL;
  ver.Version := 210;
  Render := nil;

  // Среди зарегистрированных рендеров выбираем подходящий
  Render := vRegisteredRenders.GetCompatibleRender(ver);

  // assert(Test,'Vector Math Test Failed');
  // Create default Shader with gradient FragColor output
  Shader1 := TGLSLShaderProgram.Create;
{$IFDEF MSWindows}
  path := 'Media\';
{$ENDIF}
{$IFDEF Linux}
  path := 'Media/';
{$ENDIF}
  Shader1.AttachShaderFromFile(stVertex, path + 'Raymarch.Vert');
  Shader1.AttachShaderFromFile(stFragment, path + 'Raymarch.Frag');
  Shader1.LinkShader;
  if Shader1.Error then begin
    // showmessage(Shader1.Log);
    Halt(0);
  end;

  // Making MVP Matrix
  Proj := TMatrix.PerspectiveMatrix(60, 640 / 480, 0.1, 100);
  cameraPos := TVector.Make(0, 0, -2);
  View := TMatrix.LookAtMatrix(cameraPos, VecNull, vecY);
  Model := TMatrix.RotationMatrix(vecX, Pi);

  // Load 3D image from file
  VolumeImage := TImageLoader.Create();
  VolumeImage.LoadImageFromFile(path + 'Head3D.dds');
  Projection_length := VolumeImage.Height;

  // Create texture sampler and specify edge policy and filtering mode
  sampler := TTextureSampler.Create;
  sampler.WrapS := twClampToBorder;
  sampler.WrapT := twClampToBorder;
  sampler.WrapR := twClampToBorder;
  sampler.minFilter := mnLinearMipmapLinear;
  sampler.magFilter := mgLinear;

  // Create GL Texture sampler from sampler object
  glSampler := TGLTextureSampler.CreateFrom(sampler);

  // Create Texture object from image format
  texture := VolumeImage.CreateTexture();

  // Create OGL texture from texture object and attach sampler
  glTexture := TGLTextureObject.CreateFrom(texture);
  glTexture.TextureSampler := sampler;
  // Upload texture image to VRAM with PBO
  glTexture.UploadTexture;

  Box := TGLVertexObject.CreateFrom(uPrimitives.CreateBox(1, 1, 1));
  Box.Shader := Shader1;

  with Shader1 do
  begin
    Bind;
    SetUniform('steps', Projection_length);
    SetUniform('brightness', 1.0);
    SetUniform('density', 1.0);
    SetUniform('threshold', 0.99);
    SetUniform('boxMin', TVector.Make(-1.0, -1.0, -1.0).Vec3);
    SetUniform('boxMax', TVector.Make(1.0, 1.0, 1.0).Vec3);
    SetUniform('texSize', TVector.Make(2.0, 2.0, 2.0).Vec3);
    UnBind;
  end;
end;

destructor TDemo.Destroy;
begin
  glTexture.Free;
  glSampler.Free;
  sampler.Free;
  texture.Free;
  VolumeImage.Free;
  inherited;
end;

procedure TDemo.SceneRender(Sender: TObject; Delta: double);
var
  mv, mvp: TMatrix;
begin

  mv := Model * View;
  mvp := mv * Proj;

  Shader1.Bind;
  Shader1.SetUniform('ModelViewProjMatrix', mvp.Matrix4);
  Shader1.SetUniform('eyerayOrigin', mv.Invert.Transform(TVector.Make(0, 0, 0, 1)).vec3);
  Shader1.UnBind;
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_3D, glTexture.Id);
  glSampler.Bind(0);
  Box.RenderVO;
  glBindTexture(GL_TEXTURE_3D, 0);
  glSampler.UnBind(0);
end;

end.
