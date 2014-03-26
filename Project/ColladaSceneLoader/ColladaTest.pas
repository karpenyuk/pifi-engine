unit ColladaTest;

interface

uses
  SysUtils, Classes, uBaseGL, uBaseTypes, uVMath, dglOpenGL,
  uPrimitives, uMiscUtils, uRenderResource, uBaseRenders, uGLRenders,
  uLists, uImageFormats, uImageLoader, uFileFormatDAE;

type
  TDemo = class
    procedure ContextReady(Sender: TObject);
    procedure SceneRender(Sender: TObject; Delta: double);
    procedure onResize(Sender: TObject; NewWidth,NewHeight: Integer);
    procedure onMouseDown(Sender: TObject; X,Y: integer; Buttons: TCMouseButtons);
    procedure onMouseMove(Sender: TObject; X,Y: integer; Buttons: TCMouseButtons);
  private
    { Private declarations }
    MX, MY: Integer;
    FSampler: TTextureSampler;
    FGLSampler: TGLTextureSampler;
    glTex: TGLTextureObject;
    texture: TTexture;
  public
    { Public declarations }
    destructor Destroy; override;
  end;

var
  Demo: TDemo;
  Shader1: TGLSLShaderProgram;
  cameraPos: TVector;
  Model, View, Proj: TMatrix;
  mvp: TMatrix;

  Quad, Sphere, Box, Teapod: TGLVertexObject;

  GL1xRender: Boolean = false;

  Render: TBaseRender;

  GS8Image: TImageHolder;
  t1,t2,t3: TTexture;


implementation

function MakeQuad: TGLVertexObject;
begin
  result := TGLVertexObject.CreateFrom(CreatePlane(0.6, 0.6));
end;

function MakeSphere: TGLVertexObject;
begin
  result := TGLVertexObject.CreateFrom(CreateSphere(1, 16, 32));
end;

function MakeBox: TGLVertexObject;
begin
  result := TGLVertexObject.CreateFrom(uPrimitives.CreateBox(2, 1.5, 3.5));
end;

function MakeTeapod: TGLVertexObject;
begin
  result := TGLVertexObject.CreateFrom(uPrimitives.CreateTeapod(4));
end;

procedure TDemo.onMouseDown(Sender: TObject; X, Y: integer;
  Buttons: TCMouseButtons);
begin
  MX := X;  MY := Y;
end;

procedure TDemo.onMouseMove(Sender: TObject; X, Y: integer;
  Buttons: TCMouseButtons);
begin
  if mbLeft in Buttons then
  begin
    cameraPos.RotateAround(VecNull, vecY, MY - Y, MX - X);
    View := TMatrix.LookAtMatrix(cameraPos, VecNull, vecY);
  end;
  MX := X; MY := Y;
end;

procedure TDemo.onResize(Sender: TObject; NewWidth,NewHeight: Integer);
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
  GL1xRender := false;
  Render := nil;

  // Среди зарегистрированных рендеров выбираем подходящий
  Render := vRegisteredRenders.GetCompatibleRender(ver);

  // assert(Test,'Vector Math Test Failed');
  // Create default Shader with gradient FragColor output
  if not GL1xRender then begin
    Shader1 := TGLSLShaderProgram.Create;
{$IFDEF MSWindows}
    path := 'Media\';
{$ENDIF}
{$IFDEF Linux}
    path := 'Media/';
{$ENDIF}
    Shader1.AttachShaderFromFile(stVertex, path + 'Shader.Vert');
    Shader1.AttachShaderFromFile(stFragment, path + 'Shader.Frag');
    Shader1.LinkShader;
    if Shader1.Error then begin
      //showmessage(Shader1.Log);
      Halt(0);
    end;
  end
  else Shader1 := nil;

  // Making MVP Matrix
  Proj := TMatrix.PerspectiveMatrix(60, 640/480, 0.1, 100);
  cameraPos := TVector.Make(0, 0, -5);
  View := TMatrix.LookAtMatrix(cameraPos, VecNull, vecY);
  Model.SetIdentity;

  //Create Bitmap
  GS8Image:=TImageSampler.CreateBitmap(
    //Disable memory reserving for Mipmaps
    TImageFormatSelector.CreateUInt8(bfRed), 256, 256, false);

  //Create texture sampler and specify filtering mode
  FSampler:=TTextureSampler.Create;
  FSampler.minFilter := mnLinearMipmapLinear;
  FSampler.magFilter := mgLinear;
  //Create GL Texture sampler from sampler object
  FGLSampler:=TGLTextureSampler.CreateFrom(FSampler);

  //Create Texture object from image format and enable mip-map generation
  texture := GS8Image.CreateTexture;
  texture.GenerateMipMaps:=true;

  //Create OGL texture from texture object and attach sampler
  glTex := TGLTextureObject.CreateFrom(texture);
  glTex.TextureSampler := FSampler;
  //Upload texture image to VRAM with PBO
  gltex.UploadTexture;


  Teapod := MakeTeapod;
  Teapod.Shader := Shader1;

end;

destructor TDemo.Destroy;
begin
  glTex.Free;
  FGLSampler.Free;
  FSampler.Free;
  texture.Free;
  GS8Image.Free;
  inherited;
end;

procedure TDemo.SceneRender(Sender: TObject; Delta: double);
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    mvp := Model*View;
    Shader1.Bind;
    Shader1.SetUniform('ProjMatrix', Proj.Matrix4);
    Shader1.SetUniform('ModelView', mvp.Matrix4);
    Shader1.SetUniform('tex', 0);
    Shader1.UnBind;
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    // TGLStaticRender.RenderVertexObject(box,slUseActiveShader, spUseActiveShaderFirst);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, glTex.Id);
    FGLSampler.Bind(0);
    //FGLSampler.SetSamplerParams(ttTexture2D);
    Teapod.RenderVO;
    glBindTexture(GL_TEXTURE_2D, 0);
    FGLSampler.UnBind(0);
end;

end.
