unit TexturingProject;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF} Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uGLViewer, uBaseGL, uBaseTypes, uVMath, dglOpenGL, ExtCtrls,
  uPrimitives, uMiscUtils, uRenderResource, uBaseRenders, uGLRenders,
  uLists, uImageFormats, uImageLoader;

type
  TForm2 = class(TForm)
    GLViewer1: TGLViewer;
    Timer1: TTimer;
    procedure GLViewer1ContextReady(Sender: TObject);
    procedure GLViewer1Render(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLViewer1CanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure GLViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    MX, MY: Integer;
    FImagePathList: TStringList;
    FImagesList: TList;
    FTexturesList: TList;
    FGLTexturesList: TList;
    FSampler: TTextureSampler;
    FGLSampler: TGLTextureSampler;
    procedure GL1xDebugRender(const Mesh: TGLVertexObject;
      const Model: TMatrix);
  public
    { Public declarations }
    procedure FillImage(aData: pointer; aFormat: cardinal; aSize: integer);
  end;

var
  Form2: TForm2;
  Shader1: TGLSLShaderProgram;
  cameraPos: TVector;
  Model, View, Proj: TMatrix;
  mvp: TMatrix;

  Quad, Sphere, Box, Teapod: TGLVertexObject;

  GL1xRender: Boolean = false;

  Render: TBaseRender;
  SceneGraph: TSceneGraph;

  rgba8Image: TImageSampler;
  rgba32FImage: TImageSampler;
  r32FImage: TImageSampler;

  t1,t2,t3: TTexture;


implementation

{$R *.dfm}

//{$I TexturingTest.inc}


function ImageList(Path: string): TStringList;
var F: TSearchRec;
begin
  result := TStringList.Create;
  if FindFirst(path+'*.dds', faAnyFile, F) <> 0 then exit;
  result.Add(F.Name);
  while FindNext(F) = 0 do result.Add(F.Name);
end;

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

procedure TForm2.GLViewer1CanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  Proj := TMatrix.PerspectiveMatrix(60, NewWidth / NewHeight, 0.1, 100);
end;

procedure TForm2.GLViewer1ContextReady(Sender: TObject);
var
  ver: TApiVersion;
  path: string;
  i: integer;
  image: TImageLoader;
  texture: TTexture;
  gltex: TGLTextureObject;
  fn: string;
begin
  // Checking OpenGL Version
  with GLViewer1.Context do
    if (MaxMajorVersion < 4) or (MaxMinorVersion < 2) then begin
      GL1xRender := true;
      ver.GAPI := avGL;
      ver.Version := 130;
    end else begin
      ver.GAPI := avGL;
      ver.Version := 420;
    end;
  Render := nil;
  (* //Если OpenGL 4.2 не поддерживается, значит используем отладочный OpenGL 1.x
    begin
    ShowMessage('Can''t run demo. Required at least OpenGL 4.2!');
    halt(0);
    end;
  *)
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
      showmessage(Shader1.Log);
      Halt(0);
    end;
  end
  else Shader1 := nil;

  // Making MVP Matrix
  Proj := TMatrix.PerspectiveMatrix(60, GLViewer1.Width / GLViewer1.Height,
    0.1, 100);
  cameraPos := TVector.Make(0, 0, -5);
  View := TMatrix.LookAtMatrix(cameraPos, VecNull, vecY);
  Model.SetIdentity;
{
  rgba8Image:=TImageSampler.CreateBitmap(
    TImageFormatSelector.CreateUInt8(bfRGBA), 256, 256, false);
  FillImage(rgba8Image.ImageDescriptor.Data, rgba8Image.ImageFormat,
    rgba8Image.ImageDescriptor.DataSize);

  rgba32FImage:=TImageSampler.CreateBitmap(
    TImageFormatSelector.CreateFloat32(bfRGBA), 256, 256, false);
  FillImage(rgba32FImage.ImageDescriptor.Data, rgba32FImage.ImageFormat,
    rgba32FImage.ImageDescriptor.DataSize);

  r32FImage:=TImageSampler.CreateBitmap(
    TImageFormatSelector.CreateFloat32(bfRed), 256, 256, false);
  FillImage(r32FImage.ImageDescriptor.Data, r32FImage.ImageFormat,
    r32FImage.ImageDescriptor.DataSize);

  t1 := TTexture.CreateOwned(rgba8Image);
  t2 := TTexture.CreateOwned(rgba32FImage);
  t3 := TTexture.CreateOwned(r32FImage);
}
  Quad := MakeQuad;
  Quad.Shader := Shader1;
  Sphere := MakeSphere;
  Sphere.Shader := Shader1;
  Box := MakeBox;
  Box.Shader := Shader1;
  Teapod := MakeTeapod;
  Teapod.Shader := Shader1;

  SceneGraph := TSceneGraph.Create;

  FSampler:=TTextureSampler.Create;
  FSampler.minFilter := mnLinear;
  FSampler.magFilter := mgLinear;
  FGLSampler:=TGLTextureSampler.CreateFrom(FSampler);

  FImagePathList := ImageList('Media\');
  FImagesList:=TList.Create;
  FTexturesList:=TList.Create;
  FGLTexturesList:=TList.Create;
  for i:= 0 to FImagePathList.Count-1 do begin
     image := TImageLoader.Create;
     fn:=FImagePathList[i];
     FImagePathList[i]:='Failed: ' + FImagePathList[i];
     FImagePathList.SaveToFile('E:\ddslog.txt');
     try
             image.LoadImageFromFile('Media\'+fn);
             FImagesList.add(image);
             texture := image.CreateTexture;
             texture.GenerateMipMaps := false;
             FTexturesList.Add(texture);
             gltex := TGLTextureObject.CreateFrom(texture);
             glTex.TextureSampler := FSampler;
             gltex.UploadTexture;
             FGLTexturesList.Add(gltex);
     finally
             if assigned(gltex) then
               FImagePathList[i]:='Passed: ' + fn + ' ('+inttostr(gltex.Id)+')';
     end;
     FImagePathList.SaveToFile('ddslog.txt');
  end;
//  RunTest;
end;

procedure TForm2.GLViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MX := X;
  MY := Y;
end;

procedure TForm2.GLViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Shift = [ssLeft] then
  begin
    cameraPos.RotateAround(VecNull, vecY, MY - Y, MX - X);
    View := TMatrix.LookAtMatrix(cameraPos, VecNull, vecY);
  end;
  MX := X;
  MY := Y;
end;

procedure TForm2.FillImage(aData: pointer; aFormat: cardinal; aSize: integer);
var ps: byte;
    i,j, count: integer;
begin
  ps := TImageFormatSelector.GetPixelSize(aFormat);
  count := aSize div ps;
  for i := 0 to count - 1 do begin
    if not TImageFormatBits.isFloatFormat(aFormat)
    then for j:=0 to ps - 1 do begin
      PByteArray(aData)[i*ps+j] := (i-(i div 255)*255 + j);
    end else PFloatArray(aData)[i] := (i-(i div 255)*255)/255;
  end;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLViewer1.OnRender := nil;
  FreeObjectList(FImagesList);
  FreeObjectList(FTexturesList);
  FreeObjectList(FGLTexturesList);
end;

procedure TForm2.GL1xDebugRender(const Mesh: TGLVertexObject;
  const Model: TMatrix);
var
  i: Integer;
  p, n, t: pointer;
  attr: TGLAttribObject;
const
  CGLFaceType: array [ftPoints .. ftQuads] of cardinal =
    (
    GL_Points, GL_LINE_STRIP, GL_LINE_LOOP, GL_Lines, GL_LINE_STRIP_ADJACENCY,
    GL_LINES_ADJACENCY,
    GL_Triangle_Strip, GL_Triangle_Fan, GL_Triangles,
    GL_Triangle_Strip_Adjacency,
    GL_Triangles_Adjacency, GL_Patches, GL_Quads
    );
begin
  p := nil;
  n := nil;
  t := nil;
  glMatrixMode(GL_PROJECTION);
  glLoadMatrixf(PGLFloat(Proj.GetAddr));
  glMatrixMode(GL_MODELVIEW);
  glLoadMatrixf(PGLFloat(View.GetAddr));
  glMultMatrixf(PGLFloat(Model.GetAddr));

  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
  glLineWidth(2);
  glBegin(CGLFaceType[Mesh.FaceType]);
  glColor3f(0, 0, 1);
  for i := 0 to Mesh.IndiceCount - 1 do begin
    attr := Mesh.GetAttribBySemantic(atVertex);
    if assigned(attr) then p := attr[Mesh.Indices[i]];
    attr := Mesh.GetAttribBySemantic(atNormal);
    if assigned(attr) then n := attr[Mesh.Indices[i]];
    attr := Mesh.GetAttribBySemantic(atTexCoord0);
    if assigned(attr) then t := attr[Mesh.Indices[i]];

    if assigned(n) then glNormal3fv(n);

    if assigned(t) then begin
      case Mesh.GetAttribBySemantic(atTexCoord0).AttrSize of
        2: glTexCoord2fv(t);
        3: glTexCoord3fv(t);
        4: glTexCoord4fv(t);
      end;
    end;

    if assigned(p) then begin
      case Mesh.GetAttribBySemantic(atVertex).AttrSize of
        2: glVertex2fv(p);
        3: glVertex3fv(p);
      end;
    end;
  end;
  glEnd;
end;

procedure TForm2.GLViewer1Render(Sender: TObject);
var tex: TGLTextureObject;
    i,j,n: integer;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  { //Со временем здесь будет полноценный ренедер сцены
    if assigned(Render) then begin
    Render.ProcessScene(SceneGraph); exit;
    end;
  }
  mvp := Model * View;
  if not GL1xRender then begin
    Shader1.Apply;
    Shader1.SetUniform('ProjMatrix', Proj.Matrix4);
    Shader1.SetUniform('ModelView', mvp.Matrix4);
    Shader1.SetUniform('tex', 0);
    Shader1.UnApply;
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    // TGLStaticRender.RenderVertexObject(box,slUseActiveShader, spUseActiveShaderFirst);
    glActiveTexture(GL_TEXTURE0);
    for i:=0 to 5 do for j:=0 to 6 do begin
      n:=i*7+j;
      if n < FGLTexturesList.Count then begin
        tex:=TGLTextureObject(FGLTexturesList[n]);
        glBindTexture(GL_TEXTURE_2D, tex.Id);
        //FGLSampler.Bind(0);
        FGLSampler.SetSamplerParams(ttTexture2D);
        Shader1.Apply;
        Model := TMatrix.TranslationMatrix(Vector(0.7 * (j-3), 0.7 * (i-3), 0));
        mvp := Model * View;
        Shader1.SetUniform('ModelView', mvp.Matrix4);
        Shader1.UnApply;
        Quad.RenderVO;
      end;
    end;
 {
    Box.RenderVO;
    Quad.RenderVO;
    Sphere.RenderVO;
    Teapod.RenderVO();
}
  end else begin
    GL1xDebugRender(Quad, Model);
    GL1xDebugRender(Sphere, Model);
    GL1xDebugRender(Box, Model);
    GL1xDebugRender(Teapod, Model);
  end;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  Form2.Caption := Format('FPS: %f', [GLViewer1.FPS]);
end;

end.
