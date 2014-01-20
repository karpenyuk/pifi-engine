unit TestProject;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF} Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uGLViewer, uBaseGL, uBaseTypes, uVMath, dglOpenGL, ExtCtrls,
  uPrimitives, uMiscUtils, uRenderResource, uBaseRenders, uGLRenders, uImageFormats;

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
  private
    { Private declarations }
    MX, MY: Integer;
    procedure GL1xDebugRender(const Mesh: TGLVertexObject;
      const Model: TMatrix);
  public
    { Public declarations }
    function Test: Boolean;
  end;

  TRObjData = packed record
    r, // Offset 0, size 64
    t, // Offset 64, size 64
    s: mat4; // Offset 128, size 64
    World, // Offset 192, size 64
    mvp: mat4; // Offset 256, size 64
    Culled: cardinal; // Offset 320, size 4
    Updated: cardinal; // Offset 324, size 4
    br: single; // Offset 328, size 4
    bp: vec4; // Offset 332, size 16
    index: cardinal; // Offset 348, size 4
    parent: Integer; // Offset 352, size 4
    Visible: cardinal; // Offset 356, size 4
    emin, // Offset 360, size 12
    emax: vec3; // Offset 372, size 12
  end; // TotalSize 360b

var
  Form2: TForm2;
  Shader1: TGLSLShaderProgram;
  cameraPos: TVector;
  Model, View, Proj: TMatrix;
  mvp: TMatrix;

  Quad, Sphere, Box, Teapod: TGLVertexObject;

  objbuff: TGLBufferObject;
  objts: array of TRObjData;

  GL1xRender: Boolean = false;

  Render: TBaseRender;
  SceneGraph: TSceneGraph;

implementation

{$R *.dfm}

{$I MatrixTest.inc}


function MakeQuad: TGLVertexObject;
begin
  result := TGLVertexObject.CreateFrom(CreatePlane(4, 4));
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
begin
  showmessage(inttohex(TImageFormatSelector.CreateInt8(bfRGBA),4));

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
    path := '..\Source\Media\';
{$ENDIF}
{$IFDEF Linux}
    path := '../Source/Media/';
{$ENDIF}
    Shader1.AttachShaderFromFile(stVertex, path + 'Shader.Vert');
    Shader1.AttachShaderFromFile(stFragment, path + 'Shader.Frag');
    Shader1.LinkShader;
    if Shader1.Error then begin
      showmessage(Shader1.Log);
      Halt(0);
    end;

    setlength(objts, 10000);
    objbuff := TGLBufferObject.Create(btShaderStorage);
    objbuff.Allocate(length(objts) * sizeof(TRObjData), @objts[0]);
  end
  else Shader1 := nil;

  // Making MVP Matrix
  Proj := TMatrix.PerspectiveMatrix(60, GLViewer1.Width / GLViewer1.Height,
    0.1, 100);
  cameraPos := TVector.Make(0, 0, -5);
  View := TMatrix.LookAtMatrix(cameraPos, VecNull, vecY);
  Model.SetIdentity;

  Quad := MakeQuad;
  Quad.Shader := Shader1;
  Sphere := MakeSphere;
  Sphere.Shader := Shader1;
  Box := MakeBox;
  Box.Shader := Shader1;
  Teapod := MakeTeapod;
  Teapod.Shader := Shader1;

  SceneGraph := TSceneGraph.Create;
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
    Shader1.UnApply;
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    // TGLStaticRender.RenderVertexObject(box,slUseActiveShader, spUseActiveShaderFirst);
    Box.RenderVO;
    Quad.RenderVO;
    Sphere.RenderVO;
    Teapod.RenderVO();
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
