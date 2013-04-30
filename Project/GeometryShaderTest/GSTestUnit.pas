unit GSTestUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF} Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, uGLViewer, uBaseGL, ExtCtrls,
  uBaseTypes, uVMath, uPrimitives, dglOpenGL, uRenderResource,
  uBaseRenders;

type

  { TForm5 }

  TForm5 = class(TForm)
    GLViewer1: TGLViewer;
    Timer1: TTimer;
    procedure GLViewer1CanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure GLViewer1ContextReady(Sender: TObject);
  {  procedure GLViewer1CanResize(Sender: TObject;
      var NewWidth, NewHeight: Integer; var Resize: Boolean);  }
    procedure GLViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLViewer1Render(Sender: TObject);
  private
    { Private declarations }
    MX, MY: Integer;
  public
    { Public declarations }
  end;

var
  Form5: TForm5;
  Render: TBaseRender;
  Shader1: TGLSLShaderProgram;
  cameraPos: TVector;
  Model, View, Proj: TMatrix;

  Teapod: TGLVertexObject;

implementation

{$R *.dfm}

uses
  uMeshUtils;

procedure TForm5.GLViewer1CanResize(Sender: TObject;
  var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
  Proj := TMatrix.PerspectiveMatrix(60, NewWidth / NewHeight, 0.1, 100);
end;

procedure TForm5.GLViewer1ContextReady(Sender: TObject);
var
  VO: TVertexObject;
  ver: TApiVersion;
  path:string;
  Indices, AdjIndices: TIntegerArray;
begin

  ver.GAPI := avGL;
  ver.Version := 420;
  Render := vRegisteredRenders.GetCompatibleRender(ver);

  Shader1 := TGLSLShaderProgram.Create;
  {$IFDEF MSWindows}
  path := '..\..\Source\Media\'; { :-\ }
  {$ENDIF}
  {$IFDEF Linux}
  path := '../../Source/Media/'; { :-/ }
  {$ENDIF}

  Shader1.AttachShaderFromFile(stVertex, path+'silhouette.vert');
  Shader1.AttachShaderFromFile(stGeometry, path+'silhouette.geom');
  Shader1.AttachShaderFromFile(stFragment, path+'silhouette.frag');

  Shader1.LinkShader;
  if Shader1.Error then
  begin
    showmessage(Shader1.Log);
    Halt(0);
  end;

  Proj := TMatrix.PerspectiveMatrix(60, GLViewer1.Width / GLViewer1.Height,
    0.1, 100);
  cameraPos := TVector.Make(0, 0, -5);
  View := TMatrix.LookAtMatrix(cameraPos, VecNull, vecY);
  Model.SetIdentity;

  VO := uPrimitives.CreateTeapod(4);
  Indices := VO.GetIndices;
  MeshUtils.ComputeTriangleAdjacency(
    VO.Attribs[0].Buffer.DataHandler, // ’ак, но как еще???
    Indices,
    AdjIndices);
  VO.SetAdjacencyIndices(AdjIndices);
  VO.FaceType := ftTrianglesAdjacency;
  Teapod := TGLVertexObject.CreateFrom(VO);
  Teapod.Shader := Shader1;
end;

procedure TForm5.GLViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MX := X;
  MY := Y;
end;

procedure TForm5.GLViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift = [ssLeft] then
  begin
    cameraPos.RotateAround(VecNull, vecY, MY - Y, MX - X);
    View := TMatrix.LookAtMatrix(cameraPos, VecNull, vecY);
  end;
  MX := X;
  MY := Y;
end;

procedure TForm5.GLViewer1Render(Sender: TObject);
const
  clrRed: Vec4 = (1, 0, 0, 1);
var
  VP: TMatrix;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  VP := View * Proj;
  Shader1.Apply;
  Shader1.SetUniform('ModelMatrix', Model.Matrix4);
  Shader1.SetUniform('ViewProjectionMatrix', VP.Matrix4);
  Shader1.SetUniform('Color', clrRed);
  Shader1.SetUniform('EyePosition', cameraPos.Vec4);
  Shader1.UnApply;
  glLineWidth(3);
  Teapod.RenderVO();
end;

procedure TForm5.Timer1Timer(Sender: TObject);
begin
  Form5.Caption := Format('FPS: %f', [GLViewer1.FPS]);
end;

end.
