unit uMainUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Types,
  uGLViewer,
  uBaseRenders,
  uBaseTypes,
  uBaseGL;

type
  TForm5 = class(TForm)
    GLViewer1: TGLViewer;
    procedure GLViewer1CanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure GLViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure GLViewer1Render(Sender: TObject);
    procedure GLViewer1ContextReady(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    MX, MY: Integer;
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}

uses
  uFileFormatDAE,
  uRenderResource,
  uVMath,
  dglOpenGL;

var
  Mesh: TVertexObject;
  Render: TBaseRender;
  Shader1: TGLSLShaderProgram;
  cameraPos: TVector;
  Model, View, Proj: TMatrix;

  Drawer: TGLVertexObject;

procedure TForm5.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  glFinish;
  GLViewer1.OnRender := nil;
  GLViewer1.Context.Deactivate;
  Mesh.Attribs[0].Destroy; // Notification force attributes to be packed
  Mesh.Attribs[0].Destroy;
  Mesh.Attribs[0].Destroy;
  Mesh.Destroy;
  Shader1.Destroy;
  Drawer.Destroy;
end;

procedure TForm5.GLViewer1CanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  Proj := TMatrix.PerspectiveMatrix(60, NewWidth / NewHeight, 0.1, 100);
end;

procedure TForm5.GLViewer1ContextReady(Sender: TObject);
var
  path: string;
  ver: TApiVersion;
begin
  ver.GAPI := avGL;
  ver.Version := 420;
  Render := vRegisteredRenders.GetCompatibleRender(ver);

{$IFDEF MSWindows}
  path := '..\..\Source\Media\'; { :-\ }
{$ENDIF}
{$IFDEF Linux}
  path := '../../Source/Media/'; { :-/ }
{$ENDIF}
  Mesh := FileFormatDAE.LoadAndCreateVertexObject(path + 'bunny.dae');

  Shader1 := TGLSLShaderProgram.Create;

  Shader1.AttachShaderFromFile(stVertex, path+'ColorShader3D.Vert');
  Shader1.AttachShaderFromFile(stFragment, path+'ColorShader.Frag');

  Shader1.LinkShader;
  if Shader1.Error then
  begin
    showmessage(Shader1.Log);
    Halt(0);
  end;

  Proj := TMatrix.PerspectiveMatrix(60, GLViewer1.Width / GLViewer1.Height,
    0.1, 100);
  cameraPos := TVector.Make(0, 0, 5);
  View := TMatrix.LookAtMatrix(cameraPos, VecNull, vecY);
  Model := TMatrix.ScaleMatrix(TVector.Make(12, 12, 12));

  Drawer := TGLVertexObject.CreateFrom(Mesh);
  Drawer.Shader := Shader1;
end;

procedure TForm5.GLViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MX := X;
  MY := Y;
end;

procedure TForm5.GLViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
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

procedure TForm5.GLViewer1Render(Sender: TObject);
var
  MVP: TMatrix;
begin
  GLViewer1.Context.ClearDevice;

  MVP := Model * View * Proj;
  Shader1.Apply;
  Shader1.SetUniform('MVP', MVP.Matrix4);
  Drawer.RenderVO();
  Shader1.UnApply;
end;

end.
