unit SceneGraphProject;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF} Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uGLViewer, uBaseGL, uBaseTypes, uVMath, dglOpenGL, ExtCtrls,
  uPrimitives, uMiscUtils, uRenderResource, uBaseRenders, uGLRenders,
  uLists, uImageFormats, uImageLoader, SceneConstructor, uWorldSpace,
  uStorage;

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
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
  private
    { Private declarations }
    MX, MY: Integer;
    FDemoScene: TDemoScene;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

  Render: TBaseRender;

  counter: Integer = 0;
  dummy: Integer = 0;
  cv: Integer = 0;
  dt: double = 0;

implementation

{$R *.dfm}


procedure TForm2.FormCreate(Sender: TObject);
begin
  GLViewer1.Context.DebugContext := true;
end;

procedure TForm2.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  FDemoScene.SceneGraph.Camera.AdjustDistanceToTarget(1.1);
end;

procedure TForm2.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  FDemoScene.SceneGraph.Camera.AdjustDistanceToTarget(1/1.1);
end;

procedure TForm2.GLViewer1CanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  FDemoScene.SetSize(NewWidth, NewHeight);
end;

procedure TForm2.GLViewer1ContextReady(Sender: TObject);
var
  ver: TApiVersion;
begin


  // Checking OpenGL Version
  with GLViewer1.Context do
    if (MaxMajorVersion < 4) or (MaxMinorVersion < 2) then begin
      showmessage('Can''t run demo. Required at least OpenGL 4.2!');
      Halt(0);
    end else begin
      ver.GAPI := avGL;
      ver.Version := 420;
    end;
  Render := nil;

  // Среди зарегистрированных рендеров выбираем подходящий
  Render := vRegisteredRenders.GetCompatibleRender(ver);

  FDemoScene := TDemoScene.Create;

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
    FDemoScene.SceneGraph.Camera.RotateAround(VecNull, vecY, MY - Y, MX - X);
  end;
  MX := X;
  MY := Y;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLViewer1.OnRender := nil;
  FDemoScene.Free;
end;

procedure TForm2.GLViewer1Render(Sender: TObject);
begin
  glClearColor(0.2, 0.2, 0.2, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  //Со временем здесь будет полноценный ренедер сцены
  if assigned(Render) then begin
    Render.ProcessScene(FDemoScene.SceneGraph);
  end;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  Form2.Caption := Format('FPS: %f', [GLViewer1.FPS]);
end;

end.
