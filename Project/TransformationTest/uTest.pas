unit uTest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
//  FMX.StdCtrls,
  FMX.Platform.Win, Windows, uGLWindow;

type

  TForm1 = class(TForm)
    ArcDial1: TArcDial;
    ArcDial2: TArcDial;
    ArcDial3: TArcDial;
    ArcDial4: TArcDial;
    ArcDial5: TArcDial;
    ArcDial6: TArcDial;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ArcDial1Change(Sender: TObject);
    procedure ArcDial2Change(Sender: TObject);
    procedure ArcDial3Change(Sender: TObject);
    procedure ArcDial4Change(Sender: TObject);
    procedure ArcDial5Change(Sender: TObject);
    procedure ArcDial6Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  glwnd: TGLWindow;
  OldWindowProc: Pointer = nil;
  NewWindowProc: Pointer = nil;
//  nwp: Integer;

implementation

{$R *.fmx}

uses Math, uvMath, uMain, uRenderResource;

procedure TForm1.ArcDial1Change(Sender: TObject);
var
  obj: TSceneObject;
begin
  obj := TSceneObject(Demo.SceneGraph.Items['Base']);
  obj.TurnAngle := ArcDial1.Value;
end;

procedure TForm1.ArcDial2Change(Sender: TObject);
var
  obj: TSceneObject;
begin
  obj := TSceneObject(Demo.SceneGraph.Items['Hinge1']);
  obj.RollAngle := ArcDial2.Value;
end;

procedure TForm1.ArcDial3Change(Sender: TObject);
var
  obj: TSceneObject;
begin
  obj := TSceneObject(Demo.SceneGraph.Items['Hinge2']);
  obj.RollAngle := ArcDial3.Value;
end;

procedure TForm1.ArcDial4Change(Sender: TObject);
var
  obj: TSceneObject;
begin
  obj := TSceneObject(Demo.SceneGraph.Items['Hinge3']);
  obj.RollAngle := ArcDial4.Value;
end;

procedure TForm1.ArcDial5Change(Sender: TObject);
var
  obj: TSceneObject;
begin
  obj := TSceneObject(Demo.SceneGraph.Items['Hinge3']);
  obj.TurnAngle := ArcDial5.Value;
end;

procedure TForm1.ArcDial6Change(Sender: TObject);
var
  s: Single;
  obj: TSceneObject;
begin
  s := 1.5*Cos(DegToRad(ArcDial6.Value)*0.5)+0.15;
  obj := TSceneObject(Demo.SceneGraph.Items['Finger1']);
  obj.MoveObject(Vector(s, 1.65, 0));
  obj := TSceneObject(Demo.SceneGraph.Items['Finger2']);
  obj.MoveObject(Vector(-s, 1.65, 0));
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer1.Enabled := false;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  OldWindowProc:= Pointer(GetWindowLong(FmxHandleToHWND(self.Handle), GWL_WNDPROC));
//  NewWindowProc:= Pointer(SetWindowLong(FmxHandleToHWND(self.Handle), GWL_WNDPROC, nwp));
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  glwnd.DrawGLScene;
  if glwnd.FrameTime <> 0 then
    glwnd.Caption := '[' +
      floattostr(roundto(1 / glwnd.FrameTime, 2)) + ']'
  else
    glwnd.Caption := '[NAN]';
  glwnd.SwapBuffer;
end;

end.
