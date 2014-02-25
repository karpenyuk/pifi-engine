unit uTest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls,
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

uses Math, uMain, uRenderResource;

procedure TForm1.ArcDial1Change(Sender: TObject);
var
  obj: TSceneObject;
  a: Single;
begin
  obj := TSceneObject(Demo.SceneGraph.Items[1]);
  a := 180 * obj.TurnAngle / pi;
  a := ArcDial1.Value - a;
  obj.TurnObject(pi*a/180);
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
