unit LowLevel3DFrm;

interface

uses
  System.SysUtils,
  FMX.Controls, FMX.Forms, FMX.Types3D, FMX.Forms3D,
  {$IFDEF IOS}
  iOSapi.OpenGLES,
  {$ENDIF}
  {$IFDEF ANDROID}
  Androidapi.Egl, Androidapi.Gles2, Androidapi.Gles2ext,
  {$ENDIF}
  FMX.Types, FMX.Graphics, System.Classes,
  uBaseGLES2;


type
  TForm1 = class(TForm3D)
    procedure Form1Render(Sender: TObject; const Context: TContext3D);
    procedure Timer1(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TForm1 }

procedure TForm1.Form1Render(Sender: TObject; const Context: TContext3D);
begin
  // render
  glClearColor(abs(sin(Tag)),0.0,0.0,1.0);
  glClear(GL_COLOR_BUFFER_BIT);
end;

procedure TForm1.Timer1(Sender: TObject);
begin
  Tag := Tag + 1;
  Invalidate;
end;

end.
