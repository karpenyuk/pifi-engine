unit uFTtest;

interface

uses
  {$IFDEF FPC}
  {$IFDEF MSWindows}
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
  {$ELSE}
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  {$ENDIF}

  uGLViewer,
  uBaseGL,
  uBaseTypes,
  uVMath,
  dglOpenGL,
  ExtCtrls,
  uPrimitives,
  uMiscUtils,
  uRenderResource,
  uBaseRenders,
  uGLRenders,
  uVectorFont;

type

  { TForm5 }

  TForm5 = class(TForm)
    GLViewer1: TGLViewer;
    Timer1: TTimer;
    procedure GLViewer1ContextReady(Sender: TObject);
    procedure GLViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);

    procedure GLViewer1CanResize(Sender: TObject;
      var NewWidth, NewHeight: Integer; var Resize: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GLViewer1Render(Sender: TObject);
  private
    { Private declarations }
    MX, MY: Integer;
  public
    { Public declarations }
  end;

var
  Form5: TForm5;
  cameraPos: TVector;
  Model, View, Proj: TMatrix;
  MVP: TMatrix;

  Mesh: TVertexObject;
  Shader1: TGLSLShaderProgram;
  TextObject: TGLVertexObject;

implementation

{$R *.dfm}

procedure TForm5.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  glFinish;
  Shader1.Destroy;
  TextObject.Destroy;
  Mesh.Destroy;
  GLViewer1.OnRender := nil;
  GLViewer1.Context.Deactivate;
end;

procedure TForm5.GLViewer1CanResize(Sender: TObject;
  var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
  Proj := TMatrix.PerspectiveMatrix(60, NewWidth / NewHeight, 0.1, 100);
end;

procedure TForm5.GLViewer1ContextReady(Sender: TObject);
const
  strGOST = 'GOST';
  strText: UnicodeString = 'QERTY …÷” ≈Õ';
var
  ver: TApiVersion;
  path: string;

  cChars: TCharSet;
  StatrGlyph: WideChar;
  StopGlyph: WideChar;
  bb: TExtents;

  procedure AddToCharSet;
  var ch:  WideChar;
  len: Integer;
  begin
    len := Length(cChars);
    SetLength(cChars, len + Ord(StopGlyph) - Ord(StatrGlyph)+1);
    for ch := StatrGlyph to StopGlyph  do
      cChars[len + Ord(ch) - Ord(StatrGlyph)] := ch;
  end;

begin
  ver.GAPI := avGL;
  ver.Version := 330;
  vRegisteredRenders.GetCompatibleRender(ver);

  {$IFDEF MSWindows}
  path := '..\..\Source\Media\'; { :-\ }
  {$ENDIF}
  {$IFDEF Linux}
  path := '../../Source/Media/'; { :-/ }
  {$ENDIF}

  Shader1 := TGLSLShaderProgram.Create;
  Shader1.AttachShaderFromFile(stVertex, path+'Shader.Vert');
  Shader1.AttachShaderFromFile(stFragment, path+'Shader.Frag');

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
  Model.SetIdentity;

  StatrGlyph:= ' ';
  StopGlyph:= 'Z';
  AddToCharSet;
  StatrGlyph:= '¿';
  StopGlyph:= 'ﬂ';
  AddToCharSet;

  VectorFontLibrary.BuildFontFromFile(strGOST, path + 'GOST type A.ttf', cChars, 1, 0.2);
  Mesh := VectorFontLibrary.CreateText(strGOST, strText);
  bb := VectorFontLibrary.GetExtents(strGOST, strText);
  Model := TMatrix.TranslationMatrix(bb.eMid.Negate);
  TextObject := TGLVertexObject.CreateFrom(Mesh);
  TextObject.Shader := Shader1;
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
var
  MV, MVP: TMatrix;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  MV := Model * View;
  MVP := MV * Proj;
  Shader1.Apply;
  Shader1.SetUniform('ModelView', MV.Matrix4);
  Shader1.SetUniform('MVP', MVP.Matrix4);
  TextObject.RenderVO();
  Shader1.UnApply;
end;

procedure TForm5.Timer1Timer(Sender: TObject);
begin
  Caption := Format('FPS: %f', [GLViewer1.FPS]);
  GLViewer1.ResetFPSCounter;
end;

end.
