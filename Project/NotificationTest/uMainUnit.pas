unit uMainUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFNDEF FPC}
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  {$ELSE}
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
  StdCtrls,
  {$ENDIF}

  uGLViewer,
  uBaseGL,
  uBaseTypes,
  uVMath,
  uLists,
  uPrimitives,
  uMiscUtils,
  uRenderResource,
  uBaseRenders,
  uGLRenders;

type
  TForm1 = class(TForm)
    GLViewer1: TGLViewer;
    Button1: TButton;
    procedure GLViewer1ContextReady(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GLViewer1Render(Sender: TObject);
  private
    { Private declarations }
    Mesh: TVertexObject;
    Shader1: TGLSLShaderProgram;
    Drawer: TGLVertexObject;
    PositionAttr: TAttribBuffer;
    ColorAttr: TAttribBuffer;
    Vertices: TVec2List;
    Colors: TVec3List;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  uPersistentClasses, uMeshUtils;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
//  Vertices.Destroy;
//  Colors.Destroy;
//  Shader1.Destroy;
//  PositionAttr.Destroy;
//  ColorAttr.Destroy;
//  Mesh.Destroy;
//  Drawer.Destroy;
  GLViewer1.OnRender := nil;
  GLViewer1.Context.Deactivate;
end;

procedure TForm1.GLViewer1ContextReady(Sender: TObject);
var
  ver: TApiVersion;
  path: string;
  I: Integer;
  x, y: Single;
  color: TVector;
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
  Shader1.FreeingBehavior := fbNoSubscibers;
  Shader1.AttachShaderFromFile(stVertex, path + 'ColorShader.Vert');
  Shader1.AttachShaderFromFile(stFragment, path + 'ColorShader.Frag');

  Shader1.LinkShader;
  if Shader1.Error then begin
    ShowMessage(Shader1.Log);
    Halt(0);
  end;

  Mesh := TVertexObject.Create;
  PositionAttr := TAttribBuffer.CreateAndSetup(CAttribSematics[atVertex].Name, 2);
  PositionAttr.SetAttribSemantic(atVertex);
  ColorAttr := TAttribBuffer.CreateAndSetup(CAttribSematics[atColor].Name, 3);
  ColorAttr.SetAttribSemantic(atColor);

  Vertices := TVec2List.Create;
  Colors := TVec3List.Create;
  Mesh.FaceType := ftLineStrip;

  for I := 0 to 2000 do begin
    x := (i/10)-100; y := 100*sin(I/2000*2*Pi);
    color.SetVectorTmp(vtRandom);
    Vertices.Add(TVector.Make(x, y).Vec2);
    Colors.Add(color.Vec3);
    Mesh.AddPoint(I);
  end;

  PositionAttr.Buffer.SetDataHandler(Vertices);
  ColorAttr.Buffer.SetDataHandler(Colors);

  Mesh.AddAttrib(PositionAttr, True);
  Mesh.AddAttrib(ColorAttr);

  Shader1.Apply;
  with GLViewer1 do
    Shader1.SetUniform('Projection',
      TMatrix.OrthoMatrix(-Width / 2, Width / 2, -Height / 2,  Height / 2, -1, 1).Matrix4);
  Shader1.UnApply;

  Drawer := TGLVertexObject.CreateFrom(Mesh);
  Drawer.Shader := Shader1;
end;

procedure TForm1.GLViewer1Render(Sender: TObject);
begin
  GLViewer1.Context.ClearDevice;
  Drawer.RenderVO;
end;

end.