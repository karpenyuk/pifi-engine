{
 1. Поместить геометрию всего шрифта в один буфер
 2. Сформировать второй буфер в котором будет храниться информация о символах в шрифте: FirstIndex + IndexCount
 3. Сформировать буфер отображения текста на шрифт (чтоб не хранить 65536 символов юникода просто создаем буфер в котором будет указано какие из символов реализованы в шрифте и под каким номером)
 4. Передаем строку текста, информацию о символах и буфер отображения в вычислительный шейдер.
 5. в вычислительном шейдере, на основе полученных данных, формируется буфер DrawElementsIndirectCommands
 6. Биндится буфер DrawElementsIndirectCommands
 7. Выводится весь текст за один вызов через glMultiDrawElementsIndirect
}

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
  uVectorFont,
  uLists;

const
  strGOST2D = 'GOST2D';
  strGOST3D = 'GOST3D';
  strText: UnicodeString = 'QERTY ЙЦУКЕН';

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
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    MX, MY: Integer;
    Text: string;
    Changed: Boolean;
    procedure GenIndirectBuffer;
  public
    { Public declarations }
  end;

var
  Form5: TForm5;
  cameraPos: TVector;
  Model, View, Proj, OrthoProj: TMatrix;
  MVP: TMatrix;

  Mesh, Indi, FontMesh: TVertexObject;
  FontMap: TBufferObject;
  IndiGLBuffer: TGLBufferObject;


  SymbolOffesetAttr: TAttribBuffer;
  SymbolOffesetBuffer: TGLBufferObject;
  Shader1, Shader2: TGLSLShaderProgram;
  TextObject, IndiObject: TGLVertexObject;
  FontGLMesh: TGLVertexObject;

implementation

{$R *.dfm}

uses
  uDataAccess;

const
   batch: packed array[0..1, 0..3] of GLuint =
   ((3, 1, 0, 0), (3, 1, 3, 1));
//        typedef struct {
//          GLuint count;
//          GLuint primCount;
//          GLuint first;
//          GLuint baseInstance;
//        } DrawArraysIndirectCommand;

//        typedef struct {
//          GLuint count;
//          GLuint primCount;
//          GLuint firstIndex;
//          GLint  baseVertex;
//          GLuint baseInstance;
//        } DrawElementsIndirectCommand;

type
  TDrawElementsIndirectCommand = record
    count: GLuint;
    primCount: GLuint;
    firstIndex: GLuint;
    baseVertex: GLint;
    baseInstance: GLuint;
  end;

var
  Commands: array of TDrawElementsIndirectCommand;

procedure TForm5.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  glFinish;
  Shader1.Free;
  TextObject.Free;
  Mesh.Free;
  GLViewer1.OnRender := nil;
  GLViewer1.Context.Deactivate;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  GLViewer1.Context.DebugContext := true;
  GLViewer1.Context.ForwardContext := true;
end;

procedure TForm5.GenIndirectBuffer;
var
  pos: TVec2List;
  VDA: IVectorDataAccess;
  I, C, P: Integer;
  Offset, Len: Integer;
  V: TVector;
  addr: PByte;
begin
  pos := VectorFontLibrary.CreatePositionList(strGOST2D, Text, 500, 4);
  FontGLMesh.Attribs[1].Buffer.Upload(pos.Data, pos.Size, 0);


  VDA := TVectorDataAccess.Create(FontMap.Data, vtUInt, 2, 2*SizeOf(Integer), 65535);
  SetLength(Commands, Length(Text));
  P := 0;
  for I := 1 to Length(Text) do
  begin
    C := Integer(Text[I]);
    V := VDA.Items[C];
    Offset := Round(V.X);
    Len := Round(V.Y);
    if Len > 0 then
    begin
      Commands[P].count := Len;
      Commands[P].primCount := 1;
      Commands[P].firstIndex := Offset;
      Commands[P].baseVertex := 0;
      Commands[P].baseInstance := P;
      Inc(P);
    end;
  end;
  SetLength(Commands, P);

  if not Assigned(IndiGLBuffer) then begin
    IndiGLBuffer := TGLBufferObject.Create(btDrawIndirect);
    IndiGLBuffer.Allocate(SizeOf(TDrawElementsIndirectCommand)*500, nil, GL_STREAM_DRAW);
  end;

//  IndiGLBuffer.Upload(@Commands[0], Length(Commands)*SizeOf(TDrawElementsIndirectCommand), 0);
  addr := IndiGLBuffer.MapRange(GL_MAP_WRITE_BIT or GL_MAP_INVALIDATE_RANGE_BIT,
    0, Length(Commands)*SizeOf(TDrawElementsIndirectCommand));
  Move(Commands[0], addr^, Length(Commands)*SizeOf(TDrawElementsIndirectCommand));
  IndiGLBuffer.UnMap;

  Changed := False;
end;

procedure TForm5.GLViewer1CanResize(Sender: TObject;
  var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
  Proj := TMatrix.PerspectiveMatrix(60, NewWidth / NewHeight, 0.1, 100);
  OrthoProj := TMatrix.OrthoMatrix(0, NewWidth, 0,  NewHeight, -1, 1);
end;

procedure TForm5.GLViewer1ContextReady(Sender: TObject);
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

  Shader2 := TGLSLShaderProgram.Create;
  Shader2.AttachShaderFromFile(stVertex, path+'IndirectShader.Vert');
  Shader2.AttachShaderFromFile(stFragment, path+'ColorShader.Frag');

  Shader2.LinkShader;

  if Shader2.Error then
  begin
    showmessage(Shader2.Log);
    Halt(0);
  end;

  Proj := TMatrix.PerspectiveMatrix(60, GLViewer1.Width / GLViewer1.Height,
    0.1, 100);
  cameraPos := TVector.Make(0, 0, 5);
  View := TMatrix.LookAtMatrix(cameraPos, VecNull, vecY);
  Model.SetIdentity;
  OrthoProj := TMatrix.OrthoMatrix(0, GLViewer1.Width,
  0, GLViewer1.Height, -1, 1);


  StatrGlyph:= ' ';
  StopGlyph:= 'z';
  AddToCharSet;
  StatrGlyph:= 'А';
  StopGlyph:= 'Я';
  AddToCharSet;

  VectorFontLibrary.BuildFontFromFile(strGOST3D, path + 'GOST type A.ttf', cChars, 1, 0.2);
  Mesh := VectorFontLibrary.CreateText(strGOST3D, strText);
  bb := VectorFontLibrary.GetExtents(strGOST3D, strText);
  Model := TMatrix.TranslationMatrix(bb.eMid.Negate);
  TextObject := TGLVertexObject.CreateFrom(nil, Mesh);
  TextObject.Shader := Shader1;

  VectorFontLibrary.BuildFontFromFile(strGOST2D, path + 'GOST type A.ttf', cChars, 30, 0);
  FontMesh := VectorFontLibrary.CreateFont(strGOST2D, False, False);
  SymbolOffesetAttr := TAttribBuffer.CreateAndSetup(CAttribSematics[atTexCoord1].Name, 2,
    vtFloat, 0, btArray);
  SymbolOffesetAttr.Buffer.Allocate(510*2*SizeOf(Single), nil);
  SymbolOffesetAttr.SetAttribSemantic(atTexCoord1);
  FontMesh.AddAttrib(SymbolOffesetAttr, False);

  FontGLMesh := TGLVertexObject.CreateFrom(nil, FontMesh);
  FontGLMesh.Build(Shader2.Id);
  FontMap := VectorFontLibrary.CreateFontMap(strGOST2D, False);


  Changed := True;
  Text := 'Let begin';

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
  if Shift = [Classes.ssLeft] then
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
  if Changed then
    GenIndirectBuffer;

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  MV := Model * View;
  MVP := MV * Proj;
  Shader1.Bind;
  Shader1.SetUniform('ProjMatrix', Proj.Matrix4);
  Shader1.SetUniform('ModelView', MV.Matrix4);
  Shader1.SetUniform('MVP', MVP.Matrix4);
  TextObject.RenderVO();
  Shader1.UnBind;

  if Length(Commands) > 0 then
  begin
    glDisable(GL_DEPTH_TEST);
    Shader2.Bind;
    Shader2.SetUniform('Projection', OrthoProj.Matrix4);
    Shader2.SetUniform('Origin', TVector.Make(0, Height - 60).Vec2);
    glBindVertexArray(FontGLMesh.VAOid);
    glVertexAttribDivisor(4, 1);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FontGLMesh.IndiceId);
    glVertexAttrib3f(3, 1, 1, 1);
    IndiGLBuffer.Bind;
    glMultiDrawElementsIndirect(GL_TRIANGLES, GL_UNSIGNED_INT, nil, Length(Commands), 0);
    glBindVertexArray(0);
    Shader2.UnBind;
    glEnable(GL_DEPTH_TEST);
  end;
end;

procedure TForm5.Timer1Timer(Sender: TObject);
var
  I, C: Integer;
begin
  Caption := Format('FPS: %f', [GLViewer1.FPS]);
  GLViewer1.ResetFPSCounter;

  C := 10 + Round(Random(500));
  SetLength(Text, C);
  for I := 1 to C do
    Text[I] := Char(Round(Random(Ord('z') - Ord(' '))));
  Changed := True;
end;

end.
