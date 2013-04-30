unit ComputeShaderTest;

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
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
  StdCtrls;

type
  TForm3 = class(TForm)
    GLViewer1: TGLViewer;
    Timer1: TTimer;
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Label1: TLabel;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
      var Resize: Boolean);
    procedure GLViewer1ContextReady(Sender: TObject);
    procedure GLViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLViewer1Render(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RadioButton2Click(Sender: TObject);
  private
    { Private declarations }
    MX, MY, CX, CY: Integer;
    startTime, stopTime: GLint64;
    queryID: array [0 .. 1] of cardinal;
    function GetCommandTime: double;
  public
    { Public declarations }
  end;

const
  ObjCount = 1024;
  WorkGroupSize = 32;

var
  Form3: TForm3;

  Shader1: TGLSLShaderProgram;
  Compute: TGLSLShaderProgram;
  UBOShader: TGLSLShaderProgram;

  cameraPos: TVector;
  Model, View, Proj: TMatrix;
  MVP: TMatrix;

  Box: TGLVertexObject;

  GL1xRender: Boolean = false;

  Render: TBaseRender;
  SceneGraph: TSceneGraph;

  Scale: TVector;

  Instances: array of mat4;
  InstMVP: array of TMatrix;

  ssbo: TGLBufferObject;
  mvbo: TGLBufferObject;
  mvpbo: TGLBufferObject;

  mvpBlock, mvBlock: TGLUniformBlock;

  A, B, C: array of mat4;

  TestTime: double;

  log: TStringList;

implementation

{$R *.dfm}

procedure DebugCallback(source: GLenum; type_: GLenum; id: GLuint;
  severity: GLenum; length: GLsizei; const message_: PGLchar;
  userParam: PGLvoid); stdcall;
var
  msg: ansistring;
begin
  msg := ParseDebugMessage(source, type_, id, severity, message_);
  log.Add(string(msg));
  if severity = GL_DEBUG_SEVERITY_HIGH_ARB then
    log.SaveToFile('DebugLog.txt');
  assert(not(severity = GL_DEBUG_SEVERITY_HIGH_ARB), string(msg));
end;

function TForm3.GetCommandTime: double;
var
  stopTimerAvailable: cardinal;
begin
  stopTimerAvailable := 0;
  while (stopTimerAvailable = 0) do
    glGetQueryObjectiv(queryID[1], GL_QUERY_RESULT_AVAILABLE,
      @stopTimerAvailable);
  glGetQueryObjectui64v(queryID[0], GL_QUERY_RESULT, @startTime);
  glGetQueryObjectui64v(queryID[1], GL_QUERY_RESULT, @stopTime);
  result := (stopTime - startTime) / 1000;
end;

function ObjectIndex(i, j, k: Integer; w, h, d: Integer): Integer;
begin
  result := h * w * k + j * w + i;
end;

procedure TForm3.FormCanResize(Sender: TObject;
  var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
  Proj := TMatrix.PerspectiveMatrix(60, NewWidth / NewHeight, 0.1, 100);
end;

procedure TForm3.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Shader1.Free;
  Compute.Free;
  UBOShader.Free;
  glFinish;
  GLViewer1.OnRender := nil;
  GLViewer1.Context.Deactivate;
  ssbo.Free;
  mvbo.Free;
  mvpbo.Free;
  log.Free;
end;

procedure TForm3.GLViewer1ContextReady(Sender: TObject);
var
  ver: TApiVersion;
  i, j, k: Integer;
  M: TMatrix;
  path:string;
begin
  log := TStringList.Create;
//  glDebugMessageCallback(DebugCallback, nil);
//  glDebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, nil, true);
//  glEnable(GL_DEBUG_OUTPUT);
//  glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB);
  // Checking OpenGL Version
  with GLViewer1.Context do
    if (MaxMajorVersion < 4) or (MaxMinorVersion < 3) then
    begin
      GL1xRender := true;
      ver.GAPI := avGL;
      ver.Version := 130;
    end
    else
    begin
      ver.GAPI := avGL;
      ver.Version := 430;
      GL1xRender := false;
    end;
  // Среди зарегистрированных рендеров выбираем подходящий
  Render := vRegisteredRenders.GetCompatibleRender(ver);

  {$IFDEF MSWindows}
  path := '..\..\Source\Media\'; { :-\ }
  {$ENDIF}
  {$IFDEF Linux}
  path := '../../Source/Media/'; { :-/ }
  {$ENDIF}

  // Create default Shader with gradient FragColor output
  if not GL1xRender then
  begin
    Shader1 := TGLSLShaderProgram.Create;

    Shader1.AttachShaderFromFile(stVertex,
      path+'Shader.Vert');
    Shader1.AttachShaderFromFile(stFragment,
      path+'Shader.Frag');
    Shader1.LinkShader;
    if Shader1.Error then
    begin
      showmessage(Shader1.log);
      Halt(0);
    end;
  end
  else
    Shader1 := nil;

  if not GL1xRender then
  begin
    Compute := TGLSLShaderProgram.Create;

    Compute.AttachShaderFromFile(stCompute,
      path+'MVPTransform.Comp');

    Compute.LinkShader;
    if Compute.Error then
    begin
      showmessage(Compute.log);
      Halt(0);
    end;
    CheckOpenGLError;
  end
  else
    Compute := nil;

  if not GL1xRender then
  begin
    UBOShader := TGLSLShaderProgram.Create;
    UBOShader.AttachShaderFromFile(stVertex,
      path+'MVPTransform.Vert');
    UBOShader.AttachShaderFromFile(stFragment,
      path+'Shader.Frag');
    UBOShader.LinkShader;
    if UBOShader.Error then
    begin
      showmessage(UBOShader.log);
      Halt(0);
    end;
    // mvpBlock:=UBOShader.UniformBlocks.GetUBOByName('MVPMatrix');
    // mvBlock:=UBOShader.UniformBlocks.GetUBOByName('MVMatrix');
  end
  else
    UBOShader := nil;

  // Making MVP Matrix
  Proj := TMatrix.PerspectiveMatrix(60, GLViewer1.Width / GLViewer1.Height,
    0.1, 100);
  cameraPos := TVector.Make(3, 2, -4);
  View := TMatrix.LookAtMatrix(cameraPos, VecNull, vecY);

  Scale := vector(2, 2, 2, 1);
  Model := TMatrix.ScaleMatrix(Scale);

  Box := TGLVertexObject.CreateFrom(uPrimitives.CreateBox(0.1, 0.1, 0.1));
  Box.Shader := Shader1;

  SceneGraph := TSceneGraph.Create;

  // Fill buffer with instance model matrix
  setlength(Instances, ObjCount);
  setlength(InstMVP, ObjCount);
  for i := 0 to 9 do
    for j := 0 to 9 do
      for k := 0 to 9 do
      begin
        M := TMatrix.TranslationMatrix(Scale * vector(i * 0.12 - 0.6,
          j * 0.12 - 0.6, k * 0.12 - 0.6));
        Instances[ObjectIndex(i, j, k, 10, 10, 10)] := M.Matrix4;
      end;

  setlength(A, ObjCount);
  setlength(B, ObjCount);
  setlength(C, ObjCount);

  ssbo := TGLBufferObject.Create(btShaderStorage);
  ssbo.Allocate(sizeof(mat4) * length(Instances), @Instances[0], GL_STREAM_DRAW);

  mvpbo := TGLBufferObject.Create(btShaderStorage);
  mvpbo.Allocate(sizeof(mat4) * length(Instances), nil, GL_STREAM_COPY);

  mvbo := TGLBufferObject.Create(btShaderStorage);
  mvbo.Allocate(sizeof(mat4) * length(Instances), nil, GL_STREAM_COPY);

  glGenQueries(2, @queryID[0]);
end;

procedure TForm3.GLViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MX := X;
  MY := Y;
end;

procedure TForm3.GLViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift = [ssLeft] then
  begin
    CX := X;
    CY := Y;
  end;
end;

function _GetTime: double;
var
  Freq, Tick: Int64;
begin
  QueryPerformanceFrequency(Freq);
  QueryPerformanceCounter(Tick);
  result := Tick / Freq;
end;

procedure TForm3.GLViewer1Render(Sender: TObject);
var
  i, j, n: Integer;
  t: double;
  wgs, wgc: vec3i;
  dcs: Integer;
  loc: Integer;
begin
//  glDisable(GL_DEBUG_OUTPUT); // Много жрет

  wgc := GetWorkgroupCount;
  wgs := GetWorkgroupSize;

  cameraPos.RotateAround(VecNull, vecY, MY - CY, MX - CX);
  View := TMatrix.LookAtMatrix(cameraPos, VecNull, vecY);
  MX := CX;
  MY := CY;
  Model := Model * TMatrix.RotationMatrix(vector(1, 1, 1, 1),
    GLViewer1.DeltaTime * 1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  if RadioButton1.Checked then
  begin
    Box.Shader:=Shader1;
    t := _GetTime;
    for i := 0 to high(Instances) do
      InstMVP[i] :=  Model * Instances[i] * View * Proj;

    t := _GetTime - t;
    Label3.Caption := floattostr(t * 1000);

    Shader1.Apply;

    for i := 0 to high(Instances) do
    begin
      Shader1.SetUniform('MVP', InstMVP[i].Matrix4);
      Box.RenderVO;
    end;
    Shader1.UnApply;

  end
  else
  begin
    t := _GetTime;
    ssbo.Upload(@Instances[0], sizeof(mat4) * length(Instances), 0);


    if CheckBox1.Checked then begin
      //маппинг - падение фпс
      //mvbo.MapRange(GL_MAP_WRITE_BIT + GL_MAP_INVALIDATE_BUFFER_BIT, 0, 0);
      //mvbo.UnMap;
      mvbo.Upload(nil,mvbo.Size,0);
    end;
    ssbo.BindAllRange(3);

    mvbo.BindAllRange(1);

    Compute.Apply;

    Compute.SetUniform('ViewMatrix', View.Matrix4);
    Compute.SetUniform('ProjMatrix', Proj.Matrix4);
    Compute.SetUniform('localMatrix', Model.Matrix4);

    glDispatchCompute(ObjCount div WorkGroupSize, 1, 1);

    glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT);

    Compute.UnApply;

    t := _GetTime - t;
    Label3.Caption := floattostr(t * 1000);
    Box.Shader:=UBOShader;
    UBOShader.Apply;
    UBOShader.SetUniform('ProjMatrix', Proj.Matrix4);

    for i := 0 to ObjCount - 1 do begin
      UBOShader.SetUniform('ObjectIndex', i);
      Box.RenderVO;
    end;
    UBOShader.UnApply;

  end;
end;

procedure TForm3.RadioButton2Click(Sender: TObject);
begin
  Log.Add('Compute Shader Activated!');
end;

procedure TForm3.Timer1Timer(Sender: TObject);
begin
  Form3.Caption := Format('FPS: %f', [GLViewer1.FPS]);
  GLViewer1.ResetFPSCounter;
end;

end.
