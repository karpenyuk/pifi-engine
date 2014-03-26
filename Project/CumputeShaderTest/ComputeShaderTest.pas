unit ComputeShaderTest;

interface

uses
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
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
    rgUsage: TRadioGroup;
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
    procedure FormCreate(Sender: TObject);
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
  ComputeMV: TGLSLShaderProgram;
  UBOShader: TGLSLShaderProgram;
  SSBOShader: TGLSLShaderProgram;

  cameraPos: TVector;
  Model, View, Proj: TMatrix;
  MVP: TMatrix;

  Box: TGLVertexObject;

  GL1xRender: Boolean = false;

  Scale: TVector;

  Instances: array of mat4;
  InstMVP: array of TMatrix;

  ssbo: TGLBufferObject;
  mvbo: TGLBufferObject;

  mvBlock: TGLUniformBlock;

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
  ComputeMV.Free;
  UBOShader.Free;
  SSBOShader.Free;
  glFinish;
  GLViewer1.OnRender := nil;
  GLViewer1.Context.Deactivate;
  ssbo.Free;
  mvbo.Free;
  log.Free;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  GLViewer1.Context.DebugContext := True;
end;

procedure TForm3.GLViewer1ContextReady(Sender: TObject);
var
  ver: TApiVersion;
  i, j, k: Integer;
  M: TMatrix;
  path: string;
begin
  log := TStringList.Create;
//   glDebugMessageCallbackARB(DebugCallback, nil);
//   glDebugMessageControlARB(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, nil, true);
//   glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB);
  // Checking OpenGL Version
  with GLViewer1.Context do
    if (MaxMajorVersion < 4) or (MaxMinorVersion < 2) then begin
      GL1xRender := true;
      ver.GAPI := avGL;
      ver.Version := 130;
    end else begin
      ver.GAPI := avGL;
      ver.Version := 420;
      GL1xRender := false;
    end;

{$IFDEF MSWindows}
  path := '..\..\Source\Media\'; { :-\ }
{$ENDIF}
{$IFDEF Linux}
  path := '../../Source/Media/'; { :-/ }
{$ENDIF}
  // Create default Shader with gradient FragColor output
  if not GL1xRender then begin
    Shader1 := TGLSLShaderProgram.Create;

    Shader1.AttachShaderFromFile(stVertex, path + 'Shader.Vert');
    Shader1.AttachShaderFromFile(stFragment, path + 'Shader.Frag');
    Shader1.LinkShader;
    if Shader1.Error then begin
      showmessage(Shader1.log);
      Halt(0);
    end;
  end
  else Shader1 := nil;

  if not GL1xRender then
  begin
    ComputeMV := TGLSLShaderProgram.Create;
    ComputeMV.AttachShaderFromFile(stCompute, path + 'MVTransform.Comp');

    ComputeMV.LinkShader;
    if ComputeMV.Error then begin
      showmessage(ComputeMV.log);
      Halt(0);
    end;
    CheckOpenGLError;
  end
  else ComputeMV := nil;

  if not GL1xRender then begin
    UBOShader := TGLSLShaderProgram.Create;
    UBOShader.AttachShaderFromFile(stVertex, path + 'UBOTransform.Vert');
    UBOShader.AttachShaderFromFile(stFragment, path + 'Shader.Frag');
    UBOShader.LinkShader;
    if UBOShader.Error then begin
      showmessage(UBOShader.log);
      Halt(0);
    end;
    mvBlock := UBOShader.UniformBlocks.GetUBOByName('MVMatrix');
  end
  else UBOShader := nil;

  if not GL1xRender then begin
    SSBOShader := TGLSLShaderProgram.Create;
    SSBOShader.AttachShaderFromFile(stVertex, path + 'SSBOTransform.Vert');
    SSBOShader.AttachShaderFromFile(stFragment, path + 'Shader.Frag');
    SSBOShader.LinkShader;
    if SSBOShader.Error then begin
      showmessage(SSBOShader.log);
      Halt(0);
    end;
  end
  else SSBOShader := nil;

  // Making MVP Matrix
  Proj := TMatrix.PerspectiveMatrix(60, GLViewer1.Width / GLViewer1.Height,
    0.1, 100);
  cameraPos := TVector.Make(3, 2, -4);
  View := TMatrix.LookAtMatrix(cameraPos, VecNull, vecY);

  Scale := vector(2, 2, 2, 1);
  Model := TMatrix.ScaleMatrix(Scale);

  // Box := TGLVertexObject.CreateFrom(uPrimitives.CreateBox(0.1, 0.1, 0.1));
  Box := TGLVertexObject.CreateFrom(uPrimitives.CreateSphere(0.1, 16, 32));
  Box.Shader := Shader1;

  // Fill buffer with instance model matrix
  setlength(Instances, ObjCount);
  setlength(InstMVP, ObjCount);
  for i := 0 to 9 do
    for j := 0 to 9 do
      for k := 0 to 9 do begin
        M := TMatrix.TranslationMatrix(Scale * vector(i * 0.12 - 0.6,
          j * 0.12 - 0.6, k * 0.12 - 0.6));
        Instances[ObjectIndex(i, j, k, 10, 10, 10)] := M.Matrix4;
      end;

  ssbo := TGLBufferObject.Create(btShaderStorage);
  ssbo.Allocate(sizeof(mat4) * length(Instances), @Instances[0],
    GL_STREAM_DRAW);

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
  if Shift = [Classes.ssLeft] then begin
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
  i, j, n, M, k, count: Integer;
  t: double;
  wgs, wgc: vec3i;
begin
  wgc := GetWorkgroupCount;
  wgs := GetWorkgroupSize;

  cameraPos.RotateAround(VecNull, vecY, MY - CY, MX - CX);
  View := TMatrix.LookAtMatrix(cameraPos, VecNull, vecY);
  MX := CX;
  MY := CY;
  Model := Model * TMatrix.RotationMatrix(vector(1, 1, 1, 1),
    GLViewer1.DeltaTime * 1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  if RadioButton1.Checked then begin
    Box.Shader := Shader1;
    t := _GetTime;
    for i := 0 to high(Instances) do
        InstMVP[i] := Model * Instances[i] * View;

    t := _GetTime - t;
    Label3.Caption := floattostr(t * 1000);

    Shader1.Bind;
    Shader1.SetUniform('ProjMatrix', Proj.Matrix4);
    for i := 0 to high(Instances) do begin
      Shader1.SetUniform('ModelView', InstMVP[i].Matrix4);
      Box.RenderVO;
    end;
    Shader1.UnBind;

  end else begin
    t := _GetTime;
    ssbo.Upload(@Instances[0], sizeof(mat4) * length(Instances), 0);

    ssbo.BindAllRange(3);
    mvbo.BindAllRange(1);

    ComputeMV.Bind;

    ComputeMV.SetUniform('ViewMatrix', View.Matrix4);
    ComputeMV.SetUniform('ProjMatrix', Proj.Matrix4);
    ComputeMV.SetUniform('localMatrix', Model.Matrix4);

    glDispatchCompute(ObjCount div WorkGroupSize, 1, 1);

    glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT);

    ComputeMV.UnBind;

    if rgUsage.ItemIndex = 0 then begin
      t := _GetTime - t;
      Label3.Caption := floattostr(t * 1000);

      Box.Shader := SSBOShader;
      SSBOShader.Bind;
      SSBOShader.SetUniform('ProjMatrix', Proj.Matrix4);

      for i := 0 to ObjCount - 1 do begin
        SSBOShader.SetUniform('ObjectIndex', i);
        Box.RenderVO;
      end;
      SSBOShader.UnBind;
    end;

    if rgUsage.ItemIndex = 1 then begin
      Box.Shader := UBOShader;
      UBOShader.Bind;
      UBOShader.SetUniform('ProjMatrix', Proj.Matrix4);

      count := mvBlock.BlockSize div sizeof(mat4);

      n := ObjCount div count;
      M := ObjCount mod count;
      if M <> 0 then inc(n);
      k := count;

      mvbo.UnBindBuffer;
      t := _GetTime - t;
      Label3.Caption := floattostr(t * 1000);

      for i := 0 to n - 1 do begin
        if (i = n - 1) and (M <> 0) then k := M;
        for j := 0 to k - 1 do begin
          mvbo.BindRange(btUniform, 1, i * mvBlock.BlockSize,
            mvBlock.BlockSize);
          UBOShader.SetUniform('ObjectIndex', j);
          Box.RenderVO;
        end;
      end;
      UBOShader.UnBind;
    end;

    if rgUsage.ItemIndex = 2 then begin
      mvbo.Download(@InstMVP[0],sizeof(mat4) * length(Instances),0, false);
      t := _GetTime - t;
      Label3.Caption := floattostr(t * 1000);
      Box.Shader := Shader1;
      Shader1.Bind;
      Shader1.SetUniform('ProjMatrix', Proj.Matrix4);
      for i := 0 to high(Instances) do begin
        Shader1.SetUniform('ModelView', InstMVP[i].Matrix4);
        Box.RenderVO;
      end;
      Shader1.UnBind;
    end;
  end;
end;

procedure TForm3.RadioButton2Click(Sender: TObject);
begin
  log.Add('Compute Shader Activated!');
end;

procedure TForm3.Timer1Timer(Sender: TObject);
begin
  Form3.Caption := Format('FPS: %f', [GLViewer1.FPS]);
  GLViewer1.ResetFPSCounter;
end;

end.
