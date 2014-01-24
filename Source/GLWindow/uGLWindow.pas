unit uGLWindow;

interface

{$APPTYPE CONSOLE}
{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

uses
  Windows,
  SysUtils,
  Messages,
  Types,
  Classes,
  dglOpenGL,
  uBaseTypes;

const
  cMaxUpdateTime = 1 / 5000;

type

  TGLWindow = class;

  TCadencer = class(TThread)
  private
    FLastTime: double;
    FUpdateTime: double;
    FGLWindow: TGLWindow;
    procedure update;
  protected
    procedure Execute; override;
  end;

  TOnDebugMessage = procedure(const AMessage: string) of object;

  TGLWindow = class
  private
    FKeys: array [0..255] of boolean;
    FButtons: TCMouseButtons;
    FMouseX,FMouseY: integer;
    FActive: boolean;
    FisRendering: boolean;
    FFullScreen: boolean;
    FWnd: HWnd;
    FDC: Hdc;
    FRC: HGLRC;
    FWidth, FHeight: integer;
    FColorBits, FDepthBits, FStensilBits, FAALevel : byte;
    FForwardContext, FDebugContext: boolean;
    FMajorVersion, FMinorVersion: integer;
    FCaption: string;
    FFrameTime: double;
    FonRender: TRenderEvent;
    FonInitialize: TNotifyEvent;
    FonResize: TResizeEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyUp: TKeyEvent;
    FOnDebugMessage: TOnDebugMessage;
    procedure setActive(const Value: boolean);
    procedure KillGLWindow;
    procedure GetOGLVersion;
    function InitGL: boolean;
    function InitContext: boolean;
    procedure WindProc(var Message: TMessage);
    function getKey(index: integer): boolean;
    procedure setKey(index: integer; const Value: boolean);
    function getVSync: boolean;
    procedure setVSync(const Value: boolean);
    procedure setCaption(const Value: string);
    function getFrameTime: double;
    procedure SetonRender(const Value: TRenderEvent);
    procedure SetonInitialize(const Value: TNotifyEvent);
    procedure SetonResize(const Value: TResizeEvent);
    function getButton(index: TCMouseButton): boolean;
    procedure SetOnDebugMessage(const Value: TOnDebugMessage);
  public
    constructor Create;
    destructor Destroy; override;
    procedure CreateWindow(Title: Pchar; width, height: integer;
      FullScreen: boolean);
    procedure SetPixelFormatBits(ColorBits, DepthBits, StensilBits, AALevel: byte;
      aForwardContext, aDebugContext: boolean);
    procedure DoResize(Width, Height: integer);
    procedure SwapBuffer;
    procedure DrawGLScene;

    property Active: boolean read FActive write setActive;
    property isRendering: boolean read FisRendering;
    property Keys[index: integer]: boolean read getKey write setKey;
    property Buttons[index: TCMouseButton]: boolean read getButton;
    property VSync: boolean read getVSync write setVSync;
    property ForwardContext: boolean read FForwardContext;
    property DebugContext: boolean read FDebugContext;
    property Caption: string read FCaption write setCaption;
    property FrameTime: double read getFrameTime;

    property onRender: TRenderEvent read FonRender write SetonRender;
    property onInitialize: TNotifyEvent read FonInitialize write SetonInitialize;
    property OnDebugMessage: TOnDebugMessage read FOnDebugMessage write SetOnDebugMessage;
    property onResize: TResizeEvent read FonResize write SetonResize;
    property onKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property onKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property onMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property onMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property onMouseMove: TMouseEvent read FOnMouseMove write FOnMouseMove;


  end;

implementation

uses
  uLists;

var
  UtilWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @DefWindowProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TPUtilWindow');

function AllocateGLWnd(AMethod: TWndMethod; Title: Pchar;
  width, height: integer; FullScreen: boolean): HWND;
var
  TempClass: TWndClass;
  ClassRegistered: Boolean;
  dwExStyle:dword;
  dwStyle:dword;
  WindowRect: TRect;
begin
  WindowRect.Left := 0;
  WindowRect.Top := 0;
  WindowRect.Right := width;
  WindowRect.Bottom := height;
  UtilWindowClass.hInstance := HInstance;
{$IFDEF PIC}
  UtilWindowClass.lpfnWndProc := @DefWindowProc;
{$ENDIF}
  ClassRegistered := GetClassInfo(HInstance, UtilWindowClass.lpszClassName, TempClass);
  if not ClassRegistered or (TempClass.lpfnWndProc <> @DefWindowProc)
  then begin
    if ClassRegistered then
      Windows.UnregisterClass(UtilWindowClass.lpszClassName, HInstance);
    Windows.RegisterClass(UtilWindowClass);
  end;

  if FullScreen then begin
      dwExStyle:=WS_EX_APPWINDOW;
      dwStyle:=WS_POPUP or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
      Showcursor(false);
  end else begin
      dwExStyle:=WS_EX_APPWINDOW or WS_EX_WINDOWEDGE;
      dwStyle:=WS_OVERLAPPEDWINDOW or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
  end;

  Result := CreateWindowEx(dwExStyle, UtilWindowClass.lpszClassName,
    Title, dwStyle, 0, 0, WindowRect.Right-WindowRect.Left,
    WindowRect.Bottom-WindowRect.Top, 0, 0, HInstance, nil);
  if Assigned(AMethod) then
    SetWindowLong(Result, GWL_WNDPROC, Longint(MakeObjectInstance(AMethod)));
end;

function ParseDebug(aSource, aType, aId, aSeverity: cardinal;
  const aMess: ansistring): ansistring;
var
  t: ansistring;
begin
  case aSource of
    GL_DEBUG_SOURCE_API_ARB:
      t := 'OpenGL';
    GL_DEBUG_SOURCE_WINDOW_SYSTEM_ARB:
      t := 'Windows';
    GL_DEBUG_SOURCE_SHADER_COMPILER_ARB:
      t := 'Shader Compiler';
    GL_DEBUG_SOURCE_THIRD_PARTY_ARB:
      t := 'Third Party';
    GL_DEBUG_SOURCE_APPLICATION_ARB:
      t := 'Application';
    GL_DEBUG_SOURCE_OTHER_ARB:
      t := 'Other';
  end;
  Result := 'Source: ' + t + '; ';

  case aType of
    GL_DEBUG_TYPE_ERROR_ARB:
      t := 'Error';
    GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB:
      t := 'Deprecated behavior';
    GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB:
      t := 'Undefined behavior';
    GL_DEBUG_TYPE_PORTABILITY_ARB:
      t := 'Portability';
    GL_DEBUG_TYPE_PERFORMANCE_ARB:
      t := 'Performance';
    GL_DEBUG_TYPE_OTHER_ARB:
      t := 'Other';
  end;
  Result := Result + 'Type: ' + t + '; ';

  case aSeverity of
    GL_DEBUG_SEVERITY_HIGH_ARB:
      t := 'High';
    GL_DEBUG_SEVERITY_MEDIUM_ARB:
      t := 'Medium';
    GL_DEBUG_SEVERITY_LOW_ARB:
      t := 'Low';
  end;
  Result := Result + 'Severity: ' + t + '; ';
  Result := Result + 'Message: ' + aMess;
end;

procedure GLDebugCallback(source : GLEnum; type_ : GLEnum; id : GLUInt;
             severity : GLUInt; length : GLsizei;
             const message_ : PGLCHar; userParam : PGLvoid);
{$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
var
  window: TGLWindow absolute userParam;
begin
  if Assigned(userParam) and Assigned(window.FOnDebugMessage) then
    window.FOnDebugMessage(string(ParseDebug(Source, type_, id, severity, message_)));
end;

{ TGLWindow }

destructor TGLWindow.Destroy;
begin
  killGLwindow;
  inherited;
end;

procedure TGLWindow.DoResize(Width, Height: integer);
begin
  FWidth:=Width; FHeight:=Height;
  glViewport(0,0,Width,Height);
  glClear(GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
  if Assigned(FonResize) then FonResize(self,FWidth,FHeight);
  
end;

procedure TGLWindow.DrawGLScene;
var err: cardinal;
begin
  if not Active then begin
    FFrameTime:=-1;
    exit;
  end;
  FisRendering := true;
  glClear(GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT);
  FFrameTime:=gettime;
  if assigned(FonRender) then FonRender(self, FFrameTime);
  FisRendering := false;
  err := glGetError;
  assert( err = GL_NO_ERROR, 'OpenGL Error: '+inttostr(err));
  FFrameTime:=(gettime-FFrameTime)*1000;
end;

function TGLWindow.getButton(index: TCMouseButton): boolean;
begin
  result := index in FButtons;
end;

function TGLWindow.getFrameTime: double;
begin
  if FFrameTime<>-1 then result:=FFrameTime
  else result:=0;
end;

function TGLWindow.getKey(index: integer): boolean;
begin
  result := FKeys[index];
end;

function TGLWindow.getVSync: boolean;
begin
  result := boolean(wglGetSwapIntervalEXT);
end;

procedure TGLWindow.GetOGLVersion;
var
  AnsiBuffer: ansistring;
  Buffer: String;

  procedure TrimAndSplitVersionString(Buffer: String; var Max, Min: integer);
  var
    Separator: integer;
  begin
    try
      Separator := Pos('.', Buffer);
      if (Separator > 1) and (Separator < length(Buffer)) and
        (AnsiChar(Buffer[Separator - 1]) in ['0' .. '9']) and
        (AnsiChar(Buffer[Separator + 1]) in ['0' .. '9']) then
      begin
        Dec(Separator);
        while (Separator > 0) and
          (AnsiChar(Buffer[Separator]) in ['0' .. '9']) do
          Dec(Separator);
        Delete(Buffer, 1, Separator);
        Separator := Pos('.', Buffer) + 1;
        while (Separator <= length(Buffer)) and
          (AnsiChar(Buffer[Separator]) in ['0' .. '9']) do
          inc(Separator);
        Delete(Buffer, Separator, 255);
        Separator := Pos('.', Buffer);
        Max := StrToInt(copy(Buffer, 1, Separator - 1));
        Min := StrToInt(copy(Buffer, Separator + 1, 1));
      end
      else
        Abort;
    except
      Min := 0;
      Max := 0;
    end;
  end;

begin
  AnsiBuffer := glGetString(GL_VERSION);
  Buffer := String(AnsiBuffer);
  TrimAndSplitVersionString(Buffer, FMajorVersion, FMinorVersion);
end;

function TGLWindow.InitGL: boolean;
begin
  if not FForwardContext then
  begin
    glShadeModel(GL_SMOOTH);
    glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  end;
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClearDepth(1.0);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LESS);
  if assigned(FonInitialize) then FonInitialize(self);
  Result:=true;
end;

function TGLWindow.InitContext: boolean;
type
  TPixelFormatAttribList = array [0 .. 14] of integer;
const
  PIXELFORMATATTRIBLIST: TPixelFormatAttribList = (WGL_DRAW_TO_WINDOW_ARB,
    GL_True, WGL_SUPPORT_OPENGL_ARB, GL_True, WGL_DOUBLE_BUFFER_ARB, GL_True,
    WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_ARB, WGL_COLOR_BITS_ARB, 32,
    WGL_DEPTH_BITS_ARB, 24, WGL_STENCIL_BITS_ARB, 8, 0);
var
  iPixelFormatAttribList: TPixelFormatAttribList;
  iPixelFormat, iNumFormats: integer;
  pfd: TPixelFormatDescriptor;
  RC: HGLRC;
  list: TIntegerList;
begin
  Result := False;
  DeactivateRenderingContext;

  iPixelFormatAttribList := PIXELFORMATATTRIBLIST;
  iPixelFormatAttribList[9] := FColorBits;
  iPixelFormatAttribList[11] := FDepthBits;
  iPixelFormatAttribList[13] := FStensilBits;
  wglChoosePixelFormatARB(FDC, @iPixelFormatAttribList, nil, 1,
    @iPixelFormat, @iNumFormats);
  if not SetPixelFormat(FDC, iPixelFormat, @pfd) then
    exit;
  list := TIntegerList.Create;
  list.Add(WGL_CONTEXT_MAJOR_VERSION_ARB); list.Add(FMajorVersion);
  list.Add(WGL_CONTEXT_MINOR_VERSION_ARB); list.Add(FMinorVersion);

  if FDebugContext then
  begin
    list.Add(WGL_CONTEXT_FLAGS_ARB); list.Add(WGL_CONTEXT_DEBUG_BIT_ARB);
  end;

  list.Add(WGL_CONTEXT_PROFILE_MASK_ARB);
  if FForwardContext then
    list.Add(WGL_CONTEXT_CORE_PROFILE_BIT_ARB)
  else
    list.Add(WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB);

  RC := wglCreateContextAttribsARB(FDC, 0, list.GetItemAddr(0));
  if (RC <> 0) then begin
    ActivateRenderingContext(FDC, RC);
    if (not wglMakeCurrent(FDC, RC)) then begin
      FForwardContext := false;
      FDebugContext := false;
      wglMakeCurrent(FDC, FRC);
      assert(false,'Cant''t activate the GL rendering context.');
      exit;
    end;
    wglDeleteContext(FRC);
    FRC := RC;
    Result := True;
  end;

  if Result and FDebugContext then
  begin
    glDebugMessageCallbackARB(GLDebugCallback, Self);
    glDebugMessageControlARB(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, nil, true);
    glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB);
  end;
end;

constructor TGLWindow.Create;
begin
  FColorBits:=32;
  FDepthBits:=24;
  FStensilBits:=8;
  FAALevel:=0;
  FForwardContext := False;
  FDebugContext := False;
  FFrameTime:=-1;
end;

procedure TGLWindow.CreateWindow(Title: Pchar; width,height: integer;
  FullScreen: boolean);
var
  Pixelformat: GLuint;
  pfd: pixelformatdescriptor;
  dmScreenSettings: Devmode;
begin
  FillChar(FKeys,Sizeof(FKeys),0);
  FButtons := [];
  FMouseX := -1; FMouseY := -1;
  FCaption := Title;
  FWidth := Width;
  FHeight := Height;
  FFullScreen := FullScreen;
  FFrameTime:=-1;
  if FullScreen then begin
    ZeroMemory( @dmScreenSettings, sizeof(dmScreenSettings) );
    with dmScreensettings do begin
      dmSize := sizeof(dmScreenSettings);
      dmPelsWidth  := width;
      dmPelsHeight := height;
      dmBitsPerPel := 32;
      dmFields     := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
    end;
    if (ChangeDisplaySettings(dmScreenSettings, CDS_FULLSCREEN))<>DISP_CHANGE_SUCCESSFUL then begin
      if MessageBox(0,'This FullScreen Mode Is Not Supported. Use Windowed Mode Instead?',
        'GLWindow',MB_YESNO or MB_ICONEXCLAMATION)= IDYES then FullScreen:=false
      else assert(false,'Program Will Now Close.');
    end;
  end;

  if FWnd<>0 then KillGLWindow;

  FWnd:=AllocateGLWnd(WindProc, Title, width, height, FullScreen);

  if FWnd=0 then begin
    KillGLWindow;
    assert(false,'Window creation error.');
  end;

  FillChar(pfd, SizeOf(pfd), $00);

  with pfd do begin
    nSize:= SizeOf( PIXELFORMATDESCRIPTOR );
    nVersion:= 1;
    dwFlags:= PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    iPixelType:= PFD_TYPE_RGBA;
    cColorBits:= FColorBits;
    cDepthBits:= FDepthBits;
    cStencilBits:= FStensilBits;
    iLayerType:= PFD_MAIN_PLANE;
  end;
  FDC := GetDC(FWnd);
  if FDC=0 then begin
      KillGLWindow;
      assert(false,'Cant''t create a GL device context.');
  end;
  PixelFormat := ChoosePixelFormat(FDC, @pfd);
  if (PixelFormat=0) then begin
      KillGLWindow;
      assert(false,'Cant''t Find A Suitable PixelFormat.');
  end;
  if (not SetPixelFormat(FDC,PixelFormat,@pfd)) then begin
      KillGLWindow;
      assert(false,'Cant''t set PixelFormat.');
    end;
  FRC := wglCreateContext(FDC);
  if (FRC=0) then begin
      KillGLWindow();
      assert(false,'Cant''t create a GL rendering context.');
  end;
  ActivateRenderingContext(FDC,FRC);
  if (not wglMakeCurrent(FDC, FRC)) then begin
      KillGLWindow();
      assert(false,'Cant''t activate the GL rendering context.');
  end;

  GetOGLVersion;
  InitContext();

  ShowWindow(FWnd,SW_SHOW);
  SetForegroundWindow(FWnd);
  SetFocus(FWnd);
  DoResize(width,height);
  if (not InitGL) then begin
      KillGLWindow;
      assert(false,'initialization failed.');
  end;
  FActive:=true;
end;


procedure TGLWindow.KillGLWindow;
begin
  if FFullScreen then begin
    ChangeDisplaySettings(devmode(nil^),0);
    showcursor(true);
  end;
  if FRC<>0 then begin
    assert(wglMakeCurrent(0,0),'Release of DC and RC failed.');
    if (not wglDeleteContext(FRC)) then begin
      FRC:=0;
      assert(false,'Release of Rendering Context failed.');
    end;
  end;
  if (FDC=1) and (releaseDC(FWnd,FDC)<>0) then begin
    FDC:=0;
    assert(false,'Release of Device Context failed.');
  end;

  if (FWnd<>0) and (not destroywindow(FWnd)) then begin
      FWnd:=0;
      assert(false,'Could not release hWnd.');
    end;
end;

procedure TGLWindow.setActive(const Value: boolean);
begin
  FActive := Value;
end;

procedure TGLWindow.setCaption(const Value: string);
begin
  FCaption := Value;
  SetWindowText(FWnd, PChar(Value));
end;

procedure TGLWindow.setKey(index: integer; const Value: boolean);
begin
  FKeys[index] := Value;
end;

procedure TGLWindow.SetOnDebugMessage(const Value: TOnDebugMessage);
begin
  FOnDebugMessage := Value;
end;

procedure TGLWindow.SetonInitialize(const Value: TNotifyEvent);
begin
  FonInitialize := Value;
end;

procedure TGLWindow.SetonRender(const Value: TRenderEvent);
begin
  FonRender := Value;
end;

procedure TGLWindow.SetonResize(const Value: TResizeEvent);
begin
  FonResize := Value;
end;

procedure TGLWindow.SetPixelFormatBits(ColorBits, DepthBits, StensilBits,
  AALevel: byte; aForwardContext, aDebugContext: boolean);
begin
  FColorBits:=ColorBits;
  FDepthBits:=DepthBits;
  FStensilBits:=StensilBits;
  FAALevel:=AALevel;
  FForwardContext := aForwardContext;
  FDebugContext := aDebugContext;
  if FWnd <> 0 then
    CreateWindow(pChar(FCaption),FWidth, Fheight, FFullScreen);
end;

procedure TGLWindow.setVSync(const Value: boolean);
begin
  wglSwapIntervalEXT(integer(Value));
end;

procedure TGLWindow.SwapBuffer;
begin
  SwapBuffers(FDC);
end;

procedure TGLWindow.WindProc(var Message: TMessage);
var res: integer;
begin
  if message.Msg=WM_SYSCOMMAND then begin
    case Message.wParam of
      SC_SCREENSAVE,SC_MONITORPOWER: begin
          message.Result:=0;
          exit;
      end;
    end;
  end;
  res:=0;
  case message.Msg of
   WM_CREATE: begin
       SetWindowLong (FWnd, GWL_USERDATA, Integer(PCreateStruct(Message.LParam).lpCreateParams));
       Active:=true;
    end;
    WM_ACTIVATE: begin
      if (Hiword(Message.wParam)=0) then Active:=true
      else Active:=false;
    end;
    WM_CLOSE: begin
      Active:=false;
      PostQuitMessage(0);
    end;
    WM_KEYDOWN: begin
      FKeys[Message.wParam] := TRUE;
      if assigned(FOnKeyDown) then FOnKeyDown(self,Message.wParam);
    end;
    WM_KEYUP: begin
    	FKeys[Message.wParam] := FALSE;
      if assigned(FOnKeyUp) then FOnKeyUp(self,Message.wParam);
    end;
    WM_LBUTTONDOWN,
    WM_RBUTTONDOWN,
    WM_MBUTTONDOWN: begin
      if message.Msg = WM_LBUTTONDOWN then include(FButtons,mbLeft);
      if message.Msg = WM_RBUTTONDOWN then include(FButtons,mbRight);
      if message.Msg = WM_MBUTTONDOWN then include(FButtons,mbMiddle);
      if assigned(FOnMouseDown) then FOnMouseDown(self, FMouseX, FMouseY, FButtons);
    end;
    WM_LBUTTONUP,
    WM_RBUTTONUP,
    WM_MBUTTONUP: begin
      if message.Msg = WM_LBUTTONUP then exclude(FButtons,mbLeft);
      if message.Msg = WM_RBUTTONUP then exclude(FButtons,mbRight);
      if message.Msg = WM_MBUTTONUP then exclude(FButtons,mbRight);
      if assigned(FOnMouseUp) then FOnMouseUp(self, FMouseX, FMouseY, FButtons);
    end;
    WM_MOUSEMOVE: begin
      FMouseX := SMALLINT( Message.lParam and $FFFF);
      FMouseY := SMALLINT( (Message.lParam shr 16) and $FFFF);
      if assigned(FOnMouseMove) then FOnMouseMove(self, FMouseX, FMouseY, FButtons);
    end;

    WM_SIZE: begin
    	DoResize(LOWORD(Message.lParam),HIWORD(Message.lParam));
    end
    else begin
    	Res := DefWindowProc(FWnd, message.Msg, Message.wParam, Message.lParam);
    end;
  end;
  message.result := res;
end;

{ TCadencer }

procedure TCadencer.Execute;
var t: double;
begin
  while not Terminated do begin
    if  false {not isRendering} then begin
      t := GetTime;
      if t - FLastTime > FUpdateTime then
      begin
        synchronize(update);
        FLastTime := t;
      end;
    end;
  end;
end;

procedure TCadencer.update;
begin
//  if assigned(FGLViewer) then FGLViewer.Paint;
end;

end.


