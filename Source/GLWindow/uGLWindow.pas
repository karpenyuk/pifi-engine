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
  Forms;

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

(*
  TContext = class
  private
{$IFDEF Linux}
    FDisplay: PDisplay;
{$ENDIF}
    FDepthBits: byte;
    FScencilBits: byte;
    FAALevel: byte;
    FInitialized: boolean;
    FForwardContext: boolean;
    FDebugContext: Boolean;
    procedure setStencilBits(const Value: byte); virtual;
    procedure setAALevel(const Value: byte); virtual;
  protected
    FDC: {$IFDEF MSWINDOWS} cardinal; {$ENDIF}
{$IFDEF Linux} GLXDrawable; {$ENDIF}
    FVSync: boolean;
    FPfdChanged: boolean;
    function getVSync: boolean; virtual;
    procedure SetVSync(Value: boolean); virtual;
    procedure setActive(const Value: boolean); virtual; abstract;
    function getActive: boolean; virtual; abstract;
    property Active: Boolean read getActive write setActive;
    procedure setDepthBits(const Value: byte); virtual;
    property ForwardContext: Boolean read FForwardContext write FForwardContext default True;
    property DebugContext: Boolean read FDebugContext write FDebugContext default False;
  public
    constructor Create;

    procedure InitializeContext(aDC: HDC); virtual;
    procedure Init; virtual; abstract;
    procedure ComponentPaint(Sender: TObject); virtual; abstract;
    procedure ClearDevice; virtual; abstract;
    procedure Resize(Width, Height: integer); virtual; abstract;
    procedure Activate; virtual; abstract;
    procedure Deactivate; virtual; abstract;

    property DeviceContext: cardinal read FDC;
{$IFDEF Linux}
    property Display: PDisplay read FDisplay write FDisplay;
{$ENDIF}
    property Initialized: boolean read FInitialized;
    property VSync: boolean read getVSync write SetVSync;

  published
    property DepthBits: byte read FDepthBits write setDepthBits;
    property StencilBits: byte read FScencilBits write setStencilBits;
    property AALevel: byte read FAALevel write setAALevel;
  end;

  { TGLContext }

  TGLContext = class(TContext)
  private
    FGLRCx: HGLRC;
    FiAttribs: packed array of integer;
{$IFDEF Linux}
    FCurScreen: integer;
    FCurXWindow: HWND;
    FFBConfigs: PGLXFBConfigArray;
{$ENDIF}
    FGLInit: boolean;
    FRendering: boolean;
    FMajorVersion, FMinorVersion: integer;
    FDebugLog: TStringList;
{$IFDEF MSWINDOWS}
    procedure SetDCPixelFormat(dc: HDC);
{$ENDIF}
{$IFDEF Linux}
    procedure ChooseGLXFormat;
{$ENDIF}
    procedure ClearIAttribs;
    procedure FreeIAttribs;
    procedure AddIAttrib(attrib, Value: integer);
    procedure ChangeIAttrib(attrib, newValue: integer);
    procedure DropIAttrib(attrib: integer);

    procedure ChangePixelFormat;
    procedure GetOGLVersion;
    function InitForwardContext: boolean;
    function ParseDebug(aSource, aType, aId, aSeverity: cardinal;
      const aMess: ansistring): ansistring;
  protected
    function getVSync: boolean; override;
    procedure SetVSync(Value: boolean); override;
    procedure setActive(const Value: boolean); override;
    function getActive: boolean; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure InitializeContext(aDC: HDC); override;
{$IFDEF Linux}
    procedure SwapBuffers;
{$ENDIF}
    procedure Init; override;
    procedure ComponentPaint(Sender: TObject); override;
    procedure ClearDevice; override;
    procedure Resize(Width, Height: integer); override;
    procedure Activate; override;
    procedure Deactivate; override;
    procedure CheckDebugLog;
    property DebugLog: TStringList read FDebugLog;
    property Active;
  published
    property MaxMajorVersion: integer read FMajorVersion;
    property MaxMinorVersion: integer read FMinorVersion;
    property ForwardContext;
    property DebugContext;
  end;

{$IFDEF FPC}
  TCanResizeEvent = procedure(Sender: TObject; var NewWidth, NewHeight: Integer;
    var Resize: Boolean) of object;
{$ENDIF}

*)
  TGLWindow = class
  private
    FKeys: array [0..255] of boolean;
    FActive: boolean;
    FisRendering: boolean;
    FFullScreen: boolean;
    FWnd: HWnd;
    FDC: Hdc;
    FRC: HGLRC;
    procedure setActive(const Value: boolean);
    procedure KillGLWindow;
    function InitGL: boolean;
    procedure WindProc(var Message: TMessage);
    function getKey(index: integer): boolean;
    procedure setKey(index: integer; const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure CreateWindow(Title: Pchar; width, height: integer;
      FullScreen: boolean);
    procedure DoResize(Width, Height: integer);
    procedure SwapBuffer;
    procedure DrawGLScene;
    property Active: boolean read FActive write setActive;
    property isRendering: boolean read FisRendering;
    property Keys[index: integer]: boolean read getKey write setKey;
  end;

implementation

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

{ TGLWindow }

destructor TGLWindow.Destroy;
begin
  killGLwindow;
  inherited;
end;

procedure TGLWindow.DoResize(Width, Height: integer);
begin
  glViewport(0,0,Width,Height);
end;

procedure TGLWindow.DrawGLScene;
begin
  glClear(GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT);
end;

function TGLWindow.getKey(index: integer): boolean;
begin
  result := FKeys[index];
end;

function TGLWindow.InitGL: boolean;
begin
  glShadeModel(GL_SMOOTH);
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClearDepth(1.0);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LESS);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);
  Result:=true;
end;

constructor TGLWindow.Create;
begin
//
end;

procedure TGLWindow.CreateWindow(Title: Pchar; width,height: integer;
  FullScreen: boolean);
var
  Pixelformat: GLuint;
  pfd: pixelformatdescriptor;
  dmScreenSettings: Devmode;
begin
  inherited Create;
  FFullScreen := FullScreen;
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

  FWnd:=AllocateGLWnd(WindProc, Title, width, height, FullScreen);

  if FWnd=0 then begin
    KillGLWindow;
    assert(false,'Window creation error.');
  end;

  with pfd do begin
    nSize:= SizeOf( PIXELFORMATDESCRIPTOR );
    nVersion:= 1;
    dwFlags:= PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    iPixelType:= PFD_TYPE_RGBA;
    cColorBits:= 32;
    cRedBits:= 8;
    cRedShift:= 0;
    cGreenBits:= 8;
    cBlueBits:= 8;
    cBlueShift:= 0;
    cAlphaBits:= 8;
    cAlphaShift:= 0;
    cAccumBits:= 0;
    cAccumRedBits:= 0;
    cAccumGreenBits:= 0;
    cAccumBlueBits:= 0;
    cAccumAlphaBits:= 0;
    cDepthBits:= 24;
    cStencilBits:= 8;
    cAuxBuffers:= 0;
    iLayerType:= PFD_MAIN_PLANE;
    bReserved:= 0;
    dwLayerMask:= 0;
    dwVisibleMask:= 0;
    dwDamageMask:= 0;
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
  ShowWindow(FWnd,SW_SHOW);
  SetForegroundWindow(FWnd);
  SetFocus(FWnd);
  DoResize(width,height);
  if (not InitGL) then begin
      KillGLWindow;
      assert(false,'initialization failed.');
  end;
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

procedure TGLWindow.setKey(index: integer; const Value: boolean);
begin
  FKeys[index] := Value;
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
  case message.Msg of
   WM_CREATE: begin
       SetWindowLong (FWnd, GWL_USERDATA, Integer(PCreateStruct(Message.LParam).lpCreateParams));
       Res := 0;
    end;
    WM_ACTIVATE: begin
      if (Hiword(Message.wParam)=0) then Active:=true
      else Active:=false;
      Res:=0;
    end;
    WM_CLOSE: begin
      PostQuitMessage(0);
      res:=0
    end;
    WM_KEYDOWN: begin
      FKeys[Message.wParam] := TRUE;
      res:=0;
    end;
    WM_KEYUP: begin
    	FKeys[Message.wParam] := FALSE;
      res:=0;
    end;
    WM_SIZE: begin
    	DoResize(LOWORD(Message.lParam),HIWORD(Message.lParam));
      res:=0;
    end
    else begin
    	Res := DefWindowProc(FWnd, message.Msg, Message.wParam, Message.lParam);
    end;
  end;
  message.result := res;
end;

{ TCadencer }

procedure TCadencer.Execute;
var
  t: double;
begin
  while not Terminated do
  begin
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


