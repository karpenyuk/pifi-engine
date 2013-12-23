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

  TGLWindow = class
  private
    FKeys: array [0..255] of boolean;
    FActive: boolean;
    FisRendering: boolean;
    FFullScreen: boolean;
    FWnd: HWnd;
    FDC: Hdc;
    FRC: HGLRC;
    FWidth, FHeight: integer;
    FColorBits, FDepthBits, FStensilBits, FAALevel : byte;
    FCaption: string;
    procedure setActive(const Value: boolean);
    procedure KillGLWindow;
    function InitGL: boolean;
    procedure WindProc(var Message: TMessage);
    function getKey(index: integer): boolean;
    procedure setKey(index: integer; const Value: boolean);
    function getVSync: boolean;
    procedure setVSync(const Value: boolean);
    procedure setCaption(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure CreateWindow(Title: Pchar; width, height: integer;
      FullScreen: boolean);
    procedure SetPixelFormatBits(ColorBits, DepthBits, StensilBits, AALevel: byte);
    procedure DoResize(Width, Height: integer);
    procedure SwapBuffer;
    procedure DrawGLScene;
    property Active: boolean read FActive write setActive;
    property isRendering: boolean read FisRendering;
    property Keys[index: integer]: boolean read getKey write setKey;
    property VSync: boolean read getVSync write setVSync;
    property Caption: string read FCaption write setCaption;
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
  glClear(GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
end;

procedure TGLWindow.DrawGLScene;
begin
  if not Active then exit;

  FisRendering := true;
  glClear(GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT);
  assert(glGetError = GL_NO_ERROR, 'Error');
  FisRendering := false;
end;

function TGLWindow.getKey(index: integer): boolean;
begin
  result := FKeys[index];
end;

function TGLWindow.getVSync: boolean;
begin
  result := boolean(wglGetSwapIntervalEXT);
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
  FColorBits:=32;
  FDepthBits:=24;
  FStensilBits:=8;
  FAALevel:=0;
end;

procedure TGLWindow.CreateWindow(Title: Pchar; width,height: integer;
  FullScreen: boolean);
var
  Pixelformat: GLuint;
  pfd: pixelformatdescriptor;
  dmScreenSettings: Devmode;
begin
  inherited Create;
  FCaption := Title;
  FWidth:=Width;
  FHeight:=Height;
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

  if FWnd<>0 then KillGLWindow;

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
    cColorBits:= FColorBits;
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
    cDepthBits:= FDepthBits;
    cStencilBits:= FStensilBits;
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

procedure TGLWindow.SetPixelFormatBits(ColorBits, DepthBits, StensilBits,
  AALevel: byte);
begin
  FColorBits:=ColorBits;
  FDepthBits:=DepthBits;
  FStensilBits:=StensilBits;
  FAALevel:=AALevel;
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


