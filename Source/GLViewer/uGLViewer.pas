unit uGLViewer;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF Linux} X, XLib, XUtil, {$ENDIF}
{$IFDEF FPC}
  LCLType, LCLintf, LMessages,
{$ENDIF}
{$IFDEF Linux}
  gtk2proc, gtk2, gdk2, gdk2x, gtk2def,
{$ENDIF}
  Messages, SysUtils, Types, Classes, Controls, ExtCtrls,
  dglOpenGL;

const
  cMaxUpdateTime = 1 / 5000;

type
{$IFDEF Linux}
  TGLXFBConfigArray = array [0 .. MaxInt div (SizeOf(GLXFBConfig) * 2)
    ] of GLXFBConfig;
  PGLXFBConfigArray = ^TGLXFBConfigArray;
{$ENDIF}
  TGLViewer = class;

  TCadencer = class(TThread)
  private
    FLastTime: double;
    FUpdateTime: double;
    FGLViewer: TGLViewer;
    procedure update;
  protected
    procedure Execute; override;
  end;

  TOnDebugMessage = procedure(const AMessage: string) of object;

  TContext = class(TPersistent)
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
    FOnDebug: TOnDebugMessage;
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
    property OnContextDebugMessage: TOnDebugMessage read FOnDebug write FOnDebug;
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
    property OnContextDebugMessage;
  end;

{$IFDEF FPC}
  TCanResizeEvent = procedure(Sender: TObject; var NewWidth, NewHeight: Integer;
    var Resize: Boolean) of object;
{$ENDIF}

  TGLViewer = class(TCustomControl)
  private
    FDC: HDC;
    FH, FW: single;
    FOnRender: TNotifyEvent;
    FVSync: boolean;
    FRendering: boolean;

    FOldX, FOldY: integer;
    FDeltaX, FDeltaY, FDeltaZ: single;

    FTimer: TTimer;
    FCadencer: TCadencer;
    FFixedFPS: integer;

    FNewTime: double;
    FDeltaTime: double;
    FFPS: double;
    FInstantFPS: double;
    FLastFPS: double;
    FFPSCounter: int64;

    FContext: TGLContext;
    FonContextReady: TNotifyEvent;
{$IFDEF FPC}
    FOnCanResize: TCanResizeEvent;
{$ENDIF}

    function getVSync: boolean;
    procedure SetVSync(const Value: boolean);
    procedure TimerProc(Sender: TObject);
    procedure setUpdateTime(const Value: integer);
    function ContextReady: boolean;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    function getDeltaZ: single;

  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
{$IFDEF FPC}
    procedure LMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure LMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure LMSize(var Message: TLMSize); message LM_SIZE;
    procedure LMDestroy(var Message: TLMDestroy); message LM_DESTROY;
{$ELSE}
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); Message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); Message WM_PAINT;
    procedure WMSize(var Message: TWMSize); Message WM_SIZE;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
{$ENDIF}
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
      MousePos: TPoint): boolean; override;

    procedure Paint; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure ResetFPSCounter;
    procedure Resize; override;

    property Canvas;
    property DeviceContext: HDC read FDC;
    property DeltaTime: double read FDeltaTime;
    property FPS: double read FFPS;
    property InstantFPS: double read FInstantFPS;
    property DeltaX: single read FDeltaX;
    property DeltaY: single read FDeltaY;
    property DeltaZ: single read getDeltaZ;

  published
    property OnRender: TNotifyEvent read FOnRender write FOnRender;
    property OnContextReady: TNotifyEvent read FonContextReady
      write FonContextReady;

    property VSync: boolean read getVSync write SetVSync default False;
    property FixedFPS: integer read FFixedFPS write setUpdateTime default 0;
    property Context: TGLContext read FContext;

    property Align;
    property Visible;
{$IFDEF FPC}
    property OnCanResize: TCanResizeEvent read FOnCanResize write FOnCanResize;
{$ELSE}
    property OnCanResize;
{$ENDIF}
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

procedure Register;

implementation

uses uMiscUtils;

threadvar
  vCurrentGLRCx: HGLRC;

procedure Register;
begin
  RegisterComponents('VBOMesh', [TGLViewer]);
end;

function GetTime: double;
var
  Freq, Tick: int64;
begin
  QueryPerformanceFrequency(Freq);
  QueryPerformanceCounter(Tick);
  Result := Tick / Freq;
end;

{ TGLViewer }

procedure TGLViewer.setUpdateTime(const Value: integer);
begin
  FFixedFPS := Value;
  if Value > 0 then
    FCadencer.FUpdateTime := 1 / Value
  else
    FCadencer.FUpdateTime := cMaxUpdateTime;
end;

procedure TGLViewer.SetVSync(const Value: boolean);
begin
  if (csDesigning in ComponentState) then
    FVSync := Value
  else if ContextReady then
    FContext.VSync := Value
  else
    FVSync := Value;
end;

procedure TGLViewer.TimerProc(Sender: TObject);
begin
  if (csDesigning in ComponentState) then
    Invalidate;
end;

procedure TGLViewer.Invalidate;
begin
  if ContextReady and FContext.Active then
    InvalidateRect(Handle, nil, False)
  else
    inherited;
end;

procedure TGLViewer.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  if ssLeft in Shift then
  begin
    FDeltaX := X - FOldX;
    FDeltaY := Y - FOldY;
  end
  else
  begin
    FDeltaX := 0;
    FDeltaY := 0;
  end;
  FOldX := X;
  FOldY := Y;
  inherited;
end;

procedure TGLViewer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
end;

procedure TGLViewer.CMMouseEnter(var Message: TMessage);
begin
  SetFocus;
  if assigned(onMouseEnter) then
    onMouseEnter(Self);
end;

procedure TGLViewer.CMMouseLeave(var Message: TMessage);
begin
  if assigned(onMouseLeave) then
    onMouseLeave(Self);
end;

procedure TGLViewer.CMMouseWheel(var Message: TCMMouseWheel);
begin
  with Message do
  begin
    Result := 0;
    if DoMouseWheel(ShiftState,
{$IFNDEF FPC} WheelDelta{$ELSE} WheelData
{$ENDIF}, SmallPointToPoint(TSmallPoint(Pos))) then
      Message.Result := 1
    else if Parent <> nil then
{$IFNDEF FPC}
      with TMessage(Message) do
      begin
        Result := Parent.Perform(CM_MOUSEWHEEL, WParam, LParam);
      end;
{$ENDIF}
  end;
end;

function TGLViewer.ContextReady: boolean;
begin
  if assigned(FContext) and FContext.Initialized then
    Result := true
  else
    Result := False;
end;

constructor TGLViewer.Create(AOwner: TComponent);
begin
  inherited;
  FContext := TGLContext.Create;
  Width := 100;
  Height := 100;
  Caption := '';
  FH := 1 - 1 / Height;
  FW := 1 - 1 / Width;
  FRendering := False;
  FCadencer := TCadencer.Create(true);
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 100;
  FTimer.OnTimer := TimerProc;
  if csDesigning in ComponentState then
    FTimer.Enabled := true
  else
  begin
    FTimer.Enabled := False;
    with FCadencer do
    begin
      FGLViewer := Self;
      Priority := tpIdle;
      FLastTime := 0;
      // freeonterminate := true;
      FUpdateTime := cMaxUpdateTime;
    end;
  end;
  FFPSCounter := 0;
end;

procedure TGLViewer.CreateWnd;
begin
  inherited;
  FDC := GetDC(Handle);
  if assigned(FContext) then
    FContext.InitializeContext(FDC);
  if assigned(FonContextReady) then
    FonContextReady(Self);
  if not(csDesigning in ComponentState) then
  begin
    FCadencer.Start;
    FContext.VSync := FVSync;
  end;
end;

procedure TGLViewer.DestroyWnd;
begin
  inherited;
end;

destructor TGLViewer.Destroy;
begin
  FTimer.Enabled := False;
  FTimer.Free;
  FCadencer.FGLViewer := nil;
  FCadencer.Terminate;
  // repeat until FCadencer.Terminated;
  FCadencer.Free;
  FContext.Destroy;
  FContext := nil;
  inherited;
end;

function TGLViewer.DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint): boolean;
begin
  inherited;
  FDeltaZ := WheelDelta;
  Result := False;
end;

function TGLViewer.getDeltaZ: single;
begin
  Result := FDeltaZ;
  FDeltaZ := FDeltaZ / 1.2;
end;

function TGLViewer.getVSync: boolean;
begin
  if (csDesigning in ComponentState) then
    Result := FVSync
  else if ContextReady then
    Result := FContext.VSync
  else
    Result := False;
end;

procedure TGLViewer.Paint;
var
  t: double;
begin
  // inherited;
  if FRendering then
    exit;
  FRendering := true;
  if FFPSCounter = 0 then
    FNewTime := GetTime;

  if ContextReady and (FContext.Active) then
  begin
    FContext.Activate;
    FContext.ComponentPaint(Self);
    if assigned(FOnRender) then
      FOnRender(Self);
{$IFDEF MSWINDOWS}
    SwapBuffers(FDC);
{$ENDIF}
{$IFDEF Linux}
    FContext.SwapBuffers;
{$ENDIF}
    inc(FFPSCounter);
    t := GetTime;
    if (FFixedFPS > 0) and (not VSync) then
      FDeltaTime := 1 / FFixedFPS
    else
      FDeltaTime := (t - FNewTime);
    FInstantFPS := 1 / (t - FNewTime);
    FNewTime := t;
    if FFPSCounter = 1 then
      FLastFPS := FInstantFPS
    else
      FLastFPS := FLastFPS + FInstantFPS;
    if FFPSCounter > 1000 then
    begin
      FFPS := FLastFPS / FFPSCounter;
      FFPSCounter := 1;
      FLastFPS := FInstantFPS;
      FFPS := (FFPS + FInstantFPS) / 2;
    end
    else
      FFPS := FLastFPS / FFPSCounter;
  end;
  FRendering := False;
end;

procedure TGLViewer.ResetFPSCounter;
begin
  FFPSCounter := 0;
  FFPS := 0;
end;

{$IFDEF FPC}

procedure TGLViewer.Resize;
var
  w, h: Integer;
  YesIcan: Boolean;
begin
  // Äëÿ Ëàçàðóñà áóäåò ðàáîòàòü íå òàê êàê äëÿ Äåëôè,
  // íî ðåøèòñÿ ïðîáëåìà îòñóòñòâèÿ ñîáûòèÿ
  YesIcan := True;
  w := Width;
  h := Height;
  if Assigned(FOnCanResize) then FOnCanResize(Self, w, h, YesIcan);
  if YesIcan then
  begin
    inherited Resize;
    if ContextReady then
      FContext.Resize(Width, Height);
  end;
end;

{$ELSE}

procedure TGLViewer.Resize;
begin
  inherited;
  if ContextReady then
    FContext.Resize(Width, Height);
end;

{$ENDIF}

{$IFDEF FPC}

procedure TGLViewer.LMDestroy(var Message: TLMDestroy);
{$ELSE}

procedure TGLViewer.WMDestroy(var Message: TWMDestroy);
{$ENDIF}
begin
  inherited;
end;

{$IFDEF FPC}

procedure TGLViewer.LMEraseBkgnd(var Message: TLMEraseBkgnd);
{$ELSE}

procedure TGLViewer.WMEraseBkgnd(var Message: TWMEraseBkgnd);
{$ENDIF}
begin
  Message.Result := 1
  // else inherited;
end;

{$IFDEF FPC}

procedure TGLViewer.LMSize(var Message: TLMSize);
{$ELSE}

procedure TGLViewer.WMSize(var Message: TWMSize);
{$ENDIF}
begin
  inherited;
end;


// LMPaint
{$IFDEF FPC}

procedure TGLViewer.LMPaint(var Message: TLMPaint);
{$ELSE}

procedure TGLViewer.WMPaint(var Message: TWMPaint);
{$ENDIF}
var
  PS: TPaintStruct;
  p: TPoint;
begin
  p := ClientToScreen(Point(0, 0));
  BeginPaint(Handle, PS);
  try
    if (Width > 0) and (Height > 0) then
    begin
      if (not ContextReady) or (not FContext.Active) then
        FRendering := False
      else
        Paint;
    end;
  finally
    EndPaint(Handle, PS);
    Message.Result := 0;
  end;
end;

{ TCadencer }

procedure TCadencer.Execute;
var
  t: double;
begin
  while not Terminated do
  begin
    if not FGLViewer.FRendering then
    begin
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
  // FGAPIViewer.Invalidate;
  if assigned(FGLViewer) then
    FGLViewer.Paint;
end;

{ TContext }

constructor TContext.Create;
begin
  inherited;
  FInitialized := False;
  FDepthBits := 24;
  FScencilBits := 8;
  FAALevel := 0;
  FPfdChanged := true;;
end;

function TContext.getVSync: boolean;
begin
  Result := FVSync;
end;

procedure TContext.InitializeContext(aDC: HDC);
begin
  FDC := aDC;
end;

procedure TContext.setAALevel(const Value: byte);
begin
  FAALevel := Value;
end;

procedure TContext.setDepthBits(const Value: byte);
begin
  FDepthBits := Value;
end;

procedure TContext.setStencilBits(const Value: byte);
begin
  FScencilBits := Value;
end;

procedure TContext.SetVSync(Value: boolean);
begin
  FVSync := Value;
end;

{ TGLContext }

procedure TGLContext.ClearIAttribs;
begin
  SetLength(FiAttribs, 1);
  FiAttribs[0] := 0;
end;

procedure TGLContext.FreeIAttribs;
begin
  SetLength(FiAttribs, 0);
end;

procedure TGLContext.AddIAttrib(attrib, Value: integer);
var
  n: integer;
begin
  n := length(FiAttribs);
  SetLength(FiAttribs, n + 2);
  FiAttribs[n - 1] := attrib;
  FiAttribs[n] := Value;
  FiAttribs[n + 1] := 0;
end;

procedure TGLContext.ChangeIAttrib(attrib, newValue: integer);
var
  i: integer;
begin
  i := 0;
  while i < length(FiAttribs) do
  begin
    if FiAttribs[i] = attrib then
    begin
      FiAttribs[i + 1] := newValue;
      exit;
    end;
    inc(i, 2);
  end;
  AddIAttrib(attrib, newValue);
end;

procedure TGLContext.DropIAttrib(attrib: integer);
var
  i: integer;
begin
  i := 0;
  while i < length(FiAttribs) do
  begin
    if FiAttribs[i] = attrib then
    begin
      inc(i, 2);
      while i < length(FiAttribs) do
      begin
        FiAttribs[i - 2] := FiAttribs[i];
        inc(i);
      end;
      SetLength(FiAttribs, length(FiAttribs) - 2);
      exit;
    end;
    inc(i, 2);
  end;
end;

procedure TGLContext.Activate;
begin
  inherited;
  if vCurrentGLRCx <> FGLRCx then
  begin
{$IFDEF MSWINDOWS}
  wglMakeCurrent(DeviceContext, FGLRCx);
{$ENDIF}
{$IFDEF Linux}
  glXMakeContextCurrent(FDisplay, FDC, FDC, FGLRCx);
{$ENDIF}
    vCurrentGLRCx := FGLRCx;
  end;
end;

procedure TGLContext.Deactivate;
begin
  inherited;
  if vCurrentGLRCx = FGLRCx then
  begin
{$IFDEF MSWINDOWS}
  if (FGLRCx > 0)  then
    wglMakeCurrent(DeviceContext, 0);
    vCurrentGLRCx := 0;
{$ENDIF}
{$IFDEF Linux}
  if (FGLRCx <> nil) then
    glXMakeContextCurrent(FDisplay, 0, 0, nil);
    vCurrentGLRCx := nil;
{$ENDIF}
  end;
end;

procedure TGLContext.ChangePixelFormat;
var
  temp: HGLRC;
begin
{$IFDEF MSWINDOWS}
  DeactivateRenderingContext;
  if FGLRCx <> 0 then
    wglDeleteContext(FGLRCx);
  SetDCPixelFormat(DeviceContext);
  FPfdChanged := False;
  FGLRCx := wglCreateContext(DeviceContext);
  ActivateRenderingContext(DeviceContext, FGLRCx);
  DeactivateRenderingContext;
  FPfdChanged := False;
  if FForwardContext then
  begin
    temp := FGLRCx;
    if InitForwardContext then
      wglDeleteContext(temp)
    else
    begin
      FForwardContext := False;
      FDebugContext := False;
    end;
  end;
{$ENDIF}
{$IFDEF Linux}
  DeactivateRenderingContext(FDisplay);
  if FGLRCx <> nil then
    glXDestroyContext(FDisplay, FGLRCx);

  ClearIAttribs;
  ChooseGLXFormat;

  AddIAttrib(GLX_X_RENDERABLE, GL_True);
  AddIAttrib(GLX_RENDER_TYPE, GLX_RGBA_BIT);
  AddIAttrib(GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT);

  FPfdChanged := False;
  FGLRCx := glXCreateNewContext(FDisplay, FFBConfigs[0], GLX_RGBA_TYPE,
    nil, true);
  ActivateRenderingContext(FDisplay, DeviceContext, FGLRCx);
  DeactivateRenderingContext(FDisplay);
  FPfdChanged := False;
  if FForwardContext then
  begin
    temp := FGLRCx;
    if InitForwardContext then
      glXDestroyContext(FDisplay, temp)
    else
    begin
      FForwardContext := False;
      FDebugContext := False;
    end;
  end;
{$ENDIF}
end;

procedure TGLContext.CheckDebugLog;
const
  count = 10;
  bufsize = 2048;
var
  sources, Types, ids, severities: array [0 .. count - 1] of cardinal;
  lengths: array [0 .. count - 1] of integer;
  messageLog: PAnsiChar;
  retVal: integer;
  i, Pos: integer;
  s: ansistring;
begin
  getmem(messageLog, bufsize);
  retVal := glGetDebugMessageLogARB(count, bufsize, @sources, @Types, @ids,
    @severities, @lengths, messageLog);
  if (retVal > 0) then
  begin
    Pos := 0;
    for i := 0 to retVal - 1 do
    begin
      s := copy(messageLog, Pos, lengths[i]);
      FDebugLog.Add(string(ParseDebug(sources[i], Types[i], ids[i],
        severities[i], s)));
      Pos := Pos + lengths[i];
    end;
  end;
end;

procedure TGLContext.ClearDevice;
begin
  glClear(GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
end;

procedure TGLContext.ComponentPaint(Sender: TObject);
var
  GV: TGLViewer;
begin
  if not FGLInit then
    exit;
  Init;
  GV := TGLViewer(Sender);
  Resize(GV.Width, GV.Height);
  if csDesigning in GV.ComponentState then
  begin
{$IFDEF MSWINDOWS}
    wglSwapIntervalEXT(1);
{$ENDIF}
{$IFDEF Linux}
    glXSwapIntervalEXT(FDisplay, FDC, 1);
{$ENDIF}
    glColor3f(0, 1, 0);
    glLineWidth(1.0);
    glBegin(GL_LINE_LOOP);
    glVertex2f(-1 + 1 / GV.Width, 1 - 1 / GV.Height);
    glVertex2f(1 - 1 / GV.Width, 1 - 1 / GV.Height);
    glVertex2f(1 - 1 / GV.Width, -1 + 1 / GV.Height);
    glVertex2f(-1 + 1 / GV.Width, -1 + 1 / GV.Height);
    glEnd;
  end
  else
  begin
{$IFDEF MSWINDOWS}
    if FVSync then
      wglSwapIntervalEXT(1)
    else
      wglSwapIntervalEXT(0);
{$ENDIF}
{$IFDEF Linux}
    if FVSync then
      glXSwapIntervalEXT(FDisplay, FDC, 1)
    else
      glXSwapIntervalEXT(FDisplay, FDC, 0);
{$ENDIF}
  end;
end;

{$IFDEF MSWINDOWS}

procedure TGLContext.SetDCPixelFormat(dc: HDC);
var
  pfd: TPixelFormatDescriptor;
  nPixelFormat: integer;
begin
  FillChar(pfd, SizeOf(pfd), 0);
  with pfd do
  begin
    nSize := SizeOf(TPixelFormatDescriptor);
    nVersion := 1;
    dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    iPixelType := PFD_TYPE_RGBA;
    cColorBits := 32;
    cDepthBits := DepthBits;
    cStencilBits := StencilBits;
    iLayerType := PFD_MAIN_PLANE;
  end;
  nPixelFormat := ChoosePixelFormat(dc, @pfd);
  SetPixelFormat(dc, nPixelFormat, @pfd);
end;
{$ENDIF}
{$IFDEF LINUX}

procedure TGLContext.ChooseGLXFormat;
var
  vFBConfigs: PGLXFBConfigArray;
  fnelements: integer;
  function GetFixedAttribute(attrib: TGLInt; Param: integer): integer;
  var
    i, Res, OverRes: integer;
  begin
    { : Appointment of a function to look for equal or approximate values
      of attributes from the list glx.
      If you just ask all the attributes
      that the user can put it out of ignorance
      Access Violation could appear as the list will be empty. }
    Result := -1;
    OverRes := -1;
    for i := 0 to fnelements - 1 do
    begin
      glxGetFBConfigAttrib(FDisplay, vFBConfigs[i], attrib, @Res);
      if (Res > 0) and (Res <= Param) then
        Result := Res;
      if (Res > Param) and (OverRes < Res) then
        OverRes := Res;
    end;
    if (Result = -1) and (i = fnelements - 1) then
      Result := OverRes;
  end;

  function ChooseFBConfig: boolean;
  begin
    if assigned(vFBConfigs) then
      XFree(vFBConfigs);
    vFBConfigs := glxChooseFBConfig(FDisplay, FCurScreen, @FiAttribs[0],
      @fnelements);

    Result := assigned(vFBConfigs);
  end;

var
  ColorBits: integer;
begin

  vFBConfigs := nil;
  if not ChooseFBConfig then
    raise Exception.Create('Failed to accept attributes');
  try
    ColorBits := GetFixedAttribute(GLX_BUFFER_SIZE, 24);
    AddIAttrib(GLX_BUFFER_SIZE, ColorBits);
    AddIAttrib(GLX_ALPHA_SIZE, 0);

    FDepthBits := GetFixedAttribute(GLX_DEPTH_SIZE, FDepthBits);
    AddIAttrib(GLX_DEPTH_SIZE, FDepthBits);

    AddIAttrib(GLX_DOUBLEBUFFER, GL_True);

    FScencilBits := GetFixedAttribute(GLX_STENCIL_SIZE, FScencilBits);
    AddIAttrib(GLX_STENCIL_SIZE, FScencilBits);

  finally
    if assigned(vFBConfigs) then
      XFree(vFBConfigs);
  end;

  FFBConfigs := glxChooseFBConfig(FDisplay, FCurScreen, @FiAttribs[0],
    @fnelements);
  if FFBConfigs = nil then
    raise Exception.Create('Failed to accept attributes');
end;
{$ENDIF}

procedure TGLContext.SetVSync(Value: boolean);
begin
  inherited;
  // if Value then wglSwapIntervalEXT(1) else wglSwapIntervalEXT(0);
end;

procedure TGLContext.setActive(const Value: boolean);
begin
  if not Active then
     Activate;
end;

function TGLContext.getActive: boolean;
begin
  Result := FGLRCx = vCurrentGLRCx;
end;

constructor TGLContext.Create;
begin
  inherited;
  FDebugLog := TStringList.Create;
  FForwardContext := True;
  FDebugContext := False;
  FRendering := False;
end;

procedure GLDebugCallback(source : GLEnum; type_ : GLEnum; id : GLUInt;
             severity : GLUInt; length : GLsizei;
             const message_ : PGLCHar; userParam : PGLvoid);
{$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
var
  Context: TGLContext absolute userParam;
begin
  if Assigned(userParam) and Assigned(Context.FOnDebug) then
    Context.FOnDebug(string(Context.ParseDebug(Source, type_, id, severity, message_)));
end;

destructor TGLContext.Destroy;
begin
  // if FDebug then begin Activate; CheckDebugLog; Deactivate; end;
  DeactivateRenderingContext{$IFDEF Linux}(FDisplay){$ENDIF};
{$IFDEF MSWINDOWS}
  if vCurrentGLRCx = FGLRCx then
     vCurrentGLRCx := 0;
  if FGLRCx <> 0 then
    wglDeleteContext(FGLRCx);
{$ENDIF}
{$IFDEF Linux}
  if vCurrentGLRCx = FGLRCx then
     vCurrentGLRCx := nil;
  if FGLRCx <> nil then
    glXDestroyContext(FDisplay, FGLRCx);
  XCloseDisplay(FDisplay);
{$ENDIF}
  // /if FDebugLog.Count>0 then assert(false,FDebugLog.Text);
  FDebugLog.Free;
  inherited;
end;

procedure TGLContext.GetOGLVersion;
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

function TGLContext.getVSync: boolean;
begin
{$IFDEF MSWINDOWS}
  if WGL_EXT_swap_control then
  begin
    if wglGetSwapIntervalEXT = 0 then
      Result := False
    else
      Result := true;
  end
  else
    Result := False;
{$ENDIF}
{$IFDEF Linux}
  // if GLX_EXT_swap_control then begin
  // if glxGetSwapIntervalEXT=0 then result:=false else result:=true;
  // end else result:=false;
{$ENDIF}
end;

procedure TGLContext.Init;
begin
  glClearColor(0.0, 0.0, 0.0, 0.0); // Black Background
  glClearDepth(1.0); // Depth Buffer Setup
  glEnable(GL_DEPTH_TEST); // Enable Depth Buffer
  glDepthMask(true); // Enable Depth Write
  glDepthFunc(GL_LESS); // The Type Of Depth Test To Do
  glFrontFace(GL_CCW);
  { if not FForwardContext then begin
    glShadeModel(GL_SMOOTH);                 // Enables Smooth Color Shading
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;
    end;
  }
  glClear(GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT);
end;

function TGLContext.InitForwardContext: boolean;
{$IFDEF MSWINDOWS}
const
  iPixelFormatAttribList: array [0 .. 14] of integer = (WGL_DRAW_TO_WINDOW_ARB,
    GL_True, WGL_SUPPORT_OPENGL_ARB, GL_True, WGL_DOUBLE_BUFFER_ARB, GL_True,
    WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_ARB, WGL_COLOR_BITS_ARB, 32,
    WGL_DEPTH_BITS_ARB, 24, WGL_STENCIL_BITS_ARB, 8, 0);
{$ENDIF}
var
{$IFDEF MSWINDOWS}
  iPixelFormat, iNumFormats: integer;
  pfd: TPixelFormatDescriptor;
{$ENDIF}
{$IFDEF Linux}
  fnelements: GLInt;
  vFBConfigs: PGLXFBConfigArray;
{$ENDIF}
  RC: HGLRC;
begin
{$IFDEF MSWINDOWS}
  Result := False;
  wglChoosePixelFormatARB(DeviceContext, @iPixelFormatAttribList, nil, 1,
    @iPixelFormat, @iNumFormats);
  if not SetPixelFormat(DeviceContext, iPixelFormat, @pfd) then
    exit;
  ClearIAttribs;
  AddIAttrib(WGL_CONTEXT_MAJOR_VERSION_ARB, FMajorVersion);
  AddIAttrib(WGL_CONTEXT_MINOR_VERSION_ARB, FMinorVersion);
  if FDebugContext then
    AddIAttrib(WGL_CONTEXT_FLAGS_ARB, WGL_CONTEXT_FLAG_DEBUG_BIT);
  AddIAttrib(WGL_CONTEXT_PROFILE_MASK_ARB, WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB);
  RC := wglCreateContextAttribsARB(DeviceContext, 0, @FiAttribs[0]);
  if (RC <> 0) then begin
    FGLRCx := RC;
    ActivateRenderingContext(DeviceContext, FGLRCx);
    vCurrentGLRCx := FGLRCx;
    Result := True;
  end;
{$ENDIF}
{$IFDEF Linux}
  XSync(FDisplay, False);
  vFBConfigs := glxChooseFBConfig(FDisplay, FCurScreen, @FiAttribs[0],
    @fnelements);
  ClearIAttribs;
  AddIAttrib(GLX_CONTEXT_MAJOR_VERSION_ARB, FMajorVersion);
  AddIAttrib(GLX_CONTEXT_MINOR_VERSION_ARB, FMinorVersion);
  if FDebugContext then
    AddIAttrib(GLX_CONTEXT_FLAGS_ARB, GLX_CONTEXT_DEBUG_BIT_ARB);
  AddIAttrib(GLX_CONTEXT_PROFILE_MASK_ARB, GLX_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB);
  RC := glxCreateContextAttribsARB(FDisplay, vFBConfigs[0], nil, true, @FiAttribs[0]);
  if (RC <> nil) then begin
    FGLRCx := RC;
    ActivateRenderingContext(FDisplay, FDC, FGLRCx);
    vCurrentGLRCx := FGLRCx;
    Result := True;
  end;
{$ENDIF}

  if Result and FDebugContext then
  begin
    glDebugMessageCallbackARB(GLDebugCallback, Self);
    glDebugMessageControlARB(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, nil, true);
    glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB);
  end;
end;

procedure TGLContext.InitializeContext(aDC: HDC);
var
  temp: HGLRC;
{$IFDEF Linux}
  vGTKWidget: PGTKWidget;
  ptr: pointer;
{$ENDIF}
begin
  // if FInitialized then exit else inherited;
  FGLInit := InitOpenGL;
  FInitialized := FGLInit;
  assert(FGLInit, 'Can''t initilize OpenGL');
{$IFDEF MSWINDOWS}
  FDC := aDC;
  SetDCPixelFormat(DeviceContext);
  FPfdChanged := False;
  FGLRCx := wglCreateContext(DeviceContext);
  ActivateRenderingContext(DeviceContext, FGLRCx);
  GetOGLVersion;
  DeactivateRenderingContext;
  if FForwardContext then
  begin
    temp := FGLRCx;
    if InitForwardContext then begin
      wglDeleteContext(temp);
      Activate;
    end else begin
      FForwardContext := False;
      FDebugContext := False;
      FGLRCx := temp;
      Activate;
    end;
  end;
{$ENDIF}
{$IFDEF Linux}
  vGTKWidget := TGtkDeviceContext(aDC).Widget;
  if assigned(vGTKWidget) then
    ptr := pointer(vGTKWidget)
  else
    ptr := pointer(aDC);
  vGTKWidget := GetFixedWidget(ptr);
  // Dirty workaround: force realize
  gtk_widget_realize(vGTKWidget);
  gtk_widget_set_double_buffered(vGTKWidget, False);
  FDC := GDK_WINDOW_XWINDOW(PGdkDrawable(vGTKWidget^.window));

  FDisplay := XOpenDisplay(nil);
  if (FDisplay=nil) then
    raise Exception.Create('Failed to get display');
  FCurScreen := XDefaultScreen(FDisplay);
  ClearIAttribs;
  ChooseGLXFormat;
  FPfdChanged := False;

  AddIAttrib(GLX_X_RENDERABLE, GL_True);
  AddIAttrib(GLX_RENDER_TYPE, GLX_RGBA_BIT);
  AddIAttrib(GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT);
  FGLRCx := glXCreateNewContext(FDisplay, FFBConfigs[0], GLX_RGBA_TYPE,
    nil, true);
  ActivateRenderingContext(FDisplay, FDC, FGLRCx);
  GetOGLVersion;
  FForwardContext := true;
  DeactivateRenderingContext(FDisplay);
  if FForwardContext then
  begin
    temp := FGLRCx;
    if InitForwardContext then
      glXDestroyContext(FDisplay, temp)
    else
    begin
      FForwardContext := False;
      FDebugContext := False;
      FGLRCx := temp;
      Activate;
    end;
  end;
{$ENDIF}
end;

{$IFDEF Linux}

procedure TGLContext.SwapBuffers;
begin
  glXSwapBuffers(FDisplay, FDC);
end;
{$ENDIF}

function TGLContext.ParseDebug(aSource, aType, aId, aSeverity: cardinal;
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

procedure TGLContext.Resize(Width, Height: integer);
begin
  Activate;
  glViewPort(0, 0, Width, Height);
end;

initialization

RegisterClasses([TGLViewer, TGLContext]);

end.