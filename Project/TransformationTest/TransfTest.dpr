program TransfTest;

uses
  Math,
  Windows,
  Classes,
  Messages,
  SysUtils,
  uGLWindow,
  dglOpenGL,
  uMain in 'uMain.pas',
  uAI in 'uAI.pas';

{$R *.res}

const
  WINCAPTION = 'Transformation test';

function WinMain(hInstance: HINST): integer; stdcall;
var
  msg: TMsg;
  done: Bool;
  fs: boolean;
  glwnd: TGLWindow;
begin
  done := false;
  fs := false;
  Demo := TDemo.Create;
  glwnd := TGLWindow.Create;
  glwnd.onInitialize := Demo.ContextReady;
  glwnd.onResize := Demo.onResize;
  glwnd.onRender := Demo.SceneRender;
  glwnd.OnDebugMessage := Demo.onDebugMessage;
  glwnd.onMouseDown := Demo.onMouseDown;
  glwnd.onMouseMove := Demo.onMouseMove;
  glwnd.onMouseWheel := Demo.onMouseWheel;
  glwnd.OnProgress := Demo.OnProgress;
  glwnd.SetPixelFormatBits(32, 24, 0, 0, true, true);
  glwnd.CreateWindow(WINCAPTION, 640, 480, fs);
  glwnd.VSync := true;
  while not done do begin
    if (PeekMessage(msg, 0, 0, 0, PM_REMOVE)) then begin
      if msg.message = WM_QUIT then done := true
      else begin
        TranslateMessage(msg);
        DispatchMessage(msg);
      end;
    end else begin
      if glwnd.keys[VK_ESCAPE]
      then done := true
      else begin
        CheckSynchronize;
        glwnd.DrawGLScene;
        if glwnd.FrameTime <> 0 then
            glwnd.Caption := WINCAPTION + ' [' +
            floattostr(roundto(1 / glwnd.FrameTime, 2)) + ']'
        else
            glwnd.Caption := WINCAPTION + ' [NAN]';
        glwnd.SwapBuffer;
      end;
      if glwnd.keys[VK_F1] then begin
        glwnd.keys[VK_F1] := false;
        glwnd.Free;
        fs := not fs;
        glwnd := TGLWindow.Create;
        glwnd.SetPixelFormatBits(32, 24, 0, 0, true, true);
        glwnd.CreateWindow(WINCAPTION, 640, 480, fs);
        glwnd.VSync := true;
      end;
      if glwnd.keys[VK_SPACE] then begin
        glwnd.keys[VK_SPACE] := false;
        glwnd.VSync := not glwnd.VSync;
      end;

    end;
  end;
  glwnd.Free;
  Demo.Free;
  result := msg.wParam;
end;

begin
  ReportMemoryLeaksOnShutdown := true;
  try
    InitOpenGL;
    WinMain(hInstance);
  except
    on E: Exception do
        Writeln(E.ClassName, ': ', E.message);
  end;
end.
