program GLWindow;

{$APPTYPE CONSOLE}
{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

uses
  Math,
  Windows,
  Messages,
  System.SysUtils,
  uGLWindow,
  dglOpenGL,
  ColladaTest;

function WinMain(hInstance: HINST): integer; stdcall;
var
  msg: TMsg;
  done: Bool;
  fs: boolean;
  glwnd: TGLWindow;
begin
  done:=false;
  fs:=false;
  Demo:=TDemo.Create;
  glwnd:=TGLWindow.Create;
  glwnd.onInitialize:=Demo.ContextReady;
  glwnd.onResize:=demo.onResize;
  glwnd.onRender:=demo.SceneRender;
  glwnd.onMouseDown:=demo.onMouseDown;
  glwnd.onMouseMove:=demo.onMouseMove;
  glwnd.CreateWindow('OpenGL Framework',640,480,fs);
  glwnd.VSync:=true;
  while not done do begin
    if (PeekMessage(msg, 0, 0, 0, PM_REMOVE)) then begin
      if msg.message=WM_QUIT then done:=true
      else begin
        TranslateMessage(msg);
        DispatchMessage(msg);
      end;
    end else begin
      if glwnd.keys[VK_ESCAPE]
      then done:=true
      else begin
        glwnd.DrawGLScene;
        if glwnd.FrameTime<>0 then
          glwnd.Caption:='OpenGL Framework ['+floattostr(roundto(1/glwnd.FrameTime,2))+']'
        else
          glwnd.Caption:='OpenGL Framework [NAN]';
        glwnd.SwapBuffer;
      end;
      if glwnd.keys[VK_F1] then begin
        glwnd.Keys[VK_F1] := false;
        glwnd.Free;
        fs := not fs;
        glwnd:=TGLWindow.Create;
        glwnd.CreateWindow('OpenGL Framework',640,480,fs);
        glwnd.VSync:=true;
      end;
      if glwnd.keys[VK_F2] then begin
        glwnd.Keys[VK_F2] := false;
        glwnd.SetPixelFormatBits(32,32,0,0,false,false);
      end;
      if glwnd.keys[VK_SPACE] then begin
        glwnd.Keys[VK_SPACE] := false;
        glwnd.VSync:=not glwnd.VSync;
      end;

    end;
  end;
  glwnd.Free;
  result:=msg.wParam;
end;

begin
  ReportMemoryLeaksOnShutdown := true;
  try
    InitOpenGL;
    WinMain( hInstance );
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

