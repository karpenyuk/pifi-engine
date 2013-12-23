program GLWindow;

{$APPTYPE CONSOLE}
{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

uses
  Math, Windows, Messages, SysUtils, uGLWindow, dglOpenGL;

function WinMain(hInstance: HINST): integer; stdcall;
var
  msg: TMsg;
  done: Bool;
  fs: boolean;
  glwnd: TGLWindow;
  t: double;
begin
  done:=false;
  fs:=false;
  glwnd:=TGLWindow.Create;
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
        t := gettime;
        glwnd.DrawGLScene;
        t := gettime-t;
        if t<>0 then
          glwnd.Caption:='OpenGL Framework ['+floattostr(roundto(1/t,2))+']'
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
        glwnd.SetPixelFormatBits(32,32,0,0);
      end;
    end;
  end;
  glwnd.Free;
  result:=msg.wParam;
end;

begin
  try
    InitOpenGL;
    WinMain( hInstance );
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
