program TransfTest;

uses
  FMX.Platform.Win,
  FMX.Forms,
  Math,
  Windows,
  Messages,
  SysUtils,
  uGLWindow,
  dglOpenGL,
  uTest in 'uTest.pas' {Form1} ,
  uMain in 'uMain.pas';

{$R *.res}

//function newWndProc(hwnd: hwnd; uMsg: UINT; wParam: wParam; lParam: lParam)
//  : LRESULT; stdcall;
//var
//  Mess: TMessage;
//begin
//  if hwnd = FmxHandleToHWND(Form1.Handle) then
//  begin
//    Result := CallWindowProc(OldWindowProc, hwnd, uMsg, wParam, lParam);
//    glwnd.DrawGLScene;
//    if glwnd.FrameTime <> 0 then
//      glwnd.Caption := '[' +
//        floattostr(roundto(1 / glwnd.FrameTime, 2)) + ']'
//    else
//      glwnd.Caption := '[NAN]';
//    glwnd.SwapBuffer;
//  end;
//end;

begin
  Application.Initialize;
  InitOpenGL;
  Demo := TDemo.Create;
  glwnd := TGLWindow.Create;
  glwnd.onInitialize := Demo.ContextReady;
  glwnd.onResize := Demo.onResize;
  glwnd.onRender := Demo.SceneRender;
  glwnd.OnDebugMessage := Demo.OnDebugMessage;
  glwnd.onMouseDown := Demo.onMouseDown;
  glwnd.onMouseMove := Demo.onMouseMove;
  glwnd.onMouseWheel := Demo.onMouseWheel;
  glwnd.SetPixelFormatBits(32, 24, 0, 0, true, true);
  glwnd.CreateWindow('[NAN]', 640, 480, false);
  glwnd.VSync := true;
  //nwp := Integer(@newWndProc);
  Application.CreateForm(TForm1, Form1);

  Application.Run;
  glwnd.Free;
end.
