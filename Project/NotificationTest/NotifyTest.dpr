program NotifyTest;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  {$IFDEF FPC}
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Forms,Interfaces,
  {$ELSE}
  Forms,
  {$ENDIF}
  uMainUnit in 'uMainUnit.pas' {Form1};

begin
  {$IFNDEF FPC}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
