program ColladaImportTest;

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
  uMainUnit in 'uMainUnit.pas' {Form5};

begin
  {$IFNDEF FPC}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
