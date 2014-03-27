program SceneGraphDemo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}



uses
  Forms,
  SceneGraphProject in 'SceneGraphProject.pas' {Form2},
  SceneConstructor in 'SceneConstructor.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.