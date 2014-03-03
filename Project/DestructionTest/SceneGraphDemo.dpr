program SceneGraphDemo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}



uses
  FastMM4,
  Forms,
  SceneGraphProject in 'SceneGraphProject.pas' {Form2},
  AddMeshDlg in 'AddMeshDlg.pas' {AddMeshForm};

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.