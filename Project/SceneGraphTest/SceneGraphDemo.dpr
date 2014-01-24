program SceneGraphDemo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}



uses
  Forms,
  SceneGraphProject in 'SceneGraphProject.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.