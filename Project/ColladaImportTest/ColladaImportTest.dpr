program ColladaImportTest;

uses
  Forms,
  uMainUnit in 'uMainUnit.pas' {Form5},
  uFileFormatDAE in '..\..\Source\uFileFormatDAE.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
