program TesselationShaderTest;

uses
  Vcl.Forms,
  uMainUnit in 'uMainUnit.pas' {Form1},
  uWall in 'uWall.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
