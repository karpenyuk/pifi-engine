program xProject;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}



uses
  Forms,
  TestProject in 'TestProject.pas' {Form2},
  uPrimitives in '..\Source\uPrimitives.pas',
  uBaseClasses in '..\Source\uBaseClasses.pas',
  uBaseGL in '..\Source\uBaseGL.pas',
  uLists in '..\Source\uLists.pas',
  uVMath in '..\Source\uVMath.pas',
  uBaseTypes in '..\Source\uBaseTypes.pas',
  uRenderResource in '..\Source\uRenderResource.pas',
  uGLRenders in '..\Source\uGLRenders.pas',
  uMiscUtils in '..\Source\uMiscUtils.pas',
  jpegdec in '..\Source\jpegdec.pas',
  dglOpenGL in '..\Source\GLViewer\dglOpenGL.pas',
  uGLViewer in '..\Source\GLViewer\uGLViewer.pas',
  uMath in '..\Source\uMath.pas',
  uBaseRenders in '..\Source\uBaseRenders.pas',
  uShaderGen in '..\Source\uShaderGen.pas',
  uGenericsRBTree in '..\Source\uGenericsRBTree.pas',
  uDataAccess in '..\Source\uDataAccess.pas',
  uImageFormats in '..\Source\uImageFormats.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.