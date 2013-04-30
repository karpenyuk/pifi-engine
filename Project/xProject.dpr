program xProject;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}



uses
  {$IFDEF FPC}
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  {$ENDIF}
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
  {$IFNDEF FPC}jpegdec in '..\Source\jpegdec.pas',{$ENDIF}
  dglOpenGL in '..\Source\GLViewer\dglOpenGL.pas',
  uGLViewer in '..\Source\GLViewer\uGLViewer.pas',
  uMath in '..\Source\uMath.pas',
  uBaseRenders in '..\Source\uBaseRenders.pas',
  uShaderGen in '..\Source\uShaderGen.pas',
  uGenericsRBTree in '..\Source\uGenericsRBTree.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.