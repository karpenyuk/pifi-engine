{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLViewer; 

interface

uses
  uGLViewer, dglOpenGL, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('uGLViewer', @uGLViewer.Register); 
end; 

initialization
  RegisterPackage('GLViewer', @Register); 
end.
