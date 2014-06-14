program ResourceReference;

{$APPTYPE CONSOLE}

uses
  FastMM4, Windows, Classes, SysUtils, uLists, uMiscUtils,
  uPersistentClasses, uRenderResource, uStorage;


var
  TestRes: TMaterial;
  TestRes2: TMaterial;
  TestResRef, tempResRef: TMaterialRef;
  p,p1,p2: pointer;

begin
  ReportMemoryLeaksOnShutdown := true;

  TestRes := Storage.CreateMaterial;
  writeln('Res1: ', TestRes.GUID.ToString);
  TestRes2 := Storage.CreateMaterial;
  writeln('Res2: ', TestRes2.GUID.ToString);
  TestResRef := Storage.CreateMaterial;
  writeln('Res3: ', TestResRef.Reference.GUID.ToString);
  TestResRef := testRes2;

  writeln(TestRes.GUID.ToString);
  writeln(TestRes2.GUID.ToString);
  writeln(TestResRef.Reference.GUID.ToString);

  readln;
end.
