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
  TMaterialRef(TestRes);
//For release directly created resources you could use one of these statements:
//TMaterialRef(TestRes); - wrap it to the reference type
//tempResRef := TestRes; - directly assign it to the reference var
//Storage.RemoveReference(TestRes); - manually remove it from collection with function
//Storage.GetReference<TMaterial>(TestRes.GUID).Reference := nil; - or manually

  writeln(TestRes.GUID.ToString);
//  writeln(Temp.Reference.GUID.ToString);
//  writeln(Temp.GUID.ToString);

  writeln(TestRes2.GUID.ToString);
  writeln(TestResRef.Reference.GUID.ToString);


  readln;
end.
