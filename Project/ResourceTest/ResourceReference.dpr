program ResourceReference;

{$APPTYPE CONSOLE}

uses
  FastMM4, Windows, Classes, SysUtils, uLists, uMiscUtils,
  uPersistentClasses, uRenderResource, uStorage;

type
  TTestEvent = record
    procedure onDestroyEvent(Sender: TObject);
  end;

var
  TestRes: TMaterial;
  TestRes2: TMaterial;
  TestResRef: TMaterialRef;
  TestEvent: TTestEvent;

{ TTestEvent }

procedure TTestEvent.onDestroyEvent(Sender: TObject);
begin
  if Sender is TPersistentResource then
    writeln('OnDestroyEvent: ', TPersistentResource(Sender).GUID.ToString);
end;

begin

  ReportMemoryLeaksOnShutdown := true;

  TestRes := Storage.CreateMaterial;
  TestRes.OnDestroy.Add(TestEvent.onDestroyEvent);
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
