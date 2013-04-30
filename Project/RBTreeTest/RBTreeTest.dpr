program RBTreeTest;

{$APPTYPE CONSOLE}

uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF} Classes, SysUtils, uLists,
  {$IFNDEF FPC}
    Generics.Defaults,
  {$ENDIF}
  uMiscUtils,
  uGenericsRBTree;

const
  ItemsCount = 65536;
  QueriesCount = 1024;

function GetTime:double;
var Freq, Tick: Int64;
begin
  QueryPerformanceFrequency(Freq);
  QueryPerformanceCounter(Tick);
  Result:=Tick/Freq;
end;

function CompareInt(i1, i2: Pointer): Integer;
begin
  {$IFNDEF FPC}
  result:=BinaryCompare(i1,i2,16);
  {$ELSE}
  result:=CompareByte(i1,i2,16);
  {$ENDIF}
end;

var List: TRBTree;
    List2: GRedBlackTree<Integer, PGUID>;
    GUIDList: TList;
    i,n: integer;
    t: double;
    Node:  PRBNode;
    GUID: PGUID;
    s: string;
    ic, qc: integer;
begin
  try
    if ParamCount=2 then begin
      ic:=strtoint(ParamStr(1));
      qc:=strtoint(ParamStr(2));
    end else begin
      ic:=ItemsCount; qc:=QueriesCount;
    end;
    randomize;

    List:=TRBTree.Create(CompareInt);
    List2:=GRedBlackTree<Integer, PGUID>.Create(CompareInteger, nil);
    GUIDList:=TList.Create;
    write('Generating Tree...');
    for i:=0 to ic-1 do begin
      new(GUID); CreateGUID(GUID^);
      List.Add(GUID); GUIDList.Add(GUID);
      List2.Add(i, GUID);
    end;
    writeln('Complete. Press Enter.'); readln;
    t:=GetTime; n:=0;
    for i:=0 to qc-1 do begin
      node:=List.Find(GUIDList[Random(ic)]);
      if assigned(Node) then inc(n);
    end;
    t:=GetTime-t; t:=t*1000;
    str(t:5:3,s);
    writeln('TRBTree class. Found ',n,' objects in ',s,'ms'); readln;

    t:=GetTime; n:=0;
    for i:=0 to qc-1 do begin
      if List2.Find(Random(ic), GUID) then;
        inc(n);
    end;
    t:=GetTime-t; t:=t*1000;
    str(t:5:3,s);
    writeln('GRedBlackTree class. Found ',n,' objects in ',s,'ms'); readln;

    List.Free; List2.Free; FreeList(GUIDList);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.