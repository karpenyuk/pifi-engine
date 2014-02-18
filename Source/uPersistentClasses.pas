unit uPersistentClasses;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes;

type

  TFreeingBehavior = (fbManual, fbNoSubscibers, fbGarbageCollector);

  TNotifiableObject = class(TObject)
  private
    class var FGarbageCollector: TList;
    class procedure DestroyGarbage;
  private
    FSubscribers: TList; // List of TNotifiableObject
    FUpdateCount: Integer;
    FFreeingBehavior: TFreeingBehavior;
    procedure SetFreeingBehavior(const Value: TFreeingBehavior);
    procedure AddToCollector;
    procedure RemoveFromCollector;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Subscribe(Subscriber: TNotifiableObject); virtual;
    procedure UnSubscribe(Subscriber: TNotifiableObject); virtual;
    procedure DispatchMessage(Msg: Cardinal; Params: pointer = nil); virtual;
    procedure Notify(Sender: TObject; Msg: Cardinal;
      Params: pointer = nil); virtual;
    // Ïðèîñòàíàâëèâàåò îòïðàâêó ñîîáùåíèé
    procedure BeginUpdate; virtual;
    // Âîñòàíàâëèâàåò îòïðàâêó ñîîáùåíèé
    function EndUpdate: Integer; virtual;
    property FreeingBehavior: TFreeingBehavior read FFreeingBehavior write
      SetFreeingBehavior;
  end;

  { TODO : Äîðàáîòàòü êëàññ TPersistentResource, ðåàëèçîâàâ ðåãèñòðàöèþ çàãðóæåííûõ ðåñóðñîâ â êîëëåêöèè îáúåêòîâ.
         : Serialize Owner property, Implement FixUp by object GUID
  }

  TPersistentResource = class(TNotifiableObject)
  private
    FOwner: TObject;
    FOrder: integer;
    class var Counter: integer;
  protected
    procedure setOwner(const Value: TObject); virtual;
    procedure WriteString(const s: string; const stream: TStream);
    function ReadString(const stream: TStream): string;
    procedure WriteInt(const Value: integer; const stream: TStream);
    function ReadInt(const stream: TStream): integer;
    procedure WriteBool(const Value: boolean; const stream: TStream);
    function ReadBool(const stream: TStream): boolean;
    procedure WriteGUID(const Value: TGUID; const stream: TStream);
    function ReadGUID(const stream: TStream): TGUID;
    procedure WriteFloat(const Value: single; const stream: TStream);
    function ReadFloat(const stream: TStream): single;
  public
    GUID: TGUID;
    Version: integer;
    Storage: TObject;
    constructor Create; virtual;
    destructor Destroy; override;
    constructor CreateOwned(aOwner: TObject = nil); virtual;

    procedure SaveToStream(s: TStream); virtual;
    procedure LoadFromStream(s: TStream); virtual;
    procedure SetGUID(GUIDString: string);
    class function IsInner: boolean; virtual;
    property Owner: TObject read FOwner write setOwner;
    property Order: integer read FOrder;
  end;

  TPersistentResClass = class of TPersistentResource;

implementation

uses
  SysUtils, uBaseTypes;

{ TPersistentResource }

constructor TPersistentResource.Create;
begin
  CreateGuid(GUID);
  Version := 1;
  FOwner := nil;
  FOrder := Counter;
  Inc(Counter);
  inherited;
end;

constructor TPersistentResource.CreateOwned(aOwner: TObject);
begin
  Create;
  FOwner := aOwner;
end;

destructor TPersistentResource.Destroy;
begin
  Assert(FUpdateCount = 0);
  FUpdateCount := 0;
  inherited Destroy;
end;

class function TPersistentResource.IsInner: boolean;
begin
  Result := False;
end;

procedure TPersistentResource.LoadFromStream(s: TStream);
var
  l: integer;
begin
  s.ReadBuffer(l, 4);
  s.Seek(l, soCurrent);
  s.ReadBuffer(GUID.D1, 4);
  s.ReadBuffer(GUID.D2, 2);
  s.ReadBuffer(GUID.D3, 2);
  s.ReadBuffer(GUID.D4, 8);
  s.ReadBuffer(Version, 4);
end;

function TPersistentResource.ReadBool(const stream: TStream): boolean;
begin
  stream.ReadBuffer(Result, sizeof(boolean));
end;

function TPersistentResource.ReadFloat(const stream: TStream): single;
begin
  stream.ReadBuffer(Result, 4);
end;

function TPersistentResource.ReadGUID(const stream: TStream): TGUID;
begin
  stream.ReadBuffer(Result.D1, 4);
  stream.ReadBuffer(Result.D2, 2);
  stream.ReadBuffer(Result.D3, 2);
  stream.ReadBuffer(Result.D4, 8);
end;

function TPersistentResource.ReadInt(const stream: TStream): integer;
begin
  stream.ReadBuffer(Result, 4);
end;

function TPersistentResource.ReadString(const stream: TStream): string;
var
  n: integer;
  s: string;
begin
  stream.ReadBuffer(n, 4);
  setlength(s, n);
  stream.ReadBuffer(s[1], n * 2);
  Result := s;
end;

procedure TPersistentResource.SaveToStream(s: TStream);
var
  l: integer;
begin
  l := length(ClassName);
  s.WriteBuffer(l, 4);
  s.WriteBuffer(ClassName[1], l);
  s.WriteBuffer(GUID.D1, 4);
  s.WriteBuffer(GUID.D2, 2);
  s.WriteBuffer(GUID.D3, 2);
  s.WriteBuffer(GUID.D4, 8);
  s.WriteBuffer(Version, 4);
end;

procedure TPersistentResource.SetGUID(GUIDString: string);
begin
  GUID := StringToGUID(GUIDString);
end;

procedure TPersistentResource.setOwner(const Value: TObject);
begin
  if assigned(FOwner) and (FOwner is TPersistentResource)
  then TPersistentResource(FOwner).UnSubscribe(Self);
  FOwner := Value;
  if assigned(FOwner) and (FOwner is TPersistentResource)
  then TPersistentResource(FOwner).Subscribe(Self);
end;

procedure TPersistentResource.WriteBool(const Value: boolean;
  const stream: TStream);
begin
  stream.WriteBuffer(Value, sizeof(boolean));
end;

procedure TPersistentResource.WriteFloat(const Value: single;
  const stream: TStream);
begin
  stream.WriteBuffer(Value, 4);
end;

procedure TPersistentResource.WriteGUID(const Value: TGUID;
  const stream: TStream);
begin
  stream.WriteBuffer(Value.D1, 4);
  stream.WriteBuffer(Value.D2, 2);
  stream.WriteBuffer(Value.D3, 2);
  stream.WriteBuffer(Value.D4, 8);
end;

procedure TPersistentResource.WriteInt(const Value: integer;
  const stream: TStream);
begin
  stream.WriteBuffer(Value, 4);
end;

procedure TPersistentResource.WriteString(const s: string;
  const stream: TStream);
var
  n: integer;
begin
  n := length(s);
  stream.WriteBuffer(n, 4);
  stream.WriteBuffer(s[1], n * 2);
end;

{ TNotifiableObject }

procedure TNotifiableObject.AddToCollector;
begin
  if FGarbageCollector = nil then
    FGarbageCollector := TList.Create;
  FGarbageCollector.Add(Self);
end;

procedure TNotifiableObject.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

constructor TNotifiableObject.Create;
begin
  inherited;
  FSubscribers := TList.Create;
  FFreeingBehavior := fbGarbageCollector;
  AddToCollector;
end;

destructor TNotifiableObject.Destroy;
begin
  DispatchMessage(NM_ObjectDestroyed);
  FreeAndNil(FSubscribers);
  RemoveFromCollector;
  inherited;
end;

class procedure TNotifiableObject.DestroyGarbage;
var
  I: Integer;
  list: TList;
begin
// Comented to findout memory leaks
//  if Assigned(FGarbageCollector) then
//  begin
//    list := FGarbageCollector;
//    FGarbageCollector := nil;
//    for I := list.Count - 1 downto 0 do
//      if Assigned(list[I]) then
//        TObject(list[I]).Free;
//    list.Free;
//  end;
end;

procedure TNotifiableObject.DispatchMessage(Msg: Cardinal; Params: pointer);
var i: integer;
begin
  if FUpdateCount = 0 then
  begin
    for i := 0 to FSubscribers.Count - 1 do
      if assigned(FSubscribers[i]) then
        TNotifiableObject(FSubscribers[i]).Notify(Self, Msg, Params);
  end;
end;

function TNotifiableObject.EndUpdate: Integer;
begin
  Dec(FUpdateCount);
  Assert(FUpdateCount >= 0);
  if FUpdateCount = 0 then
    DispatchMessage(NM_ResourceChanged);
  Result := FUpdateCount;
end;

procedure TNotifiableObject.Notify(Sender: TObject; Msg: Cardinal;
  Params: pointer);
begin
  // Do nothing
end;

procedure TNotifiableObject.RemoveFromCollector;
begin
  if Assigned(FGarbageCollector) and (FFreeingBehavior = fbGarbageCollector) then
    FGarbageCollector.Remove(Self);
end;

procedure TNotifiableObject.SetFreeingBehavior(const Value: TFreeingBehavior);
begin
  if Value <> FFreeingBehavior then
  begin
    if FFreeingBehavior = fbGarbageCollector then
      RemoveFromCollector;
    FFreeingBehavior := Value;
    if FFreeingBehavior = fbGarbageCollector then
      AddToCollector;
  end;
end;

procedure TNotifiableObject.Subscribe(Subscriber: TNotifiableObject);
begin
  if FSubscribers.IndexOf(Subscriber) < 0 then
    FSubscribers.Add(Subscriber);
end;

procedure TNotifiableObject.UnSubscribe(Subscriber: TNotifiableObject);
var
  n: integer;
begin
  n := FSubscribers.IndexOf(Subscriber);
  if n >= 0 then
    FSubscribers.Delete(n);
  if (FFreeingBehavior = fbNoSubscibers) and (FSubscribers.Count = 0) then
    Destroy;  
end;

initialization

finalization

  TNotifiableObject.DestroyGarbage;
  

end.
