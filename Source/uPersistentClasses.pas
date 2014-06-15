unit uPersistentClasses;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, uBaseTypes, ulists;

type

  TFreeingBehavior = (fbManual, fbNoSubscibers, fbGarbageCollector);

  TPersistentResource = class;
  TNotifiableObject = class;

  INotifiable = interface
  ['{78846929-0168-4CA5-8777-7B136920AB80}']
    procedure Subscribe(Subscriber: TNotifiableObject);
    procedure UnSubscribe(Subscriber: TNotifiableObject);
    procedure DispatchMessage(Msg: Cardinal; Params: pointer = nil);
    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil);
  end;

  TNotifyDelegate = TDelegate<TNotifyEvent>;
  INotifyDelegate = IDelegate<TNotifyEvent>;

  TNotifiableObject = class(TAggregatedObject, INotifiable)
  private
    FSubscribers: TList; // List of TNotifiableObject
    FOnDestroy: TNotifyDelegate;
    function GetOnDestroy: INotifyDelegate;
    procedure FreeSubscriptions;
    procedure AttachResource(Resource: TNotifiableObject); virtual;
    procedure DetachResource(Resource: TNotifiableObject); virtual;
  protected
    procedure DoDestroy; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Subscribe(Subscriber: TNotifiableObject); virtual;
    procedure UnSubscribe(Subscriber: TNotifiableObject); virtual;
    procedure DispatchMessage(Msg: Cardinal; Params: pointer = nil); virtual;
    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); virtual;

    property OnDestroy: INotifyDelegate read GetOnDestroy;
  end;

  TReference<T: class> = class
  private
    FReference: T;
    function GetReference: T; Inline;
    procedure SetReference(const Value: T);
  public
    constructor Create(const AReference: T);
    destructor Destroy; override;
    property Reference: T read GetReference write SetReference;
  end;

  TTReference<T: class> = record
  private
    FReference: TReference<T>;
    function GetReference: T; Inline;
    procedure SetReference(Value: T);
  public
    procedure FreeRef;

    property Reference: T read GetReference write SetReference;

    class operator Implicit(Value: T): TTReference<T>; Inline;
    class operator Implicit(Value: TTReference<T>): T; overload; Inline;
    class operator Explicit(Value: T): TTReference<T>; Inline;
    class operator Explicit(Value: TTReference<T>): T; overload; Inline;
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
    TagStorage: TObject;
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SaveToStream(s: TStream); virtual;
    procedure LoadFromStream(s: TStream); virtual;
    procedure SetGUID(GUIDString: string);
    property Owner: TObject read FOwner write setOwner;
    property Order: integer read FOrder;
  end;

  TPersistentResClass = class of TPersistentResource;


implementation

uses
  SysUtils;

{ TReference }

constructor TReference<T>.Create(const AReference: T);
begin
  FReference := AReference;
end;

destructor TReference<T>.Destroy;
begin
  FReference.Free;
  FReference := nil;
  inherited;
end;

function TReference<T>.GetReference: T;
begin
  Result := FReference;
end;

procedure TReference<T>.SetReference(const Value: T);
begin
  if Assigned(FReference) and (FReference <> Value) then
      FReference.Free;
  FReference := Value;
end;

{ TTReference }

function TTReference<T>.GetReference: T;
begin
  Result := FReference.Reference;
end;


procedure TTReference<T>.SetReference(Value: T);
begin
  {if Assigned(FReference) then
      FReference.Reference := Value
  else}
  FReference := nil;
  FReference := TReference<T>.Create(Value);
end;

class operator TTReference<T>.Implicit(Value: T): TTReference<T>;
begin
  Result.Reference := Value;
end;

class operator TTReference<T>.Implicit(Value: TTReference<T>): T;
begin
  Result := Value.Reference;
end;

class operator TTReference<T>.Explicit(Value: T): TTReference<T>;
begin
  Result := Value;
end;

class operator TTReference<T>.Explicit(Value: TTReference<T>): T;
begin
  Result := Value;
end;

procedure TTReference<T>.FreeRef;
begin
  FReference:=nil;
end;

{ TPersistentResource }

constructor TPersistentResource.Create;
begin
  CreateGuid(GUID);
  Version := 1;
  FOwner := nil;
  FOrder := Counter;
  Inc(Counter);
  inherited Create;
end;

destructor TPersistentResource.Destroy;
begin
  inherited;
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
  FOwner := Value;
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

procedure TNotifiableObject.AttachResource(Resource: TNotifiableObject);
begin
  if not assigned(Resource) then exit;
  Resource.Subscribe(Self);
  Self.Subscribe(Resource);
end;

constructor TNotifiableObject.Create;
begin
  FSubscribers := TList.Create;
end;

destructor TNotifiableObject.Destroy;
begin
  DoDestroy;
  FreeSubscriptions;
  FreeAndNil(FSubscribers);
  inherited;
end;

procedure TNotifiableObject.DetachResource(Resource: TNotifiableObject);
begin
  if not assigned(Resource) then exit;
  Resource.UnSubscribe(Self);
  Self.UnSubscribe(Resource);
end;

procedure TNotifiableObject.DispatchMessage(Msg: Cardinal; Params: pointer);
var i: integer;
begin
  for i:=0 to FSubscribers.Count - 1 do begin
    if assigned(FSubscribers[i]) then begin
      TNotifiableObject(FSubscribers[i]).Notify(Self, Msg, Params);
      if msg = NM_ObjectDestroyed then
        TNotifiableObject(FSubscribers[i]).UnSubscribe(Self);
    end;
  end;
  FSubscribers.Pack;
end;

procedure TNotifiableObject.DoDestroy;
var Event: TNotifyEvent;
    i: integer;
begin
  for i := 0 to FOnDestroy.List.Count-1 do FOnDestroy.List[i](Self);
end;

procedure TNotifiableObject.FreeSubscriptions;
begin
  while FSubscribers.Count>0 do begin
    if assigned(FSubscribers[0]) then begin
      TNotifiableObject(FSubscribers[0]).Notify(Self,NM_ObjectDestroyed);
    end;
    FSubscribers.Delete(0);
  end;
end;

function TNotifiableObject.GetOnDestroy: INotifyDelegate;
begin
  Result := FOnDestroy;
end;

procedure TNotifiableObject.Notify(Sender: TObject; Msg: Cardinal;
  Params: pointer);
begin
if not assigned(Sender) then exit;
    case Msg of
      NM_ObjectDestroyed: UnSubscribe(TNotifiableObject(Sender));
//      NM_DebugMessageStr:
//        writetolog('['+inttostr(integer(Self))+']'+'['+ClassName+']'+pchar(Params));
    end;
end;

procedure TNotifiableObject.Subscribe(Subscriber: TNotifiableObject);
begin
  if not assigned(FSubscribers) then exit;
  if FSubscribers.IndexOf(Subscriber) < 0 then begin
    FSubscribers.Add(Subscriber);
  end;
end;

procedure TNotifiableObject.UnSubscribe(Subscriber: TNotifiableObject);
var
  n: integer;
begin
  if not assigned(FSubscribers) then exit;

  n := FSubscribers.IndexOf(Subscriber);
  if n >= 0 then begin
    FSubscribers[n]:=nil;
  end;
end;

end.

