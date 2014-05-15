unit uDelegates;

interface

uses Generics.Collections;


{ IDelegate<T> }

type
  IDelegate<T> = interface
    ['{ADBC29C1-4F3D-4E4C-9A79-C805E8B9BD92}']
    procedure Add(const Handler: T);
    procedure Remove(const Handler: T);
  end;

{ IDelegateContainer<T> }

  IDelegateContainer<T> = interface
    ['{ED255F00-3112-4315-9E25-3C1B3064C932}']
    function GetDelegate: IDelegate<T> ;
    function GetEnumerator: TEnumerator<T>;
    property Delegate: IDelegate<T> read GetDelegate;
    property Enumerator: TEnumerator<T> read GetEnumerator;
  end;

{ TDelegateImpl<T> }

  TDelegateImpl<T> = class(TInterfacedObject, IDelegate<T>)
  private
    FList: TList<T>;
  protected
    { IDelegate<T> }
    procedure Add(const Event: T);
    procedure Remove(const Event: T);
  public
    constructor Create(List: TList<T>);
  end;

{ TDelegateContainerImpl<T> }

  TDelegateContainerImpl<T> = class(TInterfacedObject, IDelegateContainer<T>)
  private
    FDelegate: IDelegate<T>;
    FList: TList<T>;
  protected
    { IDelegateContainer<T> }
    function GetDelegate: IDelegate<T>;
    function GetEnumerator: TEnumerator<T>;
  public
    destructor Destroy; override;
  end;

{ TDelegate<T> }

  TDelegate<T> = record
  private
    FContainer: IDelegateContainer<T>;
    function GetContainer: IDelegateContainer<T>;
  public
    class operator Implicit(var Delegate: TDelegate<T>): IDelegate<T>;
    function GetEnumerator: TEnumerator<T>;
    procedure Add(const Handler: T);
    procedure Remove(const Handler: T);
  end;

{ Example usage:

  TNotifyDelegate = TDelegate<TNotifyEvent>;
  INotifyDelegate = IDelegate<TNotifyEvent>; }

implementation

{ TDelegateImpl<T> }

constructor TDelegateImpl<T>.Create(List: TList<T>);
begin
  inherited Create;
  FList := List;
end;

{ TDelegateImpl<T>.IDelegate<T> }

procedure TDelegateImpl<T>.Add(const Event: T);
begin
  if FList.IndexOf(Event) < 0 then
    FList.Add(Event);
end;

procedure TDelegateImpl<T>.Remove(const Event: T);
begin
  if FList.IndexOf(Event) > -1 then
    FList.Remove(Event);
end;

{ TDelegateContainerImpl<T> }

destructor TDelegateContainerImpl<T>.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

{ TDelegateContainerImpl<T>.IDelegateContainer<T> }

function TDelegateContainerImpl<T>.GetDelegate: IDelegate<T>;
begin
  if FList = nil then
    FList := TList<T>.Create;
  if FDelegate = nil then
    FDelegate := TDelegateImpl<T>.Create(FList);
  Result := FDelegate;
end;

function TDelegateContainerImpl<T>.GetEnumerator: TEnumerator<T>;
begin
  if FList = nil then
    FList := TList<T>.Create;
  Result := FList.GetEnumerator;
end;

{ TDelegate<T> }

class operator TDelegate<T>.Implicit(var Delegate: TDelegate<T>): IDelegate<T>;
begin
  Result := Delegate.GetContainer.Delegate;
end;

function TDelegate<T>.GetContainer: IDelegateContainer<T>;
begin
  if FContainer = nil then
    FContainer := TDelegateContainerImpl<T>.Create;
  Result := FContainer;
end;

function TDelegate<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := GetContainer.GetEnumerator;
end;

procedure TDelegate<T>.Add(const Handler: T);
begin
  GetContainer.Delegate.Add(Handler);
end;

procedure TDelegate<T>.Remove(const Handler: T);
begin
  GetContainer.Delegate.Remove(Handler);
end;

end.