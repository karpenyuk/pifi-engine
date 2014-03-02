unit uLists;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}
{$IFDEF FPC}
{$H+} // Enable long strings.
{$ASSERTIONS+}
{$ENDIF}

interface

uses Classes, uBaseTypes, uPersistentClasses, uMiscUtils, uVMath;

Type

  TAbstractDataList = class
  protected
    function getCount: Integer; virtual; abstract;
    procedure setCount(const Value: Integer); virtual; abstract;
  public
    constructor Create; virtual;
    function AddRaw(Item: Pointer): Integer; virtual; abstract;
    procedure Join(AList: TAbstractDataList; const AMatrix: TMatrix);
      virtual; abstract;
    procedure Flush; virtual; abstract;
    procedure Clear; virtual; abstract;
    function GetItemAddr(AnIndex: Integer): Pointer; virtual; abstract;
    function IsItemsEqual(Index1, Index2: Integer): Boolean; virtual; abstract;
    function ItemSize(): Integer; virtual; abstract;
    procedure Transform(const AMatrix: TMatrix); virtual;
    function GetItemAsVector(AnIndex: Integer): TVector; virtual;
    procedure SetItemAsVector(AnIndex: Integer;
      const aVector: TVector); virtual;
    property Count: Integer read getCount write setCount;
  end;

  TAbstractDataListClass = class of TAbstractDataList;

  TAbstractDataListArray = array of TAbstractDataList;

  TDataList<T> = class(TAbstractDataList)
  private type
    TItemsArray = array of T;
  protected
    FItems: TItemsArray;
    FCount: Integer;
    function getCapacity: Integer;
    function getData: Pointer;
    function getItem(Index: Integer): T;
    procedure setCapacity(const Value: Integer);
    procedure setItem(Index: Integer; const Value: T);
    function getSize: Integer;
    function getCount: Integer; override;
    procedure setCount(const Value: Integer); override;
  public
    constructor Create; override;

    function Add(const Item: T): Integer;
    function AddRaw(Item: Pointer): Integer; override;
    procedure Join(AList: TAbstractDataList; const AMatrix: TMatrix); override;
    procedure Flush; override;
    function GetItemAddr(AnIndex: Integer): Pointer; override;
    function IsItemsEqual(Index1, Index2: Integer): Boolean; override;
    function ItemSize(): Integer; override;
    function First: T;
    function Last: T;

    procedure Assign(aSource: TDataList<T>); virtual;
    procedure ToArray(var aDestination); // Небезопасно
    procedure Clear; override;
    procedure Delete(Index: Integer);
    function IndexOf(const AnItem: T): Integer;
    function Exchange(const AnItemIn, AnItemOut: T): Boolean; overload;
    procedure Exchange(Index1, Index2: Integer); overload;

    property Data: Pointer read getData;
    property Size: Integer read getSize;
    property Count: Integer read getCount write setCount;
    property Capacity: Integer read getCapacity write setCapacity;
    property Items[Index: Integer]: T read getItem write setItem; default;
  end;

  TPtrList = TDataList<Pointer>;
  TObjectList = class(TDataList<TObject>)
  public
    procedure FreeObjects;
  end;

  TIntegerList = class(TDataList<Integer>)
  public
    function GetItemAsVector(AnIndex: Integer): TVector; override;
    procedure SetItemAsVector(AnIndex: Integer;
      const aVector: TVector); override;
    procedure Transform(const AMatrix: TMatrix); override;
  end;

  TSingleList = class(TDataList<Double>)
  public
    function GetItemAsVector(AnIndex: Integer): TVector; override;
    procedure SetItemAsVector(AnIndex: Integer;
      const aVector: TVector); override;
    procedure Transform(const AMatrix: TMatrix); override;
  end;

  TDoubleList = class(TDataList<Double>)
  public
    function GetItemAsVector(AnIndex: Integer): TVector; override;
    procedure SetItemAsVector(AnIndex: Integer;
      const aVector: TVector); override;
    procedure Transform(const AMatrix: TMatrix); override;
  end;

  TVec2List = class(TDataList<vec2>)
  public
    function GetItemAsVector(AnIndex: Integer): TVector; override;
    procedure SetItemAsVector(AnIndex: Integer;
      const aVector: TVector); override;
    procedure Transform(const AMatrix: TMatrix); override;
    procedure Join(AList: TAbstractDataList; const AMatrix: TMatrix); override;
  end;

  TVec3List = class(TDataList<vec3>)
  public
    procedure SetNormalize();
    function Cross(AList: TVec3List): TVec3List;
    function GetItemAsVector(AnIndex: Integer): TVector; override;
    procedure SetItemAsVector(AnIndex: Integer;
      const aVector: TVector); override;
    procedure Transform(const AMatrix: TMatrix); override;
    procedure Join(AList: TAbstractDataList; const AMatrix: TMatrix); override;
  end;

  TVec4List = class(TDataList<vec4>)
  public
    function GetItemAsVector(AnIndex: Integer): TVector; override;
    procedure SetItemAsVector(AnIndex: Integer;
      const aVector: TVector); override;
    procedure Transform(const AMatrix: TMatrix); override;
    procedure Join(AList: TAbstractDataList; const AMatrix: TMatrix); override;
  end;

  THashDictionaryNode = record
    Key: Integer;
    KeyName: string;
    KeyGUID: TGUID;
    Value: TObject;
  end;

  // Key+Value pair list with linear searching
  TObjectsDictionary = class(TNotifiableObject)
  protected
    FCount: Integer;
    FItems: array of THashDictionaryNode;
    function StringHashKey(const name: string): Integer;
    function AddKey(const Key: string; Value: TObject): Integer;
      overload; virtual;
    function AddKey(const Key: TGUID; Value: TObject): Integer;
      overload; virtual;
    function GetValue(const Key: string): TObject; overload; virtual;
    function GetValue(const Key: TGUID): TObject; overload; virtual;
  public
    constructor Create; override;
    procedure Assign(aSource: TObjectsDictionary);
    property Count: Integer read FCount;
    function inList(const aItem: TObject): boolean;
  end;

  TLinkedObjectItem = record
    Key: TObject;
    Value: TObject;
  end;

  TLinkedObjectsList = class
  protected
    FCount: Integer;
    FItems: array of TLinkedObjectItem;
  public
    constructor Create;
    procedure AddKey(const Key, Value: TObject); virtual;
    function GetValue(const Key: TObject): TObject; virtual;
  end;

  TRBNodeColor = (ncRed, ncBlack);
  PRBNode = ^TRBNode;

  TRBNode = record
    Data: Pointer;
    k: Pointer;
    left, right, parent: PRBNode;
    color: TRBNodeColor;
    class procedure RBInc(var x: PRBNode); static;
    class procedure RBDec(var x: PRBNode); static;
  end;

  TRBTree = class
  private
    root: PRBNode;
    leftmost: PRBNode;
    rightmost: PRBNode;
    compareFunc: TListSortCompare;

    procedure RotateLeft(var x: PRBNode);
    procedure RotateRight(var x: PRBNode);
    function Minimum(var x: PRBNode): PRBNode;
    function Maximum(var x: PRBNode): PRBNode;

  public
    constructor Create(Compare: TListSortCompare);
    destructor Destroy(); override;

    procedure Clear();

    function Find(Key: Pointer): PRBNode;
    function Add(Key: Pointer): PRBNode;
    procedure Delete(z: PRBNode);

    property First: PRBNode read leftmost;
    property Last: PRBNode read rightmost;
  end;

function IntPtrComparer(i1, i2: Pointer): Integer;

implementation

function IntPtrComparer(i1, i2: Pointer): Integer;
begin
  result := Integer(i1) - Integer(i2);
end;

{ TDataList<T> }

function TDataList<T>.Add(const Item: T): Integer;
begin
  if Length(FItems) > FCount then
    FItems[FCount] := Item
  else
  begin
    Setlength(FItems, Length(FItems) * 2);
    FItems[FCount] := Item;
  end;
  result := FCount;
  Inc(FCount);

end;

function TDataList<T>.AddRaw(Item: Pointer): Integer;
var
  AnItem: ^T;
begin
  AnItem := Item;
  result := Add(AnItem^);

end;

procedure TDataList<T>.Assign(aSource: TDataList<T>);
begin
  Assert(Pointer(aSource) <> nil, 'Source list is not assigned!');
  Count := aSource.Count;
  Move(aSource.Data^, Data^, aSource.Size);

end;

procedure TDataList<T>.Clear;
begin
  FCount := 0;

end;

constructor TDataList<T>.Create;
begin
  inherited Create;
  Setlength(FItems, 12);
  FCount := 0;
end;

procedure TDataList<T>.Delete(Index: Integer);
var
  i: Integer;
begin
  if (index < 0) or (index >= FCount) then
    Assert(false, 'Out Of Bounds');
  if Index < FCount - 1 then
    for i := Index + 1 to FCount - 1 do
      FItems[i - i] := FItems[i];
  Dec(FCount);

end;

procedure TDataList<T>.Exchange(Index1, Index2: Integer);
var
  ex: T;
begin
  Assert(Index1 < FCount);
  Assert(Index2 < FCount);
  ex := FItems[Index1];
  FItems[Index1] := FItems[Index2];
  FItems[Index2] := ex;
end;

function TDataList<T>.Exchange(const AnItemIn, AnItemOut: T): Boolean;
var
  i: Integer;
begin
  i := IndexOf(AnItemOut);
  if i > -1 then
  begin
    FItems[i] := AnItemIn;

    exit(True);
  end;
  result := false;
end;

function TDataList<T>.IndexOf(const AnItem: T): Integer;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    if CompareMem(@FItems[i], @AnItem, SizeOf(T)) then
      exit(i);
  result := -1;
end;

function TDataList<T>.First: T;
begin
  Assert(FCount > 0);
  result := FItems[0];
end;

procedure TDataList<T>.Flush;
begin
  FCount := 0;

end;

function TDataList<T>.getCapacity: Integer;
begin
  result := Length(FItems);
end;

function TDataList<T>.getCount: Integer;
begin
  result := FCount;
end;

function TDataList<T>.getData: Pointer;
begin
  result := @FItems[0];
end;

function TDataList<T>.getItem(Index: Integer): T;
begin
  result := FItems[Index];
end;

function TDataList<T>.GetItemAddr(AnIndex: Integer): Pointer;
begin
  result := @FItems[AnIndex];
end;

function TDataList<T>.getSize: Integer;
begin
  result := FCount * SizeOf(T);
end;

function TDataList<T>.IsItemsEqual(Index1, Index2: Integer): Boolean;
begin
  Assert(Index1 < FCount);
  Assert(Index2 < FCount);
  result := CompareMem(@FItems[Index1], @FItems[Index2], SizeOf(T));
end;

function TDataList<T>.ItemSize: Integer;
begin
  result := SizeOf(T);
end;

procedure TDataList<T>.Join(AList: TAbstractDataList; const AMatrix: TMatrix);
var
  len: Integer;
begin
  Assert(Self.ClassType = AList.ClassType);
  len := AList.Count;
  if len > 0 then
  begin
    if Length(FItems) <= FCount + len then
      Setlength(FItems, Length(FItems) + len);
    Move(PByte(AList.GetItemAddr(0))^, FItems[FCount], len * SizeOf(T));
    FCount := FCount + len;

  end;
end;

function TDataList<T>.Last: T;
begin
  Assert(FCount > 0);
  result := FItems[FCount - 1];
end;

procedure TDataList<T>.setCapacity(const Value: Integer);
begin
  Setlength(FItems, Value);
end;

procedure TDataList<T>.setCount(const Value: Integer);
begin
  if Value >= Length(FItems) then
    Setlength(FItems, Value);
  FCount := Value;

end;

procedure TDataList<T>.setItem(Index: Integer; const Value: T);
begin
  FItems[Index] := Value;
end;

procedure TDataList<T>.ToArray(var aDestination);
var
  dest: ^TItemsArray;
begin
  dest := @aDestination;
  dest^ := Copy(FItems, 0, Count);
end;

{ THashedObjectList }

function TObjectsDictionary.StringHashKey(const name: string): Integer;
var
  i, n, res: Integer;
begin
  if name = '' then
    result := -1
  else
  begin
    n := Length(name);
    res := n;
    for i := 1 to n do
      res := (res shl 1) + Byte(name[i]);
    result := res;
  end;
end;

function TObjectsDictionary.AddKey(const Key: string; Value: TObject): Integer;
var
  iKey, i: Integer;
begin
  iKey := StringHashKey(Key);
  for i := 0 to FCount - 1 do
    if (FItems[i].Key = iKey) and (FItems[i].KeyName = Key) then
      exit(i);
  if FCount >= Length(FItems) then
    Setlength(FItems, FCount * 2);
  FItems[FCount].Key := iKey;
  FItems[FCount].KeyName := Key;
  FItems[FCount].Value := Value;
  result := FCount;
  Inc(FCount);
end;

function TObjectsDictionary.AddKey(const Key: TGUID; Value: TObject): Integer;
var
  iKey, i: Integer;
begin
  iKey := GetHashFromBuff(Key, 16);
  for i := 0 to FCount - 1 do
    if (FItems[i].Key = iKey) and TGUIDEx.IsEqual(FItems[i].KeyGUID, Key) then
      exit(i);
  if FCount >= Length(FItems) then
    Setlength(FItems, FCount * 2);
  FItems[FCount].Key := iKey;
  FItems[FCount].KeyGUID := Key;
  FItems[FCount].Value := Value;
  result := FCount;
  Inc(FCount);
end;

procedure TObjectsDictionary.Assign(aSource: TObjectsDictionary);
begin
  FCount := aSource.FCount;
  FItems := aSource.FItems;
end;

constructor TObjectsDictionary.Create;
begin
  inherited Create;
  Setlength(FItems, 128);
  FCount := 0;
end;

function TObjectsDictionary.GetValue(const Key: TGUID): TObject;
var
  iKey, i: Integer;
begin
  iKey := GetHashFromBuff(Key, 16);
  for i := 0 to FCount - 1 do
    if (FItems[i].Key = iKey) and TGUIDEx.IsEqual(FItems[i].KeyGUID, Key) then
    begin
      result := FItems[i].Value;
      exit;
    end;
  result := nil;
end;

function TObjectsDictionary.inList(const aItem: TObject): boolean;
var i: integer;
begin
  result:=true;
  for i := 0 to FCount - 1 do if FItems[i].Value = aItem then exit;
  result:=false;
end;

function TObjectsDictionary.GetValue(const Key: string): TObject;
var
  iKey, i: Integer;
begin
  iKey := StringHashKey(Key);
  for i := 0 to FCount - 1 do
    if (FItems[i].Key = iKey) and (FItems[i].KeyName = Key) then
    begin
      result := FItems[i].Value;
      exit;
    end;
  result := nil;
end;

{ TLinkedList }

procedure TLinkedObjectsList.AddKey(const Key, Value: TObject);
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    if (FItems[i].Key = Key) then
      exit;
  if FCount >= Length(FItems) then
    Setlength(FItems, FCount * 2);
  FItems[FCount].Key := Key;
  FItems[FCount].Value := Value;
  Inc(FCount);
end;

constructor TLinkedObjectsList.Create;
begin
  Setlength(FItems, 128);
  FCount := 0;
end;

function TLinkedObjectsList.GetValue(const Key: TObject): TObject;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    if (FItems[i].Key = Key) then
    begin
      result := FItems[i].Value;
      exit;
    end;
  result := nil;
end;

{ TRBTree }

constructor TRBTree.Create(Compare: TListSortCompare);
begin
  inherited Create;
  compareFunc := Compare;
  root := nil;
  leftmost := nil;
  rightmost := nil;
end;

destructor TRBTree.Destroy();
begin
  Clear();
  inherited Destroy;
end;

procedure fast_erase(x: PRBNode);
begin
  if (x^.left <> nil) then
    fast_erase(x^.left);
  if (x^.right <> nil) then
    fast_erase(x^.right);
  dispose(x);
end;

procedure TRBTree.Clear();
begin
  if (root <> nil) then
    fast_erase(root);
  root := nil;
  leftmost := nil;
  rightmost := nil;
end;

function TRBTree.Find(Key: Pointer): PRBNode;
var
  cmp: Integer;
begin
  result := root;
  while (result <> nil) do
  begin
    cmp := compareFunc(result^.k, Key);
    if cmp < 0 then
    begin
      result := result^.right;
    end
    else if cmp > 0 then
    begin
      result := result^.left;
    end
    else
      break;
  end;
end;

procedure TRBTree.RotateLeft(var x: PRBNode);
var
  y: PRBNode;
begin
  y := x^.right;
  x^.right := y^.left;
  if (y^.left <> nil) then
    y^.left^.parent := x;
  y^.parent := x^.parent;
  if (x = root) then
  begin
    root := y;
  end
  else if (x = x^.parent^.left) then
  begin
    x^.parent^.left := y;
  end
  else
  begin
    x^.parent^.right := y;
  end;
  y^.left := x;
  x^.parent := y;
end;

procedure TRBTree.RotateRight(var x: PRBNode);
var
  y: PRBNode;
begin
  y := x^.left;
  x^.left := y^.right;
  if (y^.right <> nil) then
    y^.right^.parent := x;
  y^.parent := x^.parent;
  if (x = root) then
  begin
    root := y;
  end
  else if (x = x^.parent^.right) then
  begin
    x^.parent^.right := y;
  end
  else
  begin
    x^.parent^.left := y;
  end;
  y^.right := x;
  x^.parent := y;
end;

function TRBTree.Minimum(var x: PRBNode): PRBNode;
begin
  result := x;
  while (result^.left <> nil) do
    result := result^.left;
end;

function TRBTree.Maximum(var x: PRBNode): PRBNode;
begin
  result := x;
  while (result^.right <> nil) do
    result := result^.right;
end;

function TRBTree.Add(Key: Pointer): PRBNode;
var
  x, y, z, zpp: PRBNode;
  cmp: Integer;
begin
  z := New(PRBNode);
  { Initialize fields in new node z }
  z^.k := Key;
  z^.left := nil;
  z^.right := nil;
  z^.color := ncRed;

  result := z;

  { Maintain leftmost and rightmost nodes }
  if ((leftmost = nil) or (compareFunc(Key, leftmost^.k) < 0)) then
    leftmost := z;

  if ((rightmost = nil) or (compareFunc(rightmost^.k, Key) < 0)) then
    rightmost := z;

  { Insert node z }
  y := nil;
  x := root;
  while (x <> nil) do
  begin
    y := x;
    cmp := compareFunc(Key, x^.k);
    if (cmp < 0) then
    begin
      x := x^.left;
    end
    else if (cmp > 0) then
    begin
      x := x^.right;
    end
    else
    begin
      { Value already exists in tree. }
      result := x;
      dispose(z);
      exit;
    end;
  end;
  z^.parent := y;
  if (y = nil) then
  begin
    root := z;
  end
  else if (compareFunc(Key, y^.k) < 0) then
  begin
    y^.left := z;
  end
  else
  begin
    y^.right := z;
  end;

  { Rebalance tree }
  while ((z <> root) and (z^.parent^.color = ncRed)) do
  begin
    zpp := z^.parent^.parent;
    if (z^.parent = zpp^.left) then
    begin
      y := zpp^.right;
      if ((y <> nil) and (y^.color = ncRed)) then
      begin
        z^.parent^.color := ncBlack;
        y^.color := ncBlack;
        zpp^.color := ncRed;
        z := zpp;
      end
      else
      begin
        if (z = z^.parent^.right) then
        begin
          z := z^.parent;
          RotateLeft(z);
        end;
        z^.parent^.color := ncBlack;
        zpp^.color := ncRed;
        RotateRight(zpp);
      end;
    end
    else
    begin
      y := zpp^.left;
      if ((y <> nil) and (y^.color = ncRed)) then
      begin
        z^.parent^.color := ncBlack;
        y^.color := ncBlack;
        zpp^.color := ncRed;
        z := zpp;
      end
      else
      begin
        if (z = z^.parent^.left) then
        begin
          z := z^.parent;
          RotateRight(z);
        end;
        z^.parent^.color := ncBlack;
        zpp^.color := ncRed;
        RotateLeft(zpp);
      end;
    end;
  end;
  root^.color := ncBlack;
end;

procedure TRBTree.Delete(z: PRBNode);
var
  w, x, y, x_parent: PRBNode;
  tmpcol: TRBNodeColor;
begin
  y := z;
  x := nil;
  x_parent := nil;

  if (y^.left = nil) then
  begin { z has at most one non-null child. y = z. }
    x := y^.right; { x might be null. }
  end
  else
  begin
    if (y^.right = nil) then
    begin { z has exactly one non-null child. y = z. }
      x := y^.left; { x is not null. }
    end
    else
    begin
      { z has two non-null children.  Set y to }
      y := y^.right; { z's successor.  x might be null. }
      while (y^.left <> nil) do
      begin
        y := y^.left;
      end;
      x := y^.right;
    end;
  end;

  if (y <> z) then
  begin
    { "copy y's sattelite data into z" }
    { relink y in place of z.  y is z's successor }
    z^.left^.parent := y;
    y^.left := z^.left;
    if (y <> z^.right) then
    begin
      x_parent := y^.parent;
      if (x <> nil) then
        x^.parent := y^.parent;
      y^.parent^.left := x; { y must be a child of left }
      y^.right := z^.right;
      z^.right^.parent := y;
    end
    else
      x_parent := y;
    if (root = z) then
      root := y
    else if (z^.parent^.left = z) then
    begin
      z^.parent^.left := y;
    end
    else
      z^.parent^.right := y;
    y^.parent := z^.parent;
    tmpcol := y^.color;
    y^.color := z^.color;
    z^.color := tmpcol;
    y := z;
    { y now points to node to be actually deleted }
  end
  else
  begin { y = z }
    x_parent := y^.parent;
    if (x <> nil) then
      x^.parent := y^.parent;
    if (root = z) then
      root := x
    else
    begin
      if (z^.parent^.left = z) then
        z^.parent^.left := x
      else
        z^.parent^.right := x;
    end;
    if (leftmost = z) then
    begin
      if (z^.right = nil) then
        leftmost := z^.parent { z^.left must be null also }
    end
    else
      leftmost := Minimum(x);
    if (rightmost = z) then
    begin
      if (z^.left = nil) then
        rightmost := z^.parent { z^.right must be null also }
      else
        rightmost := Maximum(x); { x == z^.left }
    end;
  end;

  { Rebalance tree }
  if (y^.color = ncBlack) then
  begin
    while ((x <> root) and ((x = nil) or (x^.color = ncBlack))) do
    begin
      if (x = x_parent^.left) then
      begin
        w := x_parent^.right;
        if (w^.color = ncRed) then
        begin
          w^.color := ncBlack;
          x_parent^.color := ncRed;
          RotateLeft(x_parent);
          w := x_parent^.right;
        end;
        if (((w^.left = nil) or (w^.left^.color = ncBlack)) and
          ((w^.right = nil) or (w^.right^.color = ncBlack))) then
        begin
          w^.color := ncRed;
          x := x_parent;
          x_parent := x_parent^.parent;
        end
        else
        begin
          if ((w^.right = nil) or (w^.right^.color = ncBlack)) then
          begin
            w^.left^.color := ncBlack;
            w^.color := ncRed;
            RotateRight(w);
            w := x_parent^.right;
          end;
          w^.color := x_parent^.color;
          x_parent^.color := ncBlack;
          if (w^.right <> nil) then
            w^.right^.color := ncBlack;
          RotateLeft(x_parent);
          x := root; { break; }
        end
      end
      else
      begin
        { same as above, with right <^. left. }
        w := x_parent^.left;
        if (w^.color = ncRed) then
        begin
          w^.color := ncBlack;
          x_parent^.color := ncRed;
          RotateRight(x_parent);
          w := x_parent^.left;
        end;
        if (((w^.right = nil) or (w^.right^.color = ncBlack)) and
          ((w^.left = nil) or (w^.left^.color = ncBlack))) then
        begin
          w^.color := ncRed;
          x := x_parent;
          x_parent := x_parent^.parent;
        end
        else
        begin
          if ((w^.left = nil) or (w^.left^.color = ncBlack)) then
          begin
            w^.right^.color := ncBlack;
            w^.color := ncRed;
            RotateLeft(w);
            w := x_parent^.left;
          end;
          w^.color := x_parent^.color;
          x_parent^.color := ncBlack;
          if (w^.left <> nil) then
            w^.left^.color := ncBlack;
          RotateRight(x_parent);
          x := root; { break; }
        end;
      end;
    end;
    if (x <> nil) then
      x^.color := ncBlack;
  end;
  dispose(y);
end;

{ Pre: x <> last }
class procedure TRBNode.RBInc(var x: PRBNode);
var
  y: PRBNode;
begin
  if (x^.right <> nil) then
  begin
    x := x^.right;
    while (x^.left <> nil) do
      x := x^.left;
  end
  else
  begin
    y := x^.parent;
    while (x = y^.right) do
    begin
      x := y;
      y := y^.parent;
    end;
    if (x^.right <> y) then
      x := y;
  end
end;

{ Pre: x <> first }
class procedure TRBNode.RBDec(var x: PRBNode);
var
  y: PRBNode;
begin
  if (x^.left <> nil) then
  begin
    y := x^.left;
    while (y^.right <> nil) do
      y := y^.right;
    x := y;
  end
  else
  begin
    y := x^.parent;
    while (x = y^.left) do
    begin
      x := y;
      y := y^.parent;
    end;
    x := y;
  end
end;

{ TVec3List }

function TVec3List.Cross(AList: TVec3List): TVec3List;
var
  i: Integer;
  V: TVector;
begin
  if Count = AList.Count then
  begin
    result := TVec3List.Create;
    result.Count := Count;
    for i := Count - 1 downto 0 do
    begin
      V.vec3 := FItems[i];
      result[i] := V.Cross(AList[i]).vec3;
    end;
  end
  else
    result := nil;
end;

function TVec3List.GetItemAsVector(AnIndex: Integer): TVector;
begin
  result.vec3 := Items[AnIndex];
end;

procedure TVec3List.Join(AList: TAbstractDataList; const AMatrix: TMatrix);
var
  i, Start: Integer;
  V: TVector;
begin
  Start := FCount;
  inherited;
  if not AMatrix.IsIdentity then
  begin
    for i := Start to FCount - 1 do
    begin
      V.vec3 := FItems[i];
      V[3] := 1;
      Items[i] := AMatrix.Transform(V).vec3;
    end;
  end;
end;

procedure TVec3List.SetItemAsVector(AnIndex: Integer; const aVector: TVector);
begin
  Assert(AnIndex < FCount);
  Items[AnIndex] := aVector.vec3;
end;

procedure TVec3List.SetNormalize;
var
  i: Integer;
  V: TVector;
begin
  for i := Count - 1 downto 0 do
  begin
    V.vec3 := FItems[i];
    Items[i] := V.Normalize.vec3;
  end;
end;

procedure TVec3List.Transform(const AMatrix: TMatrix);
var
  i: Integer;
  V: TVector;
begin
  for i := Count - 1 downto 0 do
  begin
    V.vec3 := FItems[i];
    Items[i] := AMatrix.Transform(V).vec3;
  end;

end;

{ TVec2List }

function TVec2List.GetItemAsVector(AnIndex: Integer): TVector;
begin
  result.vec2 := Items[AnIndex];
end;

procedure TVec2List.Join(AList: TAbstractDataList; const AMatrix: TMatrix);
var
  i, Start: Integer;
  V: TVector;
begin
  Start := FCount;
  inherited;
  if not AMatrix.IsIdentity then
  begin
    for i := Start to FCount - 1 do
    begin
      V.vec2 := FItems[i];
      Items[i] := AMatrix.Transform(V).vec2;
    end;
  end;
end;

procedure TVec2List.SetItemAsVector(AnIndex: Integer; const aVector: TVector);
begin
  Assert(AnIndex < FCount);
  Items[AnIndex] := aVector.vec2;
end;

procedure TVec2List.Transform(const AMatrix: TMatrix);
var
  i: Integer;
  V: TVector;
begin
  for i := Count - 1 downto 0 do
  begin
    V.vec2 := FItems[i];
    Items[i] := AMatrix.Transform(V).vec2;
  end;
end;

{ TDoubleList }

function TDoubleList.GetItemAsVector(AnIndex: Integer): TVector;
begin
  result.vec4 := VecNull;
  result[0] := Items[AnIndex];
end;

procedure TDoubleList.SetItemAsVector(AnIndex: Integer; const aVector: TVector);
begin
  Assert(AnIndex < FCount);
  Items[AnIndex] := aVector[0];
end;

procedure TDoubleList.Transform(const AMatrix: TMatrix);
begin
  Assert(false);
end;

{ TVec4List }

function TVec4List.GetItemAsVector(AnIndex: Integer): TVector;
begin
  result.vec4 := Items[AnIndex];
end;

procedure TVec4List.Join(AList: TAbstractDataList; const AMatrix: TMatrix);
var
  i, Start: Integer;
  V: TVector;
begin
  Start := FCount;
  inherited;
  if not AMatrix.IsIdentity then
  begin
    for i := Start to FCount - 1 do
    begin
      V.vec4 := FItems[i];
      Items[i] := AMatrix.Transform(V).vec4;
    end;
  end;
end;

procedure TVec4List.SetItemAsVector(AnIndex: Integer; const aVector: TVector);
begin
  Assert(AnIndex < FCount);
  Items[AnIndex] := aVector.vec4;
end;

procedure TVec4List.Transform(const AMatrix: TMatrix);
var
  i: Integer;
  V: TVector;
begin
  for i := Count - 1 downto 0 do
  begin
    V.vec4 := FItems[i];
    Items[i] := AMatrix.Transform(V).vec4;
  end;
end;

{ TIntegerList }

function TIntegerList.GetItemAsVector(AnIndex: Integer): TVector;
begin
  result.vec4 := VecNull;
  result[0] := Items[AnIndex];
end;

procedure TIntegerList.SetItemAsVector(AnIndex: Integer;
  const aVector: TVector);
begin
  Assert(AnIndex < FCount);
  Items[AnIndex] := Round(aVector[0]);
end;

procedure TIntegerList.Transform(const AMatrix: TMatrix);
begin
  Assert(false);
end;

{ TSingleList }

function TSingleList.GetItemAsVector(AnIndex: Integer): TVector;
begin
  result.vec4 := VecNull;
  result[0] := Items[AnIndex];
end;

procedure TSingleList.SetItemAsVector(AnIndex: Integer; const aVector: TVector);
begin
  Assert(AnIndex < FCount);
  Items[AnIndex] := aVector[0];
end;

procedure TSingleList.Transform(const AMatrix: TMatrix);
begin
  Assert(false);
end;

{ TAbstractDataList }

constructor TAbstractDataList.Create;
begin
end;

function TAbstractDataList.GetItemAsVector(AnIndex: Integer): TVector;
begin
  result.vec4 := VecNull;
end;

procedure TAbstractDataList.SetItemAsVector(AnIndex: Integer;
  const aVector: TVector);
begin
  Assert(false);
end;

procedure TAbstractDataList.Transform(const AMatrix: TMatrix);
begin
  Assert(false);
end;

{ TObjectList }

procedure TObjectList.FreeObjects;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if assigned(FItems[i]) then FItems[i].Free;
  Clear;
end;

end.
