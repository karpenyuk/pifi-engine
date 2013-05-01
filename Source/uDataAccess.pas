unit uDataAccess;

interface

{$POINTERMATH ON}

uses
  uBaseTypes, uVMath, uMiscUtils;

type

  TPtrIterator = class
  private
    FPointer: Pointer;
    FCurrent: Pointer;
  public
    constructor Create(aPointer: Pointer);
    function First: Pointer;
    function Next(aStride: Integer): Pointer;
    function Prev(aStride: Integer): Pointer;
    property Current: Pointer read FCurrent;
  end;

  TAbstractVectorData = class
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); virtual; abstract;
    class function GetValue(AnAddress: Pointer ): TVector; virtual; abstract;
  end;

  TAbstractVectorDataClass = Class of TAbstractVectorData;

{$REGION VectorDataClasses}
  TVectorTo1Byte = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo2Byte = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo3Byte = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo4Byte = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo1Word = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo2Word = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo3Word = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo4Word = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo1Int = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo2Int = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo3Int = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo4Int = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo1UInt = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo2UInt = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo3UInt = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo4UInt = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo1Float = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo2Float = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo3Float = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo4Float = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo1Double = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo2Double = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo3Double = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
  TVectorTo4Double = class(TAbstractVectorData)
    class procedure SetValue(AnAddress: Pointer; const Value: TVector); override;
    class function GetValue(AnAddress: Pointer ): TVector; override;
  end;
{$ENDREGION VectorDataClasses}

  IVectorDataAccess = interface
    function GetData: Pointer;
    function GetItem(Index: Integer): TVector;
    procedure SetItem(Index: Integer; const Value: TVector);
    function GetComponent: TValueComponent;
    function GetCount: Integer;
    function GetItemSize: Integer;
    function GetType: TValueType;
    function IsItemsEqual(Index1, Index2: Integer): Boolean;
    function GetItemAddress(anIndex: Integer): Pointer;
    property Data: Pointer read GetData;
    property Items[Index: Integer]: TVector read GetItem write SetItem; default;
    property Count: Integer read GetCount;
    property ItemSize: Integer read GetItemSize;
    property ItemType: TValueType read GetType;
    property ItemComponent: TValueComponent read GetComponent;
  end;


  TVectorDataAccess = class(TInterfacedObject, IVectorDataAccess)
  private
    FData: PByte;
    FCount: Integer;
    FType: TValueType;
    FComponent: TValueComponent;
    FItemSize: Integer;
    FStride: Integer;
    FConverter: TAbstractVectorDataClass;
    function GetData: Pointer;
    function GetItem(Index: Integer): TVector;
    procedure SetItem(Index: Integer; const Value: TVector);
    function GetComponent: TValueComponent;
    function GetCount: Integer;
    function GetItemSize: Integer;
    function GetType: TValueType;
  public
    constructor Create(aData: Pointer; aItemType: TValueType;
      aItemComponents: TValueComponent; anStride: Integer; aCount: Integer);
    function IsItemsEqual(Index1, Index2: Integer): Boolean;
    function GetItemAddress(anIndex: Integer): Pointer;
    property Data: Pointer read GetData;
    property Items[Index: Integer]: TVector read GetItem write SetItem; default;
    property Count: Integer read GetCount;
    property ItemSize: Integer read GetItemSize;
    property ItemType: TValueType read GetType;
    property ItemComponent: TValueComponent read GetComponent;
  end;

implementation

{ TIterator }

constructor TPtrIterator.Create(aPointer: Pointer);
begin
  FPointer := aPointer;
  FCurrent := FPointer;
end;

function TPtrIterator.First: Pointer;
begin
  result := FPointer;
end;

function TPtrIterator.Next(aStride: Integer): Pointer;
begin
  result := Pointer(Integer(FCurrent) + aStride);
end;

function TPtrIterator.Prev(aStride: Integer): Pointer;
begin
  result := Pointer(Integer(FCurrent) - aStride);
end;


{ TVectorDataAccess }

constructor TVectorDataAccess.Create(aData: Pointer; aItemType: TValueType;
  aItemComponents: TValueComponent; anStride: Integer; aCount: Integer);
begin
  FData := aData;
  FType := aItemType;
  FComponent := aItemComponents;
  FItemSize := CValueSizes[aItemType]*Ord(aItemComponents);
  FStride := anStride;
  Assert(FStride >= FItemSize);
  FCount := aCount;
  Assert(FCount > 0);

  case aItemType of
    vtByte:
      case aItemComponents of
        1: FConverter := TVectorTo1Byte;
        2: FConverter := TVectorTo2Byte;
        3: FConverter := TVectorTo3Byte;
        4: FConverter := TVectorTo4Byte;
      end;
    vtWord:
      case aItemComponents of
        1: FConverter := TVectorTo1Word;
        2: FConverter := TVectorTo2Word;
        3: FConverter := TVectorTo3Word;
        4: FConverter := TVectorTo4Word;
      end;
    vtInt:
      case aItemComponents of
        1: FConverter := TVectorTo1Int;
        2: FConverter := TVectorTo2Int;
        3: FConverter := TVectorTo3Int;
        4: FConverter := TVectorTo4Int;
      end;
    vtUint:
      case aItemComponents of
        1: FConverter := TVectorTo1UInt;
        2: FConverter := TVectorTo2UInt;
        3: FConverter := TVectorTo3UInt;
        4: FConverter := TVectorTo4UInt;
      end;
    vtFloat:
      case aItemComponents of
        1: FConverter := TVectorTo1Float;
        2: FConverter := TVectorTo2Float;
        3: FConverter := TVectorTo3Float;
        4: FConverter := TVectorTo4Float;
      end;
    vtDouble:
      case aItemComponents of
        1: FConverter := TVectorTo1Double;
        2: FConverter := TVectorTo2Double;
        3: FConverter := TVectorTo3Double;
        4: FConverter := TVectorTo4Double;
      end;
  end;
end;

function TVectorDataAccess.GetComponent: TValueComponent;
begin
  Result := FComponent;
end;

function TVectorDataAccess.GetCount: Integer;
begin
  Result := FCount;
end;

function TVectorDataAccess.GetData: Pointer;
begin
  Result := FData;
end;

function TVectorDataAccess.GetItem(Index: Integer): TVector;
var
  P: PByte;
begin
  Assert((Index > -1) and (Index < FCount));
  P := FData;
  Inc(P, Index*FStride);
  Result := FConverter.GetValue(P);
end;

function TVectorDataAccess.GetItemAddress(anIndex: Integer): Pointer;
var
  P: PByte;
begin
  Assert((anIndex > -1) and (anIndex < FCount));
  P := FData;
  Inc(P, anIndex*FStride);
  Result := P;
end;

function TVectorDataAccess.GetItemSize: Integer;
begin
  Result := FItemSize;
end;

function TVectorDataAccess.GetType: TValueType;
begin
  Result := FType;
end;

function TVectorDataAccess.IsItemsEqual(Index1, Index2: Integer): Boolean;
begin
  Assert(Index1 < FCount);
  Assert(Index2 < FCount);
  result := CompareMem(
    @FData[Index1*FStride], @FData[Index2*FStride], FItemSize);
end;

procedure TVectorDataAccess.SetItem(Index: Integer; const Value: TVector);
var
  P: PByte;
begin
  Assert((Index > -1) and (Index < FCount));
  P := FData;
  Inc(P, Index*FStride);
  FConverter.SetValue(P, Value);
end;

{ TVectorTo1Byte }

class function TVectorTo1Byte.GetValue(AnAddress: Pointer): TVector;
var
  P: PByte absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := 0;
  Result[2] := 0;
  Result[3] := 0;
end;

class procedure TVectorTo1Byte.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PByte absolute AnAddress;
begin
  P[0] := Trunc(255*Value[0]);
end;

{ TVectorTo2Byte }

class function TVectorTo2Byte.GetValue(AnAddress: Pointer): TVector;
var
  P: PByte absolute AnAddress;
begin
  Result[0] := P[0]/255;
  Result[1] := P[1]/255;
  Result[2] := 0;
  Result[3] := 0;
end;

class procedure TVectorTo2Byte.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PByte absolute AnAddress;
begin
  P[0] := Trunc(255*Value[0]);
  P[1] := Trunc(255*Value[1]);
end;

{ TVectorTo3Byte }

class function TVectorTo3Byte.GetValue(AnAddress: Pointer): TVector;
var
  P: PByte absolute AnAddress;
begin
  Result[0] := P[0]/255;
  Result[1] := P[1]/255;
  Result[2] := P[2]/255;
  Result[3] := 0;
end;

class procedure TVectorTo3Byte.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PByte absolute AnAddress;
begin
  P[0] := Trunc(255*Value[0]);
  P[1] := Trunc(255*Value[1]);
  P[2] := Trunc(255*Value[2]);
end;

{ TVectorTo4Byte }

class function TVectorTo4Byte.GetValue(AnAddress: Pointer): TVector;
var
  P: PByte absolute AnAddress;
begin
  Result[0] := P[0]/255;
  Result[1] := P[1]/255;
  Result[2] := P[2]/255;
  Result[3] := P[3]/255;
end;

class procedure TVectorTo4Byte.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PByte absolute AnAddress;
begin
  P[0] := Trunc(255*Value[0]);
  P[1] := Trunc(255*Value[1]);
  P[2] := Trunc(255*Value[2]);
  P[3] := Trunc(255*Value[3]);
end;

{ TVectorTo1Byte }

class function TVectorTo1Word.GetValue(AnAddress: Pointer): TVector;
var
  P: PWord absolute AnAddress;
begin
  Result[0] := Trunc(P[0]);
  Result[1] := 0;
  Result[2] := 0;
  Result[3] := 0;
end;

class procedure TVectorTo1Word.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PWord absolute AnAddress;
begin
  P[0] := Trunc(Value[0]);
end;

{ TVectorTo2Word }

class function TVectorTo2Word.GetValue(AnAddress: Pointer): TVector;
var
  P: PWord absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := P[1];
  Result[2] := 0;
  Result[3] := 0;
end;

class procedure TVectorTo2Word.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PWord absolute AnAddress;
begin
  P[0] := Trunc(Value[0]);
  P[1] := Trunc(Value[1]);
end;

{ TVectorTo3Word }

class function TVectorTo3Word.GetValue(AnAddress: Pointer): TVector;
var
  P: PWord absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := P[1];
  Result[2] := P[2];
  Result[3] := 0;
end;

class procedure TVectorTo3Word.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PWord absolute AnAddress;
begin
  P[0] := Trunc(Value[0]);
  P[1] := Trunc(Value[1]);
  P[2] := Trunc(Value[2]);
end;

{ TVectorTo4Word }

class function TVectorTo4Word.GetValue(AnAddress: Pointer): TVector;
var
  P: PWord absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := P[1];
  Result[2] := P[2];
  Result[3] := P[3];
end;

class procedure TVectorTo4Word.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PWord absolute AnAddress;
begin
  P[0] := Trunc(Value[0]);
  P[1] := Trunc(Value[1]);
  P[2] := Trunc(Value[2]);
  P[3] := Trunc(Value[3]);
end;

{ TVectorTo1Int }

class function TVectorTo1Int.GetValue(AnAddress: Pointer): TVector;
var
  P: PInteger absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := 0;
  Result[2] := 0;
  Result[3] := 0;
end;

class procedure TVectorTo1Int.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PInteger absolute AnAddress;
begin
  P[0] := Trunc(Value[0]);
end;

{ TVectorTo2Int }

class function TVectorTo2Int.GetValue(AnAddress: Pointer): TVector;
var
  P: PInteger absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := P[1];
  Result[2] := 0;
  Result[3] := 0;
end;

class procedure TVectorTo2Int.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PInteger absolute AnAddress;
begin
  P[0] := Trunc(Value[0]);
  P[1] := Trunc(Value[1]);
end;

{ TVectorTo3Int }

class function TVectorTo3Int.GetValue(AnAddress: Pointer): TVector;
var
  P: PInteger absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := P[1];
  Result[2] := P[2];
  Result[3] := 0;
end;

class procedure TVectorTo3Int.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PInteger absolute AnAddress;
begin
  P[0] := Trunc(Value[0]);
  P[1] := Trunc(Value[1]);
  P[2] := Trunc(Value[2]);
end;

{ TVectorTo4Int }

class function TVectorTo4Int.GetValue(AnAddress: Pointer): TVector;
var
  P: PInteger absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := P[1];
  Result[2] := P[2];
  Result[3] := P[3];
end;

class procedure TVectorTo4Int.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PInteger absolute AnAddress;
begin
  P[0] := Trunc(Value[0]);
  P[1] := Trunc(Value[1]);
  P[2] := Trunc(Value[2]);
  P[3] := Trunc(Value[3]);
end;

{ TVectorTo1UInt }

class function TVectorTo1UInt.GetValue(AnAddress: Pointer): TVector;
var
  P: PCardinal absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := 0;
  Result[2] := 0;
  Result[3] := 0;
end;

class procedure TVectorTo1UInt.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PCardinal absolute AnAddress;
begin
  P[0] := Trunc(Value[0]);
end;

{ TVectorTo2UInt }

class function TVectorTo2UInt.GetValue(AnAddress: Pointer): TVector;
var
  P: PCardinal absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := P[1];
  Result[2] := 0;
  Result[3] := 0;
end;

class procedure TVectorTo2UInt.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PCardinal absolute AnAddress;
begin
  P[0] := Trunc(Value[0]);
  P[1] := Trunc(Value[1]);
end;

{ TVectorTo3UInt }

class function TVectorTo3UInt.GetValue(AnAddress: Pointer): TVector;
var
  P: PCardinal absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := P[1];
  Result[2] := P[2];
  Result[3] := 0;
end;

class procedure TVectorTo3UInt.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PCardinal absolute AnAddress;
begin
  P[0] := Trunc(Value[0]);
  P[1] := Trunc(Value[1]);
  P[2] := Trunc(Value[2]);
end;

{ TVectorTo4UInt }

class function TVectorTo4UInt.GetValue(AnAddress: Pointer): TVector;
var
  P: PCardinal absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := P[1];
  Result[2] := P[2];
  Result[3] := P[3];
end;

class procedure TVectorTo4UInt.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PCardinal absolute AnAddress;
begin
  P[0] := Trunc(Value[0]);
  P[1] := Trunc(Value[1]);
  P[2] := Trunc(Value[2]);
  P[3] := Trunc(Value[3]);
end;

{ TVectorTo1Float }

class function TVectorTo1Float.GetValue(AnAddress: Pointer): TVector;
var
  P: PSingle absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := 0;
  Result[2] := 0;
  Result[3] := 0;
end;

class procedure TVectorTo1Float.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PSingle absolute AnAddress;
begin
  P[0] := Single(Value[0]);
end;

{ TVectorTo2Float }

class function TVectorTo2Float.GetValue(AnAddress: Pointer): TVector;
var
  P: PSingle absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := P[1];
  Result[2] := 0;
  Result[3] := 0;
end;

class procedure TVectorTo2Float.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PSingle absolute AnAddress;
begin
  P[0] := Single(Value[0]);
  P[1] := Single(Value[1]);
end;

{ TVectorTo3Float }

class function TVectorTo3Float.GetValue(AnAddress: Pointer): TVector;
var
  P: PSingle absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := P[1];
  Result[2] := P[2];
  Result[3] := 0;
end;

class procedure TVectorTo3Float.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PSingle absolute AnAddress;
begin
  P[0] := Single(Value[0]);
  P[1] := Single(Value[1]);
  P[2] := Single(Value[2]);
end;

{ TVectorTo4Float }

class function TVectorTo4Float.GetValue(AnAddress: Pointer): TVector;
var
  P: PSingle absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := P[1];
  Result[2] := P[2];
  Result[3] := P[3];
end;

class procedure TVectorTo4Float.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PSingle absolute AnAddress;
begin
  P[0] := Single(Value[0]);
  P[1] := Single(Value[1]);
  P[2] := Single(Value[2]);
  P[3] := Single(Value[3]);
end;

{ TVectorTo1Double }

class function TVectorTo1Double.GetValue(AnAddress: Pointer): TVector;
var
  P: PDouble absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := 0;
  Result[2] := 0;
  Result[3] := 0;
end;

class procedure TVectorTo1Double.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PDouble absolute AnAddress;
begin
  P[0] := Value[0];
end;

{ TVectorTo2Double }

class function TVectorTo2Double.GetValue(AnAddress: Pointer): TVector;
var
  P: PDouble absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := P[1];
  Result[2] := 0;
  Result[3] := 0;
end;

class procedure TVectorTo2Double.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PDouble absolute AnAddress;
begin
  P[0] := Value[0];
  P[1] := Value[1];
end;

{ TVectorTo3Double }

class function TVectorTo3Double.GetValue(AnAddress: Pointer): TVector;
var
  P: PDouble absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := P[1];
  Result[2] := P[2];
  Result[3] := 0;
end;

class procedure TVectorTo3Double.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PDouble absolute AnAddress;
begin
  P[0] := Value[0];
  P[1] := Value[1];
  P[2] := Value[2];
end;

{ TVectorTo4Double }

class function TVectorTo4Double.GetValue(AnAddress: Pointer): TVector;
var
  P: PDouble absolute AnAddress;
begin
  Result[0] := P[0];
  Result[1] := P[1];
  Result[2] := P[2];
  Result[3] := P[3];
end;

class procedure TVectorTo4Double.SetValue(AnAddress: Pointer;
  const Value: TVector);
var
  P: PDouble absolute AnAddress;
begin
  P[0] := Value[0];
  P[1] := Value[1];
  P[2] := Value[2];
  P[3] := Value[3];
end;

end.
