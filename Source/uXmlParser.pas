// Based on Timur "XProger" Gagiev (2010) code.

unit uXmlParser;

interface

uses
  Classes, uGenericsRBTree;

type

  TXMLVariant = record
    Value: WideString;
    class operator Implicit(const a: WideString ): TXMLVariant;
    class operator Implicit(const a: Byte ): TXMLVariant;
    class operator Implicit(const a: SmallInt ): TXMLVariant;
    class operator Implicit(const a: LongWord ): TXMLVariant;
    class operator Implicit(const a: Integer ): TXMLVariant;
    class operator Implicit(const a: Int64 ): TXMLVariant;
    class operator Implicit(const a: Single ): TXMLVariant;
    class operator Implicit(const a: Double ): TXMLVariant;
    class operator Implicit(const a: Boolean ): TXMLVariant;

    class operator Implicit(const a: TXMLVariant ): WideString;
    class operator Implicit(const a: TXMLVariant ): Byte;
    class operator Implicit(const a: TXMLVariant ): SmallInt;
    class operator Implicit(const a: TXMLVariant ): LongWord;
    class operator Implicit(const a: TXMLVariant ): Integer;
    class operator Implicit(const a: TXMLVariant ): Int64;
    class operator Implicit(const a: TXMLVariant ): Single;
    class operator Implicit(const a: TXMLVariant ): Double;
    class operator Implicit(const a: TXMLVariant ): Boolean;

    class operator Explicit(const a: WideString ): TXMLVariant;
    class operator Explicit(const a: Byte ): TXMLVariant;
    class operator Explicit(const a: SmallInt ): TXMLVariant;
    class operator Explicit(const a: LongWord ): TXMLVariant;
    class operator Explicit(const a: Integer ): TXMLVariant;
    class operator Explicit(const a: Int64 ): TXMLVariant;
    class operator Explicit(const a: Single ): TXMLVariant;
    class operator Explicit(const a: Double ): TXMLVariant;
    class operator Explicit(const a: Boolean ): TXMLVariant;

    class operator Explicit(const a: TXMLVariant ): WideString;
    class operator Explicit(const a: TXMLVariant ): Byte;
    class operator Explicit(const a: TXMLVariant ): SmallInt;
    class operator Explicit(const a: TXMLVariant ): LongWord;
    class operator Explicit(const a: TXMLVariant ): Integer;
    class operator Explicit(const a: TXMLVariant ): Int64;
    class operator Explicit(const a: TXMLVariant ): Single;
    class operator Explicit(const a: TXMLVariant ): Double;
    class operator Explicit(const a: TXMLVariant ): Boolean;
  end;

  TXMLParam = record
    Name: WideString;
    Value: TXMLVariant;
  end;

  TXMLParams = class
    constructor Create(const Text: WideString);
  private
    FCount: LongInt;
    FParams: array of TXMLParam;
    function GetParam(const Name: WideString): TXMLVariant;
    function GetParamI(Idx: LongInt): TXMLParam;
    procedure SetParam(const Name: WideString; const Value: TXMLVariant);
  public
    property Count: LongInt read FCount;
    property Param[const Name: WideString]: TXMLVariant read GetParam
      write SetParam; default;
    property ParamI[Idx: LongInt]: TXMLParam read GetParamI;
  end;

  IXML = interface
    ['{B5D59B72-CC8D-4126-AEB3-A7897DDF492C}']
    procedure DoBeforeCreate;
    function GetCount: LongInt;
    procedure SetTag(const aValue: WideString);
    function GetTag: WideString;
    function GetContent: TXMLVariant;
    function GetDataLen: LongInt;
    function GetParams: TXMLParams;
    function GetNode(const TagName: WideString): IXML;
    function GetNodeI(Idx: LongInt): IXML;
    procedure SetContent(const aValue: TXMLVariant);

    property Count: LongInt read GetCount;
    property Tag: WideString read GetTag write SetTag;
    property Content: TXMLVariant read GetContent write SetContent;
    property DataLen: LongInt read GetDataLen;
    property Params: TXMLParams read GetParams;
    property Nodes[const TagName: WideString]: IXML read GetNode; default;
    property NodesI[Idx: LongInt]: IXML read GetNodeI;
  end;

  TXML = class;
  TXMLClass = class of TXML;

  TXMLClassesTree = class(GRedBlackTree<AnsiString, TXMLClass>)
  public
    constructor CreateClassesTree;
  end;

  TXMLCollection = class;
  TXMLCollectionClass = class of TXMLCollection;

  IXMLList = interface
    function GetCount: Integer;
    function GetNode(const Name: WideString): IXML;
    function Add(const Node: IXML): Integer;
    procedure Clear;
    function Delete(const Index: Integer): Integer; overload;
    function Delete(const Name: WideString): Integer; overload;
    function First: IXML;
    function FindNode(NodeName: WideString): IXML; overload;
    function Get(Index: Integer): IXML;
    function IndexOf(const Node: IXML): Integer; overload;
    function IndexOf(const Name: WideString): Integer; overload;
    procedure Insert(Index: Integer; const Node: IXML);
    function Last: IXML;
    function Remove(const Node: IXML): Integer;
    function ReplaceNode(const OldNode, NewNode: IXML): IXML;
    property Count: Integer read GetCount;
    property Nodes[const Name: WideString]: IXML read GetNode; default;
    property NodesI[Index: Integer]: IXML read Get;
  end;

  TXML = class(TInterfacedObject, IXML)
  protected
    class var ClassesTree: TXMLClassesTree;
    class function FindClass(const aClassName: AnsiString): TXMLClass;
  private
    FNodes: IXMLList;
    FTag: WideString;
    FContent: TXMLVariant;
    FDataLen: LongInt;
    FParams: TXMLParams;
    function GetNode(const TagName: WideString): IXML;
    function GetNodeI(Idx: LongInt): IXML;
    function GetContent: TXMLVariant;
    function GetCount: LongInt;
    function GetDataLen: LongInt;
    function GetParams: TXMLParams;
    procedure SetTag(const aValue: WideString);
    function GetTag: WideString;
    procedure SetContent(const aValue: TXMLVariant);
    procedure SetNodes(const aList: IXMLList);
  public
    procedure DoBeforeCreate; virtual;
    constructor Create(const Text: WideString; BeginPos: LongInt);
    destructor Destroy; override;
    class function Load(const FileName: string): IXML;
    procedure RegisterChildNode(const TagName: WideString;
      ChildNodeClass: TXMLClass);
    function CreateCollection(const CollectionClass: TXMLCollectionClass;
      const anItemIterface: TGuid; const ItemTag: WideString): TXMLCollection;
    property Count: LongInt read GetCount;
    property Tag: WideString read GetTag write SetTag;
    property Content: TXMLVariant read GetContent write SetContent;
    property DataLen: LongInt read GetDataLen;
    property Params: TXMLParams read GetParams;
    property Nodes[const TagName: WideString]: IXML read GetNode; default;
    property NodesI[Idx: LongInt]: IXML read GetNodeI;
  end;

  TXMLList = class(TInterfacedObject, IXMLList)
  private
    FList: IInterfaceList;
    function GetCount: Integer;
    function GetNode(const Name: WideString): IXML;
  protected
    function Add(const Node: IXML): Integer;
    procedure Clear;
    function Delete(const Index: Integer): Integer; overload;
    function Delete(const Name: WideString): Integer; overload;
    function First: IXML;
    function FindNode(NodeName: WideString): IXML; overload;
    function Get(Index: Integer): IXML;
    function IndexOf(const Node: IXML): Integer; overload;
    function IndexOf(const Name: WideString): Integer; overload;
    procedure Insert(Index: Integer; const Node: IXML);
    function Last: IXML;
    function Remove(const Node: IXML): Integer;
    function ReplaceNode(const OldNode, NewNode: IXML): IXML;
    property Count: Integer read GetCount;
    property Nodes[const Name: WideString]: IXML read GetNode; default;
    property NodesI[Index: Integer]: IXML read Get;
    property List: IInterfaceList read FList;
  public
    constructor Create;
  end;

  IXMLCollection = interface(IXML)
    function GetCount: Integer;
    function GetNode(Index: Integer): IXML;
//    procedure Clear;
//    procedure Delete(Index: Integer);
    function Remove(const Node: IXML): Integer;
    property Count: Integer read GetCount;
    property Nodes[Index: Integer]: IXML read GetNode; default;
  end;

  TXMLCollection = class(TXML, IXMLCollection)
  private
    FList: IXMLList;
    FItemInterface: TGuid;
    FTag: WideString;
  protected
    function GetCount: Integer;
    function GetNode(Index: Integer): IXML;
    function GetList: IXMLList; virtual;
    function Remove(const Node: IXML): Integer;
    procedure SetNodes(const aList: IXMLList);
    property Count: Integer read GetCount;
    property Nodes[Index: Integer]: IXML read GetNode; default;
    property ItemInterface: TGuid read FItemInterface write FItemInterface;
    property Tag: WideString read FTag write FTag;
    function AddItem(Index: Integer): IXML; virtual;
    property List: IXMLList read GetList;
  end;

implementation

uses
  uMiscUtils;

{$REGION 'TXMLParam'}

constructor TXMLParams.Create(const Text: WideString);
var
  i: LongInt;
  Flag: (F_BEGIN, F_NAME, F_VALUE);
  ParamIdx: LongInt;
  IndexBegin: LongInt;
  ReadValue: Boolean;
  TextFlag: Boolean;
begin
  Flag := F_BEGIN;
  ParamIdx := -1;
  IndexBegin := 1;
  ReadValue := False;
  TextFlag := False;
  for i := 1 to Length(Text) do
    case Flag of
      F_BEGIN:
        if Text[i] <> ' ' then
        begin
          ParamIdx := Length(FParams);
          SetLength(FParams, ParamIdx + 1);
          FParams[ParamIdx].Name := '';
          FParams[ParamIdx].Value := '';
          Flag := F_NAME;
          IndexBegin := i;
        end;
      F_NAME:
        if Text[i] = '=' then
        begin
          FParams[ParamIdx].Name :=
            Trim(Copy(Text, IndexBegin, i - IndexBegin));
          Flag := F_VALUE;
          IndexBegin := i + 1;
        end;
      F_VALUE:
        begin
          if Text[i] = '"' then
              TextFlag := not TextFlag;
          if (Text[i] <> ' ') and (not TextFlag) then
              ReadValue := True
          else
            if ReadValue then
          begin
            FParams[ParamIdx].Value :=
              Trim(Copy(Text, IndexBegin, i - IndexBegin));
            Flag := F_BEGIN;
            ReadValue := False;
            ParamIdx := -1;
          end
          else
              continue;
        end;
    end;
  if ParamIdx <> -1 then
      FParams[ParamIdx].Value :=
      Trim(Copy(Text, IndexBegin, Length(Text) - IndexBegin + 1));
  FCount := Length(FParams);
end;

function TXMLParams.GetParam(const Name: WideString): TXMLVariant;
var
  i: LongInt;
begin
  for i := 0 to Count - 1 do
    if FParams[i].Name = Name then
        Exit(FParams[i].Value);
  Result := '';
end;

function TXMLParams.GetParamI(Idx: LongInt): TXMLParam;
begin
  Result.Name := FParams[Idx].Name;
  Result.Value := FParams[Idx].Value;
end;

procedure TXMLParams.SetParam(const Name: WideString; const Value: TXMLVariant);
var
  i: LongInt;
begin
  for i := 0 to Count - 1 do
    if FParams[i].Name = Name then
    begin
      FParams[i].Value := Value;
      Exit;
    end;
  i := Length(FParams);
  SetLength(FParams, i + 1);
  FParams[i].Name := Name;
  FParams[i].Value := Value;
end;

{$ENDREGION}

{$REGION 'TXML'}

class function TXML.Load(const FileName: string): IXML;
var
  Stream: TStream;
  Text: WideString;
  Size: LongInt;
  UTF8Text: UTF8String;
begin
  Stream := TFileStream.Create(FileName, $20);
  if Stream <> nil then
  begin
    Size := Stream.Size;
    SetLength(UTF8Text, Size);
    Stream.Read(UTF8Text[1], Size);
    Text := UTF8Decode(UTF8Text);
    // UTF8ToWideString(UTF8Text);//UTF8ToString(UTF8Text);
    Result := Create(Text, 1);
    Stream.Free;
  end
  else
    Result := nil;
end;

procedure TXML.RegisterChildNode(const TagName: WideString;
  ChildNodeClass: TXMLClass);
begin

end;

procedure TXML.SetContent(const aValue: TXMLVariant);
begin
  FContent := aValue;
end;

procedure TXML.SetNodes(const aList: IXMLList);
begin
  FNodes := aList;
end;

procedure TXML.SetTag(const aValue: WideString);
begin
  FTag := aValue;
end;

constructor TXML.Create(const Text: WideString; BeginPos: LongInt);
var
  i, j, k: LongInt;
  Flag: (F_BEGIN, F_COMMENT, F_TAG, F_PARAMS, F_CONTENT, F_END);
  BeginIndex: LongInt;
  TextFlag: Boolean;
  Len: LongInt;
  line: WideString;
  XMLClass: TXMLClass;
begin
  FNodes := TXMLList.Create;
  DoBeforeCreate;
  TextFlag := False;
  Flag := F_BEGIN;
  BeginIndex := BeginPos;
  FContent := '';
  Len := Length(Text);
  i := BeginPos - 1;
  while i < Len do
  begin
    Inc(i);
    case Flag of
      // waiting for new tag '<...'
      F_BEGIN:
        case Text[i] of
          '<':
            begin
              Flag := F_TAG;
              BeginIndex := i + 1;
            end;
          '>':
            begin
              Flag := F_END;
              Dec(i);
            end;
        end;
      // comments
      F_COMMENT:
        case Text[i] of
          '"':
            TextFlag := not TextFlag;
          '>':
            if not TextFlag then
                Flag := F_BEGIN;
        end;
      // waiting for tag name '... ' or '.../' or '...>'
      F_TAG:
        begin
          case Text[i] of
            '>': Flag := F_CONTENT;
            '/': Flag := F_END;
            ' ': Flag := F_PARAMS;
            '?', '!':
              begin
                Flag := F_COMMENT;
                continue;
              end
          else
            continue;
          end;
          FTag := Trim(Copy(Text, BeginIndex, i - BeginIndex));
          BeginIndex := i + 1;
        end;
      // parse tag parameters
      F_PARAMS:
        begin
          if Text[i] = '"' then
              TextFlag := not TextFlag;
          if not TextFlag then
          begin
            case Text[i] of
              '>': Flag := F_CONTENT;
              '/': Flag := F_END;
            else
              continue;
            end;
            FParams := TXMLParams.Create
              (Trim(Copy(Text, BeginIndex, i - BeginIndex)));
            BeginIndex := i + 1;
          end;
        end;
      // parse tag content
      F_CONTENT:
        case Text[i] of
          '<':
            begin
              FContent := Trim(Copy(Text, BeginIndex, i - BeginIndex));
              // is new tag or tag closing?
              for j := i + 1 to Length(Text) do
                if Text[j] = '>' then
                begin
                  line := Trim(Copy(Text, i + 1, j - i - 1));
                  if line <> '/' + FTag then
                  begin
                    for k := 1 to Length(line) do
                      if (line[k] = ' ') or  (line[k] = '/') then
                      begin
                        line := Copy(line, 1, k-1);
                      end;
                    XMLClass := FindClass(AnsiString(line));
                    WriteLn(FTag, ' - ', line, ': ', XMLClass.ClassName);
                    FNodes.Add(XMLClass.Create(Text, i));
                    if FNodes.Last.DataLen = 0 then
                        break;
                    i := i + FNodes.Last.DataLen - 1;
                    BeginIndex := i + 1;
                  end else
                  begin
                    i := j - 1;
                    Flag := F_END;
                  end;
                  break;
                end;
            end
        end;
      // waiting for close tag
      F_END:
        if Text[i] = '>' then
        begin
          FDataLen := i - BeginPos + 1;
          break;
        end;
    end;
  end;
  if FParams = nil then
      FParams := TXMLParams.Create('');
end;

function TXML.CreateCollection(const CollectionClass: TXMLCollectionClass;
  const anItemIterface: TGuid; const ItemTag: WideString): TXMLCollection;
begin
  Result := CollectionClass.Create('', 1);
  Result.ItemInterface := anItemIterface;
  Result.Tag := ItemTag;
  if Assigned(FNodes) then
    Result.SetNodes(FNodes);
end;

destructor TXML.Destroy;
begin
  FNodes.Clear;
  Params.Free;
end;

procedure TXML.DoBeforeCreate;
begin
end;

class function TXML.FindClass(const aClassName: AnsiString): TXMLClass;
begin
  if Assigned(ClassesTree) then
    if ClassesTree.Find(aClassName, Result) then
      exit;
  Result := TXML;
end;

function TXML.GetContent: TXMLVariant;
begin
  Result := FContent;
end;

function TXML.GetCount: LongInt;
begin
  Result := FNodes.Count;
end;

function TXML.GetDataLen: LongInt;
begin
  Result := FDataLen;
end;

function TXML.GetNode(const TagName: WideString): IXML;
var
  i: LongInt;
begin
  for i := 0 to Count - 1 do
    if FNodes.NodesI[i].Tag = TagName then
        Exit(FNodes.NodesI[i]);
  Result := nil;
end;

function TXML.GetNodeI(Idx: LongInt): IXML;
begin
  Result := FNodes.NodesI[Idx];
end;

function TXML.GetParams: TXMLParams;
begin
  Result := FParams;
end;

function TXML.GetTag: WideString;
begin
  Result := FTag;
end;

{$ENDREGION}

{$REGION 'TXMLList'}

function TXMLList.Add(const Node: IXML): Integer;
begin
  Insert(-1, Node);
  Result := Count - 1;
end;

procedure TXMLList.Clear;
begin
  List.Lock;
  try
    while Count > 0 do
      Remove(Get(0));
  finally
    List.Unlock;
  end;
end;

constructor TXMLList.Create;
begin
  FList := TInterfaceList.Create;
end;

function TXMLList.Delete(const Name: WideString): Integer;
var
  Node: IXML;
begin
  Node := FindNode(Name);
  if Assigned(Node) then
    Result := Remove(Node)
  else
   { No error when named nodes doesn't exist }
    Result := -1;
end;

function TXMLList.Delete(const Index: Integer): Integer;
begin
  Result := Remove(Get(Index));
end;

function TXMLList.FindNode(NodeName: WideString): IXML;
var
  Index: Integer;
begin
  Index := IndexOf(NodeName);
  if Index >= 0 then
    Result := Get(Index)
  else
    Result := nil;
end;

function TXMLList.First: IXML;
begin
  if List.Count > 0 then
    Result := List.First as IXML
  else Result := nil;
end;

function TXMLList.Get(Index: Integer): IXML;
begin
  Result := FList.Get(Index) as IXML;
end;

function TXMLList.GetCount: Integer;
begin
  Result := List.Count;
end;

function TXMLList.GetNode(const Name: WideString): IXML;
begin
  Result := FindNode(Name);
end;

function TXMLList.IndexOf(const Name: WideString): Integer;
begin
  for Result := 0 to Count - 1 do
    if Get(Result).Tag = Name then Exit;
  Result := -1;
end;

function TXMLList.IndexOf(const Node: IXML): Integer;
begin
  Result := List.IndexOf(Node as IXML);
end;

procedure TXMLList.Insert(Index: Integer; const Node: IXML);
begin
  if Index <> -1 then
     List.Insert(Index, Node)
  else
    List.Add(Node);
end;

function TXMLList.Last: IXML;
begin
  if List.Count > 0 then
    Result := List.Last as IXML else
    Result := nil;
end;

function TXMLList.Remove(const Node: IXML): Integer;
begin
  Result := List.Remove(Node as IXML);
end;

function TXMLList.ReplaceNode(const OldNode, NewNode: IXML): IXML;
var
  Index: Integer;
begin
  Index := Remove(OldNode);
  Insert(Index, NewNode);
  Result := OldNode;
end;

{$ENDREGION}


{$REGION 'TXMLCollection'}

function TXMLCollection.AddItem(Index: Integer): IXML;
var
  NewXML: TXML;
begin
  NewXML := TXML.Create('', 1);
  Result := NewXML;
  if Index > -1 then

end;

function TXMLCollection.GetCount: Integer;
var
  I: Integer;
  Obj: Pointer;
begin
  Result := 0;
  for I := 0 to List.Count - 1 do
    if List.NodesI[I].QueryInterface(FItemInterface, Obj) = S_OK then
      Inc(Result);
end;

function TXMLCollection.GetList: IXMLList;
begin
  Result := FList;
end;

function TXMLCollection.GetNode(Index: Integer): IXML;
var
  I, J: Integer;
  X: IXML;
begin
  Result := nil;
  J := 0;
  for I := 0 to List.Count - 1 do
  begin
    if List.NodesI[I].QueryInterface(FItemInterface, X) = S_OK then
      if J = Index then
        Exit(X)
      else
        Inc(J);
  end;
end;

function TXMLCollection.Remove(const Node: IXML): Integer;
begin
  Result := List.IndexOf(Node);
  Assert(Result >=0);
  FNodes.Remove(Node);
end;

procedure TXMLCollection.SetNodes(const aList: IXMLList);
begin
  inherited SetNodes(aList);
  FList := aList;
end;

{$ENDREGION}

{$REGION 'TXMLVariant'}

class operator TXMLVariant.Explicit(const a: Single): TXMLVariant;
begin
  Result.Value := FloatToStr(a);
end;

class operator TXMLVariant.Explicit(const a: Int64): TXMLVariant;
begin
  Result.Value := Int64ToStr(a);
end;

class operator TXMLVariant.Explicit(const a: Boolean): TXMLVariant;
begin
  Result.Value := BoolToStr(a);
end;

class operator TXMLVariant.Explicit(const a: Double): TXMLVariant;
begin
  Result.Value := FloatToStr(a, 20);
end;

class operator TXMLVariant.Explicit(const a: Byte): TXMLVariant;
begin
  Result.Value := IntToStr(a);
end;

class operator TXMLVariant.Explicit(const a: WideString): TXMLVariant;
begin
  Result.Value := a;
end;

class operator TXMLVariant.Explicit(const a: Integer): TXMLVariant;
begin
  Result.Value := IntToStr(a);
end;

class operator TXMLVariant.Explicit(const a: SmallInt): TXMLVariant;
begin
  Result.Value := IntToStr(a);
end;

class operator TXMLVariant.Explicit(const a: TXMLVariant): WideString;
begin
  Result := a.Value;
end;

class operator TXMLVariant.Explicit(const a: TXMLVariant): Single;
begin
  Result := StrToFloat(a.Value);
end;

class operator TXMLVariant.Explicit(const a: TXMLVariant): Int64;
begin
  Result := StrToInt64(a.Value);
end;

class operator TXMLVariant.Explicit(const a: TXMLVariant): Boolean;
begin
  Result := StrToBool(a.Value);
end;

class operator TXMLVariant.Explicit(const a: LongWord): TXMLVariant;
begin
  Result.Value := Int64ToStr(a);
end;

class operator TXMLVariant.Explicit(const a: TXMLVariant): Double;
begin
  Result := StrToDouble(a.Value);
end;

class operator TXMLVariant.Explicit(const a: TXMLVariant): SmallInt;
begin
  Result := StrToInt(a.Value);
end;

class operator TXMLVariant.Explicit(const a: TXMLVariant): Byte;
begin
  Result := StrToInt(a.Value);
end;

class operator TXMLVariant.Explicit(const a: TXMLVariant): Integer;
begin
  Result := StrToInt(a.Value);
end;

class operator TXMLVariant.Explicit(const a: TXMLVariant): LongWord;
begin
  Result := StrToInt64(a.Value);
end;

class operator TXMLVariant.Implicit(const a: Single): TXMLVariant;
begin
  Result.Value := FloatToStr(a);
end;

class operator TXMLVariant.Implicit(const a: Int64): TXMLVariant;
begin
  Result.Value := Int64ToStr(a);
end;

class operator TXMLVariant.Implicit(const a: Boolean): TXMLVariant;
begin
  Result.Value := BoolToStr(a);
end;

class operator TXMLVariant.Implicit(const a: Double): TXMLVariant;
begin
  Result.Value := FloatToStr(a, 20);
end;

class operator TXMLVariant.Implicit(const a: Byte): TXMLVariant;
begin
  Result.Value := IntToStr(a);
end;

class operator TXMLVariant.Implicit(const a: WideString): TXMLVariant;
begin
  Result.Value := a;
end;

class operator TXMLVariant.Implicit(const a: Integer): TXMLVariant;
begin
  Result.Value := IntToStr(a);
end;

class operator TXMLVariant.Implicit(const a: SmallInt): TXMLVariant;
begin
  Result.Value := IntToStr(a);
end;

class operator TXMLVariant.Implicit(const a: TXMLVariant): WideString;
begin
  Result := a.Value;
end;

class operator TXMLVariant.Implicit(const a: TXMLVariant): Single;
begin
  Result := StrToFloat(a.Value);
end;

class operator TXMLVariant.Implicit(const a: TXMLVariant): Int64;
begin
  Result := StrToInt64(a.Value);
end;

class operator TXMLVariant.Implicit(const a: TXMLVariant): Boolean;
begin
  Result := StrToBool(a.Value);
end;

class operator TXMLVariant.Implicit(const a: LongWord): TXMLVariant;
begin

end;

class operator TXMLVariant.Implicit(const a: TXMLVariant): Double;
begin
  Result := StrToDouble(a.Value);
end;

class operator TXMLVariant.Implicit(const a: TXMLVariant): SmallInt;
begin
  Result := StrToInt(a.Value);
end;

class operator TXMLVariant.Implicit(const a: TXMLVariant): Byte;
begin
  Result := StrToInt(a.Value);
end;

class operator TXMLVariant.Implicit(const a: TXMLVariant): Integer;
begin
  Result := StrToInt(a.Value);
end;

class operator TXMLVariant.Implicit(const a: TXMLVariant): LongWord;
begin
  Result := StrToInt64(a.Value);
end;

{$ENDREGION 'TXMLVariant'}

{ TXMLClassesTree }

function CompareAnsiStrings(const Item1, Item2: AnsiString): Integer;
var
  l1, l2: Integer;
begin
  l1 := Length(Item1);
  l2 := Length(Item2);
  if l1 < l2 then
    exit(-1)
  else if l1 > l2 then
    exit(1);
  Result := CompareMemory(@Item1[1], @Item2[1], l1);
end;

constructor TXMLClassesTree.CreateClassesTree;
begin
  Create(CompareAnsiStrings, nil);
end;

initialization

finalization

  TXML.ClassesTree.Free;

end.
