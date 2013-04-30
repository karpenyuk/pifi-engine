unit uMiscUtils;

{$IFDEF FPC}
  {$H+} // Enable long strings.
  {$mode delphi}
{$ENDIF}

interface

uses {$IFDEF MSWINDOWS} Windows,{$ENDIF} Classes, {SysUtils,} uVMath;
const
   BufSize = 10240; { Load input data in chunks of BufSize Bytes. }
   LineLen = 100;   { Allocate memory for the current line in chunks
                      of LineLen Bytes. }

Type

  TLongArray = array[0..15] of Cardinal;
  PLongArray = ^TLongArray;
  TWordArray = array[0..15] of Word;
  PWordArray = ^TWordArray;


  TTextFileParser = class
  private
    FSourceStream : TStream;     { Load from this stream }
    FBuffer: AnsiString;         { Buffer }
    FLine : String;              { current line }
    FLineNo : Integer;           { current Line number - for error messages }
    FEof : Boolean;              { Stream done? }
    FBufPos : Integer;           { Position in the buffer }
    FStreamOwner: boolean;       { Stream owner? }
    function StreamEOF(S: TStream): Boolean;
    function Rest(const s:string; Count: integer):string;
    procedure FillBuffer;
    procedure LoadFromStream(aStream: TStream);
    procedure LoadFromFile(FileName: string);
  public
    constructor Create(aStream: TStream); overload;
    constructor Create(FileName: string); overload;
    destructor Destroy; override;
    // Read a single line of text from the source stream, set FEof to true when done.
    function ReadLine: string;
    function NextToken(var s: String; delimiter: Char) : String;
    function NextTokenAsFloat(var s: String; delimiter: Char) : single;
    function NextTokenAsInt(var s: String; delimiter: Char) : integer;
    function NextTokenAsQuotedString(var s: string; const Quote: string='"'): string;

    property EoF: boolean read FEof;
    property LineNo: integer read FLineNo;
  end;

{ TBits class }

  TIntegerBits = class
  private
    FSize: Integer;
    FFullMemSize: integer;
    FBits: Pointer;
    procedure Error;
    procedure SetSize(Value: Integer);
    procedure SetBit(Index: Integer; Value: Boolean);
    function GetBit(Index: Integer): Boolean;
  public
    destructor Destroy; override;
    function OpenBit: Integer;
    procedure ResetBits;
    property Bits[Index: Integer]: Boolean read GetBit write SetBit; default;
    property Size: Integer read FSize write SetSize;
  end;

{ TGUIDEx record }

  TGUIDEx = record
    class function NewGUID: TGUID; static;
    class function Empty: TGUID; static;

    class function IsEmpty( const aVal: TGUID ): Boolean; static;
    class function IsEqual( const aVal1,aVal2: TGUID ): Boolean; static;

    class function ToString( const aVal: TGUID ): String; static;
    class function FromString( const aStr: String ): TGUID; static;
  end;


function StringHashKey(const name: string): Integer;
function BufferHash(const Buffer; Count: integer): word;
function HashKey(const v : vec4; hashSize : Integer) : Integer;
function GetHashFromBuff(const Buffer; Count: Integer): Word; assembler;
function GetLongHash(const aValue: string): LongInt; inline;

function CutString(const s: string; l: integer): string;

function min(a,b: integer): integer; overload;
function min(a,b: single): single; overload;
function max(a,b: integer): integer; overload;
function max(a,b: single): single; overload;

procedure FreeList(var List: TList);
procedure FreeObjectList(var List: TList);overload;
procedure FreeAndNil(var Obj);

function StrToInt(s:string):integer;
function StrToFloat(s:string):Single;
function FloatToStr(x: double; width: byte=10; decimals: byte=6): string;
function IntToStr(x:integer):string;
function IntToHex( aVal: Integer; aDigits: Integer ): String;
function Vector4ToStr(x: TVector): string;
function Vector3ToStr(x: TVector): string;

function TrimLeft( const aStr: String ): String;
function StrScan( const aStr: PWideChar; aChar: WideChar ): PWideChar;

function CheckPath(Path: string): string;

procedure CLog( const aMsg: String );

function CompareMem(const p1,p2: pointer; aSize: integer): boolean;

{: Returns the current value of the highest-resolution counter.<p>
   If the platform has none, should return a value derived from the highest
   precision time reference available, avoiding, if possible, timers that
   allocate specific system resources. }
procedure QueryPerformanceCounter(var val: Int64);
{: Returns the frequency of the counter used by QueryPerformanceCounter.<p>
   Return value is in ticks per second (Hz), returns False if no precision
   counter is available. }
function QueryPerformanceFrequency(var val: Int64): Boolean;

implementation

{$IFDEF FPC}
uses
  {$IFDEF UNIX}
    Unix, BaseUnix,
  {$ENDIF}
  sysutils;
{$ENDIF}

function BufferHash(const Buffer; Count: integer): word; assembler;
asm
        MOV     ECX,EDX
        MOV     EDX,EAX
        XOR     EAX,EAX
@@1:    ROL     AX,5
        XOR     AL,[EDX]
        INC     EDX
        DEC     ECX
        JNE     @@1
end;

function min(a,b: integer): integer; overload;
begin if a<b then result:=a else result:=b; end;

function max(a,b: integer): integer; overload;
begin if a>b then result:=a else result:=b; end;

function min(a,b: single): single; overload;
begin if a<b then result:=a else result:=b; end;

function max(a,b: single): single; overload;
begin if a>b then result:=a else result:=b; end;


function CutString(const s: string; l: integer): string;
begin
  result:=copy(s,1,min(length(s),l));
end;

procedure FreeList(var List: TList);
var i: integer;
    p: pointer;
begin
  if not assigned(List) then exit;
  for i:=0 to List.Count-1 do begin
    p:=List[i]; if assigned(p) then Dispose(p);
  end; List.Free; List:=nil;
end;

procedure FreeObjectList(var List: TList);overload;
var i: integer;
    p: TObject;
begin
  if not assigned(List) then exit;
  for i:=0 to List.Count-1 do begin
    p:=List[i];
    if assigned(p) then
      FreeAndNil(p);
  end; FreeAndNil(List);
end;

procedure FreeAndNil(var Obj);
var Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  if assigned(Temp) then Temp.Free;
end;

function StrToInt(s:string):integer;
var code:integer;
begin
  val(s,result,code);
end;
function StrToFloat(s:string):Single;
var code:integer;
begin
  val(s,result,code);
end;
function FloatToStr(x: double; width: byte=10; decimals: byte=6): string;
begin
  str(x:width:decimals,result);
end;
function IntToStr(x:integer):string;
var s: ansistring;
begin
   str(x,s); result:=string(s);
end;


//
// CvtInt
//
procedure CvtInt;
asm
        OR      CL,CL
        JNZ     @CvtLoop
@C1:    OR      EAX,EAX
        JNS     @C2
        NEG     EAX
        CALL    @C2
        MOV     AL,'-'
        INC     ECX
        DEC     ESI
        MOV     [ESI],AL
        RET
@C2:    MOV     ECX,10

@CvtLoop:
        PUSH    EDX
        PUSH    ESI
@D1:    XOR     EDX,EDX
        DIV     ECX
        DEC     ESI
        ADD     DL,'0'
        CMP     DL,'0'+10
        JB      @D2
        ADD     DL,('A'-'0')-10
@D2:    MOV     [ESI],DL
        OR      EAX,EAX
        JNE     @D1
        POP     ECX
        POP     EDX
        SUB     ECX,ESI
        SUB     EDX,ECX
        JBE     @D5
        ADD     ECX,EDX
        MOV     AL,'0'
        SUB     ESI,EDX
        JMP     @z
@zloop: MOV     [ESI+EDX],AL
@z:     DEC     EDX
        JNZ     @zloop
        MOV     [ESI],AL
@D5:
end;

//
// IntToHex
//


function IntToHex( aVal: Integer; aDigits: Integer ): String;
{$IFDEF FPC}
begin
  Result:=sysutils.IntToHex(aVal, aDigits);
{$ELSE}
asm
        CMP     EDX, 32
        JBE     @A1
        XOR     EDX, EDX
@A1:    PUSH    ESI
        MOV     ESI, ESP
        SUB     ESP, 32
        PUSH    ECX
        MOV     ECX, 16
        CALL    CvtInt
        MOV     EDX, ESI
        POP     EAX
        {нету в FPC, судя по гуглу есть
        функция _LStrFromPCharLen в KOL}
        CALL    System.@UStrFromPCharLen  //нету в FPC,
        ADD     ESP, 32
        POP     ESI
{$ENDIF}
end;

function HashKey(const v : vec4; hashSize : Integer) : Integer;
begin
  Result:=((Integer(@v[0])
            xor Integer(@v[1])
            xor Integer(@v[2])
            xor Integer(@v[3])) shr 16) and hashSize;
end;

function StringHashKey(const name: string): Integer;
var i, n, res: Integer;
begin
  if name='' then result:=-1 else begin
    n := Length(name); Res := n;
    for i := 1 to n do
        Res := (Res shl 1) + Byte(name[i]);
    result:=res;
  end;
end;

function GetHashFromBuff(const Buffer; Count: Integer): Word; assembler;
asm
        MOV     ECX,EDX
        MOV     EDX,EAX
        XOR     EAX,EAX
@@1:    ROL     AX,5
        XOR     AL,[EDX]
        INC     EDX
        DEC     ECX
        JNE     @@1
end;

function GetLongHash(const aValue: string): LongInt; inline;
var
  G: longint;
  i: integer;
  cHash: longint;
begin
  cHash := 0;
  for I := 1 to length(aValue) do begin
    cHash := (cHash shl 4) + ord(aValue[i]);
    G := cHash and longint($F000000);
    if G <> 0 then cHash := (cHash xor (G shr 24)) xor G;
  end;
  result := cHash;
end;



function Vector4ToStr(x: TVector): string;
begin
  result:='('+floattostr(x[0])+'; '+floattostr(x[1])+'; '+
    floattostr(x[2])+'; '+floattostr(x[3])+')';
end;

function Vector3ToStr(x: TVector): string; overload;
begin
  result:='('+floattostr(x[0])+'; '+floattostr(x[1])+'; '+
    floattostr(x[2])+')';
end;


//
// TrimLeft
//
function TrimLeft( const aStr: String ): String;
var
    i: integer;
begin

  i := 1;
  while ( i < length(aStr)) and ( aStr[i] < '!' ) do
    inc(i);

  result := copy( aStr, i, length(aStr));

end;


//
// StrScan
//
function StrScan( const aStr: PWideChar; aChar: WideChar ): PWideChar;
begin

  result := aStr;
  while result^ <> aChar do begin

    if result^ = #0 then begin
      result := nil;
      exit;
      end;

    inc(result);
    end;

end;


//
// CLog
//
procedure CLog( const aMsg: String );
begin

  if isConsole then
    writeln( aMsg );

end;


function CheckPath(Path: string): string;
begin
  if path<>'' then if path[length(path)]<>'\'
  then result:=path+'\' else result:=path;
end;




{ TTextFileParser }

// StreamEOF
//
function TTextFileParser.StreamEOF(S : TStream) : Boolean;
begin
  { Is the stream at its end? }
  if not assigned(S) then result:=true
  else Result:=(S.Position>=S.Size);
end;

function TTextFileParser.ReadLine: string;
var j : Integer;
begin
  Inc(FLineNo); j:=1; Result:='';
  if FBufPos<1 then FillBuffer;
  while True do begin
    if FBufPos>Length(FBuffer) then begin
      if StreamEof(FSourceStream) then begin
        FEof:=True; break; end else FillBuffer;
    end else begin
      case FBuffer[FBufPos] of
        #10, #13 : begin
          Inc(FBufPos);
          if FBufPos>Length(FBuffer) then
            if StreamEof(FSourceStream) then break else FillBuffer;
            if (FBuffer[FBufPos]=#10) or (FBuffer[FBufPos]=#13) then Inc(FBufPos);
          break;
        end;
      else
        if j>Length(FLine) then SetLength(FLine, Length(FLine)+LineLen);
        if FBuffer[FBufPos]=#9 then FLine[j]:=#32 else FLine[j]:=Char(FBuffer[FBufPos]);
        Inc(FBufPos); Inc(j);
      end;
    end;
  end;
  SetLength(FLine,j-1); result:=FLine;
end;

function TTextFileParser.Rest(const s:string; Count:integer):string;
{ Return the right part of s including s[Count]. }
begin
  Result:=copy(s,Count,Length(s)-Count+1);
end;

// NextToken
//
function TTextFileParser.NextToken(var s : String; delimiter : Char) : String;
{ Return the next Delimiter-delimited Token from the string s and
  remove it from s }
var p: Integer;
begin
   p:=Pos(Delimiter, s);
   if p=0 then begin Result:=s; s:=''; end else begin
     Result:=copy(s, 1, p-1); s:=TrimLeft(Rest(s, p+1));
   end;
end;

constructor TTextFileParser.Create(aStream: TStream);
begin
  inherited Create;
  FStreamOwner:=false;
  LoadFromStream(aStream);
end;

constructor TTextFileParser.Create(FileName: string);
begin
  inherited Create;
  LoadFromFile(FileName);
end;

destructor TTextFileParser.Destroy;
begin
  if FStreamOwner then FSourceStream.Free;
  inherited;
end;

procedure TTextFileParser.FillBuffer;
var l : Integer;
begin
   l:=FSourceStream.Size-FSourceStream.Position;
   if l>BufSize then l:=BufSize;
   SetLength(FBuffer, l);
   FSourceStream.Read(FBuffer[1], l); FBufPos:=1;
end;

procedure TTextFileParser.LoadFromFile(FileName: string);
var S: TStream;
begin
  S:=TFileStream.Create(FileName, $20);  //fmOpenRead or fmShareDenyWrite
  FStreamOwner:=true; LoadFromStream(S);
end;

procedure TTextFileParser.LoadFromStream(aStream: TStream);
begin
  if assigned(FSourceStream) and (FStreamOwner) then FSourceStream.Free;
  FEof:=False; FSourceStream:=aStream; FLineNo:=0;
end;

function TTextFileParser.NextTokenAsFloat(var s: String;
  delimiter: Char): single;
var x: string;
begin
  x:=NextToken(s,delimiter); result:=strtofloat(x);
end;

function TTextFileParser.NextTokenAsInt(var s: String;
  delimiter: Char): integer;
var x: string;
begin
  x:=NextToken(s,delimiter); result:=strtoint(x);
end;

function TTextFileParser.NextTokenAsQuotedString(var s: string;
  const Quote: string): string;
var p,n: integer;
    {$IFNDEF FPC} pc: PChar;{$ELSE} pc: PWideChar; {$ENDIF}
begin
  pc:={$IFNDEF FPC}PChar(Quote){$ELSE}PWideChar(Quote){$ENDIF};
  {StrScan требует в качестве аргументов PWideChar}
  if StrScan({$IFNDEF FPC} PChar(s){$ELSE}PWideChar(s) {$ENDIF} ,Quote[1])=nil then result:='' else begin
    p:=pos(Quote[1],s);
    n:=1; while (p+n<length(s)) and (StrScan(pc,s[p+n])=nil) do inc(n);
    if StrScan(pc,s[n])=nil then dec(n);
    result:=copy(s,2,n); s:=trimleft(Rest(s,p+n+1));
  end;
end;

{ TBits }

const
  BitsPerInt = SizeOf(Integer) * 8;

type
  TBitEnum = 0..BitsPerInt - 1;
  TBitSet = set of TBitEnum;
  PBitArray = ^TBitArray;
  TBitArray = array[0..4096] of TBitSet;

destructor TIntegerBits.Destroy;
begin
  SetSize(0);
  inherited Destroy;
end;

procedure TIntegerBits.Error;
var SBitsIndexError: string;
begin
  SBitsIndexError:='Bits index out of range';
  raise EBitsError.CreateRes(@SBitsIndexError);
end;

procedure TIntegerBits.SetSize(Value: Integer);
var
  NewMem: Pointer;
  NewMemSize: Integer;
  OldMemSize: Integer;

  function Min(X, Y: Integer): Integer;
  begin
    Result := X;
    if X > Y then Result := Y;
  end;

begin
  if Value <> Size then
  begin
    if Value < 0 then Error;
    NewMemSize := ((Value + BitsPerInt - 1) div BitsPerInt) * SizeOf(Integer);
    OldMemSize := ((Size + BitsPerInt - 1) div BitsPerInt) * SizeOf(Integer);
    if NewMemSize <> OldMemSize then
    begin
      NewMem := nil;
      if NewMemSize <> 0 then
      begin
        GetMem(NewMem, NewMemSize);
        FillChar(NewMem^, NewMemSize, 0);
      end;
      if OldMemSize <> 0 then
      begin
        if NewMem <> nil then
          Move(FBits^, NewMem^, Min(OldMemSize, NewMemSize));
        FreeMem(FBits, OldMemSize);
      end;
      FBits := NewMem;
    end;
    FSize := Value; FFullMemSize := NewMemSize;
  end;
end;

procedure TIntegerBits.SetBit(Index: Integer; Value: Boolean); assembler;
asm
        CMP     Index,[EAX].FSize
        JAE     @@Size

@@1:    MOV     EAX,[EAX].FBits
        OR      Value,Value
        JZ      @@2
        BTS     [EAX],Index
        RET

@@2:    BTR     [EAX],Index
        RET

@@Size: CMP     Index,0
        JL      TIntegerBits.Error
        PUSH    Self
        PUSH    Index
        PUSH    ECX {Value}
        INC     Index
        CALL    TIntegerBits.SetSize
        POP     ECX {Value}
        POP     Index
        POP     Self
        JMP     @@1
end;

function TIntegerBits.GetBit(Index: Integer): Boolean; assembler;
asm
        CMP     Index,[EAX].FSize
        JAE     TIntegerBits.Error
        MOV     EAX,[EAX].FBits
        BT      [EAX],Index
        SBB     EAX,EAX
        AND     EAX,1
end;

function TIntegerBits.OpenBit: Integer;
var
  I: Integer;
  B: TBitSet;
  J: TBitEnum;
  E: Integer;
begin
  E := (Size + BitsPerInt - 1) div BitsPerInt - 1;
  for I := 0 to E do
    if PBitArray(FBits)^[I] <> [0..BitsPerInt - 1] then
    begin
      B := PBitArray(FBits)^[I];
      for J := Low(J) to High(J) do
      begin
        if not (J in B) then
        begin
          Result := I * BitsPerInt + J;
          if Result >= Size then Result := Size;
          Exit;
        end;
      end;
    end;
  Result := Size;
end;

procedure TIntegerBits.ResetBits;
begin
  if FSize>0 then FillChar(FBits^, FFullMemSize, 0);
end;

procedure QuickSort(var A: array of Integer);
  procedure Sort(var A: array of Integer; iLo, iHi: Integer);
  var Lo, Hi, Mid, T: Integer;
  begin
    Lo := iLo; Hi := iHi;
    Mid := A[(Lo + Hi) div 2];
    repeat
      while A[Lo] < Mid do Inc(Lo);
      while A[Hi] > Mid do Dec(Hi);
      if Lo <= Hi then
      begin
        T := A[Lo]; A[Lo] := A[Hi]; A[Hi] := T;
        Inc(Lo); Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then Sort(A, iLo, Hi);
    if Lo < iHi then Sort(A, Lo, iHi);
  end;
begin
  Sort(A, Low(A), High(A));
end;


{ TGUIDEx class }

//
// TGUIDEx.Empty
//
class function TGUIDEx.Empty: TGUID;
const
    c: TGUID = '{00000000-0000-0000-0000-000000000000}';
begin

  result := c;

end;


//
// TGUIDEx.IsEqual
//
class function TGUIDEx.IsEqual( const aVal1,aVal2: TGUID ): Boolean;
var
    p1,p2: PLongArray;
begin

  p1 := PLongArray( @aVal1 );
  p2 := PLongArray( @aVal2 );

  result := ( p1[0] = p2[0] ) and ( p1[1] = p2[1] )
        and ( p1[2] = p2[2] ) and ( p1[3] = p2[3] );

end;


//
// TGUIDEx.FromString
//
class function TGUIDEx.FromString( const aStr: String ): TGUID;
begin

  if ( length( aStr ) <> 38 ) or ( aStr[1] <> '{') then begin
    CLog( 'InvalidGUID: ' + aStr );
    result := Empty;
    exit;
    end;

  result.D1 := StrToInt( '$' + copy( aStr, 2, 8 ));
  result.D2 := StrToInt( '$' + copy( aStr, 11, 4 ));
  result.D3 := StrToInt( '$' + copy( aStr, 16, 4 ));
  result.D4[0] := StrToInt( '$' + copy( aStr, 21, 2 ));
  result.D4[1] := StrToInt( '$' + copy( aStr, 23, 2 ));
  result.D4[2] := StrToInt( '$' + copy( aStr, 26, 2 ));
  result.D4[3] := StrToInt( '$' + copy( aStr, 28, 2 ));
  result.D4[4] := StrToInt( '$' + copy( aStr, 30, 2 ));
  result.D4[5] := StrToInt( '$' + copy( aStr, 32, 2 ));
  result.D4[6] := StrToInt( '$' + copy( aStr, 34, 2 ));
  result.D4[7] := StrToInt( '$' + copy( aStr, 36, 2 ));

end;


//
// TGUIDEx.IsEmpty
//
class function TGUIDEx.IsEmpty( const aVal: TGUID): Boolean;
begin

  result := IsEqual( aVal , Empty );

end;


//
// TGUIDEx.NewGUID
//
class function TGUIDEx.NewGUID: TGUID;
var
    p: PWordArray;
    i: integer;
begin

  p := @result;

  randomize;
  for i := 0 to 7 do
    p[i] := random( 65535 );

end;


//
// TGUIDEx.ToString
//
class function TGUIDEx.ToString( const aVal: TGUID ): String;
begin

  result := '{' + IntToHex( aVal.D1, 8 ) +
            '-' + IntToHex( aVal.D2, 4 ) +
            '-' + IntToHex( aVal.D3, 4 ) +
            '-' + IntToHex( aVal.D4[0], 2 ) + IntToHex( aVal.D4[1], 2 ) +
            '-' + IntToHex( aVal.D4[2], 2 ) + IntToHex( aVal.D4[3], 2 ) +
                  IntToHex( aVal.D4[4], 2 ) + IntToHex( aVal.D4[5], 2 ) +
                  IntToHex( aVal.D4[6], 2 ) + IntToHex( aVal.D4[7], 2 ) + '}';

end;

function CompareMem(const p1,p2: pointer; aSize: integer): boolean;
var i: integer;
    pb1, pb2: PByte;
begin
  result:=true; pb1:=PByte(p1); pb2:=PByte(p2); i:=0;
  while (i<aSize) and (pb1^=pb2^) do begin inc(i); inc(pb1); inc(pb2); end;
  if i<>aSize then result:=false;
end;

// QueryPerformanceCounter
//
{$IFDEF UNIX}
var
  vProgStartSecond: int64;

procedure Init_vProgStartSecond;
var
  tz: timeval;
begin
  fpgettimeofday(@tz, nil);
  vProgStartSecond := tz.tv_sec;
end;
{$ENDIF}

procedure QueryPerformanceCounter(var val: Int64);
{$IFDEF MSWINDOWS}
begin
  Windows.QueryPerformanceCounter(val);
end;
{$ENDIF}
{$IFDEF UNIX}
var
  tz: timeval;
begin
  fpgettimeofday(@tz, nil);
  val := tz.tv_sec - vProgStartSecond;
  val := val * 1000000;
  val := val + tz.tv_usec;
end;
{$ENDIF}

// QueryPerformanceFrequency
//

function QueryPerformanceFrequency(var val: Int64): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := Boolean(Windows.QueryPerformanceFrequency(val));
end;
{$ENDIF}
{$IFDEF UNIX}
begin
  val := 1000000;
  Result := True;
end;
{$ENDIF}

initialization
{$IFDEF FPC}
{$IFDEF UNIX}
  Init_vProgStartSecond;
{$ENDIF}
{$ENDIF}

end.
