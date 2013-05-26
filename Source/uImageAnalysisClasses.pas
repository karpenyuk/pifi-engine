unit uImageAnalysisClasses;

interface

uses
  Classes, uBaseTypes, uVMath;

const
  NEIGHBOUR_DIM = 5;
  HALF_NEIGHBOUR_DIM = 5 div 2;
  NEIGHBOUR_SIZE_2COLOR = 2 * NEIGHBOUR_DIM * NEIGHBOUR_DIM;
  NEIGHBOUR_SIZE_3COLOR = 3 * NEIGHBOUR_DIM * NEIGHBOUR_DIM;
  SIMILAR_NEIGHBOUR_SIZE = 8;
  INV255 = 1.0 / 255.0;

const
  GL_RG = $8227;
  GL_RGB = $1907;
  GL_RGBA = $1908;
  GL_BGR = $80E0;
  GL_BGRA = $80E1;
  GL_RG8 = $822B;
  GL_RGB8 = $8051;
  GL_RGBA8 = $8058;
  GL_UNSIGNED_BYTE = $1401;

type
  TEdgePolicy = (
    epNonRepeat,
    epRepeat,
    epNonRepeatDbl,
    epRepeatDbl);

  TEdgePolicyFunc = function(aCoord, aSize: integer; aLoc: integer = 0): integer of object;
  EdgePolicyFor = class
    class function GetFunc(aPolicy: TEdgePolicy): TEdgePolicyFunc;
    class function GetFuncN(aPolicy: TEdgePolicy): TEdgePolicyFunc;
    class function RepeatedImage(c, size: integer; aLoc: integer = 0): integer;
    class function NonRepeatedImage(c, size: integer; aLoc: integer = 0): integer;
    class function MirrorRepeatedImage(c, size: integer; aLoc: integer = 0): integer;
    class function RepeatedDblImage(c, size: integer; aLoc: integer = 0): integer;
    class function NonRepeatedDblImage(c, size: integer; aLoc: integer = 0): integer;
    class function MirrorRepeatedDblImage(c, size: integer; aLoc: integer = 0): integer;
  end;

  IVec2 = array [0 .. 1] of integer;
  TNeighborhood2c = array [0 .. NEIGHBOUR_SIZE_2COLOR - 1] of single;
  TNeighborhood3c = array [0 .. NEIGHBOUR_SIZE_3COLOR - 1] of single;
  TMostSimilar = array [0 .. SIMILAR_NEIGHBOUR_SIZE - 1] of IVec2;
  TVector6f = array [0 .. 5] of single;

  TColorPCAMatrix = array [0 .. 2, 0 .. 1] of single;
  TNeighbPCAmatrix = array [0 .. NEIGHBOUR_SIZE_3COLOR - 1, 0 .. 5] of single;

  TFloatPixel = record
    r, g, b: single;
  end;

const
  ZERO_PIXEL: TFloatPixel = (r: 0; g: 0; b: 0);
  ZERO_VECTOR6D: TVector6f = (0, 0, 0, 0, 0, 0);

type

  TIVec2Array2D = class
  private
    FData: array of IVec2;
    FWidth, FHeight: integer;
    FWidthPolicy, FHeightPolicy: TEdgePolicy;
    FWidthAccessFunc, FHeightAccessFunc: TEdgePolicyFunc;
    FCycleCounter: integer;

    function GetItem(x, y: integer): IVec2;
    procedure SetItem(x, y: integer; const aValue: IVec2);
    procedure SetWidthEdgePolisy(ap: TEdgePolicy);
    procedure SetHeightEdgePolisy(ap: TEdgePolicy);
  public
    constructor Create(w, h: integer);

    procedure Clear(const aClearValue: IVec2);
    procedure Assign(source: TIVec2Array2D);

    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property WidthEdgePolisy: TEdgePolicy read FWidthPolicy
      write SetWidthEdgePolisy;
    property HeightEdgePolisy: TEdgePolicy read FHeightPolicy
      write SetHeightEdgePolisy;
    property At[x, y: integer]: IVec2 read GetItem write SetItem; default;
    // For progressing computing
    property CycleCounter: integer read FCycleCounter write FCycleCounter;
  end;

  TNeighborhoods = class
  private
    FData: array of TNeighborhood3c;
    FProjData: array of TVector6f;
    FLevels, FWidth, FHeight: integer;
    function GetNeighb(x, y, z: integer): TNeighborhood3c;
    procedure SetNeighb(x, y, z: integer; const ANeighborhood: TNeighborhood3c);
    function GetNeighb6D(x, y, z: integer): TVector6f;
    procedure SetNeighb6D(x, y, z: integer; const ANeighborhood: TVector6f);
  public
    constructor Create(w, h, l: integer);

    procedure Save(aStream: TStream);
    procedure Load(aStream: TStream);

    function Dump(aLevel: integer): TImageDesc;

    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property At[x, y, z: integer]: TNeighborhood3c read GetNeighb
      write SetNeighb; default;
    property As6DAt[x, y, z: integer]: TVector6f read GetNeighb6D
      write SetNeighb6D;
  end;

  TMostSimilars = class
  private
    FData: array of TMostSimilar;
    FLevels, FWidth, FHeight: integer;
    function GetMostSimilar(x, y, z: integer): TMostSimilar;
    procedure SetMostSimilar(x, y, z: integer;
      const AMostSimilar: TMostSimilar);
  public
    constructor Create(w, h, l: integer);

    procedure Save(aStream: TStream);
    procedure Load(aStream: TStream);
    function Dump(aLevel: integer): TStringList;

    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property Levels: integer read FLevels;
    property At[x, y, z: integer]: TMostSimilar read GetMostSimilar
      write SetMostSimilar; default;
  end;

  TFloatImage = class
  private
    FWidth: integer;
    FHeight: integer;
    FData: array of TFloatPixel;
    function GetPixel(x, y: integer): TFloatPixel;
    procedure PutPixel(x, y: integer; const aValue: TFloatPixel);
  public
    constructor Create(aWidth: integer; aHeight: integer);
    procedure AssignFromImage(const aDesc: TImageDesc);
    function GetImage: TImageDesc;
    procedure Clear(cR, cG, cB: single);
    function BilinearWrap(u, v: single): TFloatPixel;

    procedure Save(aStream: TStream);
    procedure Load(aStream: TStream);

    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property Pixel[x, y: integer]: TFloatPixel read GetPixel
      write PutPixel;
  end;

  TImagePyramid = class
  protected
    FFilterSize: integer;
    FWeights: array of single;
    FLevels: array of TFloatImage;
  private
    function GetLevelsAmount: integer;
    function GetLevel(level: integer): TFloatImage;

    function GaussFW1(x: double): double;
    procedure GaussianFilter(src, dst: TFloatImage; downSampleFactor: integer);
  public
    constructor Create(aWidth, aHeight: integer);
    destructor Destroy; override;
    procedure AssignFromImage(const aDesc: TImageDesc; alevel: integer);
    procedure GeneratePyramid;

    property LevelsAmount: integer read GetLevelsAmount;
    property level[level: integer]: TFloatImage read GetLevel;
  end;

  TAnalyzedLevel = record
    LevelId: integer;
    FloatImage: TFloatImage; // For analisys purpose, not in saved data
    Image: TImageDesc;
    ProjectedImage: TImageDesc;
    ColorScale: TVector;
    ColorOffset: TVector;
    ColorPCAMatrix: TColorPCAMatrix;
    NeihgbPCAMatrix: TNeighbPCAmatrix;
    kNearest: TImageDesc;
    Neighborhoods: array[0..1] of TImageDesc;
    NeighbScale: TVector6f;
    NeighbOffset: TVector6f;
    CycleCounter: integer;
  end;
  PAnalyzedLevel = ^TAnalyzedLevel;

  TAnalysisData = class
  private
    // Exemplar image
    FExemplar: TImageDesc;
    FLevels: array of TAnalyzedLevel;
    FkNearests: TMostSimilars;
    FNeighborhoods: TNeighborhoods;
    FEdgePolicy: TEdgePolicy;
    FEdgeFunc: TEdgePolicyFunc;
    FNEdgeFunc: TEdgePolicyFunc;
    FValid: Boolean;
    function GetLevelsAmount: integer;
    function GetImage(alevel: integer): PImageDesc;
    function GetFloatImage(level: integer): TFloatImage;
    function GetLevel(alevel: integer): PAnalyzedLevel;
    function GetExemplar: PImageDesc;
    procedure SetEdgePolicy(const Value: TEdgePolicy);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveToFile(const aFileName: string);
    procedure LoadFromFile(const aFileName: string);
    procedure Clear;
    // Gathers neighborhood at i,j in the stack level
    function GatherNeighborhood(x, y, z: integer): TNeighborhood3c;
    procedure CreateImageStack(aPyramid: TImagePyramid);
    procedure SetExemplar(const Value: TImageDesc);
    property Exemplar: PImageDesc read GetExemplar;
    property Levels[alevel: integer]: PAnalyzedLevel
      read GetLevel; default;
    property FloatImages[alevel: integer]: TFloatImage read GetFloatImage;
    property Images[alevel: integer]: PImageDesc read GetImage;
    // All neighborhoods (pre-gathered for efficiency)
    property Neighborhoods: TNeighborhoods read FNeighborhoods;
    // k-most similar neighborhoods within same exemplar stack level
    property kNearests: TMostSimilars read FkNearests;

    property LevelsAmount: integer read GetLevelsAmount;
    property EdgePolicy: TEdgePolicy read FEdgePolicy write SetEdgePolicy;
    // Flag to check data validation
    property IsValid: Boolean read FValid write FValid;
  end;

implementation

uses
  Math, uMath, uMiscUtils;


{$REGION 'EdgePolicyFor'}

class function EdgePolicyFor.RepeatedDblImage(c, size, aLoc: integer): integer;
var
  halfsize, q: integer;
begin
  halfsize := size div 2;
  q := halfsize * (aLoc div halfsize);
  result := q + RepeatedImage(c - q, halfsize);
end;

class function EdgePolicyFor.RepeatedImage(c, size: integer;
  aLoc: integer): integer;
begin
  result := c mod size;
  if result < 0 then
    result := result + size;
end;

class function EdgePolicyFor.NonRepeatedDblImage(c, size,
  aLoc: integer): integer;
var
  halfsize, q: integer;
begin
  halfsize := size div 2;
  q := halfsize * (aLoc div halfsize);
  result := q + NonRepeatedImage(c - q, halfsize);
end;

class function EdgePolicyFor.NonRepeatedImage(c, size: integer;
  aLoc: integer): integer;
begin
  if c < 0 then
    result := 0
  else if c >= size then
    result := size - 1
  else
    result := c;
end;

class function EdgePolicyFor.GetFunc(aPolicy: TEdgePolicy): TEdgePolicyFunc;
begin
  case aPolicy of
    epRepeat:
      result := RepeatedImage;
    epNonRepeat:
      result := NonRepeatedImage;
    epNonRepeatDbl:
      result := NonRepeatedDblImage;
    epRepeatDbl:
      result := RepeatedDblImage;
  end;
end;

class function EdgePolicyFor.GetFuncN(aPolicy: TEdgePolicy): TEdgePolicyFunc;
begin
  case aPolicy of
    epRepeat:
      result := RepeatedImage;
    epNonRepeat:
      result := MirrorRepeatedImage;
    epRepeatDbl:
      result := RepeatedDblImage;
    epNonRepeatDbl:
      result := MirrorRepeatedDblImage;
  end;
end;

class function EdgePolicyFor.MirrorRepeatedDblImage(c, size,
  aLoc: integer): integer;
var
  halfsize, q: integer;
begin
  halfsize := size div 2;
  q := halfsize * (aLoc div halfsize);
  result := q + MirrorRepeatedImage(c - q, halfsize);
end;

class function EdgePolicyFor.MirrorRepeatedImage(c, size: integer;
  aLoc: integer): integer;
var
  m: integer;
begin
  result := c mod size;
  m := c div size;
  if result < 0 then
  begin
    result := result + size;
    dec(m);
  end;
  if m and 1 = 1 then
      result := size - result - 1;
end;

{$ENDREGION}
{$REGION 'TIVec2Array2D'}

constructor TIVec2Array2D.Create(w: integer; h: integer);
begin
  FWidthPolicy := epRepeat;
  FHeightPolicy := epRepeat;
  FWidthAccessFunc := EdgePolicyFor.RepeatedImage;
  FHeightAccessFunc := EdgePolicyFor.RepeatedImage;
  FWidth := w;
  FHeight := h;
  SetLength(FData, w * h);
end;

function TIVec2Array2D.GetItem(x: integer; y: integer): IVec2;
begin
  x := FWidthAccessFunc(x, FWidth);
  y := FHeightAccessFunc(y, FHeight);
  result := FData[x + y * FWidth];
end;

procedure TIVec2Array2D.SetItem(x: integer; y: integer; const aValue: IVec2);
begin
  x := FWidthAccessFunc(x, FWidth);
  y := FHeightAccessFunc(y, FHeight);
  FData[x + y * FWidth] := aValue;
end;

procedure TIVec2Array2D.Clear(const aClearValue: IVec2);
var
  i: integer;
begin
  for i := 0 to High(FData) do
      FData[i] := aClearValue;
end;

procedure TIVec2Array2D.Assign(source: TIVec2Array2D);
begin
  FWidth := source.FWidth;
  FHeight := source.FHeight;
  FWidthPolicy := source.FWidthPolicy;
  FHeightPolicy := source.FHeightPolicy;
  FWidthAccessFunc := source.FWidthAccessFunc;
  FHeightAccessFunc := source.FHeightAccessFunc;
  FData := Copy(source.FData, 0, Length(source.FData));
  FCycleCounter := source.FCycleCounter;
end;

procedure TIVec2Array2D.SetWidthEdgePolisy(ap: TEdgePolicy);
begin
  if ap <> FWidthPolicy then
  begin
    FWidthPolicy := ap;
    FWidthAccessFunc := EdgePolicyFor.GetFunc(ap);
  end;
end;

procedure TIVec2Array2D.SetHeightEdgePolisy(ap: TEdgePolicy);
begin
  if ap <> FHeightPolicy then
  begin
    FHeightPolicy := ap;
    FHeightAccessFunc := EdgePolicyFor.GetFunc(ap);
  end;
end;
{$ENDREGION}
{$REGION 'TNeighborhoods'}


constructor TNeighborhoods.Create(w: integer; h: integer; l: integer);
var
  size: integer;
begin
  FLevels := l;
  FWidth := w;
  FHeight := h;
  size := w * h * l;
  SetLength(FData, size);
  SetLength(FProjData, size);
end;

function TNeighborhoods.Dump(aLevel: integer): TImageDesc;
var
  i, j, k, mi, mj, x, y: integer;
  p: PByte;
  n: TNeighborhood3c;
begin
  FillChar(result, SizeOf(TImageDesc), $00);

  result.Width := FWidth * (NEIGHBOUR_DIM + 1) - 1;
  result.Height := FHeight * (NEIGHBOUR_DIM + 1) - 1;
  result.InternalFormat := GL_RGBA8;
  result.ColorFormat := GL_RGBA;
  result.DataType := GL_UNSIGNED_BYTE;
  result.ElementSize := 4;
  result.DataSize := result.Width * result.Height * result.ElementSize;
  GetMem(result.Data, result.DataSize);
  p := result.Data;
  FillChar(p^, result.DataSize, $00);

  for i := 0 to FHeight - 1 do
  begin
    for j := 0 to FWidth - 1 do
    begin
      n := At[j, i, aLevel];
      k := 0;
      for mi := 0 to NEIGHBOUR_DIM - 1 do
        for mj := 0 to NEIGHBOUR_DIM - 1 do
        begin
          x := j * (NEIGHBOUR_DIM + 1) + mj;
          y := i * (NEIGHBOUR_DIM + 1) + mi;
          p := result.Data;
          Inc(p, (x + y * result.Width) * result.ElementSize);
          p[0] := floor(n[k]);
          p[1] := floor(n[k + 1]);
          p[2] := floor(n[k + 2]);
          p[3] := $FF;
          Inc(k, 3);
        end;
    end;
  end;
end;

function TNeighborhoods.GetNeighb(x: integer; y: integer; z: integer)
  : TNeighborhood3c;
begin
  x := EdgePolicyFor.RepeatedImage(x, FWidth);
  y := EdgePolicyFor.RepeatedImage(y, FHeight);
  result := FData[x + y * FWidth + z * FWidth * FHeight];
end;

procedure TNeighborhoods.Save(aStream: TStream);
begin
  aStream.Write(FWidth, SizeOf(Integer));
  aStream.Write(FHeight, SizeOf(Integer));
  aStream.Write(FLevels, SizeOf(Integer));
  if Length(FProjData) > 0 then
    aStream.Write(FProjData[0], Length(FProjData)*SizeOf(TVector6f));
end;

procedure TNeighborhoods.SetNeighb(x: integer; y: integer; z: integer;
  const ANeighborhood: TNeighborhood3c);
begin
  x := EdgePolicyFor.RepeatedImage(x, FWidth);
  y := EdgePolicyFor.RepeatedImage(y, FHeight);
  FData[x + y * FWidth + z * FWidth * FHeight] := ANeighborhood;
end;

function TNeighborhoods.GetNeighb6D(x: integer; y: integer; z: integer)
  : TVector6f;
begin
  x := EdgePolicyFor.RepeatedImage(x, FWidth);
  y := EdgePolicyFor.RepeatedImage(y, FHeight);
  result := FProjData[x + y * FWidth + z * FWidth * FHeight]
end;

procedure TNeighborhoods.Load(aStream: TStream);
begin
  aStream.Read(FWidth, SizeOf(Integer));
  aStream.Read(FHeight, SizeOf(Integer));
  aStream.Read(FLevels, SizeOf(Integer));
  SetLength(FData, FWidth*FHeight*FLevels);
  SetLength(FProjData, FWidth*FHeight*FLevels);
  if Length(FProjData) > 0 then
    aStream.Read(FProjData[0], Length(FProjData)*SizeOf(TVector6f));
end;

procedure TNeighborhoods.SetNeighb6D(x: integer; y: integer; z: integer;
  const ANeighborhood: TVector6f);
begin
  x := EdgePolicyFor.RepeatedImage(x, FWidth);
  y := EdgePolicyFor.RepeatedImage(y, FHeight);
  FProjData[x + y * FWidth + z * FWidth * FHeight] := ANeighborhood;
end;

{$ENDREGION}
{$REGION 'TMostSimilars'}


constructor TMostSimilars.Create(w: integer; h: integer; l: integer);
var
  size: integer;
begin
  FLevels := l;
  FWidth := w;
  FHeight := h;

  size := w * h * l;
  SetLength(FData, size);
end;

function TMostSimilars.Dump(aLevel: integer): TStringList;
var
  i, j, k: integer;
  line: string;
  sim: TMostSimilar;
begin
  Result := TStringList.Create;
  for i := 0 to FHeight - 1 do
  begin
    line := ' y:' + IntToStr(i) + ' ';
    for j := 0 to FWidth - 1 do
    begin
      sim := At[j, i, aLevel];
      line := line + ' x:' + IntToStr(j) + ' ';
      for k := 0 to SIMILAR_NEIGHBOUR_SIZE - 1 do
        line := line + '(' + IntToStr(sim[k][0]) + ';' + IntToStr(sim[k][1]) +  ')';
    end;
    Result.Add(line);
  end;
end;

function TMostSimilars.GetMostSimilar(x: integer; y: integer; z: integer)
  : TMostSimilar;
begin
  x := EdgePolicyFor.RepeatedImage(x, FWidth);
  y := EdgePolicyFor.RepeatedImage(y, FHeight);
  result := FData[x + y * FWidth + z * FWidth * FHeight];
end;

procedure TMostSimilars.Load(aStream: TStream);
begin
  aStream.Read(FWidth, SizeOf(Integer));
  aStream.Read(FHeight, SizeOf(Integer));
  aStream.Read(FLevels, SizeOf(Integer));
  SetLength(FData, FWidth*FHeight*FLevels);
  if Length(FData) > 0 then
    aStream.Read(FData[0], Length(FData)*SizeOf(TMostSimilar));
end;

procedure TMostSimilars.Save(aStream: TStream);
begin
  aStream.Write(FWidth, SizeOf(Integer));
  aStream.Write(FHeight, SizeOf(Integer));
  aStream.Write(FLevels, SizeOf(Integer));
  if Length(FData) > 0 then
    aStream.Write(FData[0], Length(FData)*SizeOf(TMostSimilar));
end;

procedure TMostSimilars.SetMostSimilar(x, y, z: integer;
  const AMostSimilar: TMostSimilar);
begin
  x := EdgePolicyFor.RepeatedImage(x, FWidth);
  y := EdgePolicyFor.RepeatedImage(y, FHeight);
  FData[x + y * FWidth + z * FWidth * FHeight] := AMostSimilar;
end;

{$ENDREGION}
{$REGION 'TFloatImage'}


constructor TFloatImage.Create(aWidth: integer; aHeight: integer);
begin
  FWidth := aWidth;
  FHeight := aHeight;
  SetLength(FData, aWidth * aHeight);
end;

procedure TFloatImage.AssignFromImage(const aDesc: TImageDesc);
var
  i, r, b: integer;
  step: integer;
  p: PByte;
begin
  Assert(aDesc.Data <> nil);
  Assert((aDesc.ColorFormat = GL_RGB) or (aDesc.ColorFormat = GL_RGBA)
    or (aDesc.ColorFormat = GL_BGR) or (aDesc.ColorFormat = GL_BGRA));
  Assert(aDesc.DataType = GL_UNSIGNED_BYTE);

  FWidth := aDesc.Width;
  FHeight := aDesc.Height;
  SetLength(FData, FWidth * FHeight);
  r := 0;
  b := 2;
  step := 3;
  case aDesc.ColorFormat of
    GL_RGBA: step := 4;
    GL_BGR: begin
        r := 2;
        b := 0;
      end;
    GL_BGRA:
      begin
        r := 2;
        b := 0;
        step := 4;
      end;
  end;
  p := aDesc.Data;

  for i := 0 to High(FData) do
  begin
    FData[i].r := p[r];
    FData[i].g := p[1];
    FData[i].b := p[b];
    Inc(p, step);
  end;
end;

function TFloatImage.GetImage: TImageDesc;
var
  i: integer;
  p: PByte;
begin
  FillChar(result, SizeOf(TImageDesc), $00);

  result.Width := FWidth;
  result.Height := FHeight;
  result.InternalFormat := GL_RGB8;
  result.ColorFormat := GL_RGB;
  result.DataType := GL_UNSIGNED_BYTE;
  result.ElementSize := 3;
  result.DataSize := FWidth * FHeight * 3;
  GetMem(result.Data, result.DataSize);
  p := result.Data;

  for i := 0 to High(FData) do
  begin
    p[0] := Floor(FData[i].r);
    p[1] := Floor(FData[i].g);
    p[2] := Floor(FData[i].b);
    Inc(p, 3);
  end;
end;

procedure TFloatImage.Clear(cR: single; cG: single; cB: single);
var
  i: integer;
  pf: TFloatPixel;
begin
  pf.r := cR;
  pf.g := cG;
  pf.b := cB;

  for i := 0 to High(FData) do
      FData[i] := pf;
end;

function TFloatImage.GetPixel(x: integer; y: integer): TFloatPixel;
begin
  if (x >= 0) and (x < FWidth) and (y >= 0) and (y < FHeight) then
      Exit(FData[x + y * FWidth])
  else
      result := ZERO_PIXEL;
end;

procedure TFloatImage.Load(aStream: TStream);
begin
  aStream.Read(FWidth, SizeOf(Integer));
  aStream.Read(FHeight, SizeOf(Integer));
  SetLength(FData, FWidth*FHeight);
  if Length(FData) > 0 then
    aStream.Read(FData[0], Length(FData)*SizeOf(TFloatPixel));
end;

procedure TFloatImage.PutPixel(x: integer; y: integer;
  const aValue: TFloatPixel);
begin
  if (x >= 0) and (x < FWidth) and (y >= 0) and (y < FHeight) then
      FData[x + y * FWidth] := aValue;
end;

procedure TFloatImage.Save(aStream: TStream);
begin
  aStream.Write(FWidth, SizeOf(Integer));
  aStream.Write(FHeight, SizeOf(Integer));
  if Length(FData) > 0 then
    aStream.Write(FData[0], Length(FData)*SizeOf(TFloatPixel));
end;

function TFloatImage.BilinearWrap(u, v: single): TFloatPixel;
var
  i, j: single;
  i0, i1, j0, j1: integer;
  fi, fj: single;
  pix: array [0 .. 3] of TFloatPixel;

begin
  // translate into "Array coordinate space"
  j := u * FWidth - 0.5;
  i := v * FHeight - 0.5;

  // get corners of interpolation cell
  j0 := Floor(j);
  j1 := j0 + 1;
  i0 := Floor(i);
  i1 := i0 + 1;

  // apply access policy
  j0 := EdgePolicyFor.RepeatedImage(j0, FWidth);
  j1 := EdgePolicyFor.RepeatedImage(j1, FWidth);
  i0 := EdgePolicyFor.RepeatedImage(i0, FHeight);
  i1 := EdgePolicyFor.RepeatedImage(i1, FHeight);

  // compute interpolation fractions
  fj := (j - Floor(j));
  fi := (i - Floor(i));

  // interpolate each component and return
  pix[0] := Pixel[j0, i0];
  pix[1] := Pixel[j0, i1];
  pix[2] := Pixel[j1, i0];
  pix[3] := Pixel[j1, i1];

  result.r := (1.0 - fj) * ((1.0 - fi) * pix[0].r
    + (fi) * pix[1].r)
    + (fj) * ((1.0 - fi) * pix[2].r
    + (fi) * pix[3].r);
  result.g := (1.0 - fj) * ((1.0 - fi) * pix[0].g
    + (fi) * pix[1].g)
    + (fj) * ((1.0 - fi) * pix[2].g
    + (fi) * pix[3].g);
  result.b := (1.0 - fj) * ((1.0 - fi) * pix[0].b
    + (fi) * pix[1].b)
    + (fj) * ((1.0 - fi) * pix[2].b
    + (fi) * pix[3].b);
end;

{$ENDREGION}
{$REGION 'TImagePyramid'}


constructor TImagePyramid.Create(aWidth, aHeight: integer);
var
  LevelsNumber, l: integer;
  n: integer;
  freq: single;
  downFactor: integer;
begin
  LevelsNumber := Round(ln(TMath.Min(aWidth, aHeight)) / ln(2)) + 1;
  SetLength(FLevels, LevelsNumber);

  for l := 0 to LevelsNumber - 1 do
  begin
    downFactor := 1 shl l;
    FLevels[l] := TFloatImage.Create(aWidth div downFactor,
      aHeight div downFactor);
  end;

  FFilterSize := 4;
  SetLength(FWeights, FFilterSize);
  freq := 2.0 / FFilterSize;
  for n := 0 to High(FWeights) do
      FWeights[n] := GaussFW1((n - FFilterSize / 2.0 + 0.5) * freq / 1.1);
end;

destructor TImagePyramid.Destroy;
var
  l: integer;
begin
  for l := 0 to High(FLevels) do
      FLevels[l].Destroy;
  SetLength(FLevels, 0);
end;

function TImagePyramid.GetLevelsAmount: integer;
begin
  result := Length(FLevels);
end;

function TImagePyramid.GetLevel(level: integer): TFloatImage;
begin
  result := FLevels[level];
end;

procedure TImagePyramid.AssignFromImage(const aDesc: TImageDesc;
  alevel: integer);
begin
  Assert((alevel >= Low(FLevels)) and (alevel <= High(FLevels)),
    'Invalid pyramid''s level');
  FLevels[alevel].AssignFromImage(aDesc);
end;

function TImagePyramid.GaussFW1(x: double): double;
var
  w, s, fract: double;
begin
  w := 1.0; // full width at half height should be 1.
  s := w / (2.0 * sqrt(2.0 * ln(2.0)));
  fract := 1.0 / (s * sqrt(2.0 * PI));
  result := fract * exp(-x * x / (2.0 * s * s));
end;

procedure TImagePyramid.GeneratePyramid;
var
  l: integer;
begin
  for l := 1 to High(FLevels) do GaussianFilter(FLevels[l - 1], FLevels[l], 2);
end;

procedure TImagePyramid.GaussianFilter(src: TFloatImage; dst: TFloatImage;
  downSampleFactor: integer);
var
  n, x, y, i, s: integer;
  sum, Pixel: TFloatPixel;
  sumWeight: single;
  tmp: TFloatImage;
begin

  s := -Length(FWeights) div 2 + 1 - Length(FWeights) and 1;

  tmp := TFloatImage.Create(src.Width div downSampleFactor, src.Height);

  for y := 0 to src.Height - 1 do
    for x := 0 to src.Width - 1 do
    begin
      sum.r := 0;
      sum.g := 0;
      sum.b := 0;
      sumWeight := 0;
      for n := 0 to High(FWeights) do
      begin
        i := x * downSampleFactor + n + s;
        if (i >= 0) and (i < src.Width) then
        begin
          Pixel := src.Pixel[i, y];
          sum.r := sum.r + Pixel.r * FWeights[n];
          sum.g := sum.g + Pixel.g * FWeights[n];
          sum.b := sum.b + Pixel.b * FWeights[n];
          sumWeight := sumWeight + FWeights[n];
        end;
      end;
      Pixel.r := sum.r / sumWeight;
      Pixel.g := sum.g / sumWeight;
      Pixel.b := sum.b / sumWeight;
      tmp.Pixel[x, y] := Pixel;
    end;

  for y := 0 to tmp.Height - 1 do
    for x := 0 to tmp.Width - 1 do
    begin
      sum.r := 0;
      sum.g := 0;
      sum.b := 0;
      sumWeight := 0;
      for n := 0 to High(FWeights) do
      begin
        i := y * downSampleFactor + n + s;
        if (i >= 0) and (i < tmp.Height) then
        begin
          Pixel := tmp.Pixel[x, i];
          sum.r := sum.r + Pixel.r * FWeights[n];
          sum.g := sum.g + Pixel.g * FWeights[n];
          sum.b := sum.b + Pixel.b * FWeights[n];
          sumWeight := sumWeight + FWeights[n];
        end;
      end;
      Pixel.r := sum.r / sumWeight;
      Pixel.g := sum.g / sumWeight;
      Pixel.b := sum.b / sumWeight;
      dst.Pixel[x, y] := Pixel;
    end;

  tmp.Free;
end;
{$ENDREGION}
{$REGION 'TAnalysisData'}


constructor TAnalysisData.Create;
begin
  FEdgePolicy := epRepeat;
  FEdgeFunc := EdgePolicyFor.RepeatedImage;
  FNEdgeFunc := EdgePolicyFor.RepeatedImage;
end;

procedure TAnalysisData.CreateImageStack(aPyramid: TImagePyramid);
var
  l, i, j, w, h: integer;
  fi, fj: single;
begin
  Clear;
  SetLength(FLevels, aPyramid.LevelsAmount);
  w := aPyramid.level[0].Width;
  h := aPyramid.level[0].Height;
  for l := 0 to aPyramid.LevelsAmount - 1 do
    with FLevels[l] do
    begin
      LevelId := l;
      FloatImage := TFloatImage.Create(w, h);
      for i := 0 to h - 1 do
        for j := 0 to w - 1 do
        begin
          fj := (j + 0.5) / w;
          fi := (i + 0.5) / h;
          FloatImage.Pixel[j, i] := aPyramid.level[l].BilinearWrap(fj, fi);
        end;
      Image := FloatImage.GetImage;
    end;
  FkNearests := TMostSimilars.Create(w, h, aPyramid.LevelsAmount);
  FNeighborhoods := TNeighborhoods.Create(w, h, aPyramid.LevelsAmount);
end;

procedure TAnalysisData.Clear;
var
  l: integer;
begin
  for l := 0 to High(FLevels) do
    with FLevels[l] do
    begin
      FloatImage.Free;
      Image.Free;
      ProjectedImage.Free;
      kNearest.Free;
      Neighborhoods[0].Free;
      Neighborhoods[1].Free;
    end;
  SetLength(FLevels, 0);
  FreeAndNil(FkNearests);
  FreeAndNil(FNeighborhoods);
  FValid := False;
end;

destructor TAnalysisData.Destroy;
begin
  Clear;
  FExemplar.Free;
end;

function TAnalysisData.GetLevel(alevel: integer): PAnalyzedLevel;
begin
  result := @FLevels[alevel];
end;

function TAnalysisData.GatherNeighborhood(x, y, z: integer): TNeighborhood3c;
var
  img: PImageDesc;
  w, h, e, i, j, nj, ni, spacing: integer;
  At: integer;
  p: PByte;
begin
  // Gather a neighborhood within the stack. Note that contrary to neighborhoods
  // in a regular image, neighbors are not next to each others in the stack but
  // separated by a level-dependent offset.
  img := GetImage(z);
  spacing := 1 shl z;
  w := img.Width;
  h := img.Height;
  e := img.ElementSize;
  At := 0;
  for ni := -HALF_NEIGHBOUR_DIM to HALF_NEIGHBOUR_DIM do
    for nj := -HALF_NEIGHBOUR_DIM to HALF_NEIGHBOUR_DIM do
    begin
      j := FNEdgeFunc(x + nj * spacing, w);
      i := FNEdgeFunc(y + ni * spacing, h);
      p := img.Data;
      Inc(p, (j + i * w) * e);
      result[At + 0] := p[0];
      result[At + 1] := p[1];
      result[At + 2] := p[2];
      Inc(At, 3);
    end;
end;

function TAnalysisData.GetExemplar: PImageDesc;
begin
  Result := @FExemplar;
end;

function TAnalysisData.GetFloatImage(level: integer): TFloatImage;
begin
  result := FLevels[level].FloatImage;
end;

function TAnalysisData.GetLevelsAmount: integer;
begin
  result := Length(FLevels);
end;

procedure TAnalysisData.LoadFromFile(const aFileName: string);
var
  stream: TFileStream;
  I: integer;
{$IFDEF DEBUG}
  J, L: integer;
{$ENDIF}
begin
  Clear;
  stream := TFileStream.Create(aFileName, 0);
  try
    stream.Read(i, SizeOf(integer));
    Assert(i = 0); // Check version
    if i > 0 then
      Exit;

    FExemplar.Load(stream);
    stream.Read(i, SizeOf(integer));
    SetLength(FLevels, i);
    for i := 0 to High(FLevels) do
    with FLevels[i] do
    begin
      LevelId := i;
      FloatImage := TFloatImage.Create(0, 0);
      Image.Load(stream);
      ProjectedImage.Load(stream);
      stream.Read(ColorScale, SizeOf(TVector));
      stream.Read(ColorOffset, SizeOf(TVector));
      stream.Read(ColorPCAMatrix, SizeOf(TColorPCAMatrix));
      stream.Read(NeihgbPCAMatrix, SizeOf(TNeighbPCAmatrix));
      kNearest.Load(stream);
      Neighborhoods[0].Load(stream);
      Neighborhoods[1].Load(stream);
      stream.Read(NeighbScale, SizeOf(TVector6f));
      stream.Read(NeighbOffset, SizeOf(TVector6f));
    end;
    stream.Read(I, SizeOf(integer));
    SetEdgePolicy(TEdgePolicy(I));
    FkNearests := TMostSimilars.Create(0, 0, 0);
    FkNearests.Load(stream);
    FNeighborhoods := TNeighborhoods.Create(0, 0, 0);
    FNeighborhoods.Load(stream);
  finally
    stream.Free;
  end;

{$IFDEF DEBUG}
  for L := 0 to High(FLevels) do
    for i := 0 to Exemplar.Height - 1 do
      for j := 0 to Exemplar.Width - 1 do
        Neighborhoods.At[j, i, L] :=
          GatherNeighborhood(j, i, L);
{$ENDIF}

  FValid := True;
end;

procedure TAnalysisData.SaveToFile(const aFileName: string);
var
  stream: TFileStream;
  I: integer;
begin
  stream := TFileStream.Create(aFileName, fmCreate);
  try
    i := 0; // Version
    stream.Write(i, SizeOf(integer));

    FExemplar.Save(stream);
    i := Length(FLevels);
    stream.Write(i, SizeOf(integer));
    for i := 0 to High(FLevels) do
    with FLevels[i] do
    begin
      Image.Save(stream);
      ProjectedImage.Save(stream);
      stream.Write(ColorScale, SizeOf(TVector));
      stream.Write(ColorOffset, SizeOf(TVector));
      stream.Write(ColorPCAMatrix, SizeOf(TColorPCAMatrix));
      stream.Write(NeihgbPCAMatrix, SizeOf(TNeighbPCAmatrix));
      kNearest.Save(stream);
      Neighborhoods[0].Save(stream);
      Neighborhoods[1].Save(stream);
      stream.Write(NeighbScale, SizeOf(TVector6f));
      stream.Write(NeighbOffset, SizeOf(TVector6f));
    end;
    I := integer(FEdgePolicy);
    stream.Write(I, SizeOf(integer));
    FkNearests.Save(stream);
    FNeighborhoods.Save(stream);
  finally
    stream.Free;
  end;
end;

procedure TAnalysisData.SetEdgePolicy(const Value: TEdgePolicy);
begin
  FEdgePolicy := Value;
  FEdgeFunc := EdgePolicyFor.GetFunc(Value);
  FNEdgeFunc := EdgePolicyFor.GetFuncN(Value);
end;

procedure TAnalysisData.SetExemplar(const Value: TImageDesc);
var
  i: integer;
  tmp: Byte;
  p: PByte;
begin
  Assert(Value.Data <> nil);
  Assert((Value.ColorFormat = GL_RGB) or (Value.ColorFormat = GL_RGBA)
    or (Value.ColorFormat = GL_BGR) or (Value.ColorFormat = GL_BGRA));
  Assert(Value.DataType = GL_UNSIGNED_BYTE);

  FExemplar.Free;
  FExemplar := Value;
  if Value.DataSize > 0 then
  with FExemplar do
  begin
    GetMem(Data, DataSize);
    Move(PByte(Value.Data)^, PByte(Data)^, DataSize);

    if (ColorFormat = GL_BGR) or
      (ColorFormat = GL_BGRA) then
    begin
      p :=  Data;
      for i := 0 to DataSize div ElementSize - 1 do
      begin
        tmp := p[0];
        p[0] := p[2];
        p[2] := tmp;
        Inc(p, ElementSize);
      end;
      if ColorFormat = GL_BGR then
        ColorFormat := GL_RGB
      else
        ColorFormat := GL_RGBA;
    end;
  end;

  FValid := False;
end;

function TAnalysisData.GetImage(alevel: integer): PImageDesc;
begin
  result := @FLevels[alevel].Image;
end;

{$ENDREGION}

end.
