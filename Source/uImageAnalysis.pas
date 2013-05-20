unit uImageAnalysis;

interface

uses
  Classes, uBaseTypes, uVMath;

{.$IFDEF PACKED_EXEMPLAR_RG}

const
  NEIGHBOUR_DIM = 5;
  NEIGHBOUR_SIZE_2COLOR = 2 * NEIGHBOUR_DIM * NEIGHBOUR_DIM;
  NEIGHBOUR_SIZE_3COLOR = 3 * NEIGHBOUR_DIM * NEIGHBOUR_DIM;
  SIMILAR_NEIGHBOUR_SIZE = 8;

const
  GL_RG = $8227;
  GL_RGB = $1907;
  GL_RGBA = $1908;
  GL_RG8 = $822B;
  GL_RGB8 = $8051;
  GL_UNSIGNED_BYTE = $1401;

type
  TEdgePolicy = (epWrap, epClamp, epMirror);
  TEdgePolicyFunc = function(aCoord, aSize: integer): integer of object;

  IVec2 = array [0 .. 1] of integer;
  TNeighborhood2c = array [0 .. NEIGHBOUR_SIZE_2COLOR - 1] of single;
  TNeighborhood3c = array [0 .. NEIGHBOUR_SIZE_3COLOR - 1] of single;
  TMostSimilar = array [0 .. SIMILAR_NEIGHBOUR_SIZE - 1] of IVec2;
  TVector6f = array [0 .. 5] of single;

  TColorPCAMatrix = array [0 .. 2, 0 .. 1] of single;
  TNeighbPCAmatrix = array [0 .. NEIGHBOUR_SIZE_2COLOR - 1, 0 .. 5] of single;

  TIVec2Array2D = class
  private
    FData: array of IVec2;
    FWidth, FHeight: integer;
    FWidthAccess, FHeightAccess: TEdgePolicy;
    FWidthAccessFunc, FHeightAccessFunc: TEdgePolicyFunc;
    function GetItem(x, y: integer): IVec2;
    procedure SetItem(x, y: integer; const aValue: IVec2);
    procedure SetWidthAccessPolisy(ap: TEdgePolicy);
    procedure SetHeightAccessPolisy(ap: TEdgePolicy);
  public
    constructor Create(w, h: integer);

    procedure Clear(const aClearValue: IVec2);
    procedure Assign(source: TIVec2Array2D);

    class function WrapAccess(c, size: integer): integer;
    class function ClampAccess(c, size: integer): integer;
    class function MirrorAccess(c, size: integer): integer;

    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property WidthAccessPolisy: TEdgePolicy read FWidthAccess
      write SetWidthAccessPolisy;
    property HeightAccessPolisy: TEdgePolicy read FHeightAccess
      write SetHeightAccessPolisy;
    property At[x, y: integer]: IVec2 read GetItem write SetItem; default;
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
    // Using principal component analysis (PCA)
    // projecting pixel neighborhoods into a lower-dimensional space (6D vector)
    procedure ProjectTo6D(alevel: integer; const CPCA: TColorPCAMatrix;
      var NPCA: TNeighbPCAmatrix);
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
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property Levels: integer read FLevels;
    property At[x, y, z: integer]: TMostSimilar read GetMostSimilar
      write SetMostSimilar; default;
  end;

  TFloatPixel = record
    r, g, b: single;
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
    procedure CreatePyramid;

    property LevelsAmount: integer read GetLevelsAmount;
    property level[level: integer]: TFloatImage read GetLevel;
  end;

  TImageStack = class
  private
    FFloatImages: array of TFloatImage;
    FImages: array of TImageDesc;
    FProjectedImages: array of TImageDesc;
    function GetLevelsAmount: integer;
    function GetImage(alevel: integer): PImageDesc;
    function GetLevel(level: integer): TFloatImage;
    function GetPackedImage(alevel: integer): PImageDesc;
  public
    constructor Create(aPyramid: TImagePyramid);
    destructor Destroy; override;

    property FloatImages[level: integer]: TFloatImage read GetLevel;
    property Images[level: integer]: PImageDesc read GetImage;
    property PackedImages[level: integer]: PImageDesc read GetPackedImage;
    property LevelsAmount: integer read GetLevelsAmount;
  end;

  TAnalyzerThread = class;

  TAnalyzer = class
  private
    procedure SetMaxCPUThreads(const Value: integer);
  protected
    FAccessFunc: TEdgePolicyFunc;
    // Exemplar image
    FExemplar: TImageDesc;
    // True if the exemplar is toroidaly repeated
    FToroidal: boolean;
    // Exemplar stack, computed from the image
    FStack: TImageStack;
    // k-most similar neighborhoods within same exemplar stack level
    FkNearests: TMostSimilars;
    // All neighborhoods (pre-gathered for efficiency)
    FNeighborhoods: TNeighborhoods;
    // Threads
    FThreads: array of TAnalyzerThread;
    FMaxCPUThreads: integer;
    // Gathers all neighborhoods of the exemplar stack level
    procedure GatherNeighborhoods(alevel: TAnalyzerThread);
    // Gathers neighborhood at i,j in the stack level
    function GatherNeighborhood(i, j: integer; alevel: TAnalyzerThread)
      : TNeighborhood3c;
    procedure ProjectStackLevelTo2D(alevel: TAnalyzerThread);
    procedure SetToroidality(flag: boolean);
    function GetDone: boolean;
    procedure SetExemplar(const aDesc: TImageDesc);
    function GetStackLevel(alevel: integer): TImageDesc;
    function GetPCAStackLevel(alevel: integer): TImageDesc;
    function GetColorPCAmatrix(alevel: integer): TColorPCAMatrix;
    function GetNeighbPCAmatrix(alevel: integer): TNeighbPCAmatrix;
    function GetColorScale(alevel: integer): Vec2;
    function GetColorOffset(alevel: integer): Vec2;
    function GetLevelCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    // Runs analysis.
    procedure Start;
    procedure Process;
    procedure Stop;
    property Done: boolean read GetDone;

    // Analyzes one exemplar stack level
    procedure AnalyzeStackLevel(aLevel: integer);
    // Returns the neighborhood at j,i in the stack level l.
    // This is using pre-gathered neighborhoods.
    // It is meant to be called after analysis, during synthesis.
    property Neighborhoods: TNeighborhoods read FNeighborhoods;
    property kNearests: TMostSimilars read FkNearests;
    property Exemplar: TImageDesc read FExemplar write SetExemplar;
    property GaussianStackLevel[level: integer]: TImageDesc read GetStackLevel;
    property ProjectedStackLevel[level: integer]: TImageDesc
      read GetPCAStackLevel;
    property ColorPCAmatrix[level: integer]: TColorPCAMatrix
      read GetColorPCAmatrix;
    property NeighbPCAmatrix[level: integer]: TNeighbPCAmatrix
      read GetNeighbPCAmatrix;
    property ColorScale[level: integer]: Vec2 read GetColorScale;
    property ColorOffset[level: integer]: Vec2 read GetColorOffset;
    property LevelCount: integer read GetLevelCount;
    property Toroidality: boolean read FToroidal write SetToroidality;
    property MaxCPUThreads: integer read FMaxCPUThreads write SetMaxCPUThreads;
  end;

  TAnalyzerThread = class(TThread)
  private
    FAnalyzer: TAnalyzer;
    FLevel: integer;
    FExemplar: PImageDesc;
    FPCAexemplar: PImageDesc;
    FColorScale: Vec2;
    FColorOffset: Vec2;
    FColorPCAMatrix: TColorPCAMatrix;
    FNeihgbPCAMatrix: TNeighbPCAmatrix;
    FKNearest: TImageDesc;
    FNeighborhoods: TImageDesc;
    FNeighbScale: TVector6f;
    FNeighbOffset: TVector6f;
  protected
    constructor CreateOwned(aOwner: TAnalyzer; alevel: integer);
    procedure Execute; override;

    property Exemplar: PImageDesc read FExemplar;
    property PCAexemplar: PImageDesc read FPCAexemplar;
    property ColorScale: Vec2 read FColorScale write FColorScale;
    property ColorOffset: Vec2 read FColorOffset write FColorOffset;
    property ColorPCAmatrix: TColorPCAMatrix read FColorPCAMatrix;
    property NeihgbPCAMatrix: TNeighbPCAmatrix read FNeihgbPCAMatrix;
    property KNearest: TImageDesc read FKNearest;
    property Neighborhoods: TImageDesc read FNeighborhoods;
    property NeighbScale: TVector6f read FNeighbScale;
    property NeighbOffset: TVector6f read FNeighbOffset;
  end;

implementation

uses
  uMath, uMathPCA, Math, uNeighborSearching, uMiscUtils;

const
  INV255 = 1.0 / 255.0;
  ZERO_PIXEL: TFloatPixel = (r: 0; g: 0; b: 0);

{$REGION 'TIVec2Array2D'}


class function TIVec2Array2D.WrapAccess(c, size: integer): integer;
begin
  result := c mod size;
  if result < 0 then
      result := result + size;
end;

class function TIVec2Array2D.ClampAccess(c, size: integer): integer;
begin
  if c < 0 then
      result := 0
  else if c >= size then
      result := size - 1
  else
      result := c;
end;

class function TIVec2Array2D.MirrorAccess(c, size: integer): integer;
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

constructor TIVec2Array2D.Create(w: integer; h: integer);
begin
  FWidthAccess := epWrap;
  FHeightAccess := epWrap;
  FWidthAccessFunc := WrapAccess;
  FHeightAccessFunc := WrapAccess;
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
  FData := Copy(source.FData, 0, Length(source.FData));
end;

procedure TIVec2Array2D.SetWidthAccessPolisy(ap: TEdgePolicy);
begin
  if ap <> FWidthAccess then
  begin
    FWidthAccess := ap;
    case FWidthAccess of
      epWrap:
        FWidthAccessFunc := WrapAccess;
      epClamp:
        FWidthAccessFunc := ClampAccess;
      epMirror:
        FWidthAccessFunc := MirrorAccess;
    end;
  end;
end;

procedure TIVec2Array2D.SetHeightAccessPolisy(ap: TEdgePolicy);
begin
  if ap <> FHeightAccess then
  begin
    FHeightAccess := ap;
    case FHeightAccess of
      epWrap:
        FHeightAccessFunc := WrapAccess;
      epClamp:
        FHeightAccessFunc := ClampAccess;
      epMirror:
        FHeightAccessFunc := MirrorAccess;
    end;
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

function TNeighborhoods.GetNeighb(x: integer; y: integer; z: integer)
  : TNeighborhood3c;
begin
  x := TIVec2Array2D.WrapAccess(x, FWidth);
  y := TIVec2Array2D.WrapAccess(y, FHeight);
  result := FData[x + y * FWidth + z * FWidth * FHeight];
end;

procedure TNeighborhoods.SetNeighb(x: integer; y: integer; z: integer;
  const ANeighborhood: TNeighborhood3c);
begin
  x := TIVec2Array2D.WrapAccess(x, FWidth);
  y := TIVec2Array2D.WrapAccess(y, FHeight);
  FData[x + y * FWidth + z * FWidth * FHeight] := ANeighborhood;
end;

function TNeighborhoods.GetNeighb6D(x: integer; y: integer; z: integer)
  : TVector6f;
begin
  x := TIVec2Array2D.WrapAccess(x, FWidth);
  y := TIVec2Array2D.WrapAccess(y, FHeight);
  result := FProjData[x + y * FWidth + z * FWidth * FHeight]
end;

procedure TNeighborhoods.SetNeighb6D(x: integer; y: integer; z: integer;
  const ANeighborhood: TVector6f);
begin
  x := TIVec2Array2D.WrapAccess(x, FWidth);
  y := TIVec2Array2D.WrapAccess(y, FHeight);
  FProjData[x + y * FWidth + z * FWidth * FHeight] := ANeighborhood;
end;

procedure TNeighborhoods.ProjectTo6D(alevel: integer;
  const CPCA: TColorPCAMatrix;
  var NPCA: TNeighbPCAmatrix);
var
  x: TDouble2DArray;
  Dispersion: TDouble1DArray;
  PCA: TDouble2DArray;
  Info: integer;
  i, j, mi, mj, row: integer;
  V6D: TVector6f;
  Neighb: TNeighborhood3c;
  clr: single;
begin
  Assert(alevel < FLevels);
  SetLength(x, FWidth * FHeight, NEIGHBOUR_SIZE_2COLOR);
  row := 0;
  // fill array for principal component analysis
  // and simultaneously project neighborhoods color to 2D
  for i := 0 to FHeight - 1 do
    for j := 0 to FWidth - 1 do
    begin
      Neighb := GetNeighb(j, i, aLevel);
      for mj := 0 to NEIGHBOUR_SIZE_2COLOR div 2 - 1 do
      begin
        x[row][mj * 2 + 0] := CPCA[0, 0] * Neighb[mj * 3 + 0];
        x[row][mj * 2 + 0] := x[row][mj * 2 + 0] + CPCA[1, 0] *
          Neighb[mj * 3 + 1];
        x[row][mj * 2 + 0] := x[row][mj * 2 + 0] + CPCA[2, 0] *
          Neighb[mj * 3 + 2];
        x[row][mj * 2 + 1] := CPCA[0, 1] * Neighb[mj * 3 + 0];
        x[row][mj * 2 + 1] := x[row][mj * 2 + 1] + CPCA[1, 1] *
          Neighb[mj * 3 + 1];
        x[row][mj * 2 + 1] := x[row][mj * 2 + 1] + CPCA[2, 1] *
          Neighb[mj * 3 + 2];
      end;
      Inc(row);
    end;
  // run principal component analysis
  PrincipalComponentsAnalysis.BuildBasis(x, FWidth * FHeight,
    2 * NEIGHBOUR_SIZE_3COLOR div 3, Info, Dispersion, PCA);
  // store only 6 basis
  if Info = 1 then
    for i := 0 to 5 do
      for j := 0 to NEIGHBOUR_SIZE_2COLOR - 1 do
        NPCA[j, i] := PCA[j, i];

  // store lower dimension vector to 2D array
  for i := 0 to FHeight - 1 do
    for j := 0 to FWidth - 1 do
    begin
      Neighb := GetNeighb(j, i, aLevel);
      V6D[0] := 0;
      V6D[1] := 0;
      V6D[2] := 0;
      V6D[3] := 0;
      V6D[4] := 0;
      V6D[5] := 0;
      for mi := 0 to 5 do
        for mj := 0 to NEIGHBOUR_SIZE_2COLOR div 2 - 1 do
        begin
          clr := CPCA[0, 0] * Neighb[3 * mj + 0] + CPCA[1, 0] *
            Neighb[3 * mj + 1] + CPCA[2, 0] * Neighb[3 * mj + 2];
          V6D[mi] := V6D[mi] + NPCA[2 * mj + 0, mi] * clr;

          clr := CPCA[0, 1] * Neighb[3 * mj + 0] + CPCA[1, 1] *
            Neighb[3 * mj + 1] + CPCA[2, 1] * Neighb[3 * mj + 2];
          V6D[mi] := V6D[mi] + NPCA[2 * mj + 1, mi] * clr;
        end;

      SetNeighb6D(j, i, aLevel, V6D);
    end;
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

function TMostSimilars.GetMostSimilar(x: integer; y: integer; z: integer)
  : TMostSimilar;
begin
  x := TIVec2Array2D.WrapAccess(x, FWidth);
  y := TIVec2Array2D.WrapAccess(y, FHeight);
  result := FData[x + y * FWidth + z * FWidth * FHeight];
end;

procedure TMostSimilars.SetMostSimilar(x, y, z: integer;
  const AMostSimilar: TMostSimilar);
begin
  x := TIVec2Array2D.WrapAccess(x, FWidth);
  y := TIVec2Array2D.WrapAccess(y, FHeight);
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
  i: integer;
  step: integer;
  p: PByte;
begin
  Assert(aDesc.Data <> nil);
  Assert((aDesc.ColorFormat = GL_RGB) or (aDesc.ColorFormat = GL_RGBA));
  Assert(aDesc.DataType = GL_UNSIGNED_BYTE);

  FWidth := aDesc.Width;
  FHeight := aDesc.Height;
  SetLength(FData, FWidth * FHeight);
  if aDesc.ColorFormat = GL_RGB then
      step := 3
  else
      step := 4;
  p := aDesc.Data;

  for i := 0 to High(FData) do
  begin
    FData[i].r := INV255 * p[0];
    FData[i].g := INV255 * p[1];
    FData[i].b := INV255 * p[2];
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
    p^ := Floor(255 * FData[i].r);
    Inc(p);
    p^ := Floor(255 * FData[i].g);
    Inc(p);
    p^ := Floor(255 * FData[i].b);
    Inc(p);
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

procedure TFloatImage.PutPixel(x: integer; y: integer;
  const aValue: TFloatPixel);
begin
  if (x >= 0) and (x < FWidth) and (y >= 0) and (y < FHeight) then
      FData[x + y * FWidth] := aValue;
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
  j0 := TIVec2Array2D.WrapAccess(j0, FWidth);
  j1 := TIVec2Array2D.WrapAccess(j1, FWidth);
  i0 := TIVec2Array2D.WrapAccess(i0, FHeight);
  i1 := TIVec2Array2D.WrapAccess(i1, FHeight);

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

procedure TImagePyramid.CreatePyramid;
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
{$REGION 'TImageStack'}


constructor TImageStack.Create(aPyramid: TImagePyramid);
var
  l, i, j: integer;
  fi, fj: single;
begin
  SetLength(FFloatImages, aPyramid.LevelsAmount);
  SetLength(FImages, aPyramid.LevelsAmount);
  SetLength(FProjectedImages, aPyramid.LevelsAmount);
  for l := 0 to aPyramid.LevelsAmount - 1 do begin
    FFloatImages[l] := TFloatImage.Create(aPyramid.level[0].Width,
      aPyramid.level[0].Height);
    for i := 0 to FFloatImages[l].Height - 1 do
      for j := 0 to FFloatImages[l].Width - 1 do begin
        fj := (j + 0.5) / FFloatImages[l].Width;
        fi := (i + 0.5) / FFloatImages[l].Height;
        FFloatImages[l].Pixel[j, i] := aPyramid.level[l].BilinearWrap(fj, fi);
      end;
  end;
end;

destructor TImageStack.Destroy;
var
  l: integer;
begin
  for l := 0 to High(FFloatImages) do
  begin
    FFloatImages[l].Destroy;
    FImages[l].Free;
    FProjectedImages[l].Free;
  end;
end;

function TImageStack.GetLevel(level: integer): TFloatImage;
begin
  Result := FFloatImages[level];
end;

function TImageStack.GetLevelsAmount: integer;
begin
  Result := Length(FFloatImages);
end;

function TImageStack.GetPackedImage(alevel: integer): PImageDesc;
begin
  Result := @FProjectedImages[alevel];
end;

function TImageStack.GetImage(alevel: integer): PImageDesc;
begin
  if FImages[alevel].Data = nil then
      FImages[alevel] := FFloatImages[alevel].GetImage;
  result := @FImages[alevel];
end;

{$ENDREGION}
{$REGION 'TAnalyzer'}


procedure TAnalyzer.AnalyzeStackLevel(aLevel: integer);
var
  i, j, k, n: integer;
  ANNPoints: TANNpointArray;
  queryPt: TANNpoint; // query point
  Idx: TANNidx;
  nnIdx: TANNidxArray; // near neighbor indices
  dists: TANNdistArray; // near neighbor distances
  kdTree: TANNkd_tree; // search structure
  neigh: TNeighborhood3c;
  KNearest: TMostSimilar;
begin
  // build ANN structure
  SetLength(nnIdx, SIMILAR_NEIGHBOUR_SIZE);
  SetLength(dists, SIMILAR_NEIGHBOUR_SIZE);
  SetLength(ANNPoints, FNeighborhoods.Width * FNeighborhoods.Height);
  // fill-in points
  k := 0;
  for i := 0 to FNeighborhoods.Height - 1 do
    for j := 0 to FNeighborhoods.Width - 1 do
    begin
      neigh := FNeighborhoods.At[j, i, aLevel];
      SetLength(ANNPoints[k], NEIGHBOUR_SIZE_3COLOR);
      for n := 0 to NEIGHBOUR_SIZE_3COLOR - 1 do
          ANNPoints[k][n] := neigh[n]; // singe -> double
      Inc(k);
    end;
  // build search structure
  kdTree := TANNkd_tree.CreateFromPointArray(ANNPoints, NEIGHBOUR_SIZE_3COLOR);

  for i := 0 to FNeighborhoods.Height - 1 do
    for j := 0 to FNeighborhoods.Width - 1 do
    begin
      neigh := FNeighborhoods.At[j, i, aLevel];
      SetLength(queryPt, NEIGHBOUR_SIZE_3COLOR);
      for n := 0 to NEIGHBOUR_SIZE_3COLOR - 1 do
          queryPt[n] := neigh[n];
      // search for similar neighborhood in reference
      kdTree.annkSearch(queryPt, SIMILAR_NEIGHBOUR_SIZE, nnIdx, dists, 0.001);

      // store result
      for n := 0 to SIMILAR_NEIGHBOUR_SIZE - 1 do
      begin
        Idx := nnIdx[n];
        KNearest[n][0] := Idx mod FNeighborhoods.Width;
        KNearest[n][1] := Idx div FNeighborhoods.Width;
      end;
      FkNearests.At[j, i, aLevel] := KNearest;
    end;

  kdTree.Free;
end;

constructor TAnalyzer.Create;
begin
  FAccessFunc := TIVec2Array2D.WrapAccess;
  FMaxCPUThreads := 2;
end;

destructor TAnalyzer.Destroy;
begin
  Stop;
  FExemplar.Free;
  FStack.Free;
  FkNearests.Free;
  FNeighborhoods.Free;
  inherited;
end;

function TAnalyzer.GatherNeighborhood(i, j: integer; alevel: TAnalyzerThread)
  : TNeighborhood3c;
var
  x, y, dj, di, nj, ni, spacing: integer;
  At: integer;
  p: PByte;
begin
  // Gather a neighborhood within the stack. Note that contrary to neighborhoods
  // in a regular image, neighbors are not next to each others in the stack but
  // separated by a level-dependent offset.
  spacing := 1 shl alevel.FLevel;
  At := 0;
  for ni := 0 to NEIGHBOUR_DIM - 1 do
    for nj := 0 to NEIGHBOUR_DIM - 1 do
    begin
      dj := nj - NEIGHBOUR_DIM div 2;
      di := ni - NEIGHBOUR_DIM div 2;
      x := FAccessFunc(i + dj * spacing, alevel.FExemplar.Width);
      y := FAccessFunc(j + di * spacing, alevel.FExemplar.Height);
      p := alevel.FExemplar.Data;
      Inc(p, (x + y * alevel.FExemplar.Width) * alevel.FExemplar.ElementSize);
      result[At] := INV255 * p^;
      Inc(p);
      Inc(At);
      result[At] := INV255 * p^;
      Inc(p);
      Inc(At);
      result[At] := INV255 * p^;
      Inc(At);
    end;
end;

procedure TAnalyzer.GatherNeighborhoods(alevel: TAnalyzerThread);
var
  i, j: integer;
begin
  for i := 0 to alevel.FExemplar.Height - 1 do
    for j := 0 to alevel.FExemplar.Width - 1 do
        FNeighborhoods.At[j, i, alevel.FLevel] :=
        GatherNeighborhood(j, i, alevel);
end;

function TAnalyzer.GetColorOffset(alevel: integer): Vec2;
begin
  result := FThreads[alevel].FColorOffset;
end;

function TAnalyzer.GetColorPCAmatrix(alevel: integer): TColorPCAMatrix;
begin
  result := FThreads[alevel].FColorPCAMatrix;
end;

function TAnalyzer.GetColorScale(alevel: integer): Vec2;
begin
  result := FThreads[alevel].FColorScale;
end;

function TAnalyzer.GetDone: boolean;
var
  l: integer;
begin
  result := true;
  for l := 0 to High(FThreads) do
      result := result and FThreads[l].Finished;
end;

function TAnalyzer.GetNeighbPCAmatrix(alevel: integer): TNeighbPCAmatrix;
begin
  result := FThreads[alevel].FNeihgbPCAMatrix;
end;

function TAnalyzer.GetLevelCount: integer;
begin
  if Assigned(FStack) then
      result := FStack.LevelsAmount
  else
      result := 0;
end;

function TAnalyzer.GetPCAStackLevel(alevel: integer): TImageDesc;
begin
  if alevel < FStack.LevelsAmount then
      result := FStack.PackedImages[alevel]^;
end;

function TAnalyzer.GetStackLevel(alevel: integer): TImageDesc;
begin
  if alevel < FStack.LevelsAmount then
      result := FStack.Images[alevel]^;
end;

procedure TAnalyzer.Process;
var
  i, w: integer;
begin
  w := 0;
  for i := 0 to High(FThreads) do
    if not(FThreads[i].Suspended or FThreads[i].Finished) then
        Inc(w);

  if w < FMaxCPUThreads then
    for i := 0 to High(FThreads) do
    begin
      if FThreads[i].Suspended then
      begin
        FThreads[i].Start;
        Inc(w);
      end;
      if w >= FMaxCPUThreads then
          Exit;
    end;
end;

procedure TAnalyzer.ProjectStackLevelTo2D(alevel: TAnalyzerThread);
var
  x: TDouble2DArray;
  p: PByte;
  Dispersion: TDouble1DArray;
  PCA: TDouble2DArray;
  Info: integer;
  i, j, row: integer;
  clr: Vec2;
  C2, iterC2: PVec2;
  minValue, maxValue: Vec2;
begin
  // Fill array for principal components analysis
  SetLength(x, alevel.Exemplar.Width * alevel.Exemplar.Height, 3);
  row := 0;
  for i := 0 to alevel.Exemplar.Height - 1 do
  begin
    p := alevel.Exemplar.Data;
    Inc(p, i * alevel.Exemplar.Width * alevel.Exemplar.ElementSize);
    for j := 0 to alevel.Exemplar.Width - 1 do
    begin
      x[row][0] := INV255 * p[0];
      x[row][1] := INV255 * p[1];
      x[row][2] := INV255 * p[2];
      Inc(row);
      Inc(p, alevel.Exemplar.ElementSize);
    end;
  end;

  // Run analysis
  PrincipalComponentsAnalysis.BuildBasis(x,
    alevel.Exemplar.Width * alevel.Exemplar.Height, 3, Info, Dispersion, PCA);

  // store only 2 basis
  if Info = 1 then
    for i := 0 to 1 do
      for j := 0 to 2 do
          alevel.FColorPCAMatrix[j, i] := PCA[j, i];

  GetMem(C2, alevel.Exemplar.Width*alevel.Exemplar.Height*SizeOf(Vec2));
  minValue[0] := 1E30;
  minValue[1] := 1E30;
  maxValue[0] := -1E30;
  maxValue[1] := -1E30;

  try
    // project 3D color to 2D
    iterC2 := C2;
    for i := 0 to alevel.Exemplar.Height - 1 do
    begin
      p := alevel.Exemplar.Data;
      Inc(p, i * alevel.Exemplar.Width * alevel.Exemplar.ElementSize);
      for j := 0 to alevel.Exemplar.Width - 1 do
      begin
        // Red
        clr[0] :=
          alevel.ColorPCAmatrix[0, 0] * p[0] +
          alevel.ColorPCAmatrix[1, 0] * p[1] +
          alevel.ColorPCAmatrix[2, 0] * p[2];
        if clr[0] > maxValue[0] then
            maxValue[0] := clr[0];
        if clr[0] < minValue[0] then
            minValue[0] := clr[0];
        // Green
        clr[1] :=
          alevel.ColorPCAmatrix[0, 1] * p[0] +
          alevel.ColorPCAmatrix[1, 1] * p[1] +
          alevel.ColorPCAmatrix[2, 1] * p[2];
        if clr[1] > maxValue[1] then
            maxValue[1] := clr[1];
        if clr[1] < minValue[1] then
            minValue[1] := clr[1];

        iterC2^ := clr;
        Inc(iterC2);
        Inc(p, alevel.Exemplar.ElementSize);
      end;
    end;

    alevel.ColorOffset := minValue;
    clr[0] := maxValue[0] - minValue[0];
    clr[1] := maxValue[1] - minValue[1];
    if clr[0] = 0 then
      clr[0] := 1;
    if clr[1] = 0 then
      clr[1] := 1;
    alevel.ColorScale := clr;

    with alevel do
    begin
      FillChar(FPCAexemplar^, SizeOf(TImageDesc), $00);
      FPCAexemplar.Width := Exemplar.Width;
      FPCAexemplar.Height := Exemplar.Height;
{$IFDEF PACKED_EXEMPLAR_RG}
      FPCAexemplar.InternalFormat := GL_RG8;
      FPCAexemplar.ColorFormat := GL_RG;
      FPCAexemplar.ElementSize := 2;
{$ELSE}
      FPCAexemplar.InternalFormat := GL_RGB8;
      FPCAexemplar.ColorFormat := GL_RGB;
      FPCAexemplar.ElementSize := 3;
{$ENDIF}
      FPCAexemplar.DataType := GL_UNSIGNED_BYTE;
      FPCAexemplar.DataSize := Exemplar.Width * Exemplar.Height * FPCAexemplar.ElementSize;
      GetMem(FPCAexemplar.Data, FPCAexemplar.DataSize);
    end;

    // scale color to byte size
    iterC2 := C2;
    for i := 0 to alevel.PCAexemplar.Height - 1 do
    begin
      p := alevel.PCAexemplar.Data;
      Inc(p, i * alevel.PCAexemplar.Width * alevel.PCAexemplar.ElementSize);
      for j := 0 to alevel.PCAexemplar.Width - 1 do
      begin
        clr := iterC2^;
        p[0] := Floor(255 * (clr[0] - minValue[0]) / alevel.FColorScale[0]);
        p[1] := Floor(255 * (clr[1] - minValue[1]) / alevel.FColorScale[1]);
        Inc(p, alevel.PCAexemplar.ElementSize);
        Inc(iterC2);
      end;
    end;
  finally
    FreeMem(C2);
  end;
end;

procedure TAnalyzer.SetExemplar(const aDesc: TImageDesc);
begin
  Assert(aDesc.Data <> nil);
  Assert((aDesc.ColorFormat = GL_RGB) or (aDesc.ColorFormat = GL_RGBA));
  Assert(aDesc.DataType = GL_UNSIGNED_BYTE);

  FExemplar := aDesc;
  if FExemplar.DataSize > 0 then
  begin
    GetMem(FExemplar.Data, FExemplar.DataSize);
    Move(PByte(aDesc.Data)^, PByte(FExemplar.Data)^, FExemplar.DataSize);
  end;
end;

procedure TAnalyzer.SetMaxCPUThreads(const Value: integer);
begin
  Assert(Value > 0);
  FMaxCPUThreads := Value;
end;

procedure TAnalyzer.SetToroidality(flag: boolean);
begin
  if flag <> FToroidal then
  begin
    FToroidal := flag;
    if FToroidal then
        FAccessFunc := TIVec2Array2D.WrapAccess
    else
        FAccessFunc := TIVec2Array2D.MirrorAccess;
  end;
end;

procedure TAnalyzer.Start;
var
  pyramid: TImagePyramid;
  img: PImageDesc;
  l: integer;
begin
  Assert(Assigned(FExemplar.Data), 'TAnalyzer: Assign ');
  // delete previous result
  Stop;
  FkNearests.Free;
  FNeighborhoods.Free;

  // first create an image pyramid
  pyramid := TImagePyramid.Create(FExemplar.Width, FExemplar.Height);
  pyramid.AssignFromImage(FExemplar, 0);
  pyramid.CreatePyramid;
  // then compute the exemplar stack from the pyramid
  FStack := TImageStack.Create(pyramid);

  // Allocate neighborhoods
  img := FStack.Images[0];
  FkNearests := TMostSimilars.Create(img.Width, img.Height,
    FStack.LevelsAmount);
  FNeighborhoods := TNeighborhoods.Create(img.Width, img.Height,
    FStack.LevelsAmount);
  SetLength(FThreads, FStack.LevelsAmount);

  for l := 0 to FStack.LevelsAmount - 1 do
      FThreads[l] := TAnalyzerThread.CreateOwned(Self, l);
  pyramid.Free;
end;

procedure TAnalyzer.Stop;
var
  i: integer;
  wait: boolean;
begin
  // wait until threads finished
  repeat
    wait := true;
    for i := 0 to High(FThreads) do
        wait := wait and FThreads[i].Finished;
  until wait;

  for i := 0 to High(FThreads) do
      FThreads[i].Destroy;
  SetLength(FThreads, 0);

  FreeAndNil(FStack);
end;

{$ENDREGION}
{$REGION 'TAnalyzerThread'}


constructor TAnalyzerThread.CreateOwned(aOwner: TAnalyzer; alevel: integer);
begin
  inherited Create(true);
  FAnalyzer := aOwner;
  FLevel := alevel;
  FExemplar := aOwner.FStack.Images[FLevel];
  FPCAExemplar := aOwner.FStack.PackedImages[FLevel];
end;

procedure TAnalyzerThread.Execute;
begin
  // project 3D color exemplar stack level to 2D
  FAnalyzer.ProjectStackLevelTo2D(Self);
  // gather neighborhoods and store them for use during analysis and synthesis
  FAnalyzer.GatherNeighborhoods(Self);
  // compute lower-dimension neighborhoods
  FAnalyzer.FNeighborhoods.ProjectTo6D(FLevel, FColorPCAMatrix,
    FNeihgbPCAMatrix);

  FAnalyzer.AnalyzeStackLevel(FLevel);
end;
{$ENDREGION}

end.
