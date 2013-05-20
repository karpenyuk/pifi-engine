unit uImageAnalysisClasses;

interface

uses
  Classes, uBaseTypes, uVMath;

const
  NEIGHBOUR_DIM = 5;
  NEIGHBOUR_SIZE_2COLOR = 2 * NEIGHBOUR_DIM * NEIGHBOUR_DIM;
  NEIGHBOUR_SIZE_3COLOR = 3 * NEIGHBOUR_DIM * NEIGHBOUR_DIM;
  SIMILAR_NEIGHBOUR_SIZE = 8;
  INV255 = 1.0 / 255.0;

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

  TFloatPixel = record
    r, g, b: single;
  end;

const
  ZERO_PIXEL: TFloatPixel = (r: 0; g: 0; b: 0);

type

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
    procedure GeneratePyramid;

    property LevelsAmount: integer read GetLevelsAmount;
    property level[level: integer]: TFloatImage read GetLevel;
  end;

  TAnalyzedLevel = record
    LevelId: integer;
    FloatImage: TFloatImage;
    Image: TImageDesc;
    ProjectedImage: TImageDesc;
    ColorScale: TVector;
    ColorOffset: TVector;
    ColorPCAMatrix: TColorPCAMatrix;
    NeihgbPCAMatrix: TNeighbPCAmatrix;
    kNearest: TImageDesc;
    Neighborhoods: TImageDesc;
    NeighbScale: TVector6f;
    NeighbOffset: TVector6f;
  end;
  PAnalyzedLevel = ^TAnalyzedLevel;

  TAnalysisData = class
  private
    // Exemplar image
    FExemplar: TImageDesc;
    FLevels: array of TAnalyzedLevel;
    FToroidality: boolean;
    FkNearests: TMostSimilars;
    FNeighborhoods: TNeighborhoods;
    function GetLevelsAmount: integer;
    function GetImage(alevel: integer): PImageDesc;
    function GetFloatImage(level: integer): TFloatImage;
    function GetLevel(alevel: integer): PAnalyzedLevel;
    function GetExemplar: PImageDesc;
  public
    destructor Destroy; override;

    procedure SaveToFile(const aFileName: string);
    procedure LoadFromFile(const aFileName: string);
    procedure Clear;
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
    property Toroidality: Boolean read FToroidality write FToroidality;
  end;

implementation

uses
  Math, uMath;

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
      Neighborhoods.Free;
    end;
  SetLength(FLevels, 0);
  FkNearests.Free;
  FNeighborhoods.Free;
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
begin

end;

procedure TAnalysisData.SaveToFile(const aFileName: string);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(aFileName, fmCreate);
  try

  finally
    stream.Free;
  end;
end;

procedure TAnalysisData.SetExemplar(const Value: TImageDesc);
begin
  Assert(Value.Data <> nil);
  Assert((Value.ColorFormat = GL_RGB) or (Value.ColorFormat = GL_RGBA));
  Assert(Value.DataType = GL_UNSIGNED_BYTE);

  FExemplar := Value;
  if Value.DataSize > 0 then
  with FExemplar do
  begin
    GetMem(Data, DataSize);
    Move(PByte(Value.Data)^, PByte(Data)^, DataSize);
  end;
end;

function TAnalysisData.GetImage(alevel: integer): PImageDesc;
begin
  if FLevels[alevel].Image.Data = nil then
      FLevels[alevel].Image := FLevels[alevel].FloatImage.GetImage;
  result := @FLevels[alevel].Image;
end;

{$ENDREGION}

end.
