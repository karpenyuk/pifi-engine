{ TODO: Çàìåíèòü êëàññ èñêëþ÷åíèÿ íà ëîãèðîâàíèå, èçáàâèòñÿ îò SysUtils }

unit uVectorFont;

interface

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{.$DEFINE INLINE_ON}
{.$DEFINE GLU_DLL}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Graphics,
  uLIBFREETYPE,
  uLists,
  uGenericsRBTree,
  gluTokens,
  gluTessellator,
{$IFDEF GLU_DLL}
  dglOpenGL,
{$ENDIF}
  uRenderResource,
  uVMath;

const
  VF_FRONT_FACING = 1.0;
  VF_BACK_FACING = -1.0;
  VF_BEZIER_STEP_SIZE = 0.2;
  VF_TESS_LIST_CAPACITY = 512;

type
  TCharSet = array of WideChar;

  // TVF_BBox
  //
  TVF_BBox = record
  private
    FlowerX, FlowerY, FlowerZ, FupperX, FupperY, FupperZ: Double;
  public
    class operator Add(const a, b: TVF_BBox): TVF_BBox; {$IFDEF INLINE_ON} inline; {$ENDIF}
    procedure Null;
    procedure SetValue(lx, ly, lz, ux, uy, uz: Single); overload;
    procedure SetValue(AGlyph: FT_GlyphSlot); overload;

    procedure Move(const AVec: TGLUAffineVector);
    procedure SetDebth(depth: Single);
  end;

  // TVF_Size
  //
  TVF_Size = class(TObject)
  private
    FErr: FT_Error;
    fSize: Cardinal;
    ftSize: FT_Size;
    ftFace: FT_Face;
  public
    function CharSize(face: FT_Face; point_size, x_resolution,
      y_resolution: Cardinal): Boolean; overload;
    function CharSize: Cardinal; overload;
    function Ascender: Single;
    function Descender: Single;
    function Width: Single;
    function Height: Single;
    class function Underline: Single;
    function XPixelsPerEm: Cardinal;
    function YPixelsPerEm: Cardinal;
    property Error: FT_Error read FErr;
  end;

  TVF_EncodingList = array of FT_Encoding;

  // TVF_Face
  //
  TVF_Face = class
  private
    ftFace: FT_Face;
    FCharSize: TVF_Size;
    FNumGlyphs: Integer;
    FFontEncodingList: TVF_EncodingList;
    FErr: FT_Error;
  public
    constructor Create(const AFileName: string); overload;
    constructor Create(pBufferBytes: FT_Byte_ptr;
      bufferSizeInBytes: Cardinal); overload;
    destructor Destroy; override;

    function Attach(const AFileName: string): Boolean; overload;
    function Attach(pBufferBytes: FT_Byte_ptr; bufferSizeInBytes: Cardinal)
      : Boolean; overload;
    procedure Close;
    function Size(asize, ares: Cardinal): TVF_Size;
    function UnitsPerEM: Cardinal;
    function CharMapCount: Cardinal;
    function CharMapList: TVF_EncodingList;
    function KernAdvance(index1, index2: Cardinal): TGLUAffineVector;
    function Glyph(index: Cardinal; load_flags: FT_Int): FT_GlyphSlot;

    property GlyphCount: Integer read FNumGlyphs;
    property face: FT_Face read ftFace;
    property Error: FT_Error read FErr;
  end;

  TVF_CharToGlyphIndexMap = {$IFDEF GLS_GENERIC_PREFIX}specialize{$ENDIF}
    GRedBlackTree<Cardinal, Integer>;

  // TVF_Charmap
  //

  TVF_Charmap = class
  private
    FftEncoding: FT_Encoding;
    FftFace: FT_Face;
    FErr: FT_Error;
    FCharacterMap: TVF_CharToGlyphIndexMap;
  public
    constructor Create(AFace: TVF_Face);
    destructor Destroy; override;

    function GlyphListIndex(characterCode: Cardinal): Integer;
    function FontIndex(ACharacterCode: Cardinal): Cardinal;
    procedure InsertIndex(characterCode: Cardinal; containerIndex: Integer);
    function CharMap(encoding: FT_Encoding): Boolean;

    property encoding: FT_Encoding read FftEncoding;
    property Error: FT_Error read FErr;
  end;

  TVF_Font = class;
  TFontCache = GRedBlackTree<string, TVF_Font>;

  // VectorFontLibrary
  //
  VectorFontLibrary = class
  private
    class var FLibrary: FT_Library;
    class var FErr: FT_Error;
    class var FFontCache: TFontCache;
    class var FTesselator: PGLUTesselator;
    class procedure Initialize;
    class procedure Finalize;
  public
    class procedure BuildSystemFont(const AFontLabel, AFontName: string;
      aCharSet: TCharSet; AFaceSize: Integer; AnExtrusion: Single);
    class procedure BuildFontFromFile(const AFontLabel, AFontFile: string;
      aCharSet: TCharSet; AFaceSize: Integer; AnExtrusion: Single);
    class function CreateText(const AFontLabel: string;
      const AText: UnicodeString): TVertexObject;
    class function GetExtents(const AFontLabel: string;
      const AText: UnicodeString): TExtents;

    class function GetLybrary: FT_Library;
    class property Error: FT_Error read FErr;
  end;

  TVF_PointList = TDataList<TGLUAffineVector>;

  // TVF_Contour
  //
  TVF_Contour = class(TObject)
  private
    FPointList: TVF_PointList;
    FControlPoints: array [0 .. 3, 0 .. 1] of Single;
    procedure AddPoint(x, y: Single); overload; {$IFDEF INLINE_ON} inline; {$ENDIF}
    procedure AddPoint(const APoint: TGLUAffineVector); overload; {$IFDEF INLINE_ON} inline; {$ENDIF}
    procedure evaluateQuadraticCurve; {$IFDEF INLINE_ON} inline; {$ENDIF}
    procedure evaluateCubicCurve; {$IFDEF INLINE_ON} inline; {$ENDIF}
    function GetPoint(I: Integer): TGLUAffineVector;
    function GetPointCount: Cardinal;
  public
    constructor Create(const AContour: FT_Vector_ptr; pointTags: FT_Bytes;
      ANumberOfPoints: Cardinal);
    destructor Destroy; override;

    property Point[Index: Integer]: TGLUAffineVector read GetPoint;
    property PointCount: Cardinal read GetPointCount;
  end;

  // TVF_Vectoriser
  //
  TVF_Vectoriser = class(TObject)
  private
    FftContourCount: Word;
    FContourFlag: Integer;
    FOutline: FT_Outline;
    FContourList: array of TVF_Contour;
    procedure ProcessContours;
    function GetContour(I: Integer): TVF_Contour;
    function GetContourSize(I: Integer): Cardinal;
  public
    constructor Create(AGlyph: FT_GlyphSlot);
    destructor Destroy; override;
    procedure AddGlyphToMesh(const AVO: TVertexObject;
      zNormal: Double = VF_FRONT_FACING);
    procedure AddContourToMesh(const AVO: TVertexObject;
      zNormal: Double = VF_FRONT_FACING);
    function PointCount: Cardinal;
    property ContourCount: Word read FftContourCount;
    property Contour[Index: Integer]: TVF_Contour read GetContour;
    property ContourSize[Index: Integer]: Cardinal read GetContourSize;
    property ContourFlag: Integer read FContourFlag;
  end;

  // TVF_Glyph
  //
  TVF_Glyph = class(TObject)
  protected
    FVectoriser: TVF_Vectoriser;
    FMesh: TVertexObject;
    FAdvance: Single;
    FBBox: TVF_BBox;
    FErr: FT_Error;
  public
    constructor Create(AGlyph: FT_GlyphSlot); virtual;
    destructor Destroy; override;
    function AddToMesh(const AVO: TVertexObject; const APen: TVector): Single;
      virtual; abstract;
    property Advance: Single read FAdvance;
    property BBox: TVF_BBox read FBBox;
    property Error: FT_Error read FErr;
  end;

  // TVF_PolyGlyph
  //
  TVF_PolyGlyph = class(TVF_Glyph)
  protected

  public
    constructor Create(AGlyph: FT_GlyphSlot); override;
    destructor Destroy; override;

    function AddToMesh(const AVO: TVertexObject; const APen: TVector)
      : Single; override;
  end;

  // TVF_ExtrGlyph
  //
  TVF_ExtrGlyph = class(TVF_PolyGlyph)
  public
    constructor Create(AGlyph: FT_GlyphSlot; ADepth: Single); reintroduce;
    destructor Destroy; override;
  end;

  TVF_GlyphList = TDataList<TVF_Glyph>;

  TVF_GlyphContainer = class(TObject)
  private
    FGlyphList: TVF_GlyphList;
    FFace: TVF_Face;
    FCharMap: TVF_Charmap;
    FErr: FT_Error;
    function GetGlyph(ACharacterCode: Cardinal): TVF_Glyph;
    function GetBBox(ACharacterCode: Cardinal): TVF_BBox;
  public
    constructor Create(AFace: TVF_Face);
    destructor Destroy; override;

    function CharMap(AEncoding: FT_Encoding): Boolean;
    function FontIndex(ACharacterCode: Cardinal): Cardinal;
    procedure Add(AGlyph: TVF_Glyph; ACharacterCode: Cardinal);
    function Advance(ACharacterCode, ANextCharacterCode: Cardinal): Single;
    function AddToMesh(ACharacterCode, ANextCharacterCode: Cardinal;
      APen: TVector; AVO: TVertexObject): TGLUAffineVector;

    property Glyph[ACharCode: Cardinal]: TVF_Glyph read GetGlyph;
    property BBox[ACharCode: Cardinal]: TVF_BBox read GetBBox;
    property Error: FT_Error read FErr;
  end;

  // TVF_Font
  //
  TVF_Font = class(TObject)
  private
    function GetAscender: Single;
    function GetDescender: Single;
    procedure CheckGlyph(G: Cardinal); {$IFDEF INLINE_ON} inline; {$ENDIF}
    procedure GetGlyphs(const AStr: string; APos: Integer;
      out AGlyph, AnextGlyph: Cardinal); {$IFDEF INLINE_ON} inline; {$ENDIF}
  protected
    FFace: TVF_Face;
    FCharSize: TVF_Size;
    FErr: FT_Error;
    FGlyphList: TVF_GlyphContainer;
    FPen: TVector;
    function MakeGlyph(G: Cardinal): TVF_Glyph; virtual; abstract;
  public
    constructor Create(const AFontName: string); overload;
    constructor Create(pBufferBytes: FT_Byte_ptr;
      bufferSizeInBytes: Cardinal); overload;
    destructor Destroy; override;

    function CharMap(AEncoding: FT_Encoding): Boolean;
    function BBox(const AStr: string): TExtents;
    function Advance(const AStr: string): Single;
    function FaceSize(asize, ares: Cardinal): Boolean;
    procedure AddToMesh(const AStr: string; AVO: TVertexObject);

    property Error: FT_Error read FErr;
    property Ascender: Single read GetAscender;
    property Descender: Single read GetDescender;
  end;

  // TVF_PolygonFont
  //
  TVF_PolygonFont = class(TVF_Font)
  protected
    function MakeGlyph(G: Cardinal): TVF_Glyph; override;
  end;

  // TVF_ExtrudedFont
  //
  TVF_ExtrudedFont = class(TVF_Font)
  private
    FDepth: Single;
  protected
    function MakeGlyph(G: Cardinal): TVF_Glyph; override;
  public
    constructor Create(const AFontName: string); overload;
    constructor Create(pBufferBytes: FT_Byte_ptr;
      bufferSizeInBytes: Cardinal); overload;

    property depth: Single read FDepth write FDepth;
  end;

implementation

uses
  SysUtils,
  uBaseTypes,
  uMath,
  uPersistentClasses;

type
  VectorFontException = class(Exception);

resourcestring
  StrFTError = 'FREETYPE Error: %s';

procedure MakeVector(out v: TGLUAffineVector; const p: FT_Vector);
  overload; {$IFDEF INLINE_ON} inline; {$ENDIF}
begin
  v.v[0] := p.x;
  v.v[1] := p.y;
  v.v[2] := 0.0;
end;

function CompareCardinal(const Item1, Item2: Cardinal): Integer;
begin
  if Item1 < Item2 then
  begin
    Result := -1;
  end
  else if (Item1 = Item2) then
  begin
    Result := 0;
  end
  else
  begin
    Result := 1;
  end
end;

{$REGION 'TVF_Size'}
// ------------------
// ------------------ TVF_Size ------------------
// ------------------

function TVF_Size.CharSize(face: FT_Face; point_size, x_resolution,
  y_resolution: Cardinal): Boolean;
begin
  FErr := FT_Set_Char_Size(face, 0, point_size * 64, x_resolution,
    y_resolution);
  Result := FErr = 0;
  if Result then
  begin
    ftFace := face;
    fSize := point_size;
    ftSize := ftFace.Size;
  end
  else
  begin
    ftFace := nil;
    fSize := 0;
    ftSize := nil;
    raise VectorFontException.CreateFmt(StrFTError, [FT_GetErrorString(FErr)]);
  end;
end;

function TVF_Size.CharSize: Cardinal;
begin
  Result := fSize;
end;

function TVF_Size.Ascender: Single;
begin
  if Assigned(ftSize) then
    Result := ftSize.metrics.Ascender
  else
    Result := 0;
end;

function TVF_Size.Descender: Single;
begin
  if Assigned(ftSize) then
    Result := ftSize.metrics.Descender
  else
    Result := 0;
end;

function TVF_Size.Width: Single;
begin
  if Assigned(ftSize) then
  begin
    if FT_IS_SCALABLE(@ftFace) then
      Result := (ftFace.BBox.xMax - ftFace.BBox.xMin) *
        (ftSize.metrics.x_ppem / ftFace.units_per_EM)
    else
      Result := ftSize.metrics.max_advance / 64.0;
  end
  else
    Result := 0;
end;

function TVF_Size.Height: Single;
begin
  if Assigned(ftSize) then
  begin
    if FT_IS_SCALABLE(@ftFace) then
      Result := (ftFace.BBox.yMax - ftFace.BBox.yMin) *
        (ftSize.metrics.y_ppem / ftFace.units_per_EM)
    else
      Result := ftSize.metrics.Height / 64.0;
  end
  else
    Result := 0;
end;

class function TVF_Size.Underline: Single;
begin
  Result := 0;
end;

function TVF_Size.XPixelsPerEm: Cardinal;
begin
  if Assigned(ftSize) then
    Result := ftSize.metrics.x_ppem
  else
    Result := 0;
end;

function TVF_Size.YPixelsPerEm: Cardinal;
begin
  if Assigned(ftSize) then
    Result := ftSize.metrics.y_ppem
  else
    Result := 0;
end;
{$ENDREGION 'TVF_Size'}
{$REGION 'TVF_Size'}
// ------------------
// ------------------ TVF_BBox ------------------
// ------------------

class operator TVF_BBox.Add(const a, b: TVF_BBox): TVF_BBox;
begin
  Result.FlowerX := TMath.Min(a.FlowerX, b.FlowerX);
  Result.FlowerY := TMath.Min(a.FlowerY, b.FlowerY);
  Result.FlowerZ := TMath.Min(a.FlowerZ, b.FlowerZ);
  Result.FupperX := TMath.Max(a.FupperX, b.FupperX);
  Result.FupperY := TMath.Max(a.FupperY, b.FupperY);
  Result.FupperZ := TMath.Max(a.FupperZ, b.FupperZ);
end;

procedure TVF_BBox.Null;
begin
  FillChar(Self, SizeOf(TVF_BBox), $00);
end;

procedure TVF_BBox.SetValue(lx, ly, lz, ux, uy, uz: Single);
begin
  FlowerX := lx;
  FlowerY := ly;
  FlowerZ := lz;
  FupperX := ux;
  FupperY := uy;
  FupperZ := uz;
end;

procedure TVF_BBox.SetValue(AGlyph: FT_GlyphSlot);
var
  BBox: FT_BBox;
begin
  FT_Outline_Get_CBox(AGlyph.outline, @BBox);
  FlowerX := BBox.xMin / 64.0;
  FlowerY := BBox.yMin / 64.0;
  FlowerZ := 0.0;
  FupperX := BBox.xMax / 64.0;
  FupperY := BBox.yMax / 64.0;
  FupperZ := 0.0;
end;

procedure TVF_BBox.Move(const AVec: TGLUAffineVector);
begin
  FlowerX := FlowerX + AVec.v[0];
  FlowerY := FlowerY + AVec.v[1];
  FlowerZ := FlowerZ + AVec.v[2];
  FupperX := FupperX + AVec.v[0];
  FupperY := FupperY + AVec.v[1];
  FupperZ := FupperZ + AVec.v[2];
end;

procedure TVF_BBox.SetDebth(depth: Single);
begin
  FupperZ := FlowerZ + depth;
end;

{$ENDREGION 'TVF_Size'}
{$REGION 'TVF_Face'}
// ------------------
// ------------------ TVF_Face ------------------
// ------------------

constructor TVF_Face.Create(const AFileName: string);
var
  lvFileName: AnsiString;
begin
  if VectorFontLibrary.Error = 0 then
  begin
    lvFileName := AnsiString(AFileName);
    FErr := FT_New_Face(VectorFontLibrary.GetLybrary, PAnsiChar(lvFileName),
      0, ftFace);
  end
  else
    FErr := VectorFontLibrary.Error;

  if FErr <> 0 then
  begin
    ftFace := nil;
    raise VectorFontException.CreateFmt(StrFTError, [FT_GetErrorString(FErr)]);
  end
  else
  begin
    FNumGlyphs := ftFace.num_glyphs;
    FCharSize := TVF_Size.Create
  end;
end;

constructor TVF_Face.Create(pBufferBytes: FT_Byte_ptr;
  bufferSizeInBytes: Cardinal);
begin
  FErr := FT_New_Memory_Face(VectorFontLibrary.GetLybrary, pBufferBytes,
    bufferSizeInBytes, 0, ftFace);

  if FErr <> 0 then
  begin
    ftFace := nil;
    raise VectorFontException.CreateFmt(StrFTError, [FT_GetErrorString(FErr)]);
  end
  else
  begin
    FNumGlyphs := ftFace.num_glyphs;
    FCharSize := TVF_Size.Create
  end;
end;

destructor TVF_Face.Destroy;
begin
  FCharSize.Free;
  Close;
end;

function TVF_Face.Attach(const AFileName: string): Boolean;
var
  lvFileName: AnsiString;
begin
  lvFileName := AnsiString(AFileName);
  FErr := FT_Attach_File(ftFace, PAnsiChar(AFileName[1]));
  Result := FErr = 0;
end;

function TVF_Face.Attach(pBufferBytes: FT_Byte_ptr;
  bufferSizeInBytes: Cardinal): Boolean;
var
  open: FT_Open_Args;
begin
  open.flags := FT_OPEN_MEMORY;
  open.memory_base := pBufferBytes;
  open.memory_size := bufferSizeInBytes;

  FErr := FT_Attach_Stream(ftFace, open);
  Result := FErr = 0;
end;

procedure TVF_Face.Close;
begin
  if Assigned(ftFace) then
  begin
    FT_Done_Face(ftFace);
    ftFace := nil;
  end;
end;

function TVF_Face.Size(asize, ares: Cardinal): TVF_Size;
begin
  FCharSize.CharSize(ftFace, asize, ares, ares);
  FErr := FCharSize.Error;

  Result := FCharSize;
end;

function TVF_Face.UnitsPerEM: Cardinal;
begin
  Result := ftFace.units_per_EM;
end;

function TVF_Face.CharMapCount: Cardinal;
begin
  Result := ftFace.num_charmaps;
end;

function TVF_Face.CharMapList: TVF_EncodingList;
var
  I: Integer;
begin
  if not Assigned(FFontEncodingList) then
  begin
    SetLength(FFontEncodingList, CharMapCount);
    for I := 0 to CharMapCount - 1 do
      FFontEncodingList[I] := ftFace.charmaps[I].encoding;
  end;

  Result := FFontEncodingList;
end;

function TVF_Face.KernAdvance(index1, index2: Cardinal): TGLUAffineVector;
var
  x, y: Single;
  kernAdv: FT_Vector;
begin
  x := 0.0;
  y := 0.0;

  if FT_HAS_KERNING(ftFace) and (index1 <> 0) and (index2 <> 0) then
  begin
    kernAdv.x := 0;
    kernAdv.y := 0;

    FErr := FT_Get_Kerning(ftFace, index1, index2, ft_kerning_unfitted,
      kernAdv);
    if FErr = 0 then
    begin
      x := kernAdv.x / 64.0;
      y := kernAdv.y / 64.0;
    end
    else
      raise VectorFontException.CreateFmt(StrFTError,
        [FT_GetErrorString(FErr)]);
  end;

  Result.v[0] := x;
  Result.v[1] := y;
  Result.v[2] := 0.0;
end;

function TVF_Face.Glyph(index: Cardinal; load_flags: FT_Int): FT_GlyphSlot;
begin
  FErr := FT_Load_Glyph(ftFace, index, load_flags);
  if FErr <> 0 then
  begin
    raise VectorFontException.CreateFmt(StrFTError, [FT_GetErrorString(FErr)]);
    Exit(nil);
  end;
  Result := ftFace.Glyph;
end;
{$ENDREGION 'TVF_Face'}
{$REGION 'TVF_Charmap'}
// ------------------
// ------------------ TVF_Charmap ------------------
// ------------------

constructor TVF_Charmap.Create(AFace: TVF_Face);
begin
  FftFace := AFace.face;
  if FftFace.CharMap = nil then
    FErr := FT_Set_Charmap(FftFace, FftFace.charmaps[0]);

  FftEncoding := FftFace.CharMap.encoding;
  FCharacterMap := TVF_CharToGlyphIndexMap.Create(CompareCardinal, nil);
end;

destructor TVF_Charmap.Destroy;
begin
  FCharacterMap.Free;
end;

function TVF_Charmap.GlyphListIndex(characterCode: Cardinal): Integer;
begin
  if not FCharacterMap.Find(characterCode, Result) then
    Result := -1;
end;

function TVF_Charmap.FontIndex(ACharacterCode: Cardinal): Cardinal;
begin
  Result := FT_Get_Char_Index(FftFace, ACharacterCode);
end;

procedure TVF_Charmap.InsertIndex(characterCode: Cardinal;
  containerIndex: Integer);
begin
  FCharacterMap.Add(characterCode, containerIndex);
end;

function TVF_Charmap.CharMap(encoding: FT_Encoding): Boolean;
begin
  if FftEncoding = encoding then
    Exit(True);

  FErr := FT_Select_Charmap(FftFace, encoding);

  if FErr = 0 then
    FftEncoding := encoding
  else
    FftEncoding := ft_encoding_none;

  FCharacterMap.Clear;
  Result := FErr = 0;
end;
{$ENDREGION 'TVF_Charmap'}
{$REGION 'TVF_Glyph'}
// ------------------
// ------------------ TVF_Glyph ------------------
// ------------------

constructor TVF_Glyph.Create(AGlyph: FT_GlyphSlot);
begin
  if Assigned(AGlyph) then
  begin
    FAdvance := AGlyph.Advance.x / 64.0;
    FBBox.SetValue(AGlyph);
  end;
  FMesh := TVertexObject.Create;
  FMesh.FreeingBehavior := fbManual;
end;

destructor TVF_Glyph.Destroy;
begin
  FMesh.Destroy;
  FVectoriser.Free;
  inherited;
end;

{$ENDREGION 'TVF_Glyph'}
{$REGION 'TVF_Contour'}
// ------------------
// ------------------ TVF_Contour ------------------
// ------------------

constructor TVF_Contour.Create(const AContour: FT_Vector_ptr;
  pointTags: FT_Bytes; ANumberOfPoints: Cardinal);
var
  I: Cardinal;
  pointTag, nextPointTag: Byte;
  controlPoint: TGLUAffineVector;
  previousPoint: TGLUAffineVector;
  nextPoint: TGLUAffineVector;
  controlPoint2: TGLUAffineVector;
begin
  FPointList := TVF_PointList.Create;
  I := 0;
  while I < ANumberOfPoints do
  begin

    pointTag := pointTags[I];

    if (pointTag = FT_Curve_Tag_On) or (ANumberOfPoints < 2) then
    begin
      AddPoint(AContour[I].x, AContour[I].y);
      Inc(I);
      continue;
    end;

    MakeVector(controlPoint, AContour[I]);
    if I = 0 then
      MakeVector(previousPoint, AContour[ANumberOfPoints - 1])
    else
      previousPoint := FPointList.Last;

    if I = ANumberOfPoints - 1 then
      nextPoint := FPointList.First
    else
      MakeVector(nextPoint, AContour[I + 1]);

    if pointTag = FT_Curve_Tag_Conic then
    begin
      if I = ANumberOfPoints - 1 then
        nextPointTag := pointTags[0]
      else
        nextPointTag := pointTags[I + 1];

      while nextPointTag = FT_Curve_Tag_Conic do
      begin
        nextPoint.v[0] := (controlPoint.v[0] + nextPoint.v[0]) * 0.5;
        nextPoint.v[1] := (controlPoint.v[1] + nextPoint.v[1]) * 0.5;
        nextPoint.v[2] := 0;

        FControlPoints[0][0] := previousPoint.v[0];
        FControlPoints[0][1] := previousPoint.v[1];
        FControlPoints[1][0] := controlPoint.v[0];
        FControlPoints[1][1] := controlPoint.v[1];
        FControlPoints[2][0] := nextPoint.v[0];
        FControlPoints[2][1] := nextPoint.v[1];

        evaluateQuadraticCurve;
        Inc(I);

        previousPoint := nextPoint;
        MakeVector(controlPoint, AContour[I]);
        if I = ANumberOfPoints - 1 then
        begin
          nextPoint := FPointList.First;
          nextPointTag := pointTags[0];
        end
        else
        begin
          MakeVector(nextPoint, AContour[I + 1]);
          nextPointTag := pointTags[I + 1];
        end;
      end;

      FControlPoints[0][0] := previousPoint.v[0];
      FControlPoints[0][1] := previousPoint.v[1];
      FControlPoints[1][0] := controlPoint.v[0];
      FControlPoints[1][1] := controlPoint.v[1];
      FControlPoints[2][0] := nextPoint.v[0];
      FControlPoints[2][1] := nextPoint.v[1];

      evaluateQuadraticCurve;
      Inc(I);
      continue;
    end;

    if pointTag = FT_Curve_Tag_Cubic then
    begin
      controlPoint2 := nextPoint;

      if I = ANumberOfPoints - 2 then
        nextPoint := FPointList.First
      else
        MakeVector(nextPoint, AContour[I + 2]);

      FControlPoints[0][0] := previousPoint.v[0];
      FControlPoints[0][1] := previousPoint.v[1];
      FControlPoints[1][0] := controlPoint.v[0];
      FControlPoints[1][1] := controlPoint.v[1];
      FControlPoints[2][0] := controlPoint2.v[0];
      FControlPoints[2][1] := controlPoint2.v[1];
      FControlPoints[3][0] := nextPoint.v[0];
      FControlPoints[3][1] := nextPoint.v[1];

      evaluateCubicCurve;
      Inc(I);
      continue;
    end;
    Inc(I);
  end;
end;

destructor TVF_Contour.Destroy;
begin
  FPointList.Free;
end;

procedure TVF_Contour.AddPoint(x, y: Single);
var
  v: TGLUAffineVector;
begin
  v.v[0] := x;
  v.v[1] := y;
  v.v[2] := 0.0;
  AddPoint(v);
end;

procedure TVF_Contour.AddPoint(const APoint: TGLUAffineVector);
var
  dub: Boolean;
begin
  if FPointList.Count > 0 then
    dub := (APoint = FPointList.Last) or (APoint = FPointList.First)
  else
    dub := False;
  if not dub then
    FPointList.Add(APoint);
end;

procedure TVF_Contour.evaluateQuadraticCurve;
var
  I: Cardinal;
  bezierValues: array [0 .. 1, 0 .. 1] of Single;
  t: Single;
begin
  for I := 0 to Round(1.0 / VF_BEZIER_STEP_SIZE) do
  begin
    t := I * VF_BEZIER_STEP_SIZE;

    bezierValues[0][0] := (1.0 - t) * FControlPoints[0][0] + t *
      FControlPoints[1][0];
    bezierValues[0][1] := (1.0 - t) * FControlPoints[0][1] + t *
      FControlPoints[1][1];

    bezierValues[1][0] := (1.0 - t) * FControlPoints[1][0] + t *
      FControlPoints[2][0];
    bezierValues[1][1] := (1.0 - t) * FControlPoints[1][1] + t *
      FControlPoints[2][1];

    bezierValues[0][0] := (1.0 - t) * bezierValues[0][0] + t *
      bezierValues[1][0];
    bezierValues[0][1] := (1.0 - t) * bezierValues[0][1] + t *
      bezierValues[1][1];

    AddPoint(bezierValues[0][0], bezierValues[0][1]);
  end;
end;

procedure TVF_Contour.evaluateCubicCurve;
var
  I: Cardinal;
  bezierValues: array [0 .. 2, 0 .. 1] of Single;
  t: Single;
begin
  for I := 0 to Round(1.0 / VF_BEZIER_STEP_SIZE) do
  begin
    t := I * VF_BEZIER_STEP_SIZE;

    bezierValues[0][0] := (1.0 - t) * FControlPoints[0][0] + t *
      FControlPoints[1][0];
    bezierValues[0][1] := (1.0 - t) * FControlPoints[0][1] + t *
      FControlPoints[1][1];

    bezierValues[1][0] := (1.0 - t) * FControlPoints[1][0] + t *
      FControlPoints[2][0];
    bezierValues[1][1] := (1.0 - t) * FControlPoints[1][1] + t *
      FControlPoints[2][1];

    bezierValues[2][0] := (1.0 - t) * FControlPoints[2][0] + t *
      FControlPoints[3][0];
    bezierValues[2][1] := (1.0 - t) * FControlPoints[2][1] + t *
      FControlPoints[3][1];
    bezierValues[0][0] := (1.0 - t) * bezierValues[0][0] + t *
      bezierValues[1][0];
    bezierValues[0][1] := (1.0 - t) * bezierValues[0][1] + t *
      bezierValues[1][1];

    bezierValues[1][0] := (1.0 - t) * bezierValues[1][0] + t *
      bezierValues[2][0];
    bezierValues[1][1] := (1.0 - t) * bezierValues[1][1] + t *
      bezierValues[2][1];

    bezierValues[0][0] := (1.0 - t) * bezierValues[0][0] + t *
      bezierValues[1][0];
    bezierValues[0][1] := (1.0 - t) * bezierValues[0][1] + t *
      bezierValues[1][1];

    AddPoint(bezierValues[0][0], bezierValues[0][1]);
  end;
end;

function TVF_Contour.GetPoint(I: Integer): TGLUAffineVector;
begin
  Result := FPointList[I];
end;

function TVF_Contour.GetPointCount: Cardinal;
begin
  Result := FPointList.Count;
end;

{$ENDREGION 'TVF_Contour'}
{$REGION 'TVF_Vectoriser'}
// ------------------
// ------------------ TVF_Vectoriser ------------------
// ------------------

constructor TVF_Vectoriser.Create(AGlyph: FT_GlyphSlot);
begin
  if Assigned(AGlyph) then
  begin
    FOutline := AGlyph.outline;
    FftContourCount := FOutline.n_contours;
    FContourFlag := FOutline.flags;
    ProcessContours;
  end;
end;

destructor TVF_Vectoriser.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FContourList) do
    FContourList[I].Free;
  FContourList := nil;
end;

var
  vMainMesh, vTempMesh: TVertexObject;
  Vertices: TVec3List;
  TexCoords: TVec2List;
  Normals: TVec3List;
  vNormal: vec3;
  tempPointList: TVF_PointList;

procedure AddAttribs(AVO: TVertexObject);
var
  attr: TAttribBuffer;
begin
  attr := TAttribBuffer.CreateAndSetup(CAttribSematics[atVertex].Name, 3,
    vtFloat, 0, btArray);
  attr.Buffer.Allocate(Vertices.Size, Vertices.Data);
  attr.Buffer.SetDataHandler(Vertices);
  attr.SetAttribSemantic(atVertex);
  AVO.AddAttrib(attr, True);

  if Assigned(Normals) then
  begin
    attr := TAttribBuffer.CreateAndSetup(CAttribSematics[atNormal].Name, 3,
      vtFloat, 0, btArray);
    attr.Buffer.Allocate(Normals.Size, Normals.Data);
    attr.Buffer.SetDataHandler(Normals);
    attr.SetAttribSemantic(atNormal);
    AVO.AddAttrib(attr);
  end;

  if Assigned(TexCoords) then
  begin
    attr := TAttribBuffer.CreateAndSetup(CAttribSematics[atTexCoord0].Name, 2,
      vtFloat, 0, btArray);
    attr.Buffer.Allocate(TexCoords.Size, TexCoords.Data);
    attr.Buffer.SetDataHandler(TexCoords);
    attr.SetAttribSemantic(atTexCoord0);
    AVO.AddAttrib(attr);
  end;
end;

procedure tessError(errno: GLUEnum); {$IFDEF GLU_DLL} stdcall; {$ENDIF}
begin
  Assert(False, IntToStr(errno) + ': ' + string(gluErrorString(errno)));
end;

procedure tessVertex(vertexData: Pointer); {$IFDEF GLU_DLL} stdcall; {$ENDIF}
const
  k = 1 / 64;
var
  p: PGLUAffineVector absolute vertexData;
  v: vec3;
  ts: vec2;
begin
  vTempMesh.AddPoint(Vertices.Count);
  v[0] := p.v[0] * k;
  v[1] := p.v[1] * k;
  v[2] := p.v[2] * k;
  Vertices.Add(v);
  Normals.Add(vNormal);
  ts[0] := v[0];
  ts[1] := v[1];
  TexCoords.Add(ts);
end;

procedure tessCombine(const coords: TGLUAffineVector; vertex_data: TGLUData;
  const weight: TGLUVector; var outData: Pointer); {$IFDEF GLU_DLL} stdcall; {$ENDIF}
var
  I: Integer;
begin
  I := tempPointList.Count;
  tempPointList.Add(coords);
  outData := tempPointList.GetItemAddr(I);
end;

procedure tessBegin(AType: GLUEnum); {$IFDEF GLU_DLL} stdcall; {$ENDIF}
begin
  vTempMesh.Clear;
  Vertices := TVec3List.Create;
  Normals := TVec3List.Create;
  TexCoords := TVec2List.Create;
  case AType of
    GL_TRIANGLE_FAN:
      vTempMesh.FaceType := ftTriangleFan;
    GL_TRIANGLE_STRIP:
      vTempMesh.FaceType := ftTriangleStrip;
    GL_TRIANGLES:
      vTempMesh.FaceType := ftTriangles;
    GL_LINE_LOOP:
      vTempMesh.FaceType := ftLineLoop;
  end;
end;

var
  gvC: Integer = 0;

procedure tessEnd(); {$IFDEF GLU_DLL} stdcall; {$ENDIF}
begin
  AddAttribs(vTempMesh);

  case vTempMesh.FaceType of
    ftTriangleFan, ftTriangleStrip:
      vTempMesh.Triangulate;
    ftLineLoop:
      vTempMesh.LineSegmentation;
  end;

  vMainMesh.Join(vTempMesh, TMatrix.IdentityMatrix);
  Vertices := nil; // Ñïèñêè ïåðåõîäÿò âî âëàäåíèå vTempMesh
  Normals := nil;
  TexCoords := nil;
end;

procedure TVF_Vectoriser.AddGlyphToMesh(const AVO: TVertexObject;
  zNormal: Double = VF_FRONT_FACING);
var
  tess: PGLUTesselator;
  c, p: Cardinal;
  Contour: TVF_Contour;
  d: PGLUAffineVector;
{$IFDEF GLU_DLL}
  dd: ^TGLVectord3 absolute d;
{$ENDIF}
  I: Integer;
begin
  tess := VectorFontLibrary.FTesselator;
  vMainMesh := AVO;

  if not Assigned(vTempMesh) then
    vTempMesh := TVertexObject.Create;
  if not Assigned(tempPointList) then
  begin
    tempPointList := TVF_PointList.Create;
    tempPointList.Capacity := VF_TESS_LIST_CAPACITY;
  end
  else
    tempPointList.Flush;

  try
    if FContourFlag and FT_OUTLINE_EVEN_ODD_FILL <> 0 then
      gluTessProperty(tess, GLU_TESS_WINDING_RULE, GLU_TESS_WINDING_ODD)
    else
      gluTessProperty(tess, GLU_TESS_WINDING_RULE, GLU_TESS_WINDING_NONZERO);
    gluTessProperty(tess, GLU_TESS_TOLERANCE, 0.0);
    gluTessNormal(tess, 0.0, 0.0, zNormal);
    vNormal[0] := 0;
    vNormal[1] := 0;
    vNormal[2] := zNormal;
    gluTessBeginPolygon(tess, nil);
    for c := 0 to ContourCount - 1 do
    begin
      Contour := FContourList[c];
      gluTessBeginContour(tess);
      for p := 0 to Contour.PointCount - 1 do
      begin
        d := Contour.FPointList.GetItemAddr(p);
        I := tempPointList.Count;
        tempPointList.AddRaw(d);
{$IFDEF GLU_DLL}
        gluTessVertex(tess, dd^, tempPointList.GetItemAddr(I));
{$ELSE}
        gluTessVertex(tess, d^, tempPointList.GetItemAddr(I));
{$ENDIF}
      end;
      gluTessEndContour(tess);
    end;
    gluTessEndPolygon(tess);

  except
    FreeAndNil(Vertices);
    FreeAndNil(TexCoords);
    FreeAndNil(Normals);
  end;
end;

procedure TVF_Vectoriser.AddContourToMesh(const AVO: TVertexObject;
  zNormal: Double = VF_FRONT_FACING);
const
  k = 1 / 64;
var
  c, p: Cardinal;
  StartIndex, index: Integer;
  Contour: TVF_Contour;
  v: TGLUAffineVector;
  v2: vec2;
  v3: vec3;
  n: vec3;

  procedure EmitVertex;
  begin
    v.SetScale(k);
    v3[0] := v.v[0];
    v3[1] := v.v[1];
    v3[2] := v.v[2];
    Vertices.Add(v3);
    v2[0] := v.v[0];
    v2[1] := v.v[1];
    TexCoords.Add(v2);
    Normals.Add(n);
  end;

begin
  Vertices := TVec3List.Create;
  Normals := TVec3List.Create;
  TexCoords := TVec2List.Create;
  n[0] := 0;
  n[1] := 0;
  n[2] := zNormal;
  Index := 0;

  // Line loops but made with segment
  for c := 0 to ContourCount - 1 do
  begin
    Contour := FContourList[c];
    StartIndex := Index;
    v := Contour.FPointList[0];
    EmitVertex;
    AVO.AddPoint(Index);
    Inc(Index);
    for p := 1 to Contour.PointCount - 2 do
    begin
      v := Contour.FPointList[p];
      EmitVertex;
      AVO.AddLine(Index, Index);
      Inc(Index);
    end;
    v := Contour.FPointList[Contour.PointCount - 1];
    EmitVertex();
    AVO.AddTriangle(Index, Index, StartIndex);
    Inc(Index);
  end;

  AddAttribs(AVO);
  AVO.FaceType := ftLines;
end;

function TVF_Vectoriser.PointCount: Cardinal;
var
  I: Integer;
  S: Cardinal;
begin
  S := 0;
  for I := 0 to High(FContourList) do
    S := S + FContourList[I].GetPointCount;
  Result := S;
end;

procedure TVF_Vectoriser.ProcessContours;
var
  contourLength, StartIndex, endIndex, contourIndex: ShortInt;
  pointList: FT_Vector_ptr;
  tagList: FT_Bytes;
begin
  StartIndex := 0;
  SetLength(FContourList, FftContourCount);

  for contourIndex := 0 to FftContourCount - 1 do
  begin
    pointList := @FOutline.points[StartIndex];
    tagList := @FOutline.tags[StartIndex];
    endIndex := FOutline.contours[contourIndex];
    contourLength := (endIndex - StartIndex) + 1;
    FContourList[contourIndex] := TVF_Contour.Create(pointList, tagList,
      Cardinal(contourLength));
    StartIndex := endIndex + 1;
  end;
end;

function TVF_Vectoriser.GetContour(I: Integer): TVF_Contour;
begin
  Result := FContourList[I];
end;

function TVF_Vectoriser.GetContourSize(I: Integer): Cardinal;
begin
  Result := FContourList[I].PointCount;
end;

{$ENDREGION 'TVF_Vectoriser'}
{$REGION 'TVF_PolyGlyph'}
// ------------------
// ------------------ TVF_PolyGlyph ------------------
// ------------------

constructor TVF_PolyGlyph.Create(AGlyph: FT_GlyphSlot);
begin
  inherited Create(AGlyph);

  if Ord(FT_GLYPH_FORMAT_OUTLINE) <> AGlyph.format then
  begin
    FErr := $14; // Invalid_Outline
    Exit;
  end;

  FVectoriser := TVF_Vectoriser.Create(AGlyph);
  if (FVectoriser.ContourCount > 0) and (FVectoriser.PointCount() >= 3) then
    FVectoriser.AddGlyphToMesh(FMesh); // AddContourToMesh  (FMesh);
end;

destructor TVF_PolyGlyph.Destroy;
begin
  inherited Destroy;
end;

function TVF_PolyGlyph.AddToMesh(const AVO: TVertexObject;
  const APen: TVector): Single;
begin
  AVO.Join(FMesh, TMatrix.TranslationMatrix(APen));
  Result := FAdvance;
end;

{$ENDREGION}
{$REGION 'TVF_ExtrGlyph'}

// ------------------
// ------------------ TVF_ExtrGlyph ------------------
// ------------------
constructor TVF_ExtrGlyph.Create(AGlyph: FT_GlyphSlot; ADepth: Single);
const
  k = 1 / 64;
var
  LMesh: TVertexObject;
  optimusPrime: TMatrix; // of lidership
  gluPoint: TGLUAffineVector;
  zOffset, p1, p2, p3, p4: TVector;
  c, I, nextIndex, index: Integer;
  Contour: TVF_Contour;
  numberOfPoints: Cardinal;
  t0, t1: Single;
begin
  inherited Create(AGlyph);

  if Ord(FT_GLYPH_FORMAT_OUTLINE) <> AGlyph.format then
  begin
    FErr := $14; // Invalid_Outline
    Exit;
  end;

  if (FVectoriser.ContourCount > 0) and (FVectoriser.PointCount() >= 3) then
  begin
    LMesh := TVertexObject.Create;
    LMesh.Assign(FMesh);
    try
      zOffset := TVector.Make(0, 0, -ADepth);
      optimusPrime := TMatrix.TranslationMatrix(zOffset);
      optimusPrime := optimusPrime * TMatrix.ReflectionMatrix
        (TVector.Null, zOffset.Normalize);
      FMesh.Join(LMesh, optimusPrime);

      LMesh.Clear;
      LMesh.FaceType := ftTriangles;
      Vertices := TVec3List.Create;
      Normals := nil;
      TexCoords := TVec2List.Create;

      Index := 0;
      for c := 0 to FVectoriser.ContourCount - 1 do
      begin
        Contour := FVectoriser.Contour[c];
        numberOfPoints := Contour.PointCount;
        if numberOfPoints > 2 then
        begin
          t0 := 0;
          for I := 0 to numberOfPoints - 1 do
          begin
            if I = Integer(numberOfPoints - 1) then
              nextIndex := 0
            else
              nextIndex := I + 1;
            gluPoint := Contour.Point[I];
            p1[0] := gluPoint.v[0];
            p1[1] := gluPoint.v[1];
            p1[2] := gluPoint.v[2];
            gluPoint := Contour.Point[nextIndex];
            p2[0] := gluPoint.v[0];
            p2[1] := gluPoint.v[1];
            p2[2] := gluPoint.v[2];
            p1.SetScale(k);
            p2.SetScale(k);
            p3 := p1 - zOffset;
            p4 := p2 - zOffset;
            // Actully need calculation of texcoords based on contour lenght not points number
            t1 := (I + 1) / numberOfPoints;
            Vertices.Add(p1.vec3);
            TexCoords.Add(TVector.Make(t0, 0).vec2);
            Vertices.Add(p2.vec3);
            TexCoords.Add(TVector.Make(t1, 0).vec2);
            Vertices.Add(p3.vec3);
            TexCoords.Add(TVector.Make(t0, 1).vec2);
            Vertices.Add(p4.vec3);
            TexCoords.Add(TVector.Make(t1, 1).vec2);
            LMesh.AddTriangle(Index, Index + 1, Index + 2);
            LMesh.AddTriangle(Index + 2, Index + 1, Index + 3);
            Inc(Index, 4);
          end;
        end;
      end;
      if Vertices.Count > 2 then
      begin
        AddAttribs(LMesh);
        LMesh.ComputeNormals(True);
        FMesh.Join(LMesh, TMatrix.IdentityMatrix);
        FMesh.WeldVertices;
      end
      else
      begin
        Vertices.Destroy;
        TexCoords.Destroy;
      end;
    finally
      LMesh.Destroy;
      Vertices := nil;
      Normals := nil;
      TexCoords := nil;
    end;
  end;
end;

destructor TVF_ExtrGlyph.Destroy;
begin
  inherited Destroy;
end;
{$ENDREGION}
{$REGION 'TVF_GlyphContainer'}
// ------------------
// ------------------ TVF_GlyphContainer ------------------
// ------------------

constructor TVF_GlyphContainer.Create(AFace: TVF_Face);
begin
  FFace := AFace;
  FCharMap := TVF_Charmap.Create(AFace);
  FGlyphList := TVF_GlyphList.Create;
end;

destructor TVF_GlyphContainer.Destroy;
var
  I: Integer;
begin
  for I := 0 to FGlyphList.Count - 1 do
    FGlyphList[I].Free;
  FGlyphList.Destroy;
  FCharMap.Destroy;
end;

function TVF_GlyphContainer.GetGlyph(ACharacterCode: Cardinal): TVF_Glyph;
var
  I: Integer;
begin
  I := FCharMap.GlyphListIndex(ACharacterCode);
  if I > -1 then
    Result := FGlyphList[I]
  else
    Result := nil;
end;

function TVF_GlyphContainer.GetBBox(ACharacterCode: Cardinal): TVF_BBox;
var
  I: Integer;
begin
  I := FCharMap.GlyphListIndex(ACharacterCode);
  if I > -1 then
    Result := FGlyphList[I].BBox
  else
    Result.Null;
end;

function TVF_GlyphContainer.CharMap(AEncoding: FT_Encoding): Boolean;
begin
  Result := FCharMap.CharMap(AEncoding);
  FErr := FCharMap.Error;
end;

function TVF_GlyphContainer.FontIndex(ACharacterCode: Cardinal): Cardinal;
begin
  Result := FCharMap.FontIndex(ACharacterCode)
end;

procedure TVF_GlyphContainer.Add(AGlyph: TVF_Glyph; ACharacterCode: Cardinal);
begin
  FCharMap.InsertIndex(ACharacterCode, FGlyphList.Add(AGlyph));
end;

function TVF_GlyphContainer.AddToMesh(ACharacterCode, ANextCharacterCode
  : Cardinal; APen: TVector; AVO: TVertexObject): TGLUAffineVector;
var
  KernAdvance: TGLUAffineVector;
  adv: Single;
  left, right: Cardinal;
begin
  adv := 0;

  left := FCharMap.FontIndex(ACharacterCode);
  right := FCharMap.FontIndex(ANextCharacterCode);

  if FFace.Error = 0 then
  begin
    KernAdvance := FFace.KernAdvance(left, right);

    if FFace.Error = 0 then
      adv := FGlyphList[FCharMap.GlyphListIndex(ACharacterCode)].AddToMesh(AVO, APen);

    KernAdvance.V[0] := KernAdvance.V[0] + adv;
    Result := KernAdvance;
  end;
end;

function TVF_GlyphContainer.Advance(ACharacterCode, ANextCharacterCode
  : Cardinal): Single;
var
  I: Integer;
  left, right: Cardinal;
  Width: Single;
begin
  left := FCharMap.FontIndex(ACharacterCode);
  right := FCharMap.FontIndex(ANextCharacterCode);

  Width := FFace.KernAdvance(left, right).V[0];
  I := FCharMap.GlyphListIndex(ACharacterCode);
  if I > -1 then
    Width := Width + FGlyphList[I].Advance;

  Result := Width;
end;
{$ENDREGION}
{$REGION 'TVF_Font'}
// ------------------
// ------------------ TVF_Font ------------------
// ------------------

procedure TVF_Font.AddToMesh(const AStr: string; AVO: TVertexObject);
var
  I: Integer;
  G, ng: Cardinal;
  KernAdvance: TGLUAffineVector;
begin
  FPen := TVector.Null;
  for I := 1 to Length(AStr) do
  begin
    GetGlyphs(AStr, I, G, ng);
    CheckGlyph(G);
    CheckGlyph(ng);
    KernAdvance := FGlyphList.AddToMesh(G, ng, FPen, AVO);
    FPen[0] := FPen[0] + KernAdvance.V[0];
    FPen[1] := FPen[1] + KernAdvance.V[1];
    FPen[2] := FPen[2] + KernAdvance.V[2];
  end;
end;

function TVF_Font.Advance(const AStr: string): Single;
var
  I: Integer;
  G, ng: Cardinal;
  w: Single;
begin
  w := 0;
  for I := 1 to Length(AStr) do
  begin
    GetGlyphs(AStr, I, G, ng);
    CheckGlyph(G);
    CheckGlyph(ng);
    w := w + FGlyphList.Advance(G, ng);
  end;
  Result := w;
end;

function TVF_Font.BBox(const AStr: string): TExtents;
var
  totalBBox, tempBBox: TVF_BBox;
  I: Integer;
  G, ng: Cardinal;
  adv: Single;
begin
  totalBBox.Null;

  if Length(AStr) > 0 then
  begin
    GetGlyphs(AStr, 1, G, ng);
    totalBBox := FGlyphList.BBox[G];
    adv := FGlyphList.Advance(G, ng);

    for I := 2 to Length(AStr) do
    begin
      GetGlyphs(AStr, I, G, ng);
      tempBBox := FGlyphList.BBox[G];
      tempBBox.Move(TGLUAffineVector.Make(adv, 0, 0));
      totalBBox := totalBBox + tempBBox;
      adv := adv + FGlyphList.Advance(G, ng);
    end;
  end;

  Result.eMin[0] := totalBBox.FlowerX;
  Result.eMin[1] := totalBBox.FlowerY;
  Result.eMin[2] := totalBBox.FlowerZ;
  Result.eMax[0] := totalBBox.FupperX;
  Result.eMax[1] := totalBBox.FupperY;
  Result.eMax[2] := totalBBox.FupperZ;
end;

function TVF_Font.CharMap(AEncoding: FT_Encoding): Boolean;
begin
  Result := FFace.CharMapCount > 0;
end;

procedure TVF_Font.CheckGlyph(G: Cardinal);
var
  newGlyph: TVF_Glyph;
begin
  if not Assigned(FGlyphList) then
  begin
    // LogWarning('FREETYPE font size is undefined - use default');
    FaceSize(1, 72);
  end;

  if (G > 0) and (FGlyphList.Glyph[G] = nil) then
  begin
    newGlyph := MakeGlyph(FGlyphList.FontIndex(G));
    if Assigned(newGlyph) then
      FGlyphList.Add(newGlyph, G);
  end;
end;

constructor TVF_Font.Create(const AFontName: string);
begin
  FFace := TVF_Face.Create(AFontName);
  FErr := FFace.Error;
end;

constructor TVF_Font.Create(pBufferBytes: FT_Byte_ptr;
  bufferSizeInBytes: Cardinal);
begin
  FFace := TVF_Face.Create(pBufferBytes, bufferSizeInBytes);
  FErr := FFace.Error;
end;

destructor TVF_Font.Destroy;
begin
  FGlyphList.Free;
  FFace.Free;
  inherited;
end;

function TVF_Font.GetAscender: Single;
begin
  Assert(Assigned(FCharSize));
  Result := FCharSize.Ascender;
end;

function TVF_Font.GetDescender: Single;
begin
  Assert(Assigned(FCharSize));
  Result := FCharSize.Descender;
end;

procedure TVF_Font.GetGlyphs(const AStr: string; APos: Integer;
  out AGlyph, AnextGlyph: Cardinal);
begin
  AGlyph := Cardinal(AStr[APos]);
  if APos < Length(AStr) then
    AnextGlyph := Cardinal(AStr[APos + 1])
  else
    AnextGlyph := 0;
end;

function TVF_Font.FaceSize(asize, ares: Cardinal): Boolean;
begin
  FCharSize := FFace.Size(asize, ares);
  if FFace.Error <> 0 then
  begin
    // LogErrorFmt(StrFTError, [FT_GetErrorString(FErr)]);
    Exit(False);
  end;

  if Assigned(FGlyphList) then
    FGlyphList.Destroy;

  FGlyphList := TVF_GlyphContainer.Create(FFace);
  Result := True;
end;

{$ENDREGION}
{$REGION 'TVF_PolygonFont'}
// ------------------
// ------------------ TVF_PolygonFont ------------------
// ------------------

function TVF_PolygonFont.MakeGlyph(G: Cardinal): TVF_Glyph;
var
  ftGlyph: FT_GlyphSlot;
  tempGlyph: TVF_PolyGlyph;
begin
  ftGlyph := FFace.Glyph(G, FT_LOAD_NO_HINTING);

  if Assigned(ftGlyph) then
  begin
    tempGlyph := TVF_PolyGlyph.Create(ftGlyph);
    Result := tempGlyph;
    Exit;
  end;

  FErr := FFace.Error;
  Result := nil;
end;

{$ENDREGION}
{$REGION 'TVF_ExtrudedFont'}
// ------------------
// ------------------ TVF_ExtrudedFont ------------------
// ------------------

constructor TVF_ExtrudedFont.Create(const AFontName: string);
begin
  inherited Create(AFontName);
  FDepth := 0;
end;

constructor TVF_ExtrudedFont.Create(pBufferBytes: FT_Byte_ptr;
  bufferSizeInBytes: Cardinal);
begin
  inherited Create(pBufferBytes, bufferSizeInBytes);
  FDepth := 0;
end;

function TVF_ExtrudedFont.MakeGlyph(G: Cardinal): TVF_Glyph;
var
  ftGlyph: FT_GlyphSlot;
  tempGlyph: TVF_ExtrGlyph;
begin
  ftGlyph := FFace.Glyph(G, FT_LOAD_NO_HINTING);

  if Assigned(ftGlyph) then
  begin
    tempGlyph := TVF_ExtrGlyph.Create(ftGlyph, FDepth);
    Result := tempGlyph;
    Exit;
  end;

  FErr := FFace.Error;
  Result := nil;
end;

{$ENDREGION}
{$REGION 'VectorFontLibrary'}
// ------------------
// ------------------ VectorFontLibrary ------------------
// ------------------

class procedure VectorFontLibrary.Initialize;
var
  major, minor, patch: Integer;
begin
  FFontCache := TFontCache.Create(CompareStr, nil);

  if InitFreetype then
  begin
    FErr := FT_Init_FreeType(FLibrary);
    if FErr <> 0 then
    begin
      FreeMem(FLibrary);
      FLibrary := nil;
      raise VectorFontException.CreateFmt(StrFTError,
        [FT_GetErrorString(FErr)]);
    end
    else
    begin
      FT_Library_Version(FLibrary, major, minor, patch);
      // LogInfo('FreeType library %d.%d.%d loaded', [major, minor, patch]);
      FTesselator := gluNewTess();
      gluTessCallback(FTesselator, GLU_TESS_BEGIN, @tessBegin);
      gluTessCallback(FTesselator, GLU_TESS_VERTEX, @tessVertex);
      gluTessCallback(FTesselator, GLU_TESS_COMBINE, @tessCombine);
      gluTessCallback(FTesselator, GLU_TESS_END, @tessEnd);
      gluTessCallback(FTesselator, GLU_TESS_ERROR, @tessError);
    end;
  end
  else
    FErr := $04;
end;

procedure DestroyFonts(AKey: string; AValue: TVF_Font; out AContinue: Boolean);
begin
  AValue.Destroy;
  AContinue := True;
end;

class procedure VectorFontLibrary.Finalize;
begin
  if Assigned(FTesselator) then
    gluDeleteTess(FTesselator);
  if Assigned(FFontCache) then
  begin
    FFontCache.ForEach(DestroyFonts);
    FFontCache.Destroy;
  end;
  if Assigned(FLibrary) then
  begin
    FT_Done_FreeType(FLibrary);
    FLibrary := nil;
  end;
  CloseFreetype;
end;

class function VectorFontLibrary.GetLybrary: FT_Library;
begin
  Result := FLibrary;
end;

class function VectorFontLibrary.CreateText(const AFontLabel: string;
  const AText: UnicodeString): TVertexObject;
var
  font: TVF_Font;
  VO: TVertexObject;
begin
  Result := nil;
  if VectorFontLibrary.FFontCache.Find(AFontLabel, font) then
  begin
    VO := TVertexObject.Create;
    try
      font.AddToMesh(AText, VO);
    except
      VO.Free;
      raise;
    end;
    Result := VO;
  end;
end;

class procedure VectorFontLibrary.BuildSystemFont(const AFontLabel,
  AFontName: string; aCharSet: TCharSet; AFaceSize: Integer;
  AnExtrusion: Single);
var
  lPath: array [0 .. 255] of WideChar;
  sPath: string;
begin
{$IFDEF MSWINDOWS}
  GetWindowsDirectoryW(lPath, 255);
  sPath := IncludeTrailingPathDelimiter(lPath) + IncludeTrailingPathDelimiter
    ('Fonts') + AFontName + '.ttf';
{$ELSE}
  sPath := IncludeTrailingPathDelimiter('/usr/share/fonts/truetype') + AFontName + '.ttf';
  //raise VectorFontException.Create('Not yet implemented');
{$ENDIF}
  if FileExists(sPath) then
    BuildFontFromFile(AFontLabel, sPath, aCharSet, AFaceSize, AnExtrusion);
end;

class procedure VectorFontLibrary.BuildFontFromFile(const AFontLabel,
  AFontFile: string; aCharSet: TCharSet; AFaceSize: Integer;
  AnExtrusion: Single);
var
  font: TVF_Font;
  ch: WideChar;
begin
  if VectorFontLibrary.FFontCache.Find(AFontLabel, font) then
    Exit;

  if AnExtrusion > 0 then
  begin
    font := TVF_ExtrudedFont.Create(AFontFile);
    TVF_ExtrudedFont(font).depth := AnExtrusion;
  end
  else
    font := TVF_PolygonFont.Create(AFontFile);

  if font.Error = 0 then
  begin
    font.FaceSize(AFaceSize, 72);
    for ch in aCharSet do
      font.CheckGlyph(Cardinal(ch));
  end;

  VectorFontLibrary.FFontCache.Add(AFontLabel, font);
end;

class function VectorFontLibrary.GetExtents(const AFontLabel: string;
  const AText: UnicodeString): TExtents;
var
  font: TVF_Font;
begin
  Result.Reset;
  if VectorFontLibrary.FFontCache.Find(AFontLabel, font) then
  begin
    if font.Error = 0 then
      Result := font.BBox(AText);
  end;
end;

{$ENDREGION 'VectorFontLibrary'}

initialization
{$IFDEF GLU_DLL}
InitOpenGL;
{$ENDIF}

VectorFontLibrary.Initialize;

finalization

VectorFontLibrary.Finalize;

end.
