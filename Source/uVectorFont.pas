unit uVectorFont;

interface

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{$DEFINE INLINE_ON}
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
  uBaseTypes,
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
    procedure SetDebth(aThickness: Single);
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
    class var FJunk: TObjectList;
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

  TVF_GlyphMesh = record
    FaceType: TFaceType;
    Positions: TVec3List;
    Normals: TVec3List;
    TexCoords: TVec2List;
    Indices: TIntegerArray;
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
    procedure CookPolygon(var aMesh: TVF_GlyphMesh; aWelding: Boolean;
      zNormal: Double = VF_FRONT_FACING);
    procedure CookContour(var aMesh: TVF_GlyphMesh;
      zNormal: Double = VF_FRONT_FACING);
    function PointCount: Integer;
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
    FMesh: TVF_GlyphMesh;
    FAdvance: Single;
    FBBox: TVF_BBox;
    FErr: FT_Error;
  public
    constructor Create(AGlyph: FT_GlyphSlot; aThickness: Single = 0); virtual;
    destructor Destroy; override;
    function Join(var aMesh: TVF_GlyphMesh; const APen: TVector): Single;
    property Advance: Single read FAdvance;
    property BBox: TVF_BBox read FBBox;
    property Error: FT_Error read FErr;
  end;

  // TVF_PolyGlyph
  //
  TVF_PolyGlyph = class(TVF_Glyph)
  public
    constructor Create(AGlyph: FT_GlyphSlot; aThickness: Single = 0); override;
  end;

  // TVF_ExtrGlyph
  //
  TVF_ExtrGlyph = class(TVF_Glyph)
  public
    constructor Create(AGlyph: FT_GlyphSlot; aThickness: Single = 0); override;
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
      aPen: TVector; var aMesh: TVF_GlyphMesh): TVector;

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
    function CreateVertexObject(const AStr: string): TVertexObject;

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
    FThickness: Single;
  protected
    function MakeGlyph(G: Cardinal): TVF_Glyph; override;
  public
    constructor Create(const AFontName: string); overload;
    constructor Create(pBufferBytes: FT_Byte_ptr;
      bufferSizeInBytes: Cardinal); overload;

    property Thickness: Single read FThickness write FThickness;
  end;

implementation

uses
  SysUtils,
  uMath,
  uMeshUtils,
  uDataAccess;

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

procedure TVF_BBox.SetDebth(aThickness: Single);
begin
  FupperZ := FlowerZ + aThickness;
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

constructor TVF_Glyph.Create(AGlyph: FT_GlyphSlot; aThickness: Single = 0);
begin
  if Assigned(AGlyph) then
  begin
    FAdvance := AGlyph.Advance.x / 64.0;
    FBBox.SetValue(AGlyph);
  end;
  with FMesh do
  begin
    Positions := TVec3List.Create;
    Normals := TVec3List.Create;
    TexCoords := TVec2List.Create;
  end;
end;

destructor TVF_Glyph.Destroy;
begin
  with FMesh do
  begin
    Positions.Free;
    Normals.Free;
    TexCoords.Free;
  end;
  FVectoriser.Free;
  inherited;
end;

function TVF_Glyph.Join(var aMesh: TVF_GlyphMesh;
  const APen: TVector): Single;
var
  offset: Integer;
begin
  offset := aMesh.Positions.Count;
  aMesh.Positions.Join(FMesh.Positions, TMatrix.TranslationMatrix(APen));
  aMesh.Normals.Join(FMesh.Normals, TMatrix.IdentityMatrix);
  aMesh.TexCoords.Join(FMesh.TexCoords, TMatrix.IdentityMatrix);
  MeshUtils.Join(aMesh.Indices, FMesh.Indices, offset);
  Result := FAdvance;
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

threadvar
  vFaceType: TFaceType;
  vTessVertices: TVec3List;
  vNormal: vec3;
  vTempPointList: TVF_PointList;

  vGlyphVertices: TVec3List;
  vGlyphTexCoords: TVec2List;
  vGlyphNormals: TVec3List;
  vGlyphIndices: TIntegerArray;

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
begin
  v[0] := p.v[0] * k;
  v[1] := p.v[1] * k;
  v[2] := 0;
  vTessVertices.Add(v);
end;

procedure tessCombine(const coords: TGLUAffineVector; vertex_data: TGLUData;
  const weight: TGLUVector; var outData: Pointer); {$IFDEF GLU_DLL} stdcall; {$ENDIF}
var
  I: Integer;
begin
  I := vTempPointList.Count;
  vTempPointList.Add(coords);
  outData := vTempPointList.GetItemAddr(I);
end;

procedure tessBegin(AType: GLUEnum); {$IFDEF GLU_DLL} stdcall; {$ENDIF}
begin
  case AType of
    GL_TRIANGLE_FAN:
      vFaceType := ftTriangleFan;
    GL_TRIANGLE_STRIP:
      vFaceType := ftTriangleStrip;
    GL_TRIANGLES:
      vFaceType := ftTriangles;
    GL_LINE_LOOP:
      vFaceType := ftLineLoop;
  end;
  if not Assigned(vTessVertices) then
  begin
    vTessVertices := TVec3List.Create;
    // Сохраним для удаления, полезно при многопоточности
    VectorFontLibrary.FJunk.Add(vTessVertices);
  end
  else
    vTessVertices.Flush;
end;

procedure tessEnd(); {$IFDEF GLU_DLL} stdcall; {$ENDIF}
var
  InAttribs, OutAttribs: TAbstractDataListArray;
  Indices: TIntegerArray;
  I: Integer;
begin
  SetLength(Indices, vTessVertices.Count);
  for I := 0 to High(Indices) do
    Indices[I] := I;
  case vFaceType of
    ftTriangleFan, ftTriangleStrip:
      MeshUtils.Triangulate(vFaceType, Indices);
//    ftLineLoop:
//      LineSegmentation;
  end;
  SetLength(OutAttribs, 1);
  OutAttribs[0] := vGlyphVertices;
  SetLength(InAttribs, 1);
  InAttribs[0] := vTessVertices;

  MeshUtils.Join(OutAttribs, InAttribs, vGlyphIndices, Indices);
end;

procedure TVF_Vectoriser.CookPolygon(var aMesh: TVF_GlyphMesh; aWelding: Boolean;
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
  ListArray: TAbstractDataListArray;
  VDA1, VDA2, VDA3: IVectorDataAccess;
begin
  tess := VectorFontLibrary.FTesselator;
  vGlyphVertices := TVec3List.Create;
  SetLength(vGlyphIndices, 0);

  if not Assigned(vTempPointList) then
  begin
    vTempPointList := TVF_PointList.Create;
    vTempPointList.Capacity := VF_TESS_LIST_CAPACITY;
    VectorFontLibrary.FJunk.Add(vTempPointList);
  end
  else
    vTempPointList.Flush;

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
        I := vTempPointList.Count;
        vTempPointList.AddRaw(d);
  {$IFDEF GLU_DLL}
        gluTessVertex(tess, dd^, vTempPointList.GetItemAddr(I));
  {$ELSE}
        gluTessVertex(tess, d^, vTempPointList.GetItemAddr(I));
  {$ENDIF}
      end;
      gluTessEndContour(tess);
    end;
    gluTessEndPolygon(tess);

    if vGlyphVertices.Count > 0 then
    begin
      MeshUtils.UnWeldVertices([vGlyphVertices], ListArray, vGlyphIndices);
      vGlyphVertices.Destroy;
      vGlyphVertices := ListArray[0] as TVec3List;
      VDA1 := TVectorDataAccess.Create(vGlyphVertices.Data, vtFloat, 3, 3*SizeOf(Single), vGlyphVertices.Count);
      vGlyphNormals := MeshUtils.ComputeTriangleNormals(True, VDA1, vGlyphIndices);
      vGlyphTexCoords := MeshUtils.ComputeTriangleTexCoords(VDA1, vGlyphIndices);
      if aWelding then
      begin
        VDA2 := TVectorDataAccess.Create(vGlyphNormals.Data, vtFloat, 3, 3*SizeOf(Single), vGlyphNormals.Count);
        VDA3 := TVectorDataAccess.Create(vGlyphTexCoords.Data, vtFloat, 2, 2*SizeOf(Single), vGlyphTexCoords.Count);
        MeshUtils.WeldVertices([VDA1, VDA2, VDA3], ListArray, vGlyphIndices);
        vGlyphVertices.Destroy;
        vGlyphNormals.Destroy;
        vGlyphTexCoords.Destroy;
        vGlyphVertices := ListArray[0] as TVec3List;
        vGlyphNormals := ListArray[1] as TVec3List;
        vGlyphTexCoords := ListArray[2] as TVec2List;
      end;
    end;
  except
    FreeAndNil(vGlyphVertices);
    FreeAndNil(vGlyphTexCoords);
    FreeAndNil(vGlyphNormals);
    raise;
  end;

  with aMesh do
  begin
    Positions := vGlyphVertices;
    Normals := vGlyphNormals;
    TexCoords := vGlyphTexCoords;
    Indices := Copy(vGlyphIndices, 0, Length(vGlyphIndices));
    FaceType := ftTriangles;
  end;
end;

procedure TVF_Vectoriser.CookContour(var aMesh: TVF_GlyphMesh;
  zNormal: Double = VF_FRONT_FACING);
const
  k = 1 / 64;
var
  I, c, p: Cardinal;
  StartIndex, index: Integer;
  Contour: TVF_Contour;
  v: TGLUAffineVector;
  v3: vec3;
  VDA: IVectorDataAccess;

  procedure EmitVertex;
  begin
    v.SetScale(k);
    v3[0] := v.v[0];
    v3[1] := v.v[1];
    v3[2] := v.v[2];
    vGlyphVertices.Add(v3);
  end;

begin
  vGlyphVertices := TVec3List.Create;
  SetLength(vGlyphIndices, PointCount);
  try
    I := 0;
    Index := 0;
    // Line loops but made with segment
    for c := 0 to ContourCount - 1 do
    begin
      Contour := FContourList[c];
      StartIndex := Index;
      v := Contour.FPointList[0];
      EmitVertex;
      Inc(Index);
      for p := 1 to Contour.PointCount - 2 do
      begin
        v := Contour.FPointList[p];
        EmitVertex;
        vGlyphIndices[I] := Index;
        Inc(I);
        vGlyphIndices[I] := Index;
        Inc(I);
        Inc(Index);
      end;
      v := Contour.FPointList[Contour.PointCount - 1];
      EmitVertex();
      vGlyphIndices[I] := Index;
      Inc(I);
      vGlyphIndices[I] := Index;
      Inc(I);
      vGlyphIndices[I] := StartIndex;
      Inc(I);
      Inc(Index);
    end;

    if vGlyphVertices.Count > 0 then
    begin
      VDA := TVectorDataAccess.Create(vGlyphVertices.GetItemAddr(0), vtFloat, 3, 3*SizeOf(Single), vGlyphVertices.Count);
      vGlyphNormals := MeshUtils.ComputeTriangleNormals(True, VDA, vGlyphIndices);
      vGlyphTexCoords := MeshUtils.ComputeTriangleTexCoords(VDA, vGlyphIndices);
    end;

  except
    FreeAndNil(vGlyphVertices);
    FreeAndNil(vGlyphTexCoords);
    FreeAndNil(vGlyphNormals);
    raise;
  end;

  with aMesh do
  begin
    Positions := vGlyphVertices;
    Normals := vGlyphNormals;
    TexCoords := vGlyphTexCoords;
    Indices := Copy(vGlyphIndices, 0, Length(vGlyphIndices));
    FaceType := ftLines;
  end;
end;

function TVF_Vectoriser.PointCount: Integer;
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

constructor TVF_PolyGlyph.Create(AGlyph: FT_GlyphSlot; aThickness: Single = 0);
begin
  inherited Create(AGlyph);

  if Ord(FT_GLYPH_FORMAT_OUTLINE) <> AGlyph.format then
  begin
    FErr := $14; // Invalid_Outline
    Exit;
  end;

  FVectoriser := TVF_Vectoriser.Create(AGlyph);
  if (FVectoriser.ContourCount > 0) and (FVectoriser.PointCount() >= 3) then
    FVectoriser.CookPolygon(FMesh, True);
end;

{$ENDREGION}
{$REGION 'TVF_ExtrGlyph'}

// ------------------
// ------------------ TVF_ExtrGlyph ------------------
// ------------------
constructor TVF_ExtrGlyph.Create(AGlyph: FT_GlyphSlot; aThickness: Single);
const
  k = 1 / 64;
var
  tempPositions: TVec3List;
  tempIndices: TIntegerArray;
  VDA1, VDA2, VDA3: IVectorDataAccess;
  ListArray: TAbstractDataListArray;
  MirrorMatrix: TMatrix;
  gluPoint: TGLUAffineVector;
  zOffset, p1, p2, p3, p4: TVector;
  c, I, J, N, nextIndex, index: Integer;
  Contour: TVF_Contour;
  numberOfPoints: Cardinal;
begin
  inherited Create(AGlyph);

  if Ord(FT_GLYPH_FORMAT_OUTLINE) <> AGlyph.format then
  begin
    FErr := $14; // Invalid_Outline
    Exit;
  end;

  FVectoriser := TVF_Vectoriser.Create(AGlyph);
  if (FVectoriser.ContourCount > 0) and (FVectoriser.PointCount() >= 3) then
    FVectoriser.CookPolygon(FMesh, False);

  if (FVectoriser.ContourCount > 0) and (FVectoriser.PointCount() >= 3) then
  begin
    tempPositions := TVec3List.Create;
    tempPositions.Join(FMesh.Positions, TMatrix.IdentityMatrix);
    MeshUtils.Join(tempIndices, FMesh.Indices, 0);
    try
      zOffset := TVector.Make(0, 0, -aThickness);
      MirrorMatrix := TMatrix.TranslationMatrix(zOffset);
      MirrorMatrix := MirrorMatrix * TMatrix.ReflectionMatrix
        (TVector.Null, zOffset.Normalize);
      FMesh.Positions.Join(tempPositions, MirrorMatrix);
      MeshUtils.Join(FMesh.Indices, tempIndices, tempPositions.Count);

      Index := FMesh.Positions.Count;
      J := Length(FMesh.Indices);
      SetLength(FMesh.Indices, J+6*(FVectoriser.PointCount));
      for c := 0 to FVectoriser.ContourCount - 1 do
      begin
        Contour := FVectoriser.Contour[c];
        numberOfPoints := Contour.PointCount;
        if numberOfPoints > 2 then
        begin
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
            // Увы нет смысла экономить на вершинах
            // при расчете нормалей они должны быть распакованы
            FMesh.Positions.Add(p1.vec3);
            FMesh.Positions.Add(p2.vec3);
            FMesh.Positions.Add(p3.vec3);
            FMesh.Positions.Add(p3.vec3);
            FMesh.Positions.Add(p2.vec3);
            FMesh.Positions.Add(p4.vec3);
            for N := J to J+5 do
            begin
              FMesh.Indices[N] := Index;
              Inc(Index);
            end;
            Inc(J, 6);
          end;
        end;
      end;
      if FMesh.Positions.Count > 2 then
      begin
        VDA1 := TVectorDataAccess.Create(FMesh.Positions.Data, vtFloat, 3, 3*SizeOf(Single), FMesh.Positions.Count);
        FMesh.Normals := MeshUtils.ComputeTriangleNormals(True, VDA1, FMesh.Indices);
        FMesh.TexCoords := MeshUtils.ComputeTriangleTexCoords(VDA1, FMesh.Indices);
        VDA2 := TVectorDataAccess.Create(FMesh.Normals.Data, vtFloat, 3, 3*SizeOf(Single), FMesh.Normals.Count);
        VDA3 := TVectorDataAccess.Create(FMesh.TexCoords.Data, vtFloat, 2, 2*SizeOf(Single), FMesh.TexCoords.Count);
        MeshUtils.WeldVertices([VDA1, VDA2, VDA3], ListArray, FMesh.Indices);
        FMesh.Positions.Destroy;
        FMesh.Normals.Destroy;
        FMesh.TexCoords.Destroy;
        FMesh.Positions := ListArray[0] as TVec3List;
        FMesh.Normals := ListArray[1] as TVec3List;
        FMesh.TexCoords := ListArray[2] as TVec2List;
      end
      else
      begin
        // В случае непонятного бага с глифом
        FMesh.Positions.Clear;
        FMesh.Normals := TVec3List.Create;
        FMesh.TexCoords := TVec2List.Create;
        SetLength(FMesh.Indices, 0);
      end;
    finally
      tempPositions.Destroy;
    end;
  end;
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
  : Cardinal; APen: TVector; var aMesh: TVF_GlyphMesh): TVector;
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
      adv := FGlyphList[FCharMap.GlyphListIndex(ACharacterCode)].Join(aMesh, APen);

    Result := TVector.Null;
    Result[0] := KernAdvance.V[0] + adv;
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

function TVF_Font.CreateVertexObject(const AStr: string): TVertexObject;
var
  I: Integer;
  G, ng: Cardinal;
  VO: TVertexObject;
  Mesh: TVF_GlyphMesh;
  Pen: TVector;
  Attr: TAttribBuffer;
begin
  VO := TVertexObject.Create;
  Mesh.FaceType := ftTriangles;
  Mesh.Positions := TVec3List.Create;
  Mesh.Normals := TVec3List.Create;
  Mesh.TexCoords := TVec2List.Create;
  SetLength(Mesh.Indices, 0);
  Pen := TVector.Null;

  try

    for I := 1 to Length(AStr) do
    begin
      GetGlyphs(AStr, I, G, ng);
      CheckGlyph(G);
      CheckGlyph(ng);
      Pen := Pen + FGlyphList.AddToMesh(G, ng, Pen, Mesh);
    end;

    Attr := TAttribBuffer.CreateAndSetup(CAttribSematics[atVertex].Name, 3,
      vtFloat, 0, btArray);
    Attr.Buffer.Allocate(Mesh.Positions.Size, Mesh.Positions.Data);
    Attr.Buffer.SetDataHandler(Mesh.Positions);
    Attr.SetAttribSemantic(atVertex);
    VO.AddAttrib(Attr, True);

    Attr := TAttribBuffer.CreateAndSetup(CAttribSematics[atNormal].Name, 3,
      vtFloat, 0, btArray);
    Attr.Buffer.Allocate(Mesh.Normals.Size, Mesh.Normals.Data);
    Attr.Buffer.SetDataHandler(Mesh.Normals);
    Attr.SetAttribSemantic(atNormal);
    VO.AddAttrib(Attr);

    Attr := TAttribBuffer.CreateAndSetup(CAttribSematics[atTexCoord0].Name, 2,
      vtFloat, 0, btArray);
    Attr.Buffer.Allocate(Mesh.TexCoords.Size, Mesh.TexCoords.Data);
    Attr.Buffer.SetDataHandler(Mesh.TexCoords);
    Attr.SetAttribSemantic(atTexCoord0);
    VO.AddAttrib(Attr);

    VO.SetIndices(Mesh.Indices);
    VO.FaceType := Mesh.FaceType;

  except
    VO.Destroy;
    Mesh.Positions.Destroy;
    Mesh.Normals.Destroy;
    Mesh.TexCoords.Destroy;
    raise;
  end;
  Result := VO;
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
  FThickness := 0;
end;

constructor TVF_ExtrudedFont.Create(pBufferBytes: FT_Byte_ptr;
  bufferSizeInBytes: Cardinal);
begin
  inherited Create(pBufferBytes, bufferSizeInBytes);
  FThickness := 0;
end;

function TVF_ExtrudedFont.MakeGlyph(G: Cardinal): TVF_Glyph;
var
  ftGlyph: FT_GlyphSlot;
  tempGlyph: TVF_ExtrGlyph;
begin
  ftGlyph := FFace.Glyph(G, FT_LOAD_NO_HINTING);

  if Assigned(ftGlyph) then
  begin
    tempGlyph := TVF_ExtrGlyph.Create(ftGlyph, FThickness);
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
  FJunk := TObjectList.Create;
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
var
  I: Integer;
begin
  if Assigned(FTesselator) then
  begin
    gluDeleteTess(FTesselator);
    FTesselator := nil;
  end;
  if Assigned(FFontCache) then
  begin
    FFontCache.ForEach(DestroyFonts);
    FreeAndNil(FFontCache);
  end;
  if Assigned(FLibrary) then
  begin
    FT_Done_FreeType(FLibrary);
    FLibrary := nil;
  end;
  if Assigned(FJunk) then
  begin
    for I := 0 to FJunk.Count - 1 do
      FJunk[I].Destroy;
    FreeAndNil(FJunk);
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
begin
  if VectorFontLibrary.FFontCache.Find(AFontLabel, font) then
    Result := font.CreateVertexObject(AText)
  else
    Result := nil;
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
    TVF_ExtrudedFont(font).Thickness := AnExtrusion;
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
