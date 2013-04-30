unit gluTokens;

interface

const
  GLU_INVALID_ENUM = 100900;
  GLU_INVALID_VALUE = 100901;
  GLU_OUT_OF_MEMORY = 100902;
  GLU_INCOMPATIBLE_GL_VERSION = 100903;
  GLU_VERSION = 100800;
  GLU_EXTENSIONS = 100801;
  GLU_TRUE = 1;
  GLU_FALSE = 0;
  GLU_SMOOTH = 100000;
  GLU_FLAT = 100001;
  GLU_NONE = 100002;
  GLU_POINT = 100010;
  GLU_LINE = 100011;
  GLU_FILL = 100012;
  GLU_SILHOUETTE = 100013;
  GLU_OUTSIDE = 100020;
  GLU_INSIDE = 100021;
  GLU_TESS_MAX_COORD = 1.0E150;
  GLU_TESS_WINDING_RULE = 100140;
  GLU_TESS_BOUNDARY_ONLY = 100141;
  GLU_TESS_TOLERANCE = 100142;
  GLU_TESS_WINDING_ODD = 100130;
  GLU_TESS_WINDING_NONZERO = 100131;
  GLU_TESS_WINDING_POSITIVE = 100132;
  GLU_TESS_WINDING_NEGATIVE = 100133;
  GLU_TESS_WINDING_ABS_GEQ_TWO = 100134;
  GLU_TESS_BEGIN = 100100; // TGLUTessBeginProc
  GLU_TESS_VERTEX = 100101; // TGLUTessVertexProc
  GLU_TESS_END = 100102; // TGLUTessEndProc
  GLU_TESS_ERROR = 100103; // TGLUTessErrorProc
  GLU_TESS_EDGE_FLAG = 100104; // TGLUTessEdgeFlagProc
  GLU_TESS_COMBINE = 100105; // TGLUTessCombineProc
  GLU_TESS_BEGIN_DATA = 100106; // TGLUTessBeginDataProc
  GLU_TESS_VERTEX_DATA = 100107; // TGLUTessVertexDataProc
  GLU_TESS_END_DATA = 100108; // TGLUTessEndDataProc
  GLU_TESS_ERROR_DATA = 100109; // TGLUTessErrorDataProc
  GLU_TESS_EDGE_FLAG_DATA = 100110; // TGLUTessEdgeFlagDataProc
  GLU_TESS_COMBINE_DATA = 100111; // TGLUTessCombineDataProc
  GLU_TESS_ERROR1 = 100151;
  GLU_TESS_ERROR2 = 100152;
  GLU_TESS_ERROR3 = 100153;
  GLU_TESS_ERROR4 = 100154;
  GLU_TESS_ERROR5 = 100155;
  GLU_TESS_ERROR6 = 100156;
  GLU_TESS_ERROR7 = 100157;
  GLU_TESS_ERROR8 = 100158;
  GLU_TESS_MISSING_BEGIN_POLYGON = GLU_TESS_ERROR1;
  GLU_TESS_MISSING_BEGIN_CONTOUR = GLU_TESS_ERROR2;
  GLU_TESS_MISSING_END_POLYGON = GLU_TESS_ERROR3;
  GLU_TESS_MISSING_END_CONTOUR = GLU_TESS_ERROR4;
  GLU_TESS_COORD_TOO_LARGE = GLU_TESS_ERROR5;
  GLU_TESS_NEED_COMBINE_CALLBACK = GLU_TESS_ERROR6;
  GLU_AUTO_LOAD_MATRIX = 100200;
  GLU_CULLING = 100201;
  GLU_SAMPLING_TOLERANCE = 100203;
  GLU_DISPLAY_MODE = 100204;
  GLU_PARAMETRIC_TOLERANCE = 100202;
  GLU_SAMPLING_METHOD = 100205;
  GLU_U_STEP = 100206;
  GLU_V_STEP = 100207;
  GLU_PATH_LENGTH = 100215;
  GLU_PARAMETRIC_ERROR = 100216;
  GLU_DOMAIN_DISTANCE = 100217;
  GLU_MAP1_TRIM_2 = 100210;
  GLU_MAP1_TRIM_3 = 100211;
  GLU_OUTLINE_POLYGON = 100240;
  GLU_OUTLINE_PATCH = 100241;
  GLU_NURBS_ERROR1 = 100251;
  GLU_NURBS_ERROR2 = 100252;
  GLU_NURBS_ERROR3 = 100253;
  GLU_NURBS_ERROR4 = 100254;
  GLU_NURBS_ERROR5 = 100255;
  GLU_NURBS_ERROR6 = 100256;
  GLU_NURBS_ERROR7 = 100257;
  GLU_NURBS_ERROR8 = 100258;
  GLU_NURBS_ERROR9 = 100259;
  GLU_NURBS_ERROR10 = 100260;
  GLU_NURBS_ERROR11 = 100261;
  GLU_NURBS_ERROR12 = 100262;
  GLU_NURBS_ERROR13 = 100263;
  GLU_NURBS_ERROR14 = 100264;
  GLU_NURBS_ERROR15 = 100265;
  GLU_NURBS_ERROR16 = 100266;
  GLU_NURBS_ERROR17 = 100267;
  GLU_NURBS_ERROR18 = 100268;
  GLU_NURBS_ERROR19 = 100269;
  GLU_NURBS_ERROR20 = 100270;
  GLU_NURBS_ERROR21 = 100271;
  GLU_NURBS_ERROR22 = 100272;
  GLU_NURBS_ERROR23 = 100273;
  GLU_NURBS_ERROR24 = 100274;
  GLU_NURBS_ERROR25 = 100275;
  GLU_NURBS_ERROR26 = 100276;
  GLU_NURBS_ERROR27 = 100277;
  GLU_NURBS_ERROR28 = 100278;
  GLU_NURBS_ERROR29 = 100279;
  GLU_NURBS_ERROR30 = 100280;
  GLU_NURBS_ERROR31 = 100281;
  GLU_NURBS_ERROR32 = 100282;
  GLU_NURBS_ERROR33 = 100283;
  GLU_NURBS_ERROR34 = 100284;
  GLU_NURBS_ERROR35 = 100285;
  GLU_NURBS_ERROR36 = 100286;
  GLU_NURBS_ERROR37 = 100287;
  GLU_INVALID_OPERATION = 100904;
  GLU_CW = 100120;
  GLU_CCW = 100121;
  GLU_INTERIOR = 100122;
  GLU_EXTERIOR = 100123;
  GLU_UNKNOWN = 100124;
  GLU_BEGIN = GLU_TESS_BEGIN;
  GLU_VERTEX = GLU_TESS_VERTEX;
  GLU_END = GLU_TESS_END;
  GLU_ERROR = GLU_TESS_ERROR;
  GLU_EDGE_FLAG = GLU_TESS_EDGE_FLAG;
  GL_POINTS = $0000;
  GL_LINES = $0001;
  GL_LINE_LOOP = $0002;
  GL_LINE_STRIP = $0003;
  GL_TRIANGLES = $0004;
  GL_TRIANGLE_STRIP = $0005;
  GL_TRIANGLE_FAN = $0006;

type
  GLUenum = Cardinal;
  GLUfloat = Double;
  PGLUfloat = ^GLUfloat;

function gluErrorString(AnErrorCode: GLUenum): String;

implementation

type
  TGLUError = record
    Token: GLUenum;
    Message: String;
  end;

const
  Errors: array [0 .. 4] of TGLUError = ((Token: GLU_INVALID_ENUM;
    Message: 'invalid enumerant'), (Token: GLU_INVALID_VALUE;
    Message: 'invalid value'), (Token: GLU_OUT_OF_MEMORY;
    Message: 'out of memory'), (Token: GLU_INCOMPATIBLE_GL_VERSION;
    Message: 'incompatible gl version'), (Token: GLU_INVALID_OPERATION;
    Message: 'invalid operation'));

  gluNurbsErrors: array [0 .. 36] of String = ('spline order un-supported',
    'too few knots', 'valid knot range is empty',
    'decreasing knot sequence knot',
    'knot multiplicity greater than order of spline',
    'gluEndCurve() must follow gluBeginCurve()',
    'gluBeginCurve() must precede gluEndCurve()',
    'missing or extra geometric data',
    'can''t draw piecewise linear trimming curves',
    'missing or extra domain data', 'missing or extra domain data',
    'gluEndTrim() must precede gluEndSurface()',
    'gluBeginSurface() must precede gluEndSurface()',
    'curve of improper type passed as trim curve',
    'gluBeginSurface() must precede gluBeginTrim()',
    'gluEndTrim() must follow gluBeginTrim()',
    'gluBeginTrim() must precede gluEndTrim()', 'invalid or missing trim curve',
    'gluBeginTrim() must precede gluPwlCurve()',
    'piecewise linear trimming curve referenced twice',
    'piecewise linear trimming curve and nurbs curve mixed',
    'improper usage of trim data type', 'nurbs curve referenced twice',
    'nurbs curve and piecewise linear trimming curve mixed',
    'nurbs surface referenced twice', 'invalid property',
    'gluEndSurface() must follow gluBeginSurface()',
    'intersecting or misoriented trim curves', 'intersecting trim curves',
    'UNUSED', 'unconnected trim curves', 'unknown knot error',
    'negative vertex count encountered', 'negative byte-stride encounteed',
    'unknown type descriptor', 'null control point reference',
    'duplicate point on piecewise linear trimming curve');

  gluTessErrors: array [0 .. 6] of String = (' ',
    'gluTessBeginPolygon() must precede a gluTessEndPolygon()',
    'gluTessBeginContour() must precede a gluTessEndContour()',
    'gluTessEndPolygon() must follow a gluTessBeginPolygon()',
    'gluTessEndContour() must follow a gluTessBeginContour()',
    'a coordinate is too large', 'need combine callback');

function gluErrorString(AnErrorCode: GLUenum): String;
var
  i: INteger;
begin
  for i := 0 to High(Errors) do
  begin
    if (Errors[i].Token = AnErrorCode) then
      exit(Errors[i].Message);
  end;

  if ((AnErrorCode >= GLU_NURBS_ERROR1) and (AnErrorCode <= GLU_NURBS_ERROR37))
  then
    exit(gluNurbsErrors[AnErrorCode - (GLU_NURBS_ERROR1 - 1)]);
  if ((AnErrorCode >= GLU_TESS_ERROR1) and (AnErrorCode <= GLU_TESS_ERROR6))
  then
    exit(gluTessErrors[AnErrorCode - (GLU_TESS_ERROR1 - 1)]);
end;

end.
