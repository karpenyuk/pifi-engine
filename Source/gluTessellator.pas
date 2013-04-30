// Pascal port of GLU by YarUnderoaker (April 2013) for pifi-engine
// Based on Eric Veach (July 1994) code.

unit gluTessellator;

interface

uses
  gluTokens;

{$POINTERMATH ON}
{$DEFINE INLINE_ON}

// We cache vertex data for single-contour polygons so that we can
// try a quick-and-dirty decomposition first.
//
const
  TESS_MAX_CACHE = 100;

  // The begin/end calls must be properly nested.  We keep track of
  // the current state to enforce the ordering.
  //
type
  PGLUtesselator = ^TGLUtesselator;

  TessState = (T_DORMANT, T_IN_POLYGON, T_IN_CONTOUR);
  PGLUAffineVector = ^TGLUAffineVector;

  TGLUAffineVector = record
    V: array [0 .. 2] of GLUfloat;
    class function Make(x, y, z: GLUfloat): TGLUAffineVector; static;
    class operator Equal(const v1, v2: TGLUAffineVector): Boolean;
    class operator NotEqual(const v1, v2: TGLUAffineVector): Boolean;
    class operator Add(const v1, v2: TGLUAffineVector): TGLUAffineVector;
    procedure SetScale(k: GLUfloat);
    function IsNull: Boolean;
  end;

  PGLUVector = ^TGLUVector;
  TGLUVector = array [0 .. 3] of GLUfloat;
  TGLUData = array [0 .. 3] of Pointer;

  TCachedVertex = record
    coords: TGLUAffineVector;
    data: Pointer;
  end;

  PGLUhalfEdge = ^TGLUhalfEdge;

  PGLUvertex = ^TGLUvertex;

  TGLUvertex = record
    next: PGLUvertex; // next vertex (never NULL)
    prev: PGLUvertex; // previous vertex (never NULL)
    anEdge: PGLUhalfEdge; // a half-edge with this origin
    data: Pointer; // client's data

    // Internal data (keep hidden)
    coords: TGLUAffineVector; // vertex location in 3D
    s, t: GLUfloat; // projection onto the sweep plane
    pqHandle: LongInt; // to allow deletion from priority queue
  end;

  PGLUface = ^TGLUface;

  TGLUface = record
    next: PGLUface; // next face (never NULL)
    prev: PGLUface; // previous face (never NULL)
    anEdge: PGLUhalfEdge; // a half edge with this left face
    data: Pointer; // room for client's data
    // Internal data (keep hidden)
    trail: PGLUface; // "stack" for conversion to strips
    marked: Boolean; // flag for conversion to strips
    inside: Boolean; // this face is in the polygon interior
  end;

  PDictNode = ^TDictNode;

  PActiveRegion = ^TActiveRegion;

  TActiveRegion = record
    eUp: PGLUhalfEdge; // upper edge, directed right to left
    nodeUp: PDictNode; // dictionary node corresponding to eUp
    windingNumber: Integer; // used to determine which regions are
    // inside the polygon
    inside: Boolean; // is this region inside the polygon?
    sentinel: Boolean; // marks fake edges at t = +/-infinity
    dirty: Boolean; // marks regions where the upper or lower
    // edge has changed, but we haven't checked
    // whether they intersect yet
    fixUpperEdge: Boolean; // marks temporary edges introduced when
    // we process a "right vertex" (one without
    // any edges leaving to the right)
  end;

  TGLUhalfEdge = record
    next: PGLUhalfEdge; // doubly-linked list (prev==Sym->next)
    Sym: PGLUhalfEdge; // same edge, opposite direction
    Onext: PGLUhalfEdge; // next edge CCW around origin
    Lnext: PGLUhalfEdge; // next edge CCW around left face
    Org: PGLUvertex; // origin vertex (Overtex too long)
    Lface: PGLUface; // left face
    // Internal data (keep hidden)
    activeRegion: PActiveRegion; // a region with this upper edge (sweep.c)
    winding: Integer; // change in winding number when crossing
    // from the right face to the left face
  private
    function GetDnext: PGLUhalfEdge;
    function GetDprev: PGLUhalfEdge;
    function GetDst: PGLUvertex;
    function GetLprev: PGLUhalfEdge;
    function GetOprev: PGLUhalfEdge;
    function GetRface: PGLUface;
    function GetRnext: PGLUhalfEdge;
    function GetRprev: PGLUhalfEdge;
    procedure SetDnext(const Value: PGLUhalfEdge);
    procedure SetDprev(const Value: PGLUhalfEdge);
    procedure SetDst(const Value: PGLUvertex);
    procedure SetLprev(const Value: PGLUhalfEdge);
    procedure SetOprev(const Value: PGLUhalfEdge);
    procedure SetRface(const Value: PGLUface);
    procedure SetRnext(const Value: PGLUhalfEdge);
    procedure SetRprev(const Value: PGLUhalfEdge);
  public
    property Rface: PGLUface read GetRface write SetRface;
    property Dst: PGLUvertex read GetDst write SetDst;
    property Oprev: PGLUhalfEdge read GetOprev write SetOprev;
    property Lprev: PGLUhalfEdge read GetLprev write SetLprev;
    property Dprev: PGLUhalfEdge read GetDprev write SetDprev;
    property Rprev: PGLUhalfEdge read GetRprev write SetRprev;
    property Dnext: PGLUhalfEdge read GetDnext write SetDnext;
    property Rnext: PGLUhalfEdge read GetRnext write SetRnext;
  end;

  PGLUmesh = ^TGLUmesh;

  TGLUmesh = record
    vHead: TGLUvertex; // dummy header for vertex list
    fHead: TGLUface; // dummy header for face list
    eHead: TGLUhalfEdge; // dummy header for edge list
    eHeadSym: TGLUhalfEdge; // and its symmetric counterpart
  end;

  TPQkey = Pointer;
  PPQkey = ^TPQkey;
  PPPQkey = ^PPQkey;
  TPQhandle = LongInt;
  TPQleq = function(key1, key2: TPQkey): Boolean;

  PPQnode = ^TPQnode;

  TPQnode = record
    handle: TPQhandle;
  end;

  PPQhandleElem = ^TPQhandleElem;

  TPQhandleElem = record
    key: TPQkey;
    node: TPQhandle;
  end;

  PPriorityQ = ^TPriorityQ;

  TPriorityQ = record
    nodes: PPQnode;
    handles: PPQhandleElem;
    size: LongInt;
    max: LongInt;
    freeList: TPQhandle;
    initialized: Boolean;
    leq: TPQleq;
  end;

  PHeapPriorityQ = ^THeapPriorityQ;

  THeapPriorityQ = record
    heap: PPriorityQ;
    keys: PPQkey;
    order: PPPQkey;
    size: TPQhandle;
    max: TPQhandle;
    initialized: Boolean;
    leq: TPQleq;
  end;

  TDictKey = Pointer;

  TDictNode = record
    key: TDictKey;
    next: PDictNode;
    prev: PDictNode;
  end;

  TDictLeq = function(frame: PGLUtesselator; key1, key2: PActiveRegion)
    : Boolean;

  PDict = ^TDict;

  TDict = record
    head: TDictNode;
    frame: Pointer;
    leq: TDictLeq;
  end;

  TBeginCallBack = procedure(aType: GLUenum);
  TEdgeFlagCallBack = procedure(boundaryEdge: Boolean);
  TVertexCallBack = procedure(data: Pointer);
  TEndCallBack = procedure();
  TMeshCallBeck = procedure(mesh: PGLUmesh);
  TErrorCallBack = procedure(errnum: GLUenum);
  TCombineCallBack = procedure(const coords: TGLUAffineVector;
    const data: TGLUData; const weight: TGLUVector; var outData: Pointer);

  TBeginDataCallBack = procedure(aType: GLUenum; polygonData: Pointer);
  TEdgeFlagDataCallBack = procedure(boundaryEdge: Boolean;
    polygonData: Pointer);
  TVertexDataCallBack = procedure(data: Pointer; polygonData: Pointer);
  TEndDataCallBack = procedure(polygonData: Pointer);
  TErrorDataCallBack = procedure(errnum: GLUenum; polygonData: Pointer);
  TCombineDataCallBack = procedure(const coords: TGLUAffineVector;
    const data: TGLUData; const weight: TGLUVector; var outData: Pointer;
    polygonData: Pointer);

  TGLUtesselator = record
  private
    { state needed for collecting the input data }
    state: TessState; // what begin/end calls have we seen?

    lastEdge: PGLUhalfEdge; // lastEdge->Org is the most recent vertex
    mesh: PGLUmesh; // stores the input contours, and eventually
    // the tessellation itself

    callError: TErrorCallBack;

    // ** state needed for projecting onto the sweep plane **
    normal: TGLUAffineVector; // user-specified normal (if provided)
    sUnit: TGLUAffineVector; // unit vector in s-direction (debugging)
    tUnit: TGLUAffineVector; // unit vector in t-direction (debugging)

    // ** state needed for the line sweep **
    relTolerance: GLUfloat; // tolerance for merging features
    windingRule: GLUenum; // rule for determining polygon interior
    fatalError: Boolean; // fatal error: needed combine callback

    dict: PDict; // edge dictionary for sweep line
    pq: PHeapPriorityQ; // priority queue of vertex events
    event: PGLUvertex; // current sweep event being processed

    callCombine: TCombineCallBack;

    // ** state needed for rendering callbacks (see render.c) **
    flagBoundary: Boolean; // mark boundary edges (use EdgeFlag)
    boundaryOnly: Boolean; // Extract contours, not triangles
    // list of triangles which could not be rendered as strips or fans
    lonelyTriList: PGLUface;
    callBegin: TBeginCallBack;
    callEdgeFlag: TEdgeFlagCallBack;
    callVertex: TVertexCallBack;
    callEnd: TEndCallBack;
    callMesh: TMeshCallBeck;
    // ** state needed to cache single-contour polygons for renderCache()

    emptyCache: Boolean; // empty cache on next vertex() call
    cacheCount: Integer; // number of cached vertices
    cache: array [0 .. TESS_MAX_CACHE - 1] of TCachedVertex; // the vertex data

    // ** rendering callbacks that also pass polygon data  **
    callBeginData: TBeginDataCallBack;
    callEdgeFlagData: TEdgeFlagDataCallBack;
    callVertexData: TVertexDataCallBack;
    callEndData: TEndDataCallBack;
    callErrorData: TErrorDataCallBack;
    callCombineData: TCombineDataCallBack;

    polygonData: Pointer; // client data for current polygon
  end;

function gluNewTess(): PGLUtesselator;
procedure gluTessBeginPolygon(tess: PGLUtesselator; polygon_data: Pointer);
procedure gluTessBeginContour(tess: PGLUtesselator);
procedure gluTessVertex(tess: PGLUtesselator; const coords: TGLUAffineVector;
  data: Pointer = nil);
procedure gluTessEndContour(tess: PGLUtesselator);
procedure gluTessEndPolygon(tess: PGLUtesselator);
procedure gluTessProperty(tess: PGLUtesselator; which: GLUenum;
  Value: GLUfloat);
procedure gluTessNormal(tess: PGLUtesselator; x, y, z: GLUfloat);
procedure gluTessCallback(tess: PGLUtesselator; which: GLUenum; fn: Pointer);
procedure gluDeleteTess(tess: PGLUtesselator);

implementation

const
  GLU_TESS_DEFAULT_TOLERANCE = 0.0;
  GLU_TESS_MESH = 100112;
  SIGN_INCONSISTENT = 2;
  S_UNIT_X = 1.0;
  S_UNIT_Y = 0.0;
  PQ_INIT_SIZE = 32;
  SENTINEL_COORD = 4.0 * GLU_TESS_MAX_COORD;
  LONG_MAX = High(LongInt);

type
  PEdgePair = ^TEdgePair;

  TEdgePair = record
    e: TGLUhalfEdge;
    eSym: TGLUhalfEdge;
  end;

  TRenderProc = procedure(tess: PGLUtesselator; e: PGLUhalfEdge; s: LongInt);

  // This structure remembers the information we need about a primitive
  // to be able to render it later, once we have determined which
  // primitive is able to use the most triangles.
  //
  TFaceCount = record

    size: LongInt; // number of triangles used
    eStart: PGLUhalfEdge; // edge where this primitive starts
    render: TRenderProc; // routine to render this primitive
  end;

  gluException = class(TObject);

procedure noBegin(aType: GLUenum);
begin
end;

procedure noEdgeFlag(boundaryEdge: Boolean);
begin
end;

procedure noVertex(data: Pointer);
begin
end;

procedure noEnd();
begin
end;

procedure noMesh(mesh: TGLUmesh);
begin
end;

procedure noError(errnum: GLUenum);
begin
end;

procedure noCombine(coords: TGLUAffineVector; const data: TGLUData;
  const weight: TGLUVector; var outData: Pointer);
begin
end;

procedure noBeginData(aType: GLUenum; polygonData: Pointer);
begin
end;

procedure noEdgeFlagData(boundaryEdge: Boolean; polygonData: Pointer);
begin
end;

procedure noVertexData(data: Pointer; polygonData: Pointer);
begin
end;

procedure noEndData(polygonData: Pointer);
begin
end;

procedure noErrorData(errnum: GLUenum; polygonData: Pointer);
begin
end;

procedure noCombineData(coords: TGLUAffineVector; const data: TGLUData;
  const weight: TGLUVector; var outData: Pointer; polygonData: Pointer);
begin
end;

function IsCallBeginData(tess: PGLUtesselator): Boolean;
{$IFDEF INLINE_ON} inline; {$ENDIF}
begin
  Result := @tess.callBeginData <> @noBeginData;
end;

function IsCallEdgeFlagData(tess: PGLUtesselator): Boolean;
{$IFDEF INLINE_ON} inline; {$ENDIF}
begin
  Result := @tess.callEdgeFlagData <> @noEdgeFlagData;
end;

function IsCallVertexData(tess: PGLUtesselator): Boolean;
{$IFDEF INLINE_ON} inline; {$ENDIF}
begin
  Result := @tess.callVertexData <> @noVertexData;
end;

function IsCallEndData(tess: PGLUtesselator): Boolean;
{$IFDEF INLINE_ON} inline; {$ENDIF}
begin
  Result := @tess.callEndData <> @noEndData;
end;

function IsCallErrorData(tess: PGLUtesselator): Boolean;
{$IFDEF INLINE_ON} inline; {$ENDIF}
begin
  Result := @tess.callErrorData <> @noErrorData;
end;

function IsCallCombineData(tess: PGLUtesselator): Boolean;
{$IFDEF INLINE_ON} inline; {$ENDIF}
begin
  Result := @tess.callCombineData <> @noCombineData;
end;

function IsCallMesh(tess: PGLUtesselator): Boolean; {$IFDEF INLINE_ON} inline;
{$ENDIF}
begin
  Result := @tess.callMesh = @noMesh;
end;

procedure SweepEvent(tess: PGLUtesselator; vEvent: PGLUvertex); forward;
procedure AddRightEdges(tess: PGLUtesselator; regUp: PActiveRegion;
  eFirst, eLast, eTopLeft: PGLUhalfEdge; cleanUp: Boolean); forward;
procedure ConnectLeftVertex(tess: PGLUtesselator; vEvent: PGLUvertex); forward;

function gluNewTess(): PGLUtesselator;
var
  tess: PGLUtesselator;
begin
  // Only initialize fields which can be changed by the api.  Other fields
  // are initialized where they are used.
  //

  GetMem(tess, SizeOf(TGLUtesselator));
  if tess = nil then
    exit(nil);

  tess.state := T_DORMANT;

  tess.normal.V[0] := 0;
  tess.normal.V[1] := 0;
  tess.normal.V[2] := 0;

  tess.relTolerance := GLU_TESS_DEFAULT_TOLERANCE;
  tess.windingRule := GLU_TESS_WINDING_ODD;
  tess.flagBoundary := FALSE;
  tess.boundaryOnly := FALSE;

  tess.callBegin := @noBegin;
  tess.callEdgeFlag := @noEdgeFlag;
  tess.callVertex := @noVertex;
  tess.callEnd := @noEnd;

  tess.callError := @noError;
  tess.callCombine := @noCombine;
  tess.callMesh := @noMesh;

  tess.callBeginData := @noBeginData;
  tess.callEdgeFlagData := @noEdgeFlagData;
  tess.callVertexData := @noVertexData;
  tess.callEndData := @noEndData;
  tess.callErrorData := @noErrorData;
  tess.callCombineData := @noCombineData;

  tess.polygonData := nil;

  Result := tess;
end;

// will free all storage for any valid mesh.

procedure meshDeleteMesh(mesh: PGLUmesh);
var
  f: PGLUface;
  fNext: PGLUface;
  V: PGLUvertex;
  vNext: PGLUvertex;
  e: PGLUhalfEdge;
  eNext: PGLUhalfEdge;
begin

  f := mesh.fHead.next;
  while (f <> @mesh.fHead) do
  begin
    fNext := f.next;
    FreeMem(f);
    f := fNext;
  end;

  V := mesh.vHead.next;
  while (V <> @mesh.vHead) do
  begin
    vNext := V.next;
    FreeMem(V);
    V := vNext;
  end;

  e := mesh.eHead.next;
  while (e <> @mesh.eHead) do
  begin
    eNext := e.next;
    FreeMem(e);
    e := eNext;
  end;

  FreeMem(mesh);
end;

procedure MakeDormant(tess: PGLUtesselator);
begin
  // Return the tessellator to its original dormant state. */
  if (tess.mesh = nil) then
    meshDeleteMesh(tess.mesh);
  tess.state := T_DORMANT;
  tess.lastEdge := nil;
  tess.mesh := nil;
end;

procedure GotoState(tess: PGLUtesselator; newState: TessState);
begin
  while (tess.state <> newState) do
  begin
    // We change the current state one level at a time, to get to
    // the desired state.
    //
    if (tess.state < newState) then
    begin
      case (tess.state) of
        T_DORMANT:
          begin
            if IsCallErrorData(tess) then
              tess.callErrorData(GLU_TESS_MISSING_BEGIN_POLYGON,
                tess.polygonData)
            else
              tess.callError(GLU_TESS_MISSING_BEGIN_POLYGON);
            gluTessBeginPolygon(tess, nil);
          end;
        T_IN_POLYGON:
          begin
            if IsCallErrorData(tess) then
              tess.callErrorData(GLU_TESS_MISSING_BEGIN_CONTOUR,
                tess.polygonData)
            else
              tess.callError(GLU_TESS_MISSING_BEGIN_CONTOUR);
            gluTessBeginContour(tess);
          end;
      else
        break;
      end;
    end
    else
    begin
      case (tess.state) of
        T_IN_CONTOUR:
          begin
            if IsCallErrorData(tess) then
              tess.callErrorData(GLU_TESS_MISSING_END_CONTOUR, tess.polygonData)
            else
              tess.callError(GLU_TESS_MISSING_END_CONTOUR);
            gluTessEndContour(tess);
          end;
        T_IN_POLYGON:
          begin
            if IsCallErrorData(tess) then
              tess.callErrorData(GLU_TESS_MISSING_END_POLYGON, tess.polygonData)
            else
              tess.callError(GLU_TESS_MISSING_END_POLYGON);
            MakeDormant(tess);
          end;
      else
        break;
      end;
    end;
  end;
end;

procedure gluDeleteTess(tess: PGLUtesselator);
begin
  if (tess.state <> T_DORMANT) then
    GotoState(tess, T_DORMANT);
  FreeMem(tess);
end;

procedure gluTessProperty(tess: PGLUtesselator; which: GLUenum;
  Value: GLUfloat);
var
  windingRule: GLUenum;
begin

  case (which) of
    GLU_TESS_TOLERANCE:
      if not((Value < 0.0) or (Value > 1.0)) then
      begin
        tess.relTolerance := Value;
        exit;
      end;
    GLU_TESS_WINDING_RULE:
      begin
        windingRule := Round(Value);
        if (windingRule = Value) then
          case (windingRule) of
            GLU_TESS_WINDING_ODD, GLU_TESS_WINDING_NONZERO,
              GLU_TESS_WINDING_POSITIVE, GLU_TESS_WINDING_NEGATIVE,
              GLU_TESS_WINDING_ABS_GEQ_TWO:
              begin
                tess.windingRule := windingRule;
                exit;
              end;
          end;
      end;

    GLU_TESS_BOUNDARY_ONLY:
      begin
        tess.boundaryOnly := (Value <> 0);
        exit;
      end;
  else
    begin
      if IsCallErrorData(tess) then
        tess.callErrorData(GLU_INVALID_ENUM, tess.polygonData)
      else
        tess.callError(GLU_INVALID_ENUM);
      exit;
    end;
  end;
  if IsCallErrorData(tess) then
    tess.callErrorData(GLU_INVALID_VALUE, tess.polygonData)
  else
    tess.callError(GLU_INVALID_VALUE);
end;

// Returns tessellator property */
procedure gluGetTessProperty(tess: PGLUtesselator; which: GLUenum;
  var Value: GLUfloat);
begin
  case (which) of
    GLU_TESS_TOLERANCE:
      begin
        // tolerance should be in range [0..1] */
        assert((0.0 <= tess.relTolerance) and (tess.relTolerance <= 1.0));
        Value := tess.relTolerance;
      end;
    GLU_TESS_WINDING_RULE:
      begin
        assert((tess.windingRule = GLU_TESS_WINDING_ODD) or
          (tess.windingRule = GLU_TESS_WINDING_NONZERO) or
          (tess.windingRule = GLU_TESS_WINDING_POSITIVE) or
          (tess.windingRule = GLU_TESS_WINDING_NEGATIVE) or
          (tess.windingRule = GLU_TESS_WINDING_ABS_GEQ_TWO));
        Value := Int(tess.windingRule);
      end;
    GLU_TESS_BOUNDARY_ONLY:
      begin
        assert((tess.boundaryOnly = TRUE) or (tess.boundaryOnly = FALSE));
        Value := Integer(tess.boundaryOnly);
      end;
  else
    begin
      Value := 0.0;
      if IsCallErrorData(tess) then
        tess.callErrorData(GLU_INVALID_ENUM, tess.polygonData)
      else
        tess.callError(GLU_INVALID_ENUM);
    end;
  end;
end;

procedure gluTessNormal(tess: PGLUtesselator; x, y, z: GLUfloat);
begin
  tess.normal.V[0] := x;
  tess.normal.V[1] := y;
  tess.normal.V[2] := z;
end;

procedure gluTessCallback(tess: PGLUtesselator; which: GLUenum; fn: Pointer);
begin
  case (which) of
    GLU_TESS_BEGIN:
      begin
        if fn = nil then
          tess.callBegin := @noBegin
        else
          tess.callBegin := fn;
      end;
    GLU_TESS_BEGIN_DATA:
      begin
        if fn = nil then
          tess.callBeginData := @noBeginData
        else
          tess.callBeginData := fn;
      end;
    GLU_TESS_EDGE_FLAG:
      begin
        if fn = nil then
          tess.callEdgeFlag := @noEdgeFlag
        else
          tess.callEdgeFlag := fn;
        // If the client wants boundary edges to be flagged,
        // we render everything as separate triangles (no strips or fans).
        //
        tess.flagBoundary := (fn <> nil);
      end;
    GLU_TESS_EDGE_FLAG_DATA:
      begin
        if fn = nil then
          tess.callEdgeFlagData := @noEdgeFlagData
        else
          tess.callEdgeFlagData := fn;
        // If the client wants boundary edges to be flagged,
        // we render everything as separate triangles (no strips or fans).
        //
        tess.flagBoundary := (fn <> nil);
      end;
    GLU_TESS_VERTEX:
      if fn = nil then
        tess.callVertex := @noVertex
      else
        tess.callVertex := fn;
    GLU_TESS_VERTEX_DATA:
      if fn = nil then
        tess.callVertexData := @noVertexData
      else
        tess.callVertexData := fn;
    GLU_TESS_END:
      if fn = nil then
        tess.callEnd := @noEnd
      else
        tess.callEnd := fn;
    GLU_TESS_END_DATA:
      if fn = nil then
        tess.callEndData := @noEndData
      else
        tess.callEndData := fn;
    GLU_TESS_ERROR:
      if fn = nil then
        tess.callError := @noError
      else
        tess.callError := fn;
    GLU_TESS_ERROR_DATA:
      if fn = nil then
        tess.callErrorData := @noErrorData
      else
        tess.callErrorData := fn;
    GLU_TESS_COMBINE:
      if fn = nil then
        tess.callCombine := @noCombine
      else
        tess.callCombine := fn;
    GLU_TESS_COMBINE_DATA:
      if fn = nil then
        tess.callCombineData := @noCombineData
      else
        tess.callCombineData := fn;
    GLU_TESS_MESH:
      if fn = nil then
        tess.callMesh := @noMesh
      else
        tess.callMesh := fn;
  else
    if IsCallErrorData(tess) then
      tess.callErrorData(GLU_INVALID_ENUM, tess.polygonData)
    else
      tess.callError(GLU_INVALID_ENUM);
  end;
end;

// creates a new pair of half-edges which form their own loop.
// No vertex or face structures are allocated, but these must be assigned
// before the current edge operation is completed.
//
function MakeEdge(eNext: PGLUhalfEdge): PGLUhalfEdge;
var
  e: PGLUhalfEdge;
  eSym: PGLUhalfEdge;
  ePrev: PGLUhalfEdge;
  pair: PEdgePair;
begin
  GetMem(pair, SizeOf(TEdgePair));
  e := @pair.e;
  eSym := @pair.eSym;

  // Make sure eNext points to the first edge of the edge pair */
  if (eNext.Sym < eNext) then
    eNext := eNext.Sym;

  // Insert in circular doubly-linked list before eNext.
  // Note that the prev pointer is stored in Sym->next.

  ePrev := eNext.Sym.next;
  eSym.next := ePrev;
  ePrev.Sym.next := e;
  e.next := eNext;
  eNext.Sym.next := eSym;

  e.Sym := eSym;
  e.Onext := e;
  e.Lnext := eSym;
  e.Org := nil;
  e.Lface := nil;
  e.winding := 0;
  e.activeRegion := nil;

  eSym.Sym := e;
  eSym.Onext := eSym;
  eSym.Lnext := e;
  eSym.Org := nil;
  eSym.Lface := nil;
  eSym.winding := 0;
  eSym.activeRegion := nil;

  Result := e;
end;

// attaches a new vertex and makes it the
// origin of all edges in the vertex loop to which eOrig belongs. "vNext" gives
// a place to insert the new vertex in the global vertex list.  We insert
// the new vertex *before* vNext so that algorithms which walk the vertex
// list will not see the newly created vertices.
//
procedure MakeVertex(newVertex: PGLUvertex; eOrig: PGLUhalfEdge;
  vNext: PGLUvertex);
var
  e: PGLUhalfEdge;
  vPrev: PGLUvertex;
  vNew: PGLUvertex;
begin
  vNew := newVertex;
  assert(vNew <> nil);

  // insert in circular doubly-linked list before vNext
  vPrev := vNext.prev;
  vNew.prev := vPrev;
  vPrev.next := vNew;
  vNew.next := vNext;
  vNext.prev := vNew;

  vNew.anEdge := eOrig;
  vNew.data := nil;

  // leave coords, s, t undefined

  // fix other edges on this vertex loop
  e := eOrig;
  repeat
    e.Org := vNew;
    e := e.Onext;
  until e = eOrig;
end;

// attaches a new face and makes it the left
// face of all edges in the face loop to which eOrig belongs.  "fNext" gives
// a place to insert the new face in the global face list.  We insert
// the new face *before* fNext so that algorithms which walk the face
// list will not see the newly created faces.
//

procedure MakeFace(newFace: PGLUface; eOrig: PGLUhalfEdge; fNext: PGLUface);
var
  e: PGLUhalfEdge;
  fPrev: PGLUface;
  fNew: PGLUface;
begin
  fNew := newFace;

  // insert in circular doubly-linked list before fNext
  fPrev := fNext.prev;
  fNew.prev := fPrev;
  fPrev.next := fNew;
  fNew.next := fNext;
  fNext.prev := fNew;

  fNew.anEdge := eOrig;
  fNew.data := nil;
  fNew.trail := nil;
  fNew.marked := FALSE;

  // The new face is marked "inside" if the old one was.  This is a
  // convenience for the common case where a face has been split in two.
  //
  fNew.inside := fNext.inside;

  // fix other edges on this face loop
  e := eOrig;
  repeat
    e.Lface := fNew;
    e := e.Lnext;
  until e = eOrig;
end;

// creates one edge, two vertices, and a loop (face).
// The loop consists of the two new half-edges.
//
function meshMakeEdge(mesh: PGLUmesh): PGLUhalfEdge;
var
  newVertex1, newVertex2: PGLUvertex;
  newFace: PGLUface;
  e: PGLUhalfEdge;
begin
  GetMem(newVertex1, SizeOf(TGLUvertex));
  GetMem(newVertex2, SizeOf(TGLUvertex));
  GetMem(newFace, SizeOf(TGLUface));

  e := MakeEdge(@mesh.eHead);
  if e = nil then
    exit(nil);

  MakeVertex(newVertex1, e, @mesh.vHead);
  MakeVertex(newVertex2, e.Sym, @mesh.vHead);
  MakeFace(newFace, e, @mesh.fHead);
  Result := e;
end;

// destroys a vertex and removes it from the global
// vertex list. It updates the vertex loop to point to a given new vertex.
//
procedure KillVertex(vDel: PGLUvertex; newOrg: PGLUvertex);
var
  e, eStart: PGLUhalfEdge;
  vPrev: PGLUvertex;
  vNext: PGLUvertex;
begin
  eStart := vDel.anEdge;
  // change the origin of all affected edges
  e := eStart;
  repeat
    e.Org := newOrg;
    e := e.Onext;
  until (e = eStart);

  // delete from circular doubly-linked list
  vPrev := vDel.prev;
  vNext := vDel.next;
  vNext.prev := vPrev;
  vPrev.next := vNext;

  FreeMem(vDel);
end;

// destroys a face and removes it from the global face
// list. It updates the face loop to point to a given new face.
//
procedure KillFace(fDel: PGLUface; newLface: PGLUface);
var
  e, eStart: PGLUhalfEdge;
  fPrev: PGLUface;
  fNext: PGLUface;
begin
  eStart := fDel.anEdge;
  // change the left face of all affected edges
  e := eStart;
  repeat
    e.Lface := newLface;
    e := e.Lnext;
  until (e = eStart);

  // delete from circular doubly-linked list
  fPrev := fDel.prev;
  fNext := fDel.next;
  fNext.prev := fPrev;
  fPrev.next := fNext;

  FreeMem(fDel);
end;

// Is best described by the Guibas/Stolfi paper or the
// CS348a notes (see mesh.h). Basically it modifies the mesh so that
// a->Onext and b->Onext are exchanged. This can have various effects
// depending on whether a and b belong to different face or vertex rings.
// For more explanation see __gl_meshSplice() below.
//
procedure Splice(a, b: PGLUhalfEdge);
var
  aOnext, bOnext: PGLUhalfEdge;
begin
  aOnext := a.Onext;
  bOnext := b.Onext;

  aOnext.Sym.Lnext := b;
  bOnext.Sym.Lnext := a;
  a.Onext := bOnext;
  b.Onext := aOnext;
end;

// Tthe basic operation for changing the
// mesh connectivity and topology.  It changes the mesh so that
// eOrg->Onext <- OLD(eDst->Onext)
// eDst->Onext <- OLD(eOrg->Onext)
// where OLD(...) means the value before the meshSplice operation.
//
// This can have two effects on the vertex structure:
// - if eOrg->Org != eDst->Org, the two vertices are merged together
// - if eOrg->Org == eDst->Org, the origin is split into two vertices
// In both cases, eDst->Org is changed and eOrg->Org is untouched.
//
// Similarly (and independently) for the face structure,
// - if eOrg->Lface == eDst->Lface, one loop is split into two
// - if eOrg->Lface != eDst->Lface, two distinct loops are joined into one
// In both cases, eDst->Lface is changed and eOrg->Lface is unaffected.
//
// Some special cases:
// If eDst == eOrg, the operation has no effect.
// If eDst == eOrg->Lnext, the new face will have a single edge.
// If eDst == eOrg->Lprev, the old face will have a single edge.
// If eDst == eOrg->Onext, the new vertex will have a single edge.
// If eDst == eOrg->Oprev, the old vertex will have a single edge.
//

function meshSplice(eOrg, eDst: PGLUhalfEdge): Boolean;
var
  joiningLoops, joiningVertices: Boolean;
  newVertex: PGLUvertex;
  newFace: PGLUface;
begin
  joiningLoops := FALSE;
  joiningVertices := FALSE;

  if (eOrg = eDst) then
    exit(TRUE);

  if (eDst.Org <> eOrg.Org) then
  begin
    // We are merging two disjoint vertices -- destroy eDst.Org
    joiningVertices := TRUE;
    KillVertex(eDst.Org, eOrg.Org);
  end;

  if (eDst.Lface <> eOrg.Lface) then
  begin
    // We are connecting two disjoint loops -- destroy eDst.Lface
    joiningLoops := TRUE;
    KillFace(eDst.Lface, eOrg.Lface);
  end;

  // Change the edge structure
  Splice(eDst, eOrg);

  if (not joiningVertices) then
  begin
    GetMem(newVertex, SizeOf(TGLUvertex));
    // We split one vertex into two -- the new vertex is eDst.Org.
    // Make sure the old vertex points to a valid half-edge.
    //
    MakeVertex(newVertex, eDst, eOrg.Org);
    eOrg.Org.anEdge := eOrg;
  end;

  if (not joiningLoops) then
  begin
    GetMem(newFace, SizeOf(TGLUface));

    // We split one loop into two -- the new loop is eDst.Lface.
    // Make sure the old face points to a valid half-edge.
    //
    MakeFace(newFace, eDst, eOrg.Lface);
    eOrg.Lface.anEdge := eOrg;
  end;

  Result := TRUE;
end;

// creates a new edge eNew such that
// eNew == eOrg->Lnext, and eNew->Dst is a newly created vertex.
// eOrg and eNew will have the same left face.
//
function meshAddEdgeVertex(eOrg: PGLUhalfEdge): PGLUhalfEdge;
var
  eNewSym, eNew: PGLUhalfEdge;
  newVertex: PGLUvertex;
begin
  eNew := MakeEdge(eOrg);
  eNewSym := eNew.Sym;

  // Connect the new edge appropriately
  Splice(eNew, eOrg.Lnext);

  // Set the vertex and face information
  eNew.Org := eOrg.Dst;
  GetMem(newVertex, SizeOf(TGLUvertex));
  MakeVertex(newVertex, eNewSym, eNew.Org);
  eNew.Lface := eOrg.Lface;
  eNewSym.Lface := eOrg.Lface;
  Result := eNew;
end;

// splits eOrg into two edges eOrg and eNew,
// such that eNew == eOrg->Lnext.  The new vertex is eOrg->Dst == eNew->Org.
// eOrg and eNew will have the same left face.
//
function meshSplitEdge(eOrg: PGLUhalfEdge): PGLUhalfEdge;
var
  eNew, tempHalfEdge: PGLUhalfEdge;
begin
  tempHalfEdge := meshAddEdgeVertex(eOrg);

  eNew := tempHalfEdge.Sym;

  // Disconnect eOrg from eOrg->Dst and connect it to eNew->Org
  Splice(eOrg.Sym, eOrg.Sym.Oprev);
  Splice(eOrg.Sym, eNew);

  // Set the vertex and face information
  eOrg.Dst := eNew.Org;
  eNew.Dst.anEdge := eNew.Sym; // may have pointed to eOrg.Sym
  eNew.Rface := eOrg.Rface;
  eNew.winding := eOrg.winding; // copy old winding information
  eNew.Sym.winding := eOrg.Sym.winding;

  Result := eNew;
end;

function AddVertex(tess: PGLUtesselator; const coords: TGLUAffineVector;
  data: Pointer): Boolean;
var
  e: PGLUhalfEdge;
begin
  e := tess.lastEdge;
  if (e = nil) then
  begin
    // Make a self-loop (one vertex, one edge). */
    e := meshMakeEdge(tess.mesh);
    if (e = nil) then
      exit(FALSE);
    if not meshSplice(e, e.Sym) then
      exit(FALSE);
  end
  else
  begin
    // Create a new vertex and edge which immediately follow e
    // in the ordering around the left face.
    //
    if (meshSplitEdge(e) = nil) then
      exit(FALSE);
    e := e.Lnext;
  end;

  // The new vertex is now e.Org. */
  e.Org.data := data;
  e.Org.coords.V[0] := coords.V[0];
  e.Org.coords.V[1] := coords.V[1];
  e.Org.coords.V[2] := coords.V[2];

  // The winding of an edge says how the winding number changes as we
  // cross from the edge''s right face to its left face.  We add the
  // vertices in such an order that a CCW contour will add +1 to
  // the winding number of the region inside the contour.
  //
  e.winding := 1;
  e.Sym.winding := -1;

  tess.lastEdge := e;

  Result := TRUE;
end;

procedure CacheVertex(tess: PGLUtesselator; ACoords: TGLUAffineVector;
  AData: Pointer);
begin
  with tess.cache[tess.cacheCount] do
  begin
    data := AData;
    coords := ACoords;
  end;

  Inc(tess.cacheCount);
end;

// creates a new mesh with no edges, no vertices,
// and no loops (what we usually call a "face").
///
function meshNewMesh(): PGLUmesh;
var
  V: PGLUvertex;
  f: PGLUface;
  e: PGLUhalfEdge;
  eSym: PGLUhalfEdge;
  mesh: PGLUmesh;
begin
  GetMem(mesh, SizeOf(TGLUmesh));

  V := @mesh.vHead;
  f := @mesh.fHead;
  e := @mesh.eHead;
  eSym := @mesh.eHeadSym;

  V.next := V;
  V.prev := V;
  V.anEdge := nil;
  V.data := nil;

  f.next := f;
  f.prev := f;
  f.anEdge := nil;
  f.data := nil;
  f.trail := nil;
  f.marked := FALSE;
  f.inside := FALSE;

  e.next := e;
  e.Sym := eSym;
  e.Onext := nil;
  e.Lnext := nil;
  e.Org := nil;
  e.Lface := nil;
  e.winding := 0;
  e.activeRegion := nil;

  eSym.next := eSym;
  eSym.Sym := e;
  eSym.Onext := nil;
  eSym.Lnext := nil;
  eSym.Org := nil;
  eSym.Lface := nil;
  eSym.winding := 0;
  eSym.activeRegion := nil;

  Result := mesh;
End;

function emptyCache(tess: PGLUtesselator): Boolean;
var
  I: Integer;
begin
  tess.mesh := meshNewMesh();
  if (tess.mesh = nil) then
  begin
    exit(FALSE);
  end;

  for I := 0 to tess.cacheCount - 1 do
    with tess.cache[I] do
    begin
      if not AddVertex(tess, coords, data) then
        exit(FALSE);
    end;
  tess.cacheCount := 0;
  tess.emptyCache := FALSE;

  Result := TRUE;
end;

procedure gluTessVertex(tess: PGLUtesselator; const coords: TGLUAffineVector;
  data: Pointer);
var
  I: Integer;
  tooLarge: Boolean;
  x: GLUfloat;
  clamped: TGLUAffineVector;
begin
  tooLarge := FALSE;

  if (tess.state <> T_IN_CONTOUR) then
    GotoState(tess, T_IN_CONTOUR);

  if (tess.emptyCache) then
  begin
    if (not emptyCache(tess)) then
    begin
      if IsCallErrorData(tess) then
        tess.callErrorData(GLU_OUT_OF_MEMORY, tess.polygonData)
      else
        tess.callError(GLU_OUT_OF_MEMORY);
      exit;
    end;
    tess.lastEdge := nil;
  end;

  for I := 0 to 2 do
  begin
    x := coords.V[I];
    if (x < -GLU_TESS_MAX_COORD) then
    begin
      x := -GLU_TESS_MAX_COORD;
      tooLarge := TRUE;
    end;
    if (x > GLU_TESS_MAX_COORD) then
    begin
      x := GLU_TESS_MAX_COORD;
      tooLarge := TRUE;
    end;
    clamped.V[I] := x;
  end;
  if (tooLarge) then
  begin
    if IsCallErrorData(tess) then
      tess.callErrorData(GLU_TESS_COORD_TOO_LARGE, tess.polygonData)
    else
      tess.callError(GLU_TESS_COORD_TOO_LARGE);
  end;

  if (tess.mesh = nil) then
  begin
    if (tess.cacheCount < TESS_MAX_CACHE) then
    begin
      CacheVertex(tess, clamped, data);
      exit;
    end;
    if (not emptyCache(tess)) then
    begin
      if IsCallErrorData(tess) then
        tess.callErrorData(GLU_OUT_OF_MEMORY, tess.polygonData)
      else
        tess.callError(GLU_OUT_OF_MEMORY);
      exit;
    end;
  end;

  if (not AddVertex(tess, clamped, data)) then
  begin
    if IsCallErrorData(tess) then
      tess.callErrorData(GLU_OUT_OF_MEMORY, tess.polygonData)
    else
      tess.callError(GLU_OUT_OF_MEMORY);
  end;
end;

procedure gluTessBeginPolygon(tess: PGLUtesselator; polygon_data: Pointer);
begin
  if (tess.state <> T_DORMANT) then
    GotoState(tess, T_DORMANT);

  tess.state := T_IN_POLYGON;
  tess.cacheCount := 0;
  tess.emptyCache := FALSE;
  tess.mesh := nil;

  tess.polygonData := polygon_data;
end;

procedure gluTessBeginContour(tess: PGLUtesselator);
begin
  if (tess.state <> T_IN_POLYGON) then
    GotoState(tess, T_IN_POLYGON);

  tess.state := T_IN_CONTOUR;
  tess.lastEdge := nil;
  if (tess.cacheCount > 0) then
  begin
    // Just set a flag so we don't get confused by empty contours
    // -- these can be generated accidentally with the obsolete
    // NextContour() interface.
    //
    tess.emptyCache := TRUE;
  end;
end;

procedure gluTessEndContour(tess: PGLUtesselator);
begin
  if (tess.state <> T_IN_CONTOUR) then
    GotoState(tess, T_IN_CONTOUR);
  tess.state := T_IN_POLYGON;
end;

function ComputeNormal(tess: PGLUtesselator; var norm: TGLUAffineVector;
  check: Boolean): Integer; overload;
//
// If check==FALSE, we compute the polygon normal and place it in norm[].
// If check==TRUE, we check that each triangle in the fan from v0 has a
// consistent orientation with respect to norm[].  If triangles are
// consistently oriented CCW, return 1; if CW, return -1; if all triangles
// are degenerate return 0; otherwise (no consistent orientation) return
// SIGN_INCONSISTENT.
///
var
  v0, vc: Integer;
  dot, xc, yc, zc, xp, yp, zp: GLUfloat;
  n: TGLUAffineVector;
  sign: Integer;
begin
  v0 := 0;
  sign := 0;

  // Find the polygon normal.  It is important to get a reasonable
  // normal even when the polygon is self-intersecting (eg. a bowtie).
  // Otherwise, the computed normal could be very tiny, but perpendicular
  // to the true plane of the polygon due to numerical noise.  Then all
  // the triangles would appear to be degenerate and we would incorrectly
  // decompose the polygon as a fan (or simply not render it at all).
  //
  // We use a sum-of-triangles normal algorithm rather than the more
  // efficient sum-of-trapezoids method (used in CheckOrientation()
  // in normal.c).  This lets us explicitly reverse the signed area
  // of some triangles to get a reasonable normal in the self-intersecting
  // case.
  ///

  if (not check) then
  begin
    norm.V[0] := 0;
    norm.V[1] := 0;
    norm.V[2] := 0;
  end;

  vc := v0 + 1;
  xc := tess.cache[vc].coords.V[0] - tess.cache[v0].coords.V[0];
  yc := tess.cache[vc].coords.V[1] - tess.cache[v0].coords.V[1];
  zc := tess.cache[vc].coords.V[2] - tess.cache[v0].coords.V[2];
  Inc(vc);
  while (vc < tess.cacheCount) do
  begin
    xp := xc;
    yp := yc;
    zp := zc;
    xc := tess.cache[vc].coords.V[0] - tess.cache[v0].coords.V[0];
    yc := tess.cache[vc].coords.V[1] - tess.cache[v0].coords.V[1];
    zc := tess.cache[vc].coords.V[2] - tess.cache[v0].coords.V[2];

    // Compute (vp-v0) cross (vc-v0)
    n.V[0] := yp * zc - zp * yc;
    n.V[1] := zp * xc - xp * zc;
    n.V[2] := xp * yc - yp * xc;

    dot := n.V[0] * norm.V[0] + n.V[1] * norm.V[1] + n.V[2] * norm.V[2];
    if (not check) then
    begin
      // Reverse the contribution of back-facing triangles to get
      // a reasonable normal for self-intersecting polygons (see above)
      //
      if (dot >= 0) then
      begin
        norm.V[0] := norm.V[0] + n.V[0];
        norm.V[1] := norm.V[1] + n.V[1];
        norm.V[2] := norm.V[2] + n.V[2];
      end
      else
      begin
        norm.V[0] := norm.V[0] - n.V[0];
        norm.V[1] := norm.V[1] - n.V[1];
        norm.V[2] := norm.V[2] - n.V[2];
      end;
    end
    else
    begin
      if (dot <> 0) then
      begin
        // Check the new orientation for consistency with previous triangles */
        if (dot > 0) then
        begin
          if (sign < 0) then
          begin
            exit(SIGN_INCONSISTENT);
          end;
          sign := 1;
        end
        else
        begin
          if (sign > 0) then
          begin
            exit(SIGN_INCONSISTENT);
          end;
          sign := -1;
        end;
      end;
    end;
    Inc(vc);
  end;;

  Result := sign;
end;

function LongAxis(const V: TGLUAffineVector): Integer;
var
  I: Integer;
begin
  I := 0;

  if (ABS(V.V[1]) > ABS(V.V[0])) then
    I := 1;
  if (ABS(V.V[2]) > ABS(V.V[I])) then
    I := 2;

  Result := I;
end;

procedure ComputeNormal(tess: PGLUtesselator;
  var norm: TGLUAffineVector); overload;
var
  V, v1, v2: PGLUvertex;
  c, tLen2, maxLen2: GLUfloat;
  maxVal, minVal, d1, d2, tNorm: TGLUAffineVector;
  maxVert, minVert: array [0 .. 2] of PGLUvertex;
  vHead: PGLUvertex;
  I: Integer;
begin
  vHead := @tess.mesh.vHead;
  maxVal.V[0] := -2 * GLU_TESS_MAX_COORD;
  maxVal.V[1] := -2 * GLU_TESS_MAX_COORD;
  maxVal.V[2] := -2 * GLU_TESS_MAX_COORD;
  minVal.V[0] := 2 * GLU_TESS_MAX_COORD;
  minVal.V[1] := 2 * GLU_TESS_MAX_COORD;
  minVal.V[2] := 2 * GLU_TESS_MAX_COORD;

  V := vHead.next;
  while (V <> vHead) do
  begin
    for I := 0 to 2 do
    begin
      c := V.coords.V[I];
      if (c < minVal.V[I]) then
      begin
        minVal.V[I] := c;
        minVert[I] := V;
      end;
      if (c > maxVal.V[I]) then
      begin
        maxVal.V[I] := c;
        maxVert[I] := V;
      end;
    end;
    V := V.next
  end;

  // Find two vertices separated by at least 1/sqrt(3) of the maximum
  // distance between any two vertices

  I := 0;
  if (maxVal.V[1] - minVal.V[1] > maxVal.V[0] - minVal.V[0]) then
  begin
    I := 1;
  end;
  if (maxVal.V[2] - minVal.V[2] > maxVal.V[I] - minVal.V[I]) then
  begin
    I := 2;
  end;
  if (minVal.V[I] >= maxVal.V[I]) then
  begin
    // All vertices are the same -- normal doesn't matter
    norm.V[0] := 0;
    norm.V[1] := 0;
    norm.V[2] := 1;
    exit;
  end;

  // Look for a third vertex which forms the triangle with maximum area
  // (Length of normal == twice the triangle area)
  //
  maxLen2 := 0;
  v1 := minVert[I];
  v2 := maxVert[I];
  d1.V[0] := v1.coords.V[0] - v2.coords.V[0];
  d1.V[1] := v1.coords.V[1] - v2.coords.V[1];
  d1.V[2] := v1.coords.V[2] - v2.coords.V[2];
  V := vHead.next;
  while (V <> vHead) do
  begin
    d2.V[0] := V.coords.V[0] - v2.coords.V[0];
    d2.V[1] := V.coords.V[1] - v2.coords.V[1];
    d2.V[2] := V.coords.V[2] - v2.coords.V[2];
    tNorm.V[0] := d1.V[1] * d2.V[2] - d1.V[2] * d2.V[1];
    tNorm.V[1] := d1.V[2] * d2.V[0] - d1.V[0] * d2.V[2];
    tNorm.V[2] := d1.V[0] * d2.V[1] - d1.V[1] * d2.V[0];
    tLen2 := tNorm.V[0] * tNorm.V[0] + tNorm.V[1] * tNorm.V[1] + tNorm.V[2] *
      tNorm.V[2];
    if (tLen2 > maxLen2) then
    begin
      maxLen2 := tLen2;
      norm.V[0] := tNorm.V[0];
      norm.V[1] := tNorm.V[1];
      norm.V[2] := tNorm.V[2];
    end;
    V := V.next;
  end;

  if (maxLen2 <= 0) then
  begin
    // All points lie on a single line -- any decent normal will do
    norm.V[0] := 0;
    norm.V[1] := 0;
    norm.V[2] := 0;
    norm.V[LongAxis(d1)] := 1;
  end;
end;

// __gl_renderCache( tess ) takes a single contour and tries to render it
// as a triangle fan.  This handles convex polygons, as well as some
// non-convex polygons if we get lucky.
//
// Returns TRUE if the polygon was successfully rendered.  The rendering
// output is provided as callbacks (see the api).
///
function renderCache(tess: PGLUtesselator): Boolean;
var
  v0, vc: Integer;
  norm: TGLUAffineVector;
  sign: Integer;
  prim: GLUenum;
begin
  v0 := 0;

  if (tess.cacheCount < 3) then
    // Degenerate contour -- no output */
    exit(TRUE);

  norm.V[0] := tess.normal.V[0];
  norm.V[1] := tess.normal.V[1];
  norm.V[2] := tess.normal.V[2];
  if (norm.V[0] = 0) and (norm.V[1] = 0) and (norm.V[2] = 0) then
    ComputeNormal(tess, norm, FALSE);

  sign := ComputeNormal(tess, norm, TRUE);
  if (sign = SIGN_INCONSISTENT) then
    // Fan triangles did not have a consistent orientation
    exit(FALSE);
  if (sign = 0) then
    // All triangles were degenerate
    exit(TRUE);

  // Make sure we do the right thing for each winding rule
  case (tess.windingRule) of
    GLU_TESS_WINDING_ODD:
      ;
    GLU_TESS_WINDING_NONZERO:
      ;
    GLU_TESS_WINDING_POSITIVE:
      if (sign < 0) then
        exit(TRUE);
    GLU_TESS_WINDING_NEGATIVE:
      if (sign > 0) then
        exit(TRUE);
    GLU_TESS_WINDING_ABS_GEQ_TWO:
      exit(TRUE);
  end;

  if tess.boundaryOnly then
    prim := GL_LINE_LOOP
  else if tess.cacheCount > 3 then
    prim := GL_TRIANGLE_FAN
  else
    prim := GL_TRIANGLES;
  if IsCallBeginData(tess) then
    tess.callBeginData(prim, tess.polygonData)
  else
    tess.callBegin(prim);

  if IsCallVertexData(tess) then
    tess.callVertexData(tess.cache[v0].data, tess.polygonData)
  else
    tess.callVertex(tess.cache[v0].data);

  if (sign > 0) then
  begin
    for vc := v0 + 1 to tess.cacheCount - 1 do
    begin
      if IsCallVertexData(tess) then
        tess.callVertexData(tess.cache[vc].data, tess.polygonData)
      else
        tess.callVertex(tess.cache[vc].data);
    end;
  end
  else
  begin
    for vc := tess.cacheCount - 1 downto 0 do
    begin
      if IsCallVertexData(tess) then
        tess.callVertexData(tess.cache[vc].data, tess.polygonData)
      else
        tess.callVertex(tess.cache[vc].data);
    end;
  end;

  if IsCallEndData(tess) then
    tess.callEndData(tess.polygonData)
  else
    tess.callEnd();
  Result := TRUE;
end;

procedure CheckOrientation(tess: PGLUtesselator);
var
  area: GLUfloat;
  f, fHead: PGLUface;
  V, vHead: PGLUvertex;
  e: PGLUhalfEdge;
begin
  fHead := @tess.mesh.fHead;
  vHead := @tess.mesh.vHead;
  // When we compute the normal automatically, we choose the orientation
  // so that the the sum of the signed areas of all contours is non-negative.
  //
  area := 0;
  f := fHead.next;
  while (f <> fHead) do
  begin
    e := f.anEdge;
    if (e.winding <= 0) then
    begin
      f := f.next;
      continue;
    end;

    repeat
      area := area + (e.Org.s - e.Dst.s) * (e.Org.t + e.Dst.t);
      e := e.Lnext;
    until (e = f.anEdge);
    f := f.next;
  end;

  if (area < 0) then
  begin
    // Reverse the orientation by flipping all the t-coordinates
    V := vHead.next;
    while (V <> vHead) do
    begin
      V.t := -V.t;
      V := V.next;
    end;
    tess.tUnit.V[0] := -tess.tUnit.V[0];
    tess.tUnit.V[1] := -tess.tUnit.V[1];
    tess.tUnit.V[2] := -tess.tUnit.V[2];
  end;
end;

// Determine the polygon normal and project vertices onto the plane
// of the polygon.
//
procedure projectPolygon(tess: PGLUtesselator);
var
  V, vHead: PGLUvertex;
  norm: TGLUAffineVector;
  sUnit, tUnit: PGLUfloat;
  I: Integer;
  computedNormal: Boolean;
begin
  vHead := @tess.mesh.vHead;
  computedNormal := FALSE;

  norm.V[0] := tess.normal.V[0];
  norm.V[1] := tess.normal.V[1];
  norm.V[2] := tess.normal.V[2];

  if (norm.V[0] = 0) and (norm.V[1] = 0) and (norm.V[2] = 0) then
  begin
    ComputeNormal(tess, norm);
    computedNormal := TRUE;
  end;
  sUnit := @tess.sUnit;
  tUnit := @tess.tUnit;
  I := LongAxis(norm);

  // Project perpendicular to a coordinate axis -- better numerically
  sUnit[I] := 0;
  sUnit[(I + 1) mod 3] := S_UNIT_X;
  sUnit[(I + 2) mod 3] := S_UNIT_Y;

  tUnit[I] := 0;
  if norm.V[I] > 0 then
    tUnit[(I + 1) mod 3] := -S_UNIT_Y
  else
    tUnit[(I + 1) mod 3] := S_UNIT_Y;

  if norm.V[I] > 0 then
    tUnit[(I + 2) mod 3] := S_UNIT_X
  else
    tUnit[(I + 2) mod 3] := -S_UNIT_X;

  // Project the vertices onto the sweep plane
  V := vHead.next;
  while (V <> vHead) do
  begin
    V.s := V.coords.V[0] * sUnit[0] + V.coords.V[1] * sUnit[1] + V.coords.V[2]
      * sUnit[2];
    V.t := V.coords.V[0] * tUnit[0] + V.coords.V[1] * tUnit[1] + V.coords.V[2]
      * tUnit[2];
    V := V.next;
  end;
  if (computedNormal) then
    CheckOrientation(tess);
end;

// destroys an edge (the half-edges eDel and eDel->Sym),
// and removes from the global edge list.
///
procedure KillEdge(eDel: PGLUhalfEdge);
var
  ePrev, eNext: PGLUhalfEdge;
begin
  // Half-edges are allocated in pairs, see EdgePair above
  if (eDel.Sym < eDel) then
    eDel := eDel.Sym;

  // delete from circular doubly-linked list
  eNext := eDel.next;
  ePrev := eDel.Sym.next;
  eNext.Sym.next := ePrev;
  ePrev.Sym.next := eNext;

  FreeMem(eDel);
end;

// Removes the edge eDel.  There are several cases:
// if (eDel->Lface != eDel->Rface), we join two loops into one; the loop
// eDel->Lface is deleted.  Otherwise, we are splitting one loop into two;
// the newly created loop will contain eDel->Dst.  If the deletion of eDel
// would create isolated vertices, those are deleted as well.
//
// This function could be implemented as two calls to __gl_meshSplice
// plus a few calls to memFree, but this would allocate and delete
// unnecessary vertices and faces.
///
function meshDelete(eDel: PGLUhalfEdge): Boolean;
var
  eDelSym: PGLUhalfEdge;
  joiningLoops: Boolean;
  newFace: PGLUface;
begin
  eDelSym := eDel.Sym;
  joiningLoops := FALSE;
  // First step: disconnect the origin vertex eDel.Org.  We make all
  // changes to get a consistent mesh in this "intermediate" state.
  //
  if (eDel.Lface <> eDel.Rface) then
  begin
    // We are joining two loops into one -- remove the left face
    joiningLoops := TRUE;
    KillFace(eDel.Lface, eDel.Rface);
  end;

  if (eDel.Onext = eDel) then
  begin
    KillVertex(eDel.Org, nil);
  end
  else
  begin
    // Make sure that eDel.Org and eDel.Rface point to valid half-edges
    eDel.Rface.anEdge := eDel.Oprev;
    eDel.Org.anEdge := eDel.Onext;

    Splice(eDel, eDel.Oprev);
    if (not joiningLoops) then
    begin
      GetMem(newFace, SizeOf(TGLUface));

      // We are splitting one loop into two -- create a new loop for eDel.
      MakeFace(newFace, eDel, eDel.Lface);
    end;
  end;

  // Claim: the mesh is now in a consistent state, except that eDel.Org
  // may have been deleted.  Now we disconnect eDel.Dst.
  //
  if (eDelSym.Onext = eDelSym) then
  begin
    KillVertex(eDelSym.Org, nil);
    KillFace(eDelSym.Lface, nil);
  end
  else
  begin
    // Make sure that eDel.Dst and eDel.Lface point to valid half-edges
    eDel.Lface.anEdge := eDelSym.Oprev;
    eDelSym.Org.anEdge := eDelSym.Onext;
    Splice(eDelSym, eDelSym.Oprev);
  end;

  // Any isolated vertices or faces have already been freed.
  KillEdge(eDel);

  Result := TRUE;
end;

function VertEq(u, V: PGLUvertex): Boolean; {$IFDEF INLINE_ON} inline; {$ENDIF}
begin
  Result := (u.s = V.s) and (u.t = V.t);
end;

procedure callCombine(tess: PGLUtesselator; isect: PGLUvertex;
  const data: TGLUData; const weights: TGLUVector; needed: Boolean);
var
  coords: TGLUAffineVector;
begin
  // Copy coord data in case the callback changes it.
  coords.V[0] := isect.coords.V[0];
  coords.V[1] := isect.coords.V[1];
  coords.V[2] := isect.coords.V[2];

  isect.data := nil;
  if IsCallCombineData(tess) then
    tess.callCombineData(coords, data, weights, isect.data, tess.polygonData)
  else
    tess.callCombine(coords, data, weights, isect.data);

  if (isect.data = nil) then
  begin
    if (not needed) then
      isect.data := data[0]
    else
    begin
      if (not tess.fatalError) then
      begin
        // The only way fatal error is when two edges are found to intersect,
        // but the user has not provided the callback necessary to handle
        // generated intersection points.
        if IsCallErrorData(tess) then
          tess.callErrorData(GLU_TESS_NEED_COMBINE_CALLBACK, tess.polygonData)
        else
          tess.callError(GLU_TESS_NEED_COMBINE_CALLBACK);
        tess.fatalError := TRUE;
      end;
    end;
  end;
end;

// Two vertices with idential coordinates are combined into one.
// e1->Org is kept, while e2->Org is discarded.
///
procedure SpliceMergeVertices(tess: PGLUtesselator; e1, e2: PGLUhalfEdge);
const
  weights: TGLUVector = (0.5, 0.5, 0.0, 0.0);
var
  data: TGLUData;
begin
  data[0] := e1.Org.data;
  data[1] := e2.Org.data;
  data[2] := nil;
  data[3] := nil;
  callCombine(tess, e1.Org, data, weights, FALSE);
  if (not meshSplice(e1, e2)) then
    raise gluException.Create;
end;

// Remove zero-length edges, and contours with fewer than 3 vertices.
///
procedure RemoveDegenerateEdges(tess: PGLUtesselator);
var
  e, eNext, eLnext, eHead: PGLUhalfEdge;
begin
  eHead := @tess.mesh.eHead;
  e := eHead.next;
  while (e <> eHead) do
  begin
    eNext := e.next;
    eLnext := e.Lnext;

    if VertEq(e.Org, e.Dst) and (e.Lnext.Lnext <> e) then
    begin
      // Zero-length edge, contour has at least 3 edges
      SpliceMergeVertices(tess, eLnext, e); // deletes e.Org
      if (not meshDelete(e)) then
        raise gluException.Create; // e is a self-loop
      e := eLnext;
      eLnext := e.Lnext;
    end;

    if (eLnext.Lnext = e) then
    begin
      // Degenerate contour (one or two edges)
      if (eLnext <> e) then
      begin
        if (eLnext = eNext) or (eLnext = eNext.Sym) then
          eNext := eNext.next;
        if (not meshDelete(eLnext)) then
          raise gluException.Create;
      end;
      if (e = eNext) or (e = eNext.Sym) then
        eNext := eNext.next;
      if (not meshDelete(e)) then
        raise gluException.Create;
    end;
    e := eNext;
  end;
end;

function pqNewPriorityQ(aleq: TPQleq): PPriorityQ;
var
  pq: PPriorityQ;
begin
  GetMem(pq, SizeOf(TPriorityQ));

  pq.size := 0;
  pq.max := PQ_INIT_SIZE;
  GetMem(pq.nodes, (PQ_INIT_SIZE + 1) * SizeOf(TPQnode));
  GetMem(pq.handles, (PQ_INIT_SIZE + 1) * SizeOf(TPQhandleElem));

  pq.initialized := FALSE;
  pq.freeList := 0;
  pq.leq := aleq;

  // so that Minimum() returns NULL
  pq.nodes[1].handle := 1;
  pq.handles[1].key := nil;

  Result := pq;
end;

function pqNewHeapPriorityQ(aleq: TPQleq): PHeapPriorityQ;
var
  pq: PHeapPriorityQ;
begin
  GetMem(pq, SizeOf(TPriorityQ));

  pq.heap := pqNewPriorityQ(aleq);
  GetMem(pq.keys, PQ_INIT_SIZE * SizeOf(TPQkey));

  pq.size := 0;
  pq.max := PQ_INIT_SIZE;
  pq.initialized := FALSE;
  pq.leq := aleq;

  Result := pq;
end;

function VertLeq(u, V: TPQkey): Boolean;
var
  u_, v_: PGLUvertex;
begin
  u_ := PGLUvertex(u);
  v_ := PGLUvertex(V);
  Result := (u_.s < v_.s) or ((u_.s = v_.s) and (u_.t <= v_.t));
end;

procedure FloatUp(pq: PPriorityQ; curr: LongInt);
var
  n: PPQnode;
  h: PPQhandleElem;
  hCurr, hParent: TPQhandle;
  parent: LongInt;
begin
  n := pq.nodes;
  h := pq.handles;
  hCurr := n[curr].handle;
  repeat
    parent := curr div 2;
    hParent := n[parent].handle;

    if (parent = 0) or VertLeq(h[hParent].key, h[hCurr].key) then
    begin
      n[curr].handle := hCurr;
      h[hCurr].node := curr;
      break;
    end;
    n[curr].handle := hParent;
    h[hParent].node := curr;
    curr := parent;
  until FALSE;
end;

procedure FloatDown(pq: PPriorityQ; curr: LongInt);
var
  n: PPQnode;
  h: PPQhandleElem;
  hCurr, hChild: TPQhandle;
  child: LongInt;
begin
  n := pq.nodes;
  h := pq.handles;
  hCurr := n[curr].handle;
  repeat
    child := curr * 2;
    if (child < pq.size) and VertLeq(h[n[child + 1].handle].key,
      h[n[child].handle].key) then
      Inc(child);

    assert(child <= pq.max);

    hChild := n[child].handle;
    if (child > pq.size) or VertLeq(h[hCurr].key, h[hChild].key) then
    begin
      n[curr].handle := hCurr;
      h[hCurr].node := curr;
      break;
    end;
    n[curr].handle := hChild;
    h[hChild].node := curr;
    curr := child;
  until FALSE;
end;

procedure pqInit(pq: PPriorityQ);
var
  I: LongInt;
begin
  // This method of building a heap is O(n), rather than O(n lg n).
  for I := pq.size downto 1 do
    FloatDown(pq, I);
  pq.initialized := TRUE;
end;

function pqHeapInit(pq: PHeapPriorityQ): Boolean;
type
  PStack = ^TStack;

  TStack = record
    p: PPPQkey;
    r: PPPQkey;
  end;

  procedure Swap(var a, b: PPPQkey);
  var
    tmp: PPQkey;
  begin
    tmp := a^;
    a^ := b^;
    b^ := tmp;
  end;

var
  p, r, I, j: PPPQkey;
  piv: PPQkey;
  Stack: array [0 .. 49] of TStack;
  top, stop: PStack;
  seed: LongWord;
begin
  seed := 2016473283;
  top := @Stack[0];
  stop := top - 1;
  // Create an array of indirect pointers to the keys, so that we
  // the handles we have returned are still valid.
  GetMem(pq.order, (pq.size + 1) * SizeOf(pq.order[0]));
  // the previous line is a patch to compensate for the fact that IBM */
  // machines return a null on a malloc of zero bytes (unlike SGI),   */
  // so we have to put in this defense to guard against a memory      */
  // fault four lines down. from fossum@austin.ibm.com.               */
  if (pq.order = nil) then
    exit(FALSE);

  p := pq.order;
  r := p + pq.size - 1;
  piv := pq.keys;
  I := p;
  while (I <= r) do
  begin
    I^ := piv;
    Inc(piv);
    Inc(I);
  end;

  // Sort the indirect pointers in descending order,
  // using randomized Quicksort
  //
  top.p := p;
  top.r := r;
  repeat
    p := top.p;
    r := top.r;
    while (r > p + 10) do
    begin
      seed := seed * 1539415821 + 1;
      I := p + seed mod (r - p + 1);
      piv := I^;
      I^ := p^;
      p^ := piv;
      I := p - 1;
      j := r + 1;
      repeat
        repeat
          Inc(I);
        until (pq.leq(I^^, piv^));
        repeat
          Dec(j);
        until (pq.leq(piv^, j^^));
        Swap(I, j);
      until (I >= j);
      Swap(I, j); // Undo last swap */
      if (I - p < r - j) then
      begin
        top.p := j + 1;
        top.r := r;
        Inc(top);
        r := I - 1;
      end
      else
      begin
        top.p := p;
        top.r := I - 1;
        Inc(top);
        p := j + 1;
      end;
    end;

    // Insertion sort small lists
    I := p + 1;
    while (I <= r) do
    begin
      piv := I^;
      j := I;
      while (j > p) and not pq.leq(piv^, (j - 1)^^) do
      begin
        j^ := (j - 1)^;
        Dec(j);
      end;
      j^ := piv;
      Inc(I);
    end;

    Dec(top);
  until (top = stop);

  pq.max := pq.size;
  pq.initialized := TRUE;
  pqInit(pq.heap); // always succeeds

  Result := TRUE;
end;

function pqInsert(pq: PPriorityQ; keyNew: TPQkey): TPQhandle;
var
  curr: LongInt;
  pqfree: TPQhandle;
begin
  Inc(pq.size);
  curr := pq.size;
  if ((curr * 2) > pq.max) then
  begin
    // If the heap overflows, double its size.
    pq.max := pq.max * 2;
    ReallocMem(pq.nodes, (pq.max + 1) * SizeOf(TPQnode));
    ReallocMem(pq.handles, (pq.max + 1) * SizeOf(TPQhandleElem));
  end;

  if (pq.freeList = 0) then
    pqfree := curr
  else
  begin
    pqfree := pq.freeList;
    pq.freeList := pq.handles[pqfree].node;
  end;

  pq.nodes[curr].handle := pqfree;
  pq.handles[pqfree].node := curr;
  pq.handles[pqfree].key := keyNew;

  if (pq.initialized) then
    FloatUp(pq, curr);

  Result := pqfree;
end;

function pqHeapInsert(pq: PHeapPriorityQ; keyNew: TPQkey): TPQhandle;
var
  curr: LongInt;
begin
  if (pq.initialized) then
    exit(pqInsert(pq.heap, keyNew));

  curr := pq.size;
  Inc(pq.size);
  if (pq.size >= pq.max) then
  begin
    // If the heap overflows, double its size.
    pq.max := pq.max * 2;
    ReallocMem(pq.keys, pq.max * SizeOf(TPQkey));
  end;
  pq.keys[curr] := keyNew;

  // Negative handles index the sorted array.
  Result := -(curr + 1);
end;

procedure pqDeletePriorityQ(pq: PPriorityQ);
begin
  FreeMem(pq.handles);
  FreeMem(pq.nodes);
  FreeMem(pq);
end;

procedure pqHeapDeletePriorityQ(pq: PHeapPriorityQ);
begin
  assert(pq <> nil);
  if (pq.heap <> nil) then
    pqDeletePriorityQ(pq.heap);
  if (pq.order <> nil) then
    FreeMem(pq.order);
  if (pq.keys <> nil) then
    FreeMem(pq.keys);
  FreeMem(pq);
end;

// Insert all vertices into the priority queue which determines the
// order in which vertices cross the sweep line.
//
function InitPriorityQ(tess: PGLUtesselator): Boolean;
var
  pq: PHeapPriorityQ;
  V, vHead: PGLUvertex;
begin
  pq := pqNewHeapPriorityQ(VertLeq);
  tess.pq := pq;

  vHead := @tess.mesh.vHead;
  V := vHead.next;
  while (V <> vHead) do
  begin
    V.pqHandle := pqHeapInsert(pq, V);
    V := V.next;
  end;

  if (V <> vHead) then
  begin
    pqHeapDeletePriorityQ(tess.pq);
    tess.pq := nil;
    exit(FALSE);
  end;

  pqHeapInit(pq);

  Result := TRUE;
end;

function dictNewDict(frame: PGLUtesselator; aleq: TDictLeq): PDict;
var
  dict: PDict;
  head: PDictNode;
begin
  GetMem(dict, SizeOf(TDict));

  head := @dict.head;

  head.key := nil;
  head.next := head;
  head.prev := head;

  dict.frame := frame;
  dict.leq := aleq;

  Result := dict;
end;

function edgeSign(u, V, w: PGLUvertex): GLUfloat;
var
  gapL, gapR: GLUfloat;
begin
  // Returns a number whose sign matches EdgeEval(u,v,w) but which
  // is cheaper to evaluate.  Returns > 0, == 0 , or < 0
  // as v is above, on, or below the edge uw.
  //
  assert(VertLeq(u, V) and VertLeq(V, w));

  gapL := V.s - u.s;
  gapR := w.s - V.s;

  if (gapL + gapR > 0) then
    exit((V.t - w.t) * gapL + (V.t - u.t) * gapR);

  // vertical line
  Result := 0;
end;

function edgeEval(u, V, w: PGLUvertex): GLUfloat;
var
  gapL, gapR: GLUfloat;
begin
  // Given three vertices u,v,w such that VertLeq(u,v) && VertLeq(v,w),
  // evaluates the t-coord of the edge uw at the s-coord of the vertex v.
  // Returns v->t - (uw)(v->s), ie. the signed distance from uw to v.
  // If uw is vertical (and thus passes thru v), the result is zero.
  //
  // The calculation is extremely accurate and stable, even when v
  // is very close to u or w.  In particular if we set v->t = 0 and
  // let r be the negated result (this evaluates (uw)(v->s)), then
  // r is guaranteed to satisfy MIN(u->t,w->t) <= r <= MAX(u->t,w->t).
  ///

  assert(VertLeq(u, V) and VertLeq(V, w));

  gapL := V.s - u.s;
  gapR := w.s - V.s;

  if (gapL + gapR > 0) then
  begin
    if (gapL < gapR) then
      exit((V.t - u.t) + (u.t - w.t) * (gapL / (gapL + gapR)))
    else
      exit((V.t - w.t) + (w.t - u.t) * (gapR / (gapL + gapR)));
  end;

  // vertical line
  Result := 0;
end;

function EdgeLeq(tess: PGLUtesselator; reg1, reg2: PActiveRegion): Boolean;
var
  event: PGLUvertex;
  e1, e2: PGLUhalfEdge;
  t1, t2: GLUfloat;
begin
  event := tess.event;
  e1 := reg1.eUp;
  e2 := reg2.eUp;

  if (e1.Dst = event) then
  begin
    if (e2.Dst = event) then
    begin
      // Two edges right of the sweep line which meet at the sweep event.
      // Sort them by slope.
      //
      if (VertLeq(e1.Org, e2.Org)) then
        exit(edgeSign(e2.Dst, e1.Org, e2.Org) <= 0);

      exit(edgeSign(e1.Dst, e2.Org, e1.Org) >= 0);
    end;
    exit(edgeSign(e2.Dst, event, e2.Org) <= 0);
  end;

  if (e2.Dst = event) then
    exit(edgeSign(e1.Dst, event, e1.Org) >= 0);

  // General case - compute signed distance *from* e1, e2 to event
  t1 := edgeEval(e1.Dst, event, e1.Org);
  t2 := edgeEval(e2.Dst, event, e2.Org);

  Result := (t1 >= t2);
end;

function dictInsertBefore(dict: PDict; node: PDictNode; key: TDictKey)
  : PDictNode;
var
  newNode: PDictNode;
begin
  repeat
    node := node.prev;
  until not((node.key <> nil) and not dict.leq(dict.frame, node.key, key));

  GetMem(newNode, SizeOf(TDictNode));

  newNode.key := key;
  newNode.next := node.next;
  node.next.prev := newNode;
  newNode.prev := node;
  node.next := newNode;

  Result := newNode;
end;

// We add two sentinel edges above and below all other edges,
// to avoid special cases at the top and bottom.
//
procedure AddSentinel(tess: PGLUtesselator; t: GLUfloat);
var
  e: PGLUhalfEdge;
  reg: PActiveRegion;
begin
  GetMem(reg, SizeOf(TActiveRegion));

  e := meshMakeEdge(tess.mesh);
  if (e = nil) then
    raise gluException.Create;

  e.Org.s := SENTINEL_COORD;
  e.Org.t := t;
  e.Dst.s := -SENTINEL_COORD;
  e.Dst.t := t;
  tess.event := e.Dst; // initialize it

  reg.eUp := e;
  reg.windingNumber := 0;
  reg.inside := FALSE;
  reg.fixUpperEdge := FALSE;
  reg.sentinel := TRUE;
  reg.dirty := FALSE;
  reg.nodeUp := dictInsertBefore(tess.dict, @tess.dict.head, TDictKey(reg));

  if (reg.nodeUp = nil) then
    raise gluException.Create;
end;

// We maintain an ordering of edge intersections with the sweep line.
// This order is maintained in a dynamic dictionary.
//
procedure InitEdgeDict(tess: PGLUtesselator);
begin
  tess.dict := dictNewDict(tess, EdgeLeq);
  if (tess.dict = nil) then
    raise gluException.Create;
  AddSentinel(tess, -SENTINEL_COORD);
  AddSentinel(tess, SENTINEL_COORD);
end;

function pqExtractMin(pq: PPriorityQ): TPQkey;
var
  n: PPQnode;
  h: PPQhandleElem;
  hMin: TPQhandle;
  min: TPQkey;
begin
  n := pq.nodes;
  h := pq.handles;
  hMin := n[1].handle;
  min := h[hMin].key;
  if (pq.size > 0) then
  begin
    n[1].handle := n[pq.size].handle;
    h[n[1].handle].node := 1;

    h[hMin].key := nil;
    h[hMin].node := pq.freeList;
    pq.freeList := hMin;

    Dec(pq.size);
    if (pq.size > 0) then
      FloatDown(pq, 1);
  end;

  Result := min;
end;

function pqHeapExtractMin(pq: PHeapPriorityQ): TPQkey;
var
  sortMin, heapMin: TPQkey;
begin
  if (pq.size = 0) then
    exit(pqExtractMin(pq.heap));

  sortMin := pq.order[pq.size - 1]^;
  if (pq.heap.size > 0) then
  begin
    heapMin := pq.heap.handles[pq.heap.nodes[1].handle].key;
    if (VertLeq(heapMin, sortMin)) then
      exit(pqExtractMin(pq.heap));
  end;
  repeat
    Dec(pq.size);
  until not((pq.size > 0) and (pq.order[pq.size - 1]^ = nil));

  Result := sortMin;
end;

function pqMinimum(pq: PPriorityQ): TPQkey; {$IFDEF INLINE_ON} inline; {$ENDIF}
begin
  Result := pq.handles[pq.nodes[1].handle].key;
end;

function pqHeapMinimum(pq: PHeapPriorityQ): TPQkey;
var
  sortMin, heapMin: TPQkey;
begin
  if (pq.size = 0) then
    exit(pqMinimum(pq.heap));

  sortMin := pq.order[pq.size - 1]^;
  if pq.heap.size > 0 then
  begin
    heapMin := pqMinimum(pq.heap);
    if (VertLeq(heapMin, sortMin)) then
      exit(heapMin);
  end;

  Result := sortMin;
end;

function dictSearch(dict: PDict; key: TDictKey): PDictNode;
var
  node: PDictNode;
begin
  node := @dict.head;

  repeat
    node := node.next;
  until not((node.key <> nil) and not dict.leq(dict.frame, key, node.key));

  Result := node;
end;

function RegionBelow(r: PActiveRegion): PActiveRegion;
{$IFDEF INLINE_ON} inline; {$ENDIF}
begin
  Result := PActiveRegion(r.nodeUp.prev.key);
end;

function RegionAbove(r: PActiveRegion): PActiveRegion;
{$IFDEF INLINE_ON} inline; {$ENDIF}
begin
  Result := PActiveRegion(r.nodeUp.next.key);
end;

function TopRightRegion(reg: PActiveRegion): PActiveRegion;
var
  Dst: PGLUvertex;
begin
  Dst := reg.eUp.Dst;
  // Find the region above the uppermost edge with the same destination
  repeat
    reg := RegionAbove(reg);
  until (reg.eUp.Dst <> Dst);

  Result := reg;
end;

procedure dictDeleteDict(dict: PDict);
var
  node, next: PDictNode;
begin
  node := dict.head.next;
  while (node <> @dict.head) do
  begin
    next := node.next;
    FreeMem(node);
    node := next;
  end;

  FreeMem(dict);
end;

procedure dictDelete(dict: PDict; node: PDictNode);
begin
  node.next.prev := node.prev;
  node.prev.next := node.next;
  FreeMem(node);
end;

procedure DeleteRegion(tess: PGLUtesselator; reg: PActiveRegion);
begin
  if (reg.fixUpperEdge) then
  begin
    // It was created with zero winding number, so it better be
    // deleted with zero winding number (ie. it better not get merged
    // with a real edge).
    //
    assert(reg.eUp.winding = 0);
  end;
  reg.eUp.activeRegion := nil;
  dictDelete(tess.dict, reg.nodeUp);
  FreeMem(reg);
end;

function EdgeGoesLeft(e: PGLUhalfEdge): Boolean; {$IFDEF INLINE_ON} inline;
{$ENDIF}
begin
  Result := VertLeq(e.Dst, e.Org);
end;

function EdgeGoesRight(e: PGLUhalfEdge): Boolean; {$IFDEF INLINE_ON} inline;
{$ENDIF}
begin
  Result := VertLeq(e.Org, e.Dst);
end;

// Add a new active region to the sweep line, *somewhere* below "regAbove"
// (according to where the new edge belongs in the sweep-line dictionary).
// The upper edge of the new region will be "eNewUp".
// Winding number and "inside" flag are not updated.
///
function AddRegionBelow(tess: PGLUtesselator; regAbove: PActiveRegion;
  eNewUp: PGLUhalfEdge): PActiveRegion;
var
  regNew: PActiveRegion;
begin
  GetMem(regNew, SizeOf(TActiveRegion));

  regNew.eUp := eNewUp;
  regNew.nodeUp := dictInsertBefore(tess.dict, regAbove.nodeUp, regNew);
  if (regNew.nodeUp = nil) then
    raise gluException.Create;
  regNew.fixUpperEdge := FALSE;
  regNew.sentinel := FALSE;
  regNew.dirty := FALSE;

  eNewUp.activeRegion := regNew;

  Result := regNew;
end;

function IsWindingInside(tess: PGLUtesselator; n: Integer): Boolean;
begin
  case (tess.windingRule) of
    GLU_TESS_WINDING_ODD:
      exit(n and 1 <> 0);
    GLU_TESS_WINDING_NONZERO:
      exit(n <> 0);
    GLU_TESS_WINDING_POSITIVE:
      exit(n > 0);
    GLU_TESS_WINDING_NEGATIVE:
      exit(n < 0);
    GLU_TESS_WINDING_ABS_GEQ_TWO:
      exit((n >= 2) or (n <= -2));
  else
    assert(FALSE);
  end;

  Result := FALSE;
end;

procedure pqDelete(pq: PPriorityQ; hCurr: TPQhandle); overload;
var
  n: PPQnode;
  h: PPQhandleElem;
  curr: LongInt;
begin
  n := pq.nodes;
  h := pq.handles;
  assert((hCurr >= 1) and (hCurr <= pq.max) and (h[hCurr].key <> nil));

  curr := h[hCurr].node;
  n[curr].handle := n[pq.size].handle;
  h[n[curr].handle].node := curr;

  Dec(pq.size);
  if (curr <= pq.size) then
  begin
    if (curr <= 1) or VertLeq(h[n[curr div 2].handle].key, h[n[curr].handle].key)
    then
      FloatDown(pq, curr)
    else
      FloatUp(pq, curr);
  end;
  h[hCurr].key := nil;
  h[hCurr].node := pq.freeList;
  pq.freeList := hCurr;
end;

procedure pqHeapDelete(pq: PHeapPriorityQ; curr: TPQhandle);
begin
  if (curr >= 0) then
  begin
    pqDelete(pq.heap, curr);
    exit;
  end;

  curr := -(curr + 1);
  assert((curr < pq.max) and (pq.keys[curr] <> nil));

  pq.keys[curr] := nil;

  while (pq.size > 0) and (pq.order[pq.size - 1]^ = nil) do
    Dec(pq.size);
end;

// Check the upper and lower edge of "regUp", to make sure that the
// eUp->Org is above eLo, or eLo->Org is below eUp (depending on which
// origin is leftmost).
//
// The main purpose is to splice right-going edges with the same
// dest vertex and nearly identical slopes (ie. we can't distinguish
// the slopes numerically).  However the splicing can also help us
// to recover from numerical errors.  For example, suppose at one
// point we checked eUp and eLo, and decided that eUp->Org is barely
// above eLo.  Then later, we split eLo into two edges (eg. from
// a splice operation like this one).  This can change the result of
// our test so that now eUp->Org is incident to eLo, or barely below it.
// We must correct this condition to maintain the dictionary invariants.
//
// One possibility is to check these edges for intersection again
// (ie. CheckForIntersect).  This is what we do if possible.  However
// CheckForIntersect requires that tess->event lies between eUp and eLo,
// so that it has something to fall back on when the intersection
// calculation gives us an unusable answer.  So, for those cases where
// we can't check for intersection, this routine fixes the problem
// by just splicing the offending vertex into the other edge.
// This is a guaranteed solution, no matter how degenerate things get.
// Basically this is a combinatorial solution to a numerical problem.
///
function CheckForRightSplice(tess: PGLUtesselator;
  regUp: PActiveRegion): Boolean;
var
  regLo: PActiveRegion;
  eUp, eLo: PGLUhalfEdge;
begin
  regLo := RegionBelow(regUp);
  eUp := regUp.eUp;
  eLo := regLo.eUp;
  if (VertLeq(eUp.Org, eLo.Org)) then
  begin
    if (edgeSign(eLo.Dst, eUp.Org, eLo.Org) > 0) then
    begin
      exit(FALSE);
    end;

    // eUp.Org appears to be below eLo
    if (not VertEq(eUp.Org, eLo.Org)) then
    begin
      // Splice eUp.Org into eLo
      if (meshSplitEdge(eLo.Sym) = nil) then
        raise gluException.Create;
      if (not meshSplice(eUp, eLo.Oprev)) then
        raise gluException.Create;
      regUp.dirty := TRUE;
      regLo.dirty := TRUE;
    end
    else
    begin
      if (eUp.Org <> eLo.Org) then
      begin
        // merge the two vertices, discarding eUp.Org
        pqHeapDelete(tess.pq, eUp.Org.pqHandle);
        SpliceMergeVertices(tess, eLo.Oprev, eUp);
      end;
    end;
  end
  else
  begin
    if (edgeSign(eUp.Dst, eLo.Org, eUp.Org) < 0) then
      exit(FALSE);

    // eLo.Org appears to be above eUp, so splice eLo.Org into eUp
    RegionAbove(regUp).dirty := TRUE;
    regUp.dirty := TRUE;
    if (meshSplitEdge(eUp.Sym) = nil) then
      raise gluException.Create;
    if (not meshSplice(eLo.Oprev, eUp)) then
      raise gluException.Create;
  end;

  Result := TRUE;
end;

// When we merge two edges into one, we need to compute the combined
// winding of the new edge.
///
procedure AddWinding(eDst, eSrc: PGLUhalfEdge);
begin
  eDst.winding := eDst.winding + eSrc.winding;
  eDst.Sym.winding := eDst.Sym.winding + eSrc.Sym.winding;
end;

function min(a, b: GLUfloat): GLUfloat; {$IFDEF INLINE_ON} inline; {$ENDIF}
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function max(a, b: GLUfloat): GLUfloat; {$IFDEF INLINE_ON} inline; {$ENDIF}
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

procedure Swap(var a, b: PGLUvertex); {$IFDEF INLINE_ON} inline; {$ENDIF}
var
  t: PGLUvertex;
begin
  t := a;
  a := b;
  b := t;
end;

// Given parameters a,x,b,y returns the value (b//x+a//y)/(a+b),
// or (x+y)/2 if a==b==0.  It requires that a,b >= 0, and enforces
// this in the rare case that one argument is slightly negative.
// The implementation is extremely stable numerically.
// In particular it guarantees that the result r satisfies
// MIN(x,y) <= r <= MAX(x,y), and the results are very accurate
// even when a and b differ greatly in magnitude.
///
function Interpolate(a, x, b, y: GLUfloat): GLUfloat;
begin
  if a < 0 then
    a := 0;
  if b < 0 then
    b := 0;
  if a <= b then
  begin
    if b = 0 then
      Result := 0.5 * (x + y)
    else
      Result := x + (y - x) * (a / (a + b));
  end
  else
    Result := (y + (x - y) * (b / (a + b)));
end;

function TransLeq(u, V: PGLUvertex): Boolean; {$IFDEF INLINE_ON} inline;
{$ENDIF}
begin
  Result := (u.t < V.t) or ((u.t = V.t) and (u.s <= V.s));
end;

// Define versions of EdgeSign, EdgeEval with s and t transposed.
///

function transEval(u, V, w: PGLUvertex): GLUfloat;
var
  // Given three vertices u,v,w such that TransLeq(u,v) && TransLeq(v,w),
  // evaluates the t-coord of the edge uw at the s-coord of the vertex v.
  // Returns v->s - (uw)(v->t), ie. the signed distance from uw to v.
  // If uw is vertical (and thus passes thru v), the result is zero.
  //
  // The calculation is extremely accurate and stable, even when v
  // is very close to u or w.  In particular if we set v->s = 0 and
  // let r be the negated result (this evaluates (uw)(v->t)), then
  // r is guaranteed to satisfy MIN(u->s,w->s) <= r <= MAX(u->s,w->s).
  ///
  gapL, gapR: GLUfloat;
begin
  assert(TransLeq(u, V) and TransLeq(V, w));

  gapL := V.t - u.t;
  gapR := w.t - V.t;

  if (gapL + gapR > 0) then
  begin
    if (gapL < gapR) then
      exit((V.s - u.s) + (u.s - w.s) * (gapL / (gapL + gapR)))
    else
      exit((V.s - w.s) + (w.s - u.s) * (gapR / (gapL + gapR)));
  end;

  // vertical line
  Result := 0;
end;

function transSign(u, V, w: PGLUvertex): GLUfloat;
var
  /// Returns a number whose sign matches TransEval(u,v,w) but which
  // is cheaper to evaluate.  Returns > 0, == 0 , or < 0
  // as v is above, on, or below the edge uw.
  ///
  gapL, gapR: GLUfloat;
begin
  assert(TransLeq(u, V) and TransLeq(V, w));

  gapL := V.t - u.t;
  gapR := w.t - V.t;

  if (gapL + gapR > 0) then
    exit((V.s - w.s) * gapL + (V.s - u.s) * gapR);

  // vertical line
  Result := 0;
end;

procedure edgeIntersect(o1, d1, o2, d2, V: PGLUvertex);
// Given edges (o1,d1) and (o2,d2), compute their point of intersection.
// The computed point is guaranteed to lie in the intersection of the
// bounding rectangles defined by each edge.
//
var
  z1, z2: GLUfloat;
begin
  // This is certainly not the most efficient way to find the intersection
  // of two line segments, but it is very numerically stable.
  //
  // Strategy: find the two middle vertices in the VertLeq ordering,
  // and interpolate the intersection s-value from these.  Then repeat
  // using the TransLeq ordering to find the intersection t-value.
  ///

  if (not VertLeq(o1, d1)) then
  begin
    Swap(o1, d1);
  end;
  if (not VertLeq(o2, d2)) then
  begin
    Swap(o2, d2);
  end;
  if (not VertLeq(o1, o2)) then
  begin
    Swap(o1, o2);
    Swap(d1, d2);
  end;

  if (not VertLeq(o2, d1)) then
  begin
    // Technically, no intersection -- do our best
    V.s := (o2.s + d1.s) / 2;
  end
  else
  begin
    if (VertLeq(d1, d2)) then
    begin
      // Interpolate between o2 and d1
      z1 := edgeEval(o1, o2, d1);
      z2 := edgeEval(o2, d1, d2);
      if (z1 + z2 < 0) then
      begin
        z1 := -z1;
        z2 := -z2;
      end;
      V.s := Interpolate(z1, o2.s, z2, d1.s);
    end
    else
    begin
      // Interpolate between o2 and d2 /
      z1 := edgeSign(o1, o2, d1);
      z2 := -edgeSign(o1, d2, d1);
      if (z1 + z2 < 0) then
      begin
        z1 := -z1;
        z2 := -z2;
      end;
      V.s := Interpolate(z1, o2.s, z2, d2.s);
    end;
  end;

  // Now repeat the process for t
  if (not TransLeq(o1, d1)) then
  begin
    Swap(o1, d1);
  end;
  if (not TransLeq(o2, d2)) then
  begin
    Swap(o2, d2);
  end;
  if (not TransLeq(o1, o2)) then
  begin
    Swap(o1, o2);
    Swap(d1, d2);
  end;

  if (not TransLeq(o2, d1)) then
  begin
    // Technically, no intersection -- do our best
    V.t := (o2.t + d1.t) / 2;
  end
  else
  begin
    if (TransLeq(d1, d2)) then
    begin
      // Interpolate between o2 and d1
      z1 := transEval(o1, o2, d1);
      z2 := transEval(o2, d1, d2);
      if (z1 + z2 < 0) then
      begin
        z1 := -z1;
        z2 := -z2;
      end;
      V.t := Interpolate(z1, o2.t, z2, d1.t);
    end
    else
    begin
      // Interpolate between o2 and d2
      z1 := transSign(o1, o2, d1);
      z2 := -transSign(o1, d2, d1);
      if (z1 + z2 < 0) then
      begin
        z1 := -z1;
        z2 := -z2;
      end;
      V.t := Interpolate(z1, o2.t, z2, d2.t);
    end;
  end;
end;

// creates a new edge from eOrg->Dst
// to eDst->Org, and returns the corresponding half-edge eNew.
// If eOrg->Lface == eDst->Lface, this splits one loop into two,
// and the newly created loop is eNew->Lface.  Otherwise, two disjoint
// loops are merged into one, and the loop eDst->Lface is destroyed.
//
// If (eOrg == eDst), the new face will have only two edges.
// If (eOrg->Lnext == eDst), the old face is reduced to a single edge.
// If (eOrg->Lnext->Lnext == eDst), the old face is reduced to two edges.
///
function meshConnect(eOrg, eDst: PGLUhalfEdge): PGLUhalfEdge;
var
  eNewSym: PGLUhalfEdge;
  joiningLoops: Boolean;
  eNew: PGLUhalfEdge;
  newFace: PGLUface;
begin
  joiningLoops := FALSE;
  eNew := MakeEdge(eOrg);
  if (eNew = nil) then
    exit(nil);

  eNewSym := eNew.Sym;

  if (eDst.Lface <> eOrg.Lface) then
  begin
    // We are connecting two disjoint loops -- destroy eDst->Lface
    joiningLoops := TRUE;
    KillFace(eDst.Lface, eOrg.Lface);
  end;

  // Connect the new edge appropriately
  Splice(eNew, eOrg.Lnext);
  Splice(eNewSym, eDst);

  // Set the vertex and face information
  eNew.Org := eOrg.Dst;
  eNewSym.Org := eDst.Org;
  eNew.Lface := eOrg.Lface;
  eNewSym.Lface := eOrg.Lface;

  // Make sure the old face points to a valid half-edge
  eOrg.Lface.anEdge := eNewSym;

  if (not joiningLoops) then
  begin
    GetMem(newFace, SizeOf(TGLUface));

    // We split one loop into two -- the new loop is eNew->Lface
    MakeFace(newFace, eNew, eOrg.Lface);
  end;

  Result := eNew;
end;

// Replace an upper edge which needs fixing (see ConnectRightVertex).
///
function fixUpperEdge(reg: PActiveRegion; newEdge: PGLUhalfEdge): Boolean;
begin
  assert(reg.fixUpperEdge);
  if (not meshDelete(reg.eUp)) then
    exit(FALSE);
  reg.fixUpperEdge := FALSE;
  reg.eUp := newEdge;
  newEdge.activeRegion := reg;

  Result := TRUE;
end;

function TopLeftRegion(reg: PActiveRegion): PActiveRegion;
var
  Org: PGLUvertex;
  e: PGLUhalfEdge;
begin
  Org := reg.eUp.Org;
  // Find the region above the uppermost edge with the same origin
  repeat
    reg := RegionAbove(reg);
  until (reg.eUp.Org <> Org);

  // If the edge above was a temporary edge introduced by ConnectRightVertex,
  // now is the time to fix it.

  if (reg.fixUpperEdge) then
  begin
    e := meshConnect(RegionBelow(reg).eUp.Sym, reg.eUp.Lnext);
    if (e = nil) then
      exit(nil);
    if (not fixUpperEdge(reg, e)) then
      exit(nil);
    reg := RegionAbove(reg);
  end;
  Result := reg;
end;

// Delete a region from the sweep line.  This happens when the upper
// and lower chains of a region meet (at a vertex on the sweep line).
// The "inside" flag is copied to the appropriate mesh face (we could
// not do this before -- since the structure of the mesh is always
// changing, this face may not have even existed until now).
///
procedure FinishRegion(tess: PGLUtesselator; reg: PActiveRegion);
var
  e: PGLUhalfEdge;
  f: PGLUface;
begin
  e := reg.eUp;
  f := e.Lface;
  f.inside := reg.inside;
  f.anEdge := e;
  DeleteRegion(tess, reg);
end;

// We are given a vertex with one or more left-going edges.  All affected
// edges should be in the edge dictionary.  Starting at regFirst->eUp,
// we walk down deleting all regions where both edges have the same
// origin vOrg.  At the same time we copy the "inside" flag from the
// active region to the face, since at this point each face will belong
// to at most one region (this was not necessarily true until this point
// in the sweep).  The walk stops at the region above regLast; if regLast
// is NULL we walk as far as possible.	At the same time we relink the
// mesh if necessary, so that the ordering of edges around vOrg is the
// same as in the dictionary.
///
function FinishLeftRegions(tess: PGLUtesselator; regFirst: PActiveRegion;
  regLast: PActiveRegion): PGLUhalfEdge;
var
  reg, regPrev: PActiveRegion;
  e, ePrev: PGLUhalfEdge;
begin
  regPrev := regFirst;
  ePrev := regFirst.eUp;
  while (regPrev <> regLast) do
  begin
    // placement was OK
    regPrev.fixUpperEdge := FALSE;
    reg := RegionBelow(regPrev);
    e := reg.eUp;
    if (e.Org <> ePrev.Org) then
    begin
      if (not reg.fixUpperEdge) then
      begin
        /// Remove the last left-going edge.  Even though there are no further
        // edges in the dictionary with this origin, there may be further
        // such edges in the mesh (if we are adding left edges to a vertex
        // that has already been processed).  Thus it is important to call
        // FinishRegion rather than just DeleteRegion.
        ///
        FinishRegion(tess, regPrev);
        break;
      end;

      // If the edge below was a temporary edge introduced by
      // ConnectRightVertex, now is the time to fix it.
      //
      e := meshConnect(ePrev.Lprev, e.Sym);
      if (e = nil) then
        raise gluException.Create;

      if (not fixUpperEdge(reg, e)) then
        raise gluException.Create;
    end;

    // Relink edges so that ePrev.Onext == e
    if (ePrev.Onext <> e) then
    begin
      if (not meshSplice(e.Oprev, e)) then
        raise gluException.Create;
      if (not meshSplice(ePrev, e)) then
        raise gluException.Create;
    end;

    // may change reg.eUp
    FinishRegion(tess, regPrev);
    ePrev := reg.eUp;
    regPrev := reg;
  end;

  Result := ePrev;
end;

function VertL1dist(u, V: PGLUvertex): GLUfloat; {$IFDEF INLINE_ON} inline;
{$ENDIF}
begin
  Result := ABS(u.s - V.s) + ABS(u.t - V.t);
end;

// Find some weights which describe how the intersection vertex is
// which generated "isect" is allocated 50% of the weight; each edge
// splits the weight between its org and dst according to the
// relative distance to "isect".
///
procedure VertexWeights(isect, Org, Dst: PGLUvertex; weights: PGLUVector);
var
  t1, t2: GLUfloat;
begin
  t1 := VertL1dist(Org, isect);
  t2 := VertL1dist(Dst, isect);
  weights[0] := 0.5 * t2 / (t1 + t2);
  weights[1] := 0.5 * t1 / (t1 + t2);
  isect.coords.V[0] := isect.coords.V[0] + weights[0] * Org.coords.V[0] +
    weights[1] * Dst.coords.V[0];
  isect.coords.V[1] := isect.coords.V[1] + weights[0] * Org.coords.V[1] +
    weights[1] * Dst.coords.V[1];
  isect.coords.V[2] := isect.coords.V[2] + weights[0] * Org.coords.V[2] +
    weights[1] * Dst.coords.V[2];
end;

// We've computed a new intersection point, now we need a "data" pointer
// from the user so that we can refer to this new vertex in the
// rendering callbacks.
//
procedure GetIntersectData(tess: PGLUtesselator;
  isect, orgUp, dstUp, orgLo, dstLo: PGLUvertex);
var
  data: TGLUData;
  weights: TGLUVector;
begin
  isect.coords.V[0] := 0;
  isect.coords.V[1] := 0;
  isect.coords.V[2] := 0;

  data[0] := orgUp.data;
  data[1] := dstUp.data;
  data[2] := orgLo.data;
  data[3] := dstLo.data;
  VertexWeights(isect, orgUp, dstUp, @weights[0]);
  VertexWeights(isect, orgLo, dstLo, @weights[2]);

  callCombine(tess, isect, data, weights, TRUE);
end;

// Check the upper and lower edges of the given region to see if
// they intersect.  If so, create the intersection and add it
// to the data structures.
//
// Returns TRUE if adding the new intersection resulted in a recursive
// call to AddRightEdges(); in this case all "dirty" regions have been
// checked for intersections, and possibly regUp has been deleted.
///
function CheckForIntersect(tess: PGLUtesselator; regUp: PActiveRegion): Boolean;
var
  regLo: PActiveRegion;
  eUp, eLo: PGLUhalfEdge;
  orgUp, orgLo, dstUp, dstLo: PGLUvertex;
  tMinUp, tMaxLo: GLUfloat;
  isect: TGLUvertex;
  orgMin: PGLUvertex;
  e: PGLUhalfEdge;
begin
  regLo := RegionBelow(regUp);
  eUp := regUp.eUp;
  eLo := regLo.eUp;
  orgUp := eUp.Org;
  orgLo := eLo.Org;
  dstUp := eUp.Dst;
  dstLo := eLo.Dst;
  assert(not VertEq(dstLo, dstUp));
  assert(edgeSign(dstUp, tess.event, orgUp) <= 0);
  assert(edgeSign(dstLo, tess.event, orgLo) >= 0);
  assert((orgUp <> tess.event) and (orgLo <> tess.event));
  assert((not regUp.fixUpperEdge) and (not regLo.fixUpperEdge));

  if (orgUp = orgLo) then
  begin
    // right endpoints are the same///
    exit(FALSE);
  end;

  tMinUp := min(orgUp.t, dstUp.t);
  tMaxLo := max(orgLo.t, dstLo.t);
  if (tMinUp > tMaxLo) then
    // t ranges do not overlap///
    exit(FALSE);

  if (VertLeq(orgUp, orgLo)) then
  begin
    if (edgeSign(dstLo, orgUp, orgLo) > 0) then
      exit(FALSE);
  end
  else
  begin
    if (edgeSign(dstUp, orgLo, orgUp) < 0) then
      exit(FALSE);
  end;

  // At this point the edges intersect, at least marginally///

  edgeIntersect(dstUp, orgUp, dstLo, orgLo, @isect);
  // The following properties are guaranteed:///
  assert(min(orgUp.t, dstUp.t) <= isect.t);
  assert(isect.t <= max(orgLo.t, dstLo.t));
  assert(min(dstLo.s, dstUp.s) <= isect.s);
  assert(isect.s <= max(orgLo.s, orgUp.s));

  if (VertLeq(@isect, tess.event)) then
  begin
    // The intersection point lies slightly to the left of the sweep line,
    // so move it until it''s slightly to the right of the sweep line.
    // (If we had perfect numerical precision, this would never happen
    // in the first place).  The easiest and safest thing to do is
    // replace the intersection by tess.event.
    ///
    isect.s := tess.event.s;
    isect.t := tess.event.t;
  end;

  // Similarly, if the computed intersection lies to the right of the
  // rightmost origin (which should rarely happen), it can cause
  // unbelievable inefficiency on sufficiently degenerate inputs.
  // (If you have the test program, try running test54.d with the
  // "X zoom" option turned on).
  ///
  if VertLeq(orgUp, orgLo) then
    orgMin := orgUp
  else
    orgMin := orgLo;
  if (VertLeq(orgMin, @isect)) then
  begin
    isect.s := orgMin.s;
    isect.t := orgMin.t;
  end;

  if (VertEq(@isect, orgUp) or VertEq(@isect, orgLo)) then
  begin
    // Easy case -- intersection at one of the right endpoints///
    CheckForRightSplice(tess, regUp);
    exit(FALSE);
  end;

  if (not VertEq(dstUp, tess.event) and (edgeSign(dstUp, tess.event, @isect) >=
    0)) or (not VertEq(dstLo, tess.event) and (edgeSign(dstLo, tess.event,
    @isect) <= 0)) then
  begin
    // Very unusual -- the new upper or lower edge would pass on the
    // wrong side of the sweep event, or through it.  This can happen
    // due to very small numerical errors in the intersection calculation.
    ///
    if (dstLo = tess.event) then
    begin
      // Splice dstLo into eUp, and process the new region(s)///
      if (meshSplitEdge(eUp.Sym) = nil) then
        raise gluException.Create;
      if (not meshSplice(eLo.Sym, eUp)) then
        raise gluException.Create;
      regUp := TopLeftRegion(regUp);
      if (regUp = nil) then
        raise gluException.Create;
      eUp := RegionBelow(regUp).eUp;
      FinishLeftRegions(tess, RegionBelow(regUp), regLo);
      AddRightEdges(tess, regUp, eUp.Oprev, eUp, eUp, TRUE);
      exit(TRUE);
    end;

    if (dstUp = tess.event) then
    begin
      // Splice dstUp into eLo, and process the new region(s)///
      if (meshSplitEdge(eLo.Sym) = nil) then
        raise gluException.Create;
      if (not meshSplice(eUp.Lnext, eLo.Oprev)) then
        raise gluException.Create;
      regLo := regUp;
      regUp := TopRightRegion(regUp);
      e := RegionBelow(regUp).eUp.Rprev;
      regLo.eUp := eLo.Oprev;
      eLo := FinishLeftRegions(tess, regLo, nil);
      AddRightEdges(tess, regUp, eLo.Onext, eUp.Rprev, e, TRUE);

      exit(TRUE);
    end;

    // Special case: called from ConnectRightVertex.  If either
    // edge passes on the wrong side of tess.event, split it
    // (and wait for ConnectRightVertex to splice it appropriately).
    ///
    if (edgeSign(dstUp, tess.event, @isect) >= 0) then
    begin
      RegionAbove(regUp).dirty := TRUE;
      regUp.dirty := TRUE;
      if (meshSplitEdge(eUp.Sym) = nil) then
        raise gluException.Create;
      eUp.Org.s := tess.event.s;
      eUp.Org.t := tess.event.t;
    end;

    if (edgeSign(dstLo, tess.event, @isect) <= 0) then
    begin
      regUp.dirty := TRUE;
      regLo.dirty := TRUE;
      if (meshSplitEdge(eLo.Sym) = nil) then
        raise gluException.Create;
      eLo.Org.s := tess.event.s;
      eLo.Org.t := tess.event.t;
    end;

    // leave the rest for ConnectRightVertex///
    exit(FALSE);
  end;

  // General case -- split both edges, splice into new vertex.
  // When we do the splice operation, the order of the arguments is
  // arbitrary as far as correctness goes.  However, when the operation
  // creates a new face, the work done is proportional to the size of
  // the new face.  We expect the faces in the processed part of
  // the mesh (ie. eUp.Lface) to be smaller than the faces in the
  // unprocessed original contours (which will be eLo.Oprev.Lface).
  ///
  if (meshSplitEdge(eUp.Sym) = nil) then
    raise gluException.Create;
  if (meshSplitEdge(eLo.Sym) = nil) then
    raise gluException.Create;
  if (not meshSplice(eLo.Oprev, eUp)) then
    raise gluException.Create;
  eUp.Org.s := isect.s;
  eUp.Org.t := isect.t;

  eUp.Org.pqHandle := pqHeapInsert(tess.pq, eUp.Org);
  if (eUp.Org.pqHandle = LONG_MAX) then
  begin
    pqHeapDeletePriorityQ(tess.pq);
    tess.pq := nil;
    raise gluException.Create;
  end;
  GetIntersectData(tess, eUp.Org, orgUp, dstUp, orgLo, dstLo);
  RegionAbove(regUp).dirty := regUp.dirty = regLo.dirty = TRUE;

  Result := FALSE;
end;

// Check the upper and lower edge of "regUp", to make sure that the
// eUp->Dst is above eLo, or eLo->Dst is below eUp (depending on which
// destination is rightmost).
//
// Theoretically, this should always be true.  However, splitting an edge
// into two pieces can change the results of previous tests.  For example,
// suppose at one point we checked eUp and eLo, and decided that eUp->Dst
// is barely above eLo.  Then later, we split eLo into two edges (eg. from
// a splice operation like this one).  This can change the result of
// the test so that now eUp->Dst is incident to eLo, or barely below it.
// We must correct this condition to maintain the dictionary invariants
// (otherwise new edges might get inserted in the wrong place in the
// dictionary, and bad stuff will happen).
//
// We fix the problem by just splicing the offending vertex into the
// other edge.
///
function CheckForLeftSplice(tess: PGLUtesselator; regUp: PActiveRegion)
  : Boolean;
var
  regLo: PActiveRegion;
  eUp, eLo, e: PGLUhalfEdge;
begin
  regLo := RegionBelow(regUp);
  eUp := regUp.eUp;
  eLo := regLo.eUp;
  assert(not VertEq(eUp.Dst, eLo.Dst));

  if (VertLeq(eUp.Dst, eLo.Dst)) then
  begin
    if (edgeSign(eUp.Dst, eLo.Dst, eUp.Org) < 0) then
      exit(FALSE);

    // eLo.Dst is above eUp, so splice eLo.Dst into eUp
    RegionAbove(regUp).dirty := TRUE;
    regUp.dirty := TRUE;
    e := meshSplitEdge(eUp);
    if (e = nil) then
      raise gluException.Create;
    if (not meshSplice(eLo.Sym, e)) then
      raise gluException.Create;
    e.Lface.inside := regUp.inside;
  end
  else
  begin
    if (edgeSign(eLo.Dst, eUp.Dst, eLo.Org) > 0) then
      exit(FALSE);

    // eUp.Dst is below eLo, so splice eUp.Dst into eLo
    regUp.dirty := TRUE;
    regLo.dirty := TRUE;
    e := meshSplitEdge(eLo);
    if (e = nil) then
      raise gluException.Create;
    if (not meshSplice(eUp.Lnext, eLo.Sym)) then
      raise gluException.Create;
    e.Rface.inside := regUp.inside;
  end;

  Result := TRUE;
end;

// When the upper or lower edge of any region changes, the region is
// marked "dirty".  This routine walks through all the dirty regions
// and makes sure that the dictionary invariants are satisfied
// (see the comments at the beginning of this file).  Of course
// new dirty regions can be created as we make changes to restore
// the invariants.
///
procedure WalkDirtyRegions(tess: PGLUtesselator; regUp: PActiveRegion);
var
  regLo: PActiveRegion;
  eUp, eLo: PGLUhalfEdge;
begin
  regLo := RegionBelow(regUp);
  repeat
    // Find the lowest dirty region (we walk from the bottom up).
    while (regLo.dirty) do
    begin
      regUp := regLo;
      regLo := RegionBelow(regLo);
    end;
    if (not regUp.dirty) then
    begin
      regLo := regUp;
      regUp := RegionAbove(regUp);
      if (regUp = nil) or (not regUp.dirty) then
      begin
        // We've walked all the dirty regions
        exit;
      end;
    end;
    regUp.dirty := FALSE;
    eUp := regUp.eUp;
    eLo := regLo.eUp;

    if (eUp.Dst <> eLo.Dst) then
    begin
      // Check that the edge ordering is obeyed at the Dst vertices.
      if (CheckForLeftSplice(tess, regUp)) then
      begin
        // If the upper or lower edge was marked fixUpperEdge, then
        // we no longer need it (since these edges are needed only for
        // vertices which otherwise have no right-going edges).
        ///
        if (regLo.fixUpperEdge) then
        begin
          DeleteRegion(tess, regLo);
          if (not meshDelete(eLo)) then
            raise gluException.Create;
          regLo := RegionBelow(regUp);
          eLo := regLo.eUp;
        end
        else
        begin
          if (regUp.fixUpperEdge) then
          begin
            DeleteRegion(tess, regUp);
            if (not meshDelete(eUp)) then
              raise gluException.Create;
            regUp := RegionAbove(regLo);
            eUp := regUp.eUp;
          end;
        end;
      end;
    end;

    if (eUp.Org <> eLo.Org) then
    begin
      if (eUp.Dst <> eLo.Dst) and (not regUp.fixUpperEdge) and
        (not regLo.fixUpperEdge) and
        ((eUp.Dst = tess.event) or (eLo.Dst = tess.event)) then
      begin
        // When all else fails in CheckForIntersect(), it uses tess.event
        // as the intersection location.  To make this possible, it requires
        // that tess.event lie between the upper and lower edges, and also
        // that neither of these is marked fixUpperEdge (since in the worst
        // case it might splice one of these edges into tess.event, and
        // violate the invariant that fixable edges are the only right-going
        // edge from their associated vertex).
        ///
        if (CheckForIntersect(tess, regUp)) then
          // WalkDirtyRegions() was called recursively; we're done
          exit;
      end
      else
      begin
        // Even though we can't use CheckForIntersect(), the Org vertices
        // may violate the dictionary edge ordering.  Check and correct this.
        ///
        CheckForRightSplice(tess, regUp);
      end;
    end;

    if (eUp.Org = eLo.Org) and (eUp.Dst = eLo.Dst) then
    begin
      // A degenerate loop consisting of only two edges -- delete it.
      AddWinding(eLo, eUp);
      DeleteRegion(tess, regUp);
      if (not meshDelete(eUp)) then
        raise gluException.Create;
      regUp := RegionAbove(regLo);
    end;
  until FALSE;
end;

// Purpose: insert right-going edges into the edge dictionary, and update
// winding numbers and mesh connectivity appropriately.  All right-going
// edges share a common origin vOrg.  Edges are inserted CCW starting at
// eFirst; the last edge inserted is eLast->Oprev.  If vOrg has any
// left-going edges already processed, then eTopLeft must be the edge
// such that an imaginary upward vertical segment from vOrg would be
// contained between eTopLeft->Oprev and eTopLeft; otherwise eTopLeft
// should be NULL.
///
procedure AddRightEdges(tess: PGLUtesselator; regUp: PActiveRegion;
  eFirst, eLast, eTopLeft: PGLUhalfEdge; cleanUp: Boolean);
var
  reg, regPrev: PActiveRegion;
  e, ePrev: PGLUhalfEdge;
  firstTime: Boolean;
begin
  firstTime := TRUE;
  // Insert the new right-going edges in the dictionary
  e := eFirst;
  repeat
    assert(VertLeq(e.Org, e.Dst));
    AddRegionBelow(tess, regUp, e.Sym);
    e := e.Onext;
  until (e = eLast);

  // Walkall right-going edges from e.Org, in the dictionary order,
  // updating the winding numbers of each region, and re-linking the mesh
  // edges to match the dictionary ordering (if necessary).

  if (eTopLeft = nil) then
  begin
    eTopLeft := RegionBelow(regUp).eUp.Rprev;
  end;
  regPrev := regUp;
  ePrev := eTopLeft;

  repeat
    reg := RegionBelow(regPrev);
    e := reg.eUp.Sym;
    if (e.Org <> ePrev.Org) then
    begin
      break;
    end;

    if (e.Onext <> ePrev) then
    begin
      // Unlink e from its current position, and relink below ePrev
      if (not meshSplice(e.Oprev, e)) then
        raise gluException.Create;
      if (not meshSplice(ePrev.Oprev, e)) then
        raise gluException.Create;
    end;

    // Compute the winding number and "inside" flag for the new regions
    reg.windingNumber := regPrev.windingNumber - e.winding;
    reg.inside := IsWindingInside(tess, reg.windingNumber);

    // Check for two outgoing edges with same slope -- process these
    // before any intersection tests

    regPrev.dirty := TRUE;
    if (not firstTime) and CheckForRightSplice(tess, regPrev) then
    begin
      AddWinding(e, ePrev);
      DeleteRegion(tess, regPrev);
      if (not meshDelete(ePrev)) then
        raise gluException.Create;
    end;
    firstTime := FALSE;
    regPrev := reg;
    ePrev := e;
  until FALSE;
  regPrev.dirty := TRUE;
  assert(regPrev.windingNumber - e.winding = reg.windingNumber);

  if (cleanUp) then
  begin
    // Check for intersections between newly adjacent edges.
    WalkDirtyRegions(tess, regPrev);
  end;
end;

// The event vertex lies exacty on an already-processed edge or vertex.
// Adding the new vertex involves splicing it into the already-processed
// part of the mesh.
///
procedure ConnectLeftDegenerate(tess: PGLUtesselator; regUp: PActiveRegion;
  vEvent: PGLUvertex);
var
  e, eTopLeft, eTopRight, eLast: PGLUhalfEdge;
  reg: PActiveRegion;
begin
  e := regUp.eUp;
  if (VertEq(e.Org, vEvent)) then
  begin
    // e.Org is an unprocessed vertex - just combine them, and wait
    // for e.Org to be pulled from the queue

    SpliceMergeVertices(tess, e, vEvent.anEdge);
    exit;
  end;

  if (not VertEq(e.Dst, vEvent)) then
  begin
    // General case -- splice vEvent into edge e which passes through it
    if (meshSplitEdge(e.Sym) = nil) then
      raise gluException.Create;
    if (regUp.fixUpperEdge) then
    begin
      // This edge was fixable -- delete unused portion of original edge
      if (not meshDelete(e.Onext)) then
        raise gluException.Create;
      regUp.fixUpperEdge := FALSE;
    end;
    if (not meshSplice(vEvent.anEdge, e)) then
      raise gluException.Create;
    SweepEvent(tess, vEvent); // recurse
    exit;
  end;

  // vEvent coincides with e.Dst, which has already been processed.
  // Splice in the additional right-going edges.
  ///
  regUp := TopRightRegion(regUp);
  reg := RegionBelow(regUp);
  eTopRight := reg.eUp.Sym;
  eTopLeft := eTopRight.Onext;
  eLast := eTopRight.Onext;
  if (reg.fixUpperEdge) then
  begin
    // Here e.Dst has only a single fixable edge going right.
    // We can delete it since now we have some real right-going edges.
    ///
    assert(eTopLeft <> eTopRight); // there are some left edges too
    DeleteRegion(tess, reg);
    if (not meshDelete(eTopRight)) then
      raise gluException.Create;
    eTopRight := eTopLeft.Oprev;
  end;
  if (not meshSplice(vEvent.anEdge, eTopRight)) then
    raise gluException.Create;
  if (not EdgeGoesLeft(eTopLeft)) then
  begin
    // e.Dst had no left-going edges -- indicate this to AddRightEdges()
    eTopLeft := nil;
  end;
  AddRightEdges(tess, regUp, eTopRight.Onext, eLast, eTopLeft, TRUE);
end;

// Purpose: connect a "right" vertex vEvent (one where all edges go left)
// to the unprocessed portion of the mesh.  Since there are no right-going
// edges, two regions (one above vEvent and one below) are being merged
// into one. "regUp" is the upper of these two regions.
//
// There are two reasons for doing this (adding a right-going edge):
// - if the two regions being merged are "inside", we must add an edge
// to keep them separated (the combined region would not be monotone).
// - in any case, we must leave some record of vEvent in the dictionary,
// so that we can merge vEvent with features that we have not seen yet.
// For example, maybe there is a vertical edge which passes just to
// the right of vEvent; we would like to splice vEvent into this edge.
//
// However, we don't want to connect vEvent to just any vertex.  We don''t
// want the new edge to cross any other edges; otherwise we will create
// intersection vertices even when the input data had no self-intersections.
// (This is a bad thing; if the user's input data has no intersections,
// we don't want to generate any false intersections ourselves.)
//
// Our eventual goal is to connect vEvent to the leftmost unprocessed
// vertex of the combined region (the union of regUp and regLo).
// But because of unseen vertices with all right-going edges, and also
// new vertices which may be created by edge intersections, we don''t
// know where that leftmost unprocessed vertex is.  In the meantime, we
// connect vEvent to the closest vertex of either chain, and mark the region
// as "fixUpperEdge".  This flag says to delete and reconnect this edge
// to the next processed vertex on the boundary of the combined region.
// Quite possibly the vertex we connected to will turn out to be the
// closest one, in which case we won''t need to make any changes.
///
procedure ConnectRightVertex(tess: PGLUtesselator; regUp: PActiveRegion;
  eBottomLeft: PGLUhalfEdge);
var
  eNew, eTopLeft, eUp, eLo: PGLUhalfEdge;
  regLo: PActiveRegion;
  degenerate: Boolean;
begin
  eTopLeft := eBottomLeft.Onext;
  regLo := RegionBelow(regUp);
  eUp := regUp.eUp;
  eLo := regLo.eUp;
  degenerate := FALSE;
  if (eUp.Dst <> eLo.Dst) then
  begin
    CheckForIntersect(tess, regUp);
  end;

  // Possible new degeneracies: upper or lower edge of regUp may pass
  // through vEvent, or may coincide with new intersection vertex
  //
  if (VertEq(eUp.Org, tess.event)) then
  begin
    if (not meshSplice(eTopLeft.Oprev, eUp)) then
      raise gluException.Create;
    regUp := TopLeftRegion(regUp);
    if (regUp = nil) then
      raise gluException.Create;
    eTopLeft := RegionBelow(regUp).eUp;
    FinishLeftRegions(tess, RegionBelow(regUp), regLo);
    degenerate := TRUE;
  end;

  if (VertEq(eLo.Org, tess.event)) then
  begin
    if (not meshSplice(eBottomLeft, eLo.Oprev)) then
      raise gluException.Create;
    eBottomLeft := FinishLeftRegions(tess, regLo, nil);
    degenerate := TRUE;
  end;

  if (degenerate) then
  begin
    AddRightEdges(tess, regUp, eBottomLeft.Onext, eTopLeft, eTopLeft, TRUE);
    exit;
  end;

  // Non-degenerate situation -- need to add a temporary, fixable edge.
  // Connect to the closer of eLo.Org, eUp.Org.
  //
  if (VertLeq(eLo.Org, eUp.Org)) then
  begin
    eNew := eLo.Oprev;
  end
  else
  begin
    eNew := eUp;
  end;
  eNew := meshConnect(eBottomLeft.Lprev, eNew);
  if (eNew = nil) then
    raise gluException.Create;

  // Prevent cleanup, otherwise eNew might disappear before we've even
  // had a chance to mark it as a temporary edge.
  //
  AddRightEdges(tess, regUp, eNew, eNew.Onext, eNew.Onext, FALSE);
  eNew.Sym.activeRegion.fixUpperEdge := TRUE;
  WalkDirtyRegions(tess, regUp);
end;

// Does everything necessary when the sweep line crosses a vertex.
// Updates the mesh and the edge dictionary.
///
procedure SweepEvent(tess: PGLUtesselator; vEvent: PGLUvertex);
var
  regUp, reg: PActiveRegion;
  e, eTopLeft, eBottomLeft: PGLUhalfEdge;
begin
  tess.event := vEvent; // for access in EdgeLeq()

  // Check if this vertex is the right endpoint of an edge that is
  // already in the dictionary. In this case we don't need to waste
  // time searching for the location to insert new edges.
  ///
  e := vEvent.anEdge;

  while (e.activeRegion = nil) do
  begin
    e := e.Onext;
    if (e = vEvent.anEdge) then
    begin
      // All edges go right -- not incident to any processed edges
      ConnectLeftVertex(tess, vEvent);
      exit;
    end;
  end;

  // Processing consists of two phases: first we "finish" all the
  // active regions where both the upper and lower edges terminate
  // at vEvent (ie. vEvent is closing off these regions).
  // We mark these faces "inside" or "outside" the polygon according
  // to their winding number, and delete the edges from the dictionary.
  // This takes care of all the left-going edges from vEvent.
  ///
  regUp := TopLeftRegion(e.activeRegion);
  if (regUp = nil) then
    raise gluException.Create;
  reg := RegionBelow(regUp);
  eTopLeft := reg.eUp;
  eBottomLeft := FinishLeftRegions(tess, reg, nil);

  // Next we process all the right-going edges from vEvent.  This
  // involves adding the edges to the dictionary, and creating the
  // associated "active regions" which record information about the
  // regions between adjacent dictionary edges.
  ///
  if (eBottomLeft.Onext = eTopLeft) then
    // No right-going edges -- add a temporary "fixable" edge
    ConnectRightVertex(tess, regUp, eBottomLeft)
  else
    AddRightEdges(tess, regUp, eBottomLeft.Onext, eTopLeft, eTopLeft, TRUE);
end;

procedure ComputeWinding(tess: PGLUtesselator; reg: PActiveRegion);
begin
  reg.windingNumber := RegionAbove(reg).windingNumber + reg.eUp.winding;
  reg.inside := IsWindingInside(tess, reg.windingNumber);
end;

// Purpose: connect a "left" vertex (one where both edges go right)
// to the processed portion of the mesh.  Let R be the active region
// containing vEvent, and let U and L be the upper and lower edge
// chains of R.  There are two possibilities:
//
// - the normal case: split R into two regions, by connecting vEvent to
// the rightmost vertex of U or L lying to the left of the sweep line
//
// - the degenerate case: if vEvent is close enough to U or L, we
// merge vEvent into that edge chain.  The subcases are:
// - merging with the rightmost vertex of U or L
// - merging with the active edge of U or L
// - merging with an already-processed portion of U or L
///
procedure ConnectLeftVertex(tess: PGLUtesselator; vEvent: PGLUvertex);
var
  regUp, regLo, reg: PActiveRegion;
  tmp: TActiveRegion;
  eUp, eLo, eNew, tempHalfEdge: PGLUhalfEdge;
begin
  // Get a pointer to the active region containing vEvent
  tmp.eUp := vEvent.anEdge.Sym;
  regUp := PActiveRegion(dictSearch(tess.dict, @tmp).key);
  regLo := RegionBelow(regUp);
  eUp := regUp.eUp;
  eLo := regLo.eUp;

  // Try merging with U or L first
  if (edgeSign(eUp.Dst, vEvent, eUp.Org) = 0) then
  begin
    ConnectLeftDegenerate(tess, regUp, vEvent);
    exit;
  end;

  // Connect vEvent to rightmost processed vertex of either chain.
  // e.Dst is the vertex that we will connect to vEvent.
  if VertLeq(eLo.Dst, eUp.Dst) then
    reg := regUp
  else
    reg := regLo;

  if (regUp.inside) or (reg.fixUpperEdge) then
  begin
    if (reg = regUp) then
    begin
      eNew := meshConnect(vEvent.anEdge.Sym, eUp.Lnext);
      if (eNew = nil) then
        raise gluException.Create;
    end
    else
    begin
      tempHalfEdge := meshConnect(eLo.Dnext, vEvent.anEdge);
      if (tempHalfEdge = nil) then
        raise gluException.Create;

      eNew := tempHalfEdge.Sym;
    end;
    if (reg.fixUpperEdge) then
    begin
      if (not fixUpperEdge(reg, eNew)) then
        raise gluException.Create;
    end
    else
      ComputeWinding(tess, AddRegionBelow(tess, regUp, eNew));
    SweepEvent(tess, vEvent);
  end
  else
  begin
    // The new vertex is in a region which does not belong to the polygon.
    // We don''t need to connect this vertex to the rest of the mesh.

    AddRightEdges(tess, regUp, vEvent.anEdge, vEvent.anEdge, nil, TRUE);
  end;
end;

// Delete any degenerate faces with only two edges.  WalkDirtyRegions()
// will catch almost all of these, but it won't catch degenerate faces
// produced by splice operations on already-processed edges.
// The two places this can happen are in FinishLeftRegions(), when
// we splice in a "temporary" edge produced by ConnectRightVertex(),
// and in CheckForLeftSplice(), where we splice already-processed
// edges to ensure that our dictionary invariants are not violated
// by numerical errors.
//
// In both these cases it is //very// dangerous to delete the offending
// edge at the time, since one of the routines further up the stack
// will sometimes be keeping a pointer to that edge.
///
function RemoveDegenerateFaces(mesh: PGLUmesh): Boolean;
var
  f, fNext: PGLUface;
  e: PGLUhalfEdge;
begin
  f := mesh.fHead.next;
  while (f <> @mesh.fHead) do
  begin
    fNext := f.next;
    e := f.anEdge;
    assert(e.Lnext <> e);

    if (e.Lnext.Lnext = e) then
    begin
      // A face with only two edges
      AddWinding(e.Onext, e);
      if (not meshDelete(e)) then
        exit(FALSE);
    end;
    f := fNext;
  end;

  Result := TRUE;
end;

// checks a mesh for self-consistency.
///
procedure meshCheckMesh(mesh: PGLUmesh);
var
  fHead: PGLUface;
  vHead: PGLUvertex;
  eHead: PGLUhalfEdge;
  f, fPrev: PGLUface;
  V: PGLUvertex;
  vPrev: PGLUvertex;
  e, ePrev: PGLUhalfEdge;
begin
  fHead := @mesh.fHead;
  vHead := @mesh.vHead;
  eHead := @mesh.eHead;

  fPrev := fHead;
  f := fPrev.next;
  while (f <> fHead) do
  begin
    assert(f.prev = fPrev);
    e := f.anEdge;
    repeat
      assert(e.Sym <> e);
      assert(e.Sym.Sym = e);
      assert(e.Lnext.Onext.Sym = e);
      assert(e.Onext.Sym.Lnext = e);
      assert(e.Lface = f);
      e := e.Lnext;
    until (e = f.anEdge);
    fPrev := f;
    f := fPrev.next;
  end;
  assert((f.prev = fPrev) and (f.anEdge = nil) and (f.data = nil));

  vPrev := vHead;
  V := vPrev.next;
  while (V <> vHead) do
  begin
    assert(V.prev = vPrev);
    e := V.anEdge;
    repeat
      assert(e.Sym <> e);
      assert(e.Sym.Sym = e);
      assert(e.Lnext.Onext.Sym = e);
      assert(e.Onext.Sym.Lnext = e);
      assert(e.Org = V);
      e := e.Onext;
    until (e = V.anEdge);
    vPrev := V;
    V := vPrev.next;
  end;
  assert((V.prev = vPrev) and (V.anEdge = nil) and (V.data = nil));

  ePrev := eHead;
  e := ePrev.next;
  while (e <> eHead) do
  begin
    assert(e.Sym.next = ePrev.Sym);
    assert(e.Sym <> e);
    assert(e.Sym.Sym = e);
    assert(e.Org <> nil);
    assert(e.Dst <> nil);
    assert(e.Lnext.Onext.Sym = e);
    assert(e.Onext.Sym.Lnext = e);
    ePrev := e;
    e := ePrev.next;
  end;

  assert((e.Sym.next = ePrev.Sym) and (e.Sym = @mesh.eHeadSym) and
    (e.Sym.Sym = e) and (e.Org = nil) and (e.Dst = nil) and (e.Lface = nil) and
    (e.Rface = nil));
end;

procedure DoneEdgeDict(tess: PGLUtesselator);
var
  reg: PActiveRegion;
  fixedEdges: Integer;
begin
  fixedEdges := 0;

  reg := PActiveRegion(tess.dict.head.next.key);
  while (reg <> nil) do
  begin
    // At the end of all processing, the dictionary should contain
    // only the two sentinel edges, plus at most one "fixable" edge
    // created by ConnectRightVertex().
    //
    if (not reg.sentinel) then
    begin
      assert(reg.fixUpperEdge);
      Inc(fixedEdges);
      assert(fixedEdges = 1);
    end;
    assert(reg.windingNumber = 0);
    DeleteRegion(tess, reg);
    reg := PActiveRegion(tess.dict.head.next.key);
  end;
  dictDeleteDict(tess.dict);
end;

// computes the planar arrangement specified
// by the given contours, and further subdivides this arrangement
// into regions.  Each region is marked "inside" if it belongs
// to the polygon, according to the rule given by tess->windingRule.
// Each interior region is guaranteed be monotone.
///
function computeInterior(tess: PGLUtesselator): Boolean;
var
  V, vNext: PGLUvertex;
begin
  tess.fatalError := FALSE;

  // Each vertex defines an event for our sweep line.  Start by inserting
  // all the vertices in a priority queue.  Events are processed in
  // lexicographic order, ie.
  //
  // e1 < e2  iff  e1.x < e2.x || (e1.x == e2.x && e1.y < e2.y)
  ///
  RemoveDegenerateEdges(tess);
  if (not InitPriorityQ(tess)) then
    exit(FALSE); // if error

  InitEdgeDict(tess);

  V := pqHeapExtractMin(tess.pq);
  while (V <> nil) do
  begin
    repeat
      vNext := pqHeapMinimum(tess.pq);
      if (vNext = nil) or not VertEq(vNext, V) then
        break;

      // Merge together all vertices at exactly the same location.
      // This is more efficient than processing them one at a time,
      // simplifies the code (see ConnectLeftDegenerate), and is also
      // important for correct handling of certain degenerate cases.
      // For example, suppose there are two identical edges A and B
      // that belong to different contours (so without this code they would
      // be processed by separate sweep events).  Suppose another edge C
      // crosses A and B from above.  When A is processed, we split it
      // at its intersection point with C.  However this also splits C,
      // so when we insert B we may compute a slightly different
      // intersection point.  This might leave two edges with a small
      // gap between them.  This kind of error is especially obvious
      // when using boundary extraction (GLU_TESS_BOUNDARY_ONLY).
      ///
      vNext := pqHeapExtractMin(tess.pq);
      SpliceMergeVertices(tess, V.anEdge, vNext.anEdge);
    until FALSE;
    SweepEvent(tess, V);
    V := pqHeapExtractMin(tess.pq);
  end;

  // Set tess->event for debugging purposes
  tess.event := PActiveRegion(tess.dict.head.next.key).eUp.Org;
  DoneEdgeDict(tess);
  pqHeapDeletePriorityQ(tess.pq);

  if (not RemoveDegenerateFaces(tess.mesh)) then
    exit(FALSE);
  meshCheckMesh(tess.mesh);

  Result := TRUE;
end;

// ( mesh, value, keepOnlyBoundary ) resets the
// winding numbers on all edges so that regions marked "inside" the
// polygon have a winding number of "value", and regions outside
// have a winding number of 0.
//
// If keepOnlyBoundary is TRUE, it also deletes all edges which do not
// separate an interior region from an exterior one.
///
function meshSetWindingNumber(mesh: PGLUmesh; Value: Integer;
  keepOnlyBoundary: Boolean): Boolean;
var
  e, eNext: PGLUhalfEdge;
begin
  e := mesh.eHead.next;
  while (e <> @mesh.eHead) do
  begin
    eNext := e.next;
    if (e.Rface.inside <> e.Lface.inside) then
    begin
      // This is a boundary edge (one side is interior, one is exterior).
      if e.Lface.inside then
        e.winding := Value
      else
        e.winding := -Value;
    end
    else
    begin
      // Both regions are interior, or both are exterior.
      if (not keepOnlyBoundary) then
        e.winding := 0
      else if (not meshDelete(e)) then
        exit(FALSE);
    end;
    e := eNext;
  end;

  Result := TRUE;
end;

// tessellates a monotone region
// (what else would it do??)  The region must consist of a single
// loop of half-edges (see mesh.h) oriented CCW.  "Monotone" in this
// case means that any vertical line intersects the interior of the
// region in a single interval.
//
// Tessellation consists of adding interior edges (actually pairs of
// half-edges), to split the region into non-overlapping triangles.
//
// The basic idea is explained in Preparata and Shamos (which I don''t
// have handy right now), although their implementation is more
// complicated than this one.  The are two edge chains, an upper chain
// and a lower chain.  We process all vertices from both chains in order,
// from right to left.
//
// The algorithm ensures that the following invariant holds after each
// vertex is processed: the untessellated region consists of two
// chains, where one chain (say the upper) is a single edge, and
// the other chain is concave.  The left vertex of the single edge
// is always to the left of all vertices in the concave chain.
//
// Each step consists of adding the rightmost unprocessed vertex to one
// of the two chains, and forming a fan of triangles from the rightmost
// of two chain endpoints.  Determining whether we can add each triangle
// to the fan is a simple orientation test.  By making the fan as large
// as possible, we restore the invariant (check it yourself).
///
function meshTessellateMonoRegion(face: PGLUface): Boolean;
var
  up, lo, tempHalfEdge: PGLUhalfEdge;
begin
  /// All edges are oriented CCW around the boundary of the region.
  // First, find the half-edge whose origin vertex is rightmost.
  // Since the sweep goes from left to right, face.anEdge should
  // be close to the edge we want.
  ///
  up := face.anEdge;
  assert((up.Lnext <> up) and (up.Lnext.Lnext <> up));

  while VertLeq(up.Dst, up.Org) do
    up := up.Lprev;
  while VertLeq(up.Org, up.Dst) do
    up := up.Lnext;
  lo := up.Lprev;

  while (up.Lnext <> lo) do
  begin
    if (VertLeq(up.Dst, lo.Org)) then
    begin
      /// up.Dst is on the left.  It is safe to form triangles from lo.Org.
      // The EdgeGoesLeft test guarantees progress even when some triangles
      // are CW, given that the upper and lower chains are truly monotone.
      ///
      while ((lo.Lnext <> up) and (EdgeGoesLeft(lo.Lnext) or
        (edgeSign(lo.Org, lo.Dst, lo.Lnext.Dst) <= 0))) do
      begin
        tempHalfEdge := meshConnect(lo.Lnext, lo);
        if (tempHalfEdge = nil) then
          exit(FALSE);
        lo := tempHalfEdge.Sym;
      end;
      lo := lo.Lprev;
    end
    else
    begin
      /// lo.Org is on the left.  We can make CCW triangles from up.Dst. ///
      while ((lo.Lnext <> up) and (EdgeGoesRight(up.Lprev) or
        (edgeSign(up.Dst, up.Org, up.Lprev.Org) >= 0))) do
      begin
        tempHalfEdge := meshConnect(up, up.Lprev);
        if (tempHalfEdge = nil) then
          exit(FALSE);
        up := tempHalfEdge.Sym;
      end;
      up := up.Lnext;
    end;
  end;

  /// Now lo.Org == up.Dst == the leftmost vertex.  The remaining region
  // can be tessellated in a fan from this leftmost vertex.
  ///
  assert(lo.Lnext <> up);
  while (lo.Lnext.Lnext <> up) do
  begin
    tempHalfEdge := meshConnect(lo.Lnext, lo);
    if (tempHalfEdge = nil) then
      exit(FALSE);
    lo := tempHalfEdge.Sym;
  end;

  Result := TRUE;
end;

// tessellates each region of
// the mesh which is marked "inside" the polygon.  Each such region
// must be monotone.
///
function meshTessellateInterior(mesh: PGLUmesh): Boolean;
var
  f, next: PGLUface;
begin
  f := mesh.fHead.next;
  while (f <> @mesh.fHead) do
  begin
    // Make sure we don''t try to tessellate the new triangles.
    next := f.next;
    if (f.inside) then
      if (not meshTessellateMonoRegion(f)) then
        exit(FALSE);
    f := next;
  end;

  Result := TRUE;
end;

// takes a mesh, and outputs one
// contour for each face marked "inside".  The rendering output is
// provided as callbacks (see the api).
///
procedure renderBoundary(tess: PGLUtesselator; mesh: PGLUmesh);
var
  f: PGLUface;
  e: PGLUhalfEdge;
begin
  f := mesh.fHead.next;
  while (f <> @mesh.fHead) do
  begin
    if (f.inside) then
    begin
      if IsCallBeginData(tess) then
        tess.callBeginData(GL_LINE_LOOP, tess.polygonData)
      else
        tess.callBegin(GL_LINE_LOOP);
      e := f.anEdge;
      repeat
        if IsCallVertexData(tess) then
          tess.callVertexData(e.Org.data, tess.polygonData)
        else
          tess.callVertex(e.Org.data);
        e := e.Lnext;
      until (e = f.anEdge);
      if IsCallEndData(tess) then
        tess.callEndData(tess.polygonData)
      else
        tess.callEnd();
    end;
    f := f.next;
  end;
end;

function marked(f: PGLUface): Boolean;
begin
  Result := not f.inside or f.marked;
end;

procedure AddToTrail(f: PGLUface; var t: PGLUface);
begin
  f.trail := t;
  t := f;
  f.marked := TRUE;
end;

procedure FreeTrail(t: PGLUface);
begin
  while (t <> nil) do
  begin
    t.marked := FALSE;
    t := t.trail;
  end;
  // absorb trailing semicolon
end;

procedure RenderFan(tess: PGLUtesselator; e: PGLUhalfEdge; size: LongInt);
begin
  // Render as many CCW triangles as possible in a fan starting from
  // edge "e".  The fan *should* contain exactly "size" triangles
  // (otherwise we've goofed up somewhere).
  //
  if IsCallBeginData(tess) then
    tess.callBeginData(GL_TRIANGLE_FAN, tess.polygonData)
  else
    tess.callBegin(GL_TRIANGLE_FAN);
  if IsCallVertexData(tess) then
    tess.callVertexData(e.Org.data, tess.polygonData)
  else
    tess.callVertex(e.Org.data);
  if IsCallVertexData(tess) then
    tess.callVertexData(e.Dst.data, tess.polygonData)
  else
    tess.callVertex(e.Dst.data);

  while (not marked(e.Lface)) do
  begin
    e.Lface.marked := TRUE;
    Dec(size);
    e := e.Onext;
    if IsCallVertexData(tess) then
      tess.callVertexData(e.Dst.data, tess.polygonData)
    else
      tess.callVertex(e.Dst.data);
  end;

  assert(size = 0);
  if IsCallEndData(tess) then
    tess.callEndData(tess.polygonData)
  else
    tess.callEnd();
end;

procedure RenderStrip(tess: PGLUtesselator; e: PGLUhalfEdge; size: LongInt);
begin
  // Render as many CCW triangles as possible in a strip starting from
  // edge "e".  The strip *should* contain exactly "size" triangles
  // (otherwise we've goofed up somewhere).
  //
  if IsCallBeginData(tess) then
    tess.callBeginData(GL_TRIANGLE_STRIP, tess.polygonData)
  else
    tess.callBegin(GL_TRIANGLE_STRIP);
  if IsCallVertexData(tess) then
    tess.callVertexData(e.Org.data, tess.polygonData)
  else
    tess.callVertex(e.Org.data);
  if IsCallVertexData(tess) then
    tess.callVertexData(e.Dst.data, tess.polygonData)
  else
    tess.callVertex(e.Dst.data);

  while (not marked(e.Lface)) do
  begin
    e.Lface.marked := TRUE;
    Dec(size);
    e := e.Dprev;
    if IsCallVertexData(tess) then
      tess.callVertexData(e.Org.data, tess.polygonData)
    else
      tess.callVertex(e.Org.data);
    if (marked(e.Lface)) then
      break;

    e.Lface.marked := TRUE;
    Dec(size);
    e := e.Onext;
    if IsCallVertexData(tess) then
      tess.callVertexData(e.Dst.data, tess.polygonData)
    else
      tess.callVertex(e.Dst.data)
  end;

  assert(size = 0);
  if IsCallEndData(tess) then
    tess.callEndData(tess.polygonData)
  else
    tess.callEnd();
end;

function MaximumFan(eOrig: PGLUhalfEdge): TFaceCount;
var
  // eOrig->Lface is the face we want to render.  We want to find the size
  // of a maximal fan around eOrig->Org.  To do this we just walk around
  // the origin vertex as far as possible in both directions.
  //
  newFace: TFaceCount;
  trail: PGLUface;
  e: PGLUhalfEdge;
begin
  trail := nil;
  newFace.size := 0;
  newFace.eStart := nil;
  newFace.render := RenderFan;
  e := eOrig;
  while (not marked(e.Lface)) do
  begin
    AddToTrail(e.Lface, trail);
    Inc(newFace.size);
    e := e.Onext;
  end;
  e := eOrig;
  while (not marked(e.Rface)) do
  begin
    AddToTrail(e.Rface, trail);
    Inc(newFace.size);
    e := e.Oprev
  end;

  newFace.eStart := e;

  FreeTrail(trail);
  Result := newFace;
end;

function IsEven(n: LongInt): Boolean; {$IFDEF INLINE_ON} inline; {$ENDIF}
begin
  Result := (n and 1) = 0;
end;

function MaximumStrip(eOrig: PGLUhalfEdge): TFaceCount;
var
  // Here we are looking for a maximal strip that contains the vertices
  // eOrig->Org, eOrig->Dst, eOrig->Lnext->Dst (in that order or the
  // reverse, such that all triangles are oriented CCW).
  //
  // Again we walk forward and backward as far as possible.  However for
  // strips there is a twist: to get CCW orientations, there must be
  // an *even* number of triangles in the strip on one side of eOrig.
  // We walk the strip starting on a side with an even number of triangles;
  // if both side have an odd number, we are forced to shorten one side.
  //
  newFace: TFaceCount;
  headSize, tailSize: LongInt;
  trail: PGLUface;
  e, eTail, eHead: PGLUhalfEdge;
begin
  newFace.size := 0;
  newFace.eStart := nil;
  newFace.render := RenderStrip;
  headSize := 0;
  tailSize := 0;
  trail := nil;
  e := eOrig;
  while (not marked(e.Lface)) do
  begin
    AddToTrail(e.Lface, trail);
    Inc(tailSize);
    e := e.Dprev;
    if (marked(e.Lface)) then
      break;
    AddToTrail(e.Lface, trail);
    Inc(tailSize);
    e := e.Onext;
  end;
  eTail := e;
  e := eOrig;
  while (not marked(e.Rface)) do
  begin
    AddToTrail(e.Rface, trail);
    Inc(headSize);
    e := e.Oprev;

    if (marked(e.Rface)) then
      break;
    AddToTrail(e.Rface, trail);
    Inc(headSize);
    e := e.Dnext;
  end;
  eHead := e;

  newFace.size := tailSize + headSize;
  if (IsEven(tailSize)) then
    newFace.eStart := eTail.Sym
  else
  begin
    if (IsEven(headSize)) then
      newFace.eStart := eHead
    else
    begin
      // Both sides have odd length, we must shorten one of them.  In fact,
      // we must start from eHead to guarantee inclusion of eOrig->Lface.

      Dec(newFace.size);
      newFace.eStart := eHead.Onext;
    end;
  end;

  FreeTrail(trail);
  Result := newFace;
end;

procedure RenderTriangle(tess: PGLUtesselator; e: PGLUhalfEdge; size: LongInt);
begin
  // Just add the triangle to a triangle list, so we can render all
  // the separate triangles at once.
  //
  assert(size = 1);
  AddToTrail(e.Lface, tess.lonelyTriList);
end;

procedure RenderLonelyTriangles(tess: PGLUtesselator; f: PGLUface);
var
  // Now we render all the separate triangles which could not be
  // grouped into a triangle fan or strip.
  //
  e: PGLUhalfEdge;
  newState: Boolean;
  edgeState: Boolean; // force edge state output for first vertex
begin
  edgeState := TRUE;
  if IsCallBeginData(tess) then
    tess.callBeginData(GL_TRIANGLES, tess.polygonData)
  else
    tess.callBegin(GL_TRIANGLES);

  while (f <> nil) do
  begin
    // Loop once for each edge (there will always be 3 edges)

    e := f.anEdge;
    repeat
      if (tess.flagBoundary) then
      begin
        // Set the "edge state" to TRUE just before we output the
        // first vertex of each edge on the polygon boundary.
        //
        newState := not e.Rface.inside;
        if (edgeState <> newState) then
        begin
          edgeState := newState;
          if IsCallEdgeFlagData(tess) then
            tess.callEdgeFlagData(edgeState, tess.polygonData)
          else
            tess.callEdgeFlag(edgeState);
        end;
      end;
      if IsCallVertexData(tess) then
        tess.callVertexData(e.Org.data, tess.polygonData)
      else
        tess.callVertex(e.Org.data);
      e := e.Lnext;
    until (e = f.anEdge);
    f := f.trail;
  end;

  if IsCallEndData(tess) then
    tess.callEndData(tess.polygonData)
  else
    tess.callEnd();
end;

procedure RenderMaximumFaceGroup(tess: PGLUtesselator; fOrig: PGLUface);
var
  // We want to find the largest triangle fan or strip of unmarked faces
  // which includes the given face fOrig.  There are 3 possible fans
  // passing through fOrig (one centered at each vertex), and 3 possible
  // strips (one for each CCW permutation of the vertices).  Our strategy
  // is to try all of these, and take the primitive which uses the most
  // triangles (a greedy approach).
  ///
  e: PGLUhalfEdge;
  max, newFace: TFaceCount;
begin

  e := fOrig.anEdge;
  max.size := 1;
  max.eStart := e;
  max.render := RenderTriangle;

  if (not tess.flagBoundary) then
  begin
    newFace := MaximumFan(e);
    if (newFace.size > max.size) then
    begin
      max := newFace;
    end;
    newFace := MaximumFan(e.Lnext);
    if (newFace.size > max.size) then
    begin
      max := newFace;
    end;
    newFace := MaximumFan(e.Lprev);
    if (newFace.size > max.size) then
    begin
      max := newFace;
    end;

    newFace := MaximumStrip(e);
    if (newFace.size > max.size) then
    begin
      max := newFace;
    end;
    newFace := MaximumStrip(e.Lnext);
    if (newFace.size > max.size) then
    begin
      max := newFace;
    end;
    newFace := MaximumStrip(e.Lprev);
    if (newFace.size > max.size) then
    begin
      max := newFace;
    end;
  end;
  max.render(tess, max.eStart, max.size);
end;

// ************************ Strips and Fans decomposition ******************//
// takes a mesh and breaks it into triangle
// fans, strips, and separate triangles.  A substantial effort is made
// to use as few rendering primitives as possible (ie. to make the fans
// and strips as large as possible).
//
// The rendering output is provided as callbacks (see the api).
///
procedure renderMesh(tess: PGLUtesselator; mesh: PGLUmesh);
var
  f: PGLUface;
begin
  // Make a list of separate triangles so we can render them all at once
  tess.lonelyTriList := nil;
  f := mesh.fHead.next;
  while (f <> @mesh.fHead) do
  begin
    f.marked := FALSE;
    f := f.next;
  end;
  f := mesh.fHead.next;
  while (f <> @mesh.fHead) do
  begin
    // We examine all faces in an arbitrary order.  Whenever we find
    // an unprocessed face F, we output a group of faces including F
    // whose size is maximum.
    //
    if (f.inside and not f.marked) then
    begin
      RenderMaximumFaceGroup(tess, f);
      assert(f.marked);
    end;
    f := f.next;
  end;

  if (tess.lonelyTriList <> nil) then
  begin
    RenderLonelyTriangles(tess, tess.lonelyTriList);
    tess.lonelyTriList := nil;
  end;
end;


// ******************* Other Operations **********************//

// destroys a face and removes it from the
// global face list. All edges of fZap will have a NULL pointer as their
// left face. Any edges which also have a NULL pointer as their right face
// are deleted entirely (along with any isolated vertices this produces).
// An entire mesh can be deleted by zapping its faces, one at a time,
// in any order. Zapped faces cannot be used in further mesh operations!
///
procedure meshZapFace(fZap: PGLUface);
var
  eStart: PGLUhalfEdge;
  e, eNext, eSym: PGLUhalfEdge;
  fPrev, fNext: PGLUface;
begin
  eStart := fZap.anEdge;

  // walk around face, deleting edges whose right face is also NULL
  eNext := eStart.Lnext;
  repeat
    e := eNext;
    eNext := e.Lnext;

    e.Lface := nil;
    if (e.Rface = nil) then
    begin
      // delete the edge -- see MeshDelete above
      if (e.Onext = e) then
        KillVertex(e.Org, nil)
      else
      begin
        // Make sure that e.Org points to a valid half-edge
        e.Org.anEdge := e.Onext;
        Splice(e, e.Oprev);
      end;
      eSym := e.Sym;
      if (eSym.Onext = eSym) then
        KillVertex(eSym.Org, nil)
      else
      begin
        // Make sure that eSym.Org points to a valid half-edge
        eSym.Org.anEdge := eSym.Onext;
        Splice(eSym, eSym.Oprev);
      end;
      KillEdge(e);
    end;
  until (e = eStart);

  // delete from circular doubly-linked list
  fPrev := fZap.prev;
  fNext := fZap.next;
  fNext.prev := fPrev;
  fPrev.next := fNext;

  FreeMem(fZap);
end;


// zaps (ie. sets to NULL) all faces

// which are not marked "inside" the polygon.  Since further mesh operations
// on NULL faces are not allowed, the main purpose is to clean up the
// mesh so that exterior loops are not represented in the data structure.
///
procedure meshDiscardExterior(mesh: PGLUmesh);
var
  f: PGLUface;
  next: PGLUface;
begin
  f := mesh.fHead.next;
  while (f <> @mesh.fHead) do
  begin
    // Since f will be destroyed, save its next pointer.
    next := f.next;
    if (not f.inside) then
      meshZapFace(f);
    f := next;
  end;
end;

procedure gluTessEndPolygon(tess: PGLUtesselator);
var
  mesh: PGLUmesh;
  rc: Boolean;
begin

  try
    if (tess.state <> T_IN_POLYGON) then
      GotoState(tess, T_IN_POLYGON);
    tess.state := T_DORMANT;

    if (tess.mesh = nil) then
    begin
      if (not tess.flagBoundary and IsCallMesh(tess)) then
      begin
        // Try some special code to make the easy s go quickly
        // (eg. convex polygons).  This code does NOT handle multiple contours,
        // intersections, edge flags, and of course it does not generate
        // an explicit mesh either.
        //
        if (renderCache(tess)) then
        begin
          tess.polygonData := nil;
          exit;
        end;
      end;
      if (not emptyCache(tess)) then
        raise gluException.Create;
    end;

    // Determine the polygon normal and project vertices onto the plane
    // of the polygon.
    //
    projectPolygon(tess);

    // computeInterior( tess ) computes the planar arrangement specified
    // by the given contours, and further subdivides this arrangement
    // into regions.  Each region is marked "inside" if it belongs
    // to the polygon, according to the rule given by tess.windingRule.
    // Each interior region is guaranteed be monotone.
    //
    if (not computeInterior(tess)) then
      raise gluException.Create;

    mesh := tess.mesh;
    if (not tess.fatalError) then
    begin
      // If the user wants only the boundary contours, we throw away all edges
      // except those which separate the interior from the exterior.
      // Otherwise we tessellate all the regions marked "inside".
      //
      if (tess.boundaryOnly) then
        rc := meshSetWindingNumber(mesh, 1, TRUE)
      else
        rc := meshTessellateInterior(mesh);
      if (not rc) then
        raise gluException.Create;

      meshCheckMesh(mesh);

      if (@tess.callBegin = @noBegin) or (@tess.callEnd = @noEnd) or
        (@tess.callVertex = @noVertex) or (@tess.callEdgeFlag = @noEdgeFlag) or
        IsCallBeginData(tess) or IsCallEndData(tess) or IsCallVertexData(tess)
        or IsCallEdgeFlagData(tess) then
      begin
        if (tess.boundaryOnly) then
        begin
          renderBoundary(tess, mesh); // output boundary contours */
        end
        else
        begin
          renderMesh(tess, mesh); // output strips and fans */
        end;
      end;
    end;

    if (not IsCallMesh(tess)) then
    begin
      // Throw away the exterior faces, so that all faces are interior.
      // This way the user doesn't have to check the "inside" flag,
      // and we don't need to even reveal its existence.  It also leaves
      // the freedom for an implementation to not generate the exterior
      // faces in the first place.
      //
      meshDiscardExterior(mesh);
      tess.callMesh(mesh); // user wants the mesh itself */
      tess.mesh := nil;
      tess.polygonData := nil;
      exit;
    end;
    meshDeleteMesh(mesh);
    tess.polygonData := nil;
    tess.mesh := nil;

  except
    on e: gluException do
    begin
      // come back here if out of memory
      if IsCallErrorData(tess) then
        tess.callErrorData(GLU_OUT_OF_MEMORY, tess.polygonData)
      else
        tess.callError(GLU_OUT_OF_MEMORY);
    end
    else
      raise;
  end;
end;

// ******************************************************/

// Obsolete calls -- for backward compatibility */
procedure gluBeginPolygon(tess: PGLUtesselator);
begin
  gluTessBeginPolygon(tess, nil);
  gluTessBeginContour(tess);
end;

procedure gluNextContour(tess: PGLUtesselator; aType: GLUenum);
begin
  gluTessEndContour(tess);
  gluTessBeginContour(tess);
end;

procedure gluEndPolygon(tess: PGLUtesselator);
begin
  gluTessEndContour(tess);
  gluTessEndPolygon(tess);
end;

{ TGLUhalfEdge }

function TGLUhalfEdge.GetDnext: PGLUhalfEdge;
begin
  Result := Rprev.Sym;
end;

function TGLUhalfEdge.GetDprev: PGLUhalfEdge;
begin
  Result := Lnext.Sym;
end;

function TGLUhalfEdge.GetDst: PGLUvertex;
begin
  Result := Sym.Org;
end;

function TGLUhalfEdge.GetLprev: PGLUhalfEdge;
begin
  Result := Onext.Sym;
end;

function TGLUhalfEdge.GetOprev: PGLUhalfEdge;
begin
  Result := Sym.Lnext;
end;

function TGLUhalfEdge.GetRface: PGLUface;
begin
  Result := Sym.Lface;
end;

function TGLUhalfEdge.GetRnext: PGLUhalfEdge;
begin
  Result := Oprev.Sym;
end;

function TGLUhalfEdge.GetRprev: PGLUhalfEdge;
begin
  Result := Sym.Onext;
end;

procedure TGLUhalfEdge.SetDnext(const Value: PGLUhalfEdge);
begin
  Rprev.Sym := Value;
end;

procedure TGLUhalfEdge.SetDprev(const Value: PGLUhalfEdge);
begin
  Lnext.Sym := Value;
end;

procedure TGLUhalfEdge.SetDst(const Value: PGLUvertex);
begin
  Sym.Org := Value;
end;

procedure TGLUhalfEdge.SetLprev(const Value: PGLUhalfEdge);
begin
  Onext.Sym := Value;
end;

procedure TGLUhalfEdge.SetOprev(const Value: PGLUhalfEdge);
begin
  Sym.Lnext := Value;
end;

procedure TGLUhalfEdge.SetRface(const Value: PGLUface);
begin
  Sym.Lface := Value;
end;

procedure TGLUhalfEdge.SetRnext(const Value: PGLUhalfEdge);
begin
  Oprev.Sym := Value;
end;

procedure TGLUhalfEdge.SetRprev(const Value: PGLUhalfEdge);
begin
  Sym.Onext := Value;
end;

{ TGLUAffineVector }

class operator TGLUAffineVector.Add(const v1, v2: TGLUAffineVector): TGLUAffineVector;
begin
  Result.V[0] := v1.V[0] + v2.V[0];
  Result.V[1] := v1.V[1] + v2.V[1];
  Result.V[2] := v1.V[2] + v2.V[2];
end;

class operator TGLUAffineVector.Equal(const v1, v2: TGLUAffineVector): Boolean;
begin
  Result := (v1.V[0] = v2.V[0]) and (v1.V[1] = v2.V[1]) and (v1.V[2] = v2.V[2]);
end;

function TGLUAffineVector.IsNull: Boolean;
begin
  Result := (V[0] = 0.0) and (V[1] = 0.0) and (V[2] = 0.0);
end;

class function TGLUAffineVector.Make(x, y, z: GLUfloat): TGLUAffineVector;
begin
  Result.V[0] := x;
  Result.V[1] := y;
  Result.V[2] := z;
end;

class operator TGLUAffineVector.NotEqual(const v1, v2: TGLUAffineVector): Boolean;
begin
  Result := (v1.V[0] <> v2.V[0]) or (v1.V[1] <> v2.V[1]) or
    (v1.V[2] <> v2.V[2]);
end;

procedure TGLUAffineVector.SetScale(k: GLUfloat);
begin
  V[0] := k * V[0];
  V[1] := k * V[1];
  V[2] := k * V[2];
end;

initialization

Set8087CW($133F);

end.
