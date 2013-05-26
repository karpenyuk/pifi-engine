unit uNeighborSearching;

interface

{$POINTERMATH ON}

uses
  Math;

const
  ANN_AR_TOOBIG: double = 1000;
  ERR: double = 0.001; // a small value
  FS_ASPECT_RATIO: double = 3.0; // maximum allowed aspect ratio

  // ----------------------------------------------------------------------
  // Limit on number of points visited
  // We have an option for terminating the search early if the
  // number of points visited exceeds some threshold.  If the
  // threshold is 0 (its default)  this means there is no limit
  // and the algorithm applies its normal termination condition.
  // This is for applications where there are real time constraints
  // on the running time of the algorithm.
  // ----------------------------------------------------------------------

  ANNmaxPtsVisited = 0; // maximum number of pts visited

  // ----------------------------------------------------------------------
  // Self match?
  // In some applications, the nearest neighbor of a point is not
  // allowed to be the point itself. This occurs, for example, when
  // computing all nearest neighbors in a set.  By setting the
  // parameter ANN_ALLOW_SELF_MATCH to ANNfalse, the nearest neighbor
  // is the closest point whose distance from the query point is
  // strictly positive.
  // ----------------------------------------------------------------------

  ANN_ALLOW_SELF_MATCH = true;

type

  TANNcoord = single;
  TANNdist = single;
  TANNidx = integer;
  PANNidx = ^TANNidx;

  // a point
  TANNpoint = array of TANNcoord;
  // an array of points
  TANNpointArray = array of TANNpoint;
  // an array of distances
  TANNdistArray = array of TANNdist;
  // an array of point indices
  TANNidxArray = array of TANNidx;

  TANNsplitRule = (ANN_KD_STD, // the optimized kd-splitting rule
    ANN_KD_MIDPT, // midpoint split
    ANN_KD_FAIR, // fair split
    ANN_KD_SL_MIDPT, // sliding midpoint splitting method
    ANN_KD_SL_FAIR, // sliding fair split method
    ANN_KD_SUGGEST); // the authors' suggestion for best

  TPQKkey = TANNdist; // key field is distance
  TPQKinfo = integer; // info field is int
  TPPQKinfo = Pointer;

  // ----------------------------------------------------------------------
  // Constants
  // The NULL key value is used to initialize the priority queue, and
  // so it should be larger than any valid distance, so that it will
  // be replaced as legal distance values are inserted.  The NULL
  // info value must be a nonvalid array index, we use ANN_NULL_IDX,
  // which is guaranteed to be negative.
  // ----------------------------------------------------------------------

const
  PQ_NULL_KEY: TPQKkey = MaxDouble; // nonexistent key value
  PQ_NULL_INFO: TPQKinfo = -1; // nonexistent info value

  // ----------------------------------------------------------------------
  // ANNmin_k
  // An ANNmin_k structure is one which maintains the smallest
  // k values (of type PQKkey) and associated information (of type
  // PQKinfo).  The special info and key values PQ_NULL_INFO and
  // PQ_NULL_KEY means that thise entry is empty.
  //
  // It is currently implemented using an array with k items.
  // Items are stored in increasing sorted order, and insertions
  // are made through standard insertion sort.  (This is quite
  // inefficient, but current applications call for small values
  // of k and relatively few insertions.)
  //
  // Note that the list contains k+1 entries, but the last entry
  // is used as a simple placeholder and is otherwise ignored.
  // ----------------------------------------------------------------------

type

  Tmk_node = record
    key: TPQKkey; // key value
    info: TPQKinfo; // info field (user defined)
  end;

  TANNmin_k = class
  private
    FKeyNum: integer;
    FActiveKeyNum: integer; // number of keys currently active
    FKeys: array of Tmk_node; // the list itself

  public
    constructor Create(aMax: integer);

    // return minimum key
    function Min_key(): TPQKkey;

    // return maximum key
    function Max_key(): TPQKkey;

    // ith smallest key (i in [0..n-1])
    function Ith_smallest_key(i: integer): TPQKkey;

    // info for ith smallest (i in [0..n-1])
    function Ith_smallest_info(i: integer): TPQKinfo;

    // insert item (inlined for speed)
    procedure Insert(kv: TPQKkey; // key value
      inf: TPQKinfo); // item info
  end;

  Tpq_node = record
    key: TPQKkey; // key value
    info: TPPQKinfo; // info field (user defined)
  end;

  TANNpr_queue = class
  private
    FItemsNum: integer; // number of items in queue
    FNodes: array of Tpq_node; // the priority queue (array of nodes)

  public
    constructor Create(aMax: integer);
    // is queue empty?
    function IsEmpty(): Boolean;
    // make existing queue empty
    procedure Reset();

    procedure Insert(kv: TPQKkey; // key value
      inf: TPPQKinfo); // item info
    // extract minimum
    procedure Extr_min(
      out kv: TPQKkey; // key value
      out inf: TPPQKinfo); // item info
  end;

  TkSearchParams = record
    ANNkdDim: integer; // dimension of space
    ANNkdQ: TANNpoint; // query point
    ANNkdMaxErr: double; // max tolerable squared error
    ANNkdPts: TANNpointArray; // the points
    ANNkdPointMK: TANNmin_k; // set of k closest points
    ANNptsVisited: integer; // number of pts visited in search
  end;

  TPriSearchParams = record
    ANNprEps: double; // the error bound
    ANNprDim: integer; // dimension of space
    ANNprQ: TANNpoint; // query point
    ANNprMaxErr: double; // max tolerable squared error
    ANNprPts: TANNpointArray; // the points
    ANNprBoxPQ: TANNpr_queue; // priority queue for boxes
    ANNprPointMK: TANNmin_k; // set of k closest points
    ANNptsVisited: integer; // number of pts visited in search
  end;

  TkFRSearchParams = record
    ANNkdFRDim: integer; // dimension of space
    ANNkdFRQ: TANNpoint; // query point
    ANNkdFRSqRad: TANNdist; // squared radius search bound
    ANNkdFRMaxErr: double; // max tolerable squared error
    ANNkdFRPts: TANNpointArray; // the points
    ANNkdFRPointMK: TANNmin_k; // set of k closest points
    ANNkdFRPtsVisited: integer; // total points visited
    ANNkdFRPtsInRange: integer; // number of points in the range
    ANNptsVisited: integer;
  end;

  // stats on kd-tree
  //
  TANNkdStats = record
    dim: integer; // dimension of space
    n_pts: integer; // no. of points
    bkt_size: integer; // bucket size
    n_lf: integer; // no. of leaves (including trivial)
    n_tl: integer; // no. of trivial leaves (no points)
    n_spl: integer; // no. of splitting nodes
    n_shr: integer; // no. of shrinking nodes (for bd-trees)
    depth: integer; // depth of tree
    sum_ar: Single; // sum of leaf aspect ratios
    avg_ar: Single; // average leaf aspect ratio

    // reset stats
    procedure Reset(d: integer = 0; n: integer = 0; bs: integer = 0);
    // merge stats from child
    procedure Merge(const aStats: TANNkdStats);
  end;

  TANNorthRect = record
    lo: TANNpoint;
    hi: TANNpoint;

    // basic constructor
    class function Make(dim: integer; l: TANNcoord = 0.0; h: TANNcoord = 0.0)
      : TANNorthRect; overload; static;

    // (almost a) copy constructor
    class function Make(const aRect: TANNorthRect): TANNorthRect;
      overload; static;

    // construct from points
    class function Make(l: TANNpoint; h: TANNpoint): TANNorthRect;
      overload; static;

    // is point p inside rectangle?
    function Inside(dim: integer; const p: TANNpoint): Boolean;
  end;

  // generic kd-tree node (empty shell)
  TANNkd_node = class
  protected
    // tree search
    procedure ann_search(aDistToNeighbors: TANNdist;
      var aParams: TkSearchParams); virtual; abstract;

    // priority search
    procedure ann_pri_search(aDistToNeighbors: TANNdist;
      var aParams: TPriSearchParams); virtual; abstract;

    // fixed-radius search
    procedure ann_FR_search(aDistToNeighbors: TANNdist;
      var aParams: TkFRSearchParams); virtual; abstract;

    procedure getStats(aDim: integer; out aStatus: TANNkdStats;
      var aBnd_box: TANNorthRect); virtual; abstract;
  end;

  // leaf node for kd-tree
  //
  TANNkd_leaf = class(TANNkd_node)
  protected
    Fbkt: PANNidx;
    FLength: integer;
  public
    constructor Create(aBuckets: PANNidx; aLen: integer);

    // tree search
    procedure ann_search(aDistToNeighbors: TANNdist;
      var aParams: TkSearchParams); override;

    // priority search
    procedure ann_pri_search(aDistToNeighbors: TANNdist;
      var aParams: TPriSearchParams); override;

    // fixed-radius search
    procedure ann_FR_search(aDistToNeighbors: TANNdist;
      var aParams: TkFRSearchParams); override;

    procedure getStats(aDim: integer; out aStatus: TANNkdStats;
      var aBnd_box: TANNorthRect); override;
  end;

  // splitting node of a kd-tree
  //
  TANNkd_split = class(TANNkd_node)
  protected
    Fcut_dim: integer; // dim orthogonal to cutting plane
    Fcut_val: TANNcoord; // location of cutting plane
    Fcd_bnds: array [0 .. 1] of TANNcoord; // lower and upper bounds of
    // rectangle along cut_dim
    Fchild: array [0 .. 1] of TANNkd_node; // left and right children
  public
    constructor Create(aCuttingDim: integer; aCuttingValue: TANNcoord;
      aLowValue, aHighValue: TANNcoord; aLowChild: Pointer = nil;
      aHighChild: Pointer = nil);

    destructor Destroy; override;

    // tree search
    procedure ann_search(aDistToNeighbors: TANNdist;
      var aParams: TkSearchParams); override;

    // priority search
    procedure ann_pri_search(aDistToNeighbors: TANNdist;
      var aParams: TPriSearchParams); override;

    // fixed-radius search
    procedure ann_FR_search(aDistToNeighbors: TANNdist;
      var aParams: TkFRSearchParams); override;

    procedure getStats(aDim: integer; out aStatus: TANNkdStats;
      var aBnd_box: TANNorthRect); override;
  end;

  TANNkd_tree = class
  private
    class var KD_TRIVIAL: TANNkd_leaf;
    function GetPointsCount: integer;
  protected
    FDim: integer;
    FBucketSize: integer;
    FPoints: TANNpointArray;
    FPointsIndices: TANNidxArray;
    FRoot: TANNkd_node;
    FBox_lo: TANNpoint;
    FBox_hi: TANNpoint;
    procedure SkeletonTree(aDimension: integer; aBucketSize: integer;
      const aPointArray: TANNpointArray; const anIndices: TANNidxArray = nil);

  public
    // build skeleton tree
    constructor CreateSceletonTree(aDimension: integer = 0;
      aBucketSize: integer = 1); overload;

    // build from point array
    constructor CreateFromPointArray(const aPointArray: TANNpointArray;
      aDimension: integer = 0; aBucketSize: integer = 1;
      aSplittingMethod: TANNsplitRule = ANN_KD_SUGGEST); overload;

    destructor Destroy; override;

    // approx k near neighbor search
    procedure annkSearch(const aQueryPoint: TANNpoint;
      aNumberOfNeighbors: integer; var aNearestNeighbors: TANNidxArray;
      var aDistToNeighbors: TANNdistArray; eps: double = 0.0);

    // priority k near neighbor search
    procedure annkPriSearch(aQueryPoint: TANNpoint; aNumberOfNeighbors: integer;
      var aNearestNeighbors: TANNidxArray; var aDistToNeighbors: TANNdistArray;
      eps: double = 0.0);

    // approx fixed-radius kNN search
    function annkFRSearch(const aQueryPoint: TANNpoint; sqRadius: TANNdist;
      var aNearestNeighbors: TANNidxArray; var aDistToNeighbors: TANNdistArray;
      aNumberOfNeighbors: integer = 0; eps: double = 0.0): integer;

    property Dimension: integer read FDim;
    property PointsCount: integer read GetPointsCount;
    property Points: TANNpointArray read FPoints;
  end;

implementation

uses
  uMath;

type

  // ----------------------------------------------------------------------
  // kd-splitting function:
  // kd_splitter is a pointer to a splitting routine for preprocessing.
  // Different splitting procedures result in different strategies
  // for building the tree.
  // ----------------------------------------------------------------------

  TANNkd_splitter = procedure(const pa: TANNpointArray;
    // point array (unaltered)
    pidx: PANNidx; // point indices (permuted on return)
    n: integer; // number of indices
    const bnds: TANNorthRect; // bounding rectangle for cell
    dim: integer; // dimension of space
    out cut_dim: integer; // cutting dimension (returned)
    out cut_val: TANNcoord; // cutting value (returned)
    out n_lo: integer); // num of points on low side (returned)

  // Compute the aspect ratio (ratio of longest to shortest side)
  // of a rectangle.
function annAspectRatio(aDim: integer; const aBnd_box: TANNorthRect): double;
var
  length, min_length, max_length: TANNcoord;
  d: integer;
begin
  length := aBnd_box.hi[0] - aBnd_box.lo[0];
  min_length := length; // min side length
  max_length := length; // max side length
  for d := 1 to aDim - 1 do
  begin
    length := aBnd_box.hi[d] - aBnd_box.lo[d];
    if (length < min_length) then
        min_length := length;
    if (length > max_length) then
        max_length := length;
  end;
  Result := max_length / min_length;
end;

// These utilities compute the smallest rectangle and cube enclosing
// a set of points, respectively.
procedure annEnclRect(const pa: TANNpointArray; const pidx: TANNidxArray;
  dim: integer; out bnds: TANNorthRect);
var
  d, i: integer;
  lo_bnd, hi_bnd: TANNcoord;
begin
  bnds := TANNorthRect.Make(dim);
  for d := 0 to dim - 1 do
  begin
    lo_bnd := pa[pidx[0]][d]; // lower bound on dimension d
    hi_bnd := pa[pidx[0]][d]; // upper bound on dimension d
    for i := 0 to High(pidx) do
    begin
      if (pa[pidx[i]][d] < lo_bnd) then
          lo_bnd := pa[pidx[i]][d]
      else if (pa[pidx[i]][d] > hi_bnd) then
          hi_bnd := pa[pidx[i]][d];
    end;
    bnds.lo[d] := lo_bnd;
    bnds.hi[d] := hi_bnd;
  end;
end;

// ----------------------------------------------------------------------
// annSpread - find spread along given dimension
// annMinMax - find min and max coordinates along given dimension
// annMaxSpread - find dimension of max spread
// ----------------------------------------------------------------------

function annSpread( // compute point spread along dimension
  const pa: TANNpointArray; // point array
  pidx: PANNidx; // point indices;
  n: integer;
  d: integer): TANNcoord; // dimension to check
var
  min, max, c: TANNcoord;
  i: integer;
begin
  min := pa[pidx[0]][d]; // compute max and min coords
  max := min;
  for i := 1 to n - 1 do
  begin
    c := pa[pidx[i]][d];
    if (c < min) then
        min := c
    else if (c > max) then
        max := c;
  end;
  Result := max - min; // total spread is difference
end;

function annMaxSpread( // compute dimension of max spread
  const pa: TANNpointArray; // point array
  pidx: PANNidx; // point indices;
  n: integer;
  dim: integer): integer;
var
  d, max_dim: integer;
  max_spr, spr: TANNcoord;
begin
  max_dim := 0; // dimension of max spread
  max_spr := 0; // amount of max spread

  if (n = 0) then
      Exit(max_dim); // no points, who cares?

  for d := 0 to dim - 1 do // compute spread along each dim
  begin
    spr := annSpread(pa, pidx, n, d);
    if (spr > max_spr) then // bigger than current max
    begin
      max_spr := spr;
      max_dim := d;
    end;
  end;
  Result := max_dim;
end;

// ----------------------------------------------------------------------
// annMedianSplit - split point array about its median
// Splits a subarray of points pa[0..n] about an element of given
// rank (median: n_lo = n/2) with respect to dimension d.  It places
// the element of rank n_lo-1 correctly (because our splitting rule
// takes the mean of these two).  On exit, the array is permuted so
// that:
//
// pa[0..n_lo-2][d] <= pa[n_lo-1][d] <= pa[n_lo][d] <= pa[n_lo+1..n-1][d].
//
// The mean of pa[n_lo-1][d] and pa[n_lo][d] is returned as the
// splitting value.
//
// All indexing is done indirectly through the index array pidx.
//
// This function uses the well known selection algorithm due to
// C.A.R. Hoare.
// ----------------------------------------------------------------------

// swap two points in pa array
// #define PASWAP(a,b) { int ; ;  }

procedure annMedianSplit(const pa: TANNpointArray; // points to split
  pidx: PANNidx; // point indices
  n: integer;
  d: integer; // dimension along which to split
  out cv: TANNcoord; // cutting value
  n_lo: integer); // split into n_lo and n-n_lo
var
  l, r, i, k: integer;
  c: TANNcoord;

  procedure Swap(a, b: integer);
  var
    tmp: integer;
  begin
    tmp := pidx[a];
    pidx[a] := pidx[b];
    pidx[b] := tmp;
  end;

begin
  l := 0; // left end of current subarray
  r := n - 1; // right end of current subarray
  while (l < r) do
  begin
    i := (r + l) div 2; // select middle as pivot

    if (pa[pidx[i]][d] > pa[pidx[r]][d]) then // make sure last > pivot
        Swap(i, r);
    Swap(l, i); // move pivot to first position

    c := pa[pidx[l]][d]; // pivot value
    i := l;
    k := r;
    repeat // pivot about c
      repeat
          Inc(i);
      until pa[pidx[i]][d] >= c;
      repeat
          Dec(k);
      until pa[pidx[i]][d] <= c;
      if (i < k) then
          Swap(i, k)
      else
          break;
    until False;
    Swap(l, k); // pivot winds up in location k

    if (k > n_lo) then
        r := k - 1 // recurse on proper subarray
    else if (k < n_lo) then
        l := k + 1
    else
        break; // got the median exactly
  end;

  if (n_lo > 0) then // search for next smaller item
  begin
    c := pa[pidx[0]][d]; // candidate for max
    k := 0; // candidate's index
    for i := 1 to n_lo - 1 do
      if pa[pidx[i]][d] > c then
      begin
        c := pa[pidx[i]][d];
        k := i;
      end;
    Swap(n_lo - 1, k); // max among pa[0..n_lo-1] to pa[n_lo-1]
  end;
  // cut value is midpoint value
  cv := (pa[pidx[n_lo - 1]][d] + pa[pidx[n_lo]][d]) / 2.0;
end;

// ----------------------------------------------------------------------
// annPlaneSplit - split point array about a cutting plane
// Split the points in an array about a given plane along a
// given cutting dimension.  On exit, br1 and br2 are set so
// that:
//
// pa[ 0 ..br1-1] <  cv
// pa[br1..br2-1] == cv
// pa[br2.. n -1] >  cv
//
// All indexing is done indirectly through the index array pidx.
//
// ----------------------------------------------------------------------

procedure annPlaneSplit( // split points by a plane
  const pa: TANNpointArray; // points to split
  pidx: PANNidx; // point indices
  n: integer;
  d: integer; // dimension along which to split
  cv: TANNcoord; // cutting value
  out br1: integer; // first break (values < cv)
  out br2: integer); // second break (values == cv)
var
  l, r: integer;

  procedure Swap(a, b: integer);
  var
    tmp: integer;
  begin
    tmp := pidx[a];
    pidx[a] := pidx[b];
    pidx[b] := tmp;
  end;

begin
  l := 0;
  r := n - 1;

  repeat // partition pa[0..n-1] about cv
    while (l < n) and (pa[pidx[l]][d] < cv) do
        Inc(l);
    while (r >= 0) and (pa[pidx[r]][d] >= cv) do
        Dec(r);
    if (l > r) then
        break;
    Swap(l, r);
    Inc(l);
    Dec(r);
  until False;
  br1 := l; // now: pa[0..br1-1] < cv <= pa[br1..n-1]
  r := n - 1;
  repeat // partition pa[br1..n-1] about cv
    while (l < n) and (pa[pidx[l]][d] <= cv) do
        Inc(l);
    while (r >= br1) and (pa[pidx[r]][d] > cv) do
        Dec(r);
    if (l > r) then
        break;
    Swap(l, r);
    Inc(l);
    Dec(r);
  until False;
  br2 := l; // now: pa[br1..br2-1] == cv < pa[br2..n-1]
end;

procedure annMinMax( // compute min and max coordinates along dim
  const pa: TANNpointArray; // points to split
  pidx: PANNidx; // point indices
  n: integer;
  d: integer; // dimension along which to split
  out min: TANNcoord; // minimum value (returned)
  out max: TANNcoord); // maximum value (returned)
var
  i: integer;
  c: TANNcoord;
begin
  min := pa[pidx[0]][d]; // compute max and min coords
  max := min;
  for i := 1 to n - 1 do
  begin
    c := pa[pidx[i]][d];
    if (c < min) then
        min := c
    else if (c > max) then
        max := c;
  end;
end;

// ----------------------------------------------------------------------
// annSplitBalance - compute balance factor for a given plane split
// Balance factor is defined as the number of points lying
// below the splitting value minus n/2 (median).  Thus, a
// median split has balance 0, left of this is negative and
// right of this is positive.  (The points are unchanged.)
// ----------------------------------------------------------------------

function annSplitBalance( // determine balance factor of a split
  const pa: TANNpointArray; // points to split
  pidx: PANNidx; // point indices
  n: integer;
  d: integer; // dimension along which to split
  cv: TANNcoord): integer; // cutting value
var
  i, n_lo: integer;
begin
  n_lo := 0;
  for i := 0 to n - 1 do // count number less than cv
    if (pa[pidx[i]][d] < cv) then
        Inc(n_lo);

  Result := n_lo - n div 2;
end;

// ----------------------------------------------------------------------
// annBoxDistance - utility routine which computes distance from point to
// box (Note: most distances to boxes are computed using incremental
// distance updates, not this function.)
// ----------------------------------------------------------------------

function annBoxDistance( // compute distance from point to box
  const q: TANNpoint; // the point
  const lo: TANNpoint; // low point of box
  const hi: TANNpoint; // high point of box
  dim: integer): TANNdist;
var
  dist, t: TANNdist;
  d: integer;
begin
  dist := 0.0; // sum of squared distances

  for d := 0 to dim - 1 do
  begin
    if (q[d] < lo[d]) then // q is left of box
    begin
      t := lo[d] - q[d];
      dist := dist + t * t;
    end
    else if (q[d] > hi[d]) then // q is right of box
    begin
      t := q[d] - hi[d];
      dist := dist + t * t;
    end
  end;

  Result := dist;
end;

// ----------------------------------------------------------------------
// kd_split - Bentley's standard splitting routine for kd-trees
// Find the dimension of the greatest spread, and split
// just before the median point along this dimension.
// ----------------------------------------------------------------------

procedure kd_split(const pa: TANNpointArray; // point array (permuted on return)
  pidx: PANNidx; // point indices
  n: integer;
  const bnds: TANNorthRect; // bounding rectangle for cell
  dim: integer; // dimension of space
  out cut_dim: integer; // cutting dimension (returned)
  out cut_val: TANNcoord; // cutting value (returned)
  out n_lo: integer); // num of points on low side (returned)
begin
  // find dimension of maximum spread
  cut_dim := annMaxSpread(pa, pidx, n, dim);
  n_lo := n div 2; // median rank
  // split about median
  annMedianSplit(pa, pidx, n, cut_dim, cut_val, n_lo);
end;

// ----------------------------------------------------------------------
// midpt_split - midpoint splitting rule for box-decomposition trees
//
// This is the simplest splitting rule that guarantees boxes
// of bounded aspect ratio.  It simply cuts the box with the
// longest side through its midpoint.  If there are ties, it
// selects the dimension with the maximum point spread.
//
// WARNING: This routine (while simple) doesn't seem to work
// well in practice in high dimensions, because it tends to
// generate a large number of trivial and/or unbalanced splits.
// Either kd_split(), sl_midpt_split(), or fair_split() are
// recommended, instead.
// ----------------------------------------------------------------------

procedure midpt_split(const pa: TANNpointArray;
  // point array (permuted on return)
  pidx: PANNidx; // point indices
  n: integer;
  const bnds: TANNorthRect; // bounding rectangle for cell
  dim: integer; // dimension of space
  out cut_dim: integer; // cutting dimension (returned)
  out cut_val: TANNcoord; // cutting value (returned)
  out n_lo: integer); // num of points on low side (returned)
var
  d, br1, br2: integer;
  max_length, len, max_spread, spr: TANNcoord;
begin
  max_length := bnds.hi[0] - bnds.lo[0];
  for d := 1 to dim - 1 do // find length of longest box side
  begin
    len := bnds.hi[d] - bnds.lo[d];
    if (len > max_length) then
        max_length := len;
  end;

  max_spread := -1; // find long side with most spread
  for d := 0 to dim - 1 do
  begin
    // is it among longest?
    if (bnds.hi[d] - bnds.lo[d]) >= (1 - ERR) * max_length then
    begin
      // compute its spread
      spr := annSpread(pa, pidx, n, d);
      if (spr > max_spread) then // is it max so far?
      begin
        max_spread := spr;
        cut_dim := d;
      end;
    end;
  end;
  // split along cut_dim at midpoint
  cut_val := (bnds.lo[cut_dim] + bnds.hi[cut_dim]) / 2;
  // permute points accordingly
  annPlaneSplit(pa, pidx, n, cut_dim, cut_val, br1, br2);
  // ------------------------------------------------------------------
  // On return:		pa[0..br1-1] < cut_val
  // pa[br1..br2-1] == cut_val
  // pa[br2..n-1] > cut_val
  //
  // We can set n_lo to any value in the range [br1..br2].
  // We choose split so that points are most evenly divided.
  // ------------------------------------------------------------------
  if (br1 > n div 2) then
      n_lo := br1
  else if (br2 < n div 2) then
      n_lo := br2
  else
      n_lo := n div 2;
end;

// ----------------------------------------------------------------------
// sl_midpt_split - sliding midpoint splitting rule
//
// This is a modification of midpt_split, which has the nonsensical
// name "sliding midpoint".  The idea is that we try to use the
// midpoint rule, by bisecting the longest side.  If there are
// ties, the dimension with the maximum spread is selected.  If,
// however, the midpoint split produces a trivial split (no points
// on one side of the splitting plane) then we slide the splitting
// (maintaining its orientation) until it produces a nontrivial
// split. For example, if the splitting plane is along the x-axis,
// and all the data points have x-coordinate less than the x-bisector,
// then the split is taken along the maximum x-coordinate of the
// data points.
//
// Intuitively, this rule cannot generate trivial splits, and
// hence avoids midpt_split's tendency to produce trees with
// a very large number of nodes.
//
// ----------------------------------------------------------------------

procedure sl_midpt_split(const pa: TANNpointArray;
  // point array (permuted on return)
  pidx: PANNidx; // point indices
  n: integer;
  const bnds: TANNorthRect; // bounding rectangle for cell
  dim: integer; // dimension of space
  out cut_dim: integer; // cutting dimension (returned)
  out cut_val: TANNcoord; // cutting value (returned)
  out n_lo: integer); // num of points on low side (returned)
var
  d, br1, br2: integer;
  max_length, len, max_spread, spr, ideal_cut_val, min, max: TANNcoord;
begin
  max_length := bnds.hi[0] - bnds.lo[0];
  for d := 1 to dim - 1 do // find length of longest box side
  begin
    len := bnds.hi[d] - bnds.lo[d];
    if (len > max_length) then
        max_length := len;
  end;

  max_spread := -1; // find long side with most spread
  for d := 0 to dim - 1 do
  begin
    // is it among longest?
    if ((bnds.hi[d] - bnds.lo[d]) >= (1 - ERR) * max_length) then
    begin
      // compute its spread
      spr := annSpread(pa, pidx, n, d);
      if (spr > max_spread) then // is it max so far?
      begin
        max_spread := spr;
        cut_dim := d;
      end;
    end;
  end;
  // ideal split at midpoint
  ideal_cut_val := (bnds.lo[cut_dim] + bnds.hi[cut_dim]) / 2;

  annMinMax(pa, pidx, n, cut_dim, min, max); // find min/max coordinates

  if (ideal_cut_val < min) then // slide to min or max as needed
      cut_val := min
  else if (ideal_cut_val > max) then
      cut_val := max
  else
      cut_val := ideal_cut_val;

  // permute points accordingly
  annPlaneSplit(pa, pidx, n, cut_dim, cut_val, br1, br2);
  // ------------------------------------------------------------------
  // On return:		pa[0..br1-1] < cut_val
  // pa[br1..br2-1] == cut_val
  // pa[br2..n-1] > cut_val
  //
  // We can set n_lo to any value in the range [br1..br2] to satisfy
  // the exit conditions of the procedure.
  //
  // if ideal_cut_val < min (implying br2 >= 1),
  // then we select n_lo = 1 (so there is one point on left) and
  // if ideal_cut_val > max (implying br1 <= n-1),
  // then we select n_lo = n-1 (so there is one point on right).
  // Otherwise, we select n_lo as close to n/2 as possible within
  // [br1..br2].
  // ------------------------------------------------------------------
  if (ideal_cut_val < min) then
      n_lo := 1
  else if (ideal_cut_val > max) then
      n_lo := n - 1
  else if (br1 > n div 2) then
      n_lo := br1
  else if (br2 < n div 2) then
      n_lo := br2
  else
      n_lo := n div 2;
end;

// ----------------------------------------------------------------------
// fair_split - fair-split splitting rule
//
// This is a compromise between the kd-tree splitting rule (which
// always splits data points at their median) and the midpoint
// splitting rule (which always splits a box through its center.
// The goal of this procedure is to achieve both nicely balanced
// splits, and boxes of bounded aspect ratio.
//
// A constant FS_ASPECT_RATIO is defined. Given a box, those sides
// which can be split so that the ratio of the longest to shortest
// side does not exceed ASPECT_RATIO are identified.  Among these
// sides, we select the one in which the points have the largest
// spread. We then split the points in a manner which most evenly
// distributes the points on either side of the splitting plane,
// subject to maintaining the bound on the ratio of long to short
// sides. To determine that the aspect ratio will be preserved,
// we determine the longest side (other than this side), and
// determine how narrowly we can cut this side, without causing the
// aspect ratio bound to be exceeded (small_piece).
//
// This procedure is more robust than either kd_split or midpt_split,
// but is more complicated as well.  When point distribution is
// extremely skewed, this degenerates to midpt_split (actually
// 1/3 point split), and when the points are most evenly distributed,
// this degenerates to kd-split.
// ----------------------------------------------------------------------

procedure fair_split(const pa: TANNpointArray;
  // point array (permuted on return)
  pidx: PANNidx; // point indices
  n: integer;
  const bnds: TANNorthRect; // bounding rectangle for cell
  dim: integer; // dimension of space
  out cut_dim: integer; // cutting dimension (returned)
  out cut_val: TANNcoord; // cutting value (returned)
  out n_lo: integer); // num of points on low side (returned)
var
  d, br1, br2: integer;
  max_length, len, max_spread, spr, small_piece,
    lo_cut, hi_cut: TANNcoord;
begin
  max_length := bnds.hi[0] - bnds.lo[0];
  cut_dim := 0;
  for d := 1 to dim - 1 do // find length of longest box side
  begin
    len := bnds.hi[d] - bnds.lo[d];
    if (len > max_length) then
    begin
      max_length := len;
      cut_dim := d;
    end;
  end;

  max_spread := 0; // find legal cut with max spread
  cut_dim := 0;
  for d := 0 to dim - 1 do
  begin
    len := bnds.hi[d] - bnds.lo[d];
    // is this side midpoint splitable
    // without violating aspect ratio?
    if max_length * 2.0 / len <= FS_ASPECT_RATIO then
    begin
      // compute spread along this dim
      spr := annSpread(pa, pidx, n, d);
      if (spr > max_spread) then // best spread so far
      begin
        max_spread := spr;
        cut_dim := d; // this is dimension to cut
      end;
    end;
  end;

  max_length := 0; // find longest side other than cut_dim
  for d := 0 to dim - 1 do
  begin
    len := bnds.hi[d] - bnds.lo[d];
    if (d <> cut_dim) and (len > max_length) then
        max_length := len;
  end;
  // consider most extreme splits
  small_piece := max_length / FS_ASPECT_RATIO;
  lo_cut := bnds.lo[cut_dim] + small_piece; // lowest legal cut
  hi_cut := bnds.hi[cut_dim] - small_piece; // highest legal cut

  // is median below lo_cut ?
  if (annSplitBalance(pa, pidx, n, cut_dim, lo_cut) >= 0) then
  begin
    cut_val := lo_cut; // cut at lo_cut
    annPlaneSplit(pa, pidx, n, cut_dim, cut_val, br1, br2);
    n_lo := br1;
  end
  // is median above hi_cut?
  else if (annSplitBalance(pa, pidx, n, cut_dim, hi_cut) <= 0) then
  begin
    cut_val := hi_cut; // cut at hi_cut
    annPlaneSplit(pa, pidx, n, cut_dim, cut_val, br1, br2);
    n_lo := br2;
  end
  else
  begin // median cut preserves asp ratio
    n_lo := n div 2; // split about median
    annMedianSplit(pa, pidx, n, cut_dim, cut_val, n_lo);
  end;
end;

// ----------------------------------------------------------------------
// sl_fair_split - sliding fair split splitting rule
//
// Sliding fair split is a splitting rule that combines the
// strengths of both fair split with sliding midpoint split.
// Fair split tends to produce balanced splits when the points
// are roughly uniformly distributed, but it can produce many
// trivial splits when points are highly clustered.  Sliding
// midpoint never produces trivial splits, and shrinks boxes
// nicely if points are highly clustered, but it may produce
// rather unbalanced splits when points are unclustered but not
// quite uniform.
//
// Sliding fair split is based on the theory that there are two
// types of splits that are "good": balanced splits that produce
// fat boxes, and unbalanced splits provided the cell with fewer
// points is fat.
//
// This splitting rule operates by first computing the longest
// side of the current bounding box.  Then it asks which sides
// could be split (at the midpoint) and still satisfy the aspect
// ratio bound with respect to this side.	Among these, it selects
// the side with the largest spread (as fair split would).	 It
// then considers the most extreme cuts that would be allowed by
// the aspect ratio bound.	 This is done by dividing the longest
// side of the box by the aspect ratio bound.	If the median cut
// lies between these extreme cuts, then we use the median cut.
// If not, then consider the extreme cut that is closer to the
// median.	 If all the points lie to one side of this cut, then
// we slide the cut until it hits the first point.	 This may
// violate the aspect ratio bound, but will never generate empty
// cells.	However the sibling of every such skinny cell is fat,
// and hence packing arguments still apply.
//
// ----------------------------------------------------------------------

procedure sl_fair_split(const pa: TANNpointArray;
  // point array (permuted on return)
  pidx: PANNidx; // point indices
  n: integer;
  const bnds: TANNorthRect; // bounding rectangle for cell
  dim: integer; // dimension of space
  out cut_dim: integer; // cutting dimension (returned)
  out cut_val: TANNcoord; // cutting value (returned)
  out n_lo: integer); // num of points on low side (returned)
var
  d, br1, br2: integer;
  max_length, len, max_spread, spr, min, max, small_piece,
    lo_cut, hi_cut: TANNcoord;
begin
  max_length := bnds.hi[0] - bnds.lo[0];
  cut_dim := 0;
  for d := 1 to dim - 1 do // find length of longest box side
  begin
    len := bnds.hi[d] - bnds.lo[d];
    if (len > max_length) then
    begin
      max_length := len;
      cut_dim := d;
    end;
  end;

  max_spread := 0; // find legal cut with max spread
  cut_dim := 0;
  for d := 0 to dim - 1 do
  begin
    len := bnds.hi[d] - bnds.lo[d];
    // is this side midpoint splitable
    // without violating aspect ratio?
    if max_length * 2.0 / len <= FS_ASPECT_RATIO then
    begin
      // compute spread along this dim
      spr := annSpread(pa, pidx, n, d);
      if (spr > max_spread) then // best spread so far
      begin
        max_spread := spr;
        cut_dim := d; // this is dimension to cut
      end;
    end;
  end;

  max_length := 0; // find longest side other than cut_dim
  for d := 0 to dim - 1 do
  begin
    len := bnds.hi[d] - bnds.lo[d];
    if (d <> cut_dim) and (len > max_length) then
        max_length := len;
  end;
  // consider most extreme splits
  small_piece := max_length / FS_ASPECT_RATIO;
  lo_cut := bnds.lo[cut_dim] + small_piece; // lowest legal cut
  hi_cut := bnds.hi[cut_dim] - small_piece; // highest legal cut
  // find min and max along cut_dim
  annMinMax(pa, pidx, n, cut_dim, min, max);
  // is median below lo_cut?
  if (annSplitBalance(pa, pidx, n, cut_dim, lo_cut) >= 0) then
  begin
    if (max > lo_cut) then // are any points above lo_cut?
    begin
      cut_val := lo_cut; // cut at lo_cut
      annPlaneSplit(pa, pidx, n, cut_dim, cut_val, br1, br2);
      n_lo := br1; // balance if there are ties
    end
    else
    begin // all points below lo_cut
      cut_val := max; // cut at max value
      annPlaneSplit(pa, pidx, n, cut_dim, cut_val, br1, br2);
      n_lo := n - 1;
    end;
  end
  // is median above hi_cut?
  else if (annSplitBalance(pa, pidx, n, cut_dim, hi_cut) <= 0) then
  begin
    if (min < hi_cut) then // are any points below hi_cut?
    begin
      cut_val := hi_cut; // cut at hi_cut
      annPlaneSplit(pa, pidx, n, cut_dim, cut_val, br1, br2);
      n_lo := br2; // balance if there are ties
    end
    else
    begin // all points above hi_cut
      cut_val := min; // cut at min value
      annPlaneSplit(pa, pidx, n, cut_dim, cut_val, br1, br2);
      n_lo := 1;
    end;
  end
  else
  begin // median cut is good enough
    n_lo := n div 2; // split about median
    annMedianSplit(pa, pidx, n, cut_dim, cut_val, n_lo);
  end;
end;

// ----------------------------------------------------------------------
// rkd_tree - recursive procedure to build a kd-tree
//
// Builds a kd-tree for points in pa as indexed through the
// array pidx[0..n-1] (typically a subarray of the array used in
// the top-level call).  This routine permutes the array pidx,
// but does not alter pa[].
//
// The construction is based on a standard algorithm for constructing
// the kd-tree (see Friedman, Bentley, and Finkel, ``An algorithm for
// finding best matches in logarithmic expected time,'' ACM Transactions
// on Mathematical Software, 3(3):209-226, 1977).  The procedure
// operates by a simple divide-and-conquer strategy, which determines
// an appropriate orthogonal cutting plane (see below), and splits
// the points.  When the number of points falls below the bucket size,
// we simply store the points in a leaf node's bucket.
//
// One of the arguments is a pointer to a splitting routine,
// whose prototype is:
//
// void split(
// ANNpointArray pa,  // complete point array
// ANNidxArray pidx,  // point array (permuted on return)
// ANNorthRect &bnds, // bounds of current cell
// int n,			   // number of points
// int dim,		   // dimension of space
// int &cut_dim,	   // cutting dimension
// ANNcoord &cut_val, // cutting value
// int &n_lo)		   // no. of points on low side of cut
//
// This procedure selects a cutting dimension and cutting value,
// partitions pa about these values, and returns the number of
// points on the low side of the cut.
// ----------------------------------------------------------------------

function rkd_tree( // recursive construction of kd-tree
  const pa: TANNpointArray; // point array
  pidx: PANNidx; // point indices to store in subtree
  n: integer;
  dim: integer; // dimension of space
  bsp: integer; // bucket space
  var bnd_box: TANNorthRect; // bounding box for current node
  splitter: TANNkd_splitter): TANNkd_node; // splitting routine
var
  cd: integer; // cutting dimension
  cv: TANNcoord; // cutting value
  n_lo: integer; // number on low side of cut
  lo, hi: TANNkd_node; // low and high children
  lv, hv: TANNcoord;
begin
  if n <= bsp then // n small, make a leaf node
  begin
    if n = 0 then // empty leaf node
        Exit(TANNkd_tree.KD_TRIVIAL) // return (canonical) empty leaf
    else // construct the node and return
        Exit(TANNkd_leaf.Create(pidx, n));
  end
  else
  begin
    // invoke splitting procedure
    splitter(pa, pidx, n, bnd_box, dim, cd, cv, n_lo);
    // save bounds for cutting dimension
    lv := bnd_box.lo[cd];
    hv := bnd_box.hi[cd];

    bnd_box.hi[cd] := cv; // modify bounds for left subtree
    lo := rkd_tree( // build left subtree
      pa, pidx, n_lo, // ...from pidx[0..n_lo-1]
      dim, bsp, bnd_box, splitter);
    // restore bounds
    bnd_box.hi[cd] := hv;

    // modify bounds for right subtree
    bnd_box.lo[cd] := cv;
    hi := rkd_tree( // build right subtree
      pa, pidx + n_lo, n - n_lo, // ...from pidx[n_lo..n-1]
      dim, bsp, bnd_box, splitter);
    bnd_box.lo[cd] := lv; // restore bounds

    // create the splitting node
    Result := TANNkd_split.Create(cd, cv, lv, hv, lo, hi);
  end;
end;

{$REGION 'TANNkdStats'}


procedure TANNkdStats.Merge(const aStats: TANNkdStats);
begin
  n_lf := n_lf + aStats.n_lf;
  n_tl := n_tl + aStats.n_tl;
  n_spl := n_spl + aStats.n_spl;
  n_shr := n_shr + aStats.n_shr;
  depth := TMath.max(depth, aStats.depth);
  sum_ar := sum_ar + aStats.sum_ar;
end;

procedure TANNkdStats.Reset(d, n, bs: integer);
begin
  dim := d;
  n_pts := n;
  bkt_size := bs;
  n_lf := 0;
  n_tl := 0;
  n_spl := 0;
  n_shr := 0;
  depth := 0;
  sum_ar := 0;
  avg_ar := 0;
end;

{$ENDREGION}

{$REGION 'TANNorthRect'}


function TANNorthRect.Inside(dim: integer; const p: TANNpoint): Boolean;
var
  i: integer;
begin
  for i := 0 to dim - 1 do
    if (p[i] < lo[i]) or (p[i] > hi[i]) then
        Exit(False);
  Result := true;
end;

class function TANNorthRect.Make(dim: integer; l, h: TANNcoord): TANNorthRect;
begin
  SetLength(Result.lo, dim);
  Result.lo[0] := l;
  SetLength(Result.hi, dim);
  Result.hi[0] := h;
end;

class function TANNorthRect.Make(const aRect: TANNorthRect)
  : TANNorthRect;
begin
  Result.lo := aRect.lo;
  Result.hi := aRect.hi;
end;

class function TANNorthRect.Make(l, h: TANNpoint): TANNorthRect;
begin
  Result.lo := l;
  Result.hi := h;
end;

{$ENDREGION}

{$REGION 'TANNkd_leaf'}


procedure TANNkd_leaf.ann_FR_search(aDistToNeighbors: TANNdist;
  var aParams: TkFRSearchParams);
var
  dist: TANNdist;
  pp, qq: TANNpoint;
  t: TANNcoord;
  d, i: integer;
begin
  for i := 0 to FLength - 1 do // check points in bucket
  begin
    pp := aParams.ANNkdFRPts[Fbkt[i]]; // first coord of next data point
    qq := aParams.ANNkdFRQ; // first coord of query point
    dist := 0;

    for d := 0 to aParams.ANNkdFRDim - 1 do
    begin
      t := qq[d] - pp[d]; // compute length and adv coordinate
      // exceeds dist to k-th smallest?
      dist := dist + t * t;
      if dist > aParams.ANNkdFRSqRad then
          break;
    end;

    if (d >= aParams.ANNkdFRDim) and // among the k best?
      (ANN_ALLOW_SELF_MATCH or (dist <> 0)) then // and no self-match problem
    begin
      // add it to the list
      aParams.ANNkdFRPointMK.Insert(dist, Fbkt[i]);
      Inc(aParams.ANNkdFRPtsInRange); // increment point count
    end;
  end;
  // increment number of points visited
  aParams.ANNkdFRPtsVisited := aParams.ANNkdFRPtsVisited + FLength;
end;

procedure TANNkd_leaf.ann_pri_search(aDistToNeighbors: TANNdist;
  var aParams: TPriSearchParams);
var
  dist, min_dist: TANNdist;
  pp, qq: TANNpoint;
  t: TANNcoord;
  d, i: integer;
begin
  min_dist := aParams.ANNprPointMK.Max_key(); // k-th smallest distance so far

  for i := 0 to FLength - 1 do // check points in bucket
  begin
    pp := aParams.ANNprPts[Fbkt[i]]; // first coord of next data point
    qq := aParams.ANNprQ; // first coord of query point
    dist := 0;

    for d := 0 to aParams.ANNprDim - 1 do
    begin
      t := qq[d] - pp[d]; // compute length and adv coordinate
      // exceeds dist to k-th smallest?
      dist := dist + t * t;
      if dist > min_dist then
          break;
    end;

    if (d >= aParams.ANNprDim) and // among the k best?
      (ANN_ALLOW_SELF_MATCH or (dist <> 0)) then // and no self-match problem
    begin
      // add it to the list
      aParams.ANNprPointMK.Insert(dist, Fbkt[i]);
      min_dist := aParams.ANNprPointMK.Max_key();
    end;
  end;
  // increment number of points visited
  aParams.ANNptsVisited := aParams.ANNptsVisited + FLength;
end;

procedure TANNkd_leaf.ann_search(aDistToNeighbors: TANNdist;
  var aParams: TkSearchParams);
var
  dist, min_dist: TANNdist;
  pp, qq: TANNpoint;
  t: TANNcoord;
  d, i: integer;
begin
  min_dist := aParams.ANNkdPointMK.Max_key(); // k-th smallest distance so far

  for i := 0 to FLength - 1 do // check points in bucket
  begin
    pp := aParams.ANNkdPts[Fbkt[i]]; // first coord of next data point
    qq := aParams.ANNkdQ; // first coord of query point
    dist := 0;

    for d := 0 to aParams.ANNkdDim - 1 do
    begin
      t := qq[d] - pp[d]; // compute length and adv coordinate
      // exceeds dist to k-th smallest?
      dist := dist + t * t;
      if dist > min_dist then
          break;
    end;

    if (d >= aParams.ANNkdDim) and // among the k best?
      (ANN_ALLOW_SELF_MATCH or (dist <> 0)) then // and no self-match problem
    begin
      // add it to the list
      aParams.ANNkdPointMK.Insert(dist, Fbkt[i]);
      min_dist := aParams.ANNkdPointMK.Max_key();
    end;
  end;
  // increment number of points visited
  aParams.ANNptsVisited := aParams.ANNptsVisited + FLength;
end;

constructor TANNkd_leaf.Create(aBuckets: PANNidx; aLen: integer);
begin
  Fbkt := aBuckets;
  FLength := aLen;
end;

procedure TANNkd_leaf.getStats(aDim: integer; out aStatus: TANNkdStats;
  var aBnd_box: TANNorthRect);
var
  ar: double;
begin
  aStatus.Reset();
  aStatus.n_lf := 1;
  if (Self = TANNkd_tree.KD_TRIVIAL) then
      aStatus.n_tl := 1;
  ar := annAspectRatio(aDim, aBnd_box);
  // incr sum (ignore outliers)
  if ar < ANN_AR_TOOBIG then
      aStatus.sum_ar := aStatus.sum_ar + ar
  else
      aStatus.sum_ar := aStatus.sum_ar + ANN_AR_TOOBIG;
end;

{$ENDREGION}

{$REGION 'TANNkd_tree'}


function TANNkd_tree.annkFRSearch(const aQueryPoint: TANNpoint;
  sqRadius: TANNdist; var aNearestNeighbors: TANNidxArray;
  var aDistToNeighbors: TANNdistArray; aNumberOfNeighbors: integer;
  eps: double): integer;
var
  params: TkFRSearchParams;
  i: integer;
begin
  params.ANNkdFRDim := FDim; // copy arguments to static equivs
  params.ANNkdFRQ := aQueryPoint;
  params.ANNkdFRSqRad := sqRadius;
  params.ANNkdFRPts := FPoints;
  params.ANNkdFRPtsVisited := 0; // initialize count of points visited
  params.ANNkdFRPtsInRange := 0; // ...and points in the range

  params.ANNkdFRMaxErr := sqr(1.0 + eps);

  // create set for closest k points
  params.ANNkdFRPointMK := TANNmin_k.Create(aNumberOfNeighbors);
  // search starting at the root
  FRoot.ann_FR_search(annBoxDistance(aQueryPoint, FBox_lo, FBox_hi,
    FDim), params);

  for i := 0 to aNumberOfNeighbors - 1 do // extract the k-th closest points
  begin
    if length(aDistToNeighbors) <> 0 then
        aDistToNeighbors[i] := params.ANNkdFRPointMK.Ith_smallest_key(i);
    if length(aNearestNeighbors) <> 0 then
        aNearestNeighbors[i] := params.ANNkdFRPointMK.Ith_smallest_info(i);
  end;

  params.ANNkdFRPointMK.Destroy; // deallocate closest point set
  Result := params.ANNkdFRPtsInRange; // return final point count
end;

procedure TANNkd_tree.annkPriSearch(aQueryPoint: TANNpoint;
  aNumberOfNeighbors: integer; var aNearestNeighbors: TANNidxArray;
  var aDistToNeighbors: TANNdistArray; eps: double);
var
  params: TPriSearchParams;
  box_dist: TANNdist;
  np: TANNkd_node;
  i: integer;
begin
  // max tolerable squared error
  params.ANNprMaxErr := sqr(1.0 + eps);
  params.ANNprDim := FDim; // copy arguments to static equivs
  params.ANNprQ := aQueryPoint;
  params.ANNprPts := FPoints;
  params.ANNptsVisited := 0; // initialize count of points visited

  params.ANNprPointMK := TANNmin_k.Create(aNumberOfNeighbors);
  // create set for closest k points

  // distance to root box
  box_dist := annBoxDistance(aQueryPoint, FBox_lo, FBox_hi, FDim);

  params.ANNprBoxPQ := TANNpr_queue.Create(length(FPointsIndices));
  // create priority queue for boxes
  params.ANNprBoxPQ.Insert(box_dist, FRoot); // insert root in priority queue

  while (not params.ANNprBoxPQ.IsEmpty()) and
    (not((ANNmaxPtsVisited <> 0) and (params.ANNptsVisited > ANNmaxPtsVisited))) do
  begin
    // extract closest box from queue
    params.ANNprBoxPQ.Extr_min(box_dist, TPPQKinfo(np));

    if (box_dist * params.ANNprMaxErr >= params.ANNprPointMK.Max_key()) then
        break;

    np.ann_pri_search(box_dist, params); // search this subtree.
  end;

  for i := 0 to aNumberOfNeighbors - 1 do // extract the k-th closest points
  begin
    aDistToNeighbors[i] := params.ANNprPointMK.Ith_smallest_key(i);
    aNearestNeighbors[i] := params.ANNprPointMK.Ith_smallest_info(i);
  end;

  params.ANNprPointMK.Destroy; // deallocate closest point set
  params.ANNprBoxPQ.Destroy; // deallocate priority queue
end;

procedure TANNkd_tree.annkSearch(const aQueryPoint: TANNpoint;
  aNumberOfNeighbors: integer; var aNearestNeighbors: TANNidxArray;
  var aDistToNeighbors: TANNdistArray; eps: double);
var
  params: TkSearchParams;
  i: integer;
begin
  params.ANNkdDim := FDim; // copy arguments to static equivs
  params.ANNkdQ := aQueryPoint;
  params.ANNkdPts := FPoints;
  params.ANNptsVisited := 0; // initialize count of points visited

  Assert(aNumberOfNeighbors <= length(FPointsIndices),
    'Requesting more near neighbors than data points');

  params.ANNkdMaxErr := sqr(1.0 + eps);
  params.ANNkdPointMK := TANNmin_k.Create(aNumberOfNeighbors);
  // create set for closest k points
  // search starting at the root
  FRoot.ann_search(annBoxDistance(aQueryPoint, FBox_lo, FBox_hi, FDim), params);

  for i := 0 to aNumberOfNeighbors - 1 do // extract the k-th closest points
  begin
    aDistToNeighbors[i] := params.ANNkdPointMK.Ith_smallest_key(i);
    aNearestNeighbors[i] := params.ANNkdPointMK.Ith_smallest_info(i);
  end;

  params.ANNkdPointMK.Destroy; // deallocate closest point set
end;

constructor TANNkd_tree.CreateFromPointArray(const aPointArray: TANNpointArray;
  aDimension, aBucketSize: integer; aSplittingMethod: TANNsplitRule);
var
  bnd_box: TANNorthRect;
begin
  // set up the basic stuff
  SkeletonTree(aDimension, aBucketSize, aPointArray);
  if length(FPoints) = 0 then
      Exit;

  // bounding box for points
  bnd_box := TANNorthRect.Make(aDimension);
  // construct bounding rectangle
  annEnclRect(aPointArray, FPointsIndices, aDimension, bnd_box);
  // copy to tree structure
  FBox_lo := bnd_box.lo;
  FBox_hi := bnd_box.hi;

  // build by rule
  case aSplittingMethod of
    ANN_KD_STD: // standard kd-splitting rule
      FRoot := rkd_tree(aPointArray, @FPointsIndices[0], length(FPointsIndices),
        aDimension, aBucketSize,
        bnd_box, kd_split);
    ANN_KD_MIDPT: // midpoint split
      FRoot := rkd_tree(aPointArray, @FPointsIndices[0], length(FPointsIndices),
        aDimension, aBucketSize,
        bnd_box, midpt_split);
    ANN_KD_FAIR: // fair split
      FRoot := rkd_tree(aPointArray, @FPointsIndices[0], length(FPointsIndices),
        aDimension, aBucketSize,
        bnd_box, fair_split);
    ANN_KD_SUGGEST, // best (in our opinion)
    ANN_KD_SL_MIDPT: // sliding midpoint split
      FRoot := rkd_tree(aPointArray, @FPointsIndices[0], length(FPointsIndices),
        aDimension, aBucketSize,
        bnd_box, sl_midpt_split);
    ANN_KD_SL_FAIR: // sliding fair split
      FRoot := rkd_tree(aPointArray, @FPointsIndices[0], length(FPointsIndices),
        aDimension, aBucketSize,
        bnd_box, sl_fair_split);
  end;
end;

constructor TANNkd_tree.CreateSceletonTree(aDimension, aBucketSize: integer);
begin
  SkeletonTree(aDimension, aBucketSize, nil, nil);
end;

destructor TANNkd_tree.Destroy;
begin
  FRoot.Free;
  inherited;
end;

function TANNkd_tree.GetPointsCount: integer;
begin
  Result := length(FPoints);
end;

procedure TANNkd_tree.SkeletonTree(aDimension, aBucketSize: integer;
  const aPointArray: TANNpointArray; const anIndices: TANNidxArray);
var
  i: integer;
begin
  if KD_TRIVIAL = nil then // no trivial leaf node yet?
      KD_TRIVIAL := TANNkd_leaf.Create(nil, 0); // allocate it

  FDim := aDimension;
  FBucketSize := aBucketSize;
  FPoints := aPointArray;

  // no associated tree yet
  FRoot := nil;
  // point indices provided?
  if length(anIndices) = 0 then
  begin
    // no, allocate space for point indices
    SetLength(FPointsIndices, length(FPoints));
    for i := 0 to High(FPoints) do
        FPointsIndices[i] := i;
  end
  else
      FPointsIndices := anIndices;

  // bounding box is nonexistent
  SetLength(FBox_lo, 0);
  SetLength(FBox_hi, 0);
end;

{$ENDREGION}

{$REGION 'TANNkd_split' }


procedure TANNkd_split.ann_FR_search(aDistToNeighbors: TANNdist;
  var aParams: TkFRSearchParams);
var
  cut_diff, box_diff: TANNcoord;
begin
  // check dist calc term condition
  if (ANNmaxPtsVisited <> 0) and (aParams.ANNkdFRPtsVisited > ANNmaxPtsVisited)
  then Exit;

  // distance to cutting plane
  cut_diff := aParams.ANNkdFRQ[Fcut_dim] - Fcut_val;

  if (cut_diff < 0) then // left of cutting plane
  begin
    Fchild[0].ann_FR_search(aDistToNeighbors, aParams);
    // visit closer child first

    box_diff := Fcd_bnds[0] - aParams.ANNkdFRQ[Fcut_dim];
    if (box_diff < 0) then // within bounds - ignore
        box_diff := 0;
    // distance to further box
    aDistToNeighbors := aDistToNeighbors +
      (cut_diff * cut_diff - box_diff * box_diff);

    // visit further child if in range
    if (aDistToNeighbors * aParams.ANNkdFRMaxErr <= aParams.ANNkdFRSqRad) then
        Fchild[1].ann_FR_search(aDistToNeighbors, aParams);

  end
  else begin // right of cutting plane
    Fchild[1].ann_FR_search(aDistToNeighbors, aParams);
    // visit closer child first

    box_diff := aParams.ANNkdFRQ[Fcut_dim] - Fcd_bnds[1];
    if (box_diff < 0) then // within bounds - ignore
        box_diff := 0;
    // distance to further box
    aDistToNeighbors := aDistToNeighbors +
      (cut_diff * cut_diff - box_diff * box_diff);

    // visit further child if close enough
    if aDistToNeighbors * aParams.ANNkdFRMaxErr <= aParams.ANNkdFRSqRad then
        Fchild[0].ann_FR_search(aDistToNeighbors, aParams);

  end;
end;

procedure TANNkd_split.ann_pri_search(aDistToNeighbors: TANNdist;
  var aParams: TPriSearchParams);
var
  new_dist: TANNdist;
  cut_diff, box_diff: TANNcoord;
begin
  // distance to cutting plane
  cut_diff := aParams.ANNprQ[Fcut_dim] - Fcut_val;

  if (cut_diff < 0) then // left of cutting plane
  begin
    box_diff := Fcd_bnds[0] - aParams.ANNprQ[Fcut_dim];
    if (box_diff < 0) then // within bounds - ignore
        box_diff := 0;
    // distance to further box
    new_dist := aDistToNeighbors + (cut_diff * cut_diff - box_diff * box_diff);

    if (Fchild[1] <> TANNkd_tree.KD_TRIVIAL) then // enqueue if not trivial
        aParams.ANNprBoxPQ.Insert(new_dist, Fchild[1]);
    // continue with closer child
    Fchild[0].ann_pri_search(aDistToNeighbors, aParams);
  end
  else
  begin // right of cutting plane
    box_diff := aParams.ANNprQ[Fcut_dim] - Fcd_bnds[1];
    if (box_diff < 0) then // within bounds - ignore
        box_diff := 0;
    // distance to further box
    new_dist := aDistToNeighbors + (cut_diff * cut_diff - box_diff * box_diff);

    if (Fchild[0] <> TANNkd_tree.KD_TRIVIAL) then // enqueue if not trivial
        aParams.ANNprBoxPQ.Insert(new_dist, Fchild[0]);
    // continue with closer child
    Fchild[1].ann_pri_search(aDistToNeighbors, aParams);
  end;
end;

procedure TANNkd_split.ann_search(aDistToNeighbors: TANNdist;
  var aParams: TkSearchParams);
var
  cut_diff, box_diff: TANNcoord;
begin
  if (ANNmaxPtsVisited <> 0) and (aParams.ANNptsVisited > ANNmaxPtsVisited) then
      Exit;

  // distance to cutting plane
  cut_diff := aParams.ANNkdQ[Fcut_dim] - Fcut_val;

  if (cut_diff < 0) then // left of cutting plane
  begin
    Fchild[0].ann_search(aDistToNeighbors, aParams); // visit closer child first

    box_diff := Fcd_bnds[0] - aParams.ANNkdQ[Fcut_dim];
    if (box_diff < 0) then // within bounds - ignore
        box_diff := 0;
    // distance to further box
    aDistToNeighbors := aDistToNeighbors +
      (cut_diff * cut_diff - box_diff * box_diff);

    // visit further child if close enough
    if (aDistToNeighbors * aParams.ANNkdMaxErr < aParams.ANNkdPointMK.Max_key())
    then
        Fchild[1].ann_search(aDistToNeighbors, aParams);

  end
  else
  begin // right of cutting plane
    Fchild[1].ann_search(aDistToNeighbors, aParams); // visit closer child first

    box_diff := aParams.ANNkdQ[Fcut_dim] - Fcd_bnds[1];
    if (box_diff < 0) then // within bounds - ignore
        box_diff := 0;
    // distance to further box
    aDistToNeighbors := aDistToNeighbors +
      (cut_diff * cut_diff - box_diff * box_diff);

    // visit further child if close enough
    if (aDistToNeighbors * aParams.ANNkdMaxErr < aParams.ANNkdPointMK.Max_key())
    then
        Fchild[0].ann_search(aDistToNeighbors, aParams);
  end;
end;

constructor TANNkd_split.Create(aCuttingDim: integer;
  aCuttingValue, aLowValue, aHighValue: TANNcoord;
  aLowChild, aHighChild: Pointer);
begin
  Fcut_dim := aCuttingDim; // cutting dimension
  Fcut_val := aCuttingValue; // cutting value
  Fcd_bnds[0] := aLowValue; // lower bound for rectangle
  Fcd_bnds[1] := aHighValue; // upper bound for rectangle
  Fchild[0] := aLowChild; // left child
  Fchild[1] := aHighChild; // right child
end;

destructor TANNkd_split.Destroy;
begin
  if (Fchild[0] <> nil) and (Fchild[0] <> TANNkd_tree.KD_TRIVIAL) then
      Fchild[0].Destroy;
  if (Fchild[1] <> nil) and (Fchild[1] <> TANNkd_tree.KD_TRIVIAL) then
      Fchild[1].Destroy;
end;

procedure TANNkd_split.getStats(aDim: integer; out aStatus: TANNkdStats;
  var aBnd_box: TANNorthRect);
var
  ch_stats: TANNkdStats; // stats for children
  hv, lv: TANNcoord;
begin
  // get stats for low child
  // save box bounds
  hv := aBnd_box.hi[Fcut_dim];
  // upper bound for low child
  aBnd_box.hi[Fcut_dim] := Fcut_val;
  ch_stats.Reset();
  Fchild[0].getStats(aDim, ch_stats, aBnd_box);
  aStatus.Merge(ch_stats);
  // restore bound
  aBnd_box.hi[Fcut_dim] := hv;

  // get stats for high child
  // save box bounds
  lv := aBnd_box.lo[Fcut_dim];
  // lower bound for high child
  aBnd_box.lo[Fcut_dim] := Fcut_val;
  ch_stats.Reset();
  Fchild[1].getStats(aDim, ch_stats, aBnd_box);
  aStatus.Merge(ch_stats);
  // restore bound
  aBnd_box.lo[Fcut_dim] := lv;

  // increment depth
  Inc(aStatus.depth);
  Inc(aStatus.n_spl);
end;

{$ENDREGION}

{$REGION 'TANNmin_k'}


function TANNmin_k.Min_key: TPQKkey;
begin
  if FKeyNum > 0 then
      Result := FKeys[0].key
  else
      Result := PQ_NULL_KEY;
end;

constructor TANNmin_k.Create(aMax: integer);
begin
  FKeyNum := 0; // initially no items
  FActiveKeyNum := aMax; // maximum number of items
  SetLength(FKeys, aMax + 1); // sorted array of keys
end;

procedure TANNmin_k.Insert(kv: TPQKkey; inf: TPQKinfo);
var
  i: integer;
begin
  // slide larger values up
  i := FKeyNum;
  while i > 0 do
  begin
    if (FKeys[i - 1].key > kv) then
      FKeys[i] := FKeys[i - 1]
    else
      break;
    Dec(i);
  end;
  FKeys[i].key := kv; // store element here
  FKeys[i].info := inf;
  if (FKeyNum < FActiveKeyNum) then
      Inc(FKeyNum); // increment number of items
end;

function TANNmin_k.Ith_smallest_info(i: integer): TPQKinfo;
begin
  if i < FKeyNum then
      Result := FKeys[i].info
  else
      Result := PQ_NULL_INFO;
end;

function TANNmin_k.Ith_smallest_key(i: integer): TPQKkey;
begin
  if i < FKeyNum then
      Result := FKeys[i].key
  else
      Result := PQ_NULL_KEY;
end;

function TANNmin_k.Max_key: TPQKkey;
begin
  if FKeyNum = FActiveKeyNum then
      Result := FKeys[FActiveKeyNum - 1].key
  else
      Result := PQ_NULL_KEY;
end;

{$ENDREGION}

{$REGION 'TANNpr_queue'}


constructor TANNpr_queue.Create(aMax: integer);
begin
  FItemsNum := 0; // initially empty
  SetLength(FNodes, aMax + 1); // queue is array [1..max] of nodes
end;

procedure TANNpr_queue.Extr_min(out kv: TPQKkey; out inf: TPPQKinfo);
var
  kn: TPQKkey;
  p, r: integer;
begin
  kv := FNodes[1].key; // key of min item
  inf := FNodes[1].info; // information of min item
  kn := FNodes[FItemsNum].key; // last item in queue
  Dec(FItemsNum);
  p := 1; // p points to item out of position
  r := p shl 1; // left child of p
  while (r <= FItemsNum) do // while r is still within the heap
  begin
    // set r to smaller child of p
    if (r < FItemsNum) and (FNodes[r].key > FNodes[r + 1].key) then
        Inc(r);
    if (kn <= FNodes[r].key) then // in proper order
        break;
    FNodes[p] := FNodes[r]; // else swap with child
    p := r; // advance pointers
    r := p shl 1;
  end;
  FNodes[p] := FNodes[FItemsNum + 1]; // insert last item in proper place
end;

procedure TANNpr_queue.Insert(kv: TPQKkey; inf: TPPQKinfo);
var
  r, p: integer;
begin
  Inc(FItemsNum);
  r := length(FNodes);
  Assert(FItemsNum <= r);
  while r > 1 do
  begin
    p := r div 2;
    if (FNodes[p].key <= kv) then // in proper order
        break;
    FNodes[r] := FNodes[p];
    r := p;
  end;
  // insert new item at final location
  FNodes[r].key := kv;
  FNodes[r].info := inf;
end;

function TANNpr_queue.IsEmpty: Boolean;
begin
  Result := FItemsNum = 0;
end;

procedure TANNpr_queue.Reset;
begin
  FItemsNum := 0;
end;

{$ENDREGION}

initialization

finalization

TANNkd_tree.KD_TRIVIAL.Free;

end.
