unit uVMath;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  uMath;


Type

  TVecTemp = ( vt0, vt1, vtN1, vtX, vtY, vtZ, vtW, vtNX, vtNY, vtNZ, vtNW,
    vtXY, vtXZ, vtYZ, vtRandom, vtRndBox, vtRndSph );

  Vec2i = array[0..1] of Int;
  Vec3i = array[0..2] of Int;
  Vec4i = array[0..3] of Int;

  PVec2 = ^Vec2;
  Vec2 = array[0..1] of Float;

  PVec3 = ^Vec3;
  Vec3 = array[0..2] of Float;

  PVec4 = ^Vec4;
  Vec4 = array[0..3] of Float;

  Mat2 = array[0..1] of Vec2;
  Mat3 = array[0..2] of Vec3;

  PMat4 = ^Mat4;
  Mat4 = array[0..3] of Vec4;


  PVector = ^TVector;
  //
  // TVector
  //
  TVector = record
  private
    F: Vec4;

    function GetVec3: Vec3;
    function GetVec2: Vec2;
    function GetF( i: Int ): Float;
    procedure SetF( i: Int; v: Float );
    function GetVec4: Vec4;
    procedure SetVec2(const Value: Vec2);
    procedure SetVec3(const Value: Vec3);
    procedure SetVec4(const Value: Vec4);

  public

    // TVector = ( aX, aY, aZ, aW )
    class function Make( aX,aY: Float; aZ: Float = 0;
      aW: Float = 0 ): TVector; overload; static;
    procedure SetVector( aX,aY: Float; aZ: Float = 0; aW: Float = 0 );
    // TVector = ( TVecTemp )
    class function MakeTmp( aVecType: TVecTemp ): TVector; static;
    procedure SetVectorTmp( aVecType: TVecTemp );

    // Z = 0, W = 0
    function ToVec2: TVector;
    procedure MakeVec2;
    // W = 0
    function Affine: TVector;
    procedure MakeAffine;
    // W = 1
    function Point: TVector;
    procedure MakePoint;
    // TVector = ( A, B, C, D )
    function Squeeze( aA: TVecTemp; aB: TVecTemp = vt0; aC: TVecTemp = vt0;
      aD: TVecTemp = vt0 ): TVector;

    // abs( TVector[N] )
    function Abs: TVector;
    procedure SetAbs;
    // negate
    function Negate: TVector;
    procedure SetNegate;
    // normalize
    function Normalize: TVector;
    procedure SetNormalize;

    // frac( TVector[N] )
    function Fract: TVector;
    procedure SetFract;
    // floor( TVector[N] )
    function Floor: TVector;
    procedure SetFloor;

    // interpolation
    function Lerp( aVector: TVector; aRatio: Float ): TVector;
    procedure SetLerp( aVector: TVector; aRatio: Float );
    // smooth interpolation
    function LerpSmooth( aVector: TVector; aRatio: Float ): TVector;
    procedure SetLerpSmooth( aVector: TVector; aRatio: Float );

    // self * F1 + aVector * aF2
    function Combine( aVector: TVector; aF1,aF2: Float ): TVector;
    procedure SetCombine( aVector: TVector; aF1,aF2: Float );
    // self * F1 + aVector2 * aF2 + aVector3 * aF3
    function CombineEx( aV2,aV3: TVector; aF1,aF2,aF3: Float ): TVector;
    procedure SetCombineEx( aV2,aV3: TVector; aF1,aF2,aF3: Float );
    // self * aVector
    function Cross( aVector: TVector ): TVector;                         inline;
    procedure SetCross( aVector: TVector );                              inline;

    // perpendicular
    function Perpendicular( aNVector: TVector ): TVector;
    procedure SetPerpendicular( aNVector: TVector );
    // reflection
    function Reflect( aNVector: TVector ): TVector;
    procedure SetReflect( aNVector: TVector );
    // rotation
    function Rotate( aAxis: TVector; aAngle: Float ): TVector;
    procedure SetRotate( aAxis: TVector; aAngle: Float );
    procedure RotateAround( aCenter, anUp: TVector; aPitchDelta, aTurnDelta: Float);
    // scale
    function Scale( aFactor: Float ): TVector;
    procedure SetScale( aFactor: Float );

    function GetAddr: PVec4;

    function Length: Float;
    function LengthSqr: Float;
    function Distance( aPoint: TVector ): Float;
    function DistanceSqr( aPoint: TVector ): Float;
    function Dot( aVector: TVector ): Float;                             inline;
    function PointProject( aPos,aDir: TVector ): Float;
    // Given an input vector, creates an orthonormal basis which can be used as a
    // tangent space
    procedure CreateOrthonormalBasis(out aNormal, aTangent, aBinormal: TVector);
    class function Null: TVector; static;
    function IsNull: Boolean;

    procedure SwapWith( var aVector: TVector );

    property Value[i: Int]: Float read GetF write SetF; default;
    property X: Float read F[0] write F[0];
    property Y: Float read F[1] write F[1];
    property Z: Float read F[2] write F[2];
    property W: Float read F[3] write F[3];
    property Vec4: Vec4 read GetVec4 write SetVec4;
    property Vec3: Vec3 read GetVec3 write SetVec3;
    property Vec2: Vec2 read GetVec2 write SetVec2;

    class operator Negative( v: TVector ): TVector;
    class operator Add( v1,v2: TVector ): TVector;
    class operator Subtract( v1,v2: TVector ): TVector;
    class operator Multiply( v: TVector; f: Float ): TVector;
    class operator Multiply( v1,v2: TVector ): TVector;
    class operator Multiply( v: TVector; const m: Mat4 ): TVector;
    class operator Divide( v: TVector; f: Float ): TVector;
    class operator Divide( v1,v2: TVector ): TVector;
    class operator Equal( v1,v2: TVector ): Bool;
    class operator NotEqual( v1,v2: TVector ): Bool;
    class operator GreaterThan( v1,v2: TVector ): Bool;
    class operator GreaterThanOrEqual( v1,v2: TVector ): Bool;
    class operator LessThan( v1,v2: TVector ): Bool;
    class operator LessThanOrEqual( v1,v2: TVector ): Bool;

    class operator Implicit( t: TVecTemp ): TVector;
    class operator Implicit( f: Float ): TVector;
    class operator Implicit( const v: Vec2 ): TVector;
    class operator Implicit( const v: Vec3 ): TVector;
    class operator Implicit( const v: Vec4 ): TVector;
    class operator Implicit( pp: PVec4 ): TVector;

    class operator Explicit( t: TVecTemp ): TVector;
    class operator Explicit( f: Float ): TVector;
    class operator Explicit( const v: Vec2 ): TVector;
    class operator Explicit( const v: Vec3 ): TVector;
    class operator Explicit( const v: Vec4 ): TVector;
    class operator Explicit( pp: PVec4 ): TVector;

    class function MinVector( v1,v2: TVector ): TVector; static;
    class function MaxVector( v1,v2: TVector ): TVector; static;
  end;


  //
  // TExtents
  //
  TExtents = record
    eMin,eMax: TVector;
    Empty: boolean;
    procedure Include(const aVector: TVector);
    procedure Reset;
    function eMid: TVector;
  end;


  //
  // TMatrix
  //
  TMatrix = record
  private
    F: Mat4;

    function GetMat3: Mat3;
    function GetF( i1,i2: Int ): Float;
    procedure SetF( i1,i2: Int; v: Float );
    function GetRow( i: Int ): Vec4;
    procedure SetRow( i: Int; v: Vec4 );

  public

    // identity
    procedure SetIdentity; inline;
    // transpose
    function Transpose: TMatrix;
    procedure SetTranspose;
    // translate
    function Translate( aVector: TVector ): TMatrix;
    procedure SetTranslate( aVector: TVector );
    // pitch
    function Pitch( aAngle: Float ): TMatrix;
    procedure SetPitch( aAngle: Float );
    // toll
    function Roll( aAngle: Float ): TMatrix;
    procedure SetRoll( aAngle: Float );
    // turn
    function Turn( aAngle: Float ): TMatrix;
    procedure SetTurn( aAngle: Float );

    // normalize
    function Normalize: TMatrix;
    procedure SetNormalize;
    // adjoint
    function Adjoint: TMatrix;
    procedure SetAdjoint;
    // invert
    function Invert: TMatrix;
    procedure SetInvert;

    function Transform( aVector: TVector ): TVector;

    class function Make( const aF: array of Float ): TMatrix; static;
    class function IdentityMatrix: TMatrix; static;
    function IsIdentity: Boolean;
    class function ScaleMatrix(
      aVector: TVector ): TMatrix; static;
    class function TranslationMatrix(
      aVector: TVector ): TMatrix; static;
    class function RotationMatrix(
      aAxis: TVector; aAngle: Float ): TMatrix; static;
    class function LookAtMatrix(
      aEye,aTarget,aUp: TVector ): TMatrix; static;
    class function FromFrustum( aLeft,aRight,aBottom,aTop,
      aZNear,aZFar: Double ): TMatrix; static;
    class function PerspectiveMatrix( aFOVY,aAspect,
      aZNear,aZFar: Double ): TMatrix; static;
    class function ReflectionMatrix( const APlanePoint: TVector;
      const APlaneNormal: TVector): TMatrix; static;
    class function OrthoMatrix(Left, Right, Bottom, Top,
      ZNear, ZFar: Float): TMatrix; static;
    function UnProject(aWindowVector: TVector;
      const aViewport: TVector;
      out anObjectVector: TVector): Boolean;
    function GetAddr: PMat4;
    procedure Swap( var aMatrix: TMatrix );

    function Minor( aX,aY: Int ): Float; inline;
    function Determinant: Float;

    property Matrix4: Mat4 read F;
    property Matrix3: Mat3 read GetMat3;
    property A[i1,i2: Int]: Float read GetF write SetF; default;
    property Row[i: Int]: Vec4 read GetRow write SetRow;

    class operator Multiply( m: TMatrix; d: Float ): TMatrix;
    class operator Multiply( m1,m2: TMatrix ): TMatrix;
    class operator Divide( m: TMatrix; d: Float ): TMatrix;

    class operator Implicit( m: Mat3 ): TMatrix;
    class operator Implicit( m: Mat4 ): TMatrix;
    class operator Explicit( m: Mat3 ): TMatrix;
    class operator Explicit( m: Mat4 ): TMatrix;

    class operator Equal( m1, m2: TMatrix ): Bool;
    class operator NotEqual( m1, m2: TMatrix ): Bool;
  end;


  //
  // TFrustum
  //
  TFrustum = record
    vPLeft,vPTop,vPRight,vPBottom,vPNear,vPFar: TVector;

    procedure Build( aViewMat,aProjMat: TMatrix );
    procedure BuildFromMVP( aMVP: TMatrix );
    function IsVolumeClipped( aPos: TVector; aRadius: Float ): Bool;
  end;



  //
  // TQuaternion
  //
  TQuaternion = record
  private
    FVector: TVector;
    function getIm: vec3;
    function getRe: float;
    procedure setIm(const Value: vec3);
    procedure setRe(const Value: float);
    function getVec: vec4;
    procedure setVec(const Value: vec4);
    function getV(Index: integer): float;
    procedure setV(Index: integer; const Value: float);
  public
    class operator Implicit( aVec: TVector ): TQuaternion;
    class operator Explicit( aVec: TVector ): TQuaternion;
    class operator Implicit( aQuat: TQuaternion ): TVector;
    class operator Explicit( aQuat: TQuaternion): TVector;

    property Im: vec3 read getIm write setIm;
    property Re: float read getRe write setRe;
    property Vector: vec4 read getVec write setVec;
    property V[Index: integer]: float read getV write setV; default;
  end;


  //
  // TRayCast
  //
  TRayCast = record
  public

    class function Triangle(
      aPos,aDir, aPtA,aPtB,aPtC: TVector ): Bool; overload; static;
    class function Triangle( aPos,aDir,aPtA,aPtB,aPtC: TVector;
      aPoint: PVec4 = nil; aNormal: PVec4 = nil): Bool; overload; static;
    class function Sphere( aPos,aDir,aSphPos: TVector;
      aSphRadius: Float ): Bool; static;
    class function Box( aPos,aDir,aMin,aMax: TVector;
      aPoint: PVec4 = nil): Bool; static;

  end;

  PFloatRect = ^TFloatRect;
  TFloatRect = record
  private
    function GetWidth: Float;
    procedure SetWidth(const Value: Float);
    function GetHeight: Float;
    procedure SetHeight(const Value: Float);
    function GetSize: TVector;
    procedure SetSize(const Value: TVector);
    function GetLocation: TVector;
  public
    constructor Create(const Origin: TVector); overload;                               // empty rect at given origin
    constructor Create(const Origin: TVector; const Width, Height: Float); overload; // at TPoint of origin with width and height
    constructor Create(const Left, Top, Right, Bottom: Float); overload;              // at x, y with width and height
    constructor Create(const P1, P2: TVector; Normalize: Boolean = False); overload;  // with corners specified by p1 and p2
    constructor Create(const R: TFloatRect; Normalize: Boolean = False); overload;

    // operator overloads
    class operator Equal(const Lhs, Rhs: TFloatRect): Boolean;
    class operator NotEqual(const Lhs, Rhs: TFloatRect): Boolean;

    // union of two rectangles
    class operator Add(const Lhs, Rhs: TFloatRect): TFloatRect;

    // intersection of two rectangles
    class operator Multiply(const Lhs, Rhs: TFloatRect): TFloatRect;

    class function Empty: TFloatRect; inline; static;

    //utility methods
    //makes sure TopLeft is above and to the left of BottomRight
    procedure NormalizeRect;

    //returns true if left = right or top = bottom
    function IsEmpty: Boolean;

    //returns true if the point is inside the rect
    function Contains(const Pt: TVector): Boolean; overload;

    // returns true if the rect encloses R completely
    function Contains(const R: TFloatRect): Boolean; overload;

    // returns true if any part of the rect covers R
    function IntersectsWith(const R: TFloatRect): Boolean;

    // computes an intersection of R1 and R2
    class function Intersect(const R1: TFloatRect; const R2: TFloatRect): TFloatRect; overload; static;

    // replaces current rectangle with its intersection with R
    procedure Intersect(const R: TFloatRect); overload;

    // computes a union of R1 and R2
    class function Union(const R1: TFloatRect; const R2: TFloatRect): TFloatRect; overload; static;

    // replaces current rectangle with its union with R
    procedure Union(const R: TFloatRect); overload;

    // creates a minimal rectangle that contains all points from array Points
    class function Union(const Points: Array of TVector): TFloatRect; overload; static;

    // offsets the rectangle origin relative to current position
    procedure Offset(const DX, DY: Float); overload;
    procedure Offset(const Point: TVector); overload;

    // sets new origin
    procedure SetLocation(const X, Y: Float); overload;
    procedure SetLocation(const Point: TVector); overload;

    // inflate by DX and DY
    procedure Inflate(const DX, DY: Float); overload;

    // inflate in all directions
    procedure Inflate(const DL, DT, DR, DB: Float); overload;

    //returns the center point of the rectangle;
    function CenterPoint: TVector;

    // changing the width is always relative to Left;
    property Width: Float read GetWidth write SetWidth;
    // changing the Height is always relative to Top
    property Height: Float read GetHeight write SetHeight;

    property Size: TVector read GetSize write SetSize;

    property Location: TVector read GetLocation write SetLocation;

  case Integer of
    0: (Left, Top, Right, Bottom: Float);
    1: (TopLeft, BottomRight: TVector);
  end;


const

  VecNull: Vec4 = ( 0,0,0,0 );
  VecFloat: Vec4 = ( 1,1,1,1 );
  VecNegate: Vec4 = ( -1,-1,-1,-1 );
  VecX: Vec4 = ( 1,0,0,0 );
  VecY: Vec4 = ( 0,1,0,0 );
  VecZ: Vec4 = ( 0,0,1,0 );
  VecW: Vec4 = ( 0,0,0,1 );
  VecNX: Vec4 = ( -1,0,0,0 );
  VecNY: Vec4 = ( 0,-1,0,0 );
  VecNZ: Vec4 = ( 0,0,-1,0 );
  VecNW: Vec4 = ( 0,0,0,-1 );
  VecXY: Vec4 = ( 1,1,0,0 );
  VecXZ: Vec4 = ( 1,0,1,0 );
  VecYZ: Vec4 = ( 0,1,1,0 );

  MatNull: Mat4 = (( 0,0,0,0 ),( 0,0,0,0 ),( 0,0,0,0 ),( 0,0,0,0 ));
  MatIdentity: Mat4 = (( 1,0,0,0 ),( 0,1,0,0 ),( 0,0,1,0 ),( 0,0,0,1 ));


function Vector( aX,aY: Float; aZ: Float = 0; aW: Float = 0 ): Vec4;
function Vec3Make( aX,aY: Float; aZ: Float = 0 ): Vec3;
function Vec2Make( aX,aY: Float ): Vec2;
function Vec2iMake( aX,aY: Int): Vec2i;
//procedure NegateVector(var aVec: vec3);


implementation

uses
  uMiscUtils;


{ regular }

function Vector( aX,aY: Float; aZ: Float = 0; aW: Float = 0 ): Vec4;
begin

  result[0] := aX;
  result[1] := aY;
  result[2] := aZ;
  result[3] := aW;

end;

function Vec3Make( aX,aY: Float; aZ: Float = 0 ): Vec3;
begin

  result[0] := aX;
  result[1] := aY;
  result[2] := aZ;

end;

function Vec2Make( aX,aY: Float ): Vec2;
begin
  result[0] := aX;
  result[1] := aY;
end;

function Vec2iMake( aX,aY: Int): Vec2i;
begin
  result[0] := aX;
  result[1] := aY;
end;

{procedure NegateVector(var aVec: vec3);
begin

  aVec[0] := -aVec[0];
  aVec[1] := -aVec[1];
  aVec[2] := -aVec[2];

end;}


{ TVector }


//
// -TVector[N]
//
class operator TVector.Negative( v: TVector ): TVector;
begin

  result := v.Negate;

end;


//
// TVector1[N] + TVector2[N]
//
class operator TVector.Add( v1,v2: TVector ): TVector;
begin

  result.F[0] := v1.F[0] + v2.F[0];
  result.F[1] := v1.F[1] + v2.F[1];
  result.F[2] := v1.F[2] + v2.F[2];
  result.F[3] := v1.F[3] + v2.F[3];

end;


//
// TVector1[N] - TVector2[N]
//
class operator TVector.Subtract( v1,v2: TVector ): TVector;
begin

  result.F[0] := v1.F[0] - v2.F[0];
  result.F[1] := v1.F[1] - v2.F[1];
  result.F[2] := v1.F[2] - v2.F[2];
  result.F[3] := v1.F[3] - v2.F[3];

end;


//
// TVector[N] * Float
//
class operator TVector.Multiply( v: TVector; f: Float ): TVector;
begin

  result.F[0] := v.F[0] * f;
  result.F[1] := v.F[1] * f;
  result.F[2] := v.F[2] * f;
  result.F[3] := v.F[3] * f;

end;


//
// TVector1[N] * TVector2[N]
//
class operator TVector.Multiply( v1,v2: TVector ): TVector;
begin

  result.F[0] := v1.F[0] * v2.F[0];
  result.F[1] := v1.F[1] * v2.F[1];
  result.F[2] := v1.F[2] * v2.F[2];
  result.F[3] := v1.F[3] * v2.F[3];

end;


//
// TVector * Mat4
//
class operator TVector.Multiply( v: TVector; const m: Mat4 ): TVector;
begin

  result.F[0] :=
    v.F[0] * m[0,0] + v.F[1] * m[1,0] + v.F[2] * m[2,0] + v.F[3] * m[3,0];
  result.F[1] :=
    v.F[0] * m[0,1] + v.F[1] * m[1,1] + v.F[2] * m[2,1] + v.F[3] * m[3,1];
  result.F[2] :=
    v.F[0] * m[0,2] + v.F[1] * m[1,2] + v.F[2] * m[2,2] + v.F[3] * m[3,2];
  result.F[3] :=
    v.F[0] * m[0,3] + v.F[1] * m[1,3] + v.F[2] * m[2,3] + v.F[3] * m[3,3];

end;


//
// TVector[N] / Float
//
class operator TVector.Divide( v: TVector; f: Float ): TVector;
begin

  result.F[0] := v.F[0] / f;
  result.F[1] := v.F[1] / f;
  result.F[2] := v.F[2] / f;
  result.F[3] := v.F[3] / f;

end;


//
// TVector1[N] / TVector2[N]
//
class operator TVector.Divide( v1,v2: TVector ): TVector;
begin

  result.F[0] := v1.F[0] / v2.F[0];
  result.F[1] := v1.F[1] / v2.F[1];
  result.F[2] := v1.F[2] / v2.F[2];
  result.F[3] := v1.F[3] / v2.F[3];

end;


//
// TVector1[N] = TVector2[N]
//
class operator TVector.Equal( v1,v2: TVector ): Bool;
begin

  result := ( v1.F[0] = v2.F[0] ) and ( v1.F[1] = v2.F[1] ) and
            ( v1.F[2] = v2.F[2] ) and ( v1.F[3] = v2.F[3] );

end;


//
// TVector1[N] <> TVector2[N]
//
class operator TVector.NotEqual( v1,v2: TVector ): Bool;
begin

  result := not ( v1.F[0] = v2.F[0] ) and ( v1.F[1] = v2.F[1] )
            and ( v1.F[2] = v2.F[2] ) and ( v1.F[3] = v2.F[3] );

end;


class function TVector.Null: TVector;
begin
  Result.Vec4 := VecNull;
end;

//
// TVector1[N] > TVector2[N]
//
class operator TVector.GreaterThan( v1,v2: TVector ): Bool;
begin

  result := ( v1.F[0] > v2.F[0] ) and ( v1.F[1] > v2.F[1] ) and
            ( v1.F[2] > v2.F[2] ) and ( v1.F[3] > v2.F[3] );

end;


//
// TVector1[N] >= TVector2[N]
//
class operator TVector.GreaterThanOrEqual( v1,v2: TVector ): Bool;
begin

  result := ( v1.F[0] >= v2.F[0] ) and ( v1.F[1] >= v2.F[1] ) and
            ( v1.F[2] >= v2.F[2] ) and ( v1.F[3] >= v2.F[3] );

end;


//
// TVector1[N] < TVector2[N]
//
class operator TVector.LessThan( v1,v2: TVector ): Bool;
begin

  result := ( v1.F[0] < v2.F[0] ) and ( v1.F[1] < v2.F[1] ) and
            ( v1.F[2] < v2.F[2] ) and ( v1.F[3] < v2.F[3] );

end;


//
// TVector1[N] <= TVector2[N]
//
class operator TVector.LessThanOrEqual( v1,v2: TVector ): Bool;
begin

  result := ( v1.F[0] <= v2.F[0] ) and ( v1.F[1] <= v2.F[1] ) and
            ( v1.F[2] <= v2.F[2] ) and ( v1.F[3] <= v2.F[3] );

end;



class operator TVector.Implicit( t: TVecTemp ): TVector;
begin

  result := MakeTmp( t );

end;

class operator TVector.Implicit( f: Float ): TVector;
begin

  result.F[0] := f;
  result.F[1] := f;
  result.F[2] := f;
  result.F[3] := f;

end;

class operator TVector.Implicit( const v: Vec2 ): TVector;
begin

  result.F[0] := v[0];
  result.F[1] := v[1];
  result.F[2] := 0;
  result.F[3] := 0;

end;

class operator TVector.Implicit( const v: Vec3 ): TVector;
begin

  result.F[0] := v[0];
  result.F[1] := v[1];
  result.F[2] := v[2];
  result.F[3] := 0;

end;

class operator TVector.Implicit( const v: Vec4 ): TVector;
begin

  result.F := v;

end;

class operator TVector.Implicit( pp: PVec4 ): TVector;
begin

  result.F := pp^;

end;


function TVector.IsNull: Boolean;
begin
  Result := (F[0] = 0) and (F[1] = 0) and  (F[2] = 0) and (F[3] = 0);
end;

class operator TVector.Explicit( t: TVecTemp ): TVector;
begin

  result := t;

end;

class operator TVector.Explicit( f: Float ): TVector;
begin

  result := f;

end;

class operator TVector.Explicit( const v: Vec2 ): TVector;
begin

  result := v;

end;

class operator TVector.Explicit( const v: Vec3 ): TVector;
begin

  result := v;

end;

class operator TVector.Explicit( const v: Vec4 ): TVector;
begin

  result := v;

end;

class operator TVector.Explicit( pp: PVec4 ): TVector;
begin

  result.F := pp^;

end;


//
// TVector.GetAddr
//
function TVector.GetAddr: PVec4;
begin

  result := @F[0];

end;


//
// TVector.Vector
//
class function TVector.Make( aX,aY: Float; aZ: Float = 0;
  aW: Float = 0 ): TVector;
begin

  result.F[0] := aX;
  result.F[1] := aY;
  result.F[2] := aZ;
  result.F[3] := aW;

end;


//
// TVector.VectorTmp
//
class function TVector.MakeTmp( aVecType: TVecTemp ): TVector;
begin

  result.SetVectorTmp( aVecType );

end;


class function TVector.MaxVector(v1, v2: TVector): TVector;
begin
  result[0]:=TMath.Max(v1[0], v2[0]);
  result[1]:=TMath.Max(v1[1], v2[1]);
  result[2]:=TMath.Max(v1[2], v2[2]);
  result[3]:=TMath.Max(v1[3], v2[3]);
end;

class function TVector.MinVector(v1, v2: TVector): TVector;
begin
  result[0]:=TMath.Min(v1[0], v2[0]);
  result[1]:=TMath.Min(v1[1], v2[1]);
  result[2]:=TMath.Min(v1[2], v2[2]);
  result[3]:=TMath.Min(v1[3], v2[3]);
end;

//
// TVector.SetVector
//
procedure TVector.SetVec2(const Value: Vec2);
begin
  F[0] := Value[0];
  F[1] := Value[1];
  F[2] := 0;
  F[3] := 0;
end;

procedure TVector.SetVec3(const Value: Vec3);
begin
  F[0] := Value[0];
  F[1] := Value[1];
  F[2] := Value[2];
  F[3] := 0;
end;

procedure TVector.SetVec4(const Value: Vec4);
begin
  F[0] := Value[0];
  F[1] := Value[1];
  F[2] := Value[2];
  F[3] := Value[3];
end;

procedure TVector.SetVector( aX,aY: Float; aZ: Float = 0; aW: Float = 0 );
begin

  F[0] := aX;
  F[1] := aY;
  F[2] := aZ;
  F[3] := aW;

end;


//
// TVector.SetVectorTmp
//
procedure TVector.SetVectorTmp( aVecType: TVecTemp );
begin

  case aVecType of

    vt0: F := VecNull;
    vt1: F := VecFloat;
    vtN1: F := VecNegate;
    vtX: F := VecX;
    vtY: F := VecY;
    vtZ: F := VecZ;
    vtW: F := VecW;
    vtNX: F := VecNX;
    vtNY: F := VecNY;
    vtNZ: F := VecNZ;
    vtNW: F := VecNW;
    vtXY: F := VecXY;
    vtXZ: F := VecXZ;
    vtYZ: F := VecYZ;

    vtRandom: begin
      F[0] := Random;
      F[1] := Random;
      F[2] := Random;
      F[3] := Random;
      end;

    vtRndBox: begin
      F[0] := Random * 2 - 1;
      F[1] := Random * 2 - 1;
      F[2] := Random * 2 - 1;
      end;

    vtRndSph: begin
      F[2] := 2 * Random - 1;
      TMath.SinCos( CPi2 * Random, Sqrt( 1 - F[2] * F[2] ), F[1], F[0] );
      end;

  end;

end;


//
// TVector.ToVec2
//
function TVector.ToVec2: TVector;
begin

  result.SetVector( F[0], F[1], 0, 0 );

end;


//
// TVector.SetVec2
//
procedure TVector.MakeVec2;
begin

  F[2] := 0;
  F[3] := 0;

end;


//
// TVector.Affine
//
function TVector.Affine: TVector;
begin

  result.SetVector( F[0], F[1], F[2], 0 );

end;


//
// TVector.SetAffine
//
procedure TVector.MakeAffine;
begin

  F[3] := 0;

end;


//
// TVector.Point
//
function TVector.Point: TVector;
begin

  result.SetVector( F[0], F[1], F[2], 1 );

end;


//
// TVector.SetPoint
//
procedure TVector.MakePoint;
begin

  F[3] := 1;

end;


//
// TVector.Squeeze
//
function TVector.Squeeze( aA: TVecTemp; aB: TVecTemp = vt0; aC: TVecTemp = vt0;
  aD: TVecTemp = vt0 ): TVector;
var
    p: array[0..3] of TVecTemp;
    i: integer;
begin

  p[0] := aA;
  p[1] := aB;
  p[2] := aC;
  p[3] := aD;

  for i := 0 to 3 do
    case p[i] of
      vt0: result.F[i] := 0;
      vt1: result.F[i] := 1;
      vtN1: result.F[i] := -1;
      vtX: result.F[i] := F[0];
      vtY: result.F[i] := F[1];
      vtZ: result.F[i] := F[2];
      vtW: result.F[i] := F[3];
      vtNX: result.F[i] := -F[0];
      vtNY: result.F[i] := -F[1];
      vtNZ: result.F[i] := -F[2];
      vtNW: result.F[i] := -F[3];
    end;

end;


//
// TVector.Abs
//
function TVector.Abs: TVector;
begin

  result := self;
  result.SetAbs;

end;


//
// TVector.SetAbs
//
procedure TVector.SetAbs;
begin

  F[0] := TMath.Abs( F[0] );
  F[1] := TMath.Abs( F[1] );
  F[2] := TMath.Abs( F[2] );
  F[3] := TMath.Abs( F[3] );

end;


//
// TVector.Negate
//
function TVector.Negate: TVector;
begin

  result.F[0] := -F[0];
  result.F[1] := -F[1];
  result.F[2] := -F[2];
  result.F[3] := -F[3];

end;


//
// TVector.SetNegate
//
procedure TVector.SetNegate;
begin

  F[0] := -F[0];
  F[1] := -F[1];
  F[2] := -F[2];
  F[3] := -F[3];

end;


//
// TVector.Normalize
//
function TVector.Normalize: TVector;
var
    d: Float;
begin

  d := 1 / Length;
  result.F[0] := F[0] * d;
  result.F[1] := F[1] * d;
  result.F[2] := F[2] * d;
  result.F[3] := 0;

end;

//
// TVector.SetNormalize
//
procedure TVector.SetNormalize;
var
    d: Float;
begin

  d := 1 / Length;
  F[0] := F[0] * d;
  F[1] := F[1] * d;
  F[2] := F[2] * d;
  F[3] := 0;

end;


//
// TVector.Fract
//
function TVector.Fract: TVector;
begin

  result := self;
  result.SetFract;

end;


//
// TVector.SetFract
//
procedure TVector.SetFract;
begin

  F[0] := Frac( F[0] );
  F[1] := Frac( F[1] );
  F[2] := Frac( F[2] );
  F[3] := Frac( F[3] );

end;


//
// TVector.Floor
//
function TVector.Floor: TVector;
begin

  result := self;
  result.SetFloor;

end;


//
// TVector.SetFloor
//
procedure TVector.SetFloor;
begin

  F[0] := Trunc( F[0] );
  F[1] := Trunc( F[1] );
  F[2] := Trunc( F[2] );
  F[3] := Trunc( F[3] );

end;


//
// TVector.Scale
//
function TVector.Scale( aFactor: Float ): TVector;
begin

  result := self;
  result.SetScale( aFactor );

end;


//
// TVector.SetScale
//
procedure TVector.SetScale( aFactor: Float );
begin

  F[0] := F[0] * aFactor;
  F[1] := F[1] * aFactor;
  F[2] := F[2] * aFactor;
  F[3] := F[3] * aFactor;

end;


//
// TVector.GetVec3
//
function TVector.GetVec3: Vec3;
begin

  result[0] := F[0];
  result[1] := F[1];
  result[2] := F[2];

end;


function TVector.GetVec4: Vec4;
begin
  result[0] := F[0];
  result[1] := F[1];
  result[2] := F[2];
  result[3] := F[3];
end;

//
// TVector.GetVec2
//
function TVector.GetVec2: Vec2;
begin

  result[0] := F[0];
  result[1] := F[1];

end;


//
// TVector.GetVec2
//
function TVector.GetF( i: Int ): Float;
begin

  result := F[i];

end;


//
// TVector.GetVec2
//
procedure TVector.SetF( i: Int; v: Float );
begin

  F[i] := v;

end;


//
// TVector.Lerp
//
function TVector.Lerp( aVector: TVector; aRatio: Float ): TVector;
begin

  result.F[0] := F[0] + ( aVector.F[0] - F[0] ) * aRatio;
  result.F[1] := F[1] + ( aVector.F[1] - F[1] ) * aRatio;
  result.F[2] := F[2] + ( aVector.F[2] - F[2] ) * aRatio;
  result.F[3] := F[3] + ( aVector.F[3] - F[3] ) * aRatio;

end;


//
// TVector.SetLerp
//
procedure TVector.SetLerp( aVector: TVector; aRatio: Float );
begin

  F[0] := F[0] + ( aVector.F[0] - F[0] ) * aRatio;
  F[1] := F[1] + ( aVector.F[1] - F[1] ) * aRatio;
  F[2] := F[2] + ( aVector.F[2] - F[2] ) * aRatio;
  F[3] := F[3] + ( aVector.F[3] - F[3] ) * aRatio;

end;


//
// TVector.LerpSmooth
//
function TVector.LerpSmooth( aVector: TVector; aRatio: Float ): TVector;
begin

  aRatio := aRatio * ( 3 * aRatio - 2 * aRatio * aRatio );

  result.F[0] := F[0] + ( aVector.F[0] - F[0] ) * aRatio;
  result.F[1] := F[1] + ( aVector.F[1] - F[1] ) * aRatio;
  result.F[2] := F[2] + ( aVector.F[2] - F[2] ) * aRatio;
  result.F[3] := F[3] + ( aVector.F[3] - F[3] ) * aRatio;

end;


//
// TVector.SetLerpSmooth
//
procedure TVector.SetLerpSmooth( aVector: TVector; aRatio: Float );
begin

  aRatio := aRatio * ( 3 * aRatio - 2 * aRatio * aRatio );

  F[0] := F[0] + ( aVector.F[0] - F[0] ) * aRatio;
  F[1] := F[1] + ( aVector.F[1] - F[1] ) * aRatio;
  F[2] := F[2] + ( aVector.F[2] - F[2] ) * aRatio;
  F[3] := F[3] + ( aVector.F[3] - F[3] ) * aRatio;

end;



procedure TVector.CreateOrthonormalBasis(out aNormal, aTangent,
  aBinormal: TVector);
var
  vAbsN, vTemp, vN: TVector;
begin
  vAbsN := self.Abs;

  // the normal is a vector in the opposite direction of v
  vN := self.Negate;

  if vAbsN.X > vAbsN.Y then
  begin
    if vAbsN.X > vAbsN.Z then
    begin
      // x is the dominant axis
      vTemp.X := 0;
      if vN.X > 0.0 then
          vTemp.Y := 1.0
      else
          vTemp.Y := -1.0;
      vTemp.Z := 0;
    end
    else
    begin
      // z is the dominant axis
      vTemp.X := 0;
      if vN.Z > 0.0 then
          vTemp.Y := 1.0
      else
          vTemp.Y := -1.0;
      vTemp.Z := 0;
    end;
  end
  else
  begin
    if vAbsN.Y > vAbsN.Z then
    begin
      // y is the dominant axis
      vTemp.X := 0;
      vTemp.Y := 0;
      if vN.Y > 0.0 then
          vTemp.Z := 1.0
      else
          vTemp.Z := -1.0;
    end
    else
    begin
      // z is the dominant axis
      vTemp.X := 0;
      if vN.Z > 0.0 then
          vTemp.Y := 1.0
      else
          vTemp.Y := -1.0;
      vTemp.Z := 0;
    end
  end;

  aNormal := vN;
  aBinormal := vTemp.Cross(vN);
  aTangent := vN.Cross(aBinormal);
end;

//
// TVector.Cross
//
function TVector.Cross( aVector: TVector ): TVector;
begin

  result.F[0] := F[1] * aVector.F[2] - F[2] * aVector.F[1];
  result.F[1] := F[2] * aVector.F[0] - F[0] * aVector.F[2];
  result.F[2] := F[0] * aVector.F[1] - F[1] * aVector.F[0];
  result.F[3] := 0;

end;


//
// TVector.SetCrossSet
//
procedure TVector.SetCross( aVector: TVector );
begin

  F[0] := F[1] * aVector.F[2] - F[2] * aVector.F[1];
  F[1] := F[2] * aVector.F[0] - F[0] * aVector.F[2];
  F[2] := F[0] * aVector.F[1] - F[1] * aVector.F[0];
  F[3] := 0;

end;


//
// TVector.Combine
//
function TVector.Combine( aVector: TVector; aF1,aF2: Float ): TVector;
begin

  result.F[0] := aF1 * F[0] + aF2 * aVector.F[0];
  result.F[1] := aF1 * F[1] + aF2 * aVector.F[1];
  result.F[2] := aF1 * F[2] + aF2 * aVector.F[2];
  result.F[3] := aF1 * F[3] + aF2 * aVector.F[3];

end;


//
// TVector.SetCombine
//
procedure TVector.SetCombine( aVector: TVector; aF1,aF2: Float );
begin

  F[0] := aF1 * F[0] + aF2 * aVector.F[0];
  F[1] := aF1 * F[1] + aF2 * aVector.F[1];
  F[2] := aF1 * F[2] + aF2 * aVector.F[2];
  F[3] := aF1 * F[3] + aF2 * aVector.F[3];

end;


//
// TVector.CombineEx
//
function TVector.CombineEx( aV2,aV3: TVector; aF1,aF2,aF3: Float ): TVector;
begin

  result.F[0] := aF1 * F[0] + aF2 * aV2.F[0] + aF3 * aV3.F[0];
  result.F[1] := aF1 * F[1] + aF2 * aV2.F[1] + aF3 * aV3.F[1];
  result.F[2] := aF1 * F[2] + aF2 * aV2.F[2] + aF3 * aV3.F[2];
  result.F[3] := aF1 * F[3] + aF2 * aV2.F[3] + aF3 * aV3.F[3];

end;


//
// TVector.SetCombineEx
//
procedure TVector.SetCombineEx( aV2,aV3: TVector; aF1,aF2,aF3: Float );
begin

  F[0] := aF1 * F[0] + aF2 * aV2.F[0] + aF3 * aV3.F[0];
  F[1] := aF1 * F[1] + aF2 * aV2.F[1] + aF3 * aV3.F[1];
  F[2] := aF1 * F[2] + aF2 * aV2.F[2] + aF3 * aV3.F[2];
  F[3] := aF1 * F[3] + aF2 * aV2.F[3] + aF3 * aV3.F[3];

end;


//
// TVector.Perpendicular
//
function TVector.Perpendicular( aNVector: TVector ): TVector;
begin

  result := self - aNVector * self.Affine.Dot( aNVector );

end;


//
// TVector.SetPerpendicular
//
procedure TVector.SetPerpendicular( aNVector: TVector );
begin

  self := self - aNVector * Affine.Dot( aNVector );

end;


//
// TVector.Reflect
//
function TVector.Reflect( aNVector: TVector ): TVector;
begin

  result := Combine( aNVector, 1, -2 * Affine.Dot( aNVector ));

end;


//
// TVector.SetReflect
//
procedure TVector.SetReflect( aNVector: TVector );
begin

  F := Combine( aNVector, 1, -2 * Affine.Dot( aNVector )).F;

end;


//
// TVector.Rotate
//
function TVector.Rotate( aAxis: TVector; aAngle: Float ): TVector;
begin

  result := TMatrix.RotationMatrix( aAxis, aAngle ).Transform( self );

end;

//
// TVector.RotateAround
//
procedure TVector.RotateAround(aCenter, anUp: TVector;
  aPitchDelta, aTurnDelta: Float);
var
  original, direction, right: TVector;
  pitchNow, dist: Float;
begin
  original := Self - aCenter;
  direction := original;
  dist := direction.Length;
  direction.SetNormalize;
  right := anUp.Cross(direction);
  if right.Length < 0.001 then
    right := vecX
  else
    right.SetNormalize;
  pitchNow := TMath.ArcCos(anUp.Dot(direction));
  pitchNow := TMath.Clamp(pitchNow + TMath.DegToRad(aPitchDelta),
    0 + 0.025, PI - 0.025);
  direction := anUp;
  direction.SetRotate(right, -pitchNow);
  direction.SetRotate(anUp, -TMath.DegToRad(aTurnDelta));
  direction.SetScale(dist);
  Self := Self + direction - original;
end;

//
// TVector.SetRotate
//
procedure TVector.SetRotate( aAxis: TVector; aAngle: Float );
begin

  self := TMatrix.RotationMatrix( aAxis, aAngle ).Transform( self );

end;


//
// TVector.Length
//
function TVector.Length: Float;
begin

  result := Sqrt( F[0] * F[0] + F[1] * F[1] + F[2] * F[2] );

end;


//
// TVector.LengthSqr
//
function TVector.LengthSqr: Float;
begin

  result := F[0] * F[0] + F[1] * F[1] + F[2] * F[2];

end;


//
// TVector.DistanceSqr
//
function TVector.DistanceSqr( aPoint: TVector ): Float;
begin

  result := sqr( F[0] - aPoint.F[0] ) + sqr( F[1] - aPoint.F[1] ) +
            sqr( F[2] - aPoint.F[2] );

end;


//
// TVector.Distance
//
function TVector.Distance( aPoint: TVector ): Float;
begin

  result := sqrt( DistanceSqr( aPoint ));

end;


//
// TVector.Dot
//
function TVector.Dot( aVector: TVector ): Float;
begin

  result := F[0] * aVector.F[0] + F[1] * aVector.F[1] +
            F[2] * aVector.F[2] + F[3] * aVector.F[3];

end;


//
// TVector.PointProject
//
function TVector.PointProject( aPos,aDir: TVector ): Float;
begin

  result := ( self - aPos ).Dot( aDir );

end;


//
// TVector.Swap
//
procedure TVector.SwapWith( var aVector: TVector );
var
    v: TVector;
begin

  v := self;
  self := aVector;
  aVector := v;

end;




{ TMatrix }


//
// TMatrix.GetMat3;
//
function TMatrix.GetMat3: Mat3;
begin

  result[0] := PVec3( @F[0,0] )^;
  result[1] := PVec3( @F[1,0] )^;
  result[2] := PVec3( @F[2,0] )^;

end;


//
// TMatrix.GetF
//
function TMatrix.GetF( i1,i2: Int ): Float;
begin

  result := F[i1,i2];

end;


//
// TMatrix.SetF
//
procedure TMatrix.SetF( i1,i2: Int; v: Float );
begin

  F[i1,i2] := v;

end;


//
// TMatrix.GetRow
//
function TMatrix.GetRow( i: Int ): Vec4;
begin

  result := F[i];

end;


//
// TMatrix.SetRow
//
procedure TMatrix.SetRow( i: Int; v: Vec4 );
begin

  F[i] := v;

end;


//
// TMatrix.Transpose
//
function TMatrix.Transpose: TMatrix;
begin

  result := self;
  result.SetTranspose;

end;


//
// TMatrix.SetTranspose
//
procedure TMatrix.SetTranspose;
begin

  TMath.Swap( F[1,0], F[0,1] );
  TMath.Swap( F[2,0], F[0,2] );
  TMath.Swap( F[3,0], F[0,3] );
  TMath.Swap( F[2,1], F[1,2] );
  TMath.Swap( F[3,1], F[1,3] );
  TMath.Swap( F[3,2], F[2,3] );

end;


//
// TMatrix.SetIdentity
//
procedure TMatrix.SetIdentity;
begin

  F := MatIdentity;

end;


//
// TMatrix.Translate
//
function TMatrix.Translate( aVector: TVector ): TMatrix;
begin

  result := self;
  result.SetTranslate( aVector );

end;


//
// TMatrix.SetTranslate
//
procedure TMatrix.SetTranslate( aVector: TVector );
begin

  F[0,3] := F[0,3] + aVector.F[0];
  F[1,3] := F[1,3] + aVector.F[1];
  F[2,3] := F[2,3] + aVector.F[2];

end;


//
// TMatrix.Pitch
//
function TMatrix.Pitch( aAngle: Float ): TMatrix;
begin

  result := self * RotationMatrix( F[0], aAngle );

end;


//
// TMatrix.SetPitch
//
procedure TMatrix.SetPitch( aAngle: Float );
begin

  self := self * RotationMatrix( F[0], aAngle );

end;


//
// TMatrix.Roll
//
class function TMatrix.ReflectionMatrix(const APlanePoint,
  APlaneNormal: TVector): TMatrix;
var
   pv2 : Float;
begin
   // Precalcs
   pv2:=2*APlanePoint.Dot(APlaneNormal);
   // 1st column
   Result.SetF(0,0,1-2*Sqr(APlaneNormal[0]));
   Result.SetF(0,1,-2*APlaneNormal[0]*APlaneNormal[1]);
   Result.SetF(0,2,-2*APlaneNormal[0]*APlaneNormal[2]);
   Result.SetF(0,3,0);
   // 2nd column
   Result.SetF(1,0,-2*APlaneNormal[1]*APlaneNormal[0]);
   Result.SetF(1,1,1-2*Sqr(APlaneNormal[1]));
   Result.SetF(1,2,-2*APlaneNormal[1]*APlaneNormal[2]);
   Result.SetF(1,3,0);
   // 3rd column
   Result.SetF(2,0,-2*APlaneNormal[2]*APlaneNormal[0]);
   Result.SetF(2,1,-2*APlaneNormal[2]*APlaneNormal[1]);
   Result.SetF(2,2,1-2*Sqr(APlaneNormal[2]));
   Result.SetF(2,3,0);
   // 4th column
   Result.SetF(3,0,pv2*APlaneNormal[0]);
   Result.SetF(3,1,pv2*APlaneNormal[1]);
   Result.SetF(3,2,pv2*APlaneNormal[2]);
   Result.SetF(3,3,1);
end;

function TMatrix.Roll( aAngle: Float ): TMatrix;
begin

  result := self * RotationMatrix( F[2], aAngle );

end;


//
// TMatrix.SetRoll
//
procedure TMatrix.SetRoll( aAngle: Float );
begin

  self := self * RotationMatrix( F[2], aAngle );

end;


//
// TMatrix.Turn
//
function TMatrix.Turn( aAngle: Float ): TMatrix;
begin

  result := self * RotationMatrix( F[1], aAngle );

end;


//
// TMatrix.SetTurn
//
procedure TMatrix.SetTurn( aAngle: Float );
begin

  self := self * RotationMatrix( F[1], aAngle );

end;


//
// TMatrix.Normalize
//
function TMatrix.Normalize: TMatrix;
begin

  result := self;
  result.SetNormalize;

end;


class operator TMatrix.NotEqual(m1, m2: TMatrix): Bool;
begin
  Result := not CompareMem(@m1.F, @m2.F, SizeOf(mat4));
end;

//
// TMatrix.SetNormalize
//
procedure TMatrix.SetNormalize;
begin

  F[1] := TVector( F[1] ).Affine.Normalize.F;
  F[2] := TVector( F[0] ).Affine.Normalize.Cross( F[1] ).F;
  F[0] := TVector( F[1] ).Cross( F[2] ).F;
  F[3] := VecW;

end;


//
// TMatrix.Adjoint
//
function TMatrix.Adjoint: TMatrix;
begin

  result := self;
  result.SetAdjoint;

end;


//
// TMatrix.SetAdjoint
//
procedure TMatrix.SetAdjoint;
var
  M: Mat4;
begin

  M[0] := Vector(  Minor( 0,0 ), -Minor( 0,1 ),  Minor( 0,2 ), -Minor( 0,3 ));
  M[1] := Vector( -Minor( 1,0 ),  Minor( 1,1 ), -Minor( 1,2 ),  Minor( 1,3 ));
  M[2] := Vector(  Minor( 2,0 ), -Minor( 2,1 ),  Minor( 2,2 ), -Minor( 2,3 ));
  M[3] := Vector( -Minor( 3,0 ),  Minor( 3,1 ), -Minor( 3,2 ),  Minor( 3,3 ));

  F := M;
end;


//
// TMatrix.Invert
//
function TMatrix.Invert: TMatrix;
begin

  result := self;
  result.SetInvert;

end;


function TMatrix.IsIdentity: Boolean;
var R, C: Integer;
begin
  Result := True;
  for R := 0 to 3 do
    for C := 0 to 3 do
      if R = C then
      begin
        if GetF(R,C) <> 1.0 then
          exit(False);
      end
      else
        if GetF(R,C) <> 0.0 then
          exit(False);
end;

//
// TMatrix.SetInvert
//
procedure TMatrix.SetInvert;
var
   d: Float;
begin

  d := Determinant;
  if TMath.Abs( d ) < CEps3 then SetIdentity
    else self := Adjoint / d;

end;


//
// TMatrix.Transform
//
function TMatrix.Transform( aVector: TVector ): TVector;
begin

  result := aVector * F;

end;


//
// TMatrix.Matrix
//
class function TMatrix.Make( const aF: array of Float ): TMatrix;
begin

  result.F := PMat4( @aF[0] )^;

end;


//
// TMatrix.IdentityMatrix
//
class function TMatrix.IdentityMatrix: TMatrix;
begin

  result.F := MatIdentity;

end;


//
// TMatrix.ScaleMatrix
//
class function TMatrix.ScaleMatrix( aVector: TVector ): TMatrix;
begin

  result.F := MatIdentity;
  result.F[0][0] := aVector.F[0];
  result.F[1][1] := aVector.F[1];
  result.F[2][2] := aVector.F[2];

end;


//
// TMatrix.TranslationMatrix
//
class function TMatrix.TranslationMatrix( aVector: TVector ): TMatrix;
begin

  result.F := MatIdentity;
  result.F[3] := aVector.Point.F;

end;


//
// TMatrix.RotationMatrix
//
class function TMatrix.RotationMatrix( aAxis: TVector; aAngle: Float ): TMatrix;
var
    ax: TVector;
    s,c,dc: Float;
begin

  TMath.SinCos( aAngle, s, c );
  dc := 1 - c;
  ax := aAxis.Normalize;

  result.F := MatIdentity;
  result.F[0][0] := ax.F[0] * ax.F[0] * dc + c;
  result.F[0][1] := ax.F[1] * ax.F[0] * dc - ax.F[2] * s;
  result.F[0][2] := ax.F[2] * ax.F[0] * dc + ax.F[1] * s;
  result.F[1][0] := ax.F[0] * ax.F[1] * dc + ax.F[2] * s;
  result.F[1][1] := ax.F[1] * ax.F[1] * dc + c;
  result.F[1][2] := ax.F[2] * ax.F[1] * dc - ax.F[0] * s;
  result.F[2][0] := ax.F[0] * ax.F[2] * dc - ax.F[1] * s;
  result.F[2][1] := ax.F[1] * ax.F[2] * dc + ax.F[0] * s;
  result.F[2][2] := ax.F[2] * ax.F[2] * dc + c;

end;


//
// TMatrix.LookAtMatrix
//
class function TMatrix.LookAtMatrix( aEye,aTarget,aUp: TVector ): TMatrix;
begin

  with result do begin

    F[2] := ( aTarget - aEye ).Normalize.F;
    F[0] := TVector( F[2] ).Cross( aUp ).Normalize.F;
    F[1] := TVector( F[0] ).Cross( F[2] ).F;
    F[2] := TVector( F[2] ).Negate.F;
    F[3] := VecW;
    SetTranspose;
    F[3] := result.Transform( aEye.Negate.Point ).F;

    end;

end;


//
// TMatrix.FromFrustum
//
class function TMatrix.FromFrustum( aLeft,aRight,aBottom,aTop,
  aZNear,aZFar: Double ): TMatrix;
var
    dx,dy,dz: Double;
begin

  dx := 1 / ( aRight - aLeft );
  dy := 1 / ( aTop - aBottom );
  dz := 1 / ( aZFar - aZNear );

  result.F := MatNull;
  result.F[0][0] := 2 * aZNear * dx;
  result.F[1][1] := 2 * aZNear * dy;
  result.F[2] := Vector( ( aRight + aLeft ) * dx, ( aTop + aBottom ) * dy,
                       - ( aZFar + aZNear ) * dz, -1 );
  result.F[3][2] := -2 * aZFar * aZNear * dz;

end;


//
// TMatrix.PerspectiveMatrix
//
class function TMatrix.PerspectiveMatrix( aFOVY,aAspect,
  aZNear,aZFar: Double ): TMatrix;
var
    d: Double;
begin

  d := aZNear * TMath.Tan( CPiD360 * TMath.Clamp( aFOVY, 0, 179.9 ));
  result :=
    TMatrix.FromFrustum( -aAspect * d, aAspect * d, -d, d, aZNear, aZFar );

end;


//
// TMatrix.GetAddr
//
function TMatrix.GetAddr: PMat4;
begin

  result := @F[0];

end;


//
// TMatrix.Swap
//
procedure TMatrix.Swap( var aMatrix: TMatrix );
var
    m: Mat4;
begin

  m := F;
  F := aMatrix.F;
  aMatrix.F := m;

end;


//
// TMatrix.Minor
//
function TMatrix.Minor( aX,aY: Int ): Float;
var
    a,b,c: Int;
    i,j,k: PVec4;
begin

  if aX = 0 then a := 1 else a := 0;
  if aX > 1 then b := 1 else b := 2;
  if aX = 3 then c := 2 else c := 3;
  if aY = 0 then i := @F[1] else i := @F[0];
  if aY > 1 then j := @F[1] else j := @F[2];
  if aY = 3 then k := @F[2] else k := @F[3];

  result := i[a] * ( j[b] * k[c] - j[c] * k[b] )
          - j[a] * ( i[b] * k[c] - i[c] * k[b] )
          + k[a] * ( i[b] * j[c] - i[c] * j[b] );

end;


//
// TMatrix.Determinant
//
function TMatrix.Determinant: Float;
begin

  result := F[0,0] * Minor( 0,0 ) - F[0,1] * Minor( 1,0 )
          + F[0,2] * Minor( 2,0 ) - F[0,3] * Minor( 3,0 );

end;


//
// TMatrix * Float
//
class operator TMatrix.Multiply( m: TMatrix; d: Float ): TMatrix;
var
    i,j: Int;
begin

  for i := 0 to 3 do
    for j := 0 to 3 do
      result.F[i,j] := m.F[i,j] * d;

end;


//
// TMatrix1 * TMatrix2
//
class operator TMatrix.Multiply( m1,m2: TMatrix ): TMatrix;
var
    a,b,c: PMat4;
begin

  a := @m1.F[0];
  b := @m2.F[0];
  c := @result.F[0];

c[0,0] := a[0,0] * b[0,0] + a[0,1] * b[1,0] + a[0,2] * b[2,0] + a[0,3] * b[3,0];
c[0,1] := a[0,0] * b[0,1] + a[0,1] * b[1,1] + a[0,2] * b[2,1] + a[0,3] * b[3,1];
c[0,2] := a[0,0] * b[0,2] + a[0,1] * b[1,2] + a[0,2] * b[2,2] + a[0,3] * b[3,2];
c[0,3] := a[0,0] * b[0,3] + a[0,1] * b[1,3] + a[0,2] * b[2,3] + a[0,3] * b[3,3];
c[1,0] := a[1,0] * b[0,0] + a[1,1] * b[1,0] + a[1,2] * b[2,0] + a[1,3] * b[3,0];
c[1,1] := a[1,0] * b[0,1] + a[1,1] * b[1,1] + a[1,2] * b[2,1] + a[1,3] * b[3,1];
c[1,2] := a[1,0] * b[0,2] + a[1,1] * b[1,2] + a[1,2] * b[2,2] + a[1,3] * b[3,2];
c[1,3] := a[1,0] * b[0,3] + a[1,1] * b[1,3] + a[1,2] * b[2,3] + a[1,3] * b[3,3];
c[2,0] := a[2,0] * b[0,0] + a[2,1] * b[1,0] + a[2,2] * b[2,0] + a[2,3] * b[3,0];
c[2,1] := a[2,0] * b[0,1] + a[2,1] * b[1,1] + a[2,2] * b[2,1] + a[2,3] * b[3,1];
c[2,2] := a[2,0] * b[0,2] + a[2,1] * b[1,2] + a[2,2] * b[2,2] + a[2,3] * b[3,2];
c[2,3] := a[2,0] * b[0,3] + a[2,1] * b[1,3] + a[2,2] * b[2,3] + a[2,3] * b[3,3];
c[3,0] := a[3,0] * b[0,0] + a[3,1] * b[1,0] + a[3,2] * b[2,0] + a[3,3] * b[3,0];
c[3,1] := a[3,0] * b[0,1] + a[3,1] * b[1,1] + a[3,2] * b[2,1] + a[3,3] * b[3,1];
c[3,2] := a[3,0] * b[0,2] + a[3,1] * b[1,2] + a[3,2] * b[2,2] + a[3,3] * b[3,2];
c[3,3] := a[3,0] * b[0,3] + a[3,1] * b[1,3] + a[3,2] * b[2,3] + a[3,3] * b[3,3];

end;


//
// TMatrix / Float
//
class operator TMatrix.Divide( m: TMatrix; d: Float ): TMatrix;
var
    i,j: Int;
begin

  for i := 0 to 3 do
    for j := 0 to 3 do
      result.F[i,j] := m.F[i,j] / d;

end;


class operator TMatrix.Implicit( m: Mat3 ): TMatrix;
begin

  result.F := MatIdentity;
  PVec3( @result.F[0] )^ := m[0];
  PVec3( @result.F[1] )^ := m[1];
  PVec3( @result.F[2] )^ := m[2];

end;

class operator TMatrix.Implicit( m: Mat4 ): TMatrix;
begin

  result.F := m;

end;

class operator TMatrix.Explicit( m: Mat3 ): TMatrix;
begin

  result := m;

end;

class operator TMatrix.Equal(m1, m2: TMatrix): Bool;
begin
  Result := CompareMem(@m1.F, @m2.F, SizeOf(mat4));
end;

class operator TMatrix.Explicit( m: Mat4 ): TMatrix;
begin

  result.F := m;

end;

class function TMatrix.OrthoMatrix(Left, Right, Bottom, Top, ZNear, ZFar: Float): TMatrix;
begin
  Result.SetF(0,0, 2 / (Right - Left));
  Result.SetF(0,1, 0);
  Result.SetF(0,2, 0);
  Result.SetF(0,3, 0);

  Result.SetF(1,0, 0);
  Result.SetF(1,1, 2 / (Top - Bottom));
  Result.SetF(1,2, 0);
  Result.SetF(1,3, 0);

  Result.SetF(2,0, 0);
  Result.SetF(2,1, 0);
  Result.SetF(2,2, -2 / (ZFar - ZNear));
  Result.SetF(2,3, 0);

  Result.SetF(3,0, (Left + Right) / (Left - Right));
  Result.SetF(3,1, (Bottom + Top) / (Bottom - Top));
  Result.SetF(3,2, (ZNear + ZFar) / (ZNear - ZFar));
  Result.SetF(3,3, 1);
end;

function TMatrix.UnProject(
  aWindowVector: TVector;
  const aViewport: TVector;
  out anObjectVector: TVector): Boolean;
var
  invViewProjMatrix: TMatrix;
begin
  invViewProjMatrix := Invert;
  aWindowVector[3] := 1.0;
  // Map x and y from window coordinates
  aWindowVector[0] := (aWindowVector[0] - aViewport[0]) / aViewport[2];
  aWindowVector[1] := (aWindowVector[1] - aViewport[1]) / aViewport[3];
  // Map to range -1 to 1
  aWindowVector[0] := aWindowVector[0] * 2 - 1;
  aWindowVector[1] := aWindowVector[1] * 2 - 1;
  aWindowVector[2] := aWindowVector[2] * 2 - 1;
  anObjectVector := invViewProjMatrix.Transform(aWindowVector);
  if anObjectVector[3] = 0.0 then
    Exit(False);
  anObjectVector[0] := anObjectVector[0] / anObjectVector[3];
  anObjectVector[1] := anObjectVector[1] / anObjectVector[3];
  anObjectVector[2] := anObjectVector[2] / anObjectVector[3];
  Result := True;
end;

{ TFrustum }

//
// TFrustum.Build
//
procedure TFrustum.Build( aViewMat,aProjMat: TMatrix );
begin

  BuildFromMVP( aViewMat * aProjMat );

end;


//
// TFrustum.BuildFromMVP
//
procedure TFrustum.BuildFromMVP( aMVP: TMatrix );
begin

  vPLeft.SetVector(   aMVP[0,3] + aMVP[0,0], aMVP[1,3] + aMVP[1,0],
                      aMVP[2,3] + aMVP[2,0], aMVP[3,3] + aMVP[3,0] );
  vPLeft := vPLeft / vPLeft.Length;
  vPTop.SetVector(    aMVP[0,3] - aMVP[0,1], aMVP[1,3] - aMVP[1,1],
                      aMVP[2,3] - aMVP[2,1], aMVP[3,3] - aMVP[3,1] );
  vPTop := vPTop / vPTop.Length;
  vPRight.SetVector(  aMVP[0,3] - aMVP[0,0], aMVP[1,3] - aMVP[1,0],
                      aMVP[2,3] - aMVP[2,0], aMVP[3,3] - aMVP[3,0] );
  vPRight := vPRight / vPRight.Length;
  vPBottom.SetVector( aMVP[0,3] + aMVP[0,1], aMVP[1,3] + aMVP[1,1],
                      aMVP[2,3] + aMVP[2,1], aMVP[3,3] + aMVP[3,1] );
  vPBottom := vPBottom / vPBottom.Length;
  vPNear.SetVector(   aMVP[0,3] + aMVP[0,2], aMVP[1,3] + aMVP[1,2],
                      aMVP[2,3] + aMVP[2,2], aMVP[3,3] + aMVP[3,2] );
  vPNear := vPNear / vPNear.Length;
  vPFar.SetVector(    aMVP[0,3] - aMVP[0,2], aMVP[1,3] - aMVP[1,2],
                      aMVP[2,3] - aMVP[2,2], aMVP[3,3] - aMVP[3,2] );
  vPFar := vPFar / vPFar.Length;

end;


//
// TFrustum.IsVolumeClipped
//
function TFrustum.IsVolumeClipped( aPos: TVector; aRadius: Float ): Bool;
begin

  aPos.MakePoint;
  result := ( vPLeft.Dot( aPos ) < -aRadius )
         or ( vPTop.Dot( aPos ) < -aRadius )
         or ( vPRight.Dot( aPos ) < -aRadius )
         or ( vPBottom.Dot( aPos ) < -aRadius )
         or ( vPNear.Dot( aPos ) < -aRadius )
         or ( vPFar.Dot( aPos ) < -aRadius );

end;




{ TRayCast }

//
// TRayCast.Triangle
//
class function TRayCast.Triangle(aPos,aDir,aPtA,aPtB,aPtC: TVector): Bool;
var
  dAB,dBC,dCA: Float;
begin

  aPtA := aPtA - aPos;
  aPtB := aPtB - aPos;
  aPtC := aPtC - aPos;

  dAB := aPtA.Cross( aPtB ).Dot( aDir );
  dBC := aPtB.Cross( aPtC ).Dot( aDir );
  dCA := aPtC.Cross( aPtA ).Dot( aDir );

  result := (( dAB > 0 ) and ( dBC > 0 ) and ( dCA > 0 ))
    or (( dAB < 0 ) and ( dBC < 0 ) and ( dCA < 0 ));

end;


//
// TRayCast.Triangle
//
class function TRayCast.Triangle( aPos,aDir,aPtA,aPtB,aPtC: TVector;
  aPoint: PVec4 = nil; aNormal: PVec4 = nil): Bool;
var
   v1,v2,pv,qv,tv: TVector;
   t,u,v,det : Float;
begin

  result := false;

  v1 := aPtB - aPtA;
  v2 := aPtC - aPtA;

  pv := aDir.Cross( v2 );
  det := v1.Dot( pv );
  if abs ( det ) < CEps2 then
    exit;

  u := ( aPos - aPtA ).Dot( pv ) / det;
  if ( u < 0 ) or ( u > 1 ) then
    exit;

  qv := tv.Cross( v1 );
  v := aDir.Dot( qv ) / det;
  result := ( v >= 0 ) and ( u + v <= 1 );
  if not result then
    exit;

  t := v2.Dot( qv ) / det;
  result := t > 0;
  if not result then
    exit;

  if aPoint <> nil then
    aPoint^ := aPos.Combine( aDir, 1, t ).F;

  if aNormal <> nil then
    aNormal^ := v1.Cross( v2 ).F;

end;


//
// TRayCast.Sphere
//
class function TRayCast.Sphere( aPos,aDir,aSphPos: TVector;
  aSphRadius: Float ): Bool;
var
    p: Float;
begin

  p := aSphPos.PointProject( aPos, aDir );
  if p <= 0 then
    p := 0;

  result :=
    aSphPos.DistanceSqr( aPos.Combine( aDir, 1, p )) <= sqr( aSphRadius );

end;


//
// TRayCast.Box
//
class function TRayCast.Box( aPos,aDir,aMin,aMax: TVector;
  aPoint: PVec4 = nil): Bool;
var
    i,p: Int;
    ResAFV,MaxDist,Plane :TVector;
    ma: array[0..2] of Bool;
begin

  result := True;
  for i := 0 to 2 do begin

    ma[i] := not ( aPos[i] < aMin[i] ) or ( aPos[i] > aMax[i] );
    if not ma[i] then begin

      if aPos[i] < aMin[i] then plane[i] := aMin[i]
        else plane[i] := aMax[i];
      Result := False;

      end
    else
      if aPoint <> nil then aPoint^ := aPos.F;

    end;

  if Result then exit;

  p := 0;
  for i := 0 to 2 do

    if ma[i] or ( aDir[i] = 0 ) then MaxDist[i] := -1
    else begin

      MaxDist[i] := ( plane[i] - aPos[i] ) / aDir[i];
      if MaxDist[i] > 0 then begin
        if MaxDist[p] < MaxDist[i] then p := i;
        Result := True;
        end;

      end;

  if not Result then exit;

  for i := 0 to 2 do
    if p = i then ResAFV[i] := plane[i]
    else begin

      ResAFV[i] := aPos[i] + MaxDist[p] * aDir[i];
      Result := ( ResAFV[i] >= aMin[i] ) and ( ResAFV[i] <= aMax[i] );

      if not Result then exit;

      end;

  if aPoint <> nil then aPoint^ := ResAFV.F;

end;


{ TQuaternion }

class operator TQuaternion.Explicit(aVec: TVector): TQuaternion;
begin
  result.FVector:=aVec;
end;

class operator TQuaternion.Explicit(aQuat: TQuaternion): TVector;
begin
  result:=aQuat.FVector;
end;

function TQuaternion.getIm: vec3;
begin
  result:=FVector.GetVec3;
end;

function TQuaternion.getRe: float;
begin
  result:=FVector.F[3];
end;

function TQuaternion.getV(Index: integer): float;
begin
  result:=FVector.F[Index];
end;

function TQuaternion.getVec: vec4;
begin
  result:=FVector.Vec4;
end;

class operator TQuaternion.Implicit(aVec: TVector): TQuaternion;
begin
  result.FVector:=aVec;
end;

class operator TQuaternion.Implicit(aQuat: TQuaternion): TVector;
begin
  result:=aQuat.FVector;
end;

procedure TQuaternion.setIm(const Value: vec3);
begin
  FVector[0]:=Value[0]; FVector[1]:=Value[1]; FVector[2]:=Value[2];
end;

procedure TQuaternion.setRe(const Value: float);
begin
  FVector[3]:=Value;
end;

procedure TQuaternion.setV(Index: integer; const Value: float);
begin
  FVector.F[Index]:=Value;
end;

procedure TQuaternion.setVec(const Value: vec4);
begin
  FVector.F:=Value;
end;

{ TExtents }

procedure TExtents.Include(const aVector: TVector);
begin
  if Empty then begin
    eMin:=aVector; eMax:=aVector; Empty := False;
  end else begin
    eMin:=TVector.MinVector(emin,aVector);
    eMax:=TVector.MaxVector(emax,aVector);
  end;
end;

function TExtents.eMid: TVector;
begin
  Result := eMin + eMax;
  Result.SetScale(0.5);
end;

procedure TExtents.Reset;
begin
  Empty:=true;
  eMin.SetVector(1e10, 1e10, 1e10);
  eMax.SetVector(-1e10, -1e10, -1e10);
end;

{ TFloatRect }

constructor TFloatRect.Create(const R: TFloatRect; Normalize: Boolean);
begin
  Self := R;
  if Normalize then NormalizeRect;
end;

constructor TFloatRect.Create(const Origin: TVector);
begin
  TopLeft := Origin;
  BottomRight := Origin;
end;

constructor TFloatRect.Create(const Left, Top, Right, Bottom: Float);
begin
  Self.Left := Left; Self.Top := Top;
  Self.Right := Right; Self.Bottom := Bottom;
end;

constructor TFloatRect.Create(const P1, P2: TVector; Normalize: Boolean);
begin
  Self.TopLeft := P1;
  Self.BottomRight := P2;
  if Normalize then NormalizeRect;
end;

constructor TFloatRect.Create(const Origin: TVector; const Width, Height: Float);
begin
  Self.TopLeft := Origin;
  Self.Width := Width;
  Self.Height := Height;
end;

class operator TFloatRect.Equal(const Lhs, Rhs: TFloatRect): Boolean;
begin
  Result := (Lhs.TopLeft = Rhs.TopLeft) and
            (Lhs.BottomRight = Rhs.BottomRight);
end;

class operator TFloatRect.NotEqual(const Lhs, Rhs: TFloatRect): Boolean;
begin
  Result := not (Lhs = Rhs);
end;

class operator TFloatRect.Add(const Lhs, Rhs: TFloatRect): TFloatRect;
begin
  Result := TFloatRect.Union(Lhs, Rhs);
end;

class operator TFloatRect.Multiply(const Lhs, Rhs: TFloatRect): TFloatRect;
begin
  Result := TFloatRect.Intersect(Lhs, Rhs);
end;

function TFloatRect.CenterPoint: TVector;
begin
  Result.X := (Right - Left)/2.0 + Left;
  Result.Y := (Bottom - Top)/2.0 + Top;
end;

function TFloatRect.Contains(const R: TFloatRect): Boolean;
begin
  Result := Contains(R.TopLeft) and Contains(R.BottomRight);
end;

function TFloatRect.Contains(const Pt: TVector): Boolean;
begin
  Result := ((Pt.X > Self.Left) or TMath.SameValue(Pt.X, Self.Left)) and
            (Pt.X < Self.Right) and
            ((Pt.Y > Self.Top) or TMath.SameValue(Pt.Y, Self.Top)) and
            (Pt.Y < Self.Bottom);
end;

class function TFloatRect.Empty: TFloatRect;
begin
  Result := TFloatRect.Create(0,0,0,0);
end;

function TFloatRect.GetHeight: Float;
begin
  Result := Self.Bottom - Self.Top;
end;

procedure TFloatRect.SetHeight(const Value: Float);
begin
  Self.Bottom := Self.Top + Value;
end;

function TFloatRect.GetWidth: Float;
begin
  Result := Self.Right - Self.Left;
end;

procedure TFloatRect.SetWidth(const Value: Float);
begin
  Self.Right := Self.Left + Value;
end;

function TFloatRect.GetSize: TVector;
begin
  Result := TVector.Make(Width, Height);
end;

procedure TFloatRect.SetSize(const Value: TVector);
begin
  Width := Value.X;
  Height := Value.Y;
end;

procedure TFloatRect.Inflate(const DX, DY: Float);
var
  v: TVector;
begin
  v := TVector.Make(DX, DY);
  TopLeft := TopLeft + v;
  BottomRight := BottomRight - v;
end;

procedure TFloatRect.Inflate(const DL, DT, DR, DB: Float);
begin
  TopLeft := TopLeft + TVector.Make(-DL, -DT);
  BottomRight := BottomRight + TVector.Make(DR, DB);
end;

procedure TFloatRect.Offset(const Point: TVector);
begin
  TopLeft := TopLeft + Point;
  BottomRight := BottomRight + Point;
end;

procedure TFloatRect.Offset(const DX, DY: Float);
begin
  TopLeft := TopLeft + TVector.Make(DX, DY);
  BottomRight := BottomRight + TVector.Make(DX, DY);
end;

function TFloatRect.GetLocation: TVector;
begin
  Result := TopLeft;
end;

procedure TFloatRect.SetLocation(const Point: TVector);
begin
  Offset(Point.X - Left, Point.Y - Top);
end;

procedure TFloatRect.SetLocation(const X, Y: Float);
begin
  Offset(X - Left, Y - Top);
end;

function TFloatRect.IntersectsWith(const R: TFloatRect): Boolean;
begin
  Result := not ( (Self.BottomRight.X < R.TopLeft.X) or
                  (Self.BottomRight.Y < R.TopLeft.Y) or
                  (R.BottomRight.X < Self.TopLeft.X) or
                  (R.BottomRight.Y < Self.TopLeft.Y) );
end;

function TFloatRect.IsEmpty: Boolean;
begin
  Result := (Right < Left) or TMath.SameValue(Right, Left)
         or (Bottom < Top) or TMath.SameValue(Bottom, Top);
end;

procedure TFloatRect.NormalizeRect;
var
  temp: Float;
begin
  if Top > Bottom then begin
    temp := Top;
    Top := Bottom;
    Bottom := temp;
  end;
  if Left > Right then begin
    temp := Left;
    Left := Right;
    Right := temp;
  end
end;

class function TFloatRect.Intersect(const R1, R2: TFloatRect): TFloatRect;
begin
  Result := R1;
  if R2.Left > R1.Left then Result.Left := R2.Left;
  if R2.Top > R1.Top then Result.Top := R2.Top;
  if R2.Right < R1.Right then Result.Right := R2.Right;
  if R2.Bottom < R1.Bottom then Result.Bottom := R2.Bottom;
  if Result.IsEmpty then begin
    Result.Top := 0.0;
    Result.Bottom := 0.0;
    Result.Left := 0.0;
    Result.Right := 0.0;
  end;
end;

procedure TFloatRect.Intersect(const R: TFloatRect);
begin
  Self := Intersect(Self, R);
end;

class function TFloatRect.Union(const R1, R2: TFloatRect): TFloatRect;
begin
  Result := R1;
  if not R2.IsEmpty then
  begin
    if R2.Left < R1.Left then Result.Left := R2.Left;
    if R2.Top < R1.Top then Result.Top := R2.Top;
    if R2.Right > R1.Right then Result.Right := R2.Right;
    if R2.Bottom > R1.Bottom then Result.Bottom := R2.Bottom;
  end;
  if Result.IsEmpty then begin
    Result.Top :=0.0;
    Result.Bottom := 0.0;
    Result.Left := 0.0;
    Result.Right := 0.0;
  end;
end;

procedure TFloatRect.Union(const R: TFloatRect);
begin
  Self := TFloatRect.Union(Self, R);
end;

class function TFloatRect.Union(const Points: Array of TVector): TFloatRect;
var
  I: Integer;
  TLCorner, BRCorner: TVector;
begin
  if Length(Points) > 0 then
  begin
    TLCorner := Points[Low(Points)];
    BRCorner := Points[Low(Points)];

    if Length(Points) > 1 then
    begin
      for I := Low(Points) + 1 to High(Points) do
      begin
        if Points[I].X < TLCorner.X then TLCorner.X := Points[I].X;
        if Points[I].X > BRCorner.X then BRCorner.X := Points[I].X;
        if Points[I].Y < TLCorner.Y then TLCorner.Y := Points[I].Y;
        if Points[I].Y > BRCorner.Y then BRCorner.Y := Points[I].Y;
      end;
    end;

    Result := TFloatRect.Create(TLCorner, BRCorner);
  end
  else begin
    Result := TFloatRect.Empty;
  end;
end;

end.
