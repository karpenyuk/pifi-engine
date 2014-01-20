unit uMath;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Math;


const
  CEps = 1.-5;
  CEps2 = 1.0e-30;
  CEps3 = 1.0e-40;
  CPi = 3.14159265358979323;
  CPi2 = CPi * 2;
  CPiD2 = CPi / 2;
  CPiD3 = CPi / 3;
  CPiD4 = CPi / 4;
  CPiD6 = CPi / 6;
  CPiD180 = CPi / 180;
  CPiD360 = CPi / 360;


type
  Float = Single;
  Bool = Boolean;
  Long = Cardinal;
  Int = Integer;

  TMath = record
  public

    // Clamp = max( aMin, min( aMax, aValue ))
    class function Clamp( aValue,aMin,aMax: Float ): Float; static;

    // Delta = ( aCurr - aStart ) / ( aStop - aStart )
    class function Delta( aStart,aStop,aCurr: Float ): Float; static;

    // Lerp = aStart + ( aStop - aStart ) * aDelta
    class function Lerp( aStart,aStop,aDelta: Float ): Float; static;
    class function LerpSmooth(
      aStart,aStop,aDelta: Float ): Float; static;
    class function LerpSin(
      aStart,aStop,aDelta: Float ): Float; static;
    class function LerpSinAlt(
      aStart,aStop,aDelta: Float ): Float; static;
    class function LerpTan(
      aStart,aStop,aDelta: Float ): Float; static;
    class function LerpPower( aStart,aStop,aDelta: Float;
      aDistortion: Float = 1 ): Float; static;
    class function LerpLn( aStart,aStop,aDelta: Float;
      aDistortion: Float = 1 ): Float; static;
    class function LerpExp( aStart,aStop,aDelta: Float;
      aDistortion: Float = 1 ): Float; static;
    class function LerpF1( aStart,aStop,aDelta: Float;
      aAbs: Boolean = false ): Float; static;
    class function LerpF2( aStart,aStop,aDelta: Float;
      aAbs: Boolean = false ): Float; static;
    class function LerpF3( aStart,aStop,aDelta: Float;
      aAbs: Boolean = false ): Float; static;

    // aVar1 <=> aVar2
    class procedure Swap( var aVar1,aVar2: Float ); static;

    class function Abs( aValue: Float ): Float; static;

    class procedure SinCos( aTheta: Float;
      out aSVar,aCVar: Float ); overload; static;
    class procedure SinCos( aTheta,aRadius: Float;
      out aSVar,aCVar: Float ); overload; static;
    class function Tan( aTheta: Float ): Float; static;
    class function ArcCos( aTheta: Float ): Float; static;
    class function DegToRad( aDegrees: Float ): Float; static;

    class function Min(a,b: Float): Float; overload; static;
    class function Max(a,b: Float): Float; overload; static;
    class function Min(a,b: Int): Int; overload; static;
    class function Max(a,b: Int): Int; overload; static;

    class function IsPowerOfTwo(a: Integer): Boolean; static;
  end;


implementation

//
// TMath.ArcCos
//
class function TMath.ArcCos(aTheta: Float): Float;
begin
   Result:= Math.ArcTan2(Sqrt(1 - Sqr(aTheta)), aTheta);
end;

//
// TMath.Clamp
//
class function TMath.Clamp( aValue,aMin,aMax: Float ): Float;
begin

  if aValue < aMin then result := aMin
    else if aValue > aMax then result := aMax
      else result := aValue;

end;

//
// TMath.DegToRad
//
class function TMath.DegToRad(aDegrees: Float): Float;
begin
   Result := aDegrees*(PI/180);
end;

//
// TMath.Delta
//
class function TMath.Delta( aStart,aStop,aCurr: Float ): Float;
begin

  result := ( aCurr - aStart ) / ( aStop - aStart );

end;


class function TMath.IsPowerOfTwo(a: Integer): Boolean;
var
  n: integer;
begin
  n := 0;
  while (a > 0) do
  begin
    if a and 1 = 1 then
      Inc(n);
    a := a shr 1;
  end;
  result := (n = 1);
end;

//
// TMath.Swap
//
class procedure TMath.Swap( var aVar1,aVar2: Float );
var
    f: Float;
begin

  f := aVar1;
  aVar1 := aVar2;
  aVar2 := f;

end;


//
// TMath.Abs
//
class function TMath.Abs( aValue: Float ): Float;
begin

  if aValue < 0 then
    result := - aValue
  else result := aValue;

end;


//
// TMath.Lerp
//
class function TMath.Lerp( aStart,aStop,aDelta: Float ): Float;
begin

  result := aStart + ( aStop - aStart ) * aDelta;

end;


//
// TMath.Norm
//
class function TMath.LerpSmooth( aStart,aStop,aDelta: Float ): Float;
begin

  result := Lerp( aStart, aStop,
    aDelta * ( 3 * aDelta - 2 * aDelta * aDelta ));

end;


//
// TMath.LerpSin
//
class function TMath.LerpSin( aStart,aStop,aDelta: Float ): Float;
begin

  result := Lerp( aStart, aStop, Sin( aDelta * CPiD2 ));

end;


//
// TMath.LerpSinAlt
//
class function TMath.LerpSinAlt( aStart,aStop,aDelta: Float ): Float;
begin

  result := Lerp( aStart, aStop, aDelta * Sin( aDelta * CPiD2 ));

end;


//
// TMath.LerpTan
//
class function TMath.LerpTan( aStart,aStop,aDelta: Float ): Float;
begin

  result := Lerp( aStart, aStop, Tan( aDelta * CPiD4 ));

end;

class function TMath.Max(a, b: Float): Float;
begin
  if a > b then Result := a
  else Result := b;
end;

class function TMath.Min(a, b: Float): Float;
begin
  if a < b then Result := a
  else Result := b;
end;

class function TMath.Max(a, b: Int): Int;
begin
  if a > b then Result := a
  else Result := b;
end;

class function TMath.Min(a, b: Int): Int;
begin
  if a < b then Result := a
  else Result := b;
end;

//
// TMath.LerpPower
//
class function TMath.LerpPower( aStart,aStop,aDelta: Float;
  aDistortion: Float = 1 ): Float;
begin

  if ( Frac( aDistortion ) <> 0 ) and ( aDelta < 0 ) then
    aDistortion := Round( aDistortion );

  Result := Lerp( aStart, aStop, Power( aDelta, aDistortion ));

end;


//
// TMath.LerpLn
//
class function TMath.LerpLn( aStart,aStop,aDelta: Float;
  aDistortion: Float = 1 ): Float;
begin

  result := Lerp( aStart, aStop,
    Ln( 1 + aDelta * aDistortion ) / Ln( 1 + aDistortion ));

end;


//
// TMath.LerpExp
//
class function TMath.LerpExp( aStart,aStop,aDelta: Float;
  aDistortion: Float = 1 ): Float;
var
    x: Float;
begin

  x := Exp( -aDistortion );
  result := Lerp( aStart, aStop, ( Power( x, 1 - aDelta ) - x ) / ( 1 - x ));

end;


//
// TMath.LerpF1
//
class function TMath.LerpF1( aStart,aStop,aDelta: Float;
  aAbs: Boolean = false ): Float;
begin

  result := LerpSmooth( aStart, aStop * 1.31, aDelta * 1.2 );

  if aAbs and ( result > 1 ) then
    result := 2 - result;

end;


//
// TMath.LerpF2
//
class function TMath.LerpF2( aStart,aStop,aDelta: Float;
  aAbs: Boolean = false ): Float;
begin

  aDelta := aDelta * aDelta * 11;
  result := Lerp( aStart, aStop, 1 - exp( -0.5 * aDelta ) * + cos( aDelta ));

  if aAbs and ( result > 1 ) then
    result := 2 - result;

end;


//
// TMath.LerpF3
//
class function TMath.LerpF3( aStart,aStop,aDelta: Float;
  aAbs: Boolean = false ): Float;
begin

  aDelta := aDelta * aDelta * 18;
  result := Lerp( aStart, aStop, 1 - exp( -0.3 * aDelta ) * cos( aDelta ));

  if aAbs and ( result > 1 ) then
    result := 2 - result;

end;


//
// TMath.SinCos
//
class procedure TMath.SinCos( aTheta: Float; out aSVar,aCVar: Float );
var
    s,c: Extended;
begin

  Math.SinCos( aTheta, s, c );
  aSVar := s;
  aCVar := c;

end;


//
// TMath.SinCos
//
class procedure TMath.SinCos( aTheta,aRadius: Float;
  out aSVar,aCVar: Float );
var
    s,c: Extended;
begin

  Math.SinCos( aTheta, s, c );
  aSVar := s * aRadius;
  aCVar := c * aRadius;

end;


//
// TMath.Tan
//
class function TMath.Tan( aTheta: Float ): Float;
begin

  result := Math.Tan( aTheta );

end;

end.
