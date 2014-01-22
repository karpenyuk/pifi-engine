unit uInterpolators;

interface

uses Math, uMath, uVMath, uPersistentClasses, uBaseTypes;

type
  TAnimation = class (TPersistentResource)
  private
    FEndState: TPlaybackState;
    FState: TAnimationState;
    FStartTime: double;
    FLostTime: double;
    FDuration: double;
    FCurrentTime: double;

    FOnAnimationFinish: TAnimationEventProc;
    FOnAnimationStop: TAnimationEventProc;
    FOnAnimationStart: TAnimationEventProc;
    procedure SetOnAnimationFinish(const Value: TAnimationEventProc);
    procedure SetOnAnimationStart(const Value: TAnimationEventProc);
    procedure SetOnAnimationStop(const Value: TAnimationEventProc);
    function getAnimationTime: single;
  public
    property EndState: TPlaybackState read FEndState;
    property State: TAnimationState read FState;
    property Time: single read getAnimationTime;
    property OnAnimationStart: TAnimationEventProc read FOnAnimationStart write SetOnAnimationStart;
    property OnAnimationStop: TAnimationEventProc read FOnAnimationStop write SetOnAnimationStop;
    property OnAnimationFinish: TAnimationEventProc read FOnAnimationFinish write SetOnAnimationFinish;

    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure Rewind;
  end;

  TEasingFunctions = class
    // t: current time, b: begInnIng value, c: change In value
    class function linear(t, b, c: single): single; static;

    class function easeInQuad(t, b, c: single): single; static;
    class function easeOutQuad(t, b, c: single): single; static;
    class function easeInOutQuad(t, b, c: single): single; static;

    class function easeInCubic(t, b, c: single): single; static;
    class function easeOutCubic(t, b, c: single): single; static;
    class function easeInOutCubic(t, b, c: single): single; static;

    class function easeInQuart(t, b, c: single): single; static;
    class function easeOutQuart(t, b, c: single): single; static;
    class function easeInOutQuart(t, b, c: single): single; static;

    class function easeInQuint(t, b, c: single): single; static;
    class function easeOutQuint(t, b, c: single): single; static;
    class function easeInOutQuint(t, b, c: single): single; static;

    class function easeInSine(t, b, c: single): single; static;
    class function easeOutSine(t, b, c: single): single; static;
    class function easeInOutSine(t, b, c: single): single; static;

    class function easeInExpo(t, b, c: single): single; static;
    class function easeOutExpo(t, b, c: single): single; static;
    class function easeInOutExpo(t, b, c: single): single; static;

    class function easeInCirc(t, b, c: single): single; static;
    class function easeOutCirc(t, b, c: single): single; static;
    class function easeInOutCirc(t, b, c: single): single; static;

    class function easeInElastic(t, b, c: single): single; static;
    class function easeOutElastic(t, b, c: single): single; static;
    class function easeInOutElastic(t, b, c: single): single; static;

    class function easeInBack(t, b, c: single): single; static;
    class function easeOutBack(t, b, c: single): single; static;
    class function easeInOutBack(t, b, c: single): single; static;

    class function easeInBounce(t, b, c: single): single; static;
    class function easeOutBounce(t, b, c: single): single; static;
    class function easeInOutBounce(t, b, c: single): single; static;
  end;


  TAbstractInterpolator<T> = class(TPersistentResource)
  protected
    FSourceValue: T;
    FDestValue: T;
    FDistance: T;
    FEasingType: TEasingType;
    FAnimation: TAnimation;
    procedure SetAnimation(const Value: TAnimation);
    procedure setSourceValue(Value: T); virtual;
    procedure setDestValue(Value: T); virtual;
    procedure UpdateDistance; virtual; abstract;
  public
    constructor Create; override;
    function GetValue(t: single): T; overload; virtual; abstract;
    function GetValue: T; overload; virtual;
    property Animation: TAnimation read FAnimation write SetAnimation;
    property SourceValue: T read FSourceValue write setSourceValue;
    property DestValue: T read FDestValue write setDestValue;
  end;

  TConstantInterpolator<T> = class(TAbstractInterpolator<T>)
  end;

  TFloatInterpolator = class(TAbstractInterpolator<single>)
  protected
    procedure UpdateDistance; override;
  public
    function GetValue(t: single): single; override;
  end;

  TVectorInterpolator = class(TAbstractInterpolator<TVector>)
  private
    FInterp: array[0..3] of TFloatInterpolator;
  protected
    procedure setSourceValue(Value: TVector); override;
    procedure setDestValue(Value: TVector); override;
    procedure UpdateDistance; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetValue(t: single): TVector; override;
  end;

  ICustomValue = interface(IInterface)
    function getValuePtr: pointer;
    property Ptr: pointer read getValuePtr;
  end;

  TCustomValue<T: Record> = class(TInterfacedObject, ICustomValue)
  protected
    FValue: T;
    function getValuePtr: pointer; virtual;
    function getValue: T; virtual;
    procedure setValue(const aValue: T); virtual;
  public
    property Ptr: pointer read getValuePtr;
    property Value: T read getValue write setValue;
  end;

  TFloatValue = class(TCustomValue<single>);
  TIntValue = class(TCustomValue<Integer>);
  TVectorValue = class(TCustomValue<TVector>)
  protected
    function getValuePtr: pointer; override;
  end;

implementation

{ TFloatInterpolator }

function TFloatInterpolator.GetValue(t: single): single;
var ft: single;
begin
  ft:=TMath.clamp(t,0,1);
  with TEasingFunctions do
  case FEasingType of
    etLinear: result:=linear(ft,FSourceValue,FDistance);
    etInQuad: result:=easeInQuad(ft,FSourceValue,FDistance);
    etOutQuad: result:=easeOutQuad(ft,FSourceValue,FDistance);
    etInOutQuad: result:=easeInOutQuad(ft,FSourceValue,FDistance);
    etInCubic: result:=easeInCubic(ft,FSourceValue,FDistance);
    etOutCubic: result:=easeOutCubic(ft,FSourceValue,FDistance);
    etInOutCubic: result:=easeInOutCubic(ft,FSourceValue,FDistance);
    etInQuart: result:=easeInQuart(ft,FSourceValue,FDistance);
    etOutQuart: result:=easeOutQuart(ft,FSourceValue,FDistance);
    etInOutQuart: result:=easeInOutQuart(ft,FSourceValue,FDistance);
    etInQuint: result:=easeInQuint(ft,FSourceValue,FDistance);
    etOutQuint: result:=easeOutQuint(ft,FSourceValue,FDistance);
    etInOutQuint: result:=easeInOutQuint(ft,FSourceValue,FDistance);
    etInSine: result:=easeInSine(ft,FSourceValue,FDistance);
    etOutSine: result:=easeOutSine(ft,FSourceValue,FDistance);
    etInOutSine: result:=easeInOutSine(ft,FSourceValue,FDistance);
    etInExpo: result:=easeInExpo(ft,FSourceValue,FDistance);
    etOutExpo: result:=easeOutExpo(ft,FSourceValue,FDistance);
    etInOutExpo: result:=easeInOutExpo(ft,FSourceValue,FDistance);
    etInCirc: result:=easeInCirc(ft,FSourceValue,FDistance);
    etOutCirc: result:=easeOutCirc(ft,FSourceValue,FDistance);
    etInOutCirc: result:=easeInOutCirc(ft,FSourceValue,FDistance);
    etInElastic: result:=easeInElastic(ft,FSourceValue,FDistance);
    etOutElastic: result:=easeOutElastic(ft,FSourceValue,FDistance);
    etInOutElastic: result:=easeInOutElastic(ft,FSourceValue,FDistance);
    etInBack: result:=easeInBack(ft,FSourceValue,FDistance);
    etOutBack: result:=easeOutBack(ft,FSourceValue,FDistance);
    etInOutBack: result:=easeInOutBack(ft,FSourceValue,FDistance);
    etInBounce: result:=easeInBounce(ft,FSourceValue,FDistance);
    etOutBounce: result:=easeOutBounce(ft,FSourceValue,FDistance);
    etInOutBounce: result:=easeInOutBounce(ft,FSourceValue,FDistance);
  else result:=linear(ft,FSourceValue,FDistance);
  end;
end;

procedure TFloatInterpolator.UpdateDistance;
begin
  FDistance:=FDestValue - FSourceValue;
end;

{ TAbstractInterpolator<T> }

constructor TAbstractInterpolator<T>.Create;
begin
  inherited Create;
  FEasingType:= etLinear;
end;

procedure TAbstractInterpolator<T>.setSourceValue(Value: T);
begin
  FSourceValue:=Value;
  UpdateDistance;
end;

function TAbstractInterpolator<T>.GetValue: T;
begin
  assert(assigned(FAnimation), 'Animation not assigned for interpolator');
  result:=GetValue(FAnimation.Time);
end;

procedure TAbstractInterpolator<T>.SetAnimation(const Value: TAnimation);
begin
  FAnimation := Value;
end;

procedure TAbstractInterpolator<T>.setDestValue(Value: T);
begin
  FDestValue:=Value;
  UpdateDistance;
end;

{ TEasingFunctions }

class function TEasingFunctions.easeInBack(t, b, c: single): single;
const s: single = 1.70158;
begin
  result := c*t*t*((s+1)*t - s) + b;
end;

class function TEasingFunctions.easeInBounce(t, b, c: single): single;
begin
  result := c - easeOutBounce (1-t, 0, c) + b;
end;

class function TEasingFunctions.easeInCirc(t, b, c: single): single;
begin
  result := -c * (sqrt(1 - t*t) - 1) + b;
end;

class function TEasingFunctions.easeInCubic(t, b, c: single): single;
begin
  result := c*t*t*t + b;
end;

class function TEasingFunctions.easeInElastic(t, b, c: single): single;
var ft,s,p,a: single;
begin
  p:=0.3; a:=c;
  if (t=0) then result := b
  else if (t=1) then result := b+c else begin
    if (a < abs(c)) then begin a:=c; s:=p/4; end
    else s := p/(2*PI) * arcsin (c/a);
    ft:=t-1;
    result := -(a*power(2,10*ft) * sin( (ft-s)*(2*PI)/p )) + b;
  end;
end;

class function TEasingFunctions.easeInExpo(t, b, c: single): single;
begin
  if t=0 then result := b else result := c*power(2, 10 * (t - 1)) + b;
end;

class function TEasingFunctions.easeInOutBack(t, b, c: single): single;
const s: single = 1.70158*1.525;
var ft: single;
begin
  ft:=t*2;
  if (ft < 1) then
    result := c/2*(ft*ft*((s+1)*ft - s)) + b
  else begin
    ft:=ft-2;
    result := c/2*(ft*ft*((s+1)*ft + s) + 2) + b;
  end;
end;

class function TEasingFunctions.easeInOutBounce(t, b, c: single): single;
begin
  if (t < 0.5) then result := easeInBounce (t*2, 0, c) * 0.5 + b
  else result := easeOutBounce (t*2-1, 0, c) * 0.5 + c*0.5 + b;
end;

class function TEasingFunctions.easeInOutCirc(t, b, c: single): single;
var ft: single;
begin
  ft:=t*2;
  if (ft < 1) then result := -c/2 * (sqrt(1 - ft*ft) - 1) + b
  else begin
    ft:=ft-2;
    result := c/2 * (sqrt(1 - ft*ft) + 1) + b;
  end;
end;

class function TEasingFunctions.easeInOutCubic(t, b, c: single): single;
var ft: single;
begin
  ft:=t*2;
  if (ft < 1) then result := c/2*ft*ft*ft + b
  else begin
    ft:=ft-2;
    result := c/2*(ft*ft*ft + 2) + b;
  end;
end;

class function TEasingFunctions.easeInOutElastic(t, b, c: single): single;
var ft,s,p,a: single;
begin
  p:=0.3*1.5; a:=c;
  if (t=0) then result := b else begin
    ft:=t*2;
    if (ft=2) then result := b+c
    else begin
      if (a < abs(c)) then begin a:=c; s:=p/4; end else s := p/(2*PI) * arcsin (c/a);
      if (ft < 1) then begin
        ft:=ft-1;
        result := -0.5*(a*power(2,10*ft) * sin( (ft-s)*(2*PI)/p )) + b
      end else begin
        ft:=ft-1;
        result := a*power(2,-10*ft) * sin( (ft-s)*(2*PI)/p )*0.5 + c + b;
      end;
    end;
  end;
end;

class function TEasingFunctions.easeInOutExpo(t, b, c: single): single;
var ft: single;
begin
  if (t=0) then result := b
  else if (t=1) then result := b+c
  else begin
    ft:=t*2;
    if (ft < 1) then result := c/2 * power(2, 10 * (ft - 1)) + b
    else result := c/2 * (-power(2, -10 * (ft-1)) + 2) + b;
  end;
end;

class function TEasingFunctions.easeInOutQuad(t, b, c: single): single;
var ft: single;
begin
  ft:=t*2;
  if (ft < 1) then result := c/2*ft*ft + b
  else result := -c/2 * ((ft-1)*(ft-3) - 1) + b;
end;

class function TEasingFunctions.easeInOutQuart(t, b, c: single): single;
var ft: single;
begin
  ft:=t*2;
  if (ft < 1) then result := c/2*ft*ft*ft*ft + b
  else begin
    ft:=ft-2;
    result := -c/2 * (ft*ft*ft*ft - 2) + b;
  end;
end;

class function TEasingFunctions.easeInOutQuint(t, b, c: single): single;
var ft: single;
begin
  ft:=t*2;
  if (ft < 1) then result := c/2*ft*ft*ft*ft*ft + b
  else begin
    ft:=ft-2;
    result := c/2*(ft*ft*ft*ft*ft + 2) + b;
  end;
end;

class function TEasingFunctions.easeInOutSine(t, b, c: single): single;
begin
  result := -c/2 * (cos(PI*t) - 1) + b;
end;

class function TEasingFunctions.easeInQuad(t, b, c: single): single;
begin
  result := c*t*t + b;
end;

class function TEasingFunctions.easeInQuart(t, b, c: single): single;
begin
  result := c*t*t*t*t + b;
end;

class function TEasingFunctions.easeInQuint(t, b, c: single): single;
begin
  result := c*t*t*t*t*t + b;
end;

class function TEasingFunctions.easeInSine(t, b, c: single): single;
begin
  result := -c * cos(t * PI/2) + c + b;
end;

class function TEasingFunctions.easeOutBack(t, b, c: single): single;
const s: single = 1.70158;
var ft: single;
begin
  ft:=t-1;
  result := c*(ft*ft*((s+1)*ft + s) + 1) + b;
end;

class function TEasingFunctions.easeOutBounce(t, b, c: single): single;
var ft: single;
begin
  if (t < (1/2.75)) then begin
    result := c*(7.5625*t*t) + b;
  end else if (t < (2/2.75)) then begin
    ft:=t-(1.5/2.75);
    result := c*(7.5625*ft*ft + 0.75) + b;
  end else if (t < (2.5/2.75)) then begin
    ft:=t-(2.25/2.75);
    result := c*(7.5625*ft*ft + 0.9375) + b;
  end else begin
    ft:=t-(2.625/2.75);
    result := c*(7.5625*ft*ft + 0.984375) + b;
  end;
end;

class function TEasingFunctions.easeOutCirc(t, b, c: single): single;
begin
  result := c * sqrt(1 - (t-1)*(t-1)) + b;
end;

class function TEasingFunctions.easeOutCubic(t, b, c: single): single;
var ft: single;
begin
  ft:=t-1;
  result := c*(ft*ft*ft + 1) + b;
end;

class function TEasingFunctions.easeOutElastic(t, b, c: single): single;
var s,p,a: single;
begin
  p:=0.3; a:=c;
  if (t=0) then result := b
  else if (t=1) then result := b+c
  else begin
    if (a < abs(c)) then begin a:=c; s:=p/4; end else s := p/(2*PI) * arcsin (c/a);
    result := a*power(2,-10*t) * sin( (t-s)*(2*PI)/p ) + c + b;
  end;
end;

class function TEasingFunctions.easeOutExpo(t, b, c: single): single;
begin
  if t=1 then result := b+c else result := c*(-power(2, -10 * t) + 1) + b;
end;

class function TEasingFunctions.easeOutQuad(t, b, c: single): single;
begin
  result := -c *(t)*(t-2) + b;
end;

class function TEasingFunctions.easeOutQuart(t, b, c: single): single;
var ft: single;
begin
  ft:=t-1;
  result := -c * (ft*ft*ft*ft - 1) + b;
end;

class function TEasingFunctions.easeOutQuint(t, b, c: single): single;
var ft: single;
begin
  ft:=t-1;
  result := c*(ft*ft*ft*ft*ft + 1) + b;
end;

class function TEasingFunctions.easeOutSine(t, b, c: single): single;
begin
  result := c * sin(t * PI/2) + b;
end;

class function TEasingFunctions.linear(t, b, c: single): single;
begin
  result := b + c*t;
end;

{ TVectorInterpolator }

constructor TVectorInterpolator.Create;
var i: integer;
begin
  inherited;
  for i := 0 to high(FInterp) do FInterp[i]:=TFloatInterpolator.Create;
end;

destructor TVectorInterpolator.Destroy;
var i: integer;
begin
  for i := 0 to high(FInterp) do FInterp[i].Free;
  inherited;
end;

function TVectorInterpolator.GetValue(t: single): TVector;
var i: integer;
begin
  for i := 0 to high(FInterp) do result[i]:=FInterp[i].GetValue(t);
end;

procedure TVectorInterpolator.setDestValue(Value: TVector);
var i: integer;
begin
  inherited;
  for i := 0 to high(FInterp) do FInterp[i].DestValue:=Value[i];
end;

procedure TVectorInterpolator.setSourceValue(Value: TVector);
var i: integer;
begin
  inherited;
  for i := 0 to high(FInterp) do FInterp[i].SourceValue:=Value[i];
end;

procedure TVectorInterpolator.UpdateDistance;
begin
  FDistance:=FDestValue-FSourceValue;
end;

{ TCustomValue<T> }

function TCustomValue<T>.getValue: T;
begin
  result:=FValue;
end;

function TCustomValue<T>.getValuePtr: pointer;
begin
  result:=@FValue;
end;

procedure TCustomValue<T>.setValue(const aValue: T);
begin
  FValue:=aValue;
end;

{ TVectorValue }

function TVectorValue.getValuePtr: pointer;
begin
  result:=FValue.GetAddr;
end;

{ TAnimation }

function TAnimation.getAnimationTime: single;
begin
  result:=TMath.Clamp((FCurrentTime-FStartTime)/FDuration, 0.0, 1.0);
end;

procedure TAnimation.Pause;
begin

end;

procedure TAnimation.Play;
begin

end;

procedure TAnimation.Rewind;
begin

end;

procedure TAnimation.SetOnAnimationFinish(const Value: TAnimationEventProc);
begin
  FOnAnimationFinish := Value;
end;

procedure TAnimation.SetOnAnimationStart(const Value: TAnimationEventProc);
begin
  FOnAnimationStart := Value;
end;

procedure TAnimation.SetOnAnimationStop(const Value: TAnimationEventProc);
begin
  FOnAnimationStop := Value;
end;

procedure TAnimation.Stop;
begin

end;

end.

