unit uAI;

interface

uses
  uRenderResource;

type

  TRobotController = class
  private type TStatus = (rcsBaseDone, rcsForearmDone, rcsArmDone, rcsPalmDone);
  private
    FBase: TSceneObject;
    FHinge1: TSceneObject;
    FForearm: TSceneObject;
    FHinge2: TSceneObject;
    FArm: TSceneObject;
    FHinge3: TSceneObject;
    FPalm: TSceneObject;
    FFinger1: TSceneObject;
    FFinger2: TSceneObject;
    FStatus: set of TStatus;
    FWaitTime: Single;
    FBaseTurnDest: Single;
    FBaseTurnStep: Single;
    FForearmBendDest: Single;
    FForearmBendStep: Single;
    FArmBendDest: Single;
    FArmBendStep: Single;
    FPalmBendDest: Single;
    FPalmBendStep: Single;
  public
    constructor Create(aBase: TSceneObject);
    procedure Progress(aDeltaTime: Single);
  end;

implementation

uses Math;

{ TRobotController }

function RoundAngle(X: Single): Single;
begin
  Result := frac((360000 + X) / 360) * 360;
end;

function SignedStep(X: Single): Single;
begin
  Result := RoundAngle(X);
  if Result < 180
    then Result := 0.1
    else Result := -0.1;
end;

constructor TRobotController.Create(aBase: TSceneObject);
begin
  FBase := aBase;
  FHinge1 := FBase.Childs[0] as TSceneObject;
  FForearm := FHinge1.Childs[0] as TSceneObject;
  FHinge2 := FForearm.Childs[0] as TSceneObject;
  FArm :=  FHinge2.Childs[0] as TSceneObject;
  FHinge3 := FArm.Childs[0] as TSceneObject;
  FPalm := FHinge3.Childs[0] as TSceneObject;
  FFinger1 := FPalm.Childs[0] as TSceneObject;
  FFinger2 := FPalm.Childs[1] as TSceneObject;
  FStatus := [rcsBaseDone, rcsForearmDone, rcsArmDone, rcsPalmDone];
  FWaitTime := 1500;
end;

procedure TRobotController.Progress(aDeltaTime: Single);
var
  distance, sideC, alpha, betta, gamma: Single;
  delta: Single;

begin
  if FStatus <> [rcsBaseDone, rcsForearmDone, rcsArmDone, rcsPalmDone] then begin

    if not(rcsBaseDone in FStatus) then begin
      delta := FBaseTurnStep * aDeltaTime;
      if Sign(SignedStep(FBaseTurnDest - FBase.TurnAngle)) <> Sign(delta) then begin
        Include(FStatus, rcsBaseDone);
      end
      else FBase.TurnObject(delta);
    end;

    if not(rcsForearmDone in FStatus) then begin
      delta := FForearmBendStep * aDeltaTime;
      if Sign(SignedStep(FForearmBendDest - FHinge1.RollAngle)) <> Sign(delta) then begin
        Include(FStatus, rcsForearmDone);
      end
      else FHinge1.RollObject(delta);
    end;

    if not(rcsArmDone in FStatus) then begin
      delta := FArmBendStep * aDeltaTime;
      if Sign(SignedStep(FArmBendDest - FHinge2.RollAngle)) <> Sign(delta) then begin
        Include(FStatus, rcsArmDone);
      end
      else FHinge2.RollObject(delta);
    end;

    if not(rcsPalmDone in FStatus) then begin
      delta := FPalmBendStep * aDeltaTime;
      if Sign(SignedStep(FPalmBendDest - FHinge3.RollAngle)) <> Sign(delta) then begin
        Include(FStatus, rcsPalmDone);
      end
      else FHinge3.RollObject(delta);
    end;

  end
  else begin
    FWaitTime := FWaitTime + aDeltaTime;
    if FWaitTime > 1500 then
    begin
      FBaseTurnDest := FBase.TurnAngle + 180 + 90 * (Random - 0.5);
      FBaseTurnDest := RoundAngle(FBaseTurnDest);
      FBaseTurnStep := SignedStep(FBaseTurnDest - FBase.TurnAngle);
      distance := 3.345 + Random * 9.855;
      sideC := sqr(distance) + sqr(1.05);
      alpha := arcCos( (88.0625 - sideC) / 87.5 );  sideC := sqrt( sideC );
      betta := arcSin( 7 * sin( alpha ) / sideC );
      gamma := alpha + betta - arcCos( 1.05 / sideC);
      FForearmBendDest := RadToDeg( Pi / 2 - betta - arcSin( 1.05 / sideC ) );
      FForearmBendStep := SignedStep(FForearmBendDest - FHinge1.RollAngle);
      FArmBendDest := RadToDeg( Pi - alpha );
      FArmBendStep := SignedStep(FArmBendDest - FHinge2.RollAngle);
      FPalmBendDest := RadToDeg( gamma );
      FPalmBendStep := SignedStep(FPalmBendDest - FHinge3.RollAngle);
      FStatus := [];
      FWaitTime := 0;
    end;
  end;
end;

end.
