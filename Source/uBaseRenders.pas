unit uBaseRenders;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, uBaseClasses, uBaseTypes, uRenderResource, uMiscUtils, uVMath,
  uPersistentClasses, uWorldSpace;

Type

  TRenderPurpose = (rpUnknown, rpTransformation, rpResourceLoader, rpMeshRender,
    rpTerrainRender, rpActorRender, rpGUIRender, rp2DRender, rpShapesRender,
    rpParticlesRender);
  TRenderPurposes = set of TRenderPurpose;

  TBaseRender = class;

  TBaseSubRender = class (TNotifiableObject)
  protected
    FOwner: TBaseRender;
    FSupportedResources: TList; //List of Render Resource Class types
    FRequiredAPIVersion: TApiVersion;
    FRenderPurpose: TRenderPurposes;
  public
    constructor Create;
    constructor CreateOwned(aRender: TBaseRender); virtual;
    destructor Destroy; override;

    function isSupported(const aClassType: TClass): boolean; virtual;
    procedure ProcessResource(const Resource: TBaseRenderResource); virtual; abstract;

    property Owner: TBaseRender read FOwner;
  end;

  TBaseRender = class  (TNotifiableObject)
  private
  protected
    FCurrentLightNumber: integer;
    FCurrentGraph: TSceneGraph;
    FRegisteredSubRenders: TList;  //List of TBaseSubRender
    procedure UploadResource(const Res: TBaseRenderResource); virtual; abstract;
    procedure ProcessResource(const Res: TBaseRenderResource); virtual; abstract;
//    procedure ProcessMeshObjects(const aMeshObjects: TMeshObjectsList); virtual; abstract;

  public
    function UpdateWorldMatrix(const MovableObject: TMovableObject;
      UseMatrix: TTransforms=[ttAll]): TMatrix; virtual;
    function CheckVisibility(const aFrustum: TFrustum;
      const aExtents: TExtents): boolean; virtual;
    function isSupported(const aClassType: TClass): boolean; overload; virtual;
    function isSupported(const aAPI: TApiVersion): boolean; overload; virtual;

    procedure ProcessScene(const aScene: TSceneGraph); virtual; abstract;

    constructor Create;
    destructor Destroy; override;

    procedure RegisterSubRender(const SubRender: TBaseSubRender); virtual;

    // Rendering states
    property CurrentGraph: TSceneGraph read FCurrentGraph;
    property CurrentLightNumber: integer read FCurrentLightNumber;
  end;

  TRegisteredRenders = class (TNotifiableObject)
  private
    FRenders: TList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterRender(const aRender: TBaseRender);
    procedure UnRegisterRender(const aRender: TBaseRender);

    function GetCompatibleRender(aAPI: TApiVersion): TBaseRender;
  end;

var
  vRegisteredRenders: TRegisteredRenders;

implementation

{ TBaseRender }

function TBaseRender.CheckVisibility(const aFrustum: TFrustum;
  const aExtents: TExtents): boolean;
begin
  result:=true;
end;

constructor TBaseRender.Create;
begin
  inherited Create;
  FRegisteredSubRenders:=TList.Create;
end;

destructor TBaseRender.Destroy;
begin
  FreeObjectList(FRegisteredSubRenders);
  inherited;
end;

function TBaseRender.isSupported(const aAPI: TApiVersion): boolean;
begin
  result:=false;
end;

function TBaseRender.isSupported(const aClassType: TClass): boolean;
var i: integer;
begin
  result:=false;
  for i:=0 to FRegisteredSubRenders.Count-1 do begin
    if TBaseSubRender(FRegisteredSubRenders[i]).isSupported(aClassType)
    then result:=true; exit;
  end;
end;

procedure TBaseRender.RegisterSubRender(const SubRender: TBaseSubRender);
begin
  FRegisteredSubRenders.Add(SubRender);
end;

function TBaseRender.UpdateWorldMatrix(const MovableObject: TMovableObject; UseMatrix: TTransforms): TMatrix;
var wm, pm: TMatrix;
    hasParent: boolean;
begin

  hasParent := false;
  if (MovableObject.Parent<>nil) and ((ttParent in UseMatrix) or (ttAll in UseMatrix)) then begin
   if not MovableObject.Parent.WorldMatrixUpdated then
     MovableObject.parent.UpdateWorldMatrix;
   pm:=MovableObject.parent.WorldMatrix;
   hasParent := true;
  end;

  wm.SetIdentity;
  if (MovableObject.Parent<>nil) and ((ttParent in UseMatrix) or (ttAll in UseMatrix)) then begin
     if not MovableObject.Parent.WorldMatrixUpdated then
       MovableObject.parent.UpdateWorldMatrix;
     wm:=MovableObject.parent.WorldMatrix; wm:=wm * MovableObject.ModelMatrix;
  end else wm := MovableObject.ModelMatrix;

  if (not (ttModel in UseMatrix)) and (not(ttAll in UseMatrix)) then wm.SetIdentity;

  if (ttScale in UseMatrix) or (ttAll in UseMatrix) then wm := wm * MovableObject.ScaleMatrix;
  if (ttRotation in UseMatrix) or (ttAll in UseMatrix) then wm := wm * MovableObject.RotationMatrix;
  if (ttPosition in UseMatrix) or (ttAll in UseMatrix) then wm := wm * MovableObject.TranslationMatrix;

  if hasParent then
    wm:=wm * pm;
  result:= wm;
end;

{ TBaseSubRender }

constructor TBaseSubRender.Create;
begin
  Assert(False, 'Must be used CreateOwned instead Create!');
end;

constructor TBaseSubRender.CreateOwned(aRender: TBaseRender);
begin
  inherited Create;
  FOwner := aRender;
  FSupportedResources:=TList.Create;
  FRenderPurpose:=[rpUnknown];
end;

destructor TBaseSubRender.Destroy;
begin
  FSupportedResources.Free;
  inherited;
end;

function TBaseSubRender.isSupported(const aClassType: TClass): boolean;
begin
  if FSupportedResources.IndexOf(aClassType)>=0
  then result:=true else result:=false;
end;

{ TRegisteredRenders }

constructor TRegisteredRenders.Create;
begin
  inherited;
  FRenders:=TList.Create;
end;

destructor TRegisteredRenders.Destroy;
begin
  //FreeObjectList(FRenders);
  FRenders.Free;
  inherited;
end;

function TRegisteredRenders.GetCompatibleRender(aAPI: TApiVersion): TBaseRender;
var i: integer;
    Render: TBaseRender;
begin
  for i:=0 to FRenders.Count-1 do begin
    Render:=TBaseRender(FRenders[i]);
    if Render.isSupported(aAPI) then begin
      result:=Render; exit;
    end;
  end; result:=nil;
end;

procedure TRegisteredRenders.RegisterRender(const aRender: TBaseRender);
begin
  if FRenders.indexof(aRender)<0 then FRenders.Add(aRender);
end;


procedure TRegisteredRenders.UnRegisterRender(const aRender: TBaseRender);
var i: integer;
begin
  i:=FRenders.indexof(aRender);
  if i<0 then FRenders.Delete(i);
end;

initialization

  vRegisteredRenders:=TRegisteredRenders.Create;

finalization

  vRegisteredRenders.Free;

end.
