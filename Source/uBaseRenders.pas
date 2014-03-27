unit uBaseRenders;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$POINTERMATH ON}

interface

uses
  Classes, uBaseClasses, uBaseTypes, uRenderResource, uGenericsRBTree,
  uMiscUtils, uVMath, uPersistentClasses, uWorldSpace;

Type

  TRenderPurpose = (rpUnknown, rpTransformation, rpResourceLoader, rpMeshRender,
    rpTerrainRender, rpActorRender, rpGUIRender, rp2DRender, rpShapesRender,
    rpParticlesRender);
  TRenderPurposes = set of TRenderPurpose;

  TBaseRender = class;
  TBaseSubRender = class;

  TBaseGraphicResource = class(TPersistentResource)
  public
    constructor Create; override;
    constructor CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource); overload; virtual; abstract;
    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;
    procedure Update(aRender: TBaseRender); virtual;
    procedure Apply(aRender: TBaseRender); virtual;
    procedure UnApply(aRender: TBaseRender); virtual;
  end;
  TGraphicResourceClass = class of TBaseGraphicResource;


  TResourceClasses = GRedBlackTree<TRenderResourceClass, TGraphicResourceClass>;

  TBaseSubRender = class (TPersistentResource)
  protected
    FSupportedResources: TResourceClasses;
    FRequiredAPIVersion: TApiVersion;
    FRenderPurpose: TRenderPurposes;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddSupporting(aResource: TRenderResourceClass; aGraphic: TGraphicResourceClass);
    function IsSupported(aResource: TRenderResourceClass; aGraphic: TGraphicResourceClass): boolean; virtual;
    procedure ProcessResource(aResource: TBaseGraphicResource); virtual; abstract;
  end;

  TBaseRender = class(TPersistentResource)
  private
  protected
    FCurrentLightNumber: integer;
    FCurrentGraph: TSceneGraph;
    FCurrentCamera: TSceneCamera;
    FCurrentSceneObject: TSceneObject;
    FRegisteredSubRenders: TList;  //List of TBaseSubRender
    procedure UploadResource(const Res: TBaseRenderResource); virtual; abstract;
    procedure ProcessResource(const Res: TBaseRenderResource); virtual; abstract;
  public
    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;

    function CheckVisibility(const aFrustum: TFrustum;
      const aExtents: TExtents): boolean; virtual;
    function IsSupported(aResource: TRenderResourceClass; aGraphic: TGraphicResourceClass): boolean; overload; virtual;
    function IsSupported(const aAPI: TApiVersion): boolean; overload; virtual;

    procedure ProcessScene(const aScene: TSceneGraph); virtual; abstract;

    constructor Create; override;
    destructor Destroy; override;

    procedure RegisterSubRender(const SubRender: TBaseSubRender); virtual;
    procedure UnRegisterSubRender(const SubRender: TBaseSubRender); virtual;

    // Rendering states
    property CurrentGraph: TSceneGraph read FCurrentGraph;
    property CurrentCamera: TSceneCamera read FCurrentCamera;
    property CurrentSceneObject: TSceneObject read FCurrentSceneObject;
    property CurrentLightNumber: integer read FCurrentLightNumber;
  end;

  TRegisteredRenders = class (TNotifiableObject)
  private
    FRenders: TList;
  public
    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;

    constructor Create; override;
    destructor Destroy; override;

    procedure RegisterRender(const aRender: TBaseRender);
    procedure UnRegisterRender(const aRender: TBaseRender);

    function GetCompatibleRender(aAPI: TApiVersion): TBaseRender;
  end;

var
  vRegisteredRenders: TRegisteredRenders;

implementation

{ TGLBaseResource }

procedure TBaseGraphicResource.Apply(aRender: TBaseRender);
begin
  // Do nothing
end;

constructor TBaseGraphicResource.Create;
begin
  inherited Create;
  Owner := nil;
end;

procedure TBaseGraphicResource.Notify(Sender: TObject; Msg: Cardinal;
  Params: pointer);
begin
  case msg of
    NM_ResourceApply: Apply(TBaseRender(Params));
    NM_ResourceUnApply: UnApply(TBaseRender(Params));
    NM_ResourceUpdate: Update(TBaseRender(Params));
  end;

  inherited;
end;

procedure TBaseGraphicResource.UnApply(aRender: TBaseRender);
begin
  // Do nothing
end;

procedure TBaseGraphicResource.Update(aRender: TBaseRender);
begin
  // Do nothing
end;

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

procedure TBaseRender.Notify(Sender: TObject; Msg: Cardinal; Params: pointer);
begin
  inherited;
  if Sender is TBaseSubRender then begin
    case Msg of
      NM_ResourceChanged: UnRegisterSubRender(TBaseSubRender(Sender));
    end;
  end;
end;

function TBaseRender.IsSupported(aResource: TRenderResourceClass; aGraphic: TGraphicResourceClass): boolean;
var i: integer;
begin
  result:=false;
  for i:=0 to FRegisteredSubRenders.Count-1 do begin
    if TBaseSubRender(FRegisteredSubRenders[i]).IsSupported(aResource, aGraphic)
      then exit(true);
  end;
end;

procedure TBaseRender.RegisterSubRender(const SubRender: TBaseSubRender);
begin
  FRegisteredSubRenders.Add(SubRender);
  SubRender.Subscribe(Self);
end;

procedure TBaseRender.UnRegisterSubRender(const SubRender: TBaseSubRender);
var i: integer;
begin
  i := FRegisteredSubRenders.IndexOf(SubRender);
  if i>=0 then FRegisteredSubRenders.Delete(i);
end;

{ TBaseSubRender }

procedure TBaseSubRender.AddSupporting(aResource: TRenderResourceClass;
  aGraphic: TGraphicResourceClass);
begin
  FSupportedResources.Add(aResource, aGraphic);
end;

function ClassComparer(const Item1, Item2: TRenderResourceClass): Integer;
begin
  if Item1.ClassInfo = Item2.ClassInfo then
    exit(0)
  else if UIntPtr(Item1.ClassInfo) > UIntPtr(Item2.ClassInfo) then
    exit(1)
  else
    exit(-1);
end;

constructor TBaseSubRender.Create;
begin
  inherited Create;
  FSupportedResources := TResourceClasses.Create(ClassComparer, nil);
  FRenderPurpose:=[rpUnknown];
end;

destructor TBaseSubRender.Destroy;
begin
  FSupportedResources.Free;
  inherited;
end;

function TBaseSubRender.IsSupported(aResource: TRenderResourceClass; aGraphic: TGraphicResourceClass): boolean;
var
  grClass: TGraphicResourceClass;
begin
  grClass := nil;
  Result := FSupportedResources.Find(aResource, grClass) and (grClass = aGraphic);
end;

{ TRegisteredRenders }

constructor TRegisteredRenders.Create;
begin
  inherited;
  FRenders:=TList.Create;
end;

destructor TRegisteredRenders.Destroy;
begin
  FreeObjectList(FRenders);
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

procedure TRegisteredRenders.Notify(Sender: TObject; Msg: Cardinal;
  Params: pointer);
begin
  inherited;
  if Sender is TBaseRender then begin
    case Msg of
      NM_ResourceChanged: UnRegisterRender(TBaseRender(Sender));
    end;
  end;

end;

procedure TRegisteredRenders.RegisterRender(const aRender: TBaseRender);
begin
  if FRenders.indexof(aRender)<0 then begin
    FRenders.Add(aRender);
    aRender.Subscribe(Self);
  end;
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
