unit uWorldSpace;

interface

uses Classes, uMiscUtils, uBaseTypes, uBaseClasses, uPersistentClasses, uRenderResource;

type

  TSceneGraph = class(TNotifiableObject)
    // Решить вопрос со скриптовым рендером - добавить вместе с камерами в корень графа?
    // Или передавать их в рендер через ProcessScene(Scene, Camera, Target, Script)?
  private
    FRoot: TSceneObject;
    FLights: TLightsList;
    FMaterials: TMaterialList;
    FCameras: TCamerasList;

    function getItem(const aName: string): TBaseSceneItem;
    function getCount: integer;
    function getLight(Index: integer): TLightSource;
    function getLightCount: integer;
    function getMatCount: integer;
    function getMaterial(Index: integer): TMaterialObject;
    function getCamCount: integer;
    function getCamera(index: integer): TSceneCamera;
    function getMainCamera: TSceneCamera;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;

    function AddItem(aItem: TBaseSceneItem): integer;
    function AddLight(aLight: TLightSource): integer;
    function GetLights: TLightsList;

    function AddMaterial(aMat: TMaterialObject; aCapture: boolean=false): integer;
    function AddNewMaterial(aName: string): TMaterialObject;
    function GetMaterials: TMaterialList;

    property Items[const aName: string]: TBaseSceneItem read getItem; default;

    property Lights[index: integer]: TLightSource read getLight;
    property LightsCount: integer read getLightCount;

    property Materials[index: integer]: TMaterialObject read getMaterial;
    property MaterialsCount: integer read getMatCount;

    property Root: TSceneObject read FRoot;

    property Camera: TSceneCamera read getMainCamera;
    property Cameras[index: integer]: TSceneCamera read getCamera;
    property CamerasCount: integer read getCamCount;

    property Count: integer read getCount;
  end;

  TWorldSpace = class(TBaseRenderResource)
  private
    FCameras: TList; // List of Cameras
    FScripts: TList; // List of lua scripts?
    FGraphs: TList; // List of TSceneGraph
    FTimerActions: TList; // List of timer events

  end;

implementation


{ TSceneGraph }

function TSceneGraph.AddItem(aItem: TBaseSceneItem): integer;
begin
  result := FRoot.Childs.AddSceneItem(aItem);
  aItem.Subscribe(Self);
  Subscribe(aItem);
  //If aItem is Camera - add it to Camera List
  if (aItem is TSceneCamera) and (not FCameras.inList(aItem))
  then FCameras.AddCamera(aItem as TSceneCamera);
  //If aItem is Light - add it to Light List
  if (aItem is TLightSource) and (not FLights.inList(aItem))
  then FLights.AddLight(aItem as TLightSource);

end;

function TSceneGraph.AddLight(aLight: TLightSource): integer;
begin
  result := FLights.AddLight(aLight);
end;

function TSceneGraph.AddMaterial(aMat: TMaterialObject; aCapture: boolean): integer;
begin
  result := FMaterials.AddMaterial(aMat);
  if aCapture then aMat.Owner:=self;
end;

function TSceneGraph.AddNewMaterial(aName: string): TMaterialObject;
var
  aMat: TMaterialObject;
begin
  assert(not assigned(FMaterials.GetMaterial(aName)),
    'Material with name "'+aName+'" is exists in library!');
  aMat := TMaterialObject.Create;
  aMat.Name := aName;
  aMat.Owner:=FMaterials;
  FMaterials.AddMaterial(aMat);
  result := aMat;
end;

constructor TSceneGraph.Create;
var
  camera: TSceneCamera;
begin
  inherited Create;
  FRoot := TSceneObject.Create;
  FRoot.FriendlyName := 'Root';
  camera := TSceneCamera.Create;
  camera.Parent := FRoot;
  camera.FriendlyName := 'Main camera';
  FCameras := TCamerasList.Create;
  FCameras.AddCamera(camera);
  FMaterials:=TMaterialList.Create;
  FLights := TLightsList.Create;
end;

destructor TSceneGraph.Destroy;
begin
  FRoot.Free;
  Camera.Free;
  FCameras.Free;
  FMaterials.Free;
  FLights.Free;
  inherited;
end;

function TSceneGraph.getCamCount: integer;
begin
  result := FCameras.Count;
end;

function TSceneGraph.getCamera(index: integer): TSceneCamera;
begin
  result := FCameras[index];
end;

function TSceneGraph.getCount: integer;
begin
  result := FRoot.Childs.Count;
end;

function TSceneGraph.getItem(const aName: string): TBaseSceneItem;

  function DownToTree(aItem: TBaseSceneItem): TBaseSceneItem;
  var i: integer;
  begin
    if aItem.FriendlyName = aName then exit(aItem);
    for i := 0 to aItem.Childs.Count - 1 do begin
      Result := DownToTree(aItem.Childs[i]);
      if Assigned(Result) then exit;
    end;
    Result := nil;
  end;

begin
  result := DownToTree(FRoot);
end;

function TSceneGraph.getLight(Index: integer): TLightSource;
begin
  result := FLights[Index];
end;

function TSceneGraph.getLightCount: integer;
begin
  result := FLights.Count;
end;

function TSceneGraph.GetLights: TLightsList;
begin
  result := FLights;
end;

function TSceneGraph.getMainCamera: TSceneCamera;
begin
  if FCameras.Count > 0 then
    Result := FCameras[0] else Result := nil;
end;

function TSceneGraph.getMatCount: integer;
begin
  result := FMaterials.Count;
end;

function TSceneGraph.getMaterial(Index: integer): TMaterialObject;
begin
  result := TMaterialObject(FMaterials[index]);
end;

function TSceneGraph.GetMaterials: TMaterialList;
begin
  result := FMaterials;
end;

procedure TSceneGraph.Notify(Sender: TObject; Msg: Cardinal; Params: pointer);
begin
  inherited;
  if assigned(Sender) then begin
    case Msg of
      NM_ObjectDestroyed:
        if Sender is TNotifiableObject then UnSubscribe(TNotifiableObject(Sender));
    end;
  end;
end;

end.
