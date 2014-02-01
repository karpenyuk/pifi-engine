unit uWorldSpace;

interface

uses Classes, uMiscUtils, uBaseTypes, uBaseClasses, uPersistentClasses, uRenderResource;

type

  TSceneGraph = class(TSceneItemList)
    // Решить вопрос со скриптовым рендером - добавить вместе с камерами в корень графа?
    // Или передавать их в рендер через ProcessScene(Scene, Camera, Target, Script)?
  private
    FRoot: TSceneCamera;
    FLights: TLightsList;
    FMaterials: TMaterialList;
    FCameras: TCamerasList;

    function getItem(Index: integer): TBaseSceneItem;
    function getCount: integer;
    function getLight(Index: integer): TLightSource;
    function getLightCount: integer;
    function getMatCount: integer;
    function getMaterial(Index: integer): TMaterialObject;
  public
    constructor Create;
    destructor Destroy; override;

    function AddItem(aItem: TBaseSceneItem): integer;
    function AddLight(aLight: TLightSource): integer;
    function GetLights: TLightsList;

    function AddMaterial(aMat: TMaterialObject): integer;
    function AddNewMaterial(aName: string): TMaterialObject;
    function GetMaterials: TMaterialList;

    property Items[index: integer]: TBaseSceneItem read getItem; default;
    property Lights[index: integer]: TLightSource read getLight;
    property LightsCount: integer read getLightCount;

    property Materials[index: integer]: TMaterialObject read getMaterial;
    property MaterialsCount: integer read getMatCount;

    property Camera: TSceneCamera read FRoot;

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
end;

function TSceneGraph.AddLight(aLight: TLightSource): integer;
begin
  result := FLights.AddLight(aLight);
end;

function TSceneGraph.AddMaterial(aMat: TMaterialObject): integer;
begin
  result := FMaterials.AddMaterial(aMat);
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
begin
  FRoot := TSceneCamera.Create;
  FMaterials:=TMaterialList.Create;
  FLights := TLightsList.Create;
  FCameras := TCamerasList.Create;
end;

destructor TSceneGraph.Destroy;
begin
  FRoot.Free;
  FMaterials.Free;
  FLights.Free;
  FCameras.Free;
  inherited;
end;

function TSceneGraph.getCount: integer;
begin
  result := FRoot.Childs.Count;
end;

function TSceneGraph.getItem(Index: integer): TBaseSceneItem;
begin
  result := TBaseSceneItem(FRoot.Childs[index]);
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

end.
