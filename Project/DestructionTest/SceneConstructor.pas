unit SceneConstructor;

interface

uses Classes,
     uBaseTypes, uVMath, uPrimitives, uMiscUtils, uRenderResource, uBaseRenders,
     uLists, uImageFormats, uImageLoader, uWorldSpace, uBaseClasses,
     uEffectsPipeline;

type

  TDemoScene = class
  private
    FSceneGraph: TSceneGraph;
    FMeshList: TMeshList;
    SceneObject: TSceneObject;
    shader: TShaderProgram;
    Material: TMaterialObject;
    procedure CreateScene;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetSize(aWidth, aHeight: integer);
    property SceneGraph: TSceneGraph read FSceneGraph;
  end;

implementation

uses
  uShaderGen, uStorage, uMath;

{ TDemoScene }

constructor TDemoScene.Create;
begin
  FSceneGraph := TSceneGraph.Create;
  CreateScene;
end;

procedure TDemoScene.CreateScene;
const
{$IFDEF MSWindows}
  path : string = '..\..\Source\Media\';
{$ENDIF}
{$IFDEF Linux}
  path : string = '../../Source/Media/';
{$ENDIF}
var MeshObject: TMeshObject;
    Mesh: TMesh;
    Sprite_VO: TVertexObject;
    vo: TVertexObject;
    light: TLightSource;
begin
  light := Storage.CreateLight;  light.FriendlyName := 'light1';
  light.LightStyle := lsOmni;
  light.MoveObject(2, 0.5, -3);
  light.Specular.SetColor(250, 250, 250, 255);
  FSceneGraph.AddItem(light);
  // Create material which use shader
  shader := ShaderGenerator.GenForwardLightShader();

  Material := Storage.CreateMaterialObject;

  Material.AttachShader(shader);
  with Material.AddNewMaterial('RedMate') do begin
     Properties.DiffuseColor.SetColor(165, 41, 0, 255);
  end;

  FMeshList:=TMeshList.Create;
  vo := CreateTeapod(4);
  vo.Notify(Self,NM_DebugMessageStr,pchar('----------------- Teapod VertexObject -----------------'));
  FMeshList.AddNewMesh(vo).MaterialObject:=Material;
  vo := CreatePlane(3,3);
  vo.Notify(Self,NM_DebugMessageStr,pchar('----------------- Plane VertexObject ------------------'));
  FMeshList.AddNewMesh(vo).MaterialObject:=Material;

  MeshObject:=TMeshObject.CreateFrom(FMeshList);

  SceneObject:= Storage.CreateSceneObject;
  SceneObject.MeshObjects.AddMeshObject(MeshObject);
  FSceneGraph.AddItem(SceneObject);

  FSceneGraph.AddMaterial(Material);

  FSceneGraph.Cameras[0].FoV:=60;
  FSceneGraph.Cameras[0].MoveObject(0, 2, -10);
  FSceneGraph.Cameras[0].ViewTarget := SceneObject;
end;

destructor TDemoScene.Destroy;
begin
  if assigned(FMeshList) then FMeshList.Free;
  FSceneGraph.Free;
  inherited;
end;

procedure TDemoScene.SetSize(aWidth, aHeight: integer);
begin
  FSceneGraph.Camera.ViewPortSize := Vec2iMake(aWidth, aHeight);
end;

end.
