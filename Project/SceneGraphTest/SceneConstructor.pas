unit SceneConstructor;

interface

uses uBaseTypes, uVMath, uPrimitives, uMiscUtils, uRenderResource, uBaseRenders,
     uLists, uImageFormats, uImageLoader, uWorldSpace;

type

  TDemoScene = class
  private
    FSceneGraph: TSceneGraph;
    FSceneObject: TSceneObject;
    procedure CreateScene;
  public
    constructor Create;
    procedure SetSize(aWidth, aHeight: integer);
  end;

implementation

{ TDemoScene }

constructor TDemoScene.Create;
begin
  FSceneGraph := TSceneGraph.Create;
  CreateScene;
end;

procedure TDemoScene.CreateScene;
var MeshObject: TMeshObject;
    MeshList: TMeshList;
begin
  MeshList:=TMeshList.Create;
  MeshList.AddNewMesh(CreateBox(2, 1.5, 3.5)).LocalMatrix:=TMatrix.TranslationMatrix(Vector(-0.5,0.5,-0.1));
  MeshList.AddNewMesh(CreateSphere(1, 16, 32)).LocalMatrix:=TMatrix.TranslationMatrix(Vector(+0.5,-0.5,+0.1));
  MeshList.AddNewMesh(CreateTeapod(4));
  MeshList.AddNewMesh(CreatePlane(0.6,0.6)).LocalMatrix:=TMatrix.RotationMatrix(Vector(0.0,0.0,1.0),Pi/180*30);

  MeshObject:=TMeshObject.CreateFrom(MeshList);

  FSceneObject:=TSceneObject.Create;
  FSceneObject.MeshObjects.AddMeshObject(MeshObject);

  FSceneGraph.Camera.FoV:=60;
end;

procedure TDemoScene.SetSize(aWidth, aHeight: integer);
begin
  FSceneGraph.Camera.ViewPortSize := Vec2iMake(aWidth,aHeight);
end;

end.
