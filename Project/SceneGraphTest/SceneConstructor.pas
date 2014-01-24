unit SceneConstructor;

interface

uses Classes,
     uBaseTypes, uVMath, uPrimitives, uMiscUtils, uRenderResource, uBaseRenders,
     uLists, uImageFormats, uImageLoader, uWorldSpace;

type

  TDemoScene = class
  private
    FSceneGraph: TSceneGraph;
    FSceneObject: TSceneObject;
    FMaterial: TMaterialObject;

    procedure CreateScene;
  public
    constructor Create;
    procedure SetSize(aWidth, aHeight: integer);
    property SceneGraph: TSceneGraph read FSceneGraph;
  end;

implementation

{ TDemoScene }

constructor TDemoScene.Create;
begin
  FSceneGraph := TSceneGraph.Create;
  CreateScene;
end;

procedure TDemoScene.CreateScene;
const
{$IFDEF MSWindows}
  path = 'Media\';
{$ENDIF}
{$IFDEF Linux}
  path = 'Media/';
{$ENDIF}
var MeshObject: TMeshObject;
    MeshList: TMeshList;
    shader: TShaderProgram;
    sl: TStringList;
    i: integer;
begin
  MeshList:=TMeshList.Create;
  MeshList.AddNewMesh(CreateBox(2, 1.5, 3.5)).LocalMatrix:=TMatrix.TranslationMatrix(Vector(-0.5,0.5,-0.1));
  MeshList.AddNewMesh(CreateSphere(1, 16, 32)).LocalMatrix:=TMatrix.TranslationMatrix(Vector(+0.5,-0.5,+0.1));
  MeshList.AddNewMesh(CreateTeapod(4));
  MeshList.AddNewMesh(CreatePlane(0.6,0.6)).LocalMatrix:=TMatrix.RotationMatrix(Vector(0.0,0.0,1.0),Pi/180*30);

  MeshObject:=TMeshObject.CreateFrom(MeshList);


  FSceneObject:=TSceneObject.Create;
  FSceneObject.MeshObjects.AddMeshObject(MeshObject);

  // Create material which use shader
  FMaterial := TMaterialObject.Create;
  shader := FMaterial.AddNewShader('simple_light');
  sl := TStringList.Create;
  try
    sl.LoadFromFile(path + 'Shader.Vert');
    shader.ShaderText[stVertex] := sl.Text;
    sl.LoadFromFile(path + 'Shader.Frag');
    shader.ShaderText[stFragment] := sl.Text;
  finally
    sl.Free;
  end;

  for i := 0 to MeshList.Count - 1 do
    MeshList[i].MaterialObject := FMaterial;

  FSceneGraph.Camera.FoV:=60;
end;

procedure TDemoScene.SetSize(aWidth, aHeight: integer);
begin
  FSceneGraph.Camera.ViewPortSize := Vec2iMake(aWidth,aHeight);
end;

end.
