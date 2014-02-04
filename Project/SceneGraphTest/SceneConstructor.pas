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
    FMaterial: array[0..3] of TMaterialObject;
    FLight: TLightSource;
    FShader: TShaderProgram;
    FMeshList: TMeshList;
    procedure CreateScene;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetSize(aWidth, aHeight: integer);
    property SceneGraph: TSceneGraph read FSceneGraph;
  end;

implementation

uses
  uShaderGen;

{ TDemoScene }

constructor TDemoScene.Create;
begin
  FSceneGraph := TSceneGraph.Create;
  CreateScene;
end;

procedure TDemoScene.CreateScene;
var MeshObject: TMeshObject;
begin
  FMeshList:=TMeshList.Create;
  FMeshList.AddNewMesh(CreateBox(2, 1.5, 3.5)).LocalMatrix:=TMatrix.TranslationMatrix(Vector(-3,3,-0.1));
  FMeshList.AddNewMesh(CreateSphere(1, 16, 32)).LocalMatrix:=TMatrix.TranslationMatrix(Vector(3,-3,+0.1));
  FMeshList.AddNewMesh(CreateTeapod(4));
  FMeshList.AddNewMesh(CreatePlane(3,3)).LocalMatrix:=TMatrix.RotationMatrix(Vector(1.0,0.0,0.0),Pi/180*90);

  MeshObject:=TMeshObject.CreateFrom(FMeshList);

  FSceneObject:=TSceneObject.Create;
  FSceneObject.MeshObjects.AddMeshObject(MeshObject,true);
  FSceneGraph.AddItem(FSceneObject);

  // Create material which use shader
  FShader := TConfigShader.GenForwardLightShader();

  FMaterial[0] := TMaterialObject.Create;
  FMaterial[0].AttachShader(FShader);
  with FMaterial[0].AddNewMaterial('Red') do begin
     Properties.DiffuseColor.SetColor(165, 41, 0, 255);
  end;
  FMeshList[0].MaterialObject := FMaterial[0];
  FSceneGraph.AddMaterial(FMaterial[0]);

  FMaterial[1] := TMaterialObject.Create;
  FMaterial[1].AttachShader(FShader);
  with FMaterial[1].AddNewMaterial('Sphere') do begin
     Properties.DiffuseColor.SetColor(5, 5, 5, 255);
     Properties.SpecularColor.SetColor(127, 127, 127, 255);
  end;
  FMeshList[1].MaterialObject := FMaterial[1];
  FSceneGraph.AddMaterial(FMaterial[1]);

  FMaterial[1] := TMaterialObject.Create;
  FMaterial[1].AttachShader(FShader);
  with FMaterial[1].AddNewMaterial('Sphere') do begin
    Properties.AmbientColor.SetColor(5, 5, 5, 255);
     Properties.DiffuseColor.SetColor(5, 5, 5, 255);
     Properties.SpecularColor.SetColor(127, 127, 127, 255);
  end;
  FMeshList[1].MaterialObject := FMaterial[1];
  FSceneGraph.AddMaterial(FMaterial[1]);

  FMaterial[2] := TMaterialObject.Create;
  FMaterial[2].AttachShader(FShader);
  with FMaterial[2].AddNewMaterial('Teapod') do begin
     Properties.DiffuseColor.SetColor(221, 236, 192, 255);
     Properties.SpecularColor.SetColor(250, 250, 250, 255);
  end;
  FMeshList[2].MaterialObject := FMaterial[2];
  FSceneGraph.AddMaterial(FMaterial[2]);

  FMaterial[3] := TMaterialObject.Create;
  FMaterial[3].AttachShader(FShader);
  with FMaterial[3].AddNewMaterial('Plane') do begin
     Properties.DiffuseColor.SetColor(0, 0, 0, 255);
     Properties.EmissionColor.SetColor(41, 165, 0, 255);
  end;
  FMeshList[3].MaterialObject := FMaterial[3];
  FSceneGraph.AddMaterial(FMaterial[3]);


  FLight := TLightSource.Create;
  FLight.Position.SetVector(2, 10, 3);
  FLight.Specular.SetColor(250, 250, 250, 255);
  FSceneGraph.AddLight(FLight);

  FSceneGraph.Camera.FoV:=60;
  FSceneGraph.Camera.MoveObject(0, 0, -10);

  FSceneGraph.Camera.ViewTarget := FSceneObject;
end;

destructor TDemoScene.Destroy;
begin
  FSceneGraph.Free;
  FSceneObject.Free;
  FShader.Free;
  FMaterial[0].Free;
  FMaterial[1].Free;
  FMaterial[2].Free;
  FMaterial[3].Free;
  FLight.Free;
  FMeshList.Free;
  inherited;
end;

procedure TDemoScene.SetSize(aWidth, aHeight: integer);
begin
  FSceneGraph.Camera.ViewPortSize := Vec2iMake(aWidth,aHeight);
end;

end.
