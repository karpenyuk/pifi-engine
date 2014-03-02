unit uMain;

interface

uses
  SysUtils, Classes, uBaseGL, uBaseTypes, uVMath, dglOpenGL,
  uPrimitives, uMiscUtils, uRenderResource, uBaseRenders, uGLRenders,
  uLists, uImageFormats, uImageLoader, uWorldSpace;

type

  TDemo = class
    procedure ContextReady(Sender: TObject);
    procedure SceneRender(Sender: TObject; Delta: double);
    procedure onResize(Sender: TObject; NewWidth, NewHeight: Integer);
    procedure onMouseDown(Sender: TObject; X, Y: Integer;
      Buttons: TCMouseButtons);
    procedure onMouseMove(Sender: TObject; X, Y: Integer;
      Buttons: TCMouseButtons);
    procedure onDebugMessage(const AMessage: string);
  private
    { Private declarations }
    FSceneGraph: TSceneGraph;
    FRender: TBaseRender;
    MX, MY: Integer;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;
    property SceneGraph: TSceneGraph read FSceneGraph;
  end;

var
  Demo: TDemo;

implementation

uses
  uShaderGen, uStorage, uMath;

procedure TDemo.onDebugMessage(const AMessage: string);
begin
  Sleep(0);
end;

procedure TDemo.onMouseDown(Sender: TObject; X, Y: Integer;
  Buttons: TCMouseButtons);
begin
  MX := X;
  MY := Y;
end;

procedure TDemo.onMouseMove(Sender: TObject; X, Y: Integer;
  Buttons: TCMouseButtons);
begin
  if mbLeft in Buttons then
  begin
    FSceneGraph.Camera.RotateAround(VecNull, vecY, MY - Y, MX - X);
  end;
  MX := X;
  MY := Y;
end;

procedure TDemo.onResize(Sender: TObject; NewWidth, NewHeight: Integer);
begin
  FSceneGraph.Camera.ViewPortSize := Vec2iMake(NewWidth, NewHeight);
end;

procedure TDemo.ContextReady(Sender: TObject);
var
  ver: TApiVersion;
begin
  // Checking OpenGL Version
  ver.GAPI := avGL;
  ver.Version := 420;
  FRender := nil;

  // Среди зарегистрированных рендеров выбираем подходящий
  FRender := vRegisteredRenders.GetCompatibleRender(ver);

end;

constructor TDemo.Create;
var
  VO: TVertexObject;
  Mesh: TMesh;
  MeshObject: TMeshObject;
  Shader: TShaderProgram;
  Material: TMaterialObject;
  SceneObject, Last: TSceneObject;
  Light: TLightSource;
begin
  FSceneGraph := TSceneGraph.Create;

  // Create material which use shader
  Shader := ShaderGenerator.GenForwardLightShader();

  Material := Storage.CreateMaterialObject;
  Material.AttachShader(Shader);
  with Material.AddNewMaterial('Orange') do begin
     Properties.DiffuseColor.SetColor(255, 127, 0, 255);
     Properties.SpecularColor.SetColor(52, 26, 0, 255);
     Properties.Shininess := 127;
  end;
  FSceneGraph.AddMaterial(Material);

  VO := CreateCylinder(1.8, 2.4, 2, 24, 6);
  Mesh := TMesh.CreateFrom(VO);
  Mesh.MaterialObject := Material;
  MeshObject := TMeshObject.CreateFrom(Mesh);
  SceneObject:= Storage.CreateSceneObject;
  SceneObject.FriendlyName := 'Base';
  SceneObject.MeshObjects.AddMeshObject(MeshObject);
  FSceneGraph.AddItem(SceneObject);
  Last := SceneObject;
  SceneObject.MoveUp(-5);

  VO := CreateSphere(1.5, 24, 24);
  Mesh := TMesh.CreateFrom(VO);
  Mesh.MaterialObject := Material;
  MeshObject := TMeshObject.CreateFrom(Mesh);
  SceneObject:= Storage.CreateSceneObject;
  SceneObject.FriendlyName := 'Hinge1';
  SceneObject.MeshObjects.AddMeshObject(MeshObject);
  FSceneGraph.AddItem(SceneObject);
  SceneObject.Parent := Last;
  Last := SceneObject;
  SceneObject.MoveUp(1.75);
  SceneObject.RollObject(45);

  VO := CreateCylinder(1, 1.5, 5, 24, 6);
  Mesh := TMesh.CreateFrom(VO);
  Mesh.MaterialObject := Material;
  MeshObject := TMeshObject.CreateFrom(Mesh);
  SceneObject:= Storage.CreateSceneObject;
  SceneObject.FriendlyName := 'Forearm';
  SceneObject.MeshObjects.AddMeshObject(MeshObject);
  SceneObject.Parent := Last;
  Last := SceneObject;
  SceneObject.MoveUp(3.25);

  VO := CreateSphere(1.0, 24, 24);
  Mesh := TMesh.CreateFrom(VO);
  Mesh.MaterialObject := Material;
  MeshObject := TMeshObject.CreateFrom(Mesh);
  SceneObject:= Storage.CreateSceneObject;
  SceneObject.FriendlyName := 'Hinge2';
  SceneObject.MeshObjects.AddMeshObject(MeshObject);
  SceneObject.Parent := Last;
  Last := SceneObject;
  SceneObject.MoveUp(3);
  SceneObject.RollObject(-90);

  VO := CreateCylinder(1.0, 1.0, 6, 24, 6);
  Mesh := TMesh.CreateFrom(VO);
  Mesh.MaterialObject := Material;
  MeshObject := TMeshObject.CreateFrom(Mesh);
  SceneObject:= Storage.CreateSceneObject;
  SceneObject.FriendlyName := 'Arm';
  SceneObject.MeshObjects.AddMeshObject(MeshObject);
  SceneObject.Parent := Last;
  Last := SceneObject;
  SceneObject.MoveUp(3.5);

  VO := CreateSphere(1.0, 24, 24);
  Mesh := TMesh.CreateFrom(VO);
  Mesh.MaterialObject := Material;
  MeshObject := TMeshObject.CreateFrom(Mesh);
  SceneObject:= Storage.CreateSceneObject;
  SceneObject.FriendlyName := 'Hinge3';
  SceneObject.MeshObjects.AddMeshObject(MeshObject);
  SceneObject.Parent := Last;
  Last := SceneObject;
  SceneObject.MoveUp(3.5);
  SceneObject.RollObject(-45);

  VO := CreateBox(4, 0.3, 1);
  Mesh := TMesh.CreateFrom(VO);
  Mesh.MaterialObject := Material;
  MeshObject := TMeshObject.CreateFrom(Mesh);
  SceneObject:= Storage.CreateSceneObject;
  SceneObject.FriendlyName := 'Palm';
  SceneObject.MeshObjects.AddMeshObject(MeshObject);
  SceneObject.Parent := Last;
  Last := SceneObject;
  SceneObject.MoveUp(1);

  VO := CreateBox(0.4, 3, 0.8);
  Mesh := TMesh.CreateFrom(VO);
  Mesh.MaterialObject := Material;
  MeshObject := TMeshObject.CreateFrom(Mesh);
  SceneObject:= Storage.CreateSceneObject;
  SceneObject.FriendlyName := 'Finger1';
  SceneObject.MeshObjects.AddMeshObject(MeshObject);
  SceneObject.Parent := Last;
  SceneObject.MoveUp(1.65);
  SceneObject.MoveForward(1.65);

  MeshObject := TMeshObject.CreateFrom(Mesh);
  SceneObject:= Storage.CreateSceneObject;
  SceneObject.FriendlyName := 'Finger2';
  SceneObject.MeshObjects.AddMeshObject(MeshObject);
  SceneObject.Parent := Last;
  Last := SceneObject;
  SceneObject.MoveUp(1.65);
  SceneObject.MoveForward(-1.65);

  Light := Storage.CreateLight;
  Light.LightStyle := lsParallel;
  Light.MoveObject(2, 10, -3);
  Light.Specular.SetColor(250, 250, 250, 255);
  FSceneGraph.AddItem(light);

  FSceneGraph.Camera.FoV:=60;
  FSceneGraph.Camera.MoveObject(0, 5, -25);
  FSceneGraph.Camera.ViewTarget := FSceneGraph.Items['Hinge3'] as TSceneObject;
end;

destructor TDemo.Destroy;
begin
  FSceneGraph.Free;
  inherited;
end;

procedure TDemo.SceneRender(Sender: TObject; Delta: double);
begin
  if Assigned(FRender) then FRender.ProcessScene(FSceneGraph);
end;

end.
