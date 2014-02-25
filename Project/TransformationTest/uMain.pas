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
  end;
  FSceneGraph.AddMaterial(Material);

  VO := CreateCylinder(2, 3, 2, 24, 6);
  Mesh := TMesh.CreateFrom(VO);
  Mesh.MaterialObject := Material;
  MeshObject := TMeshObject.CreateFrom(Mesh);
  SceneObject:= Storage.CreateSceneObject;
  SceneObject.FriendlyName := 'Base';
  SceneObject.MeshObjects.AddMeshObject(MeshObject);
  FSceneGraph.AddItem(SceneObject);
  Last := SceneObject;

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
  SceneObject.MoveUp(2.5);
  SceneObject.RollObject(Pi/4);

  VO := CreateCylinder(1.5, 1, 5, 24, 6);
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
//  SceneObject.RollObject(-Pi/2);

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
  SceneObject.MoveUp(3.5);
//  SceneObject.RollObject(-Pi/4);

  Light := Storage.CreateLight;
  Light.LightStyle := lsParallel;
  Light.MoveObject(2, 10, -3);
  Light.Specular.SetColor(250, 250, 250, 255);
  FSceneGraph.AddItem(light);

  FSceneGraph.Camera.FoV:=60;
  FSceneGraph.Camera.MoveObject(0, 2, -20);
  FSceneGraph.Camera.ViewTarget := Last;
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
