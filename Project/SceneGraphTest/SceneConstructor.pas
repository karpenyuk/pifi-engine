unit SceneConstructor;

interface

uses Classes,
     uBaseTypes, uVMath, uPrimitives, uMiscUtils, uRenderResource, uBaseRenders,
     uLists, uImageFormats, uImageLoader, uWorldSpace, uBaseClasses;

type

  TDemoScene = class
  private
    FSceneGraph: TSceneGraph;
    FSceneObject: TSceneObject;
    FSprite: array[0..2] of TSceneObject;
    FMaterial: array[0..3] of TMaterialObject;
    FSpriteMaterial: TMaterialObject;
    FLight: array[0..2] of TLightSource;
    FShader: array[0..1] of TShaderProgram;
    FMeshList: TMeshList;
    FImageLoader: TImageLoader;
    FSampler: TTextureSampler;
    FTexture: TTexture;
    FSlaves: TObjectList;
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
  FSlaves := TObjectList.Create;
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

begin
  FMeshList:=TMeshList.Create;
  FSlaves.Add(FMeshList);
  FMeshList.AddNewMesh(CreateBox(2, 1.5, 3.5)).LocalMatrix:=TMatrix.TranslationMatrix(Vector(-3,3,-0.1));
  FMeshList.AddNewMesh(CreateSphere(1, 16, 32)).LocalMatrix:=TMatrix.TranslationMatrix(Vector(3,-3,+0.1));
  FMeshList.AddNewMesh(CreateTeapod(4));
  FMeshList.AddNewMesh(CreatePlane(3,3)).LocalMatrix:=TMatrix.RotationMatrix(Vector(1.0,0.0,0.0),Pi/180*90);

  MeshObject:=TMeshObject.CreateFrom(FMeshList);

  FSceneObject:=TSceneObject.Create;
  FSlaves.Add(FSceneObject);
  FSceneObject.MeshObjects.AddMeshObject(MeshObject,true);
  FSceneGraph.AddItem(FSceneObject);

  Mesh := TMesh.CreateFrom(CreateSprite());
  MeshObject:=TMeshObject.CreateFrom(Mesh);

  FSprite[0] := TSceneObject.Create;
  FSprite[0].DirectionBehavior := dbSphericalSprite;
  FSprite[0].MeshObjects.AddMeshObject(MeshObject,true);
  FSceneGraph.AddItem(FSprite[0]);
  FSlaves.Add(FSprite[0]);

  FSprite[1] := TSceneObject.Create;
  FSprite[1].DirectionBehavior := dbSphericalSprite;
  FSprite[1].MeshObjects.AddMeshObject(MeshObject,true);
  FSceneGraph.AddItem(FSprite[1]);
  FSlaves.Add(FSprite[1]);

  FSprite[2] := TSceneObject.Create;
  FSprite[2].DirectionBehavior := dbSphericalSprite;
  FSprite[2].MeshObjects.AddMeshObject(MeshObject,true);
  FSceneGraph.AddItem(FSprite[2]);
  FSlaves.Add(FSprite[2]);

  // Create material which use shader
  FShader[0] := ShaderGenerator.GenForwardLightShader();
  FSlaves.Add(FShader[0]);

  FMaterial[0] := TMaterialObject.Create;
  FSlaves.Add(FMaterial[0]);
  FMaterial[0].AttachShader(FShader[0]);
  with FMaterial[0].AddNewMaterial('RedMate') do begin
     Properties.DiffuseColor.SetColor(165, 41, 0, 255);
  end;
  FMeshList[0].MaterialObject := FMaterial[0];
  FSceneGraph.AddMaterial(FMaterial[0]);

  FMaterial[1] := TMaterialObject.Create;
  FSlaves.Add(FMaterial[1]);
  FMaterial[1].AttachShader(FShader[0]);
  with FMaterial[1].AddNewMaterial('DarkShine') do begin
     Properties.DiffuseColor.SetColor(15, 15, 15, 255);
     Properties.SpecularColor.SetColor(127, 127, 127, 255);
  end;
  FMeshList[1].MaterialObject := FMaterial[1];
  FSceneGraph.AddMaterial(FMaterial[1]);

  FMaterial[2] := TMaterialObject.Create;
  FSlaves.Add(FMaterial[2]);
  FMaterial[2].AttachShader(FShader[0]);
  with FMaterial[2].AddNewMaterial('Enamel') do begin
     Properties.DiffuseColor.SetColor(221, 236, 192, 255);
     Properties.SpecularColor.SetColor(250, 250, 250, 255);
  end;
  FMeshList[2].MaterialObject := FMaterial[2];
  FSceneGraph.AddMaterial(FMaterial[2]);

  FMaterial[3] := TMaterialObject.Create;
  FSlaves.Add(FMaterial[3]);
  FMaterial[3].AttachShader(FShader[0]);
  with FMaterial[3].AddNewMaterial('GreenLuminescent') do begin
     Properties.DiffuseColor.SetColor(4, 17, 0, 255);
     Properties.EmissionColor.SetColor(41, 165, 0, 255);
  end;
  FMeshList[3].MaterialObject := FMaterial[3];
  FSceneGraph.AddMaterial(FMaterial[3]);

  FShader[1] := ShaderGenerator.GenLightGlyphShader();
  FSlaves.Add(FShader[1]);
  FSpriteMaterial := TMaterialObject.Create;
  FSlaves.Add(FSpriteMaterial);
  FSpriteMaterial.AttachShader(FShader[1]);

  FImageLoader := TImageLoader.Create();
  FSlaves.Add(FImageLoader);
  FImageLoader.LoadImageFromFile(path + 'OmniLightTexture.dds');
  FTexture := FImageLoader.CreateTexture();
  FSlaves.Add(FTexture);
  FTexture.GenerateMipMaps := true;
  FSpriteMaterial.AttachTexture(FTexture);

  FSampler:=TTextureSampler.Create;
  FSlaves.Add(FSampler);
  FSampler.WrapS := twClampToEdge;
  FSampler.WrapT := twClampToEdge;
  FSampler.minFilter := mnLinearMipmapLinear;
  FSampler.magFilter := mgLinear;
  FSpriteMaterial.AttachSampler(FSampler);

  with FSpriteMaterial.AddNewMaterial('Sprite') do begin
     Properties.DiffuseColor.SetColor(255, 255, 255, 255);
  end;

  Mesh.MaterialObject := FSpriteMaterial;
  FSceneGraph.AddMaterial(FSpriteMaterial);

  FLight[0] := TLightSource.Create;
  FLight[0].LightStyle := lsOmni;
  FLight[0].Position.SetVector(2, 10, 3);
  FLight[0].Specular.SetColor(250, 250, 250, 255);
  FSceneGraph.AddLight(FLight[0]);
  FSprite[0].MoveObject(FLight[0].Position);
  FSlaves.Add(FLight[0]);

  FLight[1] := TLightSource.Create;
  FLight[1].LightStyle := lsOmni;
  FLight[1].Position.SetVector(-6, 5, -1);
  FLight[1].Diffuse.SetColor(250, 250, 15, 255);
  FSceneGraph.AddLight(FLight[1]);
  FSprite[1].MoveObject(FLight[1].Position);
  FSlaves.Add(FLight[1]);

  FLight[2] := TLightSource.Create;
  FLight[2].LightStyle := lsOmni;
  FLight[2].Position.SetVector(0, 7, 0);
  FLight[2].Specular.SetColor(5, 5, 120, 255);
  FSceneGraph.AddLight(FLight[2]);
  FSprite[2].MoveObject(FLight[2].Position);
  FSlaves.Add(FLight[2]);

  FSceneGraph.Camera.FoV:=60;
  FSceneGraph.Camera.MoveObject(0, 2, -10);

  FSceneGraph.Camera.ViewTarget := FSceneObject;
end;

destructor TDemoScene.Destroy;
begin
  FSceneGraph.Free;
  FSlaves.FreeObjects;
  FSlaves.Free;
  inherited;
end;

procedure TDemoScene.SetSize(aWidth, aHeight: integer);
begin
  FSceneGraph.Camera.ViewPortSize := Vec2iMake(aWidth,aHeight);
end;

end.
