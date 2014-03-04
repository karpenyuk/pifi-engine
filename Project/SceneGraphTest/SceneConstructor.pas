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
    FMeshList: TMeshAssembly;
    FImageLoader: TImageLoader;
    FSampler: TTextureSampler;
    FTexture: TTexture;
    FRGBAfloatImage: TImageHolder;
    SceneObject: TSceneObject;
    light: TLightSource;
    shader: TShaderProgram;
    Sprite: array[0..2] of TSceneObject;
    Material: array[0..4] of TMaterialObject;
    SpriteMaterial: TMaterialObject;
    FrameBuffer: TFrameBuffer;
    ColorAttachment: TTexture;
    effects: TEffectPipeline;
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

begin
  FMeshList:=TMeshAssembly.Create;

  vo := CreateBox(2, 1.5, 3.5); Mesh := FMeshList.AddNewMesh(vo);
  FMeshList.LocalMatrices[0] := TMatrix.TranslationMatrix(Vector(-3,3,-0.1));

  vo := CreateSphere(1, 16, 32); Mesh := FMeshList.AddNewMesh(vo);
  FMeshList.LocalMatrices[1] := TMatrix.TranslationMatrix(Vector(3,-3,+0.1));

  vo := CreateTeapod(4); FMeshList.AddNewMesh(vo);

  vo := CreatePlane(3,3);Mesh := FMeshList.AddNewMesh(vo);
  FMeshList.LocalMatrices[3] := TMatrix.RotationMatrix(Vector(1.0,0.0,0.0),Pi/180*90);

  MeshObject:=TMeshObject.CreateFrom(FMeshList);

  SceneObject:= Storage.CreateSceneObject;
  SceneObject.MeshObjects.AddMeshObject(MeshObject);
  FSceneGraph.AddItem(SceneObject);

  Sprite_VO := CreateSprite(1, 1);
  Mesh := TMesh.CreateFrom(Sprite_VO);

  Sprite[0] := Storage.CreateSceneObject;
  Sprite[0].DirectionBehavior := dbSphericalSprite;
  MeshObject:=TMeshObject.CreateFrom(Mesh); Sprite[0].MeshObjects.AddMeshObject(MeshObject);
  FSceneGraph.AddItem(Sprite[0]);

  Sprite[1] := Storage.CreateSceneObject;
  Sprite[1].DirectionBehavior := dbSphericalSprite;
  MeshObject:=TMeshObject.CreateFrom(Mesh); Sprite[1].MeshObjects.AddMeshObject(MeshObject);
  FSceneGraph.AddItem(Sprite[1]);

  Sprite[2] := Storage.CreateSceneObject;
  Sprite[2].DirectionBehavior := dbSphericalSprite;
  MeshObject:=TMeshObject.CreateFrom(Mesh); Sprite[2].MeshObjects.AddMeshObject(MeshObject);
  FSceneGraph.AddItem(Sprite[2]);

  // Create material which use shader
  shader := ShaderGenerator.GenForwardLightShader();

  Material[0] := Storage.CreateMaterialObject;
  Material[0].AttachShader(shader);
  with Material[0].AddNewMaterial('RedMate') do begin
     Properties.DiffuseColor.SetColor(165, 41, 0, 255);
  end;
  FMeshList[0].MaterialObject := Material[0];
  FSceneGraph.AddMaterial(Material[0]);

  Material[1] := Storage.CreateMaterialObject;
  Material[1].AttachShader(shader);
  with Material[1].AddNewMaterial('DarkShine') do begin
     Properties.DiffuseColor.SetColor(15, 15, 15, 255);
     Properties.SpecularColor.SetColor(127, 127, 127, 255);
  end;
  FMeshList[1].MaterialObject := Material[1];
  FSceneGraph.AddMaterial(Material[1]);

  Material[2] := Storage.CreateMaterialObject;
  Material[2].AttachShader(shader);
  with Material[2].AddNewMaterial('Enamel') do begin
     Properties.DiffuseColor.SetColor(221, 236, 192, 255);
     Properties.SpecularColor.SetColor(250, 250, 250, 255);
  end;
  FMeshList[2].MaterialObject := Material[2];
  FSceneGraph.AddMaterial(Material[2]);

  Material[3] := Storage.CreateMaterialObject;
  Material[3].AttachShader(shader);
  with Material[3].AddNewMaterial('GreenLuminescent') do begin
     Properties.DiffuseColor.SetColor(4, 17, 0, 255);
     Properties.EmissionColor.SetColor(2*41, 2*165, 0, 255);
  end;
  FMeshList[3].MaterialObject := Material[3];
  FSceneGraph.AddMaterial(Material[3]);

  shader := ShaderGenerator.GenLightGlyphShader();
  SpriteMaterial := Storage.CreateMaterialObject;
  FSceneGraph.AddMaterial(SpriteMaterial);
  SpriteMaterial.AttachShader(shader);

  FImageLoader := TImageLoader.Create();
  FImageLoader.LoadImageFromFile(path + 'OmniLightTexture.dds');
  FTexture := FImageLoader.CreateTexture();
  FTexture.GenerateMipMaps := true;
  SpriteMaterial.AttachTexture(FTexture);

  FSampler:= Storage.CreateTextureSample;
  FSampler.WrapS := twClampToEdge;
  FSampler.WrapT := twClampToEdge;
  FSampler.minFilter := mnLinearMipmapLinear;
  FSampler.magFilter := mgLinear;
  SpriteMaterial.AttachSampler(FSampler);

  with SpriteMaterial.AddNewMaterial('Sprite') do begin
     Properties.DiffuseColor.SetColor(255, 255, 255, 255);
  end;

  Mesh.MaterialObject := SpriteMaterial;
  FSceneGraph.AddMaterial(SpriteMaterial);

  light := Storage.CreateLight;  light.FriendlyName := 'light1';
  light.LightStyle := lsOmni;
  light.MoveObject(2, 0.5, -3);
  light.Specular.SetColor(250, 250, 250, 255);
  FSceneGraph.AddItem(light);
  Sprite[0].Parent := light;

  light := Storage.CreateLight;
  light.LightStyle := lsOmni;
  light.MoveObject(-4, 5, 3);
  light.Diffuse.SetColor(250, 250, 15, 255);
  FSceneGraph.AddItem(light);
  Sprite[1].Parent := light;

  light := Storage.CreateLight;
  light.LightStyle := lsOmni;
  light.MoveObject(0, 7, 0);
  light.Specular.SetColor(5, 5, 120, 255);
  FSceneGraph.AddItem(light);
  Sprite[2].Parent := light;

{
  FScreenQuad  := TSceneObject.Create;
  Mesh := TMesh.CreateFrom(Sprite_VO);
  MeshObject:=TMeshObject.CreateFrom(Mesh);
  FScreenQuad.MeshObjects.AddMeshObject(MeshObject,true);
  FSceneGraph.AddItem(FScreenQuad);
  FShader[2] := ShaderGenerator.GenScreenQuadShader();
  FMaterial[4] := TMaterialObject.Create;
  FSceneGraph.AddMaterial(FMaterial[4]);
  FMaterial[4].AttachShader(FShader[2]);
  Mesh.MaterialObject := FMaterial[4];
}
  FSceneGraph.Camera.FoV:=60;
  FSceneGraph.Camera.MoveObject(0, 2, -10);
  FSceneGraph.Camera.ViewTarget := SceneObject;

  FRGBAfloatImage:=TImageSampler.CreateBitmap(
    TImageFormatSelector.CreateSpecial(sfR11FG11FB10F), 2048, 2048, false);
  ColorAttachment := Storage.CreateTexture(FRGBAfloatImage);
  Material[4]:=Storage.CreateMaterialObject;
  Material[4].AttachTexture(ColorAttachment);

  FrameBuffer := Storage.CreateFrameBuffer;
  FrameBuffer.Size := Vec2iMake(2048, 2048);
  // frize size
  FrameBuffer.Resizable := false;
  FrameBuffer.AttachColor(ColorAttachment);
  FrameBuffer.AttachRenderBuffer(rbDepth24, bmBuffer);
  FSceneGraph.Camera.RenderTarget := FrameBuffer;

  effects := Storage.CreateEffectPipeline;
  effects.AddEffect(TGlowPipelineEffect.CreateFrom(ColorAttachment));
  FSceneGraph.Camera.EffectPipeline := effects;
end;

destructor TDemoScene.Destroy;
begin
  FTexture.Free;
  FRGBAfloatImage.Free;
  FImageLoader.Free;
  FMeshList.Free;
  FSceneGraph.Free;
  effects.Free;
  inherited;
end;

procedure TDemoScene.SetSize(aWidth, aHeight: integer);
begin
  FSceneGraph.Camera.ViewPortSize := Vec2iMake(aWidth, aHeight);
end;

end.
