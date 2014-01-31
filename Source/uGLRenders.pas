 { TODO : Реализовать использование зарегистрированных рендеров и менеджеров ресурсов }
unit uGLRenders;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses Classes, uVMath, uBaseGL, uBaseClasses, uRenderResource, uBaseRenders,
     uBaseTypes, uLists, uMiscUtils, uGenericsRBTree, uWorldSpace, dglOpenGL;

Type

  TGLResources = class;

  TGLPoolBuffers = class
  private
    FBuffers: TList;
    FObjectsCount: integer;
    FObjectSize: integer;
    FSingleBufer: boolean;
    function getBuffPool(Index: integer): TGLBufferObjectsPool;
  public
    constructor Create(aObjectSize: integer; aSingleBufer: boolean = false; aObjectsCount: integer = -1);
    destructor Destroy; override;

    property BuffersPool[Index: integer]: TGLBufferObjectsPool read getBuffPool; default;

    function isExists(const aData: pointer; var aBufferPool: TGLBufferObjectsPool;
      var aSlotIndex: integer): boolean;
    procedure GetSlot(var aBufferPool: TGLBufferObjectsPool; var aSlotIndex: integer);
  end;

  TGLMaterial = class (TGLBaseResource)
  private
    FMaterialObject: TMaterialObject;
    FTex: array of TGLTextureObject;
    FBlend: TCustomBlending;
    FShader: TGLSLShaderProgram;
    FIdexInPool: integer;
    FStructureChanged: boolean;
    procedure UpdateUBO(aPool: TGLBufferObjectsPool);
  public
    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;
    procedure Apply;
    procedure UnApply;

    constructor CreateFrom(aOwner: TGLResources; const aMat: TMaterialObject);
    destructor Destroy; override;
  end;

  TGLLight = class (TGLBaseResource)
  private
    FLight: TLightSource;
    FIdexInPool: integer;
    FStructureChanged: boolean;
    procedure UpdateUBO(aPool: TGLBufferObjectsPool);
  public
    LightSphereRadius: single;

    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;
    procedure Apply;
    procedure UnApply;

    constructor CreateFrom(aOwner: TGLResources; const aLight: TLightSource);
    destructor Destroy; override;

  end;

  TMaterialRBTree = GRedBlackTree<TMaterialObject, TGLMaterial>;

  TLightingRBTree = GRedBlackTree<TLightSource, TGLLight>;

  TGLMesh = class (TGLBaseResource)
  private
    FMesh: TMesh;
    FVertexObject: TGLVertexObject;
    FMaterialObject: TGLMaterial;
  public
    constructor CreateFrom(aOwner: TGLResources; aMesh: TMesh);
  end;

  TGLMeshObject = class (TGLBaseResource)
  private
    FMeshObject: TMeshObject;
    FLods: array of array of TGLMesh;
    FOccluder: array of TGLMesh;
  public
    constructor CreateFrom(aOwner: TGLResources; aMeshObject: TMeshObject);
  end;

  TGLSceneObject = class (TGLBaseResource)
  private
    FSceneObject: TSceneObject;
    FMeshObjects: array of TGLMeshObject;
    FIdexInPool: integer;
    FStructureChanged: boolean;
    procedure UpdateUBO(aPool: TGLBufferObjectsPool);
  public
    constructor CreateFrom(aOwner: TGLResources; aSceneObject: TSceneObject);

    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;
    procedure Apply;
    procedure UnApply;
  end;

  TGLStaticRender = class (TBaseSubRender)
  private
    FTransfPool: TGLBufferObjectsPool;
    FLightPool: TGLBufferObjectsPool;
    FMaterialPool: TGLBufferObjectsPool;
  public
    function isSupported(const aClassType: TClass): boolean; override;
    procedure ProcessResource(const Resource: TBaseRenderResource); override;

    class procedure RenderVertexObject(
      const aVertexObject: TGLVertexObject;
      aShaderUsageLogic: TShaderUsageLogic = slUseOwnShader;
      aShaderUsagePriority: TShaderUsagePriority = spUseOwnShaderFirst);

    constructor Create;
    destructor Destroy; override;

    property TransformationsPool: TGLBufferObjectsPool read FTransfPool;
    property LightsPool: TGLBufferObjectsPool read FLightPool;
    property MaterialsPool: TGLBufferObjectsPool read FMaterialPool;
  end;

  TGLResources = class (TBaseSubRender)
  private
    FResList: array of TRBTree;
    FInnerResource: TRBTree;
    function isInnerResource(const aClassType: TClass): boolean;
    function GetResource(const Resource: TBaseRenderResource): TGLBaseResource;
  public
    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;

    function GetOrCreateResource(const Resource: TBaseRenderResource): TGLBaseResource;
    procedure FreeResource(const Resource: TBaseRenderResource);
    procedure ProcessResource(const Res: TBaseRenderResource); override;

    constructor Create;
    destructor Destroy; override;
  end;


  TGLRender = class (TBaseRender)
  protected
    FResourceManager: TGLResources;

    procedure PrepareResources(const aScene: TSceneGraph);
    procedure UploadResource(const Res: TBaseRenderResource); override;
    procedure ProcessResource(const Res: TBaseRenderResource); override;
//    procedure ProcessMeshObjects(const aMeshObjects: TMeshObjectsList); override;
  public
    constructor Create;
    destructor Destroy; override;

    function isSupported(const aAPI: TApiVersion): boolean; override;

    function UpdateWorldMatrix(const MovableObject: TMovableObject;
      UseMatrix: TTransforms=[ttAll]): TMatrix; override;
    function CheckVisibility(const aFrustum: TFrustum;
      const aExtents: TExtents): boolean; override;
    procedure ProcessScene(const aScene: TSceneGraph); override;

  end;

var
  vGLRender: TGLRender = nil;

implementation

var
  vMaterialPool: TGLPoolBuffers = nil;
  vLightsPool: TGLPoolBuffers = nil;

  vActiveShader: TGLSLShaderProgram = nil;

const
  CMaterialUBOSize = 80;
  CLightUBOSize = 112;

{ TGLRender }

function TGLRender.CheckVisibility(const aFrustum: TFrustum;
  const aExtents: TExtents): boolean;
begin
  result:=inherited;
end;

constructor TGLRender.Create;
begin
  inherited;
  RegisterSubRender(TGLStaticRender.Create);
  FResourceManager:=TGLResources.Create;
end;

destructor TGLRender.Destroy;
begin
  FResourceManager.Free;
  inherited;
end;

function TGLRender.isSupported(const aAPI: TApiVersion): boolean;
begin
  result:= (aAPI.GAPI = avGL) and (aAPI.Version>=420);
end;

procedure TGLRender.PrepareResources(const aScene: TSceneGraph);
var i: integer;
    SceneItem: TBaseSceneItem;
    res: TBaseRenderResource;
begin
  inherited;
  { TODO :
    Реализовать сравнение состояний текущей библиотеки и предыдущего
    снимка (через хэш библиотеки или рассылку уведомлений?)
    Делать вычитку ресурсов только при несовпадении хэша. }

  //создаем ресурсы для всех материалов сцены
  for i:=0 to aScene.MaterialsCount-1 do begin
    res:=aScene.Materials[i];
    FResourceManager.GetOrCreateResource(res);
  end;
  //создаем ресурсы для всех источников света сцены
  for i:=0 to aScene.LightsCount-1 do begin
    res:=aScene.Lights[i];
    FResourceManager.GetOrCreateResource(res);
  end;
  //создаем ресурсы для всех объектов сцены
  for i:=0 to aScene.Count-1 do begin
    SceneItem:=aScene[i];
    FResourceManager.GetOrCreateResource(SceneItem);
  end;
end;

procedure TGLRender.ProcessResource(const Res: TBaseRenderResource);
var i: integer;
    Render: TBaseSubRender;
begin
  for i:=0 to FRegisteredSubRenders.Count-1 do begin
    render:=TBaseSubRender(FRegisteredSubRenders[i]);
    { TODO : Реализовать выбор "наилучшего" из зарегистрированных рендеров }
    if render.isSupported(Res.ClassType) then begin
       render.ProcessResource(res); exit;
    end;
  end;
end;

procedure TGLRender.ProcessScene(const aScene: TSceneGraph);
var i: integer;
    SceneItem: TBaseSceneItem;
    res: TGLBaseResource;
begin
  //Подготавливаем ресурсы сцены
  PrepareResources(aScene);
  //Обрабатываем объекты сцены (подготовка + рендеринг)
  for i:=0 to aScene.Count-1 do begin
    SceneItem:=aScene[i];
    res := FResourceManager.GetOrCreateResource(SceneItem);
    ProcessResource(res);
  end;
end;

function TGLRender.UpdateWorldMatrix(const MovableObject: TMovableObject;
  UseMatrix: TTransforms): TMatrix;
begin
  result:=inherited;
end;

procedure TGLRender.UploadResource(const Res: TBaseRenderResource);
begin
  inherited;
end;

{ TGLStaticRender }

constructor TGLStaticRender.Create;
begin
  inherited Create;
  FTransfPool := TGLBufferObjectsPool.Create(SizeOf(mat4)*6, 2000);
  FLightPool := TGLBufferObjectsPool(SizeOf(vec4)*6, 1000);
  FMaterialPool := TGLBufferObjectsPool(SizeOf(vec4)*5, 100);
end;

destructor TGLStaticRender.Destroy;
begin
  FTransfPool.Destroy;
  FLightPool.Destroy;
  FMaterialPool.Destroy;
  inherited;
end;

function TGLStaticRender.isSupported(const aClassType: TClass): boolean;
begin
  //можем рендерить только GL объекты сцены
  result:=aClassType = TGLSceneObject;
end;

procedure TGLStaticRender.ProcessResource(const Resource: TBaseRenderResource);
var i,j: integer;
    MeshObject: TGLMeshObject;
    Mesh: TGLMesh;
begin
  if Resource.ClassType = TGLSceneObject then
    with TGLSceneObject(Resource) do begin
      //рендерим объект сцены с применением материалов
      for i:=0 to length(FMeshObjects)-1 do begin
        MeshObject:=FMeshObjects[i];
        for j:=0 to length(MeshObject.FLods[0])-1 do begin
          Mesh:=MeshObject.FLods[0,j];
          Mesh.FMaterialObject.Apply;
          Mesh.FVertexObject.RenderVO();
          Mesh.FMaterialObject.UnApply;
        end;
      end;
    end;
end;

class procedure TGLStaticRender.RenderVertexObject(
  const aVertexObject: TGLVertexObject;
  aShaderUsageLogic: TShaderUsageLogic;
  aShaderUsagePriority: TShaderUsagePriority);
var ActiveShader: TGLSLShaderProgram;
const
  CFaceTypeConst: array [TFaceType] of cardinal = (GL_POINTS, GL_LINE_STRIP,
    GL_LINE_LOOP, GL_LINES, GL_LINE_STRIP_ADJACENCY, GL_LINES_ADJACENCY,
    GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN, GL_TRIANGLES,
    GL_TRIANGLE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY, GL_PATCHES, GL_QUADS);
begin
  if not assigned(aVertexObject) then exit;
  //stash old shader
  ActiveShader := vActiveShader;

  if aShaderUsageLogic in [slDisableShader, slStashActiveAndDisableShader]
  then glUseProgram(0)
  else begin
    if ((aShaderUsageLogic in [slUseActiveShader, slUseActiveAndDisable])
    or (aShaderUsagePriority = spUseActiveShaderFirst))
    and assigned(vActiveShader)
    then begin
      //Do nothing, all shaders ready
    end else begin
      //Try to use Own shader
      if assigned(aVertexObject.Shader) then aVertexObject.Shader.Apply
      //else use current active shader
    end;
  end;

  if aVertexObject.StructureChanged then
    if assigned(aVertexObject.Shader) then aVertexObject.Build(aVertexObject.Shader.Id)
    else aVertexObject.Build;

  aVertexObject.Bind;

  if (aVertexObject.IndiceCount > 0) then
  begin
    glDrawElements(CFaceTypeConst[aVertexObject.FaceType], aVertexObject.IndiceCount,
      GL_UNSIGNED_INT, nil);
  end else begin
    if aVertexObject.ElementsCount > 0 then
      glDrawArrays(CFaceTypeConst[aVertexObject.FaceType], 0, aVertexObject.ElementsCount);
  end;

  aVertexObject.UnBind;

  if aShaderUsageLogic in [slUseOwnAndStashActiveShader, slStashActiveAndDisableShader]
  then begin
    if assigned(ActiveShader) then ActiveShader.Apply
    else glUseProgram(0);
  end else begin
    if aShaderUsageLogic in [slDisableShader, slUseActiveAndDisable, slUseOwnAndDisable]
    then begin
      glUseProgram(0); vActiveShader := nil;
    end;
  end;
end;

{ TGLResources }

constructor TGLResources.Create;
var i: integer;
begin
  inherited Create;
  FSupportedResources.Clear;
  //Registering supported resources

  //Main resource
  FSupportedResources.Add(TShaderProgram);
  FSupportedResources.Add(TVertexObject);

  FSupportedResources.Add(TMeshObject);
  FSupportedResources.Add(TMaterialObject);
  FSupportedResources.Add(TSceneObject);

  //Inner resources
{ TODO : Объекты TBufferObject и TAttibObject так же нужно выбирать из коллекций }
  FSupportedResources.Add(TBufferObject);
  FSupportedResources.Add(TAttribObject);
  FSupportedResources.Add(TTexture);
  FSupportedResources.Add(TMesh);


  //Resource Tree for each of resource type, exclude inner resource
  setlength(FResList,FSupportedResources.Count - 4);
  for i := 0 to length(FResList)-1 do
    FResList[i]:=TRBTree.Create(IntPtrComparer);
  //Inner resource list: TBufferObject, TAttribObject, TTexture, TMesh
  FInnerResource:=TRBTree.Create(IntPtrComparer);

end;

destructor TGLResources.Destroy;
var i: integer;
    Node: PRBNode;
begin
  for i := 0 to length(FResList)-1 do begin
    Node:=FResList[i].First;
    while assigned(Node) do begin
      if TGLBaseResource(Node.Data).Owner=self
      then TGLBaseResource(Node.Data).Free;
      TRBNode.RBInc(Node);
    end; FResList[i].Free;
  end;
  Node:=FInnerResource.First;
  while assigned(Node) do begin
    if TGLBaseResource(Node.Data).Owner=self
    then TGLBaseResource(Node.Data).Free;
    TRBNode.RBInc(Node);
  end; FInnerResource.Free;
  inherited;
end;

procedure TGLResources.FreeResource(const Resource: TBaseRenderResource);
var idx: integer;
    Node: PRBNode;
    res: TBaseRenderResource;
begin
  idx:=FSupportedResources.IndexOf(Resource.ClassType);
  if idx <0 then exit;

  //Resource exists?
  if isInnerResource(Resource.ClassType) then begin
    Node:=FInnerResource.Find(Resource);
    if assigned(Node) then begin
      res:=TGLBaseResource(Node.Data); res.Free;
      FInnerResource.Delete(Node);
    end;
  end else begin
    Node:=FResList[idx].Find(Resource);
    if assigned(Node) then begin
      res:=TGLBaseResource(Node.Data); res.Free;
      FResList[idx].Delete(Node);
    end;
  end;

end;

function TGLResources.GetOrCreateResource(
  const Resource: TBaseRenderResource): TGLBaseResource;
var idx: integer;
    Node: PRBNode;
begin
  inherited;
  idx:=FSupportedResources.IndexOf(Resource.ClassType);
  if idx < 0 then begin result:=nil; exit; end;
  //assert(idx>=0,'Unsupported resource: "'+Resource.ClassName+'"!');

  //Resource already exists?
  result:=GetResource(Resource); if assigned(result) then exit;

  //Create new GLResources

  if Resource.ClassType = TShaderProgram then begin
    //Create GLSL Shader program
    Node:=FResList[idx].Add(Resource);
    Node.Data:=TGLSLShaderProgram.CreateFrom(Resource as TShaderProgram);
    TGLBaseResource(Node.Data).Owner:=self;
    Result:=Node.Data; Resource.Subscribe(self); exit;
  end;

  if Resource.ClassType = TVertexObject then begin
    //Create Vertex object
    Node:=FResList[idx].Add(Resource);
    Node.Data:=TGLVertexObject.CreateFrom(Resource as TVertexObject);
    TGLBaseResource(Node.Data).Owner:=self;
    Result:=Node.Data; Resource.Subscribe(self); exit;
  end;

  if Resource.ClassType = TMesh then begin
    //Create Mesh
    Node:=FInnerResource.Add(Resource);
    Node.Data:=TGLMesh.CreateFrom(self, Resource as TMesh);
    Result:=Node.Data; Resource.Subscribe(self); exit;
  end;

  if Resource.ClassType = TMeshObject then begin
    //Create Mesh object
    Node:=FResList[idx].Add(Resource);
    Node.Data:=TGLMeshObject.CreateFrom(self, Resource as TMeshObject);
    Result:=Node.Data; Resource.Subscribe(self); exit;
  end;

  if Resource.ClassType = TSceneObject then begin
    //Create Scene object
    Node:=FResList[idx].Add(Resource);
    Node.Data:=TGLSceneObject.CreateFrom(self, Resource as TSceneObject);
    Result:=Node.Data; Resource.Subscribe(self); exit;
  end;

  if Resource.ClassType = TMaterialObject then begin
    //Create Material object
    Node:=FResList[idx].Add(Resource);
    Node.Data:=TGLMaterial.CreateFrom(self, Resource as TMaterialObject);
    Result:=Node.Data; Resource.Subscribe(self); exit;
  end;

  if Resource.ClassType = TLightSource then begin
    //Create Material object
    Node:=FResList[idx].Add(Resource);
    Node.Data:=TGLLight.CreateFrom(self, Resource as TLightSource);
    Result:=Node.Data; Resource.Subscribe(self); exit;
  end;

  if Resource.ClassType = TTexture then begin
    exit;
  end;

end;

function TGLResources.GetResource(
  const Resource: TBaseRenderResource): TGLBaseResource;
var idx: integer;
    Node: PRBNode;
begin
  inherited;
  result:=nil; idx:=FSupportedResources.IndexOf(Resource.ClassType);
  if idx < 0 then exit;

  if isInnerResource(Resource.ClassType) then begin
    Node:=FInnerResource.Find(Resource);
    if assigned(Node) then begin result:=TGLBaseResource(Node.Data); end;
  end else begin
    Node:=FResList[idx].Find(Resource);
    if assigned(Node) then begin result:=TGLBaseResource(Node.Data); end;
  end;

end;

function TGLResources.isInnerResource(const aClassType: TClass): boolean;
begin
  result:=(aClassType = TBufferObject) or (aClassType = TAttribObject)
    or (aClassType = TTexture) or (aClassType = TMesh);
end;

procedure TGLResources.Notify(Sender: TObject; Msg: Cardinal; Params: pointer);
var res: TGLBaseResource;
begin
  inherited;
  if not (Sender is TBaseRenderResource) then exit;

  res:=GetResource(TBaseRenderResource(Sender));
  if assigned(res) then begin
    case Msg of
      NM_ObjectDestroyed: FreeResource(res);
    end;
  end;
end;

procedure TGLResources.ProcessResource(const Res: TBaseRenderResource);
begin
  inherited;

end;

{ TGLMesh }

constructor TGLMesh.CreateFrom(aOwner: TGLResources; aMesh: TMesh);
begin
  Create;
  assert(assigned(aOwner) and (aOwner is TGLResources),
    'Resource manager invalide or not assigned');
  Owner:=aOwner; FMesh:=aMesh;
  FVertexObject:=TGLVertexObject(aOwner.GetOrCreateResource(FMesh.VertexObject));
  FMaterialObject:=TGLMaterial(aOwner.GetOrCreateResource(FMesh.MaterialObject));
end;

{ TGLMaterial }

procedure TGLMaterial.Apply;
var bi: integer;
    tb, lb: TGLUniformBlock;
begin
  if assigned(FShader) then begin
    FShader.Apply; vActiveShader:=FShader; end else exit;
  if assigned(FUBO) and Assigned(FBlockBuffer) then begin
    bi:=FBlockBuffer.BindUBO(FIdexInPool,FUBO);
    glUniformBlockBinding(FShader.Id, FUBO.BlockIndex, bi);
  end;
  tb := FShader.UniformBlocks.GetUBOByName(CUBOSemantics[ubTransforms].Name);
  if assigned(tb) then begin
    bi:=FBlockBuffer.BindUBO(FIdexInPool,tb);
    glUniformBlockBinding(FShader.Id, tb.BlockIndex, bi);
  end;
  if assigned(vActiveLightBlock) then begin
    bi:=FBlockBuffer.BindUBO(FIdexInPool,vActiveLightBlock);
    glUniformBlockBinding(FShader.Id, vActiveLightBlock.BlockIndex, bi);
  end;
end;

constructor TGLMaterial.CreateFrom(aOwner: TGLResources;
  const aMat: TMaterialObject);
var i: integer;
begin
  Create;
  assert(assigned(aOwner) and (aOwner is TGLResources),'Resource manager invalide or not assigned');
  Owner:=aOwner; FMaterialObject:=aMat;
  FMaterialObject.Subscribe(self);
  FIdexInPool:=-1;

  FShader:=TGLSLShaderProgram(aOwner.GetOrCreateResource(FMaterialObject.Shader));
  FBlend:=FMaterialObject.Blending;
  setlength(FTex,FMaterialObject.TexCount);
  for i:=0 to FMaterialObject.TexCount-1 do begin
    FTex[i]:=TGLTextureObject(aOwner.GetOrCreateResource(FMaterialObject.TextureSlot[i]));
  end;

  FStructureChanged := true;
end;

destructor TGLMaterial.Destroy;
begin
  FShader.Destroy;
  inherited;
end;

procedure TGLMaterial.Notify(Sender: TObject; Msg: Cardinal; Params: pointer);
begin
  inherited;
  case Msg of
    NM_ResourceChanged: if Sender = FMaterialObject then FStructureChanged := true;
  end;
end;

procedure TGLMaterial.UnApply;
begin
  if assigned(FShader) then begin FShader.UnApply; vActiveShader:=nil; end else exit;
//    glUniformBlockBinding(FShader.Id, FUBO.BlockIndex, 0);
end;

procedure TGLMaterial.UpdateUBO(aPool: TGLBufferObjectsPool);
var offs: integer;
    p: PByte;
begin
  if not FStructureChanged then
    exit;

  if FIdexInPool < 0 then
    FIdexInPool := aPool.GetFreeSlotIndex();

  p := aPool.Buffer.MapRange(GL_MAP_WRITE_BIT or GL_MAP_INVALIDATE_RANGE_BIT,
    aPool.OffsetByIndex(FIdexInPool), SizeOf(vec4)*5);

  // Fill Uniform Buffer Object Data
  with FMaterialObject.Material.Properties do begin
    move(AmbientColor.ColorAsAddress^,p^,16); inc(p, 16);
    move(DiffuseColor.ColorAsAddress^,p^,16); inc(p, 16);
    move(SpecularColor.ColorAsAddress^,p^,16); inc(p, 16);
    move(EmissionColor.ColorAsAddress^,p^,16); inc(p, 16);
    move(Shininess,p^,4);
  end;
  aPool.Buffer.UnMap;
  FStructureChanged := false;
end;

{ TGLMeshObject }

constructor TGLMeshObject.CreateFrom(aOwner: TGLResources;
  aMeshObject: TMeshObject);
var i,j: integer;
begin
  Create;
  assert(assigned(aOwner) and (aOwner is TGLResources),'Resource manager invalide or not assigned');
  FMeshObject:=aMeshObject;
  setlength(FLods, aMeshObject.Lods.Count);
  for i:=0 to aMeshObject.Lods.Count-1 do begin
    setlength(FLods[i], aMeshObject.Lods[i].LoD.Count);
    for j:=0 to aMeshObject.Lods[i].LoD.Count-1 do begin
      FLods[i,j]:=TGLMesh(aOwner.GetOrCreateResource(aMeshObject.Lods[i].LoD[j]));
    end;
  end;
  setlength(FOccluder, aMeshObject.Occluder.Count);
  for i:=0 to aMeshObject.Occluder.Count-1 do begin
    FOccluder[i]:=TGLMesh(aOwner.GetOrCreateResource(aMeshObject.Occluder[i]));
  end;
end;

{ TGLSceneObject }

procedure TGLSceneObject.Apply;
begin

end;

constructor TGLSceneObject.CreateFrom(aOwner: TGLResources;
  aSceneObject: TSceneObject);
var i: integer;
begin
  Create;
  assert(assigned(aOwner) and (aOwner is TGLResources),'Resource manager invalide or not assigned');
  FSceneObject:=aSceneObject;
  aSceneObject.Subscribe(self);
  FIdexInPool := -1;
  FStructureChanged := true;
  setlength(FMeshObjects, aSceneObject.MeshObjects.Count);
  for i:=0 to aSceneObject.MeshObjects.Count-1 do begin
    FMeshObjects[i]:=TGLMeshObject(aOwner.GetOrCreateResource(aSceneObject.MeshObjects[i]));
  end;
end;

procedure TGLSceneObject.Notify(Sender: TObject; Msg: Cardinal;
  Params: pointer);
begin
  inherited;
  case Msg of
    NM_ResourceChanged: if Sender = FSceneObject then FStructureChanged := true;
  end;
end;

{ TGLMaterialPool }

constructor TGLPoolBuffers.Create(aObjectSize: integer; aSingleBufer: boolean; aObjectsCount: integer);
var maxsize,objnum: integer;
begin
  FBuffers:=TList.Create;
  glGetIntegerv(GL_MAX_UNIFORM_BLOCK_SIZE, @maxsize);
  objnum:=maxsize div aObjectSize; FObjectSize:=aObjectSize;
  if (aObjectsCount=-1) or (aObjectsCount>objnum) then FObjectsCount:=objnum
  else FObjectsCount:=aObjectsCount;
  FBuffers.Add(TGLBufferObjectsPool.Create(aObjectSize, FObjectsCount));
  FSingleBufer:=aSingleBufer;
end;

destructor TGLPoolBuffers.Destroy;
begin
  FreeObjectList(FBuffers);
  inherited;
end;

function TGLPoolBuffers.getBuffPool(Index: integer): TGLBufferObjectsPool;
begin
  result:=FBuffers[Index];
end;

procedure TGLPoolBuffers.GetSlot(var aBufferPool: TGLBufferObjectsPool;
  var aSlotIndex: integer);
var i, idx: integer;
    pool: TGLBufferObjectsPool;
begin
  for i:=0 to FBuffers.Count-1 do begin
    pool:=FBuffers[i];
    idx:=pool.GetFreeSlotIndex;
    if idx>=0 then begin aBufferPool:=pool; aSlotIndex:=idx; exit; end;
  end;

  assert(not FSingleBufer, 'UBO buffer full!');

  pool:=TGLBufferObjectsPool.Create(FObjectSize, FObjectsCount);
  aSlotIndex:=pool.GetFreeSlotIndex; aBufferPool:=pool;
  FBuffers.Add(pool);
end;

function TGLPoolBuffers.isExists(const aData: pointer;
  var aBufferPool: TGLBufferObjectsPool; var aSlotIndex: integer): boolean;
var i,idx: integer;
    pool: TGLBufferObjectsPool;
begin
  result:=false; aBufferPool:=nil; aSlotIndex:=-1;
  for i:=0 to FBuffers.Count-1 do begin
    pool:=FBuffers[i]; idx:=pool.isExists(aData);
    if idx>=0 then begin
      result:=true; aBufferPool:=pool;
      aSlotIndex:=idx; exit;
    end;
  end;
end;

procedure TGLSceneObject.UnApply;
begin

end;

procedure TGLSceneObject.UpdateUBO(aPool: TGLBufferObjectsPool);
var
    p: pointer;
begin
  if not FStructureChanged then
    exit;

  if FIdexInPool < 0 then
    FIdexInPool := aPool.GetFreeSlotIndex();

  p := aPool.Buffer.MapRange(GL_MAP_WRITE_BIT or GL_MAP_INVALIDATE_RANGE_BIT,
    aPool.OffsetByIndex(FIdexInPool), SizeOf(mat4)*6);

  // Fill Uniform Buffer Object Data
  with FSceneObject do begin
    move(ModelMatrix.GetAddr^,p^,64); inc(p, 64);
    move(FSceneObject. ViewMatrix.GetAddr^,p^,64); inc(p, 64);
    move(ModelMatrix.GetAddr^,p^,64); inc(p, 64);
    move(ModelMatrix.GetAddr^,p^,64); inc(p, 64);
    move(ModelMatrix.GetAddr^,p^,64); inc(p, 64);
  end;
  aPool.Buffer.UnMap;
  FStructureChanged := False;

  //Fill Uniform Buffer Object Data
  with FSceneObject do begin
    offs:=FUBO.OffsetByName(CUBOTransPropertySemantics[utModel]);
    p:=pointer(integer(FUBOData)+offs);
    move(ModelMatrix.GetAddr^, p^, SizeOf(Mat4));

    offs:=FUBO.OffsetByName(CUBOTransPropertySemantics[utView]);
    p:=pointer(integer(FUBOData)+offs);
    move(ModelMatrix.GetAddr^, p^, SizeOf(Mat4));

    offs:=FUBO.OffsetByName(CUBOTransPropertySemantics[utProjection]);
    p:=pointer(integer(FUBOData)+offs);
    move(ModelMatrix.GetAddr^, p^, SizeOf(Mat4));

    offs:=FUBO.OffsetByName(CUBOTransPropertySemantics[utModelView]);
    p:=pointer(integer(FUBOData)+offs);
    move(ModelMatrix.GetAddr^, p^, SizeOf(Mat4));

    offs:=FUBO.OffsetByName(CUBOTransPropertySemantics[utViewProjection]);
    p:=pointer(integer(FUBOData)+offs);
    move(ModelMatrix.GetAddr^, p^, SizeOf(Mat4));

    offs:=FUBO.OffsetByName(CUBOTransPropertySemantics[utModelViewProjection]);
    p:=pointer(integer(FUBOData)+offs);
    move(ModelMatrix.GetAddr^, p^, SizeOf(Mat4));
  end;
  FBlockBuffer.WriteToPool(FIdexInPool, FUBOData);
end;

{ TGLLight }

procedure TGLLight.Apply;
begin

end;

constructor TGLLight.CreateFrom(aOwner: TGLResources; const aLight: TLightSource);
begin
  Create;
  assert(assigned(aOwner) and (aOwner is TGLResources),'Resource manager invalide or not assigned');
  Owner:=aOwner; FLight:=aLight;
  FLight.Subscribe(self);
  FIdexInPool:=-1;
  FStructureChanged := True;
end;

destructor TGLLight.Destroy;
begin
  // Need to add slot freeing in Lights pool
  inherited;
end;

procedure TGLLight.Notify(Sender: TObject; Msg: Cardinal; Params: pointer);
begin
  inherited;
  case Msg of
    NM_ResourceChanged: if Sender = FLight then FStructureChanged := true;
  end;
end;

procedure TGLLight.UnApply;
begin

end;

procedure TGLLight.UpdateUBO(aPool: TGLBufferObjectsPool);
var
    p: PByte;
begin
  if not FStructureChanged then
    exit;

  if FIdexInPool < 0 then
    FIdexInPool := aPool.GetFreeSlotIndex();

  p := aPool.Buffer.MapRange(GL_MAP_WRITE_BIT or GL_MAP_INVALIDATE_RANGE_BIT,
    aPool.OffsetByIndex(FIdexInPool), SizeOf(vec4)*6);

  // Fill Uniform Buffer Object Data
  with FLight do begin
    move(Position.GetAddr^,p^,16); inc(p, 16);
    move(Ambient.ColorAsAddress^,p^,16); inc(p, 16);
    move(Diffuse.ColorAsAddress^,p^,16); inc(p, 16);
    move(Specular.ColorAsAddress^,p^,16); inc(p, 16);
    move(ConstAttenuation,p^,4); inc(p, 4);
    move(LinearAttenuation,p^,4); inc(p, 4);
    move(QuadraticAttenuation,p^,4); inc(p, 4);
    move(SpotCutOff,p^,4); inc(p, 4);
    move(SpotExponent,p^,4); inc(p, 4);
    move(SceneColor.ColorAsAddress^,p^,16); inc(p, 16);
    move(SpotDirection.GetAddr^,p^,16);
  end;
  aPool.Buffer.UnMap;
  FStructureChanged := False;
end;

initialization

  vGLRender:=TGLRender.Create;
  vRegisteredRenders.RegisterRender(vGLRender);

finalization
  vRegisteredRenders.UnRegisterRender(vGLRender);
  vGLRender.Free;
end.
//Projected Sphere radius
//radius * cot(fov / 2) / Z * ViewerSize
