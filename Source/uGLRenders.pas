 { TODO : Реализовать использование зарегистрированных рендеров и менеджеров ресурсов
          Оптимизировать бинд буфера объектов, убрать лишние переключения используя TMesh.IsIdentityMatrix
 }
unit uGLRenders;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses Classes, uVMath, uBaseGL, uBaseClasses, uRenderResource, uBaseRenders,
     uBaseTypes, uLists, uMiscUtils, uGenericsRBTree, uWorldSpace, dglOpenGL;

Type

  TGLResources = class;

  TGLRender = class;

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
    procedure Apply(aRender: TGLRender);
    procedure UnApply;

    constructor CreateFrom(aOwner: TGLResources; const aMat: TMaterialObject);
    destructor Destroy; override;
  end;

  TGLLight = class (TGLBaseResource)
  private
    FLight: TLightSource;
    FStructureChanged: boolean;
    FIdexInPool: integer;
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
    FMatrixChanged: boolean;
    FIdexInPool: integer;
    procedure UpdateUBO(aParent: TMovableObject; aPool: TGLBufferObjectsPool; aForceUpload: boolean);
  public
    constructor CreateFrom(aOwner: TGLResources; aMesh: TMesh);

    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;
    procedure Apply(aRender: TGLRender);
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
    procedure Apply(aRender: TGLRender);
    procedure UnApply;
  end;

  TGLStaticRender = class (TBaseSubRender)
  private
    function getRender: TGLRender;
  public
    function isSupported(const aClassType: TClass): boolean; override;
    procedure ProcessResource(const Resource: TBaseRenderResource); override;

    class procedure RenderVertexObject(
      const aVertexObject: TGLVertexObject;
      aShaderUsageLogic: TShaderUsageLogic = slUseOwnShader;
      aShaderUsagePriority: TShaderUsagePriority = spUseOwnShaderFirst);

    constructor Create;

    property Render: TGLRender read getRender;
  end;

  TResourceTree = GRedBlackTree<TBaseRenderResource, TGLBaseResource>;

  TGLResources = class (TBaseSubRender)
  private
    FResList: array of TResourceTree;
    FInnerResource: TResourceTree;
    function isInnerResource(const aClassType: TClass): boolean;
    function GetResource(const Resource: TBaseRenderResource): TGLBaseResource;
  public
    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;

    function GetOrCreateResource(const Resource: TBaseRenderResource): TGLBaseResource;
    procedure FreeResource(const Resource: TBaseRenderResource);
    procedure RemoveGLResource(const Resource: TBaseRenderResource);
    procedure ProcessResource(const Res: TBaseRenderResource); override;

    constructor CreateOwned(aRender: TBaseRender); override;
    destructor Destroy; override;
  end;


  TGLRender = class (TBaseRender)
  private
    FCameraPool: TGLBufferObjectsPool;
    FObjectPool: TGLBufferObjectsPool;
    FLightPool: TGLBufferObjectsPool;
    FMaterialPool: TGLBufferObjectsPool;
    FIdexInPool: integer;
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

    procedure BindObjectBuffer(aPoolIndex: integer);

    property CameraPool: TGLBufferObjectsPool read FCameraPool;
    property ObjectPool: TGLBufferObjectsPool read FObjectPool;
    property LightPool: TGLBufferObjectsPool read FLightPool;
    property MaterialPool: TGLBufferObjectsPool read FMaterialPool;
  end;

var
  vGLRender: TGLRender = nil;

implementation

var
  vMaterialPool: TGLPoolBuffers = nil;
  vLightsPool: TGLPoolBuffers = nil;

  vActiveShader: TGLSLShaderProgram = nil;

{ TGLRender }

procedure TGLRender.BindObjectBuffer(aPoolIndex: integer);
begin
  glBindBufferRange(GL_UNIFORM_BUFFER, CUBOSemantics[ubObject].Location,
    FObjectPool.Buffer.Id, FObjectPool.OffsetByIndex(aPoolIndex),
    FObjectPool.ObjectSize);
end;

function TGLRender.CheckVisibility(const aFrustum: TFrustum;
  const aExtents: TExtents): boolean;
begin
  result:=inherited;
end;

constructor TGLRender.Create;
begin
  inherited;
  RegisterSubRender(TGLStaticRender.CreateOwned(Self));
  FResourceManager:=TGLResources.CreateOwned(Self);
  FIdexInPool := -1;
end;

destructor TGLRender.Destroy;
begin
  FCameraPool.Free;
  FObjectPool.Free;
  FLightPool.Free;
  FMaterialPool.Free;
  FResourceManager.Free;
  inherited;
end;

function TGLRender.isSupported(const aAPI: TApiVersion): boolean;
begin
  result:= (aAPI.GAPI = avGL) and (aAPI.Version>=420);
end;

procedure TGLRender.PrepareResources(const aScene: TSceneGraph);
var i: integer; p: PByte;
    SceneItem: TBaseSceneItem;
    res: TBaseRenderResource;
    glres: TGLBaseResource;
    mat: TMatrix;
begin
  inherited;
  { TODO :
    Реализовать сравнение состояний текущей библиотеки и предыдущего
    снимка (через хэш библиотеки или рассылку уведомлений?)
    Делать вычитку ресурсов только при несовпадении хэша. }

  if not Assigned(FObjectPool) then begin
    FCameraPool := TGLBufferObjectsPool.Create(SizeOf(mat4)*3, 8);
    FObjectPool := TGLBufferObjectsPool.Create(SizeOf(mat4)*3, 2000);
    FLightPool := TGLBufferObjectsPool.Create(SizeOf(vec4)*6, 1000);
    FMaterialPool := TGLBufferObjectsPool.Create(SizeOf(vec4)*5, 100);
  end;

  if FIdexInPool < 0 then
    FIdexInPool := FCameraPool.GetFreeSlotIndex();

  p := FCameraPool.Buffer.MapRange(GL_MAP_WRITE_BIT or GL_MAP_INVALIDATE_RANGE_BIT,
    FCameraPool.OffsetByIndex(FIdexInPool), FCameraPool.ObjectSize);

  // Fill Uniform Buffer Object Data
  mat := UpdateWorldMatrix(aScene.Camera);
  with aScene.Camera do begin
    //move(WorldMatrix.GetAddr^,p^,64); inc(p, 64);
    move(mat.Matrix4, p^,64); inc(p, 64);
    move(ProjMatrix.GetAddr^,p^,64); inc(p, 64);
    mat := mat * ProjMatrix;
    move(mat.Matrix4, p^,64);
  end;
  FCameraPool.Buffer.UnMap;
  glBindBufferRange(GL_UNIFORM_BUFFER, CUBOSemantics[ubCamera].Location,
    FCameraPool.Buffer.Id, FCameraPool.OffsetByIndex(FIdexInPool), FCameraPool.ObjectSize);

  //создаем ресурсы для всех материалов сцены
  for i:=0 to aScene.MaterialsCount-1 do begin
    res:=aScene.Materials[i];
    glres := FResourceManager.GetOrCreateResource(res);
    // обновляем даные в видеопамяти
    TGLMaterial(glres).UpdateUBO(FMaterialPool);
  end;
  //создаем ресурсы для всех источников света сцены
  for i:=0 to aScene.LightsCount-1 do begin
    res:=aScene.Lights[i];
    glres := FResourceManager.GetOrCreateResource(res);
    // обновляем даные в видеопамяти
    TGLLight(glres).UpdateUBO(FLightPool);
    // ВРЕМЕННО!
    glBindBufferRange(GL_UNIFORM_BUFFER, CUBOSemantics[ubLights].Location,
      FLightPool.Buffer.Id, FLightPool.OffsetByIndex(FIdexInPool),
      FLightPool.ObjectSize);
  end;
  //создаем ресурсы для всех объектов сцены
  for i:=0 to aScene.Count-1 do begin
    SceneItem:=aScene[i];
    glres := FResourceManager.GetOrCreateResource(SceneItem);
    // обновляем даные в видеопамяти
    TGLSceneObject(glres).UpdateUBO(FObjectPool);
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
  result:= inherited UpdateWorldMatrix(MovableObject, UseMatrix);
end;

procedure TGLRender.UploadResource(const Res: TBaseRenderResource);
begin
  inherited;
end;

{ TGLStaticRender }

constructor TGLStaticRender.Create;
begin
  inherited Create;
end;


function TGLStaticRender.getRender: TGLRender;
begin
  Result := TGLRender(Owner);
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
      Apply(Render);
      //рендерим объект сцены с применением материалов
      for i:=0 to length(FMeshObjects)-1 do begin
        MeshObject:=FMeshObjects[i];
        for j:=0 to length(MeshObject.FLods[0])-1 do begin
          Mesh:=MeshObject.FLods[0,j];
          Mesh.Apply(Render);
          Mesh.FMaterialObject.Apply(Render);
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

function ResourceComparer(const Item1, Item2: TBaseRenderResource): Integer;
begin
  Result := CompareInteger(Item1.Order, Item2.Order);
end;

constructor TGLResources.CreateOwned(aRender: TBaseRender);
var i: integer;
begin
  inherited CreateOwned(aRender);
  FSupportedResources.Clear;
  //Registering supported resources

  //Main resource
  FSupportedResources.Add(TShaderProgram);
  FSupportedResources.Add(TVertexObject);

  FSupportedResources.Add(TMeshObject);
  FSupportedResources.Add(TMaterialObject);
  FSupportedResources.Add(TLightSource);
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
    FResList[i]:= TResourceTree.Create(ResourceComparer, nil);
  //Inner resource list: TBufferObject, TAttribObject, TTexture, TMesh
  FInnerResource:=TResourceTree.Create(ResourceComparer, nil);

end;

destructor TGLResources.Destroy;
var i, j: integer;
    res: TBaseRenderResource;
    glres: TGLBaseResource;
begin
  for i := 0 to length(FResList)-1 do begin
    if FResList[i].Count > 0 then begin
      res:=FResList[i].First;
      if assigned(res) then begin
        if FResList[i].Find(res, glres)then begin
          if glres.Owner=self then glres.Free;
        end;
        for j := 1 to FResList[i].Count - 1 do
          if FResList[i].NextKey(res, glres) and (glres.Owner=self) then glres.Free;
      end;
    end;
    FResList[i].Free;
  end;
  res:=FInnerResource.First;
  if assigned(res) then begin
    FInnerResource.Find(res, glres);
    if glres.Owner=self then glres.Free;
    for j := 1 to FInnerResource.Count - 1 do
      if FInnerResource.NextKey(res, glres) and (glres.Owner=self ) then glres.Free;
  end;
  FInnerResource.Free;
  inherited;
end;

procedure TGLResources.FreeResource(const Resource: TBaseRenderResource);
var idx: integer;
    glres: TGLBaseResource;
begin
  if not assigned(Resource) then exit;

  idx:=FSupportedResources.IndexOf(Resource.ClassType);
  if idx <0 then exit;

  //Resource exists?
  if isInnerResource(Resource.ClassType) then begin
    if FInnerResource.Find(Resource, glres) then begin
      FInnerResource.Delete(Resource);
      FreeAndNil(glres);
    end;
  end else if FResList[idx].Find(Resource, glres) then begin
      FResList[idx].Delete(Resource);
      FreeAndNil(glres);
  end;
end;

function TGLResources.GetOrCreateResource(
  const Resource: TBaseRenderResource): TGLBaseResource;
var idx: integer;
    glres: TGLBaseResource;
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
    glres:=TGLSLShaderProgram.CreateFrom(Resource as TShaderProgram);
    FResList[idx].Add(Resource, glres);
    glres.Owner:=self;
    Result:=glres;
    Resource.Subscribe(self);
    exit;
  end;

  if Resource.ClassType = TVertexObject then begin
    //Create Vertex object
    glres:=TGLVertexObject.CreateFrom(Resource as TVertexObject);
    FResList[idx].Add(Resource, glres);
    glres.Owner:=self;
    Result:=glres;
    Resource.Subscribe(self);
    exit(glres);
  end;

  if Resource.ClassType = TMesh then begin
    //Create Mesh
    glres:=TGLMesh.CreateFrom(self, Resource as TMesh);
    FInnerResource.Add(Resource, glres);
    glres.Owner:=self;
    Result:=glres;
    Resource.Subscribe(self);
    exit(glres);
  end;

  if Resource.ClassType = TMeshObject then begin
    //Create Mesh object
    glres:=TGLMeshObject.CreateFrom(self, Resource as TMeshObject);
    FResList[idx].Add(Resource, glres);
    glres.Owner:=self;
    Result:=glres;
    Resource.Subscribe(self);
    exit(glres);
  end;

  if Resource.ClassType = TSceneObject then begin
    //Create Scene object
    glres:=TGLSceneObject.CreateFrom(self, Resource as TSceneObject);
    FResList[idx].Add(Resource, glres);
    glres.Owner:=self;
    Result:=glres;
    Resource.Subscribe(self);
    exit(glres);
  end;

  if Resource.ClassType = TMaterialObject then begin
    //Create Material object
    glres:=TGLMaterial.CreateFrom(self, Resource as TMaterialObject);
    FResList[idx].Add(Resource, glres);
    glres.Owner:=self;
    Result:=glres;
    Resource.Subscribe(self);
    exit(glres);
  end;

  if Resource.ClassType = TLightSource then begin
    //Create Material object
    glres:=TGLLight.CreateFrom(self, Resource as TLightSource);
    FResList[idx].Add(Resource, glres);
    glres.Owner:=self;
    Result:=glres;
    Resource.Subscribe(self);
    exit(glres);
  end;

  if Resource.ClassType = TTexture then begin
    result := nil;
    exit;
  end;

end;

function TGLResources.GetResource(
  const Resource: TBaseRenderResource): TGLBaseResource;
var idx: integer;
    glres: TGLBaseResource;
begin
  inherited;
  result:=nil; idx:=FSupportedResources.IndexOf(Resource.ClassType);
  if idx < 0 then exit;

  if isInnerResource(Resource.ClassType) then begin
    if FInnerResource.Find(Resource, glres) then exit(glres);
  end else begin
    if FResList[idx].Find(Resource, glres) then exit(glres);
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
      NM_ObjectDestroyed: begin
        if assigned(res.BaseResource) then
          RemoveGLResource(res.BaseResource)
      end;
    end;
  end;
end;

procedure TGLResources.ProcessResource(const Res: TBaseRenderResource);
begin
  inherited;

end;

procedure TGLResources.RemoveGLResource(const Resource: TBaseRenderResource);
var idx: integer;
    glres: TGLBaseResource;
begin
  if not assigned(Resource) then exit;

  idx:=FSupportedResources.IndexOf(Resource.ClassType);
  if idx <0 then exit;

  //Resource exists?
  if isInnerResource(Resource.ClassType) then begin
    if FInnerResource.Find(Resource, glres) then
      FInnerResource.Delete(Resource);
  end else
    if FResList[idx].Find(Resource, glres) then
      FResList[idx].Delete(Resource);
end;

{ TGLMesh }

procedure TGLMesh.Apply(aRender: TGLRender);
begin
  aRender.BindObjectBuffer(FIdexInPool);
end;

constructor TGLMesh.CreateFrom(aOwner: TGLResources; aMesh: TMesh);
begin
  Create;
  assert(assigned(aOwner) and (aOwner is TGLResources),
    'Resource manager invalide or not assigned');
  Owner:=aOwner; FMesh:=aMesh;
  BaseResource := aMesh;
  FMesh.Subscribe(aOwner);
  FVertexObject:=TGLVertexObject(aOwner.GetOrCreateResource(FMesh.VertexObject));
  FMaterialObject:=TGLMaterial(aOwner.GetOrCreateResource(FMesh.MaterialObject));
  FIdexInPool := -1;
  FMatrixChanged := true;
end;

procedure TGLMesh.Notify(Sender: TObject; Msg: Cardinal; Params: pointer);
begin
  inherited;
    case Msg of
      NM_ObjectDestroyed: begin
        Assert(Sender <> FMesh, 'You can not destroy the owner ahead of subscribers.');
        FMesh.UnSubscribe(Self);
        FMesh := nil;
      end;
      NM_WorldMatrixChanged: begin
        if Sender = FMesh then
          FMatrixChanged := true;
      end;
    end;
end;

procedure TGLMesh.UpdateUBO(aParent: TMovableObject; aPool: TGLBufferObjectsPool; aForceUpload: boolean);
var
    p: PByte;
    mat: TMatrix;
begin
  if not (FMatrixChanged or aForceUpload) then
    exit;

  if FIdexInPool < 0 then
    FIdexInPool := aPool.GetFreeSlotIndex();

  p := aPool.Buffer.MapRange(GL_MAP_WRITE_BIT or GL_MAP_INVALIDATE_RANGE_BIT,
    aPool.OffsetByIndex(FIdexInPool), aPool.ObjectSize);

  // Calculate final world matrix
  mat := FMesh.LocalMatrix * aParent.WorldMatrix;
  // Fill Uniform Buffer Object Data
  move(mat.GetAddr^, p^, 64); inc(p, 64);
  move(mat.Invert.GetAddr^, p^,64); inc(p, 64);
  move(mat.Normalize.GetAddr^, p^,64);
  aPool.Buffer.UnMap;
  FMatrixChanged := False;
end;

{ TGLMaterial }

procedure TGLMaterial.Apply(aRender: TGLRender);
var
    tb: TGLUniformBlock;
begin
  if assigned(FShader) then begin
    FShader.Apply; vActiveShader:=FShader; end else exit;

  tb := FShader.UniformBlocks.GetUBOByName(CUBOSemantics[ubCamera].Name);
  if Assigned(tb) then begin
    glUniformBlockBinding(FShader.Id, tb.BlockIndex, CUBOSemantics[ubCamera].Location);
  end;

  tb := FShader.UniformBlocks.GetUBOByName(CUBOSemantics[ubObject].Name);
  if Assigned(tb) then begin
    glUniformBlockBinding(FShader.Id, tb.BlockIndex, CUBOSemantics[ubObject].Location);
  end;

  tb := FShader.UniformBlocks.GetUBOByName(CUBOSemantics[ubLights].Name);
  if Assigned(tb) then begin
    glUniformBlockBinding(FShader.Id, tb.BlockIndex, CUBOSemantics[ubLights].Location);
  end;

  tb := FShader.UniformBlocks.GetUBOByName(CUBOSemantics[ubMaterial].Name);
  if Assigned(tb) then begin
    aRender.MaterialPool.BindUBO(FIdexInPool, tb);
    glUniformBlockBinding(FShader.Id, tb.BlockIndex, CUBOSemantics[ubMaterial].Location);
  end;
end;

constructor TGLMaterial.CreateFrom(aOwner: TGLResources;
  const aMat: TMaterialObject);
var i: integer;
begin
  Create;
  assert(assigned(aOwner) and (aOwner is TGLResources),'Resource manager invalide or not assigned');
  Owner:=aOwner; FMaterialObject:=aMat;
  BaseResource := aMat;
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
end;

procedure TGLMaterial.UpdateUBO(aPool: TGLBufferObjectsPool);
var
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
  BaseResource := aMeshObject;
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

procedure TGLSceneObject.Apply(aRender: TGLRender);
begin
  aRender.BindObjectBuffer(FIdexInPool);
end;

constructor TGLSceneObject.CreateFrom(aOwner: TGLResources;
  aSceneObject: TSceneObject);
var i: integer;
begin
  Create;
  assert(assigned(aOwner) and (aOwner is TGLResources),'Resource manager invalide or not assigned');
  FSceneObject:=aSceneObject;
  BaseResource := aSceneObject;
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
var i, j: integer;
    p: PByte;
    MeshObject: TGLMeshObject;
    Mesh: TGLMesh;
begin
  if FStructureChanged then
  begin
    if FIdexInPool < 0 then
      FIdexInPool := aPool.GetFreeSlotIndex();

    p := aPool.Buffer.MapRange(GL_MAP_WRITE_BIT or GL_MAP_INVALIDATE_RANGE_BIT,
      aPool.OffsetByIndex(FIdexInPool), aPool.ObjectSize);

    // Fill Uniform Buffer Object Data
    with FSceneObject do begin
      move(WorldMatrix.GetAddr^,p^,64); inc(p, 64);
      move(InvWorldMatrix.GetAddr^,p^,64); inc(p, 64);
      move(WorldMatrix.Normalize.GetAddr^, p^,64);
    end;
    aPool.Buffer.UnMap;
  end;

  // Update meshes local matrices
  for i:=0 to length(FMeshObjects)-1 do begin
    MeshObject:=FMeshObjects[i];
    for j:=0 to length(MeshObject.FLods[0])-1 do begin
      Mesh:=MeshObject.FLods[0,j];
      Mesh.UpdateUBO(FSceneObject, aPool, FStructureChanged);
    end;
  end;

  FStructureChanged := False;
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
  BaseResource := aLight;
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
    aPool.OffsetByIndex(FIdexInPool), aPool.ObjectSize);

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
    //move(SceneColor.ColorAsAddress^,p^,16); inc(p, 16);
    move(SpotDirection.GetAddr^,p^,12);
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
