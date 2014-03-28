 { TODO : Реализовать использование зарегистрированных рендеров и менеджеров ресурсов
          Оптимизировать бинд буфера объектов, убрать лишние переключения используя TMesh.IsIdentityMatrix
          Заменить глобальную переменную vActiveShader на состояние (свойство) рендера
 }
unit uGLRenders;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses Windows, Classes, uVMath, uBaseGL, uBaseClasses, uRenderResource, uBaseRenders,
     uBaseTypes, uLists, uMiscUtils, uGenericsRBTree, uWorldSpace, dglOpenGL,
     uPersistentClasses;

const
  MAX_OBJECTS_RESERVED = 100000;

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

  TGLBuiltinUniform = class(TGLBaseResource)
  protected
    FUniform: TBaseBuiltinUniform;
  public
    constructor CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource); override;
  end;

  TGLUniformLightNumber = class(TGLBuiltinUniform)
  public
    procedure Apply(aRender: TBaseRender); override;
  end;

  TGLSLShaderProgramExt = class(TGLSLShaderProgram)
  private
    FBuiltinUniforms: TObjectList;
  public
    constructor CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource); override;
    destructor Destroy; override;

    procedure Apply(aRender: TBaseRender); overload; override;
  end;

  TGLFrameBufferObjectExt = class(TGLFrameBufferObject)
  private
    FFrameBuffer: TFrameBuffer;
    FStructureChanged: Boolean;
  public
    constructor CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource); override;
    destructor Destroy; override;
    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;
    procedure Update(aRender: TBaseRender); override;
  end;

  TGLCamera = class(TGLBaseResource)
  private
    FCamera: TSceneCamera;
    FFrameBuffer: TGLFrameBufferObjectExt;
    FIdexInPool: integer;
    FStructureChanged: boolean;
  public
    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;
    procedure Update(aRender: TBaseRender); override;
    procedure Apply(aRender: TBaseRender); override;
    procedure UnApply(aRender: TBaseRender); override;

    constructor CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource); override;
    destructor Destroy; override;
  end;

  TGLMaterial = class (TGLBaseResource)
  private
    FMaterialObject: TMaterialObject;
    FTextures: array of TGLTextureObject;
    FSamplers: array of TGLTextureSampler;
    FBlend: TCustomBlending;
    FShader: TGLSLShaderProgramExt;
    FIdexInPool: integer;
    FStructureChanged: boolean;
  public
    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;
    procedure Update(aRender: TBaseRender); override;
    procedure Apply(aRender: TBaseRender); override;
    procedure UnApply(aRender: TBaseRender); override;

    constructor CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource); override;
    destructor Destroy; override;
  end;

  TGLMesh = class (TGLBaseResource)
  private
    FMesh: TMesh;
    FVertexObject: TGLVertexObject;
    FMaterialObject: TGLMaterial;
  public
    constructor CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource); override;
    destructor Destroy; override;

    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;
  end;

  TMeshState = record
    Mesh: TGLMesh;
    IsIdentity: boolean;
    MatrixChanged: boolean;
  end;

  TGLMeshObject = class (TGLBaseResource)
  private
    FMeshObject: TMeshObject;
    FLods: array of array of TMeshState;
    FOccluder: array of TGLMesh;
    function GetMesh(const aLevel, anIndex: integer): TGLMesh;
    function GetLodCount: integer;
    function GetMeshCount(const aLevel: integer): integer;
  public
    constructor CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource); override;
    property MeshObject: TMeshObject read FMeshObject;
    property Lods[const aLevel, anIndex: integer]: TGLMesh read GetMesh;
    property LodCount: integer read GetLodCount;
    property MeshCount[const aLevel: integer]: integer read GetMeshCount;
  end;

  TGLMovableObject = class (TGLBaseResource)
  protected
    FMovableObject: TMovableObject;
    FBaseTfPoolIndex: integer;
    FParentObjectPoolIndex: integer;
    FObjectPoolIndex: integer;
    FLightIdxPoolIndex: integer;
    FTransformChanged: boolean;
  public
    procedure Update(aRender: TBaseRender); override;
  end;

  TGLLight = class (TGLMovableObject)
  private
    FLight: TLightSource;
    FLightPropChanged: boolean;
    FLightPoolIndex: integer;
  public
    LightSphereRadius: single;

    constructor CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource); override;
    destructor Destroy; override;
    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;

    procedure Update(aRender: TBaseRender); override;

    property LightPoolIndex: Integer read FLightPoolIndex;
  end;

  TGLSceneObject = class (TGLMovableObject)
  private
    FSceneObject: TSceneObject;
    FMeshObjects: array of TGLMeshObject;
    // Индексы слотов источников света в пуле индексов освещения
    FEffectedLights: TLightIndexArray;
    FEffectedLightNum: Integer;
  public
    constructor CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource); override;
    destructor Destroy; override;
    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;

    procedure Update(aRender: TBaseRender); override;
    procedure Apply(aRender: TBaseRender); override;
  end;

  TGLSceneObjectRender = class (TBaseSubRender)
  private
    function getRender: TGLRender;
  protected
    procedure ProclaimSupport; override;
  public
    procedure ProcessResource(aResource: TBaseGraphicResource); override;

    class procedure RenderVertexObject(
      const aVertexObject: TGLVertexObject;
      aShaderUsageLogic: TShaderUsageLogic = slUseOwnShader;
      aShaderUsagePriority: TShaderUsagePriority = spUseOwnShaderFirst);

    property Render: TGLRender read getRender;
  end;

  TResourceTree = GRedBlackTree<TBaseRenderResource, TGLBaseResource>;

  TGLResources = class (TBaseSubRender)
  private
    FResources: TResourceTree;
    function GetResource(const Resource: TBaseRenderResource): TGLBaseResource;
    function CreateResource(const Resource: TBaseRenderResource): TGLBaseResource;
  protected
    procedure ProclaimSupport; override;
  public
    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;

    function GetOrCreateResource(const Resource: TBaseRenderResource): TGLBaseResource;
    procedure FreeResource(const Resource: TBaseRenderResource);
    procedure RemoveGLResource(const Resource: TBaseRenderResource);
    procedure ProcessResource(aResource: TBaseGraphicResource); override;

    constructor CreateOwned(aOwner: TObject = nil); override;
    destructor Destroy; override;
  end;

  TDrawCommand = record
    mesh: TGLMesh;
    worldTransfMethod: (wtmDefault, wtmInstance, wtmSphere, wtmCylindr);
    objectIndex: Integer;
    instanceMatrix: PMat4;
    lightIndex: Integer;
  end;

  TDrawCommandList = TDataList<TDrawCommand>;

  TGLRender = class (TBaseRender)
  private
    FCameraPool: TGLBufferObjectsPool;
    FObjectPool: TGLBufferObjectsPool;
    FLightPool: TGLBufferObjectsPool;
    FLightIndicesPool: TGLBufferObjectsPool;
    FMaterialPool: TGLBufferObjectsPool;
    FBaseTfPool: TGLBufferObjectsPool;
    FObjectIndices: TGLBufferObject;

    FMatrixMultiplier: TGLSLShaderProgram;
    FInvocationGroups: array of TObjectList;
    FComputeTfByShader: boolean;
    FMaxWorkGroupSize: integer;
    FTfChangesCounter: integer;
    FDrawCommands: TDrawCommandList;
  protected
    FResourceManager: TGLResources;

    procedure PrepareResources(const aScene: TSceneGraph);
    procedure UploadResource(const Res: TBaseRenderResource); override;
    procedure ProcessResource(const aResource: TBaseRenderResource); override;
    procedure ExpandHierarchyLevels(const amount: integer);
  public
    constructor Create; override;
    destructor Destroy; override;

    function IsSupported(const aAPI: TApiVersion): boolean; override;

    function CheckVisibility(const aFrustum: TFrustum;
      const aExtents: TExtents): boolean; override;
    procedure ProcessScene(const aScene: TSceneGraph); override;

    procedure BindCameraBuffer(aPoolIndex: integer);
    procedure BindObjectBuffer(aPoolIndex: integer);
    procedure AddDrawCommand(const aCommand: TDrawCommand);
    property ComputeTransformByShader: boolean read FComputeTfByShader;

    property ResourceManager: TGLResources read FResourceManager;
    property CameraPool: TGLBufferObjectsPool read FCameraPool;
    property ObjectPool: TGLBufferObjectsPool read FObjectPool;
    property LightPool: TGLBufferObjectsPool read FLightPool;
    property LightIndicesPool: TGLBufferObjectsPool read FLightIndicesPool;
    property MaterialPool: TGLBufferObjectsPool read FMaterialPool;
  end;

  TGLGlowEffect = class(TGLBaseResource)
  private
    FEffect: TBaseRenderResource;
    FShader: TGLSLShaderProgramExt;
    FBlurShader: TGLSLShaderProgramExt;
    FFrameH: TGLFrameBufferObject;
    FFrameV: TGLFrameBufferObject;
    FImageHolder: TImageHolder;
    FBluredH: TGLTextureObject;
    FBluredV: TGLTextureObject;
    FVertexObject: TGLVertexObject;
    FSampler: TGLTextureSampler;
    FStructureChanged: boolean;

    fbh,fbv: TFrameBuffer;
    texv,texh: TTexture;

  public
    constructor CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource); override;
    destructor Destroy; override;

    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;

    procedure Update(aRender: TBaseRender); override;
    procedure Apply(aRender: TBaseRender); override;
  end;

var
  vGLRender: TGLRender = nil;

implementation

uses
  uEffectsPipeline, uStorage, uShaderGen;

{ TGLRender }

procedure TGLRender.AddDrawCommand(const aCommand: TDrawCommand);
begin
  FDrawCommands.Add(aCommand);
end;

procedure TGLRender.BindCameraBuffer(aPoolIndex: integer);
begin
  glBindBufferRange(GL_UNIFORM_BUFFER, CUBOSemantics[ubCamera].Location,
    FCameraPool.Buffer.Id, FCameraPool.OffsetByIndex(aPoolIndex),
    FCameraPool.ObjectSize);
end;

procedure TGLRender.BindObjectBuffer(aPoolIndex: integer);
begin
//  glBindBufferRange(GL_UNIFORM_BUFFER, CUBOSemantics[ubObject].Location,
//    FObjectPool.Buffer.Id,
//    FObjectPool.OffsetByIndex(aPoolIndex),
//    FObjectPool.ObjectSize);
  TGLSLShaderProgram.ActiveShader.SetUniform('ObjectId', aPoolIndex);
end;

function TGLRender.CheckVisibility(const aFrustum: TFrustum;
  const aExtents: TExtents): boolean;
begin
  result:=inherited;
end;

constructor TGLRender.Create;
var
  i: integer;
begin
  inherited;
  RegisterSubRender(TGLSceneObjectRender.CreateOwned(Self));
  FResourceManager:=TGLResources.CreateOwned(Self);
  FCameraPool := nil;
  FObjectPool := nil;
  FLightPool := nil;
  FLightIndicesPool := nil;
  FMaterialPool := nil;
  FBaseTfPool := nil;
  FObjectIndices := nil;
  FDrawCommands := TDrawCommandList.Create;
  SetLength(FInvocationGroups, 3);
  for i := 0 to High(FInvocationGroups) do
    FInvocationGroups[i] := TObjectList.Create;
end;

destructor TGLRender.Destroy;
var
  i: integer;
begin
  if assigned(FCameraPool) then FCameraPool.Free;
  if assigned(FObjectPool) then FObjectPool.Free;
  if assigned(FLightPool) then FLightPool.Free;
  if assigned(FLightIndicesPool) then FLightIndicesPool.Free;
  if assigned(FMaterialPool) then FMaterialPool.Free;
  if assigned(FBaseTfPool) then FBaseTfPool.Free;
  if assigned(FObjectIndices) then FObjectIndices.Free;


  if assigned(FResourceManager) then FResourceManager.Free;
  FDrawCommands.Free;
  for i := 0 to High(FInvocationGroups) do
    FInvocationGroups[i].Free;
  inherited;
end;

procedure TGLRender.ExpandHierarchyLevels(const amount: integer);
var
  i, j: integer;
begin
  i := Length(FInvocationGroups);
  SetLength(FInvocationGroups, i + amount + 1);
  for j := i to High(FInvocationGroups) do
    FInvocationGroups[j] := TObjectList.Create;
end;

function TGLRender.isSupported(const aAPI: TApiVersion): boolean;
begin
  result:= (aAPI.GAPI = avGL) and (aAPI.Version>=420);
end;



procedure TGLRender.PrepareResources(const aScene: TSceneGraph);
const
  DEFAULT_BASE_TRANSFORM: TBaseTransform = (
    scale: (( 1,0,0,0 ),( 0,1,0,0 ),( 0,0,1,0 ),( 0,0,0,1 ));
    rotation: (( 1,0,0,0 ),( 0,1,0,0 ),( 0,0,1,0 ),( 0,0,0,1 ));
    translation: (( 1,0,0,0 ),( 0,1,0,0 ),( 0,0,1,0 ),( 0,0,0,1 ));
    model: (( 1,0,0,0 ),( 0,1,0,0 ),( 0,0,1,0 ),( 0,0,0,1 ));
    );
var i, j, noLoadCount: integer;
    SceneItem: TBaseSceneItem;
    movable: TMovableObject absolute SceneItem;
    res: TBaseRenderResource;
    glres: TGLBaseResource;
    effects: TEffectPipeline;
    p: ^Vec4i;
    glMovable: TGLMovableObject;
    Invocation, WorkGroupNum: integer;
    pwt: PWorldTransform;
    WorkGroupSizeX: Integer;
    WorkGroupSizeY: Integer;

  procedure DownToTree(const anItems: TSceneItemList);
  var ii: integer;
  begin
    for ii:=0 to anItems.Count-1 do begin
      SceneItem:=anItems[ii];
      if Assigned(SceneItem) then
      begin
        glres := FResourceManager.GetOrCreateResource(SceneItem);
        // обновляем даные в видеопамяти
        if Assigned(glres) then glres.Update(Self);
        DownToTree(SceneItem.Childs);
      end;
    end;
  end;

begin
  inherited;
  { TODO :
    Реализовать сравнение состояний текущей библиотеки и предыдущего
    снимка (через хэш библиотеки или рассылку уведомлений?)
    Делать вычитку ресурсов только при несовпадении хэша. }

  if not Assigned(FCameraPool) then
    FCameraPool := TGLBufferObjectsPool.Create(SizeOf(TCameraTransform), 8);

  if not Assigned(FObjectPool) then begin
    FObjectPool := TGLBufferObjectsPool.Create(SizeOf(TWorldTransform), MAX_OBJECTS_RESERVED, btShaderStorage);
    // Делаем резерв в пуле на случай когда количество изменений меньше количества нитей
    // Количество нитей 32, в случае одного объекта нужно 31 слот для холостых записей
    for i := 1 to 31 do FObjectPool.GetFreeSlotIndex;
    pwt := FObjectPool.Buffer.Map(GL_WRITE_ONLY);
    for i := 0 to FObjectPool.ObjectCount - 1 do begin
      pwt.world := MatIdentity;
      pwt.invWorld := MatIdentity;
      pwt.worldNormal := MatIdentity;
      pwt.worldT := MatIdentity;
      pwt.pivot := MatIdentity;
      pwt.invPivot := MatIdentity;
      Inc(pwt);
    end;
    FObjectPool.Buffer.UnMap;
  end;

  if not Assigned(FLightPool) then
    FLightPool := TGLBufferObjectsPool.Create(SizeOf(TLightProp), 1000, btShaderStorage);

  if not Assigned(FLightIndicesPool) then
    FLightIndicesPool := TGLBufferObjectsPool.Create(SizeOf(TLightIndices), MAX_OBJECTS_RESERVED, btShaderStorage);

  if not Assigned(FMaterialPool) then
    FMaterialPool := TGLBufferObjectsPool.Create(SizeOf(TMaterialProp), 100);

  if not Assigned(FBaseTfPool) then begin
    FBaseTfPool := TGLBufferObjectsPool.Create(SizeOf(TBaseTransform), MAX_OBJECTS_RESERVED);
    // Делаем резер в пуле на случай когда количество изменений меньше количества нитей
    i := FBaseTfPool.GetFreeSlotIndex;
    FBaseTfPool.WriteToPool(i, @DEFAULT_BASE_TRANSFORM);
  end;

  if not Assigned(FMatrixMultiplier) then begin
    glres := FResourceManager.GetOrCreateResource(ShaderGenerator.GenMatrixMultiplier);
    FMatrixMultiplier := TGLSLShaderProgram(glres);
    FMatrixMultiplier.LinkShader;
    FComputeTfByShader := FMatrixMultiplier.IsLinked;
    glGetIntegeri_v(GL_MAX_COMPUTE_WORK_GROUP_SIZE, 0, @FMaxWorkGroupSize);
  end;

  if not Assigned(FObjectIndices) then begin
    FObjectIndices := TGLBufferObject.Create(btShaderStorage);
    FObjectIndices.Allocate(MAX_OBJECTS_RESERVED*SizeOf(Vec4i), nil);
  end;

  //создаем ресурсы для всех камер сцены
  for i:=0 to aScene.CamerasCount-1 do begin
    res:=aScene.Cameras[i];
    glres := FResourceManager.GetOrCreateResource(res);
    // обновляем даные в видеопамяти
    if assigned(glRes) then glres.Update(Self);
    effects := aScene.Cameras[i].EffectPipeline;
    if Assigned(effects) then begin
      for j := 0 to effects.EffectCount - 1 do begin
        glres := FResourceManager.GetOrCreateResource(effects.Effects[j]);
        glres.Update(Self);
      end;
    end;
  end;

  //создаем ресурсы для всех материалов сцены
  for i:=0 to aScene.MaterialsCount-1 do begin
    res:=aScene.Materials[i];
    glres := FResourceManager.GetOrCreateResource(res);
    // обновляем даные в видеопамяти
    if assigned(glRes) then glres.Update(Self);
  end;

  //создаем ресурсы для всех объектов сцены
  FCurrentCamera := aScene.Camera;
  for i := 0 to High(FInvocationGroups) do FInvocationGroups[i].Flush;
  DownToTree(aScene.Root.Childs);

  if ComputeTransformByShader then begin
    FTfChangesCounter := 0;
    p := nil;

    for i := 0 to High(FInvocationGroups) do begin
      if FInvocationGroups[i].Count > 0 then begin
        if p = nil then p := FObjectIndices.Map(GL_WRITE_ONLY);
        for j := 0 to FInvocationGroups[i].Count - 1 do begin
          glMovable := TGLMovableObject(FInvocationGroups[i].Items[j]);
          p[0] := glMovable.FBaseTfPoolIndex;
          p[1] := glMovable.FParentObjectPoolIndex;
          p[2] := glMovable.FObjectPoolIndex;
          Inc(p);
          Inc(FTfChangesCounter);
        end;
        // Холостые вычисления чтобы дополнить группу нитями кратно 32
        noLoadCount := FInvocationGroups[i].Count mod 32;
        if noLoadCount > 0 then begin
          for j := 0 to 31 - noLoadCount do begin
            p[0] := 0;
            p[1] := 0;
            p[2] := j;
            Inc(p);
            Inc(FTfChangesCounter);
          end;
        end;
      end;
    end;

    Assert(FTfChangesCounter < MAX_OBJECTS_RESERVED);

    if p <> nil then FObjectIndices.UnMap;

    if FTfChangesCounter > 0 then begin
      FBaseTfPool.Buffer.BindBase(btShaderStorage, 0);
      FObjectPool.Buffer.BindBase(btShaderStorage, 1); // to read
      FObjectPool.Buffer.BindBase(btShaderStorage, 2); // to write
      FObjectIndices.BindBase(btShaderStorage, 3);

      FMatrixMultiplier.Bind;
      Invocation := 0;
      for i := 0 to High(FInvocationGroups) do begin
        if FInvocationGroups[i].Count > 0 then begin
          WorkGroupNum := FInvocationGroups[i].Count div 32;
          WorkGroupSizeX := WorkGroupNum mod FMaxWorkGroupSize;
          WorkGroupSizeY := WorkGroupNum div FMaxWorkGroupSize;
          FMatrixMultiplier.SetUniform('Invocation', Invocation);
          glDispatchCompute(WorkGroupSizeX+1, WorkGroupSizeY+1, 1);
          Inc(Invocation, WorkGroupNum+1);
        end;
      end;
    end;
  end;
end;

procedure TGLRender.ProcessResource(const aResource: TBaseRenderResource);
var i: integer;
    Render: TBaseSubRender;
    grRes: TBaseGraphicResource;
begin
  grRes := ResourceManager.GetResource(aResource);
  if not Assigned(grRes) then exit;

  for i:=0 to FRegisteredSubRenders.Count-1 do begin
    render:=TBaseSubRender(FRegisteredSubRenders[i]);
    { TODO : Реализовать выбор "наилучшего" из зарегистрированных рендеров }
    if render.IsSupported(
      TRenderResourceClass(aResource.ClassType),
      TGraphicResourceClass(grRes.ClassType)) then begin
       render.ProcessResource(grRes);
       exit;
    end;
  end;
end;

procedure TGLRender.ProcessScene(const aScene: TSceneGraph);
const
  WORLD_MATRIX_SUB = 'WorldMatrixGetter';
var i, j: integer;
    SceneItem: TBaseSceneItem;
    DrawCommand: TDrawCommand;
    camera: TGLCamera;
    glRes: TGLBaseResource;
    effects: TEffectPipeline;
    glMovable: TGLMovableObject;
    pwtBase, pwt: PWorldTransform;

    procedure DownToTree(anItems: TSceneItemList);
    var ii: integer;
    begin
      for ii:=0 to anItems.Count-1 do begin
        SceneItem:=anItems[ii];
        if Assigned(SceneItem) then
        begin
          if SceneItem is TSceneObject then FCurrentSceneObject := SceneItem as TSceneObject;
          ProcessResource(SceneItem);
          DownToTree(SceneItem.Childs);
        end;
      end;
    end;

begin
  FCurrentGraph := aScene;
  aScene.Root.NestingDepth := 0;
  //Подготавливаем ресурсы сцены
  PrepareResources(aScene);

  // Сливаем вектор комманд
  FDrawCommands.Flush;
  // Заполняем вектор комманд
  DownToTree(aScene.Root.Childs);

  if not ComputeTransformByShader then
    glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT or GL_BUFFER_UPDATE_BARRIER_BIT);

  FObjectPool.Buffer.BindBase(btShaderStorage, 0);
  FLightPool.Buffer.BindBase(btShaderStorage, 1);
  FLightIndicesPool.Buffer.BindBase(btShaderStorage, 2);

  for i := 0 to aScene.CamerasCount - 1 do begin
    FCurrentCamera := aScene.Cameras[i];
    camera := TGLCamera(FResourceManager.GetOrCreateResource(aScene.Cameras[i]));

    glClearColor(0.0, 0.1, 0.2, 1.0);
    glClearDepth(1.0);
    glEnable(GL_DEPTH_TEST);
    glDepthMask(true);
    glDepthFunc(GL_LESS);

    camera.Apply(Self);

    for j := 0 to FDrawCommands.Count - 1 do begin
      DrawCommand := FDrawCommands[j];

      DrawCommand.mesh.FMaterialObject.Apply(Self);
      BindObjectBuffer(DrawCommand.objectIndex);
      TGLSLShaderProgram.ActiveShader.SetUniform('ObjectId', DrawCommand.objectIndex);
      TGLSLShaderProgram.ActiveShader.SetUniform('LightId', DrawCommand.lightIndex);

      case DrawCommand.worldTransfMethod of
        wtmDefault: TGLSLShaderProgram.ActiveShader.SetSubroutine(WORLD_MATRIX_SUB, 'defaultWorldMatrix');
        wtmInstance: begin
          TGLSLShaderProgram.ActiveShader.SetSubroutine(WORLD_MATRIX_SUB, 'instanceWorldMatrix');
          TGLSLShaderProgram.ActiveShader.SetUniform('InstanceMatrix', DrawCommand.instanceMatrix^);
        end;
        wtmSphere: TGLSLShaderProgram.ActiveShader.SetSubroutine(WORLD_MATRIX_SUB, 'sphereSpriteMatrix');
        wtmCylindr: TGLSLShaderProgram.ActiveShader.SetSubroutine(WORLD_MATRIX_SUB, 'cylindrSpriteMatrix');
      end;

      DrawCommand.mesh.FVertexObject.RenderVO();
      DrawCommand.mesh.FMaterialObject.UnApply(Self);
    end;

    camera.UnApply(Self);
    effects := FCurrentCamera.EffectPipeline;
    if Assigned(effects) then begin
      for j := 0 to effects.EffectCount - 1 do begin
        glRes := FResourceManager.GetOrCreateResource(effects.Effects[j]);
        glRes.Apply(Self);
      end;
    end;
  end;

  if ComputeTransformByShader and (FTfChangesCounter > 0) then begin
    glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT or GL_BUFFER_UPDATE_BARRIER_BIT);
    pwtBase := FObjectPool.Buffer.Map(GL_READ_ONLY);
    for i := 0 to High(FInvocationGroups) do begin
      for j := 0 to FInvocationGroups[i].Count - 1 do begin
        glMovable := TGLMovableObject(FInvocationGroups[i].Items[j]);
        pwt := pwtBase;
        Inc(pwt, glMovable.FObjectPoolIndex);
        glMovable.FMovableObject.UpdateWorldMatrix(
          pwt.world,
          pwt.worldNormal,
          pwt.invWorld,
          pwt.worldT,
          pwt.pivot,
          pwt.invPivot);
      end;
    end;
    FObjectPool.Buffer.UnMap;
  end;
end;

procedure TGLRender.UploadResource(const Res: TBaseRenderResource);
begin
  inherited;
end;

{ TGLStaticRender }

function TGLSceneObjectRender.getRender: TGLRender;
begin
  Result := TGLRender(Owner);
end;

procedure TGLSceneObjectRender.ProcessResource(aResource: TBaseGraphicResource);
var i,j: integer;
    MeshObject: TGLMeshObject;
    Mesh: TGLMesh;
    SceneObject: TGLSceneObject absolute aResource;
    DrawCommand: TDrawCommand;
begin
  if SceneObject.FObjectPoolIndex > -1 then begin

    SceneObject.Apply(Render);
    //рендерим объект сцены с применением материалов
    for i:=0 to length(SceneObject.FMeshObjects)-1 do begin
      MeshObject:=SceneObject.FMeshObjects[i];
      for j:=0 to length(MeshObject.FLods[0])-1 do begin
        Mesh := MeshObject.FLods[0,j].Mesh;
        DrawCommand.mesh := Mesh;
        case SceneObject.FSceneObject.DirectionBehavior of
          dbNone: begin
            if MeshObject.FLods[0,j].IsIdentity
              then DrawCommand.worldTransfMethod := wtmDefault
              else begin
                DrawCommand.worldTransfMethod := wtmInstance;
                DrawCommand.instanceMatrix := MeshObject.FMeshObject.Mesh.GetMatrixAddr(j);
              end;
          end;
          dbSphericalSprite: DrawCommand.worldTransfMethod := wtmSphere;
          dbCylindricalSprite: DrawCommand.worldTransfMethod := wtmCylindr;
        end;
        DrawCommand.objectIndex := SceneObject.FObjectPoolIndex;
        DrawCommand.lightIndex := SceneObject.FLightIdxPoolIndex;
        Render.FDrawCommands.Add(DrawCommand);
      end;
    end;

  end;
end;

procedure TGLSceneObjectRender.ProclaimSupport;
begin
  FSupportedResources.Add(TSceneObject, TGLSceneObject);
end;

class procedure TGLSceneObjectRender.RenderVertexObject(
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
  ActiveShader := TGLSLShaderProgram.ActiveShader;

  if aShaderUsageLogic in [slDisableShader, slStashActiveAndDisableShader]
  then glUseProgram(0)
  else begin
    if ((aShaderUsageLogic in [slUseActiveShader, slUseActiveAndDisable])
    or (aShaderUsagePriority = spUseActiveShaderFirst))
    and assigned(TGLSLShaderProgram.ActiveShader)
    then begin
      //Do nothing, all shaders ready
    end else begin
      //Try to use Own shader
      if assigned(aVertexObject.Shader) then aVertexObject.Shader.Bind
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
    if assigned(ActiveShader) then ActiveShader.Bind
    else glUseProgram(0);
  end else begin
    if aShaderUsageLogic in [slDisableShader, slUseActiveAndDisable, slUseOwnAndDisable]
    then begin
      glUseProgram(0); TGLSLShaderProgram.ActiveShader := nil;
    end;
  end;
end;

{ TGLResources }

function ResourceComparer(const Item1, Item2: TBaseRenderResource): Integer;
begin
  if Item1.Order < Item2.Order then
    exit(-1)
  else if (Item1.Order = Item2.Order) then
    exit(0)
  else
    Result := 1;
end;

constructor TGLResources.CreateOwned(aOwner: TObject);
begin
  inherited CreateOwned(aOwner);
  FResources := TResourceTree.Create(ResourceComparer, nil);
end;

function TGLResources.CreateResource(const Resource: TBaseRenderResource): TGLBaseResource;
var
  grClass: TGraphicResourceClass;
begin
  if FSupportedResources.Find(TRenderResourceClass(Resource.ClassType), grClass)
    then Result := TGLBaseResourceClass(grClass).CreateFrom(Self, Resource)
    else Result := nil;
end;

procedure ResourceDestroyer(AKey: TBaseRenderResource; AValue: TGLBaseResource;
  aData: Pointer; out AContinue: Boolean);
begin
  if AValue.Owner = aData then AValue.Free;
  AContinue := true;
end;

destructor TGLResources.Destroy;
begin
  FResources.ForEach(ResourceDestroyer, Self);
  FResources.Free;
  inherited;
end;

procedure TGLResources.FreeResource(const Resource: TBaseRenderResource);
var glres: TGLBaseResource;
begin
  if Assigned(Resource) and FResources.Find(Resource, glres) then begin
    glres.UnSubscribe(Self);
    UnSubscribe(glres);
    FResources.Delete(Resource);
    FreeAndNil(glRes);
  end;
end;

function TGLResources.GetOrCreateResource(
  const Resource: TBaseRenderResource): TGLBaseResource;
begin
  if not assigned(Resource) then exit(nil);

  result := GetResource(Resource);
  if Assigned(result) then exit;
  Result := CreateResource(Resource);
  if Assigned(Result) then begin
    AttachResource(Resource);
    FResources.Add(Resource, Result);
    Result.Owner:=self;
    Resource.Subscribe(self);
    Result.Subscribe(Self);
    Subscribe(Result);
  end;
end;

function TGLResources.GetResource(
  const Resource: TBaseRenderResource): TGLBaseResource;
begin
  if not FResources.Find(Resource, Result) then Result := nil;
end;

procedure TGLResources.Notify(Sender: TObject; Msg: Cardinal; Params: pointer);
var res: TGLBaseResource;
begin
  inherited;
  if Msg = NM_ObjectDestroyed then
  begin
    Unsubscribe(TNotifiableObject(Sender));
    if Sender is TBaseRenderResource then
    begin
      res:=GetResource(TBaseRenderResource(Sender));
      if assigned(res) then  FreeResource(TBaseRenderResource(Sender));
    end;
  end;
end;

procedure TGLResources.ProcessResource(aResource: TBaseGraphicResource);
begin
  // Do nothing
end;

procedure TGLResources.ProclaimSupport;
begin
  FSupportedResources.Add(TShaderProgram, TGLSLShaderProgramExt);
  FSupportedResources.Add(TVertexObject, TGLVertexObject);

  FSupportedResources.Add(TMeshObject, TGLMeshObject);
  FSupportedResources.Add(TMaterialObject, TGLMaterial);
  FSupportedResources.Add(TLightSource, TGLLight);
  FSupportedResources.Add(TSceneObject, TGLSceneObject);

  { TODO : Объекты TBufferObject и TAttibObject так же нужно выбирать из коллекций }
  FSupportedResources.Add(TBufferObject, TGLBufferObject);
  FSupportedResources.Add(TAttribObject, TGLAttribObject);
  FSupportedResources.Add(TTexture, TGLTextureObject);
  FSupportedResources.Add(TMesh, TGLMesh);
  FSupportedResources.Add(TBuiltinUniformLightNumber, TGLUniformLightNumber);
  FSupportedResources.Add(TTextureSampler, TGLTextureSampler);
  FSupportedResources.Add(TFrameBuffer, TGLFrameBufferObjectExt);
  FSupportedResources.Add(TSceneCamera, TGLCamera);
  FSupportedResources.Add(TGlowPipelineEffect, TGLGlowEffect);
end;

procedure TGLResources.RemoveGLResource(const Resource: TBaseRenderResource);
var glres: TGLBaseResource;
begin
  if Assigned(Resource) and FResources.Find(Resource, glres) then
    FResources.Delete(Resource);
end;

{ TGLMesh }

constructor TGLMesh.CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource);
var
  aManager: TGLResources absolute aOwner;
  aMesh: TMesh absolute aResource;
begin
  Create;
  Assert(Assigned(aOwner) and (aOwner is TGLResources),
    'Resource manager invalide or not assigned');
  Assert(Assigned(aResource) and (aResource is TMesh),
    'Base resource invalide or not assigned');
  Owner := aOwner;
  FMesh := aMesh;
  FMesh.Subscribe(aOwner);
  FVertexObject := TGLVertexObject(aManager.GetOrCreateResource(FMesh.VertexObject));
  FMaterialObject := TGLMaterial(aManager.GetOrCreateResource(FMesh.MaterialObject));
end;

destructor TGLMesh.Destroy;
begin
  if Assigned(FMesh) then FMesh.UnSubscribe(Self);
  inherited;
end;

procedure TGLMesh.Notify(Sender: TObject; Msg: Cardinal; Params: pointer);
begin
  inherited;
  case Msg of
    NM_ObjectDestroyed: if Sender = FMesh then FMesh := nil;
  end;
end;

{ TGLMaterial }

procedure TGLMaterial.Apply(aRender: TBaseRender);
var
  glRender: TGLRender absolute aRender;
  tb: TGLUniformBlock;
  i: integer;
begin
  if assigned(FShader)
    then FShader.Apply(aRender)
    else exit;

  tb := FShader.UniformBlocks.GetUBOByName(CUBOSemantics[ubCamera].Name);
  if Assigned(tb) then begin
    glUniformBlockBinding(FShader.Id, tb.BlockIndex, CUBOSemantics[ubCamera].Location);
  end;

  tb := FShader.UniformBlocks.GetUBOByName(CUBOSemantics[ubMaterial].Name);
  if Assigned(tb) then begin
    glRender.MaterialPool.BindUBO(FIdexInPool, tb);
    glUniformBlockBinding(FShader.Id, tb.BlockIndex, CUBOSemantics[ubMaterial].Location);
  end;

  for i := High(FTextures) downto 0 do FTextures[i].Bind(i);
  for i := High(FSamplers) downto 0 do FSamplers[i].Bind(i);
end;

constructor TGLMaterial.CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource);
var
  aManager: TGLResources absolute aOwner;
  aMat: TMaterialObject absolute aResource;
  i, idx: integer;
begin
  Create;
  Assert(Assigned(aOwner) and (aOwner is TGLResources),
    'Resource manager invalide or not assigned');
  Assert(Assigned(aResource) and (aResource is TMaterialObject),
    'Base resource invalide or not assigned');
  Owner := aOwner;
  FMaterialObject := aMat;
  FMaterialObject.Subscribe(self);
  FIdexInPool:=-1;

  FShader:=TGLSLShaderProgramExt(aManager.GetOrCreateResource(FMaterialObject.Shader));
  FBlend:=FMaterialObject.Blending;
  setlength(FTextures,FMaterialObject.TexCount);
  idx := 0;
  if Assigned(FMaterialObject.Texture) then
  begin
    FTextures[idx]:=TGLTextureObject(aManager.GetOrCreateResource(FMaterialObject.Texture));
    FTextures[idx].UploadTexture();
    Inc(idx);
  end;
  for i:=idx to FMaterialObject.TexCount-1 do begin
    FTextures[idx]:=TGLTextureObject(aManager.GetOrCreateResource(FMaterialObject.TextureSlot[i]));
    FTextures[idx].UploadTexture();
    Inc(Idx);
  end;

  if Assigned(FMaterialObject.TextureSampler) then
  begin
    SetLength(FSamplers, 1);
    FSamplers[0] := TGLTextureSampler(aManager.GetOrCreateResource(FMaterialObject.TextureSampler));
  end;

  FStructureChanged := true;
end;

destructor TGLMaterial.Destroy;
begin
  if Assigned(FMaterialObject) then FMaterialObject.UnSubscribe(Self);
  inherited;
end;

procedure TGLMaterial.Notify(Sender: TObject; Msg: Cardinal; Params: pointer);
begin
  inherited;
  case Msg of
    NM_ResourceChanged: if Sender = FMaterialObject then FStructureChanged := true;
    NM_ObjectDestroyed: if Sender = FMaterialObject then FMaterialObject := nil;
  end;
end;

procedure TGLMaterial.UnApply;
begin
  //if assigned(FShader) then FShader.UnBind;
end;

procedure TGLMaterial.Update(aRender: TBaseRender);
var
  glRender: TGLRender absolute aRender;
  p: PMaterialProp;
begin
  if not FStructureChanged then
    exit;

  if FIdexInPool < 0 then
    FIdexInPool := glRender.FMaterialPool.GetFreeSlotIndex();

  p := glRender.FMaterialPool.Buffer.MapRange(
    GL_MAP_WRITE_BIT or GL_MAP_INVALIDATE_RANGE_BIT,
    glRender.FMaterialPool.OffsetByIndex(FIdexInPool), SizeOf(vec4)*5);

  // Fill Uniform Buffer Object Data
  if assigned(FMaterialObject.Material) then begin
    with FMaterialObject.Material.Properties do begin
      move(AmbientColor.ColorAsAddress^,p.ambient,SizeOf(vec4));
      move(DiffuseColor.ColorAsAddress^,p.diffuse,SizeOf(vec4));
      move(SpecularColor.ColorAsAddress^,p.specular,SizeOf(vec4));
      move(EmissionColor.ColorAsAddress^,p.emissive,SizeOf(vec4));
      move(Shininess,p.shininess, SizeOf(Single));
    end;
  end else begin
    move(cAmbientColor,p.ambient,SizeOf(vec4));
    move(cDiffuseColor,p.diffuse,SizeOf(vec4));
    move(cSpecularColor,p.specular,SizeOf(vec4));
    move(cEmissiveColor,p.emissive,SizeOf(vec4));
    move(cShininess,p.shininess, SizeOf(Single));
  end;
  glRender.FMaterialPool.Buffer.UnMap;
  FStructureChanged := false;
end;

{ TGLMeshObject }

constructor TGLMeshObject.CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource);
var
  aManager: TGLResources absolute aOwner;
  aMeshObject: TMeshObject absolute aResource;
  i, j: integer;
begin
  Create;
  Assert(Assigned(aOwner) and (aOwner is TGLResources),
    'Resource manager invalide or not assigned');
  Assert(Assigned(aResource) and (aResource is TMeshObject),
    'Base resource invalide or not assigned');
  FMeshObject:=aMeshObject;
  setlength(FLods, aMeshObject.Lods.Count);
  for i:=0 to aMeshObject.Lods.Count-1 do begin
    setlength(FLods[i], aMeshObject.Lods[i].Assembly.Count);
    for j:=0 to aMeshObject.Lods[i].Assembly.Count-1 do begin
      FLods[i,j].Mesh:=TGLMesh(aManager.GetOrCreateResource(aMeshObject.Lods[i].Assembly[j]));
      FLods[i,j].MatrixChanged := true;
    end;
  end;
  setlength(FOccluder, aMeshObject.Occluder.Count);
  for i:=0 to aMeshObject.Occluder.Count-1 do begin
    FOccluder[i]:=TGLMesh(aManager.GetOrCreateResource(aMeshObject.Occluder[i]));
  end;
end;

function TGLMeshObject.GetLodCount: integer;
begin
  Result := Length(FLods);
end;

function TGLMeshObject.GetMesh(const aLevel, anIndex: integer): TGLMesh;
begin
  Result := FLods[aLevel, anIndex].Mesh;
end;

function TGLMeshObject.GetMeshCount(const aLevel: integer): integer;
begin
  Result := Length(FLods[aLevel]);
end;

{ TGLSceneObject }

procedure TGLSceneObject.Apply(aRender: TBaseRender);
var
  glRender: TGLRender absolute aRender;
  glLight: TGLLight;
  i, j: Integer;
  changed: boolean;
  pl: PLightIndices;
begin
  j := 0;
  changed := false;
  // Just add all lights
  for i := 0 to aRender.CurrentGraph.LightsCount - 1 do begin
    glLight := glRender.FResourceManager.GetResource(aRender.CurrentGraph.Lights[i]) as TGLLight;
    if not glLight.FLightPropChanged then begin
      if FEffectedLights[j][0] <> glLight.LightPoolIndex then begin
        FEffectedLights[j][0] := glLight.LightPoolIndex;
        changed := true;
      end;
      Inc(j);
    end;
  end;

  if (FEffectedLightNum <> j) or changed then begin
    FEffectedLightNum := j;
    pl := glRender.FLightIndicesPool.Buffer.MapRange(
      GL_MAP_WRITE_BIT or GL_MAP_INVALIDATE_RANGE_BIT,
      glRender.FLightIndicesPool.OffsetByIndex(FLightIdxPoolIndex),
      glRender.FLightIndicesPool.ObjectSize);
    pl.count[0] := FEffectedLightNum;
    pl.indices := FEffectedLights;
    glRender.FLightIndicesPool.Buffer.UnMap;
  end;
end;

constructor TGLSceneObject.CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource);
var
  aManager: TGLResources absolute aOwner;
  aSceneObject: TSceneObject absolute aResource;
  i: integer;
begin
  Assert(Assigned(aOwner) and (aOwner is TGLResources),
    'Resource manager invalide or not assigned');
  Assert(Assigned(aResource) and (aResource is TSceneObject),
    'Base resource invalide or not assigned');
  Create;
  Owner := aOwner;
  FSceneObject := aSceneObject;
  FSceneObject.Subscribe(self);
  FMovableObject := aSceneObject;
  FObjectPoolIndex := -1;
  FBaseTfPoolIndex := -1;
  FLightIdxPoolIndex := -1;
  FTransformChanged := true;
  setlength(FMeshObjects, aSceneObject.MeshObjects.Count);
  for i:=0 to aSceneObject.MeshObjects.Count-1 do begin
    FMeshObjects[i]:=TGLMeshObject(aManager.GetOrCreateResource(aSceneObject.MeshObjects[i]));
  end;
end;

destructor TGLSceneObject.Destroy;
begin
  if Assigned(FSceneObject) then FSceneObject.UnSubscribe(Self);
  inherited;
end;

procedure TGLSceneObject.Notify(Sender: TObject; Msg: Cardinal;
  Params: pointer);
begin
  inherited;
  case Msg of
    NM_WorldMatrixChanged: if Sender = FSceneObject then FTransformChanged := true;
    NM_ResourceChanged: if Sender = FSceneObject then FTransformChanged := true;
    NM_ObjectDestroyed: if Sender = FSceneObject then FSceneObject := nil;
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

procedure TGLSceneObject.Update(aRender: TBaseRender);
var
  glRender: TGLRender absolute aRender;
  i, j: integer;
  MeshObject: TGLMeshObject;
  mat: TMatrix;
begin
  inherited Update(aRender);

  if FLightIdxPoolIndex < 0 then
    FLightIdxPoolIndex := glRender.FLightIndicesPool.GetFreeSlotIndex;

  // Update meshes local matrices
  // TODO: заменить постоянную проверку на нотификацию
  for i:=0 to length(FMeshObjects)-1 do begin
    MeshObject:=FMeshObjects[i];
    for j:=0 to length(MeshObject.FLods[0])-1 do begin
      mat := MeshObject.FMeshObject.LODS.LODS[0].Assembly.LocalMatrices[j];
      MeshObject.FLods[0,j].IsIdentity := mat.IsIdentity;
    end;
  end;
end;

{ TGLLight }

constructor TGLLight.CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource);
var
  aManager: TGLResources absolute aOwner;
  aLight: TLightSource absolute aResource;
begin
  Create;
  Assert(Assigned(aOwner) and (aOwner is TGLResources),
    'Resource manager invalide or not assigned');
  Assert(Assigned(aResource) and (aResource is TLightSource),
    'Base resource invalide or not assigned');
  Owner:=aOwner;
  FLight:=aLight;
  FMovableObject := aLight;
  FLight.Subscribe(self);
  FBaseTfPoolIndex := -1;
  FLightPoolIndex := -1;
  FObjectPoolIndex := -1;
  FLightPropChanged := True;
  FTransformChanged := True;
end;

destructor TGLLight.Destroy;
begin
  // Need to add slot freeing in Lights pool
  if Assigned(FLight) then FLight.UnSubscribe(Self);
  inherited;
end;

procedure TGLLight.Notify(Sender: TObject; Msg: Cardinal; Params: pointer);
begin
  inherited;
  if Sender = FLight then
    case Msg of
      NM_WorldMatrixChanged: FTransformChanged := true;
      NM_ResourceChanged: FLightPropChanged := true;
      NM_ObjectDestroyed: FLight := nil;
    end;
end;

procedure TGLLight.Update(aRender: TBaseRender);
var
  glRender: TGLRender absolute aRender;
  p: PLightProp;
  pos: TVector;
  cos_cuoff: single;
begin
  if FTransformChanged then
    inherited Update(aRender) // Update transformation firstly
  else if FLightPropChanged then begin // Then update light
    if FLightPoolIndex < 0 then
      FLightPoolIndex := glRender.FLightPool.GetFreeSlotIndex();

    p := glRender.FLightPool.Buffer.MapRange(
      GL_MAP_WRITE_BIT or GL_MAP_INVALIDATE_RANGE_BIT,
      glRender.FLightPool.OffsetByIndex(FLightPoolIndex),
      glRender.FLightPool.ObjectSize);

    // Fill Uniform Buffer Object Data
    with FLight do begin
      pos := AbsolutePosition;
      if LightStyle in [lsParallel, lsParallelSpot] then
        pos.MakeAffine else pos.MakePoint;
      move(pos.GetAddr^,p.position, SizeOf(vec4));
      move(Ambient.ColorAsAddress^,p.ambient, SizeOf(vec4));
      move(Diffuse.ColorAsAddress^,p.diffuse, SizeOf(vec4));
      move(Specular.ColorAsAddress^,p.specular, SizeOf(vec4));
      move(ConstAttenuation,p.constant_attenuation, SizeOf(Single));
      move(LinearAttenuation,p.linear_attenuation, SizeOf(Single));
      move(QuadraticAttenuation,p.quadratic_attenuation, SizeOf(Single));
      if LightStyle in [lsSpot, lsParallelSpot] then
        cos_cuoff := Cos(Pi*SpotCutOff/180)
      else
        cos_cuoff := -1;
      move(cos_cuoff,p.spot_cutoff, SizeOf(Single));
      move(SpotExponent,p.spot_exponent, SizeOf(Single));
      //move(SceneColor.ColorAsAddress^,p^,16); inc(p, 16);
      move(SpotDirection.GetAddr^,p.spot_direction, SizeOf(vec3));
    end;
    glRender.FLightPool.Buffer.UnMap;

    FLightPropChanged := False;
  end;
end;

{ TGLBuiltinUniform }

constructor TGLBuiltinUniform.CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource);
var
  aManager: TGLResources absolute aOwner;
  anUniform: TBaseBuiltinUniform absolute aResource;
begin
  Assert(Assigned(aOwner) and (aOwner is TGLResources),
    'Resource manager invalide or not assigned');
  Assert(Assigned(aResource) and (aResource is TBaseBuiltinUniform),
    'Base resource invalide or not assigned');
  Create;
  Owner := aOwner;
  FUniform := anUniform;
end;

{ TGLUniformLightNumber }

procedure TGLUniformLightNumber.Apply(aRender: TBaseRender);
begin
  Assert(TGLSLShaderProgram.ActiveShader <> nil);
//  TGLSLShaderProgram.ActiveShader.SetUniform(FUniform.Name, aRender.CurrentLightNumber);
end;

{ TGLSLShaderProgramExt }

procedure TGLSLShaderProgramExt.Apply(aRender: TBaseRender);
var
  i: integer;
begin
  Bind();
  if Assigned(FBuiltinUniforms) then
    for i := 0 to FBuiltinUniforms.Count - 1 do
      TGLBuiltinUniform(FBuiltinUniforms[i]).Apply(aRender);
end;

constructor TGLSLShaderProgramExt.CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource);
var
  aManager: TGLResources absolute aOwner;
  aShaderProgram: TShaderProgram absolute aResource;
  i: integer;
  glres: TGLBaseResource;
begin
  Assert(Assigned(aOwner) and (aOwner is TGLResources),
    'Resource manager invalide or not assigned');
  Assert(Assigned(aResource) and (aResource is TShaderProgram),
    'Base resource invalide or not assigned');
  inherited CreateFrom(aOwner, aShaderProgram);
  if Assigned(aShaderProgram.BuildinUniforms) then
  begin
    FBuiltinUniforms := TObjectList.Create;
    for I := 0 to aShaderProgram.BuildinUniforms.Count - 1 do
    begin
      glres := aManager.GetOrCreateResource(aShaderProgram.BuildinUniforms[I] as TBaseRenderResource);
      FBuiltinUniforms.Add(glres);
    end;
  end;
end;

destructor TGLSLShaderProgramExt.Destroy;
begin
  FBuiltinUniforms.Free;
  inherited;
end;

{ TGLFrameBufferObjectExt }

constructor TGLFrameBufferObjectExt.CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource);
var
  aManager: TGLResources absolute aOwner;
  aFrameBuffer: TFrameBuffer absolute aResource;
  i: integer;
  texture: TTexture;
  gltexture: TGLTextureObject;
begin
  Assert(Assigned(aOwner) and (aOwner is TGLResources),
    'Resource manager invalide or not assigned');
  Assert(Assigned(aResource) and (aResource is TFrameBuffer),
    'Base resource invalide or not assigned');
  Create;
  Owner := aOwner;
  FFrameBuffer := aFrameBuffer;
  AttachResource(FFrameBuffer);

  ConfigFBO(FFrameBuffer.RenderBuffers);
  ConfigDepthBuffer(bmBuffer);
  InitFBO(FFrameBuffer.Size);
  Multisample := FFrameBuffer.Multisample;
  for i := 0 to FFrameBuffer.ColorAttachmentCount - 1 do begin
    texture := FFrameBuffer.ColorAttachments[i];
    gltexture := aManager.GetOrCreateResource(texture) as TGLTextureObject;
    AttachResource(gltexture);
    gltexture.AllocateStorage;
    AttachTexture(gltexture);
  end;
  FStructureChanged := false;
end;


destructor TGLFrameBufferObjectExt.Destroy;
begin
  inherited;
end;

procedure TGLFrameBufferObjectExt.Notify(Sender: TObject; Msg: Cardinal;
  Params: pointer);
begin
  if Msg = NM_ResourceChanged then begin
    if Sender = FFrameBuffer then begin
      FStructureChanged := true;
      DispatchMessage(NM_ResourceChanged);
    end;
  end else
    if Msg = NM_ObjectDestroyed then begin
      if Sender = FFrameBuffer then FFrameBuffer := nil;
    end;
  inherited;
end;

procedure TGLFrameBufferObjectExt.Update(aRender: TBaseRender);
var
  size: vec2i;
begin
  if FStructureChanged then begin
    if assigned(FFrameBuffer) then begin
      size := FFrameBuffer.Size;
      if (size[0] <> FWidth) or (size[1] <> FHeight) then InitFBO(size);
      FStructureChanged := false;
    end;
  end;
end;

{ TGLCamera }

procedure TGLCamera.Apply(aRender: TBaseRender);
var
  glRender: TGLRender absolute aRender;
begin
  glRender.BindCameraBuffer(FIdexInPool);
  if Assigned(FFrameBuffer) then FFrameBuffer.Bind();
  glViewport(0, 0, FCamera.ViewPortSize[0], FCamera.ViewPortSize[1]);
end;

constructor TGLCamera.CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource);
var
  aManager: TGLResources absolute aOwner;
  aCamera: TSceneCamera absolute aResource;
begin
  Assert(Assigned(aOwner) and (aOwner is TGLResources),
    'Resource manager invalide or not assigned');
  Assert(Assigned(aResource) and (aResource is TSceneCamera),
    'Base resource invalide or not assigned');
  Create;
  Owner := aOwner;
  FCamera:=aCamera;
  FCamera.Subscribe(Self);
  FIdexInPool:=-1;
  if Assigned(FCamera.RenderTarget) then
  begin
    FFrameBuffer := aManager.GetOrCreateResource(FCamera.RenderTarget) as TGLFrameBufferObjectExt;
  end;
  FStructureChanged := true;
end;

destructor TGLCamera.Destroy;
begin
  if Assigned(FCamera) then begin
    FCamera.UnSubscribe(self);
    FCamera := nil;
  end;
  inherited;
end;

procedure TGLCamera.Notify(Sender: TObject; Msg: Cardinal; Params: pointer);
begin
  inherited;
  if (Msg = NM_ViewMatrixChanged) or (Msg = NM_ProjMatrixChanged) then begin
    if Sender = FCamera then FStructureChanged := true;
  end
  else if Msg = NM_ObjectDestroyed then if Sender = FCamera then FCamera := nil;
end;

procedure TGLCamera.UnApply;
begin
  if Assigned(FFrameBuffer) then FFrameBuffer.UnBind;
end;

procedure TGLCamera.Update(aRender: TBaseRender);
var
  glRender: TGLRender absolute aRender;
  p: PCameraTransform;
  mat: TMatrix;
begin
  if Assigned(FFrameBuffer) then
    FFrameBuffer.Update(aRender);

  if FStructureChanged then begin

    if FIdexInPool < 0 then
      FIdexInPool := glRender.FCameraPool.GetFreeSlotIndex();

    p := glRender.FCameraPool.Buffer.MapRange(
      GL_MAP_WRITE_BIT or GL_MAP_INVALIDATE_RANGE_BIT,
      glRender.FCameraPool.OffsetByIndex(FIdexInPool), glRender.FCameraPool.ObjectSize);

    with FCamera do begin
      move(ViewMatrix.GetAddr^, p.View, SizeOf(mat4));
      move(ProjMatrix.GetAddr^,p.Projection, SizeOf(mat4));
      mat := ViewMatrix * ProjMatrix;
      move(mat.GetAddr^, p.ViewProjection, SizeOf(mat4));
    end;
    glRender.FCameraPool.Buffer.UnMap;
    FStructureChanged := false;
  end;
end;

{ TGLGlowEffect }

procedure TGLGlowEffect.Apply(aRender: TBaseRender);
var
  glRender: TGLRender absolute aRender;
  eff: TGlowPipelineEffect;
  tex: TGLTextureObject;
  vp: vec2i;
  scale: vec2;
  w, h: integer;
begin
  if Assigned(FEffect) then begin
    eff := TGlowPipelineEffect(FEffect);
    tex := glRender.FResourceManager.GetOrCreateResource(eff.SceneTexture) as TGLTextureObject;
    if Assigned(tex) then begin
      glDisable(GL_DEPTH_TEST);
      tex.Bind(0);
      FSampler.Bind(0);
      FSampler.Bind(1);

      FBlurShader.Apply(aRender);
      FFrameH.Bind(false);
      vp := glRender.CurrentCamera.ViewPortSize;
      w := vp[0] div 4;
      h := vp[1] div 4;
      glViewport(0, 0, w, h);
      scale := Vec2Make(w / FImageHolder.Width, h / FImageHolder.Height);
      FBlurShader.SetUniform('TexCoordScale', scale);
      FBlurShader.SetUniform('Step', Vec2Make(scale[0]/FImageHolder.Width, 0));
      TGLSceneObjectRender.RenderVertexObject(FVertexObject);

      FFrameV.Bind(false);
      FBluredH.Bind(0);
      FBlurShader.SetUniform('Step', Vec2Make(0, scale[1]/FImageHolder.Height));
      TGLSceneObjectRender.RenderVertexObject(FVertexObject);
      FBlurShader.UnBind;
      FFrameV.UnBind;

      glViewport(0, 0, vp[0], vp[1]);
      FShader.Bind;
      FShader.SetUniform('TexCoordScale', scale);
      tex.Bind(0);
      FBluredV.Bind(1);
      TGLSceneObjectRender.RenderVertexObject(FVertexObject);
      FShader.UnBind;
    end;
  end;
end;

constructor TGLGlowEffect.CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource);
begin
  Assert(assigned(aOwner) and (aOwner is TGLResources),
    'Resource manager invalide or not assigned');
  Assert(assigned(aResource) and (aResource is TGlowPipelineEffect),
    'Effect invalide or not assigned');
  Create;
  Owner := aOwner;
  FEffect := aResource;
  AttachResource(aResource);
  FStructureChanged := true;
end;

destructor TGLGlowEffect.Destroy;
begin
  if Assigned(FEffect) then begin
    DetachResource(FEffect);
    FEffect := nil;
  end;
  //FImageHolder.Free;


  FreeAndNil(fbh);
  FreeAndNil(fbv);
  FreeAndNil(texv);
  FreeAndNil(texh);
  FreeAndNil(FImageHolder);
  inherited;
end;

procedure TGLGlowEffect.Notify(Sender: TObject; Msg: Cardinal; Params: pointer);
begin
  if Msg = NM_ObjectDestroyed then  begin
    if Sender = FEffect then FEffect := nil;
    if Sender = FShader then FShader := nil;
    if Sender = FBlurShader then FBlurShader := nil;
    if Sender = FFrameH then FFrameH := nil;
    if Sender = FFrameV then FFrameV := nil;
    if Sender = FImageHolder then FImageHolder := nil;
    if Sender = FBluredH then FBluredH := nil;
    if Sender = FBluredV then FBluredV := nil;
    if Sender = FVertexObject then FVertexObject := nil;
    if Sender = FSampler then FSampler := nil;
  end;
  inherited;
end;

procedure TGLGlowEffect.Update(aRender: TBaseRender);
var
  glRender: TGLRender absolute aRender;
  eff: TGlowPipelineEffect;
begin
  if not assigned(FEffect) then exit;

  eff := TGlowPipelineEffect(FEffect);
  if FStructureChanged then begin
    FShader := glRender.FResourceManager.GetOrCreateResource(eff.ShaderProgram) as TGLSLShaderProgramExt;
    AttachResource(FShader);

    FShader.SetUniform('BlurAmount', eff.BlurAmount);
    FBlurShader := glRender.FResourceManager.GetOrCreateResource(eff.ConvolutionShader) as TGLSLShaderProgramExt;
    AttachResource(FBlurShader);

    FBlurShader.SetUniform('Weights', eff.Weights, eff.WeightCount);
    FBlurShader.SetUniform('Width', eff.WeightCount div 2);

    FVertexObject := glRender.FResourceManager.GetOrCreateResource(eff.ScreenQuad) as TGLVertexObject;
    AttachResource(FVertexObject);

    FSampler := glRender.FResourceManager.GetOrCreateResource(eff.SceneSampler) as TGLTextureSampler;
    AttachResource(FSampler);

    FStructureChanged := false;
  end;

  if not Assigned(FImageHolder) then begin
    FImageHolder := TImageSampler.CreateBitmap(eff.SceneTexture.ImageFormat,
      eff.SceneTexture.ImageHolder.Width div 4,
      eff.SceneTexture.ImageHolder.Height div 4, false);
  end;

  if not Assigned(FFrameH) then begin
    fbh := Storage.CreateFrameBuffer;
    fbh.Size:=Vec2iMake(FImageHolder.Width, FImageHolder.Height);
    FFrameH := glRender.FResourceManager.GetOrCreateResource(fbh) as TGLFrameBufferObject;
    AttachResource(FFrameH);
    texh := Storage.CreateTexture(FImageHolder);
    FBluredH := glRender.FResourceManager.GetOrCreateResource(texh) as TGLTextureObject;
    AttachResource(FBluredH);
    FBluredH.AllocateStorage;
    FFrameH.AttachTexture(FBluredH);
  end;

  if not Assigned(FFrameV) then begin
    fbv := Storage.CreateFrameBuffer;
    fbv.Size:=Vec2iMake(FImageHolder.Width, FImageHolder.Height);
    FFrameV := glRender.FResourceManager.GetOrCreateResource(fbv) as TGLFrameBufferObject;
    AttachResource(FFrameV);
    texv := Storage.CreateTexture(FImageHolder);
    FBluredV := glRender.FResourceManager.GetOrCreateResource(texv) as TGLTextureObject;
    AttachResource(FBluredV);
    FBluredV.AllocateStorage;
    FFrameV.AttachTexture(FBluredV);
  end;
end;

{ TGLMovableObject }

procedure TGLMovableObject.Update(aRender: TBaseRender);
var
  glRender: TGLRender absolute aRender;
  glParent: TGLBaseResource;
  i: integer;
  pwt: PWorldTransform;
  pbt: PBaseTransform;
begin
  // Находим индекс родительского объекта
  if glRender.ComputeTransformByShader then begin
    FParentObjectPoolIndex := 0;
    if Assigned(FMovableObject.Pivot) then begin
      glParent := glRender.FResourceManager.GetResource(FMovableObject.Pivot);
      if Assigned(glParent) and (glParent is TGLMovableObject) then
        FParentObjectPoolIndex := TGLMovableObject(glParent).FObjectPoolIndex;
    end;
  end;

  if FMovableObject.NestingDepth < 0 then FTransformChanged := false;

  if FTransformChanged then
  begin
    if FObjectPoolIndex < 0 then
      FObjectPoolIndex := glRender.FObjectPool.GetFreeSlotIndex();

    if glRender.ComputeTransformByShader then begin
      i := High(glRender.FInvocationGroups);
      if i < FMovableObject.NestingDepth then glRender.ExpandHierarchyLevels(FMovableObject.NestingDepth - i);
      glRender.FInvocationGroups[FMovableObject.NestingDepth].Add(Self);

      if FBaseTfPoolIndex < 0 then
        FBaseTfPoolIndex := glRender.FBaseTfPool.GetFreeSlotIndex();

      pbt := glRender.FBaseTfPool.Buffer.MapRange(
        GL_MAP_WRITE_BIT or GL_MAP_INVALIDATE_RANGE_BIT,
        glRender.FBaseTfPool.OffsetByIndex(FBaseTfPoolIndex),
        glRender.FBaseTfPool.ObjectSize);

      with FMovableObject do begin
        move(ScaleMatrix.GetAddr^,pbt.scale,SizeOf(mat4));
        move(RotationMatrix.GetAddr^,pbt.rotation,SizeOf(mat4));
        move(TranslationMatrix.GetAddr^, pbt.translation,SizeOf(mat4));
        move(ModelMatrix.GetAddr^, pbt.model,SizeOf(mat4));
      end;

      glRender.FBaseTfPool.Buffer.UnMap;
    end
    else begin
      pwt := glRender.FObjectPool.Buffer.MapRange(
        GL_MAP_WRITE_BIT or GL_MAP_INVALIDATE_RANGE_BIT,
        glRender.FObjectPool.OffsetByIndex(FObjectPoolIndex),
        glRender.FObjectPool.ObjectSize);

      // Fill Uniform Buffer Object Data
      with FMovableObject do begin
        move(WorldMatrix.GetAddr^,pwt.world,SizeOf(mat4));
        move(InvWorldMatrix.GetAddr^,pwt.invWorld,SizeOf(mat4));
        move(NormalMatrix.GetAddr^, pwt.worldNormal,SizeOf(mat4));
        move(WorldMatrixT.GetAddr^, pwt.worldT,SizeOf(mat4));
        move(PivotMatrix.GetAddr^, pwt.pivot,SizeOf(mat4));
        move(InvPivotMatrix.GetAddr^, pwt.invPivot,SizeOf(mat4));
      end;

      glRender.FObjectPool.Buffer.UnMap;
    end;

    FTransformChanged := false;
  end;
end;

initialization

  vGLRender:=TGLRender.Create;
  vRegisteredRenders.RegisterRender(vGLRender);

finalization
//  if assigned(vRegisteredRenders) then vRegisteredRenders.UnRegisterRender(vGLRender);
//  vGLRender.DispatchMessage(NM_ResourceChanged);
//  FreeAndNil(vGLRender);
  vGLRender := nil;
end.
//Projected Sphere radius
//radius * cot(fov / 2) / Z * ViewerSize
