unit uGizmoGL;

interface

uses uGizmo, uBaseClasses, uBaseRenders, uGLRenders, uPersistentClasses,
  uBaseTypes, uLists;

type

  TGLGizmoObject = class(TGLMovableObject)
  private
    FGizmoObject: TGizmoObject;
    FMoveMeshObject: TGLMeshObject;
    FRotateMeshObject: TGLMeshObject;
    FScaleMeshObject: TGLMeshObject;
    FLight: TGLLight;
    FLightsChaged: Boolean;
  public
    constructor CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource); override;
    destructor Destroy; override;
    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;

    procedure Update(aRender: TBaseRender); override;
    procedure Apply(aRender: TBaseRender); override;
  end;

  TGLGizmoObjectRender = class(TGLSceneObjectRender)
  protected
    procedure ProclaimSupport; override;
  public
    procedure ProcessResource(aResource: TBaseGraphicResource); override;
  end;

implementation

uses
   uShaderGen, dglOpenGL;

{ TGLGizmoObject }

procedure TGLGizmoObject.Apply(aRender: TBaseRender);
var
  glRender: TGLRender absolute aRender;
  pl: PLightIndices;
begin
  // Worked once because gizmo light is constant
  if FLightsChaged then begin
    pl := glRender.LightIndicesPool.Buffer.MapRange(
      GL_MAP_WRITE_BIT or GL_MAP_INVALIDATE_RANGE_BIT,
      glRender.LightIndicesPool.OffsetByIndex(FLightIdxPoolIndex),
      glRender.LightIndicesPool.ObjectSize);
    pl.count[0] := 1;
    pl.indices[0][0] := FLight.LightPoolIndex;
    glRender.LightIndicesPool.Buffer.UnMap;
    FLightsChaged := false;
  end;
end;

constructor TGLGizmoObject.CreateFrom(aOwner: TBaseSubRender; aResource: TBaseRenderResource);
var
  aManager: TGLResources absolute aOwner;
  aGizmoObject: TGizmoObject absolute aResource;
begin
  Create;
  Assert(Assigned(aOwner) and (aOwner is TGLResources),
    'Resource manager invalide or not assigned');
  Assert(Assigned(aResource) and (aResource is TGizmoObject),
    'Base resource invalide or not assigned');
  Owner := aOwner;
  FGizmoObject := aGizmoObject;
  FGizmoObject.Subscribe(self);
  FMovableObject := FGizmoObject;
  FObjectPoolIndex := -1;
  FBaseTfPoolIndex := -1;
  FLightIdxPoolIndex := -1;
  FTransformChanged := true;
  FLightsChaged := true;
  FMoveMeshObject := TGLMeshObject(aManager.GetOrCreateResource(aGizmoObject.Meshes[gmMoving]));
//  FRotateMeshObject := TGLMeshObject(aManager.GetOrCreateResource(aGizmoObject.Meshes[gmRotating]));
//  FScaleMeshObject := TGLMeshObject(aManager.GetOrCreateResource(aGizmoObject.Meshes[gmScaling]));
end;

destructor TGLGizmoObject.Destroy;
begin
  if Assigned(FGizmoObject) then FGizmoObject.UnSubscribe(Self);
  inherited;
end;

procedure TGLGizmoObject.Notify(Sender: TObject; Msg: Cardinal;
  Params: pointer);
begin
  inherited;
  case Msg of
    NM_WorldMatrixChanged: if Sender = FGizmoObject then FTransformChanged := true;
    NM_ResourceChanged: if Sender = FGizmoObject then FTransformChanged := true;
    NM_ObjectDestroyed: if Sender = FGizmoObject then FGizmoObject := nil;
  end;
end;

procedure TGLGizmoObject.Update(aRender: TBaseRender);
var
  glRender: TGLRender absolute aRender;
begin
  inherited Update(aRender);
  if not Assigned(FLight) then
    FLight := TGLLight(glRender.ResourceManager.GetOrCreateResource(FGizmoObject.Light));
  FLight.Update(aRender);
  if FLightIdxPoolIndex < 0 then FLightIdxPoolIndex := glRender.LightIndicesPool.GetFreeSlotIndex;
end;

{ TGLGizmoObjectRender }

procedure TGLGizmoObjectRender.ProcessResource(aResource: TBaseGraphicResource);
var i: integer;
    MeshObject: TGLMeshObject;
    glGizmoObject: TGLGizmoObject absolute aResource;
    DrawCommand: TDrawCommand;
begin
  glGizmoObject.Apply(Render);

  case glGizmoObject.FGizmoObject.Mode of
    gmNone: exit;
    gmMoving: MeshObject := glGizmoObject.FMoveMeshObject;
    gmRotating: MeshObject := glGizmoObject.FRotateMeshObject;
    gmScaling: MeshObject := glGizmoObject.FScaleMeshObject;
    else MeshObject := nil;
  end;

  for i := 0 to MeshObject.MeshCount[0] - 1 do begin
    DrawCommand.mesh := MeshObject.Lods[0, i];
    DrawCommand.worldTransfMethod := wtmInstance;
    DrawCommand.instanceMatrix := MeshObject.MeshObject.Mesh.GetMatrixAddr(i);
    DrawCommand.objectIndex := glGizmoObject.FObjectPoolIndex;
    DrawCommand.lightIndex := glGizmoObject.FLightIdxPoolIndex;
    Render.AddDrawCommand(DrawCommand);
  end;
end;

procedure TGLGizmoObjectRender.ProclaimSupport;
begin
  FSupportedResources.Add(TGizmoObject, TGLGizmoObject);
  vGLRender.ResourceManager.AddSupporting(TGizmoObject, TGLGizmoObject);
end;

initialization

  vGLRender.RegisterSubRender(TGLGizmoObjectRender.CreateOwned(vGLRender));



end.