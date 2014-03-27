unit uGizmoGL;

interface

uses uGizmo, uBaseRenders, uGLRenders, uPersistentClasses, uBaseTypes, uLists;

type

  TGLGizmoObject = class(TGLMovableObject)
  private
    FGizmoObject: TGizmoObject;
    FMoveMeshObject: TGLMeshObject;
    FRotateMeshObject: TGLMeshObject;
    FScaleMeshObject: TGLMeshObject;
  public
    constructor CreateFrom(aOwner: TGLResources; aResource: TPersistentResource); override;
    destructor Destroy; override;
    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;

    procedure Update(aRender: TBaseRender); override;
    procedure Apply(aRender: TBaseRender); override;
  end;

implementation

{ TGLGizmoObject }

procedure TGLGizmoObject.Apply(aRender: TBaseRender);
var
  glRender: TGLRender absolute aRender;
  glLight: TGLLight;
  list: TObjectList;
  i: Integer;
begin
  list := TObjectList.Create;
  try
    // TODO: собственный направленый источник освещения
    for i := 0 to aRender.CurrentGraph.LightsCount - 1 do begin
      glLight := glRender.ResourceManager.GetOrCreateResource(aRender.CurrentGraph.Lights[i]) as TGLLight;
      list.Add(glLight);
    end;
    glRender.ApplyLights(list);
  finally
    list.Free;
  end;
end;

constructor TGLGizmoObject.CreateFrom(aOwner: TGLResources;
  aResource: TPersistentResource);
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
  FTransformChanged := true;
  FMoveMeshObject := TGLMeshObject(aManager.GetOrCreateResource(aGizmoObject.Meshes[gmMoving]));
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
begin
  inherited Update(aRender);
end;

end.