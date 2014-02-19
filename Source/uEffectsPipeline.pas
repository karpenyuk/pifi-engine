unit uEffectsPipeline;

interface

uses uPersistentClasses, uLists, uRenderResource, uBaseTypes;

Type

  TMultipassPipelineEffect = class(TPipelineAbstractEffect)
  public
  end;

  TGlowPipelineEffect = class(TPipelineAbstractEffect)
  private
    FSceneTexture: TTexture;
  public
    constructor CreateFrom(aSceneTexture: TTexture);
    destructor Destroy; override;
    property SceneTexture: TTexture read FSceneTexture;
  end;


implementation

{ TGlowPipelineEffect }

constructor TGlowPipelineEffect.CreateFrom(aSceneTexture: TTexture);
begin
  Create;
  FSceneTexture := aSceneTexture;
  aSceneTexture.Subscribe(Self);
end;

destructor TGlowPipelineEffect.Destroy;
begin
  if Assigned(FSceneTexture) then begin
    FSceneTexture.Unsubscribe(Self);
    FSceneTexture := nil;
  end;
  inherited;
end;

end.
