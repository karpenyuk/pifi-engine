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
    class var FScreenQuad: TVertexObject;
    class var FSceneSampler: TTextureSampler;
    function GetScreenQuad: TVertexObject;
    function GetSceneSampler: TTextureSampler;
  public
    constructor CreateFrom(aSceneTexture: TTexture);
    destructor Destroy; override;
    class function IsInner: boolean; override;

    property SceneTexture: TTexture read FSceneTexture;
    property SceneSampler: TTextureSampler read GetSceneSampler;
    property ScreenQuad: TVertexObject read GetScreenQuad;
  end;


implementation

uses
  uPrimitives, uShaderGen, uStorage;

{ TGlowPipelineEffect }

constructor TGlowPipelineEffect.CreateFrom(aSceneTexture: TTexture);
begin
  Create;
  FSceneTexture := aSceneTexture;
  aSceneTexture.Subscribe(Self);
  FShaderProgram := ShaderGenerator.GenScreenQuadShader;
end;

destructor TGlowPipelineEffect.Destroy;
begin
  if Assigned(FSceneTexture) then begin
    FSceneTexture.Unsubscribe(Self);
    FSceneTexture := nil;
  end;
  inherited;
end;

function TGlowPipelineEffect.GetSceneSampler: TTextureSampler;
begin
  if not Assigned(FSceneSampler) then begin
    FSceneSampler := Storage.CreateTextureSample;
    FSceneSampler.WrapS := twClampToEdge;
    FSceneSampler.WrapT := twClampToEdge;
    FSceneSampler.minFilter := mnNearest;
    FSceneSampler.magFilter := mgNearest;
  end;
  Result := FSceneSampler;
end;

function TGlowPipelineEffect.GetScreenQuad: TVertexObject;
begin
  if not Assigned(FScreenQuad) then FScreenQuad := CreateSprite(2, 2);
  Result := FScreenQuad;
end;

class function TGlowPipelineEffect.IsInner: boolean;
begin
  Result := true;
end;

end.
