unit uEffectsPipeline;

interface

uses uPersistentClasses, uLists, uRenderResource, uBaseTypes, uvMath;

Type

  TMultipassPipelineEffect = class(TPipelineAbstractEffect)
  public
  end;

  TGaussianWeights = array of Single;

  TGlowPipelineEffect = class(TPipelineAbstractEffect)
  private
    FSceneTexture: TTexture;
    FBlurWidth: Single;
    FBlurAmount: Single;
    FWeigts: TGaussianWeights;
    class var FScreenQuad: TVertexObject;
    class var FSceneSampler: TTextureSampler;
    class var FConvolutionShader: TShaderProgram;
    function GetScreenQuad: TVertexObject;
    function GetSceneSampler: TTextureSampler;
    function GetConvolutionShader: TShaderProgram;
    function Gaussian(x, s: Single): Single;
    procedure GenGaussianWeights;
    procedure SetBlurWidth(const Value: Single);
    function GetWeigts: PSingle;
    function GetWeightCount: integer;
  public
    constructor CreateFrom(aSceneTexture: TTexture);
    destructor Destroy; override;
    class function IsInner: boolean; override;

    property SceneTexture: TTexture read FSceneTexture;
    property SceneSampler: TTextureSampler read GetSceneSampler;
    property ScreenQuad: TVertexObject read GetScreenQuad;
    property ConvolutionShader: TShaderProgram read GetConvolutionShader;
    property BlurWidth: Single read FBlurWidth write SetBlurWidth;
    property BlurAmount: Single read FBlurAmount write FBlurAmount;
    property Weights: PSingle read GetWeigts;
    property WeightCount: integer read GetWeightCount;
  end;


implementation

uses
  uPrimitives, uShaderGen, uStorage, Math;

{ TGlowPipelineEffect }

constructor TGlowPipelineEffect.CreateFrom(aSceneTexture: TTexture);
begin
  Create;
  FSceneTexture := aSceneTexture;
  aSceneTexture.Subscribe(Self);
  FShaderProgram := ShaderGenerator.GenCompositionShader;
  SetBlurWidth(4.0);
  FBlurAmount := 0.005;
end;

destructor TGlowPipelineEffect.Destroy;
begin
  if Assigned(FSceneTexture) then begin
    FSceneTexture.Unsubscribe(Self);
    FSceneTexture := nil;
  end;
  inherited;
end;

function TGlowPipelineEffect.GetConvolutionShader: TShaderProgram;
begin
  if not Assigned(FConvolutionShader) then FConvolutionShader := ShaderGenerator.Gen1DConvolution;
  Result := FConvolutionShader;
end;

function TGlowPipelineEffect.GetSceneSampler: TTextureSampler;
begin
  if not Assigned(FSceneSampler) then begin
    FSceneSampler := Storage.CreateTextureSample;
    FSceneSampler.WrapS := twClampToEdge;
    FSceneSampler.WrapT := twClampToEdge;
    FSceneSampler.minFilter := mnNearest;
    FSceneSampler.magFilter := mgLinear;
  end;
  Result := FSceneSampler;
end;

function TGlowPipelineEffect.GetScreenQuad: TVertexObject;
begin
  if not Assigned(FScreenQuad) then FScreenQuad := CreateSprite(2, 2);
  Result := FScreenQuad;
end;

function TGlowPipelineEffect.GetWeightCount: integer;
begin
  Result := Length(FWeigts);
end;

function TGlowPipelineEffect.GetWeigts: PSingle;
begin
  Result := @FWeigts[0];
end;

class function TGlowPipelineEffect.IsInner: boolean;
begin
  Result := true;
end;

procedure TGlowPipelineEffect.SetBlurWidth(const Value: Single);
begin
  if FBlurWidth <> Value then begin
    FBlurWidth := Value;
    GenGaussianWeights;
  end;
end;

// 1d Gaussian distribution, s is standard deviation
function TGlowPipelineEffect.Gaussian(x, s: Single): Single;
begin
  Result := exp(-x*x/(2.0*s*s)) / (s*sqrt(2.0*PI));
end;

// generate array of weights for Gaussian blur
procedure TGlowPipelineEffect.GenGaussianWeights;
var
  width, size, x: integer;
  sum: Single;
begin
  width := floor(3.0 * FBlurWidth) - 1;
  size := width * 2 + 1;
  SetLength(FWeigts, size);
  sum := 0.0;
  for x := 0 to size - 1 do
  begin
    FWeigts[x] := Gaussian(x - width, FBlurWidth);
    sum := sum + FWeigts[x];
  end;

  for x := 0 to size - 1 do
    FWeigts[x] := FWeigts[x] / sum;
end;

end.
