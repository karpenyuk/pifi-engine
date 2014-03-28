unit uGizmo;

interface

uses uBaseClasses, uRenderResource, uBaseTypes, uLists;

type

  TGizmoMode = (gmNone, gmMoving, gmRotating, gmScaling);

  TGizmoObject = class(TMovableObject)
  private
    FMoveMeshe: TMeshObject;
    FRotateMeshe: TMeshObject;
    FScaleMeshe: TMeshObject;
    FLight: TLightSource;
    FMatRed: TMaterialObject;
    FMatGreen: TMaterialObject;
    FMatBlue: TMaterialObject;
    FMode: TGizmoMode;
    function GetMeshes(aMode: TGizmoMode): TMeshObject;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetMaterials: TObjectList; override;

    property Meshes[aMode: TGizmoMode]: TMeshObject read GetMeshes;
    property Light: TLightSource read FLight;
    property Mode: TGizmoMode read FMode write FMode;
  end;


implementation

uses
  uStorage, uPrimitives, uvMath, uShaderGen;

{ TGizmoObject }

constructor TGizmoObject.Create;
var
  vo: TVertexObject;
  ma: TMeshAssembly;
  shader: TShaderProgram;
begin
  inherited;
  FLight := Storage.CreateLight;
  FLight.LightStyle := lsParallel;
  FLight.Diffuse.SetColor(255, 255, 255, 255);
  FLight.Ambient.SetColor(255, 255, 255, 255);
  FLight.Specular.SetColor(255, 255, 255, 255);
  FLight.SpotDirection.SetVector(1, 1, 1);
  FLight.SpotDirection.SetNormalize;

  shader := ShaderGenerator.GenForwardLightShader();
  FMatRed := Storage.CreateMaterialObject;
  FMatRed.AttachShader(shader);
  with FMatRed.AddNewMaterial('GizmoRed') do begin
    Properties.AmbientColor.SetColor(200, 0, 0, 255);
    Properties.DiffuseColor.SetColor(30, 30, 30, 255);
    Properties.SpecularColor.SetColor(255, 255, 255, 255);
    Properties.Shininess := 3;
  end;
  FMatGreen := Storage.CreateMaterialObject;
  FMatGreen.AttachShader(shader);
  with FMatGreen.AddNewMaterial('GizmoGreen') do begin
    Properties.AmbientColor.SetColor(0, 200, 0, 255);
    Properties.DiffuseColor.SetColor(30, 30, 30, 255);
    Properties.SpecularColor.SetColor(255, 255, 255, 255);
    Properties.Shininess := 3;
  end;
  FMatBlue := Storage.CreateMaterialObject;
  FMatBlue.AttachShader(shader);
  with FMatBlue.AddNewMaterial('GizmoBlue') do begin
    Properties.AmbientColor.SetColor(0, 0, 200, 255);
    Properties.DiffuseColor.SetColor(30, 30, 30, 255);
    Properties.SpecularColor.SetColor(255, 255, 255, 255);
    Properties.Shininess := 3;
  end;

  vo := CreateCylinder(0.02, 0.02, 1.6, 12, 10);
  ma := Storage.CreateMeshAssembly;
  ma.AddNewMesh(vo).MaterialObject := FMatRed; // X arrow stick
  ma.AddNewMesh(vo).MaterialObject := FMatGreen; // Y arrow stick
  ma.AddNewMesh(vo).MaterialObject := FMatBlue; // Z arrow stick
  vo := CreateCone(0.12, 0.27, 16, 3);
  ma.AddNewMesh(vo).MaterialObject := FMatRed; // X arrow spike
  ma.AddNewMesh(vo).MaterialObject := FMatGreen; // Y arrow spike
  ma.AddNewMesh(vo).MaterialObject := FMatBlue; // Z arrow spike

  ma.LocalMatrices[0] := TMatrix.RotationMatrix(Vector(0.0,0.0,1.0),Pi/2) * TMatrix.TranslationMatrix(Vector(0.8,0,0));
  ma.LocalMatrices[1] := TMatrix.TranslationMatrix(Vector(0,0.8,0));
  ma.LocalMatrices[2] := TMatrix.RotationMatrix(Vector(1.0,0.0,0.0),Pi/2) * TMatrix.TranslationMatrix(Vector(0,0,0.8));
  ma.LocalMatrices[3] := TMatrix.RotationMatrix(Vector(0.0,0.0,1.0),Pi/2) * TMatrix.TranslationMatrix(Vector(1.735,0,0));
  ma.LocalMatrices[4] := TMatrix.TranslationMatrix(Vector(0,1.735,0));
  ma.LocalMatrices[5] := TMatrix.RotationMatrix(Vector(1.0,0.0,0.0),-Pi/2) * TMatrix.TranslationMatrix(Vector(0,0,1.735));

  FMoveMeshe := Storage.CreateMeshObject(ma);

  FMode := gmMoving;
end;

destructor TGizmoObject.Destroy;
begin
  inherited;
end;

function TGizmoObject.GetMaterials: TObjectList;
begin
  Result := inherited GetMaterials;
  Result.Add(FMatRed);
  Result.Add(FMatGreen);
  Result.Add(FMatBlue);
end;

function TGizmoObject.GetMeshes(aMode: TGizmoMode): TMeshObject;
begin
  Result := nil;
  case aMode of
    gmMoving: Result := FMoveMeshe;
    gmRotating: Result := FRotateMeshe;
    gmScaling: Result := FScaleMeshe;
  end;
end;

end.

