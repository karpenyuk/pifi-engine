unit uGizmo;

interface

uses uBaseClasses, uRenderResource;

type

  TGizmoMode = (gmNone, gmMoving, gmRotating, glScaling);

  TGizmoObject = class(TMovableObject)
  private
    FMoveMeshes: TMeshObjectsList;
    FRotateMeshes: TMeshObjectsList;
    FScaleMeshes: TMeshObjectsList;
    FMode: TGizmoMode;
    function GetMeshes(aMode: TGizmoMode): TMeshObjectsList;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Meshes[aMode: TGizmoMode]: TMeshObjectsList read GetMeshes;
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
  mo: TMeshObject;
  shader: TShaderProgram;
  matRed, matGreen, matBlue: TMaterialObject;
begin
  inherited;
  FMoveMeshes := TMeshObjectsList.Create;
  FRotateMeshes := TMeshObjectsList.Create;
  FScaleMeshes := TMeshObjectsList.Create;

  shader := ShaderGenerator.GenForwardLightShader();
  matRed := Storage.CreateMaterialObject;
  matRed.AttachShader(shader);
  with matRed.AddNewMaterial('GizmoRed') do begin
    Properties.AmbientColor.SetColor(200, 0, 0, 255);
    Properties.DiffuseColor.SetColor(0, 0, 0, 255);
    Properties.SpecularColor.SetColor(220, 210, 210, 255);
  end;
  matGreen := Storage.CreateMaterialObject;
  matGreen.AttachShader(shader);
  with matGreen.AddNewMaterial('GizmoGreen') do begin
    Properties.AmbientColor.SetColor(200, 0, 0, 255);
    Properties.DiffuseColor.SetColor(0, 0, 0, 255);
    Properties.SpecularColor.SetColor(220, 210, 210, 255);
  end;
  matBlue := Storage.CreateMaterialObject;
  matBlue.AttachShader(shader);
  with matBlue.AddNewMaterial('GizmoBlue') do begin
    Properties.AmbientColor.SetColor(200, 0, 0, 255);
    Properties.DiffuseColor.SetColor(0, 0, 0, 255);
    Properties.SpecularColor.SetColor(220, 210, 210, 255);
  end;

  vo := CreateCylinder(0.1, 0.1, 16, 12, 10);
  ma := Storage.CreateMeshAssembly;
  ma.AddNewMesh(vo).MaterialObject := matRed; // X arrow stick
  ma.AddNewMesh(vo).MaterialObject := matGreen; // Y arrow stick
  ma.AddNewMesh(vo).MaterialObject := matBlue; // Z arrow stick
  vo := CreateCone(0.3, 0.45, 16, 3);
  ma.AddNewMesh(vo).MaterialObject := matRed; // X arrow spike
  ma.AddNewMesh(vo).MaterialObject := matGreen; // Y arrow spike
  ma.AddNewMesh(vo).MaterialObject := matBlue; // Z arrow spike

  ma.LocalMatrices[0] := TMatrix.RotationMatrix(Vector(0.0,0.0,1.0),Pi/2) * TMatrix.TranslationMatrix(Vector(8,0,0));
  ma.LocalMatrices[1] := TMatrix.TranslationMatrix(Vector(0,8,0));
  ma.LocalMatrices[2] := TMatrix.RotationMatrix(Vector(1.0,0.0,0.0),Pi/2) * TMatrix.TranslationMatrix(Vector(0,0,8));
  ma.LocalMatrices[3] := TMatrix.RotationMatrix(Vector(0.0,0.0,1.0),Pi/2) * TMatrix.TranslationMatrix(Vector(16.225,0,0));
  ma.LocalMatrices[4] := TMatrix.TranslationMatrix(Vector(0,16.225,0));
  ma.LocalMatrices[5] := TMatrix.RotationMatrix(Vector(1.0,0.0,0.0),Pi/2) * TMatrix.TranslationMatrix(Vector(0,0,16.225));

  mo := Storage.CreateMeshObject(ma);
  FMoveMeshes.AddMeshObject(mo);

  FMode := gmMoving;
end;

destructor TGizmoObject.Destroy;
begin
  FMoveMeshes.Destroy;
  FRotateMeshes.Destroy;
  FScaleMeshes.Destroy;
  inherited;
end;

function TGizmoObject.GetMeshes(aMode: TGizmoMode): TMeshObjectsList;
begin
  Result := nil;
  case aMode of
    gmMoving: Result := FMoveMeshes;
    gmRotating: Result := FRotateMeshes;
    glScaling: Result := FScaleMeshes;
  end;
end;

end.

