unit uShaderGen;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses Classes, uRenderResource, uBaseTypes, uMiscUtils;

type

  TShaderParts = record
    Header, Input, Output, Impl, Exec: ansistring;
  end;

  TConfigShader = class
    class function UBOParamShader: TShaderProgram; static;
  end;

  TShaderMaterial = class
  private
    FVertText: ansistring;
    FFragText: ansistring;
    FGLSLVersion: integer;
    function CreateAttribs(const attr: TAttribObject): ansistring;
    function CreateVarings(const attr: TAttribObject; const Mat: TMaterialObject): ansistring;
    function CreateVertexLights(const aLights: TList): TShaderParts;
  public
    constructor Create(aGLSLVersion: integer = 330);
    procedure GenShaderForMesh(const aMesh: TMesh; const aLights: TList);
  end;

implementation

{ TShaderMaterial }

constructor TShaderMaterial.Create(aGLSLVersion: integer);
begin
  FGLSLVersion:=aGLSLVersion;
end;

procedure TShaderMaterial.GenShaderForMesh(const aMesh: TMesh; const aLights: TList);
var attr: TAttribObject;
    i: integer;
    vs,fs: TShaderParts;
    outname: ansistring;
begin
  aMesh.MaterialObject.AttachShader(TShaderProgram.CreateOwned(aMesh.MaterialObject));
  vs.Header:='#version '+inttostr(FGLSLVersion)+#13+#10;
  if FGLSLVersion>=130 then outname:='out ' else outname:='varying ';
  for i:=0 to aMesh.VertexObject.AttribsCount-1 do begin
    attr:=aMesh.VertexObject.Attribs[i];
    if FGLSLVersion<330 then with aMesh.MaterialObject.Shader do
      SetAttribBindPos(CAttribSematics[attr.Semantic].Name,CAttribSematics[attr.Semantic].Location);
    vs.Input:=vs.Input+CreateAttribs(attr);
    vs.Output:=vs.Output+outname+'vec'+inttostr(attr.AttrSize)+
        CAttribSematics[attr.Semantic].Name+';' + #13+#10;
  end;
end;

function TShaderMaterial.CreateAttribs(const attr: TAttribObject): ansistring;
var sem: TAttribSemantic;
begin
  sem:=CAttribSematics[attr.Semantic];
  if FGLSLVersion >= 330 then
    result:=result + 'layout(location = '+inttostr(sem.Location) + ')' + #13+#10;
  if FGLSLVersion < 130 then result:=result+'attribute ' else result:=result+'in ';
  result:=result+'vec'+inttostr(attr.AttrSize)+sem.Name+';' + #13+#10;
end;


function TShaderMaterial.CreateVarings(const attr: TAttribObject; const Mat: TMaterialObject): ansistring;
begin
end;

function TShaderMaterial.CreateVertexLights(const aLights: TList): TShaderParts;
begin
//
end;

{ TConfigShader }

class function TConfigShader.UBOParamShader: TShaderProgram;
var vt,ft: ansistring;
begin
  result:=TShaderProgram.Create;
  vt:=
'#version 420'+#13#10 +
'layout(location = 0) in vec3 in_Position;'+#13#10 +
'layout(location = 1) in vec3 in_Normal;'+#13#10 +
'layout(location = 2) in vec2 in_TexCoord;'+#13#10 +

'struct Object'+#13#10 +
'{'+#13#10 +
'     mat4 World;'+#13#10 +
'     mat4 WorldInv;'+#13#10 +
'     mat4 WorldNormal;'+#13#10 +
 '};'+#13#10 +

 'struct Camera'+#13#10 +
'{'+#13#10 +
'     mat4 View;'+#13#10 +
'     mat4 Projection;'+#13#10 +
'     mat4 ViewProjection;'+#13#10 +
 '};'+#13#10 +

'layout(std140, location = 2) uniform Objects'+#13#10 +
'{'+#13#10 +
'    Object object;'+#13#10 +
'} objects;'+#13#10 +

'layout(std140, location = 1) uniform Cameras'+#13#10 +
'{'+#13#10 +
'    Camera camera;'+#13#10 +
'} cameras;'+#13#10 +

'struct Light'+#13#10 +
'{'+#13#10 +
'    vec4    position;'+#13#10 +
'    vec4    ambient;'+#13#10 +
'    vec4    diffuse;'+#13#10 +
'    vec4    specular;'+#13#10 +
'    float   constant_attenuation;'+#13#10 +
'    float   linear_attenuation;'+#13#10 +
'    float   quadratic_attenuation;'+#13#10 +
'    float   spot_cutoff;'+#13#10 +
'    float   spot_exponent;'+#13#10 +
'    vec3    spot_direction;'+#13#10 +
'};'+#13#10 +

'layout(std140, location = 4) uniform Lights'+#13#10 +
'{'+#13#10 +
'    Light light;'+#13#10 +
'} lights;'+#13#10 +

'layout(std140, location = 3) uniform Material'+#13#10 +
'{'+#13#10 +
'    vec4    ambient;'+#13#10 +
'    vec4    diffuse;'+#13#10 +
'    vec4    specular;'+#13#10 +
'    vec4    emissive;'+#13#10 +
'    float   shininess;'+#13#10 +
'    vec3    padding;'+#13#10 +
'} material;'+#13#10 +

'out vec2 TexCoord;'+#13#10 +
'out vec3 Normal;'+#13#10 +
'out vec4 Color;'+#13#10 +

'void main(void)'+#13#10 +
'{'+#13#10 +
'	vec4 pos = objects.object.World * vec4(in_Position, 1.0);'+#13#10 +
'	gl_Position = cameras.camera.ViewProjection * pos;'+#13#10 +
'	TexCoord = in_TexCoord;'+#13#10 +
'	Normal = in_Normal;'+#13#10 +
'	vec4 nm = objects.object.WorldNormal * vec4(Normal, 0.0);'+#13#10 +
'	vec3 lightDir = vec3(lights.light.position.xyz - pos.xyz);'+#13#10 +
'	vec3 eyeVec = -pos.xyz;'+#13#10 +
'	vec4 final_color = lights.light.ambient * material.ambient;'+#13#10 +
'	vec3 N = normalize(nm.xyz);'+#13#10 +
'	vec3 L = normalize(lightDir);'+#13#10 +
'	float lambertTerm = max (dot(N,L), 0.0);'+#13#10 +
'	vec3 E = normalize(eyeVec);'+#13#10 +
'	vec3 R = reflect(-L, N);'+#13#10 +
'	float pf = pow( max(dot(R, E), 0.0), material.shininess );'+#13#10 +
'	final_color += lights.light.diffuse * material.diffuse * lambertTerm;'+#13#10 +
'	final_color += material.specular * lights.light.specular * pf;'+#13#10 +
'	Color = final_color;'+#13#10 +
'}';
  ft:=
'#version 330'+#13#10 +
'in vec2 TexCoord;'+#13#10 +
'in vec4 Color;'+#13#10 +
'layout(location = 0) out vec4 FragColor;'+#13#10 +
'void main()'+#13#10 +
'{'+#13#10 +
'  FragColor = Color;'+#13#10 +
'}';
  result.ShaderText[stVertex]:=vt;
  result.ShaderText[stFragment]:=ft;
end;

end.
