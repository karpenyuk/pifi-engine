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

  ShaderGenerator = class
  private
    class var UniformLightNumber: TBuiltinUniformLightNumber;
  public
    class function UBOParamShader: TShaderProgram; static;
    class function GenForwardLightShader: TShaderProgram; static;
    class function GenLightGlyphShader: TShaderProgram; static;
    class function GenScreenQuadShader: TShaderProgram; static;

    class function GetUniformLightNumber: TBuiltinUniformLightNumber;
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

const
  SG_STRUCT_OBJECT: ansistring =
  'struct Object'+#10#13 +
  '{'+#10#13 +
  '     mat4 World;'+#10#13 +
  '     mat4 WorldInv;'+#10#13 +
  '     mat4 WorldNormal;'+#10#13 +
  '};'+#10#13;

  SG_STRUCT_CAMERA: ansistring =
  'struct Camera'+#10#13 +
  '{'+#10#13 +
  '     mat4 View;'+#10#13 +
  '     mat4 Projection;'+#10#13 +
  '     mat4 ViewProjection;'+#10#13 +
   '};'+#10#13;

  SG_STRUCT_LIGHT: ansistring =
  'struct Light'+#10#13 +
  '{'+#10#13 +
  '    vec4    position;'+#10#13 +
  '    vec4    ambient;'+#10#13 +
  '    vec4    diffuse;'+#10#13 +
  '    vec4    specular;'+#10#13 +
  '    float   constant_attenuation;'+#10#13 +
  '    float   linear_attenuation;'+#10#13 +
  '    float   quadratic_attenuation;'+#10#13 +
  '    float   spot_cutoff;'+#10#13 +
  '    float   spot_exponent;'+#10#13 +
  '    vec3    spot_direction;'+#10#13 +
  '};'+#10#13;

  SG_BLOCK_OBJECT: ansistring =
  'layout(std140, binding = 2) uniform Objects'+#10#13 +
  '{'+#10#13 +
  '    Object object;'+#10#13 +
  '} objects;'+#10#13;

  SG_BLOCK_CAMERA: ansistring =
  'layout(std140, binding = 1) uniform Cameras'+#10#13 +
  '{'+#10#13 +
  '    Camera camera;'+#10#13 +
  '} cameras;'+#10#13;

  SG_BLOCK_MATERIAL: ansistring =
  'layout(std140, binding = 3) uniform Material'+#10#13 +
  '{'+#10#13 +
  '    vec4    ambient;'+#10#13 +
  '    vec4    diffuse;'+#10#13 +
  '    vec4    specular;'+#10#13 +
  '    vec4    emissive;'+#10#13 +
  '    float   shininess;'+#10#13 +
  '    vec3    padding;'+#10#13 +
  '} material;'+#10#13;

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

class function ShaderGenerator.GenForwardLightShader: TShaderProgram;
var vt,ft: ansistring;
begin
  result:=TShaderProgram.Create;
  vt:=
'#version 430'+#10#13 +
'layout(location = 0) in vec3 in_Position;'+#10#13 +
'layout(location = 1) in vec3 in_Normal;'+#10#13 +
'layout(location = 2) in vec2 in_TexCoord;'+#10#13 +
SG_STRUCT_OBJECT +
SG_STRUCT_CAMERA +
SG_BLOCK_OBJECT +
SG_BLOCK_CAMERA +

'out vec3 WorldPosition;'+#10#13 +
'out vec2 TexCoord;'+#10#13 +
'out vec3 WorldNormal;'+#10#13 +
'out vec3 ViewDir;'+#10#13 +

'void main(void)'+#10#13 +
'{'+#10#13 +
'	vec4 pos = objects.object.World * vec4(in_Position, 1.0);'+#10#13 +
' WorldPosition = pos.xyz;'+#10#13 +
' ViewDir = cameras.camera.View[2].xyz;'+#10#13 +
'	gl_Position = cameras.camera.ViewProjection * pos;'+#10#13 +
'	TexCoord = in_TexCoord;'+#10#13 +
'	WorldNormal = mat3(objects.object.WorldNormal) * in_Normal;'+#10#13 +
'}';
  ft :=
'#version 430'+#10#13 +

SG_STRUCT_LIGHT +

'layout(std140, binding = 4) uniform LightIndices'+#10#13 +
'{'+#10#13 +
'    ivec4 indices[8];'+#10#13 +
'} lightIndices;'+#10#13 +

SG_BLOCK_MATERIAL +

'in vec3 WorldPosition;'+#10#13 +
'in vec2 TexCoord;'+#10#13 +
'in vec3 WorldNormal;'+#10#13 +
'in vec3 ViewDir;'+#10#13 +
'layout(location = 0) out vec4 FragColor;'+#10#13 +
'layout(binding = 10) uniform samplerBuffer Lights;'+#10#13 +
'uniform int LightNumber = 0;'#10#13+

'vec3 Normal;'+#10#13 +
'vec3 CameraVector;'+#10#13 +
'vec4 LightAmbient;'+#10#13 +
'vec4 LightDiffuse;'+#10#13 +
'vec4 LightSpecular;'+#10#13 +

    'void pointLight(Light A)'#10#13+
    '{'#10#13+
    '    float nDotVP;'#10#13+
    '    float nDotHV;'#10#13+
    '    float attenuation;'#10#13+
    '    float d;'#10#13+
    '    vec3 VP;'#10#13+
    '    vec3 halfVector;'#10#13+
    '    float pf;'#10#13+
    '    VP = A.position.xyz - WorldPosition.xyz;'#10#13+
    '    d = length(VP);'#10#13+
    '    VP = normalize(VP);'#10#13+
    '    attenuation = 1.0 / (A.constant_attenuation + A.linear_attenuation * d + A.quadratic_attenuation * d * d);'#10#13+
    '    halfVector = normalize(VP + CameraVector);'#10#13+
    '    nDotVP = max(0.0, dot(Normal, VP));'#10#13+
    '    pf = max(0.0, dot(Normal, halfVector));'#10#13+
    '    nDotHV = sign(pf)*pow(pf, material.shininess);'#10#13+
    '    LightAmbient += A.ambient * attenuation;'#10#13+
    '    LightDiffuse += A.diffuse * nDotVP * attenuation;'#10#13+
    '    LightSpecular += A.specular * nDotHV * attenuation;'#10#13+
    '}'#10#13+
    'void spotLight(Light A)'#10#13+
    '{'#10#13+
    '    float nDotVP;'#10#13+
    '    float nDotHV;'#10#13+
    '    float spotDot;'#10#13+
    '    float spotAttenuation;'#10#13+
    '    float attenuation;'#10#13+
    '    float d;'#10#13+
    '    vec3 VP;'#10#13+
    '    vec3 halfVector;'#10#13+
    '    float pf;'#10#13+
    '    VP = A.position.xyz - WorldPosition.xyz;'#10#13+
    '    d = length(VP);'#10#13+
    '    VP = normalize(VP);'#10#13+
    '    attenuation = 1.0 / (A.constant_attenuation + A.linear_attenuation * d + A.quadratic_attenuation * d * d);'#10#13+
    '    spotDot = dot(-VP, normalize(A.spot_direction));'#10#13+
    '    if (spotDot < A.spot_cutoff) { spotAttenuation = 0.0; }'#10#13+
    '    else { spotAttenuation = pow(spotDot, A.spot_exponent); }'#10#13+
    '    attenuation *= spotAttenuation;'#10#13+
    '    halfVector = normalize(VP + CameraVector);'#10#13+
    '    nDotVP = max(0.0, dot(Normal, VP));'#10#13+
    '    pf = max(0.0, dot(Normal, halfVector));'#10#13+
    '    nDotHV = sign(pf)*pow(pf, material.shininess);'#10#13+
    '    LightAmbient += A.ambient * attenuation;'#10#13+
    '    LightDiffuse += A.diffuse * nDotVP * attenuation;'#10#13+
    '    LightSpecular += A.specular * nDotHV * attenuation;'#10#13+
    '}'#10#13+
    'void directionalLight(Light A)'#10#13+
    '{'#10#13+
    '    float nDotVP;'#10#13+
    '    float nDotHV;'#10#13+
    '    vec3 VP;'#10#13+
    '    vec3 halfVector;'#10#13+
    '    float pf;'#10#13+
    '    VP = normalize(A.position.xyz);'#10#13+
    '    halfVector = normalize(VP + CameraVector);'#10#13+
    '    nDotVP = max(0.0, dot(Normal, VP));'#10#13+
    '    nDotHV = pow(max(0.0, dot(Normal, halfVector)), material.shininess);'#10#13+
    '    LightAmbient += A.ambient;'#10#13+
    '    LightDiffuse += A.diffuse * nDotVP;'#10#13+
    '    LightSpecular += A.specular * nDotHV;'#10#13+
    '}'#10#13+
    'void infiniteSpotLight(Light A)'#10#13+
    '{'#10#13+
    '    float nDotVP;'#10#13+
    '    float nDotHV;'#10#13+
    '    vec3 VP;'#10#13+
    '    vec3 halfVector;'#10#13+
    '    float spotAttenuation;'#10#13+
    '    vec3 Ppli;'#10#13+
    '    vec3 Sdli;'#10#13+
    '    float pf;'#10#13+
    '    VP = normalize(A.position.xyz);'#10#13+
    '    halfVector = normalize(VP + CameraVector);'#10#13+
    '    nDotVP = max(0.0, dot(Normal, VP));'#10#13+
    '    pf = max(0.0, dot(Normal, halfVector));'#10#13+
    '    nDotHV = sign(pf)*pow(pf, material.shininess);'#10#13+
    '    Ppli = -VP;'#10#13+
    '    Sdli = A.spot_direction;'#10#13+
    '    spotAttenuation = pow(dot(Ppli, Sdli), A.spot_exponent);'#10#13+
    '    LightAmbient += A.ambient * spotAttenuation;'#10#13+
    '    LightDiffuse += A.diffuse * nDotVP * spotAttenuation;'#10#13+
    '    LightSpecular += A.specular * nDotHV * spotAttenuation;'#10#13+
    '}'#10#13+
    'void processLight(int idx) {'#10#13+
    '    vec4 value;'#10#13+
    '    Light source;'#10#13+
    '    source.position = texelFetch(Lights, idx).rgba; idx++;'#10#13+
    '    source.ambient = texelFetch(Lights, idx).rgba; idx++;'+#10#13 +
    '    source.diffuse = texelFetch(Lights, idx).rgba; idx++;'+#10#13 +
    '    source.specular = texelFetch(Lights, idx).rgba; idx++;'+#10#13 +
    '    value = texelFetch(Lights, idx).rgba; idx++;'+#10#13 +
    '    source.constant_attenuation = value.r;'+#10#13 +
    '    source.linear_attenuation = value.g;'+#10#13 +
    '    source.quadratic_attenuation = value.b;'+#10#13 +
    '    source.spot_cutoff = value.a;'+#10#13 +
    '    value = texelFetch(Lights, idx).rgba; idx++;'+#10#13 +
    '    source.spot_exponent = value.r;'+#10#13 +
    '    source.spot_direction = value.gba;'+#10#13 +
    '    if (source.position.w == 1.0)'#10#13+
    '    {'#10#13+
    '        if (source.spot_cutoff == -1.0){ pointLight(source); }'#10#13+
    '        else { spotLight(source); }'#10#13+
    '    }'#10#13+
    '    else'#10#13+
    '    {'#10#13+
    '        if (source.spot_cutoff == -1.0) { directionalLight(source); }'#10#13+
    '        else { infiniteSpotLight(source); }'#10#13+
    '    }'#10#13+
    '}'#10#13+

'void main()'+#10#13 +
'{'+#10#13 +
'  Normal = normalize(WorldNormal);'+#10#13 +
'  CameraVector = normalize(ViewDir);'+#10#13 +
'  LightAmbient = vec4(0.0);'+#10#13 +
'  LightDiffuse = vec4(0.0);'+#10#13 +
'  LightSpecular = vec4(0.0);'+#10#13 +
'  for (int I = 0; I < 8 && I < LightNumber; I++) {'#10#13+
'    processLight(lightIndices.indices[I].x);'#10#13+
'  }'#10#13+
'  LightAmbient = clamp(LightAmbient, vec4(0.0), vec4(1.0));'#10#13+
'  LightDiffuse = clamp(LightDiffuse, vec4(0.0), vec4(1.0));'#10#13+
'  LightSpecular = clamp(LightSpecular, vec4(0.0), vec4(1.0));'#10#13+
'  vec4 finalColor = material.emissive + material.ambient * LightAmbient;'#10#13+
'  finalColor += material.diffuse * LightDiffuse;'#10#13+
'  finalColor += material.specular * LightSpecular;'#10#13+
'  FragColor = vec4(finalColor.rgb, material.diffuse.a);'#10#13+
'}';
  result.ShaderText[stVertex]:=vt;
  result.ShaderText[stFragment]:=ft;
  result.AddBuildinUniform(GetUniformLightNumber);
end;

class function ShaderGenerator.GenLightGlyphShader: TShaderProgram;
var vt,ft: ansistring;
begin
  result:=TShaderProgram.Create;
  vt:=
'#version 430'#10#13 +
'layout(location = 0) in vec3 in_Position;'#10#13 +
'layout(location = 2) in vec2 in_TexCoord;'#10#13 +
SG_STRUCT_CAMERA +
SG_STRUCT_OBJECT +
SG_BLOCK_OBJECT +
SG_BLOCK_CAMERA +
'out vec2 v2f_TexCoord0;'#10#13 +
'void main() {'#10#13 +
'	vec4 pos = objects.object.World * vec4(in_Position, 1.0);'+#10#13 +
'	gl_Position = cameras.camera.Projection * pos;'+#10#13 +
'	v2f_TexCoord0 = in_TexCoord;'+#10#13 +
'}';
  ft :=
'#version 430'#10#13 +
'in vec2 v2f_TexCoord0;'#10#13 +
'layout(location = 0) out vec4 FragColor;'#10#13 +
'layout(binding = 0) uniform sampler2D DiffuseTexture;'#10#13 +
SG_STRUCT_OBJECT +
SG_BLOCK_OBJECT +
SG_BLOCK_MATERIAL +
'void main() {'#10#13 +
' vec4 Color = texture(DiffuseTexture, v2f_TexCoord0);'#10#13 +
' if (Color.a < 0.5) discard;'#10#13 +
' FragColor = material.diffuse * Color; }'#10#13;
  result.ShaderText[stVertex]:=vt;
  result.ShaderText[stFragment]:=ft;
end;

class function ShaderGenerator.GenScreenQuadShader: TShaderProgram;
var vt,ft: ansistring;
begin
  result:=TShaderProgram.Create;
  vt:=
'#version 430'+#10#13 +
'layout(location = 0) in vec3 in_Position;'+#10#13 +
'layout(location = 2) in vec2 in_TexCoord;'+#10#13 +
'out vec2 v2f_TexCoord0;'#10#13 +
'void main() {'#10#13 +
'	gl_Position = vec4(in_Position, 1.0);'+#10#13 +
'	v2f_TexCoord0 = in_TexCoord; }';
  ft :=
'#version 430'#10#13 +
'in vec2 v2f_TexCoord0;'#10#13 +
'layout(location = 0) out vec4 FragColor;'#10#13 +
'layout(binding = 0) uniform sampler2D DiffuseTexture;'#10#13 +
'void main() {'#10#13 +
' FragColor = texture(DiffuseTexture, v2f_TexCoord0); }';
  result.ShaderText[stVertex]:=vt;
  result.ShaderText[stFragment]:=ft;
end;

class function ShaderGenerator.GetUniformLightNumber: TBuiltinUniformLightNumber;
begin
  if not Assigned(UniformLightNumber) then
    UniformLightNumber := TBuiltinUniformLightNumber.Create;
  Result := UniformLightNumber;
end;

class function ShaderGenerator.UBOParamShader: TShaderProgram;
var vt,ft: ansistring;
begin
  result:=TShaderProgram.Create;
  vt:=
'#version 430'+#10#13 +
'layout(location = 0) in vec3 in_Position;'+#10#13 +
'layout(location = 1) in vec3 in_Normal;'+#10#13 +
'layout(location = 2) in vec2 in_TexCoord;'+#10#13 +
SG_STRUCT_OBJECT +
SG_STRUCT_CAMERA +
SG_BLOCK_OBJECT +
SG_BLOCK_CAMERA +
SG_STRUCT_LIGHT +
'layout(std140, binding = 4) uniform Lights'+#10#13 +
'{'+#10#13 +
'    Light light;'+#10#13 +
'} lights;'+#10#13 +
SG_BLOCK_MATERIAL +
'out vec2 TexCoord;'+#10#13 +
'out vec3 Normal;'+#10#13 +
'out vec4 Color;'+#10#13 +

'void main(void)'+#10#13 +
'{'+#10#13 +
'	vec4 pos = objects.object.World * vec4(in_Position, 1.0);'+#10#13 +
'	gl_Position = cameras.camera.ViewProjection * pos;'+#10#13 +
'	TexCoord = in_TexCoord;'+#10#13 +
'	Normal = in_Normal;'+#10#13 +
'	vec4 nm = objects.object.WorldNormal * vec4(Normal, 0.0);'+#10#13 +
'	vec3 lightDir = vec3(lights.light.position.xyz - pos.xyz);'+#10#13 +
'	vec3 eyeVec = -pos.xyz;'+#10#13 +
'	vec4 final_color = lights.light.ambient * material.ambient;'+#10#13 +
'	vec3 N = normalize(nm.xyz);'+#10#13 +
'	vec3 L = normalize(lightDir);'+#10#13 +
'	float lambertTerm = max (dot(N,L), 0.0);'+#10#13 +
'	vec3 E = normalize(eyeVec);'+#10#13 +
'	vec3 R = reflect(-L, N);'+#10#13 +
'	float pf = max(pow( max(dot(R, E), 0.0), material.shininess ), 0.0);'+#10#13 +
'	final_color += lights.light.diffuse * material.diffuse * lambertTerm;'+#10#13 +
'	final_color += material.specular * lights.light.specular * pf;'+#10#13 +
'	Color = final_color;'+#10#13 +
'}';
  ft:=
'#version 330'+#10#13 +
'in vec2 TexCoord;'+#10#13 +
'in vec4 Color;'+#10#13 +
'layout(location = 0) out vec4 FragColor;'+#10#13 +
'void main()'+#10#13 +
'{'+#10#13 +
'  FragColor = Color;'+#10#13 +
'}';
  result.ShaderText[stVertex]:=vt;
  result.ShaderText[stFragment]:=ft;
end;

initialization

finalization

  ShaderGenerator.UniformLightNumber.Free;

end.
