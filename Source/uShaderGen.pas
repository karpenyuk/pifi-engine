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
'#version 430'+#13#10 +
'#extension GL_ARB_enhanced_layouts: enable'+#13#10 +
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

'layout(std140, binding = 2) uniform Objects'+#13#10 +
'{'+#13#10 +
'    Object object;'+#13#10 +
'} objects;'+#13#10 +

'layout(std140, binding = 1) uniform Cameras'+#13#10 +
'{'+#13#10 +
'    Camera camera;'+#13#10 +
'} cameras;'+#13#10 +

'out vec3 WorldPosition;'+#13#10 +
'out vec2 TexCoord;'+#13#10 +
'out vec3 WorldNormal;'+#13#10 +
'out vec3 ViewDir;'+#13#10 +

'void main(void)'+#13#10 +
'{'+#13#10 +
'	vec4 pos = objects.object.World * vec4(in_Position, 1.0);'+#13#10 +
' WorldPosition = pos.xyz;'+#13#10 +
' ViewDir = cameras.camera.View[2].xyz;'+#13#10 +
'	gl_Position = cameras.camera.ViewProjection * pos;'+#13#10 +
'	TexCoord = in_TexCoord;'+#13#10 +
'	WorldNormal = mat3(objects.object.WorldNormal) * in_Normal;'+#13#10 +
'}';
  ft :=
'#version 430'+#13#10 +
'#extension GL_ARB_enhanced_layouts: enable'+#13#10 +
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

'layout(std140, binding = 4) uniform LightIndices'+#13#10 +
'{'+#13#10 +
'    int indices[8];'+#13#10 +
'} lightIndices;'+#13#10 +

'layout(std140, binding = 3) uniform Material'+#13#10 +
'{'+#13#10 +
'    vec4    ambient;'+#13#10 +
'    vec4    diffuse;'+#13#10 +
'    vec4    specular;'+#13#10 +
'    vec4    emissive;'+#13#10 +
'    float   shininess;'+#13#10 +
'    vec3    padding;'+#13#10 +
'} material;'+#13#10 +

'in vec3 WorldPosition;'+#13#10 +
'in vec2 TexCoord;'+#13#10 +
'in vec3 WorldNormal;'+#13#10 +
'in vec3 ViewDir;'+#13#10 +
'layout(location = 0) out vec4 FragColor;'+#13#10 +
'layout(binding = 0) uniform samplerBuffer Lights;'+#13#10 +
'uniform int LightNumber = 0;'#10#13+

'vec3 Normal;'+#13#10 +
'vec3 CameraVector;'+#13#10 +
'vec4 LightAmbient;'+#13#10 +
'vec4 LightDiffuse;'+#13#10 +
'vec4 LightSpecular;'+#13#10 +

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

'void main()'+#13#10 +
'{'+#13#10 +
'  Normal = normalize(WorldNormal);'+#13#10 +
'  CameraVector = normalize(ViewDir);'+#13#10 +
'  LightAmbient = vec4(0.0);'+#13#10 +
'  LightDiffuse = vec4(0.0);'+#13#10 +
'  LightSpecular = vec4(0.0);'+#13#10 +
'    for (int I = 0; I < 8 && I < LightNumber; I++) {'#10#13+
'        int idx = lightIndices.indices[I];'#10#13+
'        vec4 value;'#10#13+
'        Light source;'#10#13+
'        source.position = texelFetch(Lights, idx).rgba; idx++;'#10#13+
'        source.ambient = texelFetch(Lights, idx).rgba; idx++;'+#13#10 +
'        source.diffuse = texelFetch(Lights, idx).rgba; idx++;'+#13#10 +
'        source.specular = texelFetch(Lights, idx).rgba; idx++;'+#13#10 +
'        value = texelFetch(Lights, idx).rgba; idx++;'+#13#10 +
'        source.constant_attenuation = value.r;'+#13#10 +
'        source.linear_attenuation = value.g;'+#13#10 +
'        source.quadratic_attenuation = value.b;'+#13#10 +
'        source.spot_cutoff = value.a;'+#13#10 +
'        value = texelFetch(Lights, idx).rgba; idx++;'+#13#10 +
'        source.spot_exponent = value.r;'+#13#10 +
'        source.spot_direction = value.gba;'+#13#10 +
'        if (source.position.w == 1.0)'#10#13+
'        {'#10#13+
'            if (source.spot_cutoff == -1.0){ pointLight(source); }'#10#13+
'            else { spotLight(source); }'#10#13+
'        }'#10#13+
'        else'#10#13+
'        {'#10#13+
'            if (source.spot_cutoff == -1.0) { directionalLight(source); }'#10#13+
'            else { infiniteSpotLight(source); }'#10#13+
'        }'#10#13+
'    }'#10#13+
'    LightAmbient = clamp(LightAmbient, vec4(0.0), vec4(1.0));'#10#13+
'    LightDiffuse = clamp(LightDiffuse, vec4(0.0), vec4(1.0));'#10#13+
'    LightSpecular = clamp(LightSpecular, vec4(0.0), vec4(1.0));'#10#13+
'    vec4 finalColor = material.emissive + material.ambient * LightAmbient;'#10#13+
'    finalColor += material.diffuse * LightDiffuse;'#10#13+
'    finalColor += material.specular * LightSpecular;'#10#13+
'    FragColor = vec4(finalColor.rgb, material.diffuse.a);'#10#13+
'}';
  result.ShaderText[stVertex]:=vt;
  result.ShaderText[stFragment]:=ft;
  result.AddBuildinUniform(GetUniformLightNumber);
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
'#version 430'+#13#10 +
'#extension GL_ARB_enhanced_layouts: enable'+#13#10 +
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

'layout(std140, binding = 2) uniform Objects'+#13#10 +
'{'+#13#10 +
'    Object object;'+#13#10 +
'} objects;'+#13#10 +

'layout(std140, binding = 1) uniform Cameras'+#13#10 +
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

'layout(std140, binding = 4) uniform Lights'+#13#10 +
'{'+#13#10 +
'    Light light;'+#13#10 +
'} lights;'+#13#10 +

'layout(std140, binding = 3) uniform Material'+#13#10 +
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
'	float pf = max(pow( max(dot(R, E), 0.0), material.shininess ), 0.0);'+#13#10 +
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

initialization

finalization

  ShaderGenerator.UniformLightNumber.Free;

end.
