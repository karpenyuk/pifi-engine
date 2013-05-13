#version 420

in vec3 fTexCoord;
in vec2 fDecalTexCoord;
in vec3 fWorldNormal;
in vec3 fWorldLightDir;
in vec3 fTextureLightDir;
in float fIsDecal;

layout(location = 0) out vec4 FragColor;

layout(binding=0) uniform sampler1D WallStruct;
layout(binding=1) uniform sampler2DArray WallLayers;
layout(binding=2) uniform sampler2D DecalNormalMap;

void main() 
{
  vec3 L;
  vec3 N;

  if (fIsDecal > 0.99)
  {
    N = normalize(texture(DecalNormalMap, fDecalTexCoord).xyz*vec3(2.0) - vec3(1.0));
    L = normalize(fTextureLightDir);
  }
  else
  {
    N = normalize(fWorldNormal);
    L = normalize(fWorldLightDir);
  }
	
  float lambertTerm = max (dot(N,L), 0.05);

  vec3 tc = fTexCoord;
  tc.z = texture(WallStruct, tc.z).r * 255.0;
  vec3 diffuse = texture(WallLayers, tc).rgb;
  FragColor = vec4(diffuse*lambertTerm, 1.0);
}