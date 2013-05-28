#version 430

in vec2 TexCoord;

layout(binding=0) uniform sampler2D Map;
layout(binding=1) uniform isampler2D Patches;

layout(location = 0) out vec4 FragColor;

void main() 
{
  FragColor = texture(Map, TexCoord).rgba;
  FragColor += vec4(texture(Patches, TexCoord).rgba) / vec4(64.0);
}