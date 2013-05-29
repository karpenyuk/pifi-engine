#version 430

in vec2 TexCoord;

layout(binding=0) uniform isampler2D Patches;
layout(location = 0) out vec4 FragColor;

void main() 
{
  FragColor = vec4(texture(Patches, TexCoord).rgba) / vec4(64.0);
}