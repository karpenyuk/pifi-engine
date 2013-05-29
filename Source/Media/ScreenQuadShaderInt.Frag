#version 430

in vec2 TexCoord;

layout(binding=0) uniform isampler2D Patches;
layout(location = 0) out vec4 FragColor;

void main() 
{
  ivec4 p = texture(Patches, TexCoord).rgba % ivec4(64);
  if (p.x < 0) p.x += 64;
  if (p.y < 0) p.y += 64;
  FragColor = vec4(p) / vec4(64.0);
}