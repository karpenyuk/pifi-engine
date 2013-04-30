#version 430

in vec2 TexCoord;

layout(location = 0) out vec4 FragColor;

void main() 
{
  FragColor = vec4(TexCoord,0.0,1.0);
}