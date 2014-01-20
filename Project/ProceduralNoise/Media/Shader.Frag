#version 400

uniform sampler2D tex;

in vec2 TexCoord;

layout(location = 0) out vec4 FragColor;

void main() 
{
  vec4 color = texture2D(tex, TexCoord);
  FragColor.rgb = color.rrr;
  FragColor.a = 1.0;
}
