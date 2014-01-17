#version 400

uniform sampler2D tex;

in vec2 TexCoord;

layout(location = 0) out vec4 FragColor;

void main() 
{
  vec4 color = texture2D(tex, TexCoord);
  FragColor.rgb = color.rgb;
  FragColor.a = 1.0;
  vec2 tc = abs(TexCoord * 2.0 - 1.0);
  if (tc.s > 0.96875 || tc.t > 0.96875) FragColor.rgb = vec3(1.0,0.0,0.0);
}
