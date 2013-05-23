#version 420

layout(location = 0) 
in vec2 in_Position;
layout(location = 2) 
in vec2 in_TexCoord;

uniform mat4 Projection;
uniform vec2 Ration;

out vec2 TexCoord;

void main()
{ 
	gl_Position = vec4(Ration * in_Position, 0.0, 1.0);
	TexCoord = in_TexCoord;
}