#version 420

layout(location = 0) 
in vec3 in_Position;
layout(location = 1) 
in vec3 in_Normal;
layout(location = 3) 
in vec3 in_Color;

uniform mat4 MVP;

out vec3 Color;

void main(void)
{ 
	gl_Position = MVP * vec4(in_Position, 1.0);
	Color = in_Color;
}