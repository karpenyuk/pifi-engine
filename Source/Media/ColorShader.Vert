#version 420

layout(location = 0) 
in vec2 in_Position;
layout(location = 3) 
in vec3 in_Color;

uniform mat4 Projection;

out vec3 Color;

void main()
{ 
	gl_Position = Projection*vec4(in_Position, 0.0, 1.0);
	Color = in_Color;
}