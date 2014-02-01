#version 430 core

layout(location = 0) 
in vec2 in_Position;
layout(location = 3) 
in vec3 in_Color;
layout(location = 4) 
in vec2 in_TexCoord1;

uniform mat4 Projection;
uniform vec2 Origin;

out vec3 Color;

void main()
{ 
	vec2 p = Origin + in_Position + in_TexCoord1;
	gl_Position = Projection*vec4(p, 0.0, 1.0);
	Color = in_Color;
}