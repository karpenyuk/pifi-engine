#version 420

layout(location = 0) 
in vec3 in_Position;
layout(location = 1) 
in vec3 in_Normal;
layout(location = 2) 
in vec3 in_TexCoord;

uniform mat4 ModelMatrix;

out vec3 vWorldPos;
out vec3 vTexCoord;
out vec3 vNormal;

void main(void)
{ 
	vWorldPos = (ModelMatrix * vec4(in_Position, 1.0)).xyz;
	vTexCoord = in_TexCoord;
	vNormal = mat3(ModelMatrix) * in_Normal;
	vNormal = normalize(vNormal);
}