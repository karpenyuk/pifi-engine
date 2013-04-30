#version 150

invariant in vec3 in_Position;
uniform mat4 ModelMatrix;
uniform mat4 ViewProjectionMatrix;
out vec4 AbsolutePosition;

void main(void)
{
	AbsolutePosition = ModelMatrix * vec4(in_Position, 1.0);
	gl_Position = ViewProjectionMatrix * AbsolutePosition;
}
