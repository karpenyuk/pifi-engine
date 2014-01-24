#version 420

layout(location = 0) in vec3 in_Position;
out vec3 eyerayDirection;

uniform vec3 eyerayOrigin;
uniform mat4 ModelViewProjMatrix;

void main()
{
    eyerayDirection = in_Position - eyerayOrigin;
    gl_Position = ModelViewProjMatrix * vec4(in_Position, 1.0);
}