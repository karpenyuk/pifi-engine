#version  150
precision highp float;

in vec4 fpColor;
out vec4 FragColor;

void main(void)
{
    FragColor = fpColor;
}