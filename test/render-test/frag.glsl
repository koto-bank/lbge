#version 330

in vec3 color_vert;
out vec4 color;

void main ()
{
    color.xyz = color_vert;
    color.w = 1.0;
}
