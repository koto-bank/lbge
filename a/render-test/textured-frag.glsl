#version 330

in vec2 tex_coord0;

uniform sampler2D sampler0;

uniform vec4 in_color;

out vec4 color;

void main()
{
    color = texture(sampler0, tex_coord0);
}
