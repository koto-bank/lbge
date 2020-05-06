#version 330

in vec3 vert_color0;
in vec2 tex_coord0;

uniform sampler2D sampler0;

out vec4 color;

void main()
{
    color.xyz = vert_color0;
    if (tex_coord0 != vec2(0.0f)) {
        color = texture(sampler0, tex_coord0);
    }
}
