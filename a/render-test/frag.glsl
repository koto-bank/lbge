#version 420

in vec2 tex_coord0;

layout(binding = 0) uniform sampler2D sampler0;

uniform vec4 in_color;

out vec4 color;

void main()
{
    if (tex_coord0 != vec2(0.0f)) {
        color = texture(sampler0, tex_coord0);
    } else {
        color = in_color;
    }
}
