#version 420

in vec2 tex_coord0;

layout(binding = 0) uniform sampler2D sampler0;

out vec4 color;

void main()
{
    color.xyz = vec3(1.0, 0.0, 0.0);
    if (tex_coord0 != vec2(0.0f)) {
        color = texture(sampler0, tex_coord0);
    }
}
