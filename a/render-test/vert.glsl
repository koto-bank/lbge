#version 330

layout(location = 0) in vec4 vert_pos;
layout(location = 1) in vec2 tex_coord;

uniform mat4 model_view;
uniform mat4 projection;

out vec3 vert_color0;
out vec2 tex_coord0;

void main() {
    gl_Position = (projection * model_view) * vert_pos;
    tex_coord0 = tex_coord;
}
