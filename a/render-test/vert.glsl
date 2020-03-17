#version 330

layout(location = 0) in vec4 vert_pos;
layout(location = 1) in vec3 vert_color;

uniform mat4 model_view;
uniform mat4 projection;

out vec3 color_vert;

void main() {
    gl_Position = (projection * model_view) * vert_pos;
    color_vert = vert_color;
    if (color_vert == vec3(0.0f)) {
        color_vert = vec3(0.1f, 0.4f, 0.4f);
    }
}
