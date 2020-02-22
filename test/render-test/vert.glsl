#version 330

layout(location = 0) in vec4 vert_pos;

uniform mat4 model_view;
uniform mat4 projection;

void main() {
    mat4 proj = projection;
    gl_Position = (proj * model_view) * vert_pos;
}
