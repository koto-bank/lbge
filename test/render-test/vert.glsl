#version 330

layout(location = 0) in vec4 vert_pos;

uniform mat4 model_view;

void main() {
    gl_Position = model_view * vert_pos;
}
