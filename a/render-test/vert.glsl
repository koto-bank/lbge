#version 330

layout(location = 0) in vec4 vert_pos;

uniform mat4 MV;
uniform mat4 P;

out vec4 vert;

void main() {
    gl_Position = P * MV * vert_pos;
    vert = vert_pos;
}
