#version 430
#extension GL_EXT_gpu_shader4 : enable
#extension GL_EXT_geometry_shader4 : enable

layout(triangles_adjacency) in;
layout(line_strip, max_vertices = 6) out;

uniform vec4 Color;
uniform vec4 EyePosition;
in vec4 AbsolutePosition[6];
out vec4 fpColor;

/*
       1 - - - 2- - ->3
              ^\
        \     | \     |
              |  \
          \   |   \   |
              |    \
            \ |     \ |
              |      v
              0<------4

                \     |

                  \   |

                    \ |

                      5
*/

// calculating facing of a triangle relative to eye
float facing(vec4 v0, vec4 v1, vec4 v2, vec4 eye_pos)
{
    vec3 e0 = v1.xyz - v0.xyz;
    vec3 e1 = v2.xyz - v0.xyz;
    vec4 p;
    p.xyz = cross(e1, e0);
    p.w = -dot(v0.xyz, p.xyz);
    return dot(p, eye_pos);
}

// output lines on silhouette edges by comparing facing of adjacent triangles
void main(void)
{
    float f = facing(AbsolutePosition[0], AbsolutePosition[2], AbsolutePosition[4], EyePosition);
    // only look at front facing triangles
    if (f > 0)
    {
        float f;
        // test edge 0
        f = facing(AbsolutePosition[0], AbsolutePosition[1], AbsolutePosition[2], EyePosition);
        if (f <= 0)
        {
            gl_Position = gl_PositionIn[0];
            fpColor = Color;
            EmitVertex();
            gl_Position = gl_PositionIn[2];
            fpColor = Color;
            EmitVertex();
            EndPrimitive();
        }

        // test edge 1
        f = facing(AbsolutePosition[2], AbsolutePosition[3], AbsolutePosition[4], EyePosition);
        if (f <= 0)
        {
            gl_Position = gl_PositionIn[2];
            fpColor = Color;
            EmitVertex();
            gl_Position = gl_PositionIn[4];
            fpColor = Color;
            EmitVertex();
            EndPrimitive();
        }

        // test edge 2
        f = facing(AbsolutePosition[4], AbsolutePosition[5], AbsolutePosition[0], EyePosition);
        if (f <= 0)
        {
            gl_Position = gl_PositionIn[4];
            fpColor = Color;
            EmitVertex();
            gl_Position = gl_PositionIn[0];
            fpColor = Color;
            EmitVertex();
            EndPrimitive();
        }
    }
}
