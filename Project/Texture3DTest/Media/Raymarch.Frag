#version 420

uniform vec3 eyerayOrigin;
layout(binding=0) uniform sampler3D volumeTex;
uniform int steps;
uniform float brightness;
uniform float density;
uniform float threshold;
uniform vec3 boxMin;
uniform vec3 boxMax;
uniform vec3 texSize;

in vec3 eyerayDirection;
out vec4 FragColor;

vec3 NormEyerayDirection;

#define FRONT_TO_BACK

bool IntersectBox(vec3 boxmin, vec3 boxmax, out float tnear, out float tfar)
{
    // compute intersection of ray with all six bbox planes
    vec3 invR = 1.0 / NormEyerayDirection;
    vec3 tbot = invR * (boxmin.xyz - eyerayOrigin);
    vec3 ttop = invR * (boxmax.xyz - eyerayOrigin);

    // re-order intersections to find smallest and largest on each axis
    vec3 tmin = min (ttop, tbot);
    vec3 tmax = max (ttop, tbot);

    // find the largest tmin and the smallest tmax
    vec2 t0 = max (tmin.xx, tmin.yz);
    float largest_tmin = max (t0.x, t0.y);
    t0 = min (tmax.xx, tmax.yz);
    float smallest_tmax = min (t0.x, t0.y);

    // check for hit
    bool hit;
    if ((largest_tmin > smallest_tmax)) 
        hit = false;
    else
        hit = true;

    tnear = largest_tmin;
    tfar = smallest_tmax;

    return hit;
}


// fragment program
void main()
{
    float stepsize = 1.7 / float(steps);

    NormEyerayDirection = normalize(eyerayDirection);

    // calculate ray intersection with bounding box
    float tnear, tfar;
    bool hit = IntersectBox(boxMin, boxMax, tnear, tfar);
    if (!hit) discard;
    if (tnear < 0.0) tnear = 0.0;

    // calculate intersection points
    vec3 Pnear = eyerayOrigin + NormEyerayDirection*tnear;
    Pnear *= texSize;
    vec3 Pfar = eyerayOrigin + NormEyerayDirection*tfar;
    // convert to texture space
    Pnear = Pnear*0.5 + 0.5;
    Pfar = Pfar*0.5 + 0.5;
    
    // march along ray, accumulating color
    vec4 c = vec4(0., 0., 0., 0.);

#ifdef FRONT_TO_BACK
    // use front-to-back rendering
    vec3 P = Pnear;
    vec3 Pstep = NormEyerayDirection * stepsize * texSize;
#else
    // use back-to-front rendering
    vec3 P = Pfar;
    vec3 Pstep = -NormEyerayDirection * stepsize * texSize;
#endif

    for(int i=0; i<steps; i++) {
        vec4 s = texture(volumeTex, P);

        s = abs(s);
        s.a = clamp(s.a, 0., 1.);
        s.a *= density;

#ifdef FRONT_TO_BACK
        s.rgb *= s.a;   // premultiply alpha
        c = (1. - c.a)*s + c;
        // early exit if opaque
        if (c.a > threshold)
            break;
#else
        c = mix(c, s, s.a);
#endif
        P += Pstep;
    }
    c.rgb *= brightness;

    FragColor = c;
}
