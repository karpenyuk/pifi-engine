#version 430 core
#extension GL_ARB_shader_storage_buffer_object : require

layout(location = 0) 
in vec3 in_Position;
layout(location = 1) 
in vec3 in_Normal;
layout(location = 2) 
in vec2 in_TexCoord;

struct Light
{
    vec4    position;
    vec4    ambient;
    vec4    diffuse;
    vec4    specular;
    float   constant_attenuation;
    float   linear_attenuation;
    float   quadratic_attenuation;
    vec3    spot_direction;
    float   spot_cutoff;
    float   spot_exponent;
};
 
layout(std140) uniform Lights
{
    Light light;
} lights;
 
layout(std140) uniform Material
{
    vec4    ambient;
    vec4    diffuse;
    vec4    specular;
    vec4    emissive;
    float   shininess;
} material;

//layout(std430, binding = 5) buffer MVPMatrix 
//{ 
//	mat4 MVP[];
//} mvpMat;

//layout(std140, binding = 6) buffer OutMatrix { mat4 res[];};

uniform int ObjectIndex;
uniform mat4 ProjMatrix;

layout(std140, binding = 1) uniform MVMatrix 
{ 
	mat4[256] ModelView;
} mvMat;

out vec2 TexCoord;
out vec3 Normal;
//out mat4 mvp;

void main(void)
{ 
	TexCoord = in_TexCoord;
	Normal = in_Normal;
    mat4 mv = mvMat.ModelView[ObjectIndex];

	vec4 nm = mv * vec4(Normal, 0.0);
	vec4 pos = mv * vec4(in_Position, 1.0);

    gl_Position = ProjMatrix * pos;



	vec3 lightDir = vec3(lights.light.position.xyz - pos.xyz);
	vec3 eyeVec = -pos.xyz;

	vec4 final_color = lights.light.ambient * material.ambient;
	
	vec3 N = normalize(nm.xyz);
	vec3 L = normalize(lightDir);
	
	float lambertTerm = max (dot(N,L), 0.0);

 		
	vec3 E = normalize(eyeVec);
	vec3 R = reflect(-L, N);
	float pf = pow( max(dot(R, E), 0.0), material.shininess );

	final_color += lights.light.diffuse * material.diffuse * lambertTerm;	
	final_color += material.specular * lights.light.specular * pf;
   
//	gl_FrontColor = final_color;
}
