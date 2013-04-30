#version 420

layout(location = 0) 
in vec3 in_Position;
layout(location = 1) 
in vec3 in_Normal;
layout(location = 2) 
in vec2 in_TexCoord;

uniform mat4 MVP;
uniform mat4 ModelView;
uniform mat4 Projection;

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

out vec2 TexCoord;
out vec3 Normal;

void main(void)
{ 
	gl_Position = MVP*vec4(in_Position, 1.0);
	TexCoord = in_TexCoord;
	Normal = in_Normal;
	vec4 nm = MVP * vec4(Normal, 0.0);
	vec4 pos = ModelView * vec4(in_Position, 1.0);
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
   
	gl_FrontColor = final_color;
}