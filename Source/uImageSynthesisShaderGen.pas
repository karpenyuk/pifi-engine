unit uImageSynthesisShaderGen;

interface

const
  GLSL_SYNTH_HEADER: ansistring =
'#version 430 core'#10#13+
'#extension GL_ARB_compute_shader : require'#10#13+
'#extension GL_ARB_shader_storage_buffer_object : require'#10#13;

  GLSL_SYNTH_TILINGSUB: ansistring =
'ivec2 tiling(ivec2 c) {'#10#13+
'  c.xy = c.xy % exemplarSize;'#10#13+
'  if (c.x < 0) c.x += exemplarSize.x;'#10#13+
'  if (c.y < 0) c.y += exemplarSize.y;'#10#13+
'  return c; }'#10#13+
'ivec4 tiling(ivec4 c) {'#10#13+
'  c.xy = c.xy % exemplarSize;'#10#13+
'  if (c.x < 0) c.x += exemplarSize.x;'#10#13+
'  if (c.y < 0) c.y += exemplarSize.y;'#10#13+
'  return c; }';

  GLSL_SYNTH_RANDSUB: ansistring =
'ivec4 rand(ivec2 c) {'#10#13+
'  ivec2 tc = c + randOffeset;'#10#13+
'  tc %= synthSize;'#10#13+
'  vec2 rnd = texelFetch(Random, tc, 0).xy;'#10#13+
'  rnd = rnd * vec2(2.0) - vec2(1.0);'#10#13+
'  rnd *= strength;'#10#13+
'  return ivec4(ivec2(rnd), 0, 0);'#10#13+
'}';

  GLSL_SYNTH_UPSAMPLE_JITTER_INTERFACE: ansistring =
'layout(local_size_x = 16, local_size_y = 16, local_size_z = 1) in;'#10#13+
'layout(binding = 0) uniform isampler2D DownLevel;'#10#13+
'layout(binding = 0, rg16i) uniform iimage2D UpLevel;'#10#13+
'layout(binding = 1) uniform sampler2D Random;'#10#13+
'uniform ivec4 spacing[3];'#10#13+
'uniform ivec2 quarter;'#10#13+
'uniform ivec2 exemplarSize;'#10#13+
'uniform ivec2 synthSize;'#10#13+
'uniform ivec2 randOffeset;'#10#13+
'uniform vec2 strength;';

  GLSL_SYNTH_UPSAMPLE_JITTER_MAIN: ansistring =
'void main()'#10#13+
'{'#10#13+
'  ivec2 coords = ivec2(gl_WorkGroupID.xy * uvec2(16u) + gl_LocalInvocationID.xy);'#10#13+
'  ivec2 up_coords = coords * ivec2(2);'#10#13+
'  coords += quarter;'#10#13+
'  ivec4 p = ivec4(texelFetch(DownLevel, coords, 0).xy, 0, 0);'#10#13+
'  ivec4 delta;'#10#13+
'  delta = rand(up_coords);'#10#13+
'  imageStore(UpLevel, up_coords, delta + p);'#10#13+
'  up_coords += ivec2(1, 0);'#10#13+
'  delta = rand(up_coords);'#10#13+
'  imageStore(UpLevel, up_coords, delta + tiling(p + spacing[0]));'#10#13+
'  up_coords += ivec2(-1, 1);'#10#13+
'  delta = rand(up_coords);'#10#13+
'  imageStore(UpLevel, up_coords, delta + tiling(p + spacing[1]));'#10#13+
'  up_coords += ivec2(1, 0);'#10#13+
'  delta = rand(up_coords);'#10#13+
'  imageStore(UpLevel, up_coords, delta + tiling(p + spacing[2]));'#10#13+
'}';

  GLSL_SYNTH_CORRECTION_INTERFACE: ansistring =
'layout(local_size_x = 16, local_size_y = 16, local_size_z = 1) in;'#10#13+
'uniform ivec2 subPassOffset;'#10#13+
'uniform ivec2 exemplarSize;'#10#13+
'uniform vec3 NeighbScale1;'#10#13+
'uniform vec3 NeighbScale2;'#10#13+
'uniform vec3 NeighbOffset1;'#10#13+
'uniform vec3 NeighbOffset2;'#10#13+
'uniform ivec2 spacing;'#10#13+
'uniform float Kappa;'#10#13+
'layout(binding = 0) uniform sampler2D Exemplar;'#10#13+
'layout(binding = 1) uniform isampler2D ReadSynth;'#10#13+
'layout(binding = 2) uniform isampler2D kNearestMap;'#10#13+
'layout(binding = 3) uniform sampler2D neigbMapPart1;'#10#13+
'layout(binding = 4) uniform sampler2D neigbMapPart2;'#10#13+
'layout(binding = 0, rg16i) uniform iimage2D WriteSynth;'#10#13+
'layout(std430, binding = 0) buffer NeihgbPCAMatrix {'#10#13+
'  vec4 Matrix[150];'#10#13+
'} NPCA;';

  GLSL_SYNTH_CORRECTION_MAIN: ansistring =
'void main()'#10#13+
'{'#10#13+
'  ivec2 coords = ivec2(gl_WorkGroupID.xy * uvec2(16) + gl_LocalInvocationID.xy);'#10#13+
'  coords = coords * ivec2(2) + subPassOffset;'#10#13+
'	 vec3 N123 = vec3(0.0);'#10#13+
'	 vec3 N456 = vec3(0.0);'#10#13+
'	 int idx = 0;'#10#13+
'	 for (int ni = -2; ni<=2; ni++) {'#10#13+
'	   for (int nj = -2; nj<=2; nj++) {'#10#13+
'      ivec2 xy = tiling(coords + ivec2(nj, ni));'#10#13+
'      ivec2 p = texelFetch( ReadSynth, xy, 0 ).xy;'#10#13+
'		   vec3 N = texelFetch( Exemplar, p, 0 ).rgb;'#10#13+
'			 N123.x += dot(N, NPCA.Matrix[idx].xyz);	idx += 25;'#10#13+
'			 N123.y += dot(N, NPCA.Matrix[idx].xyz);	idx += 25;'#10#13+
'			 N123.z += dot(N, NPCA.Matrix[idx].xyz);	idx += 25;'#10#13+
'			 N456.x += dot(N, NPCA.Matrix[idx].xyz);	idx += 25;'#10#13+
'			 N456.y += dot(N, NPCA.Matrix[idx].xyz);	idx += 25;'#10#13+
'			 N456.z += dot(N, NPCA.Matrix[idx].xyz);	idx -= 124;'#10#13+
'		 }'#10#13+
'	 }'#10#13+

' float minDis;'#10#13+
' float dis;'#10#13+
' vec3 exN123, exN456, diff;'#10#13+

' ivec2 Coherent = tiling(coords);'#10#13+
' exN123 = texelFetch( neigbMapPart1, Coherent, 0 ).rgb;'#10#13+
' exN123 = exN123 * NeighbScale1 + NeighbOffset1;'#10#13+
' exN456 = texelFetch( neigbMapPart2, Coherent, 0 ).rgb;'#10#13+
' exN456 = exN456 * NeighbScale2 + NeighbOffset2;'#10#13+
' diff = N123 - exN123;'#10#13+
' minDis = dot(diff, diff);'#10#13+
' diff = N456 - exN456;'#10#13+
' minDis += dot(diff, diff);'#10#13+
' minDis *= Kappa;'#10#13+
' ivec4 best = ivec4(Coherent, 0, 0);'#10#13+

'	for (int i = -1; i<=1; i++) {'#10#13+
'		for (int j = -1; j<=1; j++) {'#10#13+
'     ivec2 xy = tiling( coords + ivec2(j, i) );'#10#13+
'			ivec2 n = texelFetch( ReadSynth, xy, 0 ).xy;'#10#13+
'			ivec4 candidate = texelFetch( kNearestMap, n, 0 ).xyzw;'#10#13+
'			ivec2 delta = ivec2(j, i) * spacing;'#10#13+
'			candidate.xy -= delta;'#10#13+
'			candidate.zw -= delta;'#10#13+

'			exN123 = texelFetch( neigbMapPart1, tiling(candidate.xy), 0 ).rgb;'#10#13+
'			exN123 = exN123 * NeighbScale1 + NeighbOffset1;'#10#13+
'			exN456 = texelFetch( neigbMapPart2, tiling(candidate.xy), 0 ).rgb;'#10#13+
'			exN456 = exN456 * NeighbScale2 + NeighbOffset2;'#10#13+

'			diff = N123 - exN123;'#10#13+
'			dis = dot(diff, diff); '#10#13+
'			diff = N456 - exN456;'#10#13+
'			dis += dot(diff, diff);'#10#13+

'			if (dis < minDis) {'#10#13+
'				minDis = dis;'#10#13+
'				best.xy = candidate.xy;'#10#13+
'			}'#10#13+

'			exN123 = texelFetch( neigbMapPart1, tiling(candidate.zw), 0 ).rgb;'#10#13+
'			exN123 = exN123 * NeighbScale1 + NeighbOffset1;'#10#13+
'			exN456 = texelFetch( neigbMapPart2, tiling(candidate.zw), 0 ).rgb;'#10#13+
'			exN456 = exN456 * NeighbScale2 + NeighbOffset2;'#10#13+

'			diff = N123 - exN123;'#10#13+
'			dis = dot(diff, diff);'#10#13+
'			diff = N456 - exN456;'#10#13+
'			dis += dot(diff, diff);'#10#13+

'			if (dis < minDis) {'#10#13+
'				minDis = dis;'#10#13+
'				best.xy = candidate.zw;'#10#13+
'			}'#10#13+

'		}'#10#13+
'	}'#10#13+
' imageStore(WriteSynth, coords, best);'#10#13+
'}';

implementation

end.
