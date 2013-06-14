unit uImageSynthesisShaderGen;

interface

uses
  uRenderResource;

type

  TRandomFunc = (rfRandomTexture, rfRandomMath);

  SynthesisShaderGenerator = class
    class function GenUpsampleJitterShader(
      anOwner: TObject;
      aFirstLevel: Boolean;
      aRandomFunc: TRandomFunc = rfRandomTexture): TShaderProgram;

    class function GenCorrectionShader(
      anOwner: TObject): TShaderProgram;

    class function GenCopyImageShader(
     anOwner: TObject): TShaderProgram;

    class function GenViewShader(
      anOwner: TObject): TShaderProgram;

    class function GenPatchesViewShader(
      anOwner: TObject): TShaderProgram;

    class function GenImageGenShader(
      anOwner: TObject): TShaderProgram;
  end;

implementation

uses
  uBaseTypes;

const
  GLSL_SYNTH_HEADER: ansistring =
    '#version 430 core'#10#13 +
    '#extension GL_ARB_compute_shader : require'#10#13 +
    '#extension GL_ARB_shader_subroutine : require'#10#13 +
    '#define M_PI 3.14159265358979323846'#10#13;

  GLSL_SYNTH_WRAPPINGSUB: ansistring =
    'subroutine ivec2 TWrapping(ivec2 c, ivec2 size);'#10#13 +
    'subroutine(TWrapping) ivec2 wrapRepeat(ivec2 c, ivec2 size) {'#10#13 +
    '  c.xy = c.xy % size;'#10#13 +
    '  if (c.x < 0) c.x += size.x;'#10#13 +
    '  if (c.y < 0) c.y += size.y;'#10#13 +
    '  return c; }'#10#13 +
    'subroutine(TWrapping) ivec2 wrapMirrorRepeat(ivec2 c, ivec2 size) {'#10#13 +
    '  ivec2 r = c.xy % size;'#10#13 +
    '  ivec2 m = c.xy / size;'#10#13 +
    '  if (r.x < 0) {r.x += size.x; --m.x;}'#10#13 +
    '  if (r.y < 0) {r.y += size.y; --m.y;}'#10#13 +
    '  if ((m.x & 1) == 1) r.x = size.x - r.x - 1;'#10#13 +
    '  if ((m.y & 1) == 1) r.y = size.y - r.y - 1;'#10#13 +
    '  return r; }'#10#13+
    'ivec4 wrapRepeat(ivec4 c, ivec2 size) {'#10#13 +
    '  c.xy = c.xy % size;'#10#13 +
    '  if (c.x < 0) c.x += size.x;'#10#13 +
    '  if (c.y < 0) c.y += size.y;'#10#13 +
    '  return c; }'#10#13;

  GLSL_SYNTH_RANDSUB_TEXTURE: ansistring =
    'layout(binding = 1) uniform sampler2D Random;'#10#13 +
    'ivec4 rand(ivec2 c) {'#10#13 +
    '  ivec2 tc = c * randScaleOffset.xy + randScaleOffset.zw;'#10#13 +
    '  tc %= textureSize(Random, 0);'#10#13 +
    '  vec2 rnd = texelFetch(Random, tc, 0).xy;'#10#13 +
    '  rnd *= strength;'#10#13 +
    '  return ivec4(round(rnd), 0, 0);'#10#13 +
    '}'#10#13;

  GLSL_SYNTH_RANDSUB_MATH: ansistring =
    'ivec4 rand(ivec2 c) {'#10#13 +
    '  const vec4 a = vec4(M_PI * M_PI * M_PI * M_PI, exp(4.0),  1.0, 0.0);'#10#13
    +
    '  const vec4 b = vec4(pow(13.0, M_PI / 2.0), sqrt(1997.0),  0.0, 1.0);'#10#13
    +
    '  vec2 xy0    = vec2(c) / M_PI;'#10#13 +
    '  vec2 xym    = fract(xy0.xy / vec2(257.0) ) * vec2(257.0) + vec2(1.0);'#10#13
    +
    '  vec2 xym2   = fract(xym * xym);'#10#13 +
    '  vec4 pxy    = vec4(xym.yx * xym2 , fract(xy0));'#10#13 +
    '  vec2 rnd    = vec2(dot(pxy, a) , dot(pxy, b));'#10#13 +
    '  rnd *= strength;'#10#13 +
    '  return ivec4(round(rnd), 0, 0);'#10#13 +
    '}'#10#13;

  GLSL_SYNTH_UPSAMPLE_JITTER_INTERFACE: ansistring =
    'layout(local_size_x = 16, local_size_y = 16, local_size_z = 1) in;'#10#13 +
    'layout(binding = 0) uniform isampler2D DownLevel;'#10#13 +
    'layout(binding = 0, rg16i) uniform iimage2D UpLevel;'#10#13 +
    'uniform ivec4 spacing[3];'#10#13 +
    'uniform ivec2 quarter;'#10#13 +
    'uniform ivec2 exemplarSize;'#10#13 +
    'uniform ivec4 randScaleOffset;'#10#13 +
    'uniform vec2 strength;'#10#13;

  GLSL_SYNTH_UPSAMPLE_JITTER_MAIN: ansistring =
    'void main()'#10#13 +
    '{'#10#13 +
    '  ivec2 coords = ivec2(gl_WorkGroupID.xy * uvec2(16u) + gl_LocalInvocationID.xy);'#10#13
    +
    '  ivec2 up_coords = coords * ivec2(2);'#10#13 +
    '  coords += quarter;'#10#13 +
    '  ivec4 p = ivec4(texelFetch(DownLevel, coords, 0).xy, 0, 0);'#10#13 +
    '  ivec4 delta;'#10#13 +
    '  delta = rand(up_coords);'#10#13 +
    '  imageStore(UpLevel, up_coords, delta + p);'#10#13 +
    '  up_coords += ivec2(1, 0);'#10#13 +
    '  delta = rand(up_coords);'#10#13 +
    '  imageStore(UpLevel, up_coords, delta + p + spacing[0]);'#10#13
    +
    '  up_coords += ivec2(-1, 1);'#10#13 +
    '  delta = rand(up_coords);'#10#13 +
    '  imageStore(UpLevel, up_coords, delta + p + spacing[1]);'#10#13
    +
    '  up_coords += ivec2(1, 0);'#10#13 +
    '  delta = rand(up_coords);'#10#13 +
    '  imageStore(UpLevel, up_coords, delta + p + spacing[2]);'#10#13
    +
    '}'#10#13;

  GLSL_SYNTH_UPSAMPLE_JITTER_FIRST_MAIN: ansistring =
    'uniform ivec2 baseCoords;'#10#13 +
    'void main()'#10#13 +
    '{'#10#13 +
    '  ivec2 coords = ivec2(gl_WorkGroupID.xy * uvec2(16u) + gl_LocalInvocationID.xy);'#10#13
    +
    '  ivec2 up_coords = coords * ivec2(2);'#10#13 +
    '  ivec4 p = ivec4(baseCoords, 0, 0);'#10#13 +
    '  ivec4 delta;'#10#13 +
    '  delta = rand(up_coords);'#10#13 +
    '  imageStore(UpLevel, up_coords, delta + p);'#10#13 +
    '  up_coords += ivec2(1, 0);'#10#13 +
    '  delta = rand(up_coords);'#10#13 +
    '  imageStore(UpLevel, up_coords, delta + p + spacing[0]);'#10#13
    +
    '  up_coords += ivec2(-1, 1);'#10#13 +
    '  delta = rand(up_coords);'#10#13 +
    '  imageStore(UpLevel, up_coords, delta + p + spacing[1]);'#10#13
    +
    '  up_coords += ivec2(1, 0);'#10#13 +
    '  delta = rand(up_coords);'#10#13 +
    '  imageStore(UpLevel, up_coords, delta + p + spacing[2]);'#10#13
    +
    '}'#10#13;

  GLSL_SYNTH_CORRECTION_INTERFACE: ansistring =
    'layout(local_size_x = 16, local_size_y = 16, local_size_z = 1) in;'#10#13 +
    'uniform ivec2 subPassOffset;'#10#13 +
    'uniform vec3 NeighbScale1;'#10#13 +
    'uniform vec3 NeighbScale2;'#10#13 +
    'uniform vec3 NeighbOffset1;'#10#13 +
    'uniform vec3 NeighbOffset2;'#10#13 +
    'uniform ivec2 spacing;'#10#13 +
    'uniform float Kappa;'#10#13 +
    'subroutine uniform TWrapping synthWrapping;'#10#13 +
    'layout(binding = 0) uniform usampler2D Exemplar;'#10#13 +
    'layout(binding = 1) uniform isampler2D ReadSynth;'#10#13 +
    'layout(binding = 2) uniform isampler2D kNearestMap;'#10#13 +
    'layout(binding = 3) uniform sampler2D neigbMapPart1;'#10#13 +
    'layout(binding = 4) uniform sampler2D neigbMapPart2;'#10#13 +
    'layout(binding = 0, rg16i) uniform iimage2D WriteSynth;'#10#13 +
    'layout(std140, binding = 0) uniform NeihgbPCAMatrix {'#10#13 +
    '  vec4 Matrix[150];'#10#13 +
    '} NPCA;'#10#13;

  GLSL_SYNTH_CORRECTION_MAIN: ansistring =
    'void main()'#10#13 +
    '{'#10#13 +
    '  ivec2 coords = ivec2(gl_WorkGroupID.xy * uvec2(16u) + gl_LocalInvocationID.xy);'#10#13
    +
    '  coords = coords * ivec2(2) + subPassOffset;'#10#13 +
    '  ivec2 exemplarSize = textureSize(Exemplar, 0);'#10#13 +
    '  ivec2 synthSize = textureSize(ReadSynth, 0);'#10#13 +
    '	 vec3 N123 = vec3(0.0);'#10#13 +
    '	 vec3 N456 = vec3(0.0);'#10#13 +
    '	 int idx = 0;'#10#13 +
    '	 for (int ni = -2; ni<=2; ni++) {'#10#13 +
    '	   for (int nj = -2; nj<=2; nj++) {'#10#13 +
    '      ivec2 xy = coords + ivec2(nj, ni);'#10#13 +
    '      xy = synthWrapping(xy, synthSize);'#10#13 +
    '      ivec2 p = texelFetch( ReadSynth, xy, 0 ).xy;'#10#13 +
    '      p = wrapRepeat(p, exemplarSize);'#10#13 +
    '		   vec3 N = texelFetch( Exemplar, p, 0 ).rgb;'#10#13 +
    '			 N123.x += dot(N, NPCA.Matrix[idx].xyz);	idx += 25;'#10#13 +
    '			 N123.y += dot(N, NPCA.Matrix[idx].xyz);	idx += 25;'#10#13 +
    '			 N123.z += dot(N, NPCA.Matrix[idx].xyz);	idx += 25;'#10#13 +
    '			 N456.x += dot(N, NPCA.Matrix[idx].xyz);	idx += 25;'#10#13 +
    '			 N456.y += dot(N, NPCA.Matrix[idx].xyz);	idx += 25;'#10#13 +
    '			 N456.z += dot(N, NPCA.Matrix[idx].xyz);	idx -= 124;'#10#13 +
    '		 }'#10#13 +
    '	 }'#10#13 +

    ' float minDis;'#10#13 +
    ' float dis;'#10#13 +
    ' vec3 exN123, exN456, diff;'#10#13 +

    ' ivec2 Coherent = texelFetch( ReadSynth, coords, 0 ).xy;'#10#13 +
    ' ivec4 best = ivec4(Coherent, 0, 0);'#10#13 +
    ' Coherent = wrapRepeat(Coherent, exemplarSize);'#10#13 +
    ' exN123 = texelFetch( neigbMapPart1, Coherent, 0 ).rgb;'#10#13 +
    ' exN123 = exN123 * NeighbScale1 + NeighbOffset1;'#10#13 +
    ' exN456 = texelFetch( neigbMapPart2, Coherent, 0 ).rgb;'#10#13 +
    ' exN456 = exN456 * NeighbScale2 + NeighbOffset2;'#10#13 +
    ' diff = N123 - exN123;'#10#13 +
    ' minDis = dot(diff, diff);'#10#13 +
    ' diff = N456 - exN456;'#10#13 +
    ' minDis += dot(diff, diff);'#10#13 +
    ' minDis *= Kappa;'#10#13 +

    '	for (int i = -1; i<=1; i++) {'#10#13 +
    '		for (int j = -1; j<=1; j++) {'#10#13 +
    '     ivec2 xy = coords + ivec2(j, i);'#10#13 +
    '     xy = synthWrapping(xy, synthSize);'#10#13 +
    '			ivec2 n = texelFetch( ReadSynth, xy, 0 ).xy;'#10#13 +
    '     n = wrapRepeat(n, exemplarSize);'#10#13 +
    '			ivec4 candidate = texelFetch( kNearestMap, n, 0 ).xyzw;'#10#13 +
    '			ivec2 delta = ivec2(j, i) * spacing;'#10#13 +
    '			candidate.xy -= delta;'#10#13 +
    '			candidate.zw -= delta;'#10#13 +

    '     xy = wrapRepeat(candidate.xy, exemplarSize);'#10#13 +
    '			exN123 = texelFetch( neigbMapPart1, xy, 0 ).rgb;'#10#13 +
    '			exN123 = exN123 * NeighbScale1 + NeighbOffset1;'#10#13 +
    '			exN456 = texelFetch( neigbMapPart2, xy, 0 ).rgb;'#10#13 +
    '			exN456 = exN456 * NeighbScale2 + NeighbOffset2;'#10#13 +

    '			diff = N123 - exN123;'#10#13 +
    '			dis = dot(diff, diff); '#10#13 +
    '			diff = N456 - exN456;'#10#13 +
    '			dis += dot(diff, diff);'#10#13 +

    '			if (dis < minDis) {'#10#13 +
    '				minDis = dis;'#10#13 +
    '				best.xy = candidate.xy;'#10#13 +
    '			}'#10#13 +

    '     xy = wrapRepeat(candidate.zw, exemplarSize);'#10#13 +
    '			exN123 = texelFetch( neigbMapPart1, xy, 0 ).rgb;'#10#13 +
    '			exN123 = exN123 * NeighbScale1 + NeighbOffset1;'#10#13 +
    '			exN456 = texelFetch( neigbMapPart2, xy, 0 ).rgb;'#10#13 +
    '			exN456 = exN456 * NeighbScale2 + NeighbOffset2;'#10#13 +

    '			diff = N123 - exN123;'#10#13 +
    '			dis = dot(diff, diff);'#10#13 +
    '			diff = N456 - exN456;'#10#13 +
    '			dis += dot(diff, diff);'#10#13 +

    '			if (dis < minDis) {'#10#13 +
    '				minDis = dis;'#10#13 +
    '				best.xy = candidate.zw;'#10#13 +
    '			}'#10#13 +

    '		}'#10#13 +
    '	}'#10#13 +
    ' imageStore(WriteSynth, coords, best);'#10#13 +
    '}'#10#13;

  GLSL_SYNTH_COPY_IMAGE: ansistring =
    'layout(local_size_x = 16, local_size_y = 16, local_size_z = 1) in;'#10#13 +
    'layout(binding = 7) uniform isampler2D ReadSynth;'#10#13 +
    'layout(binding = 1, rg16i) uniform iimage2D WriteSynth;'#10#13 +
    'void main()'#10#13 +
    '{'#10#13 +
    '  ivec2 coords = ivec2(gl_WorkGroupID.xy * uvec2(16u) + gl_LocalInvocationID.xy);'#10#13
    +
    '  ivec4 pass = texelFetch(ReadSynth, coords, 0);'#10#13 +
    '  imageStore(WriteSynth, coords, pass);'#10#13 +
    '}'#10#13;

  GLSL_SYNTH_VIEW_IMAGE_VTX: ansistring =
    '#version 420'#10#13 +
    'layout(location = 0)'#10#13 +
    'in vec2 in_Position;'#10#13 +
    'layout(location = 2)'#10#13 +
    'in vec2 in_TexCoord;'#10#13 +
    'uniform mat4 Projection;'#10#13 +
    'uniform vec2 Ratio;'#10#13 +
    'out vec2 TexCoord;'#10#13 +
    'void main()'#10#13 +
    '{'#10#13 +
    '	gl_Position = vec4(Ratio * in_Position, 0.0, 1.0);'#10#13 +
    '	TexCoord = in_TexCoord;'#10#13 +
    '}'#10#13;

  GLSL_SYNTH_VIEW_IMAGE_FRAG: ansistring =
    '#version 430'#10#13 +
    'in vec2 TexCoord;'#10#13 +
    'layout(binding = 0) uniform isampler2D Exemplar;'#10#13 +
    'layout(binding = 1) uniform isampler2D Patches;'#10#13 +
    'layout(location = 0) out vec4 FragColor;'#10#13 +
    'void main()'#10#13 +
    '{'#10#13 +
    '  vec2 s = textureSize(Exemplar, 0);'#10#13 +
    '  vec2 p = vec2(texture(Patches, TexCoord).xy) / s.xy;'#10#13 +
    '  p += fract(TexCoord * s.xy) / s.xy;'#10#13 +
    '  FragColor = texture(Exemplar, p) / 255.0;'#10#13 +
    '}'#10#13;

  GLSL_SYNTH_VIEW_PATCHES_FRAG: ansistring =
    '#version 430'#10#13 +
    'in vec2 TexCoord;'#10#13 +
    'layout(binding = 0) uniform isampler2D Patches;'#10#13 +
    'layout(location = 0) out vec4 FragColor;'#10#13 +
    'layout(location = 0) uniform ivec2 exemplarSize;'#10#13 +
    'void main()'#10#13 +
    '{'#10#13 +
    '  ivec4 p = ivec4(0);'#10#13 +
    '  p.xy = texture(Patches, TexCoord).xy % exemplarSize;'#10#13 +
    '  if (p.x < 0) p.x += exemplarSize.x;'#10#13 +
    '  if (p.y < 0) p.y += exemplarSize.y;'#10#13 +
    '  FragColor = vec4(p) / vec4(exemplarSize);'#10#13 +
    '}'#10#13;

  GLSL_SYNTH_GEN_IMAGE: ansistring =
    'layout(local_size_x = 16, local_size_y = 16, local_size_z = 1) in;'#10#13 +
    'layout(binding = 0) uniform isampler2D Exemplar;'#10#13 +
    'layout(binding = 1) uniform isampler2D Pathces;'#10#13 +
    'layout(binding = 0, rgba) uniform image2D Destination;'#10#13 +
    'uniform ivec2 offset;'#10#13 +
    'void main()'#10#13 +
    '{'#10#13 +
    '  ivec2 coords = ivec2(gl_WorkGroupID.xy * uvec2(16u) + gl_LocalInvocationID.xy);'#10#13
    +
    '  coords += offset;'#10#13 +
    '  ivec2 exemplarSize = textureSize(Exemplar, 0);'#10#13 +
    '  ivec2 p = texelFetch(Pathces, coords, 0).xy;'#10#13 +
    '  p = wrapRepeat(p, exemplarSize);'#10#13 +
    '  vec4 color = texelFetch(Exemplar, p, 0) / vec4(255.0);'#10#13 +
    '  imageStore(Destination, coords, pass);'#10#13 +
    '}'#10#13;

  { SynthesisShaderGenerator }

class function SynthesisShaderGenerator.GenCopyImageShader(
  anOwner: TObject): TShaderProgram;
begin
  Result := TShaderProgram.CreateOwned(anOwner);
  Result.ShaderText[stCompute] :=
    GLSL_SYNTH_HEADER +
    GLSL_SYNTH_COPY_IMAGE;
end;

class function SynthesisShaderGenerator.GenCorrectionShader(anOwner: TObject)
  : TShaderProgram;
begin
  Result := TShaderProgram.CreateOwned(anOwner);
  Result.ShaderText[stCompute] :=
    GLSL_SYNTH_HEADER +
    GLSL_SYNTH_WRAPPINGSUB +
    GLSL_SYNTH_CORRECTION_INTERFACE +
    GLSL_SYNTH_CORRECTION_MAIN;
end;

class function SynthesisShaderGenerator.GenImageGenShader(
  anOwner: TObject): TShaderProgram;
begin
  Result := TShaderProgram.CreateOwned(anOwner);
  Result.ShaderText[stCompute] :=
    GLSL_SYNTH_HEADER +
    GLSL_SYNTH_WRAPPINGSUB +
    GLSL_SYNTH_GEN_IMAGE;
end;

class function SynthesisShaderGenerator.GenPatchesViewShader(
  anOwner: TObject): TShaderProgram;
begin
  Result := TShaderProgram.CreateOwned(anOwner);
  Result.ShaderText[stVertex] := GLSL_SYNTH_VIEW_IMAGE_VTX;
  Result.ShaderText[stFragment] := GLSL_SYNTH_VIEW_PATCHES_FRAG;
end;

class function SynthesisShaderGenerator.GenUpsampleJitterShader(
  anOwner: TObject; aFirstLevel: Boolean; aRandomFunc: TRandomFunc)
  : TShaderProgram;
var
  rand, main: ansistring;
begin
  case aRandomFunc of
    rfRandomTexture: rand := GLSL_SYNTH_RANDSUB_TEXTURE;
    rfRandomMath: rand := GLSL_SYNTH_RANDSUB_MATH;
  end;

  if aFirstLevel then
    main := GLSL_SYNTH_UPSAMPLE_JITTER_FIRST_MAIN
  else
    main := GLSL_SYNTH_UPSAMPLE_JITTER_MAIN;

  Result := TShaderProgram.CreateOwned(anOwner);
  Result.ShaderText[stCompute] :=
    GLSL_SYNTH_HEADER +
    GLSL_SYNTH_UPSAMPLE_JITTER_INTERFACE +
    GLSL_SYNTH_WRAPPINGSUB +
    rand +
    main;
end;

class function SynthesisShaderGenerator.GenViewShader(anOwner: TObject)
  : TShaderProgram;
begin
  Result := TShaderProgram.CreateOwned(anOwner);
  Result.ShaderText[stVertex] := GLSL_SYNTH_VIEW_IMAGE_VTX;
  Result.ShaderText[stFragment] := GLSL_SYNTH_VIEW_IMAGE_FRAG;
end;

end.
