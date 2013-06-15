unit uBaseTypes;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses Classes, uVMath;

Type
{$IFNDEF FPC}
  // These new types were added to be able to cast pointers to integers
  // in 64 bit mode, because in FPC "Integer" type is always 32 bit
  // (or 16 bit in Pascal mode), but in Delphi it is platform-specific and
  // can be 16, 32 or 64 bit.
  PtrInt = NativeInt;
  PtrUInt = NativeUInt;
{$ENDIF}

  PByteArray = ^TByteArray;
  TByteArray = array[0..32767] of Byte;

  TIntegerArray = array of Integer;

//  TPtrList = TDataList<pointer>;
//  TObjectList = TDataList<TObject>;

  TValueType = (vtByte, vtWord, vtInt, vtUint, vtFloat, vtDouble);
  TValueComponent = 1..4;

  TShaderType = (stVertex, stTessControl, stTessEval,stGeometry, stFragment, stCompute);

  TBufferType = (btArray, btAtomicCounter, btCopyRead, btCopyWrite, btDrawIndirect,
                 btDispatchIndirect, btElementArray, btPixelPack, btPixelUnpack,
                 btShaderStorage, btTexture, btTransformFeedback, btUniform);

  TFaceType =
    (ftPoints, ftLineStrip, ftLineLoop, ftLines, ftLineStripAdjacency, ftLinesAdjacency,
     ftTriangleStrip, ftTriangleFan, ftTriangles, ftTriangleStripAdjacency,
     ftTrianglesAdjacency, ftPatches, ftQuads);

  TTexTarget = (ttTexture1D, ttTexture2D, ttTexture3D, ttTextureRectangle,
                ttTextureRectangleNV,
                ttCubemap, ttCubemapPX, ttCubemapPY, ttCubemapNX, ttCubemapNY,
                ttCubemapPZ, ttCubemapNZ, tt1DArray, tt2DArray, ttCubeMapArray);
  TTextureWraps = (twClamp, twRepeat, twClampToEdge, twClampToBorder, twMirrorRepeat);
  TTexGens = (tgDisable,tgObjectLinear,tgEyeLinear,tgSphereMap,tgNormalMap,tgReflectionMap);
  TMagFilter = (mgNearest, mgLinear);
  TMinFilter = (mnNearest, mnLinear, mnNearestMipmapNearest, mnNearestMipmapLinear,
                mnLinearMipmapNearest, mnLinearMipmapLinear);

  TBlendingModes = (bmOpaque, bmTransparency, bmAdditive, bmAlphaTest50,
                    bmAlphaTest100, bmModulate, bmCustom);
  TSBlendFactor = (
    sbf_ZERO,sbf_ONE,sbf_SRC_COLOR,sbf_ONE_MINUS_SRC_COLOR,sbf_DST_COLOR,
    sbf_ONE_MINUS_DST_COLOR,sbf_SRC_ALPHA,sbf_ONE_MINUS_SRC_ALPHA,sbf_DST_ALPHA,
    sbf_ONE_MINUS_DST_ALPHA,sbf_ONE_MINUS_CONSTANT_COLOR,
    sbf_CONSTANT_ALPHA,sbf_ONE_MINUS_CONSTANT_ALPHA,sbf_SRC_ALPHA_SATURATE);
  TDBlendFactor = (
    dbf_ZERO,dbf_ONE,dbf_SRC_COLOR,dbf_ONE_MINUS_SRC_COLOR,dbf_DST_COLOR,
    dbf_ONE_MINUS_DST_COLOR,dbf_SRC_ALPHA,dbf_ONE_MINUS_SRC_ALPHA,
    dbf_DST_ALPHA,dbf_ONE_MINUS_DST_ALPHA,
    dbf_ONE_MINUS_CONSTANT_COLOR,dbf_CONSTANT_ALPHA,dbf_ONE_MINUS_CONSTANT_ALPHA);
  TAlphaFunc = (
    af_NEVER,af_LESS,af_EQUAL,af_LEQUAL,af_GREATER,af_NOTEQUAL,af_GEQUAL,af_ALWAYS);

  TMapTarget = (mtAmbient, mtDiffuse, mtSpecular, mtShininess, mtBumpMap,
                mtNormalMap, mtAlpha, mtOpacity, mtReflection);
  TMapTargets = set of TMapTarget;

  TTextureMode = (tmDecal, tmModulate, tmBlend, tmReplace, tmAdd);

  TTextureUpdate = (tuWrapS, tuWrapT, tuWrapR, tuminFilter, tumagFilter, tuTextureGenS,
                     tuTextureGenT, tuTextureGenR, tuGenMipMaps, tuAnisotropyLevel,
                     tuMinLod, tuMaxLod, tuLodBias, tuCompareMode, tuCompareFunc,
                     tuImage);
  TTextureUpdates = set of TTextureUpdate;

  TTextureCompareMode = (cmNone, cmCompareRefToTexture);
  TTextureCompareFunc = (cfLEqual, cfGEqual, cfLess, cfGreater, cfEqual, cfNotEqual,
                          cfAlways, cfNever);

  TTextureDesc = record
     WrapS, WrapT, WrapR: TTextureWraps;
     minFilter: TMinFilter;
     magFilter: TMagFilter;
     TextureGenS: TTexGens;
     TextureGenT: TTexGens;
     TextureGenR: TTexGens;
     MinLod, MaxLod, LodBias: single;
     CompareMode: TTextureCompareMode;
     CompareFunc: TTextureCompareFunc;
     AnisotropyLevel: single;
  end;
  PTextureDesc = ^TTextureDesc;

  TImageLevelDesc = record
    Width: Integer;
    Height: Integer;
    Depth: Integer;
    Offset: LongWord;
    Size: integer;
    LayersOffset: array of LongWord;
  end;

  TImageDesc = record
    InternalFormat: cardinal;
    ColorFormat: cardinal;
    DataType: cardinal;

    ElementSize: integer;
    DataSize: integer;
    ReservedMem: integer;
    Data: pointer;
    Width, Height, Depth, Levels: integer;
    LODS: array[0..15] of TImageLevelDesc;
    Compressed: boolean;
    CubeMap: boolean;
    TextureArray: boolean;

    procedure Save(aStream: TStream);
    procedure Load(aStream: TStream);
    procedure Free;
  end;
  PImageDesc = ^TImageDesc;

  TApiVersion = record
    GAPI: (avGL, avDX, avES, avCanvas);
    VendorName: string; //ATI, NVidia, Intel etc.
    Version: integer; //Version = Major*100+Minor*10: 4.3 -> 430
  end;

  TBinaryData = record
    Data: pointer;
    Size: integer;
    Format: cardinal;
  end;

  TUBOParam = record
    Name: string;
    Offset: cardinal;
  end;

  TUniformBlock = record
    BlockName: ansistring;
    BlockIndex: cardinal;
    BlockSize: cardinal;
    uniforms: array of TUBOParam;
  end;

  TAttribType = (atVertex,atNormal,atTexCoord0,atColor,atTexCoord1,atTexCoord2,
    atTexCoord3,atTexCoord4,atTexCoord5,atTexCoord6,atTexCoord7,atTangent,
    atBinormal, atUserAttrib);
  TAttribTypes = set of TAttribType;

  TAttribSemantic = record
    Name: ansistring;
    Location: integer;
  end;

  TUniformBlocksType = (ubTransforms, ubMaterial, ubLights, ubUnknown);
  TUBOMaterialProperties = (umAmbient, umDiffuse, umSpecular, umEmissive, umShininess);
  TUBOLightProperties = (ulAmbient, ulDiffuse, ulSpecular, ulSceneColor,
    ulSpotDirection, ulSpotCutOff, ulSpotExponent, ulPosition, ulConstAttenuation,
    ulLinearAttenuation, ulQuadraticAttenuation);

  TTriangleEdgeInfo = record
    adjacentTriangle: array[0..2] of LongWord;
    // Bits 0:1 is edge number of adjacent triangle 0
    // Bits 2:3 is edge number of adjacent triangle 1
    // Bits 4:5 is edge number of adjacent triangle 2
    adjacentTriangleEdges: Byte;
    openEdgeMask: Byte;
  end;

  TTriangleEdgeInfoArray = array of TTriangleEdgeInfo;

  TTriangleBoundary = record
    vertexIndex: LongWord;
    triangle: LongWord;
    edge: LongWord;
    prev: LongWord;
    next: array[0..2] of LongWord;
    active: LongWord;
    maxSqArea: Single;
  end;

  TTriangleBoundaryArray = array of TTriangleBoundary;

  //Логика использования шейдеров рендером вершинных объектов
  //slDisableShader - деактивировать активный шейдер и не использовать собственный
  //slUseActiveShader - использовать активный шейдер вместо собственного
  //slUseOwnShader - использовать собственный шейдер и оставить его активным
  //slUseOwnAndStashActiveShader - сохранить активный шейдер, активировать собственный
  //                               и по завершению рендеринга вернуть старый активный
  //slStashActiveAndDisableShader - сохранить активный шейдер, деактивировать шейдер
  //                               и по завершению рендеринга вернуть старый активный
  //slUseActiveAndDisable - использовать активный шейдер, по завершению рендеринга
  //                        деактивировать шейдер
  //slUseOwnAndDisable - использовать собственный шейдер и деактивировать
  //                     его по завершению рендеринга
  TShaderUsageLogic = (slDisableShader, slUseActiveShader, slUseOwnShader,
    slUseOwnAndStashActiveShader, slStashActiveAndDisableShader,
    slUseActiveAndDisable, slUseOwnAndDisable);

  //Приоритет использования шейдера, если назначены оба или один не доступен
  TShaderUsagePriority = (spUseOwnShaderFirst, spUseActiveShaderFirst);

const
  CValueSizes: array[TValueType] of byte = (1, 2, 4, 4, 4, 8);

  CAttribSematics: array[TAttribType] of TAttribSemantic =
  (
    (Name: 'in_Position';  Location: 0),
    (Name: 'in_Normal';    Location: 1),
    (Name: 'in_TexCoord';  Location: 2),
    (Name: 'in_Color';     Location: 3),
    (Name: 'in_TexCoord1'; Location: 4),
    (Name: 'in_TexCoord2'; Location: 5),
    (Name: 'in_TexCoord3'; Location: 6),
    (Name: 'in_TexCoord4'; Location: 7),
    (Name: 'in_TexCoord5'; Location: 8),
    (Name: 'in_TexCoord6'; Location: 9),
    (Name: 'in_TexCoord7'; Location: 10),
    (Name: 'in_Tangent';   Location: 11),
    (Name: 'in_Binormal';  Location: 12),
    (Name: 'in_Custom';    Location: 13)
  );
  CUserAttribLocation = 14;

  CUBOSemantics: array[TUniformBlocksType] of TAttribSemantic =
    (
      (Name: 'Transforms'; Location: 1),
      (Name: 'Material'; Location: 2),
      (Name: 'Lights'; Location: 3),
      (Name: ''; Location: 4)
    );
  CUBOMatPropertySemantics: array [TUBOMaterialProperties] of ansistring =
    ('ambient', 'diffuse', 'specular', 'emissive', 'shininess');
  CUBOLightPropertySemantics: array [TUBOLightProperties] of ansistring =
    ('ambient', 'diffuse', 'specular', 'scene_color', 'spot_direction', 'spot_cutoff',
    'spot_exponent', 'position', 'const_attenuation', 'linear_attenuation', 'quadratic_attenuation');

const
  // Notification Messages
  NM_WorldMatrixChanged = 10001;
  NM_ResourceLoaded  = 10101;
  NM_ResourceChanged = 10102;
  NM_ObjectDestroyed = 10201;

implementation

procedure TImageDesc.Free;begin
  if Assigned(Data) then
  begin
    FreeMem(Data);
    Data := nil;
  end;
end;

procedure TImageDesc.Save(aStream: TStream);
var
  i: integer;
begin
  i := 0; // Version
  aStream.Write(i, SizeOf(integer));
  aStream.Write(InternalFormat, SizeOf(cardinal));
  aStream.Write(ColorFormat, SizeOf(cardinal));
  aStream.Write(DataType, SizeOf(cardinal));
  aStream.Write(ElementSize, SizeOf(integer));
  aStream.Write(DataSize, SizeOf(integer));
  aStream.Write(ReservedMem, SizeOf(integer));
  aStream.Write(Width, SizeOf(integer));
  aStream.Write(Height, SizeOf(integer));
  aStream.Write(Depth, SizeOf(integer));
  aStream.Write(Levels, SizeOf(integer));
  aStream.Write(LODS, SizeOf(LODS));
  aStream.Write(Compressed, SizeOf(boolean));
  aStream.Write(CubeMap, SizeOf(boolean));
  aStream.Write(TextureArray, SizeOf(boolean));
  if Assigned(Data) and (DataSize > 0) then
    aStream.Write(PByte(Data)^, DataSize);
end;

procedure TImageDesc.Load(aStream: TStream);var  i: integer;begin
  Free;
  aStream.Read(i, SizeOf(integer));
  Assert(i = 0); // Check version
  aStream.Read(InternalFormat, SizeOf(cardinal));
  aStream.Read(ColorFormat, SizeOf(cardinal));
  aStream.Read(DataType, SizeOf(cardinal));
  aStream.Read(ElementSize, SizeOf(integer));
  aStream.Read(DataSize, SizeOf(integer));
  aStream.Read(ReservedMem, SizeOf(integer));
  aStream.Read(Width, SizeOf(integer));
  aStream.Read(Height, SizeOf(integer));
  aStream.Read(Depth, SizeOf(integer));
  aStream.Read(Levels, SizeOf(integer));
  aStream.Read(LODS, SizeOf(LODS));
  aStream.Read(Compressed, SizeOf(boolean));
  aStream.Read(CubeMap, SizeOf(boolean));
  aStream.Read(TextureArray, SizeOf(boolean));
  if DataSize > 0 then
  begin
    GetMem(Data, DataSize);
    aStream.Read(PByte(Data)^, DataSize);
  end;
end;

end.