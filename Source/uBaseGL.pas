unit uBaseGL;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses Classes, uPersistentClasses, uVMath, uLists, uMiscUtils, {ImageLoader,}
  uBaseTypes, uImageFormats, uBaseClasses, uRenderResource, dglOpenGL;

Type

  TRenderBuffer = (rbDepth, rbStencil);
  TRenderBuffers = set of TRenderBuffer;
  TBufferMode = (bmNone, bmBuffer, bmTexture);
  TDepthPrecision = (dpDefault, dp16, dp24, dp32);
  TStencilPrecision = (spDefault, sp1bit, sp4bits, sp8bits, sp16bits);
  TMultisampleFormat = (MSAA2, MSAA4, MSAA8);
  TMRTTarget = (tgTexture, tgDepth, tgDepthStencil, tgMRT0, tgMRT1,
    tgMRT2, tgMRT3);

  // Base GL Resource
  TGLBaseResource = class(TBaseRenderResource)
    Owner: TObject;
  end;

  TGLTextureObject = class;

  TFBORenderTarget = record
    Texture: TGLTextureObject;
    BuffId: GLUInt;
    Mode: TBufferMode;
    Precision: GLEnum;
  end;

  TAttachments = record
    Textures: TList;
    DepthBuffer: TFBORenderTarget;
    StencilBuffer: TFBORenderTarget;
    DepthStencilBuffer: TFBORenderTarget;
  end;

  TFBOTarget = record
    Texture: TGLTextureObject;
    TargetTo: TMRTTarget;
  end;

  TGLTextureFormatDescriptor = record
    InternalFormat: cardinal;
    BaseFormat: cardinal;
    PixelFormat: cardinal;
    Compressed: boolean;
  end;

  TGLBufferObject = class(TGLBaseResource)
  private
    FBuffer: TBufferObject;
    FBuffId: cardinal;
    FLastTarget: TBufferType;
    FLocked: boolean;
    FMappedPointer: pointer;
    FBuffType: TBufferType;
    FSize: integer;
    FData: pointer;
  protected
    procedure setBufferType(const Value: TBufferType);
  public
    Name: string;
    constructor Create(BuffType: TBufferType = btArray); reintroduce;
    constructor CreateFrom(const aBuffer: TBufferObject); overload;
    destructor Destroy; override;
    procedure Notify(Sender: TObject; Msg: cardinal;
      Params: pointer = nil); override;

    procedure Allocate(aSize: integer; aData: pointer = nil;
      aUsage: cardinal = GL_STATIC_DRAW);

    procedure Upload(NewData: pointer; aSize, aOffset: integer);
    procedure Download(DestPtr: pointer; aSize, aOffset: integer; aMapBuffer: boolean = true);
    procedure Bind; overload;
    procedure Bind(AsTarget: TBufferType); overload;
    procedure BindBase(Index: cardinal); overload;
    procedure BindBase(AsTarget: TBufferType; Index: cardinal); overload;
    procedure BindAllRange(Index: cardinal);
    procedure BindRange(AsTarget: TBufferType; Index: cardinal;
      Offset, Size: integer);

    procedure UnBindBuffer;

    function Map(AccessType: cardinal = GL_READ_WRITE): pointer;
    function MapRange(AccessType, Offset, Length: cardinal): pointer;
    procedure UnMap;

    property Id: cardinal read FBuffId;
    property BufferType: TBufferType read FBuffType write setBufferType;
    property Size: integer read FSize;
    property Data: pointer read FData;

  end;

  TGLAtomicCounter = class(TGLBufferObject)
  private
    function getValue: cardinal;
    procedure setValue(const Value: cardinal);
  public
    constructor Create;
    property Value: cardinal read getValue write setValue;
  end;

  TGLAttribObject = class(TGLBaseResource)
  private
    FLocation: integer;
    FShaderId: cardinal;
    FFixedLocation: boolean;
    FAttribObject: TAttribObject;
    FGLBuffer: TGLBufferObject;
    FElementSize: integer;
    FNormalized: boolean;

    FSettedParam: set of (apAttrName, apAttrSize, apAttrType, apAttrStride);
    FAttribName: ansistring;
    FSize: integer;
    FType: TValueType;
    FStride: integer;
    FSemantic: TAttribType;
    function getIndexPrt(Index: integer): pointer;

  public
    constructor Create; override;
    destructor Destroy; override;
    constructor CreateFrom(const aAttribObject: TAttribObject); virtual;
    constructor CreateAndSetup(AttrName: ansistring; aSize: integer;
      AType: TValueType = vtFloat; AStride: integer = 0;
      aBuffType: TBufferType = btArray); virtual;

    procedure Notify(Sender: TObject; Msg: Cardinal;
      Params: pointer = nil); override;
    procedure AssignBuffer(aBuffer: TGLBufferObject); overload;
    procedure AssignBuffer(aBuffer: TBufferObject); overload;
    procedure BindAttrib(ShaderId: cardinal; DataPtr: pointer = nil);
    procedure Bind(DataPtr: pointer = nil);
    procedure Unbind;
    procedure SetAttribLocation(Location: cardinal);
    procedure SetAttribSemantic(aSemantic: TAttribType);

    property Normalized: boolean read FNormalized write FNormalized;
    property Buffer: TGLBufferObject read FGLBuffer;
    property ElementSize: integer read FElementSize write FElementSize;
    property AttrSize: integer read FSize;
    property AttrType: TValueType read FType;
    property AttrName: ansistring read FAttribName;
    property AttrStride: integer read FStride;
    property PtrByIndex[Index: integer]: pointer read getIndexPrt; default;
  end;

  TGLAttribBuffer = class(TGLAttribObject)
  public
    constructor CreateAndSetup(AttrName: ansistring;
      aSize: integer; AType: TValueType = vtFloat;
      AStride: integer = 0; aBuffType: TBufferType = btArray); override;
  end;

  { TODO : Написать пресеты для стандартных типов атрибутов
    с встроенными списками координат }
  TGLVertexAttribBuffer = class(TGLAttribBuffer);
  TGLNormalAttribBuffer = class(TGLAttribBuffer);
  TGLColor3fAttribBuffer = class(TGLAttribBuffer);
  TGLColor4fAttribBuffer = class(TGLAttribBuffer);
  TGLColor3bAttribBuffer = class(TGLAttribBuffer);
  TGLColor4bAttribBuffer = class(TGLAttribBuffer);
  TGLTexCoord2fAttribBuffer = class(TGLAttribBuffer);
  TGLTexCoord3fAttribBuffer = class(TGLAttribBuffer);
  TGLTexCoord4fAttribBuffer = class(TGLAttribBuffer);

  TUniformList = class
  private
    FItems: array of record
     Key: integer;
     KeyName: ansistring;
     Value: integer;
    end;

    FCount: integer;
  public
    constructor Create;
    procedure AddKey(const Key: ansistring; Value: integer);
    function getValue(const Key: ansistring): integer;
  end;

  TGLUniformBlock = class
  private
    FBlockType: TUniformBlocksType;
    FBlockName: ansistring;
    FBlockIndex: integer;
    FBlockSize: integer;
    FUniformsCount: integer;
    FubNames: array of ansistring;
    FubIndices: array of integer;
    FubOffsets: array of integer;

    FMaxUBOSize: integer;

    function getIndex(Index: integer): integer;
    function getName(Index: integer): ansistring;
    function getOffset(Index: integer): integer;
    procedure QueryUniformInfo(aProgram: cardinal; aBlockIndex: integer);
    function getMaxBC: integer;
  public
    property BlockName: ansistring read FBlockName;
    property BlockSize: integer read FBlockSize;
    property BlockIndex: integer read FBlockIndex;
    property UniformsCount: integer read FUniformsCount;
    property BlockType: TUniformBlocksType read FBlockType;

    property Offsets[Index: integer]: integer read getOffset; default;
    property Names[Index: integer]: ansistring read getName;
    property Indices[Index: integer]: integer read getIndex;

    property MaxBlocksCount: integer read getMaxBC;
    property MaxUBOSize: integer read FMaxUBOSize;

    function OffsetByName(aName: ansistring): integer;

    constructor Create(const aProgram: cardinal = 0; aIndex: integer = -1);
  end;

  TGLBufferObjectsPool = class
  private
    FBuffer: TGLBufferObject;
    FFreeRooms: TIntegerList;
    FBuffSize: integer;
    FUsedCount: integer;
    FUBOSize: integer;
    FStackTop: integer;
  public
    constructor Create(aObjectSize: integer; aObjectsCount: integer = 10000);
    destructor Destroy; override;

    property Buffer: TGLBufferObject read FBuffer;

    function OffsetByIndex(const Index: integer): integer;
    // Reserve mem in pool, return object index in pool
    function GetFreeSlotIndex: integer;
    procedure FreeSlot(const Index: integer; aCheck: boolean = false);
    function isExists(const aData: pointer): integer;
    procedure WriteToPool(const Index: integer; Data: pointer);
    function BindUBO(const Index: integer; const aUBO: TGLUniformBlock)
      : integer;
    procedure UnBindUBO(const Index: integer; const aUBO: TGLUniformBlock);

  end;

  TUBOList = class
  private
    FItems: TList;
    function getCount: integer;
    function getUBO(Index: integer): TGLUniformBlock;
  public
    constructor Create;
    destructor Destroy; override;

    function AddUBO(const aBlock: TGLUniformBlock): integer;
    function GetUBOByName(const aName: ansistring): TGLUniformBlock;

    property Count: integer read getCount;
    property UBOs[Index: integer]: TGLUniformBlock read getUBO;
  end;

  TGLSLShaderProgram = class(TGLBaseResource)
  private
    FShaderId: cardinal;
    FLog: string;
    FDetachList: array of cardinal;
    FLinked: boolean;
    FError: boolean;
    FUniforms: TUniformList;
    FUBOList: TUBOList;
    FActiveUniforms: integer;
    FActiveAttribs: integer;
    FActiveUniformsBlocks: integer;
    function getShaderId: cardinal;
    function GetUniformLocation(ShaderId: cardinal;
      const Name: ansistring): integer;
    procedure QueryProgramInfo;
  public
    property Id: cardinal read getShaderId;
    property Log: string read FLog;
    property Error: boolean read FError;
    property ActiveUniformsCount: integer read FActiveUniforms;
    property ActiveAttribsCount: integer read FActiveAttribs;
    property UniformBlocksCount: integer read FActiveUniformsBlocks;
    property UniformBlocks: TUBOList read FUBOList;

    constructor Create; override;
    constructor CreateFrom(const aShaderProgram: TShaderProgram); overload;
    destructor Destroy; override;

    procedure AttachShader(ShaderType: TShaderType; Source: ansistring);
    procedure AttachShaderFromFile(ShaderType: TShaderType; FileName: string);
    procedure SetFragDataLocation(Index: cardinal; Name: ansistring);
    procedure SetAttribLocation(Index: cardinal; Name: ansistring);
    function LinkShader: cardinal;
    function ProgramBinary(Binary: pointer; Size: integer; Format: GLEnum)
      : cardinal;
    procedure GetProgramBinary(var Binary: pointer; var Size: integer;
      var Format: GLEnum);

    procedure Apply;
    procedure UnApply;

    procedure SetUniform(const Name: ansistring; const Value: array of TVector);
      overload;
    procedure SetUniform(const Name: ansistring; const Value: single;
      Count: GLsizei = 1); overload;
    procedure SetUniform(const Name: ansistring; const Value: vec2;
      Count: GLsizei = 1); overload;
    procedure SetUniform(const Name: ansistring; const Value: vec3;
      Count: GLsizei = 1); overload;
    procedure SetUniform(const Name: ansistring; const Value: vec4;
      Count: GLsizei = 1); overload;
    procedure SetUniform(const Name: ansistring; const Value: integer;
      Count: GLsizei = 1); overload;
    procedure SetUniform(const Name: ansistring; const Value: mat2;
      Count: GLsizei = 1; transpose: boolean = false); overload;
    procedure SetUniform(const Name: ansistring; const Value: mat3;
      Count: GLsizei = 1; transpose: boolean = false); overload;
    procedure SetUniform(const Name: ansistring; const Value: mat4;
      Count: GLsizei = 1; transpose: boolean = false); overload;
  end;

  TGLVertexObject = class(TGLBaseResource)
  private
    FVAO: cardinal;
    FIndiceId: cardinal;
    FVertexObject: TVertexObject;
    FShader: TGLSLShaderProgram;
    FAttribs: TList; // TGLAttribObject
    FSubMeshes: TList;
    FStructureChanged: boolean;
    FIndices: TIntegerArray;
    FIndicePtr: pointer;
    FIndiceCount: integer;
    FIndiceChanged: boolean;
    FFaceType: TFaceType;
    FElementsCount: integer;

    function getAttrib(Index: integer): TGLAttribObject;
    function getAttrCount: integer;
    function getIndice(Index: integer): integer;
    function getECount: integer;
    procedure SetShader(const Value: TGLSLShaderProgram);
  public
    constructor Create; override;
    constructor CreateFrom(const aVertexObject: TVertexObject); overload;
    destructor Destroy; override;
    procedure Notify(Sender: TObject; Msg: Cardinal;
      Params: pointer = nil); override;

    procedure Bind;
    procedure UnBind;
    procedure Build(ShaderId: cardinal;
      const aVertexObject: TVertexObject = nil); overload;
    procedure Build(const aVertexObject: TVertexObject = nil); overload;

    procedure RenderVO(aShader: integer = -1);

    function GetAttribBySemantic(aSemantic: TAttribType): TGLAttribObject;

    property Attribs[index: integer]: TGLAttribObject read getAttrib; default;
    property Indices[index: integer]: integer read getIndice;
    property IndiceCount: integer read FIndiceCount;
    property AttribsCount: integer read getAttrCount;
    property Shader: TGLSLShaderProgram read FShader write SetShader;
    property FaceType: TFaceType read FFaceType;
    property StructureChanged: boolean read FStructureChanged;
    property ElementsCount: integer read getECount;
    property VAOid: cardinal read FVAO;
    property IndiceId: cardinal read FIndiceId;
  end;

  TGLTextureSampler = class(TGLBaseResource)
  private
    FTextureSampler: TTextureSampler;
    FSamplerId: cardinal;
    function getSamplerHash: integer;
  public
    constructor Create; override;
    constructor CreateFrom(aSampler: TTextureSampler);
    destructor Destroy; override;

    procedure Bind(aUnit: cardinal); overload;
    procedure UnBind(aUnit: cardinal);
    procedure SetSamplerParams(aTarget: TTexTarget); overload;

    property Hash: integer read getSamplerHash;
  end;

  TGLTextureObject = class(TGLBaseResource)
  private
    FTexId: cardinal;
    FpboId: cardinal;
    FImageHolder: TImageHolder;
    FFormatDescr: TGLTextureFormatDescriptor;
    FTexDesc: PTextureDesc;
    FTarget: TTexTarget;
    FTextureObject: TTexture;
    FTextureSampler: TTextureSampler;
    FGenerateMipMaps: boolean;
    function getInternalFormat: cardinal;
    function getColorFormat: cardinal;
    function getDataType: cardinal;

  public
    constructor Create; override;
    constructor CreateFrom(const aTarget: TTexTarget; const aImageHolder: TImageHolder;
      const aTexDesc: PTextureDesc = nil); overload;
    constructor CreateFrom(const aTexture: TTexture); overload;

    destructor Destroy; override;

    procedure UploadTexture(Data: pointer; Size: cardinal);

    property TextureSampler: TTextureSampler read FTextureSampler write FTextureSampler;

    property Id: cardinal read FTexId;
    property Target: TTexTarget read FTarget;

    property InternalFormat: cardinal read getInternalFormat;
    property ColorFormat: cardinal read getColorFormat;
    property DataType: cardinal read getDataType;
    property GenerateMipMaps: boolean read FGenerateMipMaps write FGenerateMipMaps;
  end;

  TGLFrameBufferObject = class(TGLBaseResource)
  private
    FBOId: GLUInt;
    FMSFBOId: GLUInt;
    FAttachments: TAttachments;
    FRenderBuffers: TRenderBuffers;
    FReadBackBuffers: TList;
    FWidth, FHeight: integer;
    FViewport: array [0 .. 3] of integer;
    FInit: boolean;
    FActive: boolean;
    FDeactivate: boolean;
    FMultisample: TMultisampleFormat;

    procedure AttachTextureTarget(tex: TGLTextureObject; attachement: GLEnum);
    function OGLDBPrecision(Precision: TDepthPrecision): GLEnum;
    function OGLSBPrecision(Precision: TStencilPrecision): GLEnum;
    function GetTexture(Index: integer): TGLTextureObject;
    procedure SetTexture(Index: integer; const Value: TGLTextureObject);
    function GetAttachmentsCount: integer;
    function CheckCompleteness: boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure InitFBO(Width, Height: integer);
    procedure ResetFBO(ResetConfig: boolean = true);

    procedure ConfigFBO(RenderBuffers: TRenderBuffers);
    procedure ConfigDepthBuffer(Mode: TBufferMode;
      Precision: TDepthPrecision = dpDefault);
    procedure ConfigStencilBuffer(Mode: TBufferMode;
      Precision: TStencilPrecision = spDefault);
    procedure ConfigDepthStencilBuffer(Mode: TBufferMode);
    procedure AttachTexture(tex: TGLTextureObject;
      aTarget: TMRTTarget = tgTexture);
    procedure AttachDepthTexture(tex: TGLTextureObject);
    procedure AttachStencilTexture(tex: TGLTextureObject);
    procedure AttachDepthStencilTexture(tex: TGLTextureObject);
    procedure DetachDepthTexture;
    procedure DetachStencilTexture;
    procedure DetachDepthStencilTexture;

    procedure DetachTexture(Index: integer);
    procedure DetachAllTextures;

    procedure Apply(ClearBuffers: boolean = true);
    procedure UnApply;
    procedure SetReadBackBuffer(const ColorBufers: array of GLUInt);

    property Textures[index: integer]: TGLTextureObject read GetTexture
      write SetTexture;
    property AttachmentsCount: integer read GetAttachmentsCount;
    property Multisample: TMultisampleFormat read FMultisample
      write FMultisample;

    property Active: boolean read FActive write FActive;
    property DeactivateAfter: boolean read FDeactivate write FDeactivate;
    property Handle: cardinal read FBOId;
  end;

  TGLTextureFormatSelector = class (TAbstractPixelFormatSelector<TGLTextureFormatDescriptor>)
    //Get GL texture format from ImageFormatBits
    class function GetTextureFormat(aFormat: cardinal): TGLTextureFormatDescriptor;
    //virtual functions
    class function CreateInt8(aFormat: TBaseImageFormat): TGLTextureFormatDescriptor; override;
    class function CreateInt16(aFormat: TBaseImageFormat): TGLTextureFormatDescriptor; override;
    class function CreateInt32(aFormat: TBaseImageFormat): TGLTextureFormatDescriptor; override;
    class function CreateUInt8(aFormat: TBaseImageFormat): TGLTextureFormatDescriptor; override;
    class function CreateUInt16(aFormat: TBaseImageFormat): TGLTextureFormatDescriptor; override;
    class function CreateUInt32(aFormat: TBaseImageFormat): TGLTextureFormatDescriptor; override;
    class function CreateFloat16(aFormat: TBaseImageFormat): TGLTextureFormatDescriptor; override;
    class function CreateFloat32(aFormat: TBaseImageFormat): TGLTextureFormatDescriptor; override;

    class function CreateCompressed(aFormat: TS3TCCompressedFormats): TGLTextureFormatDescriptor; override;
    class function CreateDepthStencil(aDepthBit: byte; aStencil: boolean = false): TGLTextureFormatDescriptor; override;
    class function CreateSpecial(aFormat: TImageSpecialFormat): TGLTextureFormatDescriptor; override;
  end;

procedure CheckOpenGLError;
function ParseDebugMessage(aSource,aType,aId,aSeverity: cardinal;
  const aMess: ansistring): ansistring;
function GetWorkgroupCount: vec3i;
function GetWorkgroupSize: vec3i;

implementation

var
  vActiveShader: TGLSLShaderProgram = nil;

const
  CFaceTypeConst: array [TFaceType] of cardinal = (GL_POINTS, GL_LINE_STRIP,
    GL_LINE_LOOP, GL_LINES, GL_LINE_STRIP_ADJACENCY, GL_LINES_ADJACENCY,
    GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN, GL_TRIANGLES,
    GL_TRIANGLE_STRIP_ADJACENCY, GL_TRIANGLES_ADJACENCY, GL_PATCHES, GL_QUADS);

  CBufferTypes: array [TBufferType] of cardinal = (GL_ARRAY_BUFFER,
    GL_ATOMIC_COUNTER_BUFFER, GL_COPY_READ_BUFFER, GL_COPY_WRITE_BUFFER,
    GL_DRAW_INDIRECT_BUFFER, GL_DISPATCH_INDIRECT_BUFFER,
    GL_ELEMENT_ARRAY_BUFFER, GL_PIXEL_PACK_BUFFER, GL_PIXEL_UNPACK_BUFFER,
    GL_SHADER_STORAGE_BUFFER, GL_TEXTURE_BUFFER, GL_TRANSFORM_FEEDBACK_BUFFER,
    GL_UNIFORM_BUFFER);

  CShaderTypes: array [TShaderType] of cardinal = (GL_VERTEX_SHADER,
    GL_TESS_CONTROL_SHADER, GL_TESS_EVALUATION_SHADER, GL_GEOMETRY_SHADER,
    GL_FRAGMENT_SHADER, GL_COMPUTE_SHADER);

  CShaderNames: array [TShaderType] of string = ('VERTEX SHADER',
    'TESS CONTROL SHADER', 'TESS EVALUATION SHADER', 'GEOMETRY SHADER',
    'FRAGMENT SHADER', 'COMPUTE SHADER');

  CTexTargets: array [TTexTarget] of GLEnum = (GL_TEXTURE_1D, GL_TEXTURE_2D,
    GL_TEXTURE_3D, GL_TEXTURE_RECTANGLE, GL_TEXTURE_RECTANGLE_NV,
    GL_TEXTURE_CUBE_MAP, GL_TEXTURE_CUBE_MAP_POSITIVE_X,
    GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY,
    GL_TEXTURE_CUBE_MAP_ARRAY);

  CWpars: array [TTextureWraps] of GLEnum = (GL_CLAMP, GL_REPEAT,
    GL_CLAMP_TO_EDGE, GL_CLAMP_TO_BORDER, GL_MIRRORED_REPEAT);

  CMinFilters: array [TMinFilter] of GLEnum = (GL_NEAREST, GL_LINEAR,
    GL_NEAREST_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR,
    GL_LINEAR_MIPMAP_NEAREST, GL_LINEAR_MIPMAP_LINEAR);

  CMagFilters: array [TMagFilter] of GLEnum = (GL_NEAREST, GL_LINEAR);

  CValueTypes: array [TValueType] of cardinal = (GL_UNSIGNED_BYTE,
    GL_UNSIGNED_SHORT, GL_INT, GL_UNSIGNED_INT, GL_FLOAT, GL_DOUBLE);

  CTextureGen: array [TTexGens] of cardinal = (0, GL_OBJECT_LINEAR, GL_EYE_LINEAR,
    GL_SPHERE_MAP, GL_NORMAL_MAP, GL_REFLECTION_MAP);

  CCompareMode: array[TTextureCompareMode] of cardinal = (GL_NONE, GL_COMPARE_REF_TO_TEXTURE);

  CCompareFunc: array[TTextureCompareFunc] of cardinal = (GL_LEQUAL, GL_GEQUAL,
    GL_LESS, GL_GREATER, GL_EQUAL, GL_NOTEQUAL, GL_ALWAYS, GL_NEVER);

procedure CheckOpenGLError;
var err: cardinal;
    msg, etx: string;
    error: boolean;
begin
  etx:=''; error:=false;
  repeat
    err := glGetError();
    if err<>GL_NO_ERROR then begin
      case err of
        GL_INVALID_ENUM: msg:='GL_INVALID_ENUM';
        GL_INVALID_VALUE: msg:='GL_INVALID_VALUE';
        GL_INVALID_OPERATION: msg:='GL_INVALID_OPERATION';
        GL_STACK_OVERFLOW: msg:='GL_STACK_OVERFLOW';
        GL_STACK_UNDERFLOW: msg:='GL_STACK_UNDERFLOW';
        GL_OUT_OF_MEMORY: msg:='GL_OUT_OF_MEMORY';
        GL_INVALID_FRAMEBUFFER_OPERATION: msg:='GL_INVALID_FRAMEBUFFER_OPERATION';
      end;
      etx:=etx+ '0x'+inttohex(err,4)+': '+msg + #13+#10;
      error:=true;
    end;
  until err = GL_NO_ERROR;
  assert(not error, #13+#10+etx);
end;

function ParseDebugMessage(aSource,aType,aId,aSeverity: cardinal;
  const aMess: ansistring): ansistring;
var t: ansistring;
begin
  case aSource of
    GL_DEBUG_SOURCE_API_ARB: t:='OpenGL';
    GL_DEBUG_SOURCE_WINDOW_SYSTEM_ARB: t:='Windows';
    GL_DEBUG_SOURCE_SHADER_COMPILER_ARB: t:='Shader Compiler';
    GL_DEBUG_SOURCE_THIRD_PARTY_ARB: t:='Third Party';
    GL_DEBUG_SOURCE_APPLICATION_ARB: t:='Application';
    GL_DEBUG_SOURCE_OTHER_ARB: t:='Other';
  end;
  result:='Source: '+t+'; ';

  case aType of
    GL_DEBUG_TYPE_ERROR_ARB: t:='Error';
    GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB: t:='Deprecated behavior';
    GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB: t:='Undefined behavior';
    GL_DEBUG_TYPE_PORTABILITY_ARB: t:='Portability';
    GL_DEBUG_TYPE_PERFORMANCE_ARB: t:='Performance';
    GL_DEBUG_TYPE_OTHER_ARB: t:='Other';
  end;
  result:=result+'Type: '+t+'; ';

  case aSeverity of
    GL_DEBUG_SEVERITY_HIGH_ARB: t:='High';
    GL_DEBUG_SEVERITY_MEDIUM_ARB: t:='Medium';
    GL_DEBUG_SEVERITY_LOW_ARB: t:='Low';
  end;
  result:=result+'Severity: '+t+'; ';
  result:=result+'Message: '+aMess;
end;

function GetWorkgroupCount: vec3i;
begin
  glGetIntegeri_v(GL_MAX_COMPUTE_WORK_GROUP_COUNT, 0, @Result[0]);
  glGetIntegeri_v(GL_MAX_COMPUTE_WORK_GROUP_COUNT, 1, @Result[1]);
  glGetIntegeri_v(GL_MAX_COMPUTE_WORK_GROUP_COUNT, 2, @Result[2]);
end;

function GetWorkgroupSize: vec3i;
begin
  glGetIntegeri_v(GL_MAX_COMPUTE_WORK_GROUP_SIZE, 0, @Result[0]);
  glGetIntegeri_v(GL_MAX_COMPUTE_WORK_GROUP_SIZE, 1, @Result[1]);
  glGetIntegeri_v(GL_MAX_COMPUTE_WORK_GROUP_SIZE, 2, @Result[2]);
end;

{ TGLBufferObject }

procedure TGLBufferObject.Bind;
begin
  assert(not FLocked, 'Buffer locked for mapping, please unmap it first');
  glBindBuffer(CBufferTypes[FBuffType], FBuffId);
  FLastTarget := FBuffType;
end;

procedure TGLBufferObject.Bind(AsTarget: TBufferType);
begin
  assert(not FLocked, 'Buffer locked for mapping, please unmap it first');
  glBindBuffer(CBufferTypes[AsTarget], FBuffId);
  FLastTarget := AsTarget;
end;

procedure TGLBufferObject.BindAllRange(Index: cardinal);
begin
  BindRange(FBuffType, Index, 0, FSize);
end;

procedure TGLBufferObject.BindBase(Index: cardinal);
begin
  assert(not FLocked, 'Buffer locked for mapping, please unmap it first');
  glBindBufferBase(CBufferTypes[FBuffType], Index, FBuffId);
  FLastTarget := FBuffType;
end;

procedure TGLBufferObject.BindBase(AsTarget: TBufferType; Index: cardinal);
begin
  assert(not FLocked, 'Buffer locked for mapping, please unmap it first');
  glBindBufferBase(CBufferTypes[AsTarget], Index, FBuffId);
  FLastTarget := AsTarget;
end;

procedure TGLBufferObject.BindRange(AsTarget: TBufferType; Index: cardinal;
  Offset, Size: integer);
begin
  assert(not FLocked, 'Buffer locked for mapping, please unmap it first');
  if AsTarget in [btAtomicCounter, btTransformFeedback, btUniform,
    btShaderStorage] then
  begin
    glBindBufferRange(CBufferTypes[AsTarget], Index, FBuffId, Offset, Size);
  end
  else
    assert(false, 'Bind Range not supported for this buffer type!');
end;

constructor TGLBufferObject.CreateFrom(const aBuffer: TBufferObject);
begin
  Create(aBuffer.BufferType);
  Allocate(aBuffer.Size, aBuffer.Data);
  Name := aBuffer.Name;
  FBuffer := aBuffer;
  aBuffer.Subscribe(self);
end;

procedure TGLBufferObject.UnBindBuffer;
begin
  assert(not FLocked, 'Buffer locked for mapping, please unmap it first');
  glBindBuffer(CBufferTypes[FLastTarget], 0);
end;

function TGLBufferObject.Map(AccessType: cardinal): pointer;
begin
  if FLocked then
  begin
    result := FMappedPointer;
    exit;
  end;
  assert(FSize > -1, 'You need set size first!');
  glBindBuffer(CBufferTypes[FBuffType], FBuffId);
  FMappedPointer := glMapBuffer(CBufferTypes[FBuffType], AccessType);
  FLocked := true;
  result := FMappedPointer;
end;

function TGLBufferObject.MapRange(AccessType, Offset, Length: cardinal)
  : pointer;
begin
  if FLocked then
  begin
    result := FMappedPointer;
    exit;
  end;
  assert(FSize > -1, 'You need set size first!');
  glBindBuffer(CBufferTypes[FBuffType], FBuffId);
  FMappedPointer := glMapBufferRange(CBufferTypes[FBuffType], Offset, Length,
    AccessType);
  FLocked := true;
  result := FMappedPointer;
end;

procedure TGLBufferObject.Notify(Sender: TObject; Msg: cardinal;
  Params: pointer);
var
  buff: TBufferObject;
begin
  inherited;
  case Msg of
    NM_ResourceChanged:
      if Sender.ClassType = TBufferObject then
      begin
        buff := TBufferObject(Sender);
        if buff.Size = FSize then
          Upload(buff.Data, buff.Size, 0)
        else
        begin
          glDeleteBuffers(1, @FBuffId);
          glGenBuffers(1, @FBuffId);
          Allocate(buff.Size, buff.Data);
        end;
      end;
    NM_ObjectDestroyed:
      if Sender = FBuffer then
      begin
        FBuffer := nil;
        glDeleteBuffers(1, @FBuffId);
      end;
  end;
end;

procedure TGLBufferObject.setBufferType(const Value: TBufferType);
begin
  assert(not FLocked, 'Buffer locked for mapping, please unmap it first');
  FBuffType := Value;
end;

procedure TGLBufferObject.UnMap;
begin
  if not FLocked then
    exit;
  glUnmapBuffer(CBufferTypes[FBuffType]);
  glBindBuffer(CBufferTypes[FBuffType], 0);
  FLocked := false;
  FMappedPointer := nil;
end;

constructor TGLBufferObject.Create(BuffType: TBufferType);
begin
  inherited Create;
  glGenBuffers(1, @FBuffId);
  FBuffType := BuffType;
  FSize := -1;
  Name := '';
  FLastTarget := FBuffType;
  FLocked := false;
  FMappedPointer := nil;
  FBuffer := nil;
end;

destructor TGLBufferObject.Destroy;
begin
  glDeleteBuffers(1, @FBuffId);
  if assigned(FBuffer) then
    FBuffer.UnSubscribe(self);
  inherited;
end;

procedure TGLBufferObject.Download(DestPtr: pointer; aSize, aOffset: integer; aMapBuffer: boolean);
var
  p: pointer;
begin
  assert(not FLocked, 'Buffer locked for mapping, please unmap it first');
  assert(FSize > -1, 'You need set size first!');
  assert(aSize + aOffset <= FSize, 'Size out of bounds!');
  assert(DestPtr <> nil, 'Destination buffer is not assigned!');
  glBindBuffer(CBufferTypes[FBuffType], FBuffId);
  if aMapBuffer then begin
    p := glMapBuffer(CBufferTypes[FBuffType], GL_READ_ONLY);
    p := pointer(integer(p) + aOffset);
    Move(p^, DestPtr^, aSize);
    glUnmapBuffer(CBufferTypes[FBuffType]);
  end else begin
    glGetBufferSubData(CBufferTypes[FBuffType], aOffset, aSize, DestPtr);
  end;
  glBindBuffer(CBufferTypes[FBuffType], 0);
end;

procedure TGLBufferObject.Upload(NewData: pointer; aSize, aOffset: integer);
begin
  assert(not FLocked, 'Buffer locked for mapping, please unmap it first');
  assert(FSize > -1, 'You need set size first!');
  assert(aSize + aOffset <= FSize, 'Size out of bounds!');
  glBindBuffer(CBufferTypes[FBuffType], FBuffId);
  glBufferSubData(CBufferTypes[FBuffType], aOffset, aSize, NewData);
  glBindBuffer(CBufferTypes[FBuffType], 0);
end;

procedure TGLBufferObject.Allocate(aSize: integer; aData: pointer; aUsage: cardinal);
begin
  assert(FSize = -1, 'You can''t change buffer size!');
  glBindBuffer(CBufferTypes[FBuffType], FBuffId);

  if ((FBuffType = btAtomicCounter) or (FBuffType = btShaderStorage))
  and (aUsage = GL_STATIC_DRAW) then
    glBufferData(CBufferTypes[FBuffType], aSize, aData, GL_DYNAMIC_COPY)
  else
    glBufferData(CBufferTypes[FBuffType], aSize, aData, aUsage);
  glBindBuffer(CBufferTypes[FBuffType], 0);
  FSize := aSize;
  FData := aData;
end;

{ TAtomicCounter }

constructor TGLAtomicCounter.Create;
var
  FValue: cardinal;
begin
  inherited Create(btAtomicCounter);
  FValue := 0;
  Allocate(sizeof(cardinal), @FValue);
end;

function TGLAtomicCounter.getValue: cardinal;
var
  FValue: cardinal;
begin
  Download(@FValue, sizeof(FValue), 0);
  result := FValue;
end;

procedure TGLAtomicCounter.setValue(const Value: cardinal);
begin
  Upload(@Value, sizeof(Value), 0);
end;

{ TAttribObject }

procedure TGLAttribObject.AssignBuffer(aBuffer: TGLBufferObject);
begin
  if Assigned(FGLBuffer) then
    FGLBuffer.UnSubscribe(Self);
  FGLBuffer := aBuffer;
  if Assigned(FGLBuffer) then
    FGLBuffer.Subscribe(Self);
end;

procedure TGLAttribObject.AssignBuffer(aBuffer: TBufferObject);
begin
  FGLBuffer := TGLBufferObject.CreateFrom(aBuffer);
  FGLBuffer.Subscribe(Self);
end;

procedure TGLAttribObject.Bind(DataPtr: pointer);
begin
  if not(apAttrSize in FSettedParam) or not(apAttrName in FSettedParam) or
    not(apAttrType in FSettedParam) then
    assert(false, 'Attribute not configured');

  if (FSemantic <> atUserAttrib) then begin
    FLocation:=CAttribSematics[FSemantic].Location;
  end else
    assert(FFixedLocation and (FLocation<>-1), 'Attrib location not found!');

  if Assigned(FGLBuffer) then
  begin
    FGLBuffer.Bind;
    glEnableVertexAttribArray(FLocation);
    glVertexAttribPointer(FLocation, FSize, CValueTypes[FType], FNormalized,
      FStride, DataPtr);
  end;
end;

procedure TGLAttribObject.BindAttrib(ShaderId: cardinal; DataPtr: pointer);
var
  idx: integer;
begin
  assert(ShaderId > 0, 'Shader Program is not assigned!');
  if FShaderId <> ShaderId then
  begin
    assert((ShaderId > 0) and glIsProgram(ShaderId),
      'Shader Program is not assigned!');
    if not FFixedLocation then
    begin
      idx := glGetAttribLocation(ShaderId, PAnsiChar(FAttribName));
      { TODO : Заменить Assert на Warning с записью в лог }
      // assert(idx>=0,'Attrib "'+FAttribName+'" location is not found in shader!');
      FLocation := idx;
    end;
    FShaderId := ShaderId;
  end;
  if assigned(FGLBuffer) then
    FGLBuffer.Bind;
  if FLocation > -1 then
  begin
    glEnableVertexAttribArray(FLocation);

    if not(apAttrSize in FSettedParam) or not(apAttrName in FSettedParam) or
      not(apAttrType in FSettedParam) then
      assert(false, 'Attribute not configured');

    glVertexAttribPointer(FLocation, FSize, CValueTypes[FType], FNormalized,
      FStride, DataPtr);
  end;
end;

constructor TGLAttribObject.CreateFrom(const aAttribObject: TAttribObject);
begin
  Create;
  FSize := aAttribObject.AttrSize;
  FAttribName := aAttribObject.AttrName;
  FType := aAttribObject.AttrType;
  FStride := aAttribObject.AttrStride;
  include(FSettedParam, apAttrName);
  include(FSettedParam, apAttrSize);
  include(FSettedParam, apAttrType);
  include(FSettedParam, apAttrStride);
  FNormalized := aAttribObject.Normalized;
  FElementSize := aAttribObject.ElementSize;
  FSemantic := aAttribObject.Semantic;
  FAttribObject := aAttribObject;
  aAttribObject.Subscribe(Self);
  if Assigned(aAttribObject.Buffer) then
  begin
    FGLBuffer := TGLBufferObject.CreateFrom(aAttribObject.Buffer);
    FGLBuffer.Subscribe(Self);
  end;
end;

destructor TGLAttribObject.Destroy;
begin
  if Assigned(FAttribObject) then
    FAttribObject.UnSubscribe(Self);
  if Assigned(FGLBuffer) then
  begin
    FGLBuffer.UnSubscribe(Self);
    FGLBuffer.Destroy;
  end;
  inherited;
end;

constructor TGLAttribObject.Create;
begin
  inherited;
  FSettedParam := [];
  FShaderId := 0;
  FNormalized := false;
  FLocation := -1;
  FGLBuffer := nil;
  FStride := 0;
  FElementSize := -1;
  FFixedLocation := false;
  FSemantic := atUserAttrib;
end;

constructor TGLAttribObject.CreateAndSetup(AttrName: ansistring; aSize: integer;
  AType: TValueType; AStride: integer; aBuffType: TBufferType);
begin
  Create;
  FSize := aSize;
  FAttribName := AttrName;
  FType := AType;
  FStride := AStride;
  include(FSettedParam, apAttrName);
  include(FSettedParam, apAttrSize);
  include(FSettedParam, apAttrType);
  include(FSettedParam, apAttrStride);
end;

function TGLAttribObject.getIndexPrt(Index: integer): pointer;
var
  p: integer;
begin
  result := nil;
  if not assigned(FGLBuffer) then
    exit;
  p := integer(FGLBuffer.Data);
  if p = 0 then
    exit;
  if Index * FElementSize > FGLBuffer.Size then
    exit;
  p := p + Index * FElementSize;
  result := pointer(p);
end;

procedure TGLAttribObject.Notify(Sender: TObject; Msg: Cardinal;
  Params: pointer);
begin
  case Msg of
    NM_ResourceChanged: DispatchMessage(NM_ResourceChanged);
    NM_ObjectDestroyed:
    begin
      if Sender = FGLBuffer then
        FGLBuffer := nil;
      if Sender = FAttribObject then
        FAttribObject := nil;
      DispatchMessage(NM_ResourceChanged);
    end;
  end;
end;

procedure TGLAttribObject.SetAttribLocation(Location: cardinal);
begin
  FLocation := Location;
  FFixedLocation := true;
end;

procedure TGLAttribObject.SetAttribSemantic(aSemantic: TAttribType);
begin
  FSemantic := aSemantic;
end;

procedure TGLAttribObject.Unbind;
begin
  if assigned(FGLBuffer) then
    FGLBuffer.UnBindBuffer;
  if FLocation > 0 then
    glDisableVertexAttribArray(FLocation);
end;

{ TAttribBuffer }

constructor TGLAttribBuffer.CreateAndSetup(
  AttrName: ansistring;
  aSize: integer; AType: TValueType; AStride: integer;
  aBuffType: TBufferType);
begin
  inherited CreateAndSetup(AttrName, aSize, AType, AStride, aBuffType);
  FGLBuffer := TGLBufferObject.Create(aBuffType);
  FGLBuffer.Subscribe(Self);
end;

{ TUniformList }

function StringHashKey(const Name: ansistring): integer;
var
  i, n, res: integer;
begin
  if name = '' then
    result := -1
  else
  begin
    n := Length(name);
    res := n;
    for i := 1 to n do
      res := (res shl 1) + Byte(name[i]);
    result := res;
  end;
end;

procedure TUniformList.AddKey(const Key: ansistring; Value: integer);
var
  iKey, i: integer;
begin
  iKey := StringHashKey(Key);
  for i := 0 to FCount - 1 do
    if (FItems[i].Key = iKey) and (FItems[i].KeyName = Key) then
      exit;
  if FCount >= Length(FItems) then
    setlength(FItems, FCount * 2);
  FItems[FCount].Key := iKey;
  FItems[FCount].KeyName := Key;
  FItems[FCount].Value := Value;
  inc(FCount);
end;

constructor TUniformList.Create;
begin
  setlength(FItems, 128);
  FCount := 0;
end;

function TUniformList.getValue(const Key: ansistring): integer;
var
  iKey, i: integer;
begin
  iKey := StringHashKey(Key);
  for i := 0 to FCount - 1 do
    if (FItems[i].Key = iKey) and (FItems[i].KeyName = Key) then
    begin
      result := FItems[i].Value;
      exit;
    end;
  result := -2;
end;

{ TShaderProgram }

procedure TGLSLShaderProgram.Apply;
begin
  if not FLinked then
    exit;
  if vActiveShader <> Self then
  begin
    glUseProgram(FShaderId);
    vActiveShader := self;
  end;
end;

procedure TGLSLShaderProgram.UnApply;
begin
  glUseProgram(0);
  vActiveShader := nil;
end;

procedure TGLSLShaderProgram.AttachShader(ShaderType: TShaderType;
  Source: ansistring);
var
  Shader: GLUInt;
  compiled: GLint;
  val, len: integer;
  pLog: PAnsiChar;
  p: PGLChar;
begin
  Shader := glCreateShader(CShaderTypes[ShaderType]);
  p := PGLChar(Source);
  if (Shader > 0) then
  begin
    glShaderSource(Shader, 1, @p, nil);
    glCompileShader(Shader);
    compiled := GL_FALSE;
    glGetShaderiv(Shader, GL_COMPILE_STATUS, @compiled);
    FLog := FLog + CShaderNames[ShaderType] + inttostr(Shader) +
      ' Compilation:';
    if compiled = GL_FALSE then
    begin
      FLog := FLog + ' Failed!' + #13#10;
      FError := true;
    end
    else
    begin
      glAttachShader(FShaderId, Shader);
      FLog := FLog + ' Successful' + #13#10;
    end;
    glGetShaderiv(Shader, GL_INFO_LOG_LENGTH, @val);
    getmem(pLog, val);
    glGetShaderInfoLog(Shader, val, len, pLog);
    if Length(pLog) > 0 then
      FLog := FLog + string(pLog) + #13#10;
    FreeMem(pLog, val);
    setlength(FDetachList, Length(FDetachList) + 1);
    FDetachList[high(FDetachList)] := Shader;
  end;
end;

procedure TGLSLShaderProgram.AttachShaderFromFile(ShaderType: TShaderType;
  FileName: string);
var
  s: TStringList;
begin
  s := TStringList.Create;
  s.LoadFromFile(FileName);
  AttachShader(ShaderType, ansistring(s.Text));
  s.Free;
end;

constructor TGLSLShaderProgram.CreateFrom(const aShaderProgram: TShaderProgram);
var
  st: TShaderType;
  i: integer;
begin
  Create;
  if (aShaderProgram.Binary.Size > 0) and assigned(aShaderProgram.Binary.Data)
  then
    with aShaderProgram.Binary do
      ProgramBinary(Data, Size, Format)
  else
  begin
    for st := stVertex to stCompute do
      AttachShader(st, aShaderProgram[st]);
    if aShaderProgram.FragDataBindPos.Location >= 0 then
      with aShaderProgram.FragDataBindPos do
        SetFragDataLocation(Location, Name);
    with aShaderProgram do
      for i := 0 to AttribBindsCount - 1 do
        SetAttribLocation(AttribBinds[i].Location, AttribBinds[i].Name);
  end;
  LinkShader;
end;

constructor TGLSLShaderProgram.Create;
begin
  inherited;
  FShaderId := glCreateProgram;
  FLinked := false;
  FError := false;
  FUniforms := TUniformList.Create;
  FUBOList := TUBOList.Create;
end;

destructor TGLSLShaderProgram.Destroy;
var
  i: integer;
begin
  glUseProgram(0);
  for i := 0 to Length(FDetachList) - 1 do
  begin
    glDetachShader(FShaderId, FDetachList[i]);
    glDeleteShader(FDetachList[i]);
  end;
  glDeleteProgram(FShaderId);
  FUniforms.Free;
  FUBOList.Free;
  inherited;
end;

procedure TGLSLShaderProgram.GetProgramBinary(var Binary: pointer;
  var Size: integer; var Format: GLEnum);
var
  binaryLength: integer;
  binaryFormat: cardinal;
begin
  if not FLinked then
  begin
    Binary := nil;
    Size := -1;
    Format := 0;
    exit;
  end;

  glGetProgramiv(FShaderId, GL_PROGRAM_BINARY_LENGTH, @binaryLength);
  Size := binaryLength;
  getmem(Binary, Size);
  glGetProgramBinary(FShaderId, Size, @binaryLength, @binaryFormat, Binary);
  assert(Size = binaryLength, 'Can''t write Shader Binary');
  Format := binaryFormat;
end;

function TGLSLShaderProgram.getShaderId: cardinal;
begin
  if FLinked then
    result := FShaderId
  else
    result := 0;
end;

function TGLSLShaderProgram.GetUniformLocation(ShaderId: cardinal;
  const Name: ansistring): integer;
begin
  result := FUniforms.getValue(Name);
  if result >= -1 then
    exit;
  if result = -2 then
    result := glGetUniformLocation(ShaderId, PAnsiChar(Name));
  if result >= 0 then
    FUniforms.AddKey(Name, result);
end;

function TGLSLShaderProgram.LinkShader: cardinal;
var
  val, len: integer;
  pLog: PAnsiChar;
  LinkError: boolean;
begin
  if (FShaderId > 0) and FLinked then
  begin
    result := FShaderId;
    exit;
  end;
  glProgramParameteri(FShaderId, GL_PROGRAM_BINARY_RETRIEVABLE_HINT, GL_TRUE);
  LinkError := false;
  glLinkProgram(FShaderId);
  glGetProgramiv(FShaderId, GL_LINK_STATUS, @val);
  FLog := FLog + 'Linking Program ' + inttostr(FShaderId);
  if val = GL_TRUE then
    FLog := FLog + ' Successful' + #13#10
  else
  begin
    FLog := FLog + ' Failed' + #13#10;
    FError := true;
    LinkError := true;
  end;
  glGetProgramiv(FShaderId, GL_INFO_LOG_LENGTH, @val);
  getmem(pLog, val);
  glGetProgramInfoLog(FShaderId, val, len, pLog);
  if Length(pLog) > 0 then
    FLog := FLog + string(pLog) + #13#10;
  FreeMem(pLog, val); // Assert(not FError,string(FLog));
  if LinkError then
  begin
    result := 0;
    FLinked := false;
  end
  else
  begin
    result := FShaderId;
    FLinked := true;
  end;
  QueryProgramInfo;
end;

function TGLSLShaderProgram.ProgramBinary(Binary: pointer; Size: integer;
  Format: GLEnum): cardinal;
var
  val, len, Status: integer;
  LoadingError: boolean;
  pLog: PAnsiChar;
begin
  LoadingError := false;
  glProgramBinary(FShaderId, Format, Binary, Size);
  glGetProgramiv(FShaderId, GL_LINK_STATUS, @Status);
  FLog := FLog + 'Loading Program ' + inttostr(FShaderId);
  if Status = GL_TRUE then
    FLog := FLog + ' Successful' + #13#10
  else
  begin
    FLog := FLog + ' Failed' + #13#10;
    FError := true;
    LoadingError := true;
  end;
  glGetProgramiv(FShaderId, GL_INFO_LOG_LENGTH, @val);
  getmem(pLog, val);
  glGetProgramInfoLog(FShaderId, val, len, pLog);
  if Length(pLog) > 0 then
    FLog := FLog + string(pLog) + #13#10;
  FreeMem(pLog, val); // Assert(not FError,string(FLog));
  if LoadingError then
  begin
    result := 0;
    FLinked := false;
  end
  else
  begin
    result := FShaderId;
    FLinked := true;
  end;
  QueryProgramInfo;
end;

procedure TGLSLShaderProgram.QueryProgramInfo;
var
  i: integer;
begin
  glGetProgramiv(FShaderId, GL_ACTIVE_ATTRIBUTES, @FActiveAttribs);
  glGetProgramiv(FShaderId, GL_ACTIVE_UNIFORMS, @FActiveUniforms);
  glGetProgramiv(FShaderId, GL_ACTIVE_UNIFORM_BLOCKS, @FActiveUniformsBlocks);
  for i := 0 to FActiveUniformsBlocks - 1 do
    FUBOList.AddUBO(TGLUniformBlock.Create(FShaderId, i));
end;

procedure TGLSLShaderProgram.SetUniform(const Name: ansistring;
  const Value: vec4; Count: GLsizei);
begin
  glUniform4fv(GetUniformLocation(FShaderId, name), Count, @Value);
end;

procedure TGLSLShaderProgram.SetUniform(const Name: ansistring;
  const Value: integer; Count: GLsizei);
begin
  glUniform1iv(GetUniformLocation(FShaderId, name), Count, @Value);
end;

procedure TGLSLShaderProgram.SetUniform(const Name: ansistring;
  const Value: vec3; Count: GLsizei);
begin
  glUniform3fv(GetUniformLocation(FShaderId, name), Count, @Value);
end;

procedure TGLSLShaderProgram.SetUniform(const Name: ansistring;
  const Value: single; Count: GLsizei);
begin
  glUniform1fv(GetUniformLocation(FShaderId, name), Count, @Value);
end;

procedure TGLSLShaderProgram.SetUniform(const Name: ansistring;
  const Value: vec2; Count: GLsizei);
begin
  glUniform2fv(GetUniformLocation(FShaderId, name), Count, @Value);
end;

procedure TGLSLShaderProgram.SetUniform(const Name: ansistring;
  const Value: mat3; Count: GLsizei; transpose: boolean);
begin
  glUniformMatrix3fv(GetUniformLocation(FShaderId, name), Count,
    transpose, @Value);
end;

procedure TGLSLShaderProgram.SetAttribLocation(Index: cardinal;
  Name: ansistring);
begin
  assert(not FLinked, 'Setup FragData before program linked');
  if FLinked then
    exit;
  glBindAttribLocation(FShaderId, Index, PAnsiChar(Name));
end;

procedure TGLSLShaderProgram.SetFragDataLocation(Index: cardinal;
  Name: ansistring);
begin
  assert(not FLinked, 'Setup FragData before program linked');
  if FLinked then
    exit;
  glBindFragDataLocation(FShaderId, Index, PAnsiChar(Name));
end;

procedure TGLSLShaderProgram.SetUniform(const Name: ansistring;
  const Value: mat4; Count: GLsizei; transpose: boolean);
begin
  glUniformMatrix4fv(GetUniformLocation(FShaderId, name), Count,
    transpose, @Value);
end;

procedure TGLSLShaderProgram.SetUniform(const Name: ansistring;
  const Value: array of TVector);
begin
  glUniform4fv(GetUniformLocation(FShaderId, name), Length(Value),
    PGLFloat(Value[0].GetAddr));
end;

procedure TGLSLShaderProgram.SetUniform(const Name: ansistring;
  const Value: mat2; Count: GLsizei; transpose: boolean);
begin
  glUniformMatrix2fv(glGetUniformLocation(FShaderId, PGLChar(name)), Count,
    transpose, @Value);
end;

{ TVertexObject }

procedure TGLVertexObject.Build(ShaderId: cardinal;
  const aVertexObject: TVertexObject);
var
  i: integer;
  attr: TGLAttribObject;
begin
  if FStructureChanged then
  begin
    glBindVertexArray(0);
    if FVAO > 0 then
      glDeleteVertexArrays(1, @FVAO);
    if FIndiceChanged then
    begin
      if FIndiceId = 0 then
        glGenBuffers(1, @FIndiceId);
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FIndiceId);
      if assigned(FIndicePtr) then
        glBufferData(GL_ELEMENT_ARRAY_BUFFER, FIndiceCount * 4, FIndicePtr,
          GL_STATIC_DRAW)
      else
        glBufferData(GL_ELEMENT_ARRAY_BUFFER, FIndiceCount * 4, @FIndices[0],
          GL_STATIC_DRAW);
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
      FIndiceChanged := false;
    end;

    glGenVertexArrays(1, @FVAO);
    glBindVertexArray(FVAO);

    for i := 0 to FAttribs.Count - 1 do
    begin
      attr := FAttribs[i];
      if attr.FSemantic<>atUserAttrib then attr.Bind
      else attr.BindAttrib(ShaderId);
    end;
    glBindVertexArray(0);
    FStructureChanged := false;
  end;
end;

constructor TGLVertexObject.Create;
begin
  inherited;
  FVAO := 0;
  FIndiceId := 0;
  FShader := nil;
  FIndicePtr := nil;
  FAttribs := TList.Create;
  FSubMeshes := TList.Create;
  FStructureChanged := true;
  FIndiceChanged := true;
end;

procedure TGLVertexObject.Bind;
var i: integer;
begin
  if FStructureChanged then Build;
//  if FVAO>0 then glBindVertexArray(FVAO)
//  else
    for i := 0 to FAttribs.Count - 1 do Attribs[i].Bind();
  if FIndiceCount > 0 then
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FIndiceId);

end;

procedure TGLVertexObject.Build(const aVertexObject: TVertexObject);
var
  i: integer;
  attr: TGLAttribObject;
begin
  if FStructureChanged then
  begin
    glBindVertexArray(0);
    if FVAO > 0 then
      glDeleteVertexArrays(1, @FVAO);
    if FIndiceChanged then
    begin
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
      glDeleteBuffers(1, @FIndiceId);
      glGenBuffers(1, @FIndiceId);
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FIndiceId);
      if assigned(FIndicePtr) then
        glBufferData(GL_ELEMENT_ARRAY_BUFFER, FIndiceCount * 4, FIndicePtr,
          GL_STATIC_DRAW)
      else
        glBufferData(GL_ELEMENT_ARRAY_BUFFER, FIndiceCount * 4, @FIndices[0],
          GL_STATIC_DRAW);
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
      FIndiceChanged := false;
    end;

    glGenVertexArrays(1, @FVAO);
    glBindVertexArray(FVAO);

    for i := 0 to FAttribs.Count - 1 do
    begin
      attr := FAttribs[i]; attr.Bind;
    end;
    glBindVertexArray(0);
    for i := 0 to FAttribs.Count - 1 do
    begin
      attr := FAttribs[i];
      attr.Unbind;
    end;
    FStructureChanged := false;
  end;
end;

constructor TGLVertexObject.CreateFrom(const aVertexObject: TVertexObject);
var
  i: integer;
  ab: TGLAttribBuffer;
begin
  Create;
  FVertexObject := aVertexObject;
  aVertexObject.Subscribe(Self);
  FFaceType := aVertexObject.FaceType;
  for i := 0 to aVertexObject.AttribsCount - 1 do
  begin
    ab := TGLAttribBuffer.CreateFrom(aVertexObject.Attribs[i]);
    FAttribs.Add(ab);
    ab.Subscribe(Self);
  end;
  FIndicePtr := aVertexObject.IndicePtr;
  FIndiceCount := aVertexObject.IndiceCount;
end;

destructor TGLVertexObject.Destroy;
var
  I: Integer;
  attr: TGLAttribObject;
begin
  if FIndiceId > 0 then
  begin
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    glDeleteBuffers(1, @FIndiceId);
  end;
  for I := 0 to FAttribs.Count - 1 do
  begin
    attr := TGLAttribObject(FAttribs[I]);
    attr.UnSubscribe(Self);
    attr.Destroy;
  end;
  if Assigned(FShader) then
    FShader.UnSubscribe(Self);
  if Assigned(FVertexObject) then
    FVertexObject.UnSubscribe(Self);
  FAttribs.Destroy;
  FSubMeshes.Destroy;
  inherited;
end;

function TGLVertexObject.getAttrCount: integer;
begin
  result := FAttribs.Count;
end;

function TGLVertexObject.getAttrib(Index: integer): TGLAttribObject;
begin
  if (Index >= 0) and (Index < FAttribs.Count) then
    result := FAttribs[Index]
  else
    result := nil;
end;

function TGLVertexObject.GetAttribBySemantic(aSemantic: TAttribType)
  : TGLAttribObject;
var
  i: integer;
  attr: TGLAttribObject;
begin
  for i := 0 to FAttribs.Count - 1 do
  begin
    attr := FAttribs[i];
    if attr.FSemantic = aSemantic then
    begin
      result := attr;
      exit;
    end;
  end;
  result := nil;
end;

function TGLVertexObject.getECount: integer;
var ec,i: integer;
begin
  if (FElementsCount>=0) and (not FStructureChanged)
  then result := FElementsCount
  else if (FIndiceCount>0) and (FIndiceId>0) then begin
    result := FIndiceCount; FElementsCount := FIndiceCount;
  end else begin
    ec := high(integer);
    for i := 0 to FAttribs.Count - 1 do
    begin
      if Attribs[i].ElementSize > 0 then
        ec := min(ec, Attribs[i].Buffer.Size div Attribs[i].ElementSize);
    end;
    result := ec; FElementsCount := ec;
  end;
end;

function TGLVertexObject.getIndice(Index: integer): integer;
begin
  if not assigned(FIndicePtr) then
    result := FIndices[index]
  else
  begin
    result := PInteger(integer(FIndicePtr) + index * 4)^;
  end;
end;

procedure TGLVertexObject.Notify(Sender: TObject; Msg: Cardinal;
  Params: pointer);
var
  I: Integer;
begin
  if Msg = NM_ObjectDestroyed then
  begin
    if Sender = FVertexObject then
      FVertexObject := nil;
    if Sender = FShader then
      FShader := nil;
    for I := 0 to FAttribs.Count - 1 do
      if FAttribs[I] = Sender then
      begin
        FAttribs.Delete(I);
        break;
      end;
    FStructureChanged := True;
    DispatchMessage(NM_ResourceChanged);
  end
  else if Msg = NM_ResourceChanged then
  begin
    FStructureChanged := True;
    DispatchMessage(NM_ResourceChanged);
  end;
end;

procedure TGLVertexObject.RenderVO(aShader: integer);
var
  ActiveShader: integer;
  i, ec: integer;
  function min(a, b: integer): integer;
  begin
    if a < b then
      result := a
    else
      result := b;
  end;

begin
  if Assigned(vActiveShader) then
    ActiveShader := vActiveShader.FShaderId
  else
    ActiveShader := 0;

  if aShader > 0 then
  begin
    if ActiveShader <> aShader then
    begin
      glUseProgram(aShader);
      ActiveShader := aShader;
    end;
  end
  else if (aShader = -1) and (assigned(FShader)) then
  begin
    FShader.Apply;
    ActiveShader := Shader.Id;
  end
  else
  begin
    glUseProgram(0);
    ActiveShader := 0;
  end;
  if FStructureChanged and (ActiveShader = 0) then
    exit;
  if FStructureChanged then
    Build(ActiveShader);
  ec := high(integer);
  for i := 0 to FAttribs.Count - 1 do
  begin
    if Attribs[i].ElementSize > 0 then
      ec := min(ec, Attribs[i].Buffer.Size div Attribs[i].ElementSize);
  end;
  glBindVertexArray(FVAO);
  if (FIndiceCount > 0) and (FIndiceId > 0) then
  begin
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FIndiceId);
    glDrawElements(CFaceTypeConst[FFaceType], FIndiceCount,
      GL_UNSIGNED_INT, nil);
  end
  else
  begin
    if ec > 0 then
      glDrawArrays(CFaceTypeConst[FFaceType], 0, ec);
  end;
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
  { if (ActiveShader>0) and ((ActiveShader=aShader) or (ActiveShader = FShader.Id))
    then glUseProgram(0); }
end;

procedure TGLVertexObject.SetShader(const Value: TGLSLShaderProgram);
begin
  if Assigned(FShader) then
    FShader.UnSubscribe(Self);
  FShader := Value;
  if Assigned(FShader) then
    FShader.Subscribe(Self);
end;

procedure TGLVertexObject.UnBind;
var i: integer;
begin
  if FVAO>0 then glBindVertexArray(0)
  else
    for i := 0 to FAttribs.Count - 1 do Attribs[i].UnBind;
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
end;

{ TGLTextureObject }

constructor TGLTextureObject.Create;
begin
  inherited Create;
  glGenTextures(1, @FTexId);
  glGenBuffers(1, @FpboId);
  FTarget := ttTexture2D;
  FTextureSampler := nil;
  FTextureObject := nil;
  FImageHolder:=nil;
  FTexDesc:=nil;
  FGenerateMipMaps := false;
end;

constructor TGLTextureObject.CreateFrom(const aTarget: TTexTarget;
  const aImageHolder: TImageHolder; const aTexDesc: PTextureDesc);
begin
  Create;
  glGenTextures(1, @FTexId);
  glGenBuffers(1, @FpboId);
  FTexDesc := aTexDesc;
  FImageHolder:= aImageHolder;
  if assigned(FImageHolder) then
    FFormatDescr := TGLTextureFormatSelector.GetTextureFormat(FImageHolder.ImageFormat);
  FTarget := aTarget;
end;

constructor TGLTextureObject.CreateFrom(const aTexture: TTexture);
begin
  Create;
  glGenTextures(1, @FTexId);
  glGenBuffers(1, @FpboId);
  FImageHolder := aTexture.ImageHolder;
  if assigned(FImageHolder) then
    FFormatDescr := TGLTextureFormatSelector.GetTextureFormat(FImageHolder.ImageFormat);
  FTarget := aTexture.Target;
end;

destructor TGLTextureObject.Destroy;
begin
  glBindTexture(CTexTargets[FTarget], 0);
  glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
  glBindBuffer(GL_PIXEL_PACK_BUFFER, 0);
  glDeleteTextures(1, @FTexId);
  glDeleteBuffers(1, @FpboId);
  inherited;
end;

function TGLTextureObject.getColorFormat: cardinal;
begin
  result := FFormatDescr.BaseFormat;
end;

function TGLTextureObject.getDataType: cardinal;
begin
  result := FFormatDescr.PixelFormat;
end;

function TGLTextureObject.getInternalFormat: cardinal;
begin
  result := FFormatDescr.InternalFormat;
end;

procedure TGLTextureObject.UploadTexture(Data: pointer; Size: cardinal);
begin
  assert(assigned(FImageHolder),'Image holder is not assigned!');
  with FImageHolder, FFormatDescr do begin
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, FpboId);
    glBindTexture(CTexTargets[FTarget], FTexId);
    case FTarget of
      ttTexture1D:
        glTexImage1D(CTexTargets[FTarget], 0, InternalFormat, Width, 0,
          BaseFormat, PixelFormat, nil);
      ttTexture2D, ttTextureRectangle, ttCubemap .. ttCubemapNZ:
        glTexImage2D(CTexTargets[FTarget], 0, InternalFormat, Width, Height, 0,
          BaseFormat, PixelFormat, nil);
      ttTexture3D:
        glTexImage3D(CTexTargets[FTarget], 0, InternalFormat, Width, Height,
          Depth, 0, BaseFormat, PixelFormat, nil);
    end;
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    if assigned(FTextureObject) and FTextureObject.GenerateMipMaps then
      glGenerateMipmap(CTexTargets[FTarget]);
    glBindTexture(CTexTargets[FTarget], 0);
  end;
end;

{ TFrameBufferObject }

procedure TGLFrameBufferObject.AttachTexture(tex: TGLTextureObject;
  aTarget: TMRTTarget);
var
  i, n, m: integer;
begin
  glBindFramebuffer(GL_FRAMEBUFFER, FBOId);
  n := -1;
  if aTarget = tgTexture then
  begin
    AttachTextureTarget(tex, GL_COLOR_ATTACHMENT0 +
      FAttachments.Textures.Count);
    FAttachments.Textures.Add(tex);
  end
  else
  begin
    case aTarget of
      tgDepth:
        AttachDepthTexture(tex);
      tgDepthStencil:
        AttachDepthStencilTexture(tex);
      tgMRT0:
        begin
          AttachTextureTarget(tex, GL_COLOR_ATTACHMENT0);
          n := 0;
        end;
      tgMRT1:
        begin
          AttachTextureTarget(tex, GL_COLOR_ATTACHMENT1);
          n := 1;
        end;
      tgMRT2:
        begin
          AttachTextureTarget(tex, GL_COLOR_ATTACHMENT2);
          n := 2;
        end;
      tgMRT3:
        begin
          AttachTextureTarget(tex, GL_COLOR_ATTACHMENT3);
          n := 3;
        end;
    end;
    if n >= 0 then
    begin
      if n > FAttachments.Textures.Count - 1 then
      begin
        m := FAttachments.Textures.Count;
        FAttachments.Textures.Count := n + 1;
        for i := m to n do
          FAttachments.Textures[i] := nil;
      end;
      FAttachments.Textures[n] := tex;
    end;
  end;
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
end;

procedure TGLFrameBufferObject.DetachTexture(Index: integer);
var
  tex: TGLTextureObject;
begin
  if (index < FAttachments.Textures.Count) and (index >= 0) then
  begin
    tex := FAttachments.Textures[index];
    glBindFramebuffer(GL_FRAMEBUFFER, FBOId);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + index,
      CTexTargets[tex.Target], 0, 0);
    FAttachments.Textures.Delete(index);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
  end;
end;

procedure TGLFrameBufferObject.ConfigFBO(RenderBuffers: TRenderBuffers);
begin
  FRenderBuffers := RenderBuffers;
end;

constructor TGLFrameBufferObject.Create;
begin
  inherited;
  with FAttachments do
  begin
    Textures := TList.Create;
    DepthBuffer.Mode := bmNone;
    StencilBuffer.Mode := bmNone;
    DepthStencilBuffer.Mode := bmNone;
    glGenRenderbuffers(1, @DepthBuffer.BuffId);
    glGenRenderbuffers(1, @StencilBuffer.BuffId);
    glGenRenderbuffers(1, @DepthStencilBuffer.BuffId);
    FReadBackBuffers := TList.Create;
  end;
  glGenFramebuffers(1, @FBOId);
  glGenFramebuffers(1, @FMSFBOId);
  FRenderBuffers := [];
  FActive := false;
  FDeactivate := false;
  FInit := false;
end;

destructor TGLFrameBufferObject.Destroy;
var
  FBTarget: GLEnum;
  i: integer;
begin
  FBTarget := GL_FRAMEBUFFER;

  for i := 0 to FAttachments.Textures.Count - 1 do
    DetachTexture(i);
  FAttachments.Textures.Destroy;
  glBindFramebuffer(FBTarget, 0);
  glBindRenderbuffer(GL_RENDERBUFFER, 0);
  glDeleteFramebuffers(1, @FBOId);
  glDeleteFramebuffers(1, @FMSFBOId);
  FReadBackBuffers.Free;
  with FAttachments do
  begin
    glDeleteRenderbuffers(1, @DepthBuffer.BuffId);
    glDeleteRenderbuffers(1, @StencilBuffer.BuffId);
    glDeleteRenderbuffers(1, @DepthStencilBuffer.BuffId);
  end;
  inherited;
end;

function TGLFrameBufferObject.GetTexture(Index: integer): TGLTextureObject;
begin
  assert(index < FAttachments.Textures.Count,
    'Not enough attached texture units');
  result := FAttachments.Textures[index];
end;

procedure TGLFrameBufferObject.SetTexture(Index: integer;
  const Value: TGLTextureObject);
begin
  assert(index < FAttachments.Textures.Count,
    'Not enough attached texture units');
  FAttachments.Textures[index] := Value;
end;

function TGLFrameBufferObject.CheckCompleteness: boolean;
var
  Status: cardinal;
begin
  Status := glCheckFramebufferStatus(GL_FRAMEBUFFER);
  result := false;
  if Status = GL_FRAMEBUFFER_COMPLETE then
  begin
    result := true;
    exit;
  end;
  case Status of
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT:
      assert(false, 'Incomplete attachment');
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT:
      assert(false, 'Incompleate or missing attachment');
    // GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT: assert(false, 'Dublicate attachments');
    // GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS: assert(false, 'Incomplete dimensions');
    // GL_FRAMEBUFFER_INCOMPLETE_FORMATS: assert(false, 'Incomplete formats');
    GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER:
      assert(false, 'Incomplete draw buffer');
    GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER:
      assert(false, 'Incomplete read buffer');
    GL_FRAMEBUFFER_UNSUPPORTED:
      assert(false, 'Unsupported configuration');
    // GL_FRAMEBUFFER_STATUS_ERROR: assert(false, 'Status Error');
  end;
end;

procedure TGLFrameBufferObject.ConfigDepthBuffer(Mode: TBufferMode;
  Precision: TDepthPrecision);
begin
  FAttachments.DepthBuffer.Mode := Mode;
  FAttachments.DepthBuffer.Precision := OGLDBPrecision(Precision);
end;

procedure TGLFrameBufferObject.ConfigStencilBuffer(Mode: TBufferMode;
  Precision: TStencilPrecision);
begin
  FAttachments.StencilBuffer.Mode := Mode;
  FAttachments.StencilBuffer.Precision := OGLSBPrecision(Precision);
end;

procedure TGLFrameBufferObject.ConfigDepthStencilBuffer(Mode: TBufferMode);
begin
  FAttachments.DepthStencilBuffer.Mode := Mode;
  FAttachments.DepthStencilBuffer.Precision := 0;
end;

procedure TGLFrameBufferObject.InitFBO(Width, Height: integer);
var
  FBTarget: GLEnum;
begin
  if (Width = FWidth) and (Height = FHeight) and FInit then
    exit
  else if FInit then
    ResetFBO(false);
  FWidth := Width;
  FHeight := Height;
  FBTarget := GL_FRAMEBUFFER;

  with FAttachments do
  begin
    if (rbDepth in FRenderBuffers) and (not(rbStencil in FRenderBuffers)) then
    begin
      if DepthBuffer.Mode <> bmNone then
      begin
        glBindRenderbuffer(GL_RENDERBUFFER, DepthBuffer.BuffId);
        glRenderbufferStorage(GL_RENDERBUFFER, DepthBuffer.Precision,
          FWidth, FHeight);
        glBindRenderbuffer(GL_RENDERBUFFER, 0);
      end;
      case DepthBuffer.Mode of
        bmBuffer:
          begin
            glBindFramebuffer(FBTarget, FBOId);
            glFramebufferRenderbuffer(FBTarget, GL_DEPTH_ATTACHMENT,
              GL_RENDERBUFFER, DepthBuffer.BuffId);
            glBindFramebuffer(FBTarget, 0);
          end;
      end;
    end;
    if (not(rbDepth in FRenderBuffers)) and (rbStencil in FRenderBuffers) then
    begin
      if StencilBuffer.Mode <> bmNone then
      begin
        glBindRenderbuffer(GL_RENDERBUFFER, StencilBuffer.BuffId);
        glRenderbufferStorage(GL_RENDERBUFFER, StencilBuffer.Precision,
          FWidth, FHeight);
        glBindRenderbuffer(GL_RENDERBUFFER, 0);
      end;
      case StencilBuffer.Mode of
        bmBuffer:
          begin
            glBindFramebuffer(FBTarget, FBOId);
            glFramebufferRenderbuffer(FBTarget, GL_STENCIL_ATTACHMENT,
              GL_RENDERBUFFER, StencilBuffer.BuffId);
            glBindFramebuffer(FBTarget, 0);
          end;
      end;
    end;

    if (rbDepth in FRenderBuffers) and (rbStencil in FRenderBuffers) then
    begin
      glBindFramebuffer(FBTarget, FBOId);
      glBindRenderbuffer(GL_RENDERBUFFER, DepthStencilBuffer.BuffId);
      glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH24_STENCIL8,
        FWidth, FHeight);
      glBindRenderbuffer(GL_RENDERBUFFER, 0);
      case DepthStencilBuffer.Mode of
        bmBuffer:
          begin
            glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT,
              GL_RENDERBUFFER, DepthStencilBuffer.BuffId);
            glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT,
              GL_RENDERBUFFER, DepthStencilBuffer.BuffId);
          end;
      end;
      glBindFramebuffer(FBTarget, 0);
    end;
  end;
  FInit := true;
  // CheckCompleteness;
end;

procedure TGLFrameBufferObject.AttachDepthTexture(tex: TGLTextureObject);
begin
  if FAttachments.DepthBuffer.Mode <> bmBuffer then
  begin
    if tex <> FAttachments.DepthBuffer.Texture then
    begin
      glBindFramebuffer(GL_FRAMEBUFFER, FBOId);
      if assigned(FAttachments.DepthBuffer.Texture) then
        AttachTextureTarget(nil, GL_DEPTH_ATTACHMENT);
      AttachTextureTarget(tex, GL_DEPTH_ATTACHMENT);
      glBindFramebuffer(GL_FRAMEBUFFER, 0);
      FAttachments.DepthBuffer.Texture := tex;
      FAttachments.DepthBuffer.Mode := bmTexture;
    end;
  end;
end;

procedure TGLFrameBufferObject.AttachStencilTexture(tex: TGLTextureObject);
begin
  if FAttachments.StencilBuffer.Mode <> bmBuffer then
  begin
    if tex <> FAttachments.StencilBuffer.Texture then
    begin
      glBindFramebuffer(GL_FRAMEBUFFER, FBOId);
      if assigned(FAttachments.StencilBuffer.Texture) then
        AttachTextureTarget(nil, GL_STENCIL_ATTACHMENT);
      AttachTextureTarget(tex, GL_STENCIL_ATTACHMENT);
      glBindFramebuffer(GL_FRAMEBUFFER, 0);
      FAttachments.StencilBuffer.Texture := tex;
      FAttachments.StencilBuffer.Mode := bmTexture;
    end;
  end;
end;

procedure TGLFrameBufferObject.AttachDepthStencilTexture(tex: TGLTextureObject);
begin
  if FAttachments.DepthStencilBuffer.Mode <> bmBuffer then
  begin
    if tex <> FAttachments.DepthStencilBuffer.Texture then
    begin
      glBindFramebuffer(GL_FRAMEBUFFER, FBOId);
      if assigned(FAttachments.DepthStencilBuffer.Texture) then
      begin
        AttachTextureTarget(nil, GL_STENCIL_ATTACHMENT);
        AttachTextureTarget(nil, GL_DEPTH_ATTACHMENT);
      end;
      AttachTextureTarget(tex, GL_DEPTH_ATTACHMENT);
      AttachTextureTarget(tex, GL_STENCIL_ATTACHMENT);
      glBindFramebuffer(GL_FRAMEBUFFER, 0);
      FAttachments.DepthStencilBuffer.Texture := tex;
      FAttachments.DepthStencilBuffer.Mode := bmTexture;
    end;
  end;
end;

procedure TGLFrameBufferObject.DetachAllTextures;
var
  tex: TGLTextureObject;
  i: integer;
begin
  glBindFramebuffer(GL_FRAMEBUFFER, FBOId);
  for i := 0 to FAttachments.Textures.Count - 1 do
  begin
    tex := FAttachments.Textures[i];
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + i,
      CTexTargets[tex.Target], 0, 0);
  end;
  FAttachments.Textures.Clear;
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
end;

procedure TGLFrameBufferObject.DetachDepthStencilTexture;
begin
  if FAttachments.DepthStencilBuffer.Mode = bmTexture then
  begin
    glBindFramebuffer(GL_FRAMEBUFFER, FBOId);
    AttachTextureTarget(nil, GL_DEPTH_ATTACHMENT);
    AttachTextureTarget(nil, GL_STENCIL_ATTACHMENT);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
    FAttachments.DepthStencilBuffer.Texture := nil;
    FAttachments.DepthStencilBuffer.Mode := bmNone;
  end;
end;

procedure TGLFrameBufferObject.DetachDepthTexture;
begin
  if FAttachments.DepthBuffer.Mode = bmTexture then
  begin
    glBindFramebuffer(GL_FRAMEBUFFER, FBOId);
    AttachTextureTarget(nil, GL_DEPTH_ATTACHMENT);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
    FAttachments.DepthBuffer.Texture := nil;
    FAttachments.DepthBuffer.Mode := bmNone;
  end;
end;

procedure TGLFrameBufferObject.DetachStencilTexture;
begin
  if FAttachments.StencilBuffer.Mode = bmTexture then
  begin
    glBindFramebuffer(GL_FRAMEBUFFER, FBOId);
    AttachTextureTarget(nil, GL_STENCIL_ATTACHMENT);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
    FAttachments.StencilBuffer.Texture := nil;
    FAttachments.StencilBuffer.Mode := bmNone;
  end;
end;

function TGLFrameBufferObject.OGLDBPrecision
  (Precision: TDepthPrecision): GLEnum;
begin
  case Precision of
    dpDefault:
      result := GL_DEPTH_COMPONENT;
    dp16:
      result := GL_DEPTH_COMPONENT16;
    dp24:
      result := GL_DEPTH_COMPONENT24;
    dp32:
      result := GL_DEPTH_COMPONENT32;
  else
    result := GL_DEPTH_COMPONENT;
  end;
end;

function TGLFrameBufferObject.OGLSBPrecision
  (Precision: TStencilPrecision): GLEnum;
begin
  case Precision of
    spDefault:
      result := GL_STENCIL_INDEX;
    sp1bit:
      result := GL_STENCIL_INDEX1;
    sp4bits:
      result := GL_STENCIL_INDEX4;
    sp8bits:
      result := GL_STENCIL_INDEX8;
    sp16bits:
      result := GL_STENCIL_INDEX16;
  else
    result := GL_STENCIL_INDEX;
  end;
end;

procedure TGLFrameBufferObject.AttachTextureTarget(tex: TGLTextureObject;
  attachement: GLEnum);
var
  FBTarget: GLEnum;
  th: integer;
begin
  FBTarget := GL_FRAMEBUFFER;
  if assigned(tex) then
    th := tex.Id
  else
  begin
    glFramebufferTexture2D(FBTarget, attachement, GL_TEXTURE_2D, 0, 0);
    exit;
  end;

  case tex.Target of
    ttTexture1D:
      glFramebufferTexture1D(FBTarget, attachement, GL_TEXTURE_1D, th, 0);
    ttTexture2D:
      glFramebufferTexture2D(FBTarget, attachement, GL_TEXTURE_2D, th, 0);
    ttTexture3D:
      glFramebufferTexture3D(FBTarget, attachement, GL_TEXTURE_3D, th, 0, 0);
  end;
end;

procedure TGLFrameBufferObject.Apply(ClearBuffers: boolean);
var
  buffers: array of GLEnum;
  i, n: integer;
  cb: GLUInt;
  FBTarget: GLEnum;
begin
  FBTarget := GL_FRAMEBUFFER;

  glBindFramebuffer(FBTarget, FBOId);
  glGetIntegerv(GL_VIEWPORT, @FViewport);
  if (FViewport[2] <> FWidth) or (FViewport[3] <> FHeight) then
    glViewport(0, 0, FWidth, FHeight);

  with FAttachments do
  begin
    if Textures.Count > 0 then
    begin
      n := 0;
      setlength(buffers, Textures.Count);
      for i := 0 to Textures.Count - 1 do
      begin
        if assigned(Textures[i]) then
        begin
          buffers[n] := GL_COLOR_ATTACHMENT0 + n;
          inc(n);
        end;
      end;
      setlength(buffers, n);
      glDrawBuffers(n, @buffers[0]);
      glColorMask(true, true, true, true);
    end
    else
    begin
      glDrawBuffer(GL_NONE);
      glReadBuffer(GL_NONE);
      glColorMask(false, false, false, false);
    end;
    CheckCompleteness;
    if ClearBuffers then
    begin
      if Textures.Count > 0 then
        cb := GL_COLOR_BUFFER_BIT
      else
        cb := 0;
      if (rbDepth in FRenderBuffers) or
        ((DepthBuffer.Mode = bmTexture) and assigned(DepthBuffer.Texture)) then
        cb := cb or GL_DEPTH_BUFFER_BIT;
      if (rbStencil in FRenderBuffers) or
        ((StencilBuffer.BuffId > 0) and (StencilBuffer.Mode = bmTexture)) then
        cb := cb or GL_STENCIL_BUFFER_BIT;
      if cb <> 0 then
        glClear(cb);
    end;
  end;
end;

procedure TGLFrameBufferObject.UnApply;
var
  tex: TGLTextureObject;
  i, n: integer;
  FBTarget: GLEnum;
begin

  FBTarget := GL_FRAMEBUFFER;
  for i := 0 to FReadBackBuffers.Count - 1 do
  begin
    n := integer(FReadBackBuffers[i]);
    if n < FAttachments.Textures.Count then
    begin
      tex := FAttachments.Textures[n];
      if assigned(tex) then
      begin
        glReadBuffer(GL_COLOR_ATTACHMENT0 + n);
        glBindBuffer(GL_PIXEL_PACK_BUFFER, tex.FpboId);
        glReadPixels(0, 0, FWidth, FHeight, tex.ColorFormat,
          tex.DataType, nil);
      end;
    end;
  end;
  if (FViewport[2] <> FWidth) or (FViewport[2] <> FHeight) then
    glViewport(FViewport[0], FViewport[1], FViewport[2], FViewport[3]);

  glBindFramebuffer(FBTarget, 0);
  glReadBuffer(GL_BACK);
  glDrawBuffer(GL_BACK);
  with FAttachments do
  begin
    for i := 0 to Textures.Count - 1 do
    begin
      tex := Textures[i];
      if assigned(tex) and tex.GenerateMipMaps then
      begin
        glBindTexture(CTexTargets[tex.Target], tex.Id);
        glGenerateMipmap(CTexTargets[tex.Target]);
        glBindTexture(CTexTargets[tex.Target], 0);
      end;
    end;
    if DepthBuffer.Mode = bmTexture then
    begin
      tex := DepthBuffer.Texture;
      if assigned(tex) and tex.GenerateMipMaps then
      begin
        glBindTexture(CTexTargets[tex.Target], tex.Id);
        glGenerateMipmap(CTexTargets[tex.Target]);
        glBindTexture(CTexTargets[tex.Target], 0);
      end;
    end;
    if StencilBuffer.Mode = bmTexture then
    begin
      tex := StencilBuffer.Texture;
      if assigned(tex) and tex.GenerateMipMaps then
      begin
        glBindTexture(CTexTargets[tex.Target], tex.Id);
        glGenerateMipmap(CTexTargets[tex.Target]);
        glBindTexture(CTexTargets[tex.Target], 0);
      end;
    end;
  end;
  if FDeactivate then
    FActive := false;
end;

procedure TGLFrameBufferObject.SetReadBackBuffer(const ColorBufers
  : array of GLUInt);
var
  i: integer;
begin
  FReadBackBuffers.Clear;
  for i := 0 to Length(ColorBufers) - 1 do
    FReadBackBuffers.Add(pointer(ColorBufers[i]));
end;

function TGLFrameBufferObject.GetAttachmentsCount: integer;
begin
  result := FAttachments.Textures.Count;
end;

procedure TGLFrameBufferObject.ResetFBO(ResetConfig: boolean);
var
  i: integer;
begin
  FInit := false;

  for i := 0 to FAttachments.Textures.Count - 1 do
    DetachTexture(i);
  if FAttachments.DepthBuffer.Mode = bmTexture then
    DetachDepthTexture;
  if FAttachments.StencilBuffer.Mode = bmTexture then
    DetachStencilTexture;
  if FAttachments.DepthStencilBuffer.Mode = bmTexture then
    DetachDepthStencilTexture;

  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glBindRenderbuffer(GL_RENDERBUFFER, 0);
  glDeleteFramebuffers(1, @FBOId);
  glDeleteFramebuffers(1, @FMSFBOId);
  FReadBackBuffers.Clear;
  with FAttachments do
  begin
    Textures.Clear;
    glDeleteRenderbuffers(1, @DepthBuffer.BuffId);
    glDeleteRenderbuffers(1, @StencilBuffer.BuffId);
    glDeleteRenderbuffers(1, @DepthStencilBuffer.BuffId);
  end;
  with FAttachments do
  begin
    if ResetConfig then
    begin
      DepthBuffer.Mode := bmNone;
      StencilBuffer.Mode := bmNone;
      DepthStencilBuffer.Mode := bmNone;
      FRenderBuffers := [];
    end;
    glGenRenderbuffers(1, @DepthBuffer.BuffId);
    glGenRenderbuffers(1, @StencilBuffer.BuffId);
    glGenRenderbuffers(1, @DepthStencilBuffer.BuffId);
  end;
  glGenFramebuffers(1, @FBOId);
  glGenFramebuffers(1, @FMSFBOId);
  FActive := false;
  FDeactivate := false;

end;

{ TGLUniformBlock }

constructor TGLUniformBlock.Create(const aProgram: cardinal; aIndex: integer);
begin
  assert(aProgram > 0, 'Shader program is not assigned');
  QueryUniformInfo(aProgram, aIndex);
end;

function TGLUniformBlock.getIndex(Index: integer): integer;
begin
  result := FubIndices[Index];
end;

function TGLUniformBlock.getMaxBC: integer;
begin
  result:=FMaxUBOSize div FBlockSize;
end;

function TGLUniformBlock.getName(Index: integer): ansistring;
begin
  result := FubNames[Index];
end;

function TGLUniformBlock.getOffset(Index: integer): integer;
begin
  result := FubOffsets[index];
end;

function TGLUniformBlock.OffsetByName(aName: ansistring): integer;
var
  i: integer;
begin
  for i := 0 to Length(FubNames) - 1 do
    if FubNames[i] = aName then
    begin
      result := FubOffsets[i];
      exit;
    end;
  result := -1;
end;

procedure TGLUniformBlock.QueryUniformInfo(aProgram: cardinal;
  aBlockIndex: integer);
var
  j: integer;
  len, bl: integer;
  cbuff: PAnsiChar;
  bt: TUniformBlocksType;
begin
  FBlockIndex := aBlockIndex;
  getmem(cbuff, 256);
  FBlockType := ubUnknown;
  glGetActiveUniformBlockiv(aProgram, aBlockIndex,
    GL_UNIFORM_BLOCK_NAME_LENGTH, @len);
  glGetActiveUniformBlockName(aProgram, aBlockIndex, 256, @len, cbuff);
  bl := len;
  FBlockName := copy(cbuff, 0, len);
  glGetActiveUniformBlockiv(aProgram, aBlockIndex, GL_UNIFORM_BLOCK_DATA_SIZE,
    @FBlockSize);
  glGetActiveUniformBlockiv(aProgram, aBlockIndex,
    GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS, @FUniformsCount);

  setlength(FubIndices, FUniformsCount);
  setlength(FubNames, FUniformsCount);
  setlength(FubOffsets, FUniformsCount);

  glGetActiveUniformBlockiv(aProgram, aBlockIndex,
    GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES, @FubIndices[0]);
  glGetActiveUniformsiv(aProgram, FUniformsCount, @FubIndices[0],
    GL_UNIFORM_OFFSET, @FubOffsets[0]);
  for j := 0 to FUniformsCount - 1 do
  begin
    glGetActiveUniformName(aProgram, FubIndices[j], 256, @len, cbuff);
    if bl <= 0 then
      FubNames[j] := copy(cbuff, 0, len)
    else
      FubNames[j] := copy(cbuff, bl + 2, len - bl - 1);
  end;
  for bt := Low(CUBOSemantics) to High(CUBOSemantics) do
    if FBlockName = CUBOSemantics[bt].Name then
      FBlockType := bt;
  FreeMem(cbuff, 256);
  glGetIntegerv(GL_MAX_UNIFORM_BLOCK_SIZE, @FMaxUBOSize);
end;

{ TUBOList }

function TUBOList.AddUBO(const aBlock: TGLUniformBlock): integer;
begin
  result := FItems.Add(aBlock);
end;

constructor TUBOList.Create;
begin
  inherited;
  FItems := TList.Create;
end;

destructor TUBOList.Destroy;
begin
  FreeObjectList(FItems);
  inherited;
end;

function TUBOList.getCount: integer;
begin
  result := FItems.Count;
end;

function TUBOList.getUBO(Index: integer): TGLUniformBlock;
begin
  result := FItems[Index];
end;

function TUBOList.GetUBOByName(const aName: ansistring): TGLUniformBlock;
var
  i: integer;
  ubo: TGLUniformBlock;
begin
  for i := 0 to FItems.Count - 1 do
  begin
    ubo := FItems[i];
    if ubo.BlockName = aName then
    begin
      result := ubo;
      exit;
    end;
  end;
  result := nil;
end;

{ TBufferObjectsPool }

function TGLBufferObjectsPool.BindUBO(const Index: integer;
  const aUBO: TGLUniformBlock): integer;
begin
  glBindBufferRange(GL_UNIFORM_BUFFER, CUBOSemantics[aUBO.BlockType].Location,
    FBuffer.Id, OffsetByIndex(Index), aUBO.BlockSize);
  result := CUBOSemantics[aUBO.BlockType].Location;
end;

constructor TGLBufferObjectsPool.Create(aObjectSize, aObjectsCount: integer);
var
  i: integer;
begin
  inherited Create;
  FUsedCount := 0;
  FBuffSize := aObjectsCount;
  FUBOSize := aObjectSize;
  FBuffer := TGLBufferObject.Create(btUniform);
  FBuffer.Allocate(aObjectSize * aObjectsCount, nil);
  FFreeRooms := TIntegerList.Create;
  FFreeRooms.Count := aObjectsCount;
  for i := 0 to aObjectsCount - 1 do
    FFreeRooms[i] := aObjectsCount - 1 - i;
  FStackTop := aObjectsCount - 1;
end;

destructor TGLBufferObjectsPool.Destroy;
begin
  FBuffer.Free;
  FFreeRooms.Free;
  inherited;
end;

procedure TGLBufferObjectsPool.FreeSlot(const Index: integer; aCheck: boolean);
var
  i: integer;
begin
  if aCheck then
  begin
    for i := 0 to FStackTop do
      if FFreeRooms[i] = index then
        exit;
  end;
  inc(FStackTop);
  FFreeRooms[FStackTop] := Index;
  dec(FUsedCount);
end;

function TGLBufferObjectsPool.OffsetByIndex(const Index: integer): integer;
begin
  if Index < FUsedCount then
    result := Index * FUBOSize
  else
    result := -1;
end;

procedure TGLBufferObjectsPool.UnBindUBO(const Index: integer;
  const aUBO: TGLUniformBlock);
begin
  glBindBufferBase(GL_UNIFORM_BUFFER, CUBOSemantics[aUBO.BlockType]
    .Location, 0);
end;

function TGLBufferObjectsPool.GetFreeSlotIndex: integer;
begin
  if FStackTop < 0 then
    result := -1
  else
  begin
    result := FFreeRooms[FStackTop] * FUBOSize;
    dec(FStackTop);
    inc(FUsedCount);
  end;
end;

function TGLBufferObjectsPool.isExists(const aData: pointer): integer;
var
  i, offs: integer;
  p: pointer;
begin
  for i := FStackTop + 1 to FBuffSize - 1 do
  begin
    offs := OffsetByIndex(FFreeRooms[i]);
    p := pointer(integer(FBuffer.Data) + offs);
    if CompareMem(p, aData, FUBOSize) then
    begin
      result := FFreeRooms[i];
      exit;
    end;
  end;
  result := -1;
end;

procedure TGLBufferObjectsPool.WriteToPool(const Index: integer; Data: pointer);
var
  p: pointer;
begin
  p := FBuffer.MapRange(GL_MAP_WRITE_BIT or GL_MAP_INVALIDATE_RANGE_BIT,
    OffsetByIndex(Index), FUBOSize);
  Move(Data^, p^, FUBOSize);
  FBuffer.UnMap;
end;

{ TGLTextureSampler }

procedure TGLTextureSampler.Bind(aUnit: cardinal);
begin
  if GL_ARB_sampler_objects then begin
    glBindSampler(aUnit, FSamplerId);
  end;
end;

procedure TGLTextureSampler.SetSamplerParams(aTarget: TTexTarget);
begin
  with FTextureSampler do begin
    glTexParameteri(CTexTargets[aTarget], GL_TEXTURE_WRAP_S, CWpars[WrapS]);
    glTexParameteri(CTexTargets[aTarget], GL_TEXTURE_WRAP_T, CWpars[WrapT]);
    glTexParameteri(CTexTargets[aTarget], GL_TEXTURE_WRAP_R, CWpars[WrapR]);

    glTexParameteri(CTexTargets[aTarget], GL_TEXTURE_MAG_FILTER, CMagFilters[magFilter]);
    glTexParameteri(CTexTargets[aTarget], GL_TEXTURE_MIN_FILTER, CMinFilters[minFilter]);

    glTexParameterf(CTexTargets[aTarget], GL_TEXTURE_MIN_LOD, MinLod);
    glTexParameterf(CTexTargets[aTarget], GL_TEXTURE_MAX_LOD, MaxLod);
    glTexParameterf(CTexTargets[aTarget], GL_TEXTURE_LOD_BIAS, LodBias);

    glTexParameteri(CTexTargets[aTarget], GL_TEXTURE_COMPARE_MODE, CCompareMode[CompareMode]);
    glTexParameteri(CTexTargets[aTarget], GL_TEXTURE_COMPARE_FUNC, CCompareFunc[CompareFunc]);

    if FTextureSampler.UseTexGen and GL_ARB_multitexture then begin
      if TextureGenS = tgDisable then glDisable(GL_TEXTURE_GEN_S)
      else begin
        glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, CTextureGen[TextureGenS]);
        glEnable(GL_TEXTURE_GEN_S);
      end;
      if TextureGenT = tgDisable then glDisable(GL_TEXTURE_GEN_T)
      else begin
        glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, CTextureGen[TextureGenT]);
        glDisable(GL_TEXTURE_GEN_T);
      end;
      if TextureGenR = tgDisable then glDisable(GL_TEXTURE_GEN_R)
      else begin
        glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, CTextureGen[TextureGenR]);
        glDisable(GL_TEXTURE_GEN_R);
      end;
      glDisable(GL_TEXTURE_GEN_Q)
    end;
  end;
end;

constructor TGLTextureSampler.Create;
begin
  inherited;
  FSamplerId := 0;
  CreateFrom(TTextureSampler.Create);
  FTextureSampler.Owner:=self;
end;

constructor TGLTextureSampler.CreateFrom(aSampler: TTextureSampler);
begin
  FTextureSampler := aSampler;
  FSamplerId := 0;
  if GL_ARB_sampler_objects then begin
    glGenSamplers(1, @FSamplerId);
    with FTextureSampler do begin
      glSamplerParameteri(FSamplerId, GL_TEXTURE_WRAP_S, CWpars[WrapS]);
      glSamplerParameteri(FSamplerId, GL_TEXTURE_WRAP_T, CWpars[WrapT]);
      glSamplerParameteri(FSamplerId, GL_TEXTURE_WRAP_R, CWpars[WrapR]);

      glSamplerParameteri(FSamplerId, GL_TEXTURE_MAG_FILTER, CMagFilters[magFilter]);
      glSamplerParameteri(FSamplerId, GL_TEXTURE_MIN_FILTER, CMinFilters[minFilter]);

      glSamplerParameterf(FSamplerId, GL_TEXTURE_MIN_LOD, MinLod);
      glSamplerParameterf(FSamplerId, GL_TEXTURE_MAX_LOD, MaxLod);
      glSamplerParameterf(FSamplerId, GL_TEXTURE_LOD_BIAS, LodBias);

      glSamplerParameterf(FSamplerId, GL_TEXTURE_COMPARE_MODE, CCompareMode[CompareMode]);
      glSamplerParameterf(FSamplerId, GL_TEXTURE_COMPARE_FUNC, CCompareFunc[CompareFunc]);

      if GL_EXT_texture_filter_anisotropic then begin
        glSamplerParameterf(FSamplerId, GL_TEXTURE_MAX_ANISOTROPY_EXT, AnisotropyLevel);
      end;
    end;
  end;
end;

destructor TGLTextureSampler.Destroy;
begin
  if FTextureSampler.Owner = self then FTextureSampler.Free;
  if FSamplerId > 0 then glDeleteSamplers(1, @FSamplerId);
  inherited;
end;

function TGLTextureSampler.getSamplerHash: integer;
begin
  result:=FTextureSampler.SamplerHash;
end;

procedure TGLTextureSampler.UnBind(aUnit: cardinal);
begin
  if GL_ARB_sampler_objects then glBindSampler(aUnit,FSamplerId);
end;

{ TGLTextureFormat }

class function TGLTextureFormatSelector.CreateCompressed(
  aFormat: TS3TCCompressedFormats): TGLTextureFormatDescriptor;
begin
  result.Compressed:=true; result.BaseFormat := 0;
  result.PixelFormat := 0; result.InternalFormat := 0;
  case aFormat of
    cfRGB_DXT1: result.InternalFormat := GL_COMPRESSED_RGB_S3TC_DXT1_EXT;
    cfSRGB_DXT1: result.InternalFormat := GL_COMPRESSED_SRGB_S3TC_DXT1_EXT;
    cfRGBA_DXT1: result.InternalFormat := GL_COMPRESSED_RGBA_S3TC_DXT1_EXT;
    cfSRGBA_DXT1: result.InternalFormat := GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT;
    cfRGBA_DXT3: result.InternalFormat := GL_COMPRESSED_RGBA_S3TC_DXT3_EXT;
    cfSRGBA_DXT3: result.InternalFormat := GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT;
    cfRGBA_DXT5: result.InternalFormat := GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;
    cfSRGBA_DXT5: result.InternalFormat := GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT;
    else assert(false, 'Unsupported pixel format, try another selector');
  end;
end;

class function TGLTextureFormatSelector.CreateDepthStencil(aDepthBit: byte;
  aStencil: boolean): TGLTextureFormatDescriptor;
begin
  result.Compressed := false;
  if not aStencil then begin
    result.BaseFormat := GL_DEPTH_COMPONENT;
    case aDepthBit of
      16: begin
        result.InternalFormat := GL_DEPTH_COMPONENT16;
        result.PixelFormat := GL_UNSIGNED_SHORT;
      end;
      24: begin
        result.InternalFormat := GL_DEPTH_COMPONENT24;
        result.PixelFormat := GL_UNSIGNED_INT;
      end;
      32: begin
        result.InternalFormat := GL_DEPTH_COMPONENT32;
        result.PixelFormat := GL_UNSIGNED_INT;
      end;
      else assert(false, 'Unsupported Depth format!');
    end;
  end else begin
    result.BaseFormat := GL_DEPTH_STENCIL;
    case aDepthBit of
       0: begin
        result.BaseFormat := GL_STENCIL;
        result.InternalFormat := GL_STENCIL_INDEX8;
        result.PixelFormat := GL_UNSIGNED_BYTE;
       end;
      24: begin
        result.InternalFormat := GL_DEPTH24_STENCIL8;
        result.PixelFormat := GL_UNSIGNED_INT_24_8;
      end;
      32: begin
        result.InternalFormat := GL_DEPTH32F_STENCIL8;
        result.PixelFormat := GL_FLOAT_32_UNSIGNED_INT_24_8_REV;
      end;
      else assert(false, 'Unsupported DepthStencil format!');
    end;
  end;
end;

class function TGLTextureFormatSelector.CreateSpecial(
  aFormat: TImageSpecialFormat): TGLTextureFormatDescriptor;
begin
  result.Compressed := false;
  case aFormat of
    sfR3G3B2: begin
        result.BaseFormat := GL_RGB;
        result.InternalFormat := GL_R3_G3_B2;
        result.PixelFormat := GL_UNSIGNED_BYTE_3_3_2;
      end;
    sfRGB565: begin
        result.BaseFormat := GL_RGB;
        result.InternalFormat := GL_RGB565;
        result.PixelFormat := GL_UNSIGNED_SHORT_5_6_5;
      end;
    sfRGB5A1: begin
        result.BaseFormat := GL_RGBA;
        result.InternalFormat := GL_RGB5_A1;
        result.PixelFormat := GL_UNSIGNED_SHORT_5_5_5_1;
      end;
    sfRGB10A2: begin
        result.BaseFormat := GL_RGBA;
        result.InternalFormat := GL_RGB10_A2;
        result.PixelFormat := GL_UNSIGNED_INT_10_10_10_2;
      end;
    sfRGB10A2UI: begin
        result.BaseFormat := GL_RGBA;
        result.InternalFormat := GL_RGB10_A2UI;
        result.PixelFormat := GL_UNSIGNED_INT_10_10_10_2;
      end;
    sfR11FG11FB10F: begin
        result.BaseFormat := GL_RGB;
        result.InternalFormat := GL_R11F_G11F_B10F;
        result.PixelFormat := GL_UNSIGNED_INT_10F_11F_11F_REV;
      end;
    sfRGB9E5: begin
        result.BaseFormat := GL_RGB;
        result.InternalFormat := GL_RGB9_E5;
        result.PixelFormat := GL_UNSIGNED_INT_5_9_9_9_REV;
      end;
    else assert(false, 'Unsupported image format, try another selector');
  end;
end;

class function TGLTextureFormatSelector.CreateFloat16(aFormat: TBaseImageFormat): TGLTextureFormatDescriptor;
begin
  result.PixelFormat := GL_HALF_FLOAT; result.Compressed:=false;
  with result do
  case aFormat of
    bfRed: begin InternalFormat := GL_R16F; BaseFormat := GL_RED; end;
    bfRG: begin InternalFormat := GL_RG16F; BaseFormat := GL_RG; end;
    bfRGB: begin InternalFormat := GL_RGB16F; BaseFormat := GL_RGB; end;
    bfBGR: begin InternalFormat := GL_RGB16F; BaseFormat := GL_BGR; end;
    bfRGBA: begin InternalFormat := GL_RGBA16F; BaseFormat := GL_RGBA; end;
    bfBGRA: begin InternalFormat := GL_RGBA16F; BaseFormat := GL_BGRA; end;
    else assert(false, 'Unsupported pixel format, try another selector');
  end;
end;

class function TGLTextureFormatSelector.CreateFloat32(aFormat: TBaseImageFormat): TGLTextureFormatDescriptor;
begin
  result.PixelFormat := GL_FLOAT; result.Compressed:=false;
  with result do
  case aFormat of
    bfRed: begin InternalFormat := GL_R32F; BaseFormat := GL_RED; end;
    bfRG: begin InternalFormat := GL_RG32F; BaseFormat := GL_RG; end;
    bfRGB: begin InternalFormat := GL_RGB32F; BaseFormat := GL_RGB; end;
    bfBGR: begin InternalFormat := GL_RGB32F; BaseFormat := GL_BGR; end;
    bfRGBA: begin InternalFormat := GL_RGBA32F; BaseFormat := GL_RGBA; end;
    bfBGRA: begin InternalFormat := GL_RGBA32F; BaseFormat := GL_BGRA; end;
    else assert(false, 'Unsupported pixel format, try another selector');
  end;
end;

class function TGLTextureFormatSelector.CreateInt16(aFormat: TBaseImageFormat): TGLTextureFormatDescriptor;
begin
  result.PixelFormat := GL_SHORT; result.Compressed:=false;
  with result do
  case aFormat of
    bfRed: begin InternalFormat := GL_R16I; BaseFormat := GL_RED; end;
    bfRG: begin InternalFormat := GL_RG16I; BaseFormat := GL_RG; end;
    bfRGB: begin InternalFormat := GL_RGB16I; BaseFormat := GL_RGB; end;
    bfBGR: begin InternalFormat := GL_RGB16I; BaseFormat := GL_BGR; end;
    bfRGBA: begin InternalFormat := GL_RGBA16I; BaseFormat := GL_RGBA; end;
    bfBGRA: begin InternalFormat := GL_RGBA16I; BaseFormat := GL_BGRA; end;
    else assert(false, 'Unsupported pixel format, try another selector');
  end;
end;

class function TGLTextureFormatSelector.CreateInt32(aFormat: TBaseImageFormat): TGLTextureFormatDescriptor;
begin
  result.PixelFormat := GL_INT; result.Compressed:=false;
  with result do
  case aFormat of
    bfRed: begin InternalFormat := GL_R32I; BaseFormat := GL_RED; end;
    bfRG: begin InternalFormat := GL_RG32I; BaseFormat := GL_RG; end;
    bfRGB: begin InternalFormat := GL_RGB32I; BaseFormat := GL_RGB; end;
    bfBGR: begin InternalFormat := GL_RGB32I; BaseFormat := GL_BGR; end;
    bfRGBA: begin InternalFormat := GL_RGBA32I; BaseFormat := GL_RGBA; end;
    bfBGRA: begin InternalFormat := GL_RGBA32I; BaseFormat := GL_BGRA; end;
    else assert(false, 'Unsupported pixel format, try another selector');
  end;
end;

class function TGLTextureFormatSelector.CreateInt8(aFormat: TBaseImageFormat): TGLTextureFormatDescriptor;
begin
  result.PixelFormat := GL_BYTE; result.Compressed:=false;
  with result do
  case aFormat of
    bfRed: begin InternalFormat := GL_R8; BaseFormat := GL_RED; end;
    bfRG: begin InternalFormat := GL_RG8; BaseFormat := GL_RG; end;
    bfRGB: begin InternalFormat := GL_RGB8; BaseFormat := GL_RGB; end;
    bfBGR: begin InternalFormat := GL_RGB8; BaseFormat := GL_BGR; end;
    bfRGBA: begin InternalFormat := GL_RGBA8; BaseFormat := GL_RGBA; end;
    bfBGRA: begin InternalFormat := GL_RGBA8; BaseFormat := GL_BGRA; end;
    else assert(false, 'Unsupported pixel format, try another selector');
  end;
end;

class function TGLTextureFormatSelector.CreateUInt16(aFormat: TBaseImageFormat): TGLTextureFormatDescriptor;
begin
  result.PixelFormat := GL_UNSIGNED_SHORT; result.Compressed:=false;
  with result do
  case aFormat of
    bfRed: begin InternalFormat := GL_R16UI; BaseFormat := GL_RED; end;
    bfRG: begin InternalFormat := GL_RG16UI; BaseFormat := GL_RG; end;
    bfRGB: begin InternalFormat := GL_RGB16UI; BaseFormat := GL_RGB; end;
    bfBGR: begin InternalFormat := GL_RGB16UI; BaseFormat := GL_BGR; end;
    bfRGBA: begin InternalFormat := GL_RGBA16UI; BaseFormat := GL_RGBA; end;
    bfBGRA: begin InternalFormat := GL_RGBA16UI; BaseFormat := GL_BGRA; end;
    else assert(false, 'Unsupported pixel format, try another selector');
  end;
end;

class function TGLTextureFormatSelector.CreateUInt32(aFormat: TBaseImageFormat): TGLTextureFormatDescriptor;
begin
  result.PixelFormat := GL_UNSIGNED_INT; result.Compressed:=false;
  with result do
  case aFormat of
    bfRed: begin InternalFormat := GL_R32UI; BaseFormat := GL_RED; end;
    bfRG: begin InternalFormat := GL_RG32UI; BaseFormat := GL_RG; end;
    bfRGB: begin InternalFormat := GL_RGB32UI; BaseFormat := GL_RGB; end;
    bfBGR: begin InternalFormat := GL_RGB32UI; BaseFormat := GL_BGR; end;
    bfRGBA: begin InternalFormat := GL_RGBA32UI; BaseFormat := GL_RGBA; end;
    bfBGRA: begin InternalFormat := GL_RGBA32UI; BaseFormat := GL_BGRA; end;
    else assert(false, 'Unsupported pixel format, try another selector');
  end;
end;

class function TGLTextureFormatSelector.CreateUInt8(aFormat: TBaseImageFormat): TGLTextureFormatDescriptor;
begin
  result.PixelFormat := GL_UNSIGNED_BYTE; result.Compressed:=false;
  with result do
  case aFormat of
    bfRed: begin InternalFormat := GL_R8UI; BaseFormat := GL_RED; end;
    bfRG: begin InternalFormat := GL_RG8UI; BaseFormat := GL_RG; end;
    bfRGB: begin InternalFormat := GL_RGB8UI; BaseFormat := GL_RGB; end;
    bfBGR: begin InternalFormat := GL_RGB8UI; BaseFormat := GL_BGR; end;
    bfRGBA: begin InternalFormat := GL_RGBA8UI; BaseFormat := GL_RGBA; end;
    bfBGRA: begin InternalFormat := GL_RGBA8UI; BaseFormat := GL_BGRA; end;
    else assert(false, 'Unsupported pixel format, try another selector');
  end;
end;

class function TGLTextureFormatSelector.GetTextureFormat(
  aFormat: cardinal): TGLTextureFormatDescriptor;
var bFormat: TBaseImageFormat;
    dFormat: TDepthStencilFormat;
    cFormat: TS3TCCompressedFormats;
    sFormat: TImageSpecialFormat;
    pFormat: TImagePixelFormat;
begin
  bFormat := TImageFormatBits.GetBaseFormat(aFormat);
  if bFormat in [bfRed..bfBGRA] then begin
    pFormat := TImageFormatBits.GetPixelFormat(aFormat);
    case pFormat of
      pfUByte: result := CreateUInt8(bFormat);
      pfByte: result := CreateInt8(bFormat);
      pfUShort: result := CreateUInt16(bFormat);
      pfShort: result := CreateInt16(bFormat);
      pfUInt: result := CreateUInt32(bFormat);
      pfInt: result := CreateInt32(bFormat);
      pfFloat16: result := CreateFloat16(bFormat);
      pfFloat: result := CreateFloat32(bFormat);
    end;
  end else
    if bFormat in [bfDepth, bfDepthStencil] then begin
      dFormat := TImageFormatBits.GetDepthStencilFormat(aFormat);
      case dFormat of
        dfDepth16: result := CreateDepthStencil(16, false);
        dfDepth24: result := CreateDepthStencil(24, false);
        dfDepth32: result := CreateDepthStencil(32, false);
        dfDepth24Stencil8: result := CreateDepthStencil(24, true);
        dfDepth32FStencil8: result := CreateDepthStencil(32, true);
        dfStencilIndex8: result := CreateDepthStencil(0, true);
      end;
    end else
      if bFormat = bfCompressed then begin
        cFormat := TImageFormatBits.GetCompressedFormat(aFormat);
        result :=CreateCompressed(cFormat);
      end else
        if bFormat = bfSpecial then begin
          sFormat := TImageFormatBits.GetSpecialFormat(aFormat);
          result := CreateSpecial(sFormat);
        end else assert(false, 'Unsupported format!');
end;

initialization

vActiveShader := nil;

finalization

vActiveShader := nil;

end.
