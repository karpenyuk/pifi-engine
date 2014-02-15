{ TODO :
  - Добавить манипуляцию мешами через скелетку:
  MeshObjectList.Hierarchy - связь между отдельными MeshObject через установку
  матриц трансформации их корневых костей.
  MeshObject.Hierarchy- связь между отдельными Mesh через установку
  их локальных матриц на основе корневой кости MeshObject.
  Разработать класс Hierarchy, каждый нод это ссылка на родительскую кость,
  имя/GUID кости и ее трансформации (локальные, глобальные, кватернион/матрица)

  - Реализовать TVertexObject.LineSegmentation
}
unit uRenderResource;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, uLists, uVMath, uMiscUtils, uBaseTypes, uBaseClasses,
  uGenericsRBTree, uPersistentClasses, uDataAccess, uImageFormats;

const
  cDiffuseColor: vec4 = (0.8, 0.8, 0.8, 1);
  cAmbientColor: vec4 = (0.2, 0.2, 0.2, 1);
  cSpecularColor: vec4 = (0, 0, 0, 1);
  cEmissiveColor: vec4 = (0, 0, 0, 1);
  cShininess: integer = 127;

Type

  TRegisteredResource = class(TNotifiableObject)
  private
    FResource: TList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure RegisterResource(const aResource: TBaseRenderResource);
    procedure UnRegisterResource(const aResource: TBaseRenderResource);
  end;

  TShaderProgram = class;

  TUniformInfo = record
    Shader: cardinal;
    Index: integer;
    Name: ansistring;
    Size: integer;
    ValueType: cardinal;
  end;
  PUniformInfo = ^TUniformInfo;

  TBaseBuiltinUniform = class;
  TBaseBuiltinUniformClass = class of TBaseBuiltinUniform;

  TShaderProgram = class(TBaseRenderResource)
  private
    FShaderText: array [stVertex .. stCompute] of ansistring;
    FBinary: TBinaryData;
    FFragDataBindPos: TAttribSemantic;
    FAttribsBindPos: array of TAttribSemantic;
    FBuildinUniforms: TObjectList;

    function getShaderText(ShaderType: TShaderType): ansistring;
    procedure setShaderText(ShaderType: TShaderType; const Value: ansistring);
    function getShaderBinary: TBinaryData;
    procedure setShaderBinary(const Value: TBinaryData);
    function getAttr(Index: integer): TAttribSemantic;
    function getAttrCount: integer;
  public
    Name: string;

    procedure AddBuildinUniform(anUniform: TBaseBuiltinUniform);

    function SetAttribBindPos(const aName: ansistring;
      const aLocation: integer): integer;

    property ShaderText[ShaderType: TShaderType]: ansistring read getShaderText
      write setShaderText; default;
    property Binary: TBinaryData read getShaderBinary write setShaderBinary;

    property FragDataBindPos: TAttribSemantic read FFragDataBindPos
      write FFragDataBindPos;
    property AttribBinds[Index: integer]: TAttribSemantic read getAttr;
    property AttribBindsCount: integer read getAttrCount;

    property BuildinUniforms: TObjectList read FBuildinUniforms;

    constructor CreateOwned(aOwner: TObject = nil); override;
    destructor Destroy; override;
  end;

  // Base class of automatic uniform setup
  TBaseBuiltinUniform = class(TBaseRenderResource)
  public
    class function Name: ansistring; virtual; abstract;
    class function IsInner: boolean; override;
  end;

  TBuiltinUniformLightNumber = class(TBaseBuiltinUniform)
  public
    class function Name: ansistring; override;
  end;


  TUniformObject<T> = class(TBaseRenderResource);

  TFloatUniform = TUniformObject<single>;
  TVec2Uniform = TUniformObject<vec2>;
  TVec3Uniform = TUniformObject<vec3>;
  TVec4Uniform = TUniformObject<vec4>;
  TIntUniform = TUniformObject<integer>;
  TInt2Uniform = TUniformObject<vec2i>;
  TInt3Uniform = TUniformObject<vec3i>;
  TInt4Uniform = TUniformObject<vec4i>;
  TMat2Uniform = TUniformObject<mat2>;
  TMat3Uniform = TUniformObject<mat3>;
  TMat4Uniform = TUniformObject<mat4>;

  TColorVectorClass = class
  private
    FColorVector: TVector;
    FColorRec: TColor;
    FColorKey: integer;
    procedure RecalcHashKey;
    procedure setAlpha(const Value: single);
    procedure setBlue(const Value: single);
    procedure setGreen(const Value: single);
    procedure setRed(const Value: single);

    function getVecAddr: Pointer;
    procedure SetColorVector(const Value: TVector);
    procedure SetColorRec(const Value: TColor);
    function getIntColorVect: Vec4i;
    procedure setIntColorVect(const Value: Vec4i);
    function getAlpha: single;
    function getBValue: single;
    function getGValue: single;
    function getRValue: single;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Color: TColorVectorClass);

    procedure SetColor(r, g, b, a: byte); overload;
    procedure SetColor(r, g, b, a: single); overload;

    property HasKey: integer read FColorKey;

    property ColorRec: TColor read FColorRec write SetColorRec;
    property ColorVector: TVector read FColorVector write SetColorVector;
    property ColorVector4i: Vec4i read getIntColorVect write setIntColorVect;
    property ColorAsAddress: Pointer read getVecAddr;

    property Red: single read getRValue write setRed;
    property Green: single read getGValue write setGreen;
    property Blue: single read getBValue write setBlue;
    property Alpha: single read getAlpha write setAlpha;
  end;

  TLightSource = class(TMovableObject)
  private
    FEnabled: boolean;
    FLightStyle: TLightStyle;
    FLightModel: TLightModels;
    FSpotDirection: TVector;
    FSpotCutOff: single;
    FSpotExponent: single;
    FAmbient: TColorVectorClass;
    FDiffuse: TColorVectorClass;
    FSpecular: TColorVectorClass;
    FSceneColor: TColorVectorClass;
    FConstAttenuation: single;
    FLinearAttenuation: single;
    FQuadraticAttenuation: single;
    FLightSlot: integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    property SpotDirection: TVector read FSpotDirection write FSpotDirection;
    property LightStyle: TLightStyle read FLightStyle write FLightStyle;
    property LightModel: TLightModels read FLightModel write FLightModel;
    property SpotCutOff: single read FSpotCutOff write FSpotCutOff;
    property SpotExponent: single read FSpotExponent write FSpotExponent;
    property Ambient: TColorVectorClass read FAmbient;
    property Diffuse: TColorVectorClass read FDiffuse;
    property Specular: TColorVectorClass read FSpecular;
    property SceneColor: TColorVectorClass read FSceneColor write FSceneColor;
    property ConstAttenuation: single read FConstAttenuation
      write FConstAttenuation;
    property LinearAttenuation: single read FLinearAttenuation
      write FLinearAttenuation;
    property QuadraticAttenuation: single read FQuadraticAttenuation
      write FQuadraticAttenuation;
    property LightSlot: integer read FLightSlot write FLightSlot;

    property Enabled: boolean read FEnabled write FEnabled;
  end;

  TLightsList = class (TObjectsDictionary)
  private
    function getItemObj(index: integer): TLightSource;
  public
    destructor Destroy; override;

    function AddLight(const aItem: TLightSource): integer;
    function GetLight(aKey: TGUID): TLightSource; overload;
    function GetLight(aName: string): TLightSource; overload;

    procedure RemoveLight(const aItem: TLightSource);

    property Lights[index: integer]: TLightSource read getItemObj; default;
  end;

  TMaterialProperties = class
  private
    FDiffuseColor: TColorVectorClass;
    FAmbientColor: TColorVectorClass;
    FSpecularColor: TColorVectorClass;
    FEmissionColor: TColorVectorClass;
    FShininess: single;
    FName: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Material: TMaterialProperties); overload;
    function IsEqual(MatProp: TMaterialProperties): boolean;

    property DiffuseColor: TColorVectorClass read FDiffuseColor
      write FDiffuseColor stored true;
    property AmbientColor: TColorVectorClass read FAmbientColor
      write FAmbientColor stored true;
    property SpecularColor: TColorVectorClass read FSpecularColor
      write FSpecularColor stored true;
    property EmissionColor: TColorVectorClass read FEmissionColor
      write FEmissionColor stored true;
    property Shininess: single read FShininess write FShininess stored true;
    property Name: string read FName write FName;
  end;

  TMaterial = class(TBaseRenderResource)
  private
    FMaterialProperties: TMaterialProperties;
    FLightProperties: TLightSource;
    FColorReplacing: TColorReplacing;
    FMaterialType: TMaterialType;
    FUseMaterial: boolean;
    FName: string;
    FNameHash: integer;
    procedure SetName(const Value: string);
    function GetHashName(const Name: string): integer;
  public
    constructor CreateOwned(aOwner: TObject = nil); override;
    destructor Destroy; override;

    property Light: TLightSource read FLightProperties;
    property Properties: TMaterialProperties read FMaterialProperties;
    property ColorReplacing: TColorReplacing read FColorReplacing
      write FColorReplacing;
    property MaterialType: TMaterialType read FMaterialType write FMaterialType;
    property Name: string read FName write SetName;
    property UseMaterial: boolean read FUseMaterial write FUseMaterial;
  end;

  TTextureSampler = class(TBaseRenderResource)
  private
    FUpdates: TTextureUpdates;
    FTextureDescriptor: TTextureDesc;
    FUseTexGen: boolean;

    function getTexGenR: TTexGens;
    function getTexGenS: TTexGens;
    function getTexGenT: TTexGens;
    function getWrapR: TTextureWraps;
    function getWrapS: TTextureWraps;
    function getWrapT: TTextureWraps;
    procedure setMagFilter(const Value: TMagFilter);
    procedure setMinFilter(const Value: TMinFilter);
    procedure setTexGenR(const Value: TTexGens);
    procedure setTexGenS(const Value: TTexGens);
    procedure setTexGenT(const Value: TTexGens);
    procedure setWrapR(const Value: TTextureWraps);
    procedure setWrapS(const Value: TTextureWraps);
    procedure setWrapT(const Value: TTextureWraps);
    function getMagFilter: TMagFilter;
    function getMinFilter: TMinFilter;
    function getMinLod: single;
    procedure setMinLod(const Value: single);
    function getMaxLod: single;
    procedure setMaxLod(const Value: single);
    function getLodBias: single;
    procedure setLodBias(const Value: single);
    function getCFunc: TTextureCompareFunc;
    function getCMode: TTextureCompareMode;
    procedure setCFunc(const Value: TTextureCompareFunc);
    procedure setCMode(const Value: TTextureCompareMode);
    function getAnisotropyLevel: single;
    procedure SetAnisotropyLevel(const Value: single);

    function getTexDescr: PTextureDesc;
    function getSamplerHash: integer;
  public
    constructor Create; override;
    class function IsInner: boolean; override;
    // Texture Descriptors
    property WrapS: TTextureWraps read getWrapS write setWrapS;
    property WrapT: TTextureWraps read getWrapT write setWrapT;
    property WrapR: TTextureWraps read getWrapR write setWrapR;
    property minFilter: TMinFilter read getMinFilter write setMinFilter;
    property magFilter: TMagFilter read getMagFilter write setMagFilter;
    property TextureGenS: TTexGens read getTexGenS write setTexGenS;
    property TextureGenT: TTexGens read getTexGenT write setTexGenT;
    property TextureGenR: TTexGens read getTexGenR write setTexGenR;
    property MinLod: single read getMinLod write setMinLod;
    property MaxLod: single read getMaxLod write setMaxLod;
    property LodBias: single read getLodBias write setLodBias;
    property CompareMode: TTextureCompareMode read getCMode write setCMode;
    property CompareFunc: TTextureCompareFunc read getCFunc write setCFunc;
    property AnisotropyLevel: single read getAnisotropyLevel
      write SetAnisotropyLevel;

    property UseTexGen: boolean read FUseTexGen write FUseTexGen;
    property TextureDescriptor: PTextureDesc read getTexDescr;
    property SamplerHash: integer read getSamplerHash;
  end;

  TTexture = class;
  { TODO : TImageHolder: Check logic for texture arrays }
  TImageLayers = TDataList<PImageLods>;
  TImageHolder = class(TBaseRenderResource)
  protected
    FImageFormat: cardinal;
    FImageType: TImageType;

    FElementSize: integer;
    FDataSize: integer;
    FReservedMem: integer;
    FData: pointer;
    FWidth, FHeight, FDepth: integer;
    FLevels: integer;

    FLODS: TImageLods;
    FCompressed: boolean;

    FLayers: TImageLayers;

    procedure Deallocate;
    procedure FillLodsStructure(aFormatCode: cardinal; aWidth, aHeight, aDepth: integer; aArray: boolean);
    procedure FillZeroLod(aFormatCode: cardinal; aWidth, aHeight, aDepth: integer; aArray: boolean);
    procedure setImageFormat(const Value: cardinal);
  private
    function getDataSize: integer;
    function getImageLod(Index: integer): PImageLevelDesc;

    procedure setDepth(const Value: integer);
    procedure setHeight(const Value: integer);
    procedure setWidth(const Value: integer);

    procedure setImageType(const Value: TImageType);
    function getBitmapState: boolean;
    function getCubeMapState: boolean;
    function getTextureArrayState: boolean;
    function getVolumeState: boolean;

  public
    constructor Create; overload; override;
    constructor Create(aFormatCode: cardinal; aImageType: TImageType = itBitmap); overload;
    constructor CreateFromStream(aStream: TStream); virtual;
    destructor Destroy; override;

    function CreateTexture: TTexture;

    procedure Allocate(aWithMipmaps: boolean = false);
    procedure SaveToStream(aStream: TStream); virtual;
    procedure LoadFromStream(aStream: TStream); override;
    //virtual function for implementing different image format loader
    procedure LoadImageFromStream(aStream: TStream); virtual;
    procedure LoadImageFromFile(aFileName: string);
    //save image to stream/file, image format settet by extention: bmp, jpg, tga, dds etc.
    procedure SaveImageToStream(aStream: TStream; ImageFormat: string = ''); virtual;
    procedure SaveImageToFile(aFileName: string; ImageFormat: string = '');

    procedure DiscardLods;
    procedure VolumeToArray;

    property ImageFormat: cardinal read FImageFormat write setImageFormat;
    property ImageType: TImageType read FImageType write setImageType;

    property DataSize: integer read getDataSize;
    property ElementSize: integer read FElementSize;
    property Data: pointer read FData;
    property Width: integer read FWidth write setWidth;
    property Height: integer read FHeight write setHeight;
    property Depth: integer read FDepth write setDepth;
    property LevelsCount: integer read FLevels;
    property LODS[Index: integer]: PImageLevelDesc read getImageLod;

    property Compressed: boolean read FCompressed;
    property isBitmap: boolean read getBitmapState;
    property isCubeMap: boolean read getCubeMapState;
    property isTextureArray: boolean read getTextureArrayState;
    property isVolume: boolean read getVolumeState;
  end;

  TImageSampler = class(TBaseRenderResource)
  public
    class function CreateBitmap(aFormatCode: cardinal; aWidth, aHeight: integer; aMipmapping: boolean = false): TImageHolder;
    class function CreateBitmapArray(aFormatCode: cardinal; aWidth, aHeight, aDepth: integer; aMipmapping: boolean = false): TImageHolder;
    class function CreateVolume(aFormatCode: cardinal; aWidth, aHeight, aDepth: integer; aMipmapping: boolean = false): TImageHolder;
    class function CreateCubeMap(aFormatCode: cardinal; aWidth, aHeight: integer; aMipmapping: boolean = false): TImageHolder;
    class function CreateCubeMapArray(aFormatCode: cardinal; aWidth, aHeight, aDepth: integer; aMipmapping: boolean = false): TImageHolder;

    //Presets

    class function CreateRGBA8Texture2D(aWidth, aHeight: integer; aMipmapping: boolean = false): TImageHolder;
    class function CreateRGBA32fTexture2D(aWidth, aHeight: integer; aMipmapping: boolean = false): TImageHolder;
    class function CreateFloat32Texture2D(aWidth, aHeight: integer; aMipmapping: boolean = false): TImageHolder;
    class function CreateDepth32Texture(aWidth, aHeight: integer; aMipmapping: boolean = false): TImageHolder;
    class function CreateDepthStencilTexture(aWidth, aHeight: integer; aMipmapping: boolean = false): TImageHolder;

  end;

  TTexture = class(TBaseRenderResource)
  private
    FImageDescriptor: TImageHolder;
    FReady: boolean;
    FName: string;
    FUpdates: TTextureUpdates;
    FDisabled: boolean;
    FMapTargets: TMapTargets;
    FTextureMode: TTextureMode;
    FTexMatrix: TMatrix;
    FTexMatrixChanged: boolean;
    FTwoSides: boolean;
    FTarget: TTexTarget;
    FGenerateMipMaps: boolean;

    function getTarget: TTexTarget;
    procedure SetName(const Value: string);
    procedure setTexMatrix(const Value: TMatrix);

    procedure setGenMipMaps(const Value: boolean);
    function getImageFormat: TImageFormat;

  public

    constructor CreateOwned(aImageHolder: TImageHolder; aOwner: TObject = nil);
    class function IsInner: boolean; override;

    property ImageHolder: TImageHolder read FImageDescriptor;
    property ImageFormat: TImageFormat read getImageFormat;
    property Updates: TTextureUpdates read FUpdates write FUpdates;

    property Disabled: boolean read FDisabled write FDisabled;
    property Target: TTexTarget read getTarget;
    property Name: string read FName write SetName;
    property MapTargets: TMapTargets read FMapTargets write FMapTargets;
    property TextureMode: TTextureMode read FTextureMode write FTextureMode;
    property Matrix: TMatrix read FTexMatrix write setTexMatrix;
    property MatrixChanged: boolean read FTexMatrixChanged;
    property TwoSides: boolean read FTwoSides write FTwoSides;
    property GenerateMipMaps: boolean read FGenerateMipMaps write setGenMipMaps;
  end;

  TCustomBlending = class(TBaseRenderResource)
  private
    FBlendEnable: boolean;
    FAlphaTestEnable: boolean;
    FSrcBlendFunc: TSBlendFactor;
    FDstBlendFunc: TDBlendFactor;
    FAlphaFunc: TAlphaFunc;
    FAlphaThreshold: single;
    FHashKey: integer;
    FBlendingMode: TBlendingModes;
    function GetHash: integer;
    procedure SetBlend(const Value: boolean);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetByMode(aBlendingMode: TBlendingModes);
    procedure Assign(CustomBlending: TCustomBlending);

    property BlendEnable: boolean read FBlendEnable write SetBlend;
    property AlphaTestEnable: boolean read FAlphaTestEnable
      write FAlphaTestEnable;
    property SrcBlendFunc: TSBlendFactor read FSrcBlendFunc write FSrcBlendFunc;
    property DstBlendFunc: TDBlendFactor read FDstBlendFunc write FDstBlendFunc;
    property AlphaFunc: TAlphaFunc read FAlphaFunc write FAlphaFunc;
    property AlphaThreshold: single read FAlphaThreshold write FAlphaThreshold;
    property BlendingMode: TBlendingModes read FBlendingMode;

    property HashKey: integer read GetHash;
  end;

  TMaterialObject = class(TBaseRenderResource)
  private
    FActive: boolean;
    FBlending: TCustomBlending;
    FTexture: TTexture;
    FSampler: TTextureSampler;
    FMaterial: TMaterial;
    FShader: TShaderProgram;
    FspId: cardinal;
    FHashKey: integer;
    FUseTexture: boolean;
    FUseMaterial: boolean;
    FUseShader: boolean;
    FTwoSideLighting: boolean;
    FIgnoreLighting: boolean;
    FName: string;
    FNameChanged: boolean;
    FAdditionalTextures: array of TTexture;
    FUseAddTex: boolean;
    FLastAddTex: integer;
    FNormalScale: single;

    function GetHash: integer;
    function CheckTransparency: boolean;
    procedure setUseMaterial(const Value: boolean);
    procedure setUseShader(const Value: boolean);
    procedure setUseTexture(const Value: boolean);
    procedure SetName(const Value: string);
    function getAddTex(Index: integer): TTexture;
    procedure setAddTex(Index: integer; const Value: TTexture);
    procedure setIgnoreLighting(const Value: boolean);
    procedure setTwoSideLighting(const Value: boolean);
    function getTexCount: integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(MaterialObject: TMaterialObject);
    procedure AttachTexture(tex: TTexture);
    procedure AttachSampler(aSampler: TTextureSampler);
    procedure AttachMaterial(mat: TMaterial);
    procedure AttachShader(Shader: TShaderProgram);
    procedure AddExTextures(tex: TTexture);

    function AddNewMaterial(aName: string = ''): TMaterial;
    function AddNewTexture(aName: string = ''): TTexture;
    function AddNewShader(aName: string = ''): TShaderProgram;

    function TextureByMapTarget(Map: TMapTarget): TTexture;

    property Name: string read FName write SetName;
    property Active: boolean read FActive write FActive;
    property HashKey: integer read GetHash;
    property Blending: TCustomBlending read FBlending;
    property Texture: TTexture read FTexture;
    property TextureSampler: TTextureSampler read FSampler;
    property Material: TMaterial read FMaterial;
    property Shader: TShaderProgram read FShader;
    property TextureSlot[index: integer]: TTexture read getAddTex
      write setAddTex;
    property TexCount: integer read getTexCount;
    property IsTransparency: boolean read CheckTransparency;
    property UseMaterial: boolean read FUseMaterial write setUseMaterial;
    property UseTexture: boolean read FUseTexture write setUseTexture;
    property UseShader: boolean read FUseShader write setUseShader;
    property UseAddinionalTextures: boolean read FUseAddTex write FUseAddTex;
    property LastTextureIndex: integer read FLastAddTex;
    property TwoSideLighting: boolean read FTwoSideLighting
      write setTwoSideLighting;
    property IgnoreLighting: boolean read FIgnoreLighting
      write setIgnoreLighting;

    property NormalScale: single read FNormalScale write FNormalScale;

  end;

  TMaterialList = class (TObjectsDictionary)
  private
    function getItemObj(index: integer): TMaterialObject;
  public
    destructor Destroy; override;

    function AddMaterial(const aItem: TMaterialObject): integer;
    function GetMaterial(aKey: TGUID): TMaterialObject; overload;
    function GetMaterial(aName: string): TMaterialObject; overload;

    procedure RemoveMaterial(const aItem: TMaterialObject);

    property Materials[index: integer]: TMaterialObject read getItemObj; default;
  end;

  TBufferObject = class(TBaseRenderResource)
  strict private
    FDataHandler: TAbstractDataList; // Второй источник данных
    FData: Pointer; // Первый источник данных
    FSize: integer; // Размер первого источника
    function GetData: Pointer;
    function GetSize: integer;
  protected
    FBuffType: TBufferType;
    FHandlerOwned: boolean;
    procedure setBufferType(const Value: TBufferType); virtual;
  public
    Name: string;
    constructor Create(BuffType: TBufferType = btArray); reintroduce;
    destructor Destroy; override;
    class function IsInner: boolean; override;
    procedure Notify(Sender: TObject; Msg: Cardinal;
      Params: pointer = nil); override;

    procedure Allocate(aSize: integer; aData: Pointer = nil); virtual;
    procedure SetDataHandler(aDataHandler: TAbstractDataList;
      aOwned: boolean = False);
    procedure Update;

    property BufferType: TBufferType read FBuffType write setBufferType;
    property Size: integer read GetSize;
    property Data: Pointer read GetData;
    property DataHandler: TAbstractDataList read FDataHandler;

  end;

  TAttribObject = class(TBaseRenderResource)
  private
    FValue: TVector;
    FBuffer: TBufferObject;
    FIteratorPtr: Pointer;

    procedure SetName(const Value: ansistring);
    procedure setSize(const Value: TValueComponent);
    procedure setStride(const Value: integer);
    procedure setType(const Value: TValueType);
    function GetValue: TVector;
    procedure SetValue(const Value: TVector);
  protected
    FSettedParam: set of (apAttrName, apAttrSize, apAttrType, apAttrStride);
    FAttribName: ansistring;
    FSize: TValueComponent;
    FType: TValueType;
    FStride: integer;
    FNormalized: boolean;
    FElementSize: integer;
    FSemantic: TAttribType;
    FTagObject: TObject;
  public
    constructor Create; override;
    constructor CreateAndSetup(AttrName: ansistring; aSize: TValueComponent;
      AType: TValueType = vtFloat; AStride: integer = 0;
      BuffType: TBufferType = btArray; aOwner: TObject = nil); virtual;
    constructor CreateClone(ASample: TAttribObject);
    destructor Destroy; override;
    class function IsInner: boolean; override;

    procedure Notify(Sender: TObject; Msg: Cardinal;
      Params: pointer = nil); override;

    procedure AssignBuffer(aBuffer: TBufferObject);
    procedure SetAttribSemantic(aSemantic: TAttribType);
    function IsEqual(AnObject: TAttribObject): Boolean;
    function GetVectorDataAccess: TVectorDataAccess;

    property Value: TVector read GetValue write SetValue;
    property Normalized: boolean read FNormalized write FNormalized;
    property Buffer: TBufferObject read FBuffer;
    property ElementSize: integer read FElementSize;

    property AttrSize: TValueComponent read FSize write setSize;
    property AttrType: TValueType read FType write setType;
    property AttrName: ansistring read FAttribName write SetName;
    property AttrStride: integer read FStride write setStride;
    property Semantic: TAttribType read FSemantic;
    property TagObject: TObject read FTagObject write FTagObject;
  end;

  TAttribObjectClass = class of TAttribObject;

  TAttribBuffer = class(TAttribObject)
  public
    constructor CreateAndSetup(AttrName: ansistring; aSize: TValueComponent;
      AType: TValueType = vtFloat; AStride: integer = 0;
      BuffType: TBufferType = btArray; aOwner: TObject = nil); override;
    destructor Destroy; override;
  end;

  TVertexObject = class;

  TSubMesh = record
    VertexObject: TVertexObject;
    Name: string;
    PrimType: TFaceType;
    Extents: TExtents;
    MaterialName: string;
    Visible: boolean;
    // SubMeshArrays
    VertexCount: integer;
    StartingVertex: integer;
    // SubMeshElements
    StartingIndex: integer;
    PrimitiveCount: integer;
  end;

  PSubMesh = ^TSubMesh;

  TAttribList = class(TDataList<TAttribObject>)
  public
    destructor Destroy; override;
    function GetAttribObject(aSemantic: TAttribType): TAttribObject;
    function GetAttribBuffer(aSemantic: TAttribType): TAttribBuffer;
    function ExtractAttrib(aSemantic: TAttribType): Boolean;
    procedure Pack;
  end;

  TIntIntRBTree = GRedBlackTree<integer, integer>;
  TVertexHashMap = GRedBlackTree<Double, integer>;

  { TODO : Реализовать заполнение 0-го сабмеша у TVertexObject }
  { TODO : Кеш окаймляющего бокса }

  TVertexObject = class(TBaseRenderResource)
  strict private

    FAttribs: TAttribList; // TAttribObject
    function getSubMesh(Index: integer): PSubMesh;
    function getIndicePtr: Pointer;
    function getAttrCount: integer;
    function getIndice(Index: integer): integer;
    procedure setIndice(Index: integer; const Value: integer);
    procedure setIndiceCount(const Value: integer);
    function getExtents: TExtents;
    function GetIndiceCount: integer;

  protected
    FVertexAttribIndex: integer;
    FSubMeshes: TList;
    FStructureChanged: boolean;
    FIndices: TIntegerArray;
    FAdjacencyIndices: TIntegerArray;
    FIndiceCount: integer;
    FIndiceChanged: boolean;
    FFaceType: TFaceType;
    FRestartIndex: integer;

    function getAttrib(Index: integer): TAttribObject;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(ASource: TVertexObject);
    procedure Notify(Sender: TObject; Msg: Cardinal;
      Params: pointer = nil); override;

    function AddAttrib(Attrib: TAttribObject;
      aVertices: boolean = false): integer;
    procedure RemoveAttrib(Index: integer);

    procedure AddPoint(Index: integer);
    procedure AddLine(i1, i2: integer);
    procedure AddTriangle(i1, i2, i3: integer);
    procedure AddQuad(i1, i2, i3, i4: integer);
    procedure ReservIndices(Capacity: integer);
    function GetIndices: TIntegerArray;
    procedure SetIndices(const anIndices: TIntegerArray);
    procedure SetAdjacencyIndices(const AnIndices: TIntegerArray);

    function AddSubMeshArrays(APrimType: TFaceType; AVertexCount: integer;
      AMaterialName: string; AStartingVertex: integer = 0; aName: string = '')
      : PSubMesh;
    function AddSubMeshElements(APrimType: TFaceType; APrimitiveCount: integer;
      AMaterialName: string; AStartingIndex: integer = 0; aName: string = '')
      : PSubMesh;

    procedure Clear;

    property Attribs[index: integer]: TAttribObject read getAttrib; default;
    property SubMeshes[index: integer]: PSubMesh read getSubMesh;
    property FaceType: TFaceType read FFaceType write FFaceType;
    property IndiceCount: integer read GetIndiceCount write setIndiceCount;
    property IndicePtr: Pointer read getIndicePtr;
    property AttribsCount: integer read getAttrCount;
    property Indices[Index: integer]: integer read getIndice write setIndice;
    property Extents: TExtents read getExtents;
  end;

  { TODO : Реализовать вычисление AABB, Sr, Sp для TMesh по TVertexObject
    - Добавить управление Локальной матрицей меша через скелет
  }

  TMesh = class(TBaseRenderResource)
  private
    FVertexObject: TVertexObject;
    FExtents: TExtents;
    FbbPosition: vec3;
    FbbRadius: single;
    FLocalMatrix: TMatrix;
    FIdentityMatrix: boolean;
    procedure SetLocalMatrix(const Value: TMatrix);
  public
    Visible: boolean;


    FriendlyName: string;

    MaterialObject: TMaterialObject;

    constructor CreateFrom(const aVertexObject: TVertexObject; const aGUID: TGUID); overload;
    constructor CreateFrom(const aVertexObject: TVertexObject); overload;
    constructor CreateFrom(const aVertexObject: TVertexObject; aName: string); overload;
    class function IsInner: boolean; override;

    property VertexObject: TVertexObject read FVertexObject;
    property Extents: TExtents read FExtents;
    property bbPosition: vec3 read FbbPosition;
    property bbRadius: single read FbbRadius;
    property LocalMatrix: TMatrix read FLocalMatrix write SetLocalMatrix;
    property IsIdentityMatrix: boolean read FIdentityMatrix;
  end;

  TMeshList = class
  private
    FList: TList;
    function getMesh(Index: integer): TMesh;
    function getCount: integer;
  public
    constructor Create;
    constructor CreateFrom(aMesh: TMesh); overload;
    constructor CreateFrom(aVertexObject: TVertexObject); overload;

    destructor Destroy; override;
    function AddNewMesh(aVertexObject: TVertexObject): TMesh;

    property Items[Index: integer]: TMesh read getMesh; default;
    property Count: integer read getCount;
  end;

  TLodFunction = function (aDistance: single): integer;

  TLoD = class
    LoD: TMeshList;
    Distance: single;
    constructor Create(aLoD: TMeshList; aDistance: single);
  end;

  TLodsDataList = TDataList<TLoD>;

  { TODO : Реализовать закон смены ЛОДов + функцию GetMeshLod с выборкой
           по расстоянию из библиотеки ЛОДов.
  }

  TLODsController = class (TPersistentResource)
  private
    FList: TLodsDataList;
    FLodFunction: TLodFunction;
    FConstant, FLinear, FQuadric: single;
    FMinLod, FMaxLod: integer;
    function getLod(Index: integer): TLod;
    function getCount: integer;
  public
    constructor Create(aBaseLod: TMeshList);
    constructor CreateOwner(aBaseLod: TMeshList; aOwner: TObject);
    destructor Destroy; override;

    procedure SetLodEquation(aConstant, aLinear, aQuadric: single);
    procedure SetLodRange(aMinLod, aMaxLod: integer);

    function GetMeshLod(const aLod: integer): TMeshList; overload;
    function GetMeshLod(const aDistance: single): TMeshList; overload;

    function AddLod(aLoD: TMeshList; aDistance: single): integer; overload;
    function AddLod(aLoD: TLoD): integer; overload;

    property onGetLod: TLodFunction read FLodFunction write FLodFunction;
    property LODS[Index: integer]: TLod read getLod; default;
    property Count: integer read getCount;

  end;

  TMeshObject = class(TBaseRenderResource)
  private
    FLods: TLODsController;
    FOccluder: TMeshList;
    FCollider: TTriangleList;
    FExtents: TExtents;
    function getMeshes: TMeshList;
  protected
    procedure setOwner(const Value: TObject); override;
  public
    FriendlyName: string;

    constructor Create; override;
    constructor CreateFrom(aMesh: TMeshList); overload;
    constructor CreateFrom(aMesh: TMesh); overload;
    destructor Destroy; override;

    function SetMesh(aMesh: TMeshList): integer; overload;
    function SetMesh(aMesh: TMesh): integer; overload;

    property Meshes: TMeshList read getMeshes;
    property LODS: TLODsController read FLods;
    property Occluder: TMeshList read FOccluder;
    property Collider: TTriangleList read FCollider;
  end;

  TMeshObjectsList = class(TObjectsDictionary)
  private
    function getMeshObj(Index: integer): TMeshObject;
  public
    function AddMeshObject(const aMeshObject: TMeshObject; aCapture: boolean = false): integer;
    function GetMeshObject(aKey: TGUID): TMeshObject; overload;
    function GetMeshObject(aFriendlyName: string): TMeshObject; overload;

    procedure RemoveMeshObject(const aMeshObject: TMeshObject);

    property MeshObjects[index: integer]: TMeshObject read getMeshObj; default;

    destructor Destroy; override;
  end;

  { TODO : Доработать структуру отображаемого объекта сцены.
  }
  TSceneObject = class(TMovableObject)
  private
    FMeshObjects: TMeshObjectsList;
  public
    Visible: boolean;
    Culled: boolean;

    constructor Create; override;
    destructor Destroy; override;

    property MeshObjects: TMeshObjectsList read FMeshObjects;
  end;


  TTexturesList = TDataList<TTexture>;
  TImageHolders = TDataList<TImageHolder>;

  TRenderBufferAttachment = record
    Texture: TTexture;
    Format: TRenderBufferFormat;
    Mode: TBufferMode;
  end;

  TFrameBuffer = class (TPersistentResource)
  private
    FReadBackBuffers: TImageHolders;
    FRenderBuffers: TRenderBuffers;
    FDepthBuffer: TRenderBufferAttachment;
    FStencilBuffer: TRenderBufferAttachment;
    FDepthStencilBuffer: TRenderBufferAttachment;
    FColorAttachments: TTexturesList;

    FActive: boolean;
    FMultisample: TMultisampleFormat;

    FSize: vec2i;

    procedure SetSize(aSize: vec2i);
    procedure SetActive(const Value: boolean);
    procedure SetMultisample(const Value: TMultisampleFormat);
    function GetRenderBuffer(aRenderBuffer: TRenderBuffer): TTexture;
    function GetTexture(aAttachmentSlot: cardinal): TTexture;
    function GetColorCount: Integer;

  public
    constructor Create; override;
    destructor Destroy; override;
    class function IsInner: boolean; override;


    procedure AttachRenderBuffer(aFormat: TRenderBufferFormat; aMode: TBufferMode = bmBuffer);
    procedure AttachColor(aTexture: TTexture);

    procedure ResetColorAttachments;
    procedure ResetRenderBuffers;
    procedure ResetReedBackBuffers;

    property Size: vec2i read FSize write SetSize;
    property ColorAttachmentCount: Integer read GetColorCount;
    property ColorAttachments[aSlot: cardinal]: TTexture read GetTexture;
    property BufferAttachments[aBuffer: TRenderBuffer]: TTexture read GetRenderBuffer;
    property RenderBuffers: TRenderBuffers read FRenderBuffers;
    property ReadBackBuffers: TImageHolders read FReadBackBuffers;
    property Active: boolean read FActive write SetActive;
    property Multisample: TMultisampleFormat read FMultisample write SetMultisample;
  end;

  { TODO : Update dictionary when object name changed }
  TSceneCamera = class(TMovableObject)
  private
    FViewMatrix: TMatrix;
    FProjMatrix: TMatrix;
    FRenderTarget: TFrameBuffer;
    FViewPortSize: vec2i;
    FFoV: single;
    FzNear: single;
    FzFar: single;
    FViewTarget: TMovableObject;
    FName: string;

    procedure SetFoV(const Value: single);
    procedure SetViewMatrix(const Value: TMatrix);
    procedure SetProjMatrix(const Value: TMatrix);
    procedure SetRenderTarget(const Value: TFrameBuffer);
    procedure SetViewPortSize(const Value: vec2i);
    procedure SetViewTarget(const Value: TMovableObject);
    procedure SetzFar(const Value: single);
    procedure SetzNear(const Value: single);
    procedure RebuildProjMatrix;
    procedure RebuildViewMatrix;

  public
    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;
    function GetViewMatrix: TMatrix;
  public
    constructor Create; override;

    procedure UpdateWorldMatrix(UseMatrix: TTransforms=[ttAll]); override;
    {: Adjusts distance from camera to target by applying a ratio.
       If ViewTarget is nil, nothing happens. This method helps in quickly
       implementing camera controls. Only the camera's position is changed. }
    procedure AdjustDistanceToTarget(distanceRatio: Single);

    property Name: string read FName write FName;
    property ViewPortSize: vec2i read FViewPortSize write SetViewPortSize;
    property ViewTarget: TMovableObject read FViewTarget write SetViewTarget;
    property FoV: single read FFoV write SetFoV;
    property zNear: single read FzNear write SetzNear;
    property zFar: single read FzFar write SetzFar;
    property ViewMatrix: TMatrix read GetViewMatrix write SetViewMatrix;
    property ProjMatrix: TMatrix read FProjMatrix write SetProjMatrix;
    property RenderTarget: TFrameBuffer read FRenderTarget write SetRenderTarget;
  end;

  TCamerasList = class (TObjectsDictionary)
  private
    function getItemObj(index: integer): TSceneCamera;
  public
    destructor Destroy; override;

    function AddCamera(const aItem: TSceneCamera): integer;
    function GetCamera(aKey: TGUID): TSceneCamera; overload;
    function GetCamera(aName: string): TSceneCamera; overload;

    procedure RemoveCamera(const aItem: TSceneCamera);

    property Cameras[index: integer]: TSceneCamera read getItemObj; default;
  end;


var
  vRegisteredResource: TRegisteredResource;

implementation

{ TColorVectorClass }

procedure TColorVectorClass.Assign(Color: TColorVectorClass);
begin
  Red := Color.Red;
  Green := Color.Green;
  Blue := Color.Blue;
  Alpha := Color.Alpha;
  RecalcHashKey;
end;

constructor TColorVectorClass.Create;
begin
  inherited Create;
  Red := 0;
  Green := 0;
  Blue := 0;
  Alpha := 1;
  RecalcHashKey;
end;

destructor TColorVectorClass.Destroy;
begin
  inherited;
end;

function TColorVectorClass.getAlpha: single;
begin
  result := FColorVector[3];
end;

function TColorVectorClass.getBValue: single;
begin
  result := FColorVector[2];
end;

function TColorVectorClass.getGValue: single;
begin
  result := FColorVector[1];
end;

function TColorVectorClass.getIntColorVect: Vec4i;
begin
  result[0] := trunc(FColorVector[0] * 255);
  result[1] := trunc(FColorVector[1] * 255);
  result[2] := trunc(FColorVector[2] * 255);
  result[3] := trunc(FColorVector[3] * 255);
end;

function TColorVectorClass.getRValue: single;
begin
  result := FColorVector[0];
end;

function TColorVectorClass.getVecAddr: Pointer;
begin
  result := FColorVector.GetAddr;
end;

procedure TColorVectorClass.RecalcHashKey;
begin
  FColorKey := HashKey(FColorVector.vec4, 65535);
end;

procedure TColorVectorClass.setAlpha(const Value: single);
begin
  FColorVector[3] := Value;
  FColorRec.Alpha := Value;
  RecalcHashKey;
end;

procedure TColorVectorClass.setBlue(const Value: single);
begin
  FColorVector[2] := Value;
  FColorRec.Blue := Value;
  RecalcHashKey;
end;

procedure TColorVectorClass.SetColorVector(const Value: TVector);
begin
  FColorVector := Value;
  FColorRec.Red := FColorVector[0];
  FColorRec.Green := FColorVector[1];
  FColorRec.Blue := FColorVector[2];
  FColorRec.Alpha := FColorVector[3];
  RecalcHashKey;
end;

procedure TColorVectorClass.SetColor(r, g, b, a: byte);
begin
  setRed(r / 255);
  setGreen(g / 255);
  setBlue(b / 255);
  setAlpha(a / 255);
end;

procedure TColorVectorClass.SetColor(r, g, b, a: single);
begin
  setRed(r);
  setGreen(g);
  setBlue(b);
  setAlpha(a);
end;

procedure TColorVectorClass.SetColorRec(const Value: TColor);
begin
  FColorRec := Value;
  RecalcHashKey;
end;

procedure TColorVectorClass.setGreen(const Value: single);
begin
  FColorVector[1] := Value;
  FColorRec.Green := Value;
  RecalcHashKey;
end;

procedure TColorVectorClass.setIntColorVect(const Value: Vec4i);
begin
  setRed(Value[0] / 255);
  setGreen(Value[1] / 255);
  setBlue(Value[2] / 255);
  setAlpha(Value[3] / 255);
end;

procedure TColorVectorClass.setRed(const Value: single);
begin
  FColorVector[0] := Value;
  FColorRec.Red := Value;
  RecalcHashKey;
end;

{ TMaterialProp }

procedure TMaterialProperties.Assign(Material: TMaterialProperties);
begin
  FDiffuseColor.Assign(Material.DiffuseColor);
  FAmbientColor.Assign(Material.AmbientColor);
  FSpecularColor.Assign(Material.SpecularColor);
  FEmissionColor.Assign(Material.EmissionColor);
  FShininess := Material.Shininess;
end;

constructor TMaterialProperties.Create;
begin
  inherited;
  FDiffuseColor := TColorVectorClass.Create;
  FAmbientColor := TColorVectorClass.Create;
  FSpecularColor := TColorVectorClass.Create;
  FEmissionColor := TColorVectorClass.Create;

  FAmbientColor.ColorVector := cAmbientColor;
  FDiffuseColor.ColorVector := cDiffuseColor;
  FSpecularColor.ColorVector := cSpecularColor;
  FEmissionColor.ColorVector := cEmissiveColor;
  FShininess := 128;
  FName := '';
end;

destructor TMaterialProperties.Destroy;
begin
  FDiffuseColor.Free;
  FAmbientColor.Free;
  FSpecularColor.Free;
  FEmissionColor.Free;
  inherited;
end;

function TMaterialProperties.IsEqual(MatProp: TMaterialProperties): boolean;
begin
  result := (MatProp.FDiffuseColor.FColorKey = FDiffuseColor.FColorKey) and
    (MatProp.FAmbientColor.FColorKey = FAmbientColor.FColorKey) and
    (MatProp.FSpecularColor.FColorKey = FSpecularColor.FColorKey) and
    (MatProp.FEmissionColor.FColorKey = FEmissionColor.FColorKey) and
    (MatProp.Shininess = Shininess);
end;

{ TLightSource }

constructor TLightSource.Create;
begin
  inherited;
  Position := Vector(0, 0, 1, 0);
  FAmbient := TColorVectorClass.Create;
  FDiffuse := TColorVectorClass.Create;
  FSpecular := TColorVectorClass.Create;
  FSceneColor := TColorVectorClass.Create;
  FAmbient.ColorVector := Vector(0, 0, 0, 1);
  FDiffuse.ColorVector := Vector(1, 1, 1, 1);
  FSpecular.ColorVector := Vector(1, 1, 1, 1);
  FSceneColor.ColorVector := Vector(0.2, 0.2, 0.2, 1);
  FSpotDirection := Vector(0, 0, -1, 0);
  FSpotExponent := 0;
  FSpotCutOff := 180;
  FConstAttenuation := 1;
  FLinearAttenuation := 0;
  FQuadraticAttenuation := 0;
  FLightStyle := lsOmni;
  FLightModel := lmGouraud;
  FLightSlot := -1;
  Enabled := false;
end;

destructor TLightSource.Destroy;
begin
  FAmbient.Free;
  FDiffuse.Free;
  FSpecular.Free;
  FSceneColor.Free;
  inherited;
end;

{ TMaterial }

constructor TMaterial.CreateOwned(aOwner: TObject);
begin
  inherited Create;
  Owner := aOwner;
  FMaterialProperties := TMaterialProperties.Create;
  FLightProperties := TLightSource.Create;
  FUseMaterial := true;
  FColorReplacing := crDisable;
  FMaterialType := mtFFP;
end;

destructor TMaterial.Destroy;
begin
  FMaterialProperties.Free;
  FLightProperties.Free;
  inherited;
end;

function TMaterial.GetHashName(const Name: string): integer;
var
  i, n: integer;
begin
  n := Length(name);
  result := n;
  for i := 1 to n do
    result := (result shl 1) + byte(name[i]);
end;

procedure TMaterial.SetName(const Value: string);
begin
  FName := Value;
  FNameHash := GetHashName(FName);
end;

{ TTexture }

procedure TTexture.SetName(const Value: string);
begin
  FName := Value;
end;

function TTexture.getImageFormat: TImageFormat;
begin
  if assigned(FImageDescriptor)
  then result:=FImageDescriptor.ImageFormat
  else result:=$FFFFFFFF;
end;

function TTexture.getTarget: TTexTarget;
begin
  result := FTarget;
end;

class function TTexture.IsInner: boolean;
begin
  result := true;
end;

procedure TTexture.setGenMipMaps(const Value: boolean);
begin
  include(FUpdates, tuGenMipMaps);
  FGenerateMipMaps := Value;
end;

procedure TTexture.setTexMatrix(const Value: TMatrix);
begin
  FTexMatrix := Value;
  FTexMatrixChanged := true;
end;

constructor TTexture.CreateOwned(aImageHolder: TImageHolder; aOwner: TObject);
begin
  assert(assigned(aImageHolder),'Image holder is not assigned!');
  Create;
  FTexMatrixChanged := false;
  Owner := aOwner;
  FImageDescriptor := aImageHolder;
  aImageHolder.Subscribe(Self);

  case FImageDescriptor.ImageType of
    itBitmap: begin
      FTarget := ttTexture2D;
      if FImageDescriptor.Height <= 1 then FTarget := ttTexture1D;
      if FImageDescriptor.Depth > 1 then FTarget := ttTexture3D;
    end;
    itBitmapArray: begin
      if FImageDescriptor.Height <= 1 then FTarget := tt1DArray
      else FTarget := tt2DArray;
    end;
    itVolume: begin
      FTarget := ttTexture3D;
    end;
    itCubemap: begin
      FTarget := ttCubemap;
    end;
    itCubemapArray: begin
      FTarget := ttCubemapArray;
    end;
  end;
end;

{ TTextureSampler }

function TTextureSampler.getTexDescr: PTextureDesc;
begin
  result := @FTextureDescriptor;
end;

constructor TTextureSampler.Create;
begin
  inherited;
  FUpdates := [tuWrapS, tuWrapT, tuWrapR, tuminFilter, tumagFilter, tuTextureGenS,
                tuTextureGenT, tuTextureGenR, tuMinLod, tuMaxLod, tuLodBias,
                tuCompareMode, tuCompareFunc];
  FTextureDescriptor.WrapS := twRepeat;
  FTextureDescriptor.WrapT := twRepeat;
  FTextureDescriptor.WrapR := twRepeat;
  FTextureDescriptor.minFilter := mnLinear;
  FTextureDescriptor.magFilter := mgLinear;
  FTextureDescriptor.TextureGenS := tgDisable;
  FTextureDescriptor.TextureGenT := tgDisable;
  FTextureDescriptor.TextureGenR := tgDisable;
  FTextureDescriptor.MinLod := -1000;
  FTextureDescriptor.MaxLod := 1000;
  FTextureDescriptor.LodBias := 0;
  FTextureDescriptor.CompareMode := cmNone;
  FTextureDescriptor.CompareFunc := cfLEqual;
  FTextureDescriptor.AnisotropyLevel := 1;
  FUseTexGen := false;
end;

procedure TTextureSampler.SetAnisotropyLevel(const Value: single);
begin
  include(FUpdates, tuAnisotropyLevel);
  FTextureDescriptor.AnisotropyLevel := Value;
end;

function TTextureSampler.getAnisotropyLevel: single;
begin
  result := FTextureDescriptor.AnisotropyLevel;
end;

function TTextureSampler.getCFunc: TTextureCompareFunc;
begin
  result:=FTextureDescriptor.CompareFunc;
end;

function TTextureSampler.getCMode: TTextureCompareMode;
begin
  result:=FTextureDescriptor.CompareMode;
end;

function TTextureSampler.getLodBias: single;
begin
  result := FTextureDescriptor.LodBias;
end;

function TTextureSampler.getMagFilter: TMagFilter;
begin
  result := FTextureDescriptor.magFilter;
end;

function TTextureSampler.getMaxLod: single;
begin
  result := FTextureDescriptor.MaxLod;
end;

function TTextureSampler.getMinFilter: TMinFilter;
begin
  result := FTextureDescriptor.minFilter;
end;

function TTextureSampler.getMinLod: single;
begin
  result := FTextureDescriptor.MinLod;
end;

function TTextureSampler.getSamplerHash: integer;
var buff: array[0..14] of cardinal;
begin
  buff[0]:=cardinal(FTextureDescriptor.WrapS);
  buff[1]:=cardinal(FTextureDescriptor.WrapT);
  buff[2]:=cardinal(FTextureDescriptor.WrapR);
  buff[3]:=cardinal(FTextureDescriptor.minFilter);
  buff[4]:=cardinal(FTextureDescriptor.magFilter);
  buff[5]:=cardinal(FTextureDescriptor.TextureGenS);
  buff[6]:=cardinal(FTextureDescriptor.TextureGenT);
  buff[7]:=cardinal(FTextureDescriptor.TextureGenR);
  buff[8]:=Pcardinal(@FTextureDescriptor.MinLod)^;
  buff[9]:=Pcardinal(@FTextureDescriptor.MaxLod)^;
  buff[10]:=Pcardinal(@FTextureDescriptor.LodBias)^;
  buff[11]:=cardinal(FTextureDescriptor.CompareMode);
  buff[12]:=cardinal(FTextureDescriptor.CompareFunc);
  buff[13]:=Pcardinal(@FTextureDescriptor.AnisotropyLevel)^;
  buff[14]:=Cardinal(FUseTexGen);
  result:=GetLongHash(@buff[0], sizeof(buff));
end;

function TTextureSampler.getTexGenR: TTexGens;
begin
  result := FTextureDescriptor.TextureGenR;
end;

function TTextureSampler.getTexGenS: TTexGens;
begin
  result := FTextureDescriptor.TextureGenS;
end;

function TTextureSampler.getTexGenT: TTexGens;
begin
  result := FTextureDescriptor.TextureGenT;
end;

function TTextureSampler.getWrapR: TTextureWraps;
begin
  result := FTextureDescriptor.WrapR;
end;

function TTextureSampler.getWrapS: TTextureWraps;
begin
  result := FTextureDescriptor.WrapS;
end;

function TTextureSampler.getWrapT: TTextureWraps;
begin
  result := FTextureDescriptor.WrapT;
end;

class function TTextureSampler.IsInner: boolean;
begin
  Result := true;
end;

procedure TTextureSampler.setCFunc(const Value: TTextureCompareFunc);
begin
  FTextureDescriptor.CompareFunc := Value;
  include(FUpdates, tuCompareFunc);
end;

procedure TTextureSampler.setCMode(const Value: TTextureCompareMode);
begin
  FTextureDescriptor.CompareMode:= Value;
  include(FUpdates, tuCompareMode);
end;

procedure TTextureSampler.setLodBias(const Value: single);
begin
  include(FUpdates, tuLodBias);
  FTextureDescriptor.LodBias := Value;;
end;

procedure TTextureSampler.setMagFilter(const Value: TMagFilter);
begin
  include(FUpdates, tuMagFilter);
  FTextureDescriptor.magFilter := Value;
end;

procedure TTextureSampler.setMaxLod(const Value: single);
begin
  include(FUpdates, tuMaxLod);
  FTextureDescriptor.MaxLod := Value;
end;

procedure TTextureSampler.setMinFilter(const Value: TMinFilter);
begin
  include(FUpdates, tuMinFilter);
  FTextureDescriptor.minFilter := Value;
end;

procedure TTextureSampler.setMinLod(const Value: single);
begin
  FTextureDescriptor.MinLod := Value;
  include(FUpdates, tuMinLod);
end;

procedure TTextureSampler.setTexGenR(const Value: TTexGens);
begin
  include(FUpdates, tuTextureGenR);
  FTextureDescriptor.TextureGenR := Value;
end;

procedure TTextureSampler.setTexGenS(const Value: TTexGens);
begin
  include(FUpdates, tuTextureGenS);
  FTextureDescriptor.TextureGenS := Value;
end;

procedure TTextureSampler.setTexGenT(const Value: TTexGens);
begin
  include(FUpdates, tuTextureGenT);
  FTextureDescriptor.TextureGenT := Value;
end;

procedure TTextureSampler.setWrapR(const Value: TTextureWraps);
begin
  include(FUpdates, tuWrapR);
  FTextureDescriptor.WrapR := Value;
end;

procedure TTextureSampler.setWrapS(const Value: TTextureWraps);
begin
  include(FUpdates, tuWrapS);
  FTextureDescriptor.WrapS := Value;
end;

procedure TTextureSampler.setWrapT(const Value: TTextureWraps);
begin
  include(FUpdates, tuWrapT);
  FTextureDescriptor.WrapT := Value;
end;


{ TBufferObject }

procedure TBufferObject.Allocate(aSize: integer; aData: Pointer);
begin
  FSize := aSize;
  FData := aData;
end;

constructor TBufferObject.Create(BuffType: TBufferType);
begin
  inherited Create;
  FBuffType := BuffType;
  FDataHandler := nil;
  FHandlerOwned := false;
end;

destructor TBufferObject.Destroy;
begin
  if Assigned(FDataHandler) then
  begin
    if FHandlerOwned then
      FreeAndNil(FDataHandler);
  end;
  inherited;
end;

function TBufferObject.GetData: Pointer;
begin
  if Assigned(FDataHandler) and (FDataHandler.Count > 0) then
    Result := FDataHandler.GetItemAddr(0)
  else
    Result := FData;
end;

function TBufferObject.GetSize: integer;
begin
  if Assigned(FDataHandler) and (FDataHandler.Count > 0) then
    Result := FDataHandler.Count * FDataHandler.ItemSize
  else
    Result := FSize;
end;

class function TBufferObject.IsInner: boolean;
begin
  result := true;
end;

procedure TBufferObject.Notify(Sender: TObject; Msg: Cardinal; Params: pointer);
begin
  if (Sender = FDataHandler) then
    case Msg of
      NM_ObjectDestroyed: FDataHandler := nil;
      NM_ResourceChanged: DispatchMessage(NM_ResourceChanged);
    end;
end;

procedure TBufferObject.setBufferType(const Value: TBufferType);
begin
  FBuffType := Value;
end;

procedure TBufferObject.SetDataHandler(aDataHandler: TAbstractDataList;
  aOwned: boolean);
begin
  if Assigned(FDataHandler) and FHandlerOwned then
      FreeAndNil(FDataHandler);

  FDataHandler := aDataHandler;
  FHandlerOwned := aOwned;
end;

procedure TBufferObject.Update;
begin
  DispatchMessage(NM_ResourceChanged);
end;

{ TAttribObject }

procedure TAttribObject.AssignBuffer(aBuffer: TBufferObject);
begin
  if Assigned(FBuffer) then
    FBuffer.UnSubscribe(Self);
  FBuffer := aBuffer;
  if Assigned(FBuffer) then
    FBuffer.Subscribe(Self);
end;

constructor TAttribObject.Create;
begin
  inherited;
  FSettedParam := [];
  FNormalized := false;
  FBuffer := nil;
  FStride := 0;
  FElementSize := -1;
  FIteratorPtr := nil;
end;

constructor TAttribObject.CreateAndSetup(AttrName: ansistring; aSize: TValueComponent;
      AType: TValueType; AStride: integer;
      BuffType: TBufferType; aOwner: TObject);
begin
  Create;
  FSize := aSize;
  FAttribName := AttrName;
  FType := AType;
  FStride := AStride;
  Owner := aOwner;
  if AStride = 0 then
    FElementSize := Ord(aSize) * CValueSizes[AType]
  else
    FElementSize := AStride;
  include(FSettedParam, apAttrName);
  include(FSettedParam, apAttrSize);
  include(FSettedParam, apAttrType);
  include(FSettedParam, apAttrStride);
end;

constructor TAttribObject.CreateClone(ASample: TAttribObject);
begin
  CreateAndSetup(ASample.FAttribName, ASample.FSize, ASample.FType,
    ASample.FStride);
  FNormalized := ASample.FNormalized;
  FSemantic := ASample.FSemantic;
  if Assigned(ASample.FBuffer) then
  begin
    FBuffer := TBufferObject.Create(ASample.FBuffer.FBuffType);
    FBuffer.SetDataHandler(TAbstractDataListClass(ASample.FBuffer.DataHandler.ClassType).Create);
    FBuffer.DataHandler.Join(ASample.FBuffer.DataHandler, TMatrix.IdentityMatrix);
    FBuffer.Subscribe(Self);
  end;
end;

destructor TAttribObject.Destroy;
begin
  if assigned(FBuffer) then
    if FBuffer.Owner=self then FBuffer.Free;
  inherited;
end;

function TAttribObject.GetValue: TVector;
begin
  Result := FValue;
end;

function TAttribObject.GetVectorDataAccess: TVectorDataAccess;
var
  aCount: Integer;
begin
  if Assigned(FBuffer) then
  begin
    aCount := FBuffer.Size div Ord(FSize) * CValueSizes[FType];
    Result := TVectorDataAccess.Create(FBuffer.Data, FType, FSize, FElementSize, aCount);
  end
  else
    Result := TVectorDataAccess.Create(FValue.GetAddr, FType, FSize, FElementSize, 1);
end;

function TAttribObject.IsEqual(AnObject: TAttribObject): Boolean;
begin
  if Self.ClassType = AnObject.ClassType then
    Result := (FSettedParam = AnObject.FSettedParam) and
      (FAttribName = AnObject.FAttribName) and
      (FSize = AnObject.FSize) and
      (FType = AnObject.FType) and
      (FStride = AnObject.FStride) and
      (FNormalized = AnObject.FNormalized) and
    (FElementSize = AnObject.FElementSize) and
    (FSemantic = AnObject.FSemantic)
  else
    Result := False;
end;

class function TAttribObject.IsInner: boolean;
begin
  result := true;
end;

procedure TAttribObject.Notify(Sender: TObject; Msg: Cardinal; Params: pointer);
begin
  case Msg of
    NM_ResourceChanged: DispatchMessage(NM_ResourceChanged);
    NM_ObjectDestroyed:
    begin
      if Sender = FBuffer then
        FBuffer := nil;
      DispatchMessage(NM_ResourceChanged);
    end;
  end;
end;

procedure TAttribObject.SetAttribSemantic(aSemantic: TAttribType);
begin
  FSemantic := aSemantic;
end;

procedure TAttribObject.SetName(const Value: ansistring);
begin
  FAttribName := Value;
  include(FSettedParam, apAttrName);
end;

procedure TAttribObject.setSize(const Value: TValueComponent);
begin
  FSize := Value;
  include(FSettedParam, apAttrSize);
end;

procedure TAttribObject.setStride(const Value: integer);
begin
  FStride := Value;
  include(FSettedParam, apAttrStride);
end;

procedure TAttribObject.setType(const Value: TValueType);
begin
  FType := Value;
  include(FSettedParam, apAttrType);
end;

procedure TAttribObject.SetValue(const Value: TVector);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    DispatchMessage(NM_ResourceChanged);
  end;
end;

{ TAttribBuffer }

constructor TAttribBuffer.CreateAndSetup(AttrName: ansistring; aSize: TValueComponent;
      AType: TValueType; AStride: integer;
      BuffType: TBufferType; aOwner: TObject);
begin
  inherited CreateAndSetup(AttrName, aSize, AType, AStride, BuffType);
  FBuffer := TBufferObject.Create(BuffType);
  FBuffer.Subscribe(Self);
  FBuffer.Owner:=self;
  Owner := aOwner;
end;

destructor TAttribBuffer.Destroy;
begin
  if Assigned(FBuffer) then begin
    FBuffer.UnSubscribe(Self);
    FBuffer.Free;
  end;
  inherited;
end;

{ TVertexObject }

function TVertexObject.AddAttrib(Attrib: TAttribObject;
  aVertices: boolean): integer;
var
  attr: TAttribObject;
begin
  attr := FAttribs.GetAttribObject(Attrib.Semantic);
  if Assigned(attr) then
  begin
    // Заменяем атрибут с той же семантикой
    attr.UnSubscribe(Self);
    Result := FAttribs.IndexOf(attr);
    FAttribs[Result] := Attrib;
  end
  else
    Result := FAttribs.Add(Attrib);
  Attrib.Subscribe(Self);

  FStructureChanged := true;
  if aVertices then
    FVertexAttribIndex := Result;

  DispatchMessage(NM_ResourceChanged);
end;

procedure TVertexObject.SetIndices(const anIndices: TIntegerArray);
begin
  FIndices := Copy(anIndices, 0, Length(anIndices));
  FIndiceCount := Length(anIndices);
  if FIndiceCount = 0 then
    ReservIndices(4);
end;

procedure TVertexObject.AddLine(i1, i2: integer);
begin
  if Length(FIndices) <= FIndiceCount + 2 then
    setlength(FIndices, Length(FIndices) * 2);
  FIndices[FIndiceCount] := i1;
  inc(FIndiceCount);
  FIndices[FIndiceCount] := i2;
  inc(FIndiceCount);
  FStructureChanged := true;
  FIndiceChanged := true;
end;

procedure TVertexObject.AddPoint(Index: integer);
begin
  if Length(FIndices) = FIndiceCount then
    setlength(FIndices, Length(FIndices) * 2);
  FIndices[FIndiceCount] := Index;
  Inc(FIndiceCount);
  FStructureChanged := true;
  FIndiceChanged := true;
  DispatchMessage(NM_ResourceChanged);
end;

procedure TVertexObject.AddQuad(i1, i2, i3, i4: integer);
begin
  if Length(FIndices) <= FIndiceCount + 6 then
    setlength(FIndices, Length(FIndices) * 2);
  FIndices[FIndiceCount] := i1;
  inc(FIndiceCount);
  FIndices[FIndiceCount] := i2;
  inc(FIndiceCount);
  FIndices[FIndiceCount] := i4;
  inc(FIndiceCount);
  FIndices[FIndiceCount] := i4;
  inc(FIndiceCount);
  FIndices[FIndiceCount] := i2;
  inc(FIndiceCount);
  FIndices[FIndiceCount] := i3;
  inc(FIndiceCount);
  FStructureChanged := true;
  FIndiceChanged := true;
  DispatchMessage(NM_ResourceChanged);
end;

function TVertexObject.AddSubMeshArrays(APrimType: TFaceType;
  AVertexCount: integer; AMaterialName: string; AStartingVertex: integer;
  aName: string): PSubMesh;
var
  psm: PSubMesh;
begin
  new(psm);
  result := psm;
  FSubMeshes.Add(psm);
  psm.VertexObject := Self;
  psm.Name := aName;
  psm.PrimType := APrimType;
  psm.MaterialName := AMaterialName;
  psm.VertexCount := AVertexCount;
  psm.StartingVertex := AStartingVertex;
  psm.StartingIndex := -1;
  psm.PrimitiveCount := -1;
  psm.Visible := true;
end;

function TVertexObject.AddSubMeshElements(APrimType: TFaceType;
  APrimitiveCount: integer; AMaterialName: string; AStartingIndex: integer;
  aName: string): PSubMesh;
var
  psm: PSubMesh;
begin
  new(psm);
  result := psm;
  FSubMeshes.Add(psm);
  psm.VertexObject := Self;
  psm.Name := aName;
  psm.PrimType := APrimType;
  psm.MaterialName := AMaterialName;
  psm.VertexCount := -1;
  psm.StartingVertex := -1;
  psm.StartingIndex := AStartingIndex;
  psm.PrimitiveCount := APrimitiveCount;
  psm.Visible := true;
end;

procedure TVertexObject.AddTriangle(i1, i2, i3: integer);
begin
  if Length(FIndices) <= FIndiceCount + 3 then
    setlength(FIndices, Length(FIndices) * 2);
  FIndices[FIndiceCount] := i1;
  inc(FIndiceCount);
  FIndices[FIndiceCount] := i2;
  inc(FIndiceCount);
  FIndices[FIndiceCount] := i3;
  inc(FIndiceCount);
  FStructureChanged := true;
  FIndiceChanged := true;
  DispatchMessage(NM_ResourceChanged);
end;

procedure TVertexObject.Assign(ASource: TVertexObject);
var
  attr: TAttribObject;
  AttribClass: TAttribObjectClass;
  I: Integer;
begin
  Clear;
  for I := 0 to ASource.AttribsCount - 1 do
  begin
    AttribClass := TAttribObjectClass(ASource.Attribs[I].ClassType);
    attr := AttribClass.CreateClone(ASource.Attribs[I]);
    FAttribs.Add(attr);
    attr.Subscribe(Self);
  end;
  FVertexAttribIndex := ASource.FVertexAttribIndex;
  // FSubMeshes попосжее
  FIndices := ASource.FIndices;
  FAdjacencyIndices := ASource.FAdjacencyIndices;
  FIndiceCount := ASource.FIndiceCount;
  FFaceType := ASource.FFaceType;
  FRestartIndex := ASource.FRestartIndex;
  DispatchMessage(NM_ResourceChanged);
end;

function CompareInteger_(const Item1, Item2: integer): boolean;
begin
  result := Item1 = Item2;
end;

procedure TVertexObject.Clear;
var
  I: Integer;
begin
  for I := 0 to FAttribs.Count - 1 do
    Attribs[I].UnSubscribe(Self);
  FAttribs.Clear;
  FAdjacencyIndices := nil;
  FIndiceCount := 0;
  if Length(FIndices) = 0 then
    ReservIndices(4);
  FFaceType := ftTriangles;
  FVertexAttribIndex := -1;
  FIndiceChanged := True;
  FStructureChanged := True;
  DispatchMessage(NM_ResourceChanged);
end;

constructor TVertexObject.Create;
begin
  inherited;
  FAttribs := TAttribList.Create;
  FStructureChanged := true;
  FIndiceChanged := false;
  FIndiceCount := 0;
  ReservIndices(4);
  FFaceType := ftTriangles;
  FVertexAttribIndex := -1;
  FSubMeshes := TList.Create;
  FRestartIndex := -1;
end;

destructor TVertexObject.Destroy;
var
  I: Integer;
begin
  for I := 0 to AttribsCount - 1 do
    Attribs[I].UnSubscribe(Self);
  FreeAndNil(FAttribs);
  FreeList(FSubMeshes);
  inherited;
end;

function TVertexObject.getAttrCount: integer;
begin
  result := FAttribs.Count;
end;

function TVertexObject.getAttrib(Index: integer): TAttribObject;
begin
  if (Index >= 0) and (Index < FAttribs.Count) then
    result := FAttribs[Index]
  else
    result := nil;
end;

function TVertexObject.getExtents: TExtents;
var
  i: integer;
  attr: TAttribObject;
begin
  result.Reset;
  if FVertexAttribIndex = -1 then
    exit;

  attr := Attribs[FVertexAttribIndex];
  if attr.FType <> vtFloat then
    exit;
  for i := 0 to attr.Buffer.DataHandler.Count - 1 do
    result.include(attr.Buffer.DataHandler.GetItemAsVector(i));
end;

function TVertexObject.getIndice(Index: integer): integer;
begin
  result := FIndices[index];
end;

function TVertexObject.GetIndiceCount: integer;
begin
  if (Length(FAdjacencyIndices) > 0) and
    (FFaceType in [ftLineStripAdjacency, ftLinesAdjacency,
    ftTriangleStripAdjacency, ftTrianglesAdjacency]) then
    result := Length(FAdjacencyIndices)
  else
    result := FIndiceCount;
end;

function TVertexObject.getIndicePtr: Pointer;
begin
  if (Length(FAdjacencyIndices) > 0) and
    (FFaceType in [ftLineStripAdjacency, ftLinesAdjacency,
    ftTriangleStripAdjacency, ftTrianglesAdjacency]) then
    result := @FAdjacencyIndices[0]
  else
    result := @FIndices[0];
end;

function TVertexObject.GetIndices: TIntegerArray;
begin
  Result := Copy(FIndices, 0, FIndiceCount);
end;

function TVertexObject.getSubMesh(Index: integer): PSubMesh;
begin
  result := FSubMeshes[index];
end;

procedure TVertexObject.Notify(Sender: TObject; Msg: Cardinal; Params: pointer);
var
  I: Integer;
begin
  for I := 0 to FAttribs.Count - 1 do
    if Attribs[I] = Sender then
    begin
      if Msg = NM_ObjectDestroyed then
      begin
        FAttribs[I] := nil;
        FAttribs.Pack;
        DispatchMessage(NM_ResourceChanged);
        exit;
      end
      else if Msg = NM_ResourceChanged then
      begin
        DispatchMessage(NM_ResourceChanged);
        exit;
      end;
    end;
end;

procedure TVertexObject.RemoveAttrib(Index: integer);
begin
  FAttribs.Delete(Index);
  FStructureChanged := true;
  DispatchMessage(NM_ResourceChanged);
end;

procedure TVertexObject.ReservIndices(Capacity: integer);
begin
  setlength(FIndices, Capacity);
end;

procedure TVertexObject.SetAdjacencyIndices(const AnIndices: TIntegerArray);
begin
  FAdjacencyIndices := AnIndices;
end;

procedure TVertexObject.setIndice(Index: integer; const Value: integer);
begin
  FIndices[index] := Value;
  DispatchMessage(NM_ResourceChanged);
end;

procedure TVertexObject.setIndiceCount(const Value: integer);
begin
  FIndiceCount := Value;
  if FIndiceCount > Length(FIndices) then
    setlength(FIndices, FIndiceCount);
end;

{ TMeshObject }

constructor TMeshObject.Create;
begin
  inherited;
  FLods := TLODsController.Create(nil);
  FOccluder := TMeshList.Create;
  FCollider := TTriangleList.Create;
end;

constructor TMeshObject.CreateFrom(aMesh: TMeshList);
begin
  inherited Create;
  FOccluder := TMeshList.Create;
  FCollider := TTriangleList.Create;
  FLods := TLODsController.Create(aMesh);
end;

constructor TMeshObject.CreateFrom(aMesh: TMesh);
begin
  inherited Create;
  FOccluder := TMeshList.Create;
  FCollider := TTriangleList.Create;
  FLods := TLODsController.Create(TMeshList.CreateFrom(aMesh));
end;

destructor TMeshObject.Destroy;
begin
  FLods.Free;
  FOccluder.Free;
  FCollider.Free;
  if assigned(Owner) and (Owner is TSceneItemList)
  then TSceneItemList(Owner).RemoveSceneItem(TBaseSceneItem(self));
  inherited;
end;

function TMeshObject.getMeshes: TMeshList;
begin
  result:=FLods.GetMeshLod(0);
end;

function TMeshObject.SetMesh(aMesh: TMeshList): integer;
begin

end;

function TMeshObject.SetMesh(aMesh: TMesh): integer;
begin

end;

procedure TMeshObject.setOwner(const Value: TObject);
begin
  if assigned(Owner) and (Owner is TSceneItemList)
  then TSceneItemList(Owner).RemoveSceneItem(TBaseSceneItem(self));

  inherited;
end;

{ TRegisteredResource }

constructor TRegisteredResource.Create;
begin
  inherited Create;
  FResource := TList.Create;
end;

destructor TRegisteredResource.Destroy;
begin
  // FreeObjectList(FResource);
  FResource.Free;
  inherited;
end;

procedure TRegisteredResource.RegisterResource(const aResource
  : TBaseRenderResource);
begin
  if FResource.indexof(aResource) < 0 then
    FResource.Add(aResource);
end;

procedure TRegisteredResource.UnRegisterResource(const aResource
  : TBaseRenderResource);
var
  i: integer;
begin
  i := FResource.indexof(aResource);
  if i < 0 then
    FResource.Delete(i);
end;

{ TMeshList }

function TMeshList.AddNewMesh(aVertexObject: TVertexObject): TMesh;
begin
  result := TMesh.CreateFrom(aVertexObject,'VertexObject'+inttostr(FList.Count));
  FList.Add(result);
end;

constructor TMeshList.Create;
begin
  inherited;
  FList := TList.Create;
end;

constructor TMeshList.CreateFrom(aMesh: TMesh);
begin
  Create;
  FList.Add(aMesh);
end;

constructor TMeshList.CreateFrom(aVertexObject: TVertexObject);
begin
  CreateFrom(TMesh.CreateFrom(aVertexObject, 'VertexObject'+inttostr(FList.Count)));
end;

destructor TMeshList.Destroy;
begin
  FreeObjectList(FList);
  inherited;
end;

function TMeshList.getCount: integer;
begin
  result := FList.Count;
end;

function TMeshList.getMesh(Index: integer): TMesh;
begin
  result := FList[Index];
end;

{ TMesh }

constructor TMesh.CreateFrom(const aVertexObject: TVertexObject;
  const aGUID: TGUID);
begin
  Create;
  FVertexObject := aVertexObject; // Возможно надо добавить Subscribe(aVertexObject)
  GUID := aGUID;
  FriendlyName := '';
  LocalMatrix := TMatrix.IdentityMatrix;
  FIdentityMatrix := true;
end;

constructor TMesh.CreateFrom(const aVertexObject: TVertexObject);
begin
  Create;
  FVertexObject := aVertexObject;
  FriendlyName := '';
  LocalMatrix := TMatrix.IdentityMatrix;
  FIdentityMatrix := true;
end;

constructor TMesh.CreateFrom(const aVertexObject: TVertexObject; aName: string);
begin
  Create;
  FVertexObject := aVertexObject;
  FriendlyName := aName;
  LocalMatrix := TMatrix.IdentityMatrix;
  FIdentityMatrix := true;
end;

class function TMesh.IsInner: boolean;
begin
  Result := true;
end;

procedure TMesh.SetLocalMatrix(const Value: TMatrix);
begin
  if FLocalMatrix <> Value then
  begin
    FLocalMatrix := Value;
    FIdentityMatrix := FLocalMatrix.IsIdentity;
    DispatchMessage(NM_WorldMatrixChanged);
  end;
end;

{ TShaderProgram }

procedure TShaderProgram.AddBuildinUniform(anUniform: TBaseBuiltinUniform);
begin
  if not assigned(FBuildinUniforms) then
    FBuildinUniforms := TObjectList.Create;
  FBuildinUniforms.Add(anUniform);
end;

constructor TShaderProgram.CreateOwned(aOwner: TObject);
var
  st: TShaderType;
begin
  inherited Create;
  Owner := aOwner;
  for st := Low(FShaderText) to high(FShaderText) do
    FShaderText[st] := '';
  FBinary.Size := 0;
  FFragDataBindPos.Name := '';
  FFragDataBindPos.Location := -1;
end;

destructor TShaderProgram.Destroy;
begin
  FBuildinUniforms.Free;
  inherited;
end;

function TShaderProgram.getAttr(Index: integer): TAttribSemantic;
begin
  result := FAttribsBindPos[Index];
end;

function TShaderProgram.getAttrCount: integer;
begin
  result := Length(FAttribsBindPos);
end;

function TShaderProgram.getShaderBinary: TBinaryData;
begin
  result := FBinary;
end;

function TShaderProgram.getShaderText(ShaderType: TShaderType): ansistring;
begin
  result := FShaderText[ShaderType];
end;

function TShaderProgram.SetAttribBindPos(const aName: ansistring;
  const aLocation: integer): integer;
var
  n: integer;
begin
  n := Length(FAttribsBindPos);
  setlength(FAttribsBindPos, n + 1);
  FAttribsBindPos[n].Name := aName;
  FAttribsBindPos[n].Location := aLocation;
  result := n;
end;

procedure TShaderProgram.setShaderBinary(const Value: TBinaryData);
begin
  FBinary := Value;
end;

procedure TShaderProgram.setShaderText(ShaderType: TShaderType;
  const Value: ansistring);
begin
  FShaderText[ShaderType] := Value;
end;

{ TCustomBlending }

procedure TCustomBlending.Assign(CustomBlending: TCustomBlending);
begin
  FBlendEnable := CustomBlending.BlendEnable;
  FAlphaTestEnable := CustomBlending.AlphaTestEnable;
  FSrcBlendFunc := CustomBlending.SrcBlendFunc;
  FDstBlendFunc := CustomBlending.DstBlendFunc;
  FAlphaFunc := CustomBlending.AlphaFunc;
  FAlphaThreshold := CustomBlending.AlphaThreshold;
end;

constructor TCustomBlending.Create;
begin
  inherited;
  FBlendingMode := bmOpaque;
  FBlendEnable := false;
  FAlphaTestEnable := false;
end;

destructor TCustomBlending.Destroy;
begin
  inherited;
end;

function TCustomBlending.GetHash: integer;
var
  hash: array [0 .. 5] of cardinal;
begin
  if FHashKey <> -1 then
  begin
    result := FHashKey;
    exit;
  end;

  hash[0] := cardinal(FBlendEnable);
  hash[1] := cardinal(FAlphaTestEnable);
  hash[2] := cardinal(FSrcBlendFunc);
  hash[3] := cardinal(FDstBlendFunc);
  hash[4] := cardinal(FAlphaFunc);
  hash[5] := trunc(FAlphaThreshold * 1000);
  FHashKey := GetHashFromBuff(hash, 16);
  result := FHashKey;
end;

procedure TCustomBlending.SetBlend(const Value: boolean);
begin
  FBlendEnable := Value;
end;

procedure TCustomBlending.SetByMode(aBlendingMode: TBlendingModes);
begin
  FBlendingMode := aBlendingMode;
  case aBlendingMode of
    bmOpaque:
      begin
        BlendEnable := false;
        FAlphaTestEnable := false;
        FSrcBlendFunc := sbf_ONE;
        FDstBlendFunc := dbf_ZERO;
        FAlphaFunc := af_ALWAYS;
        FAlphaThreshold := 0;
      end;
    bmTransparency:
      begin
        BlendEnable := true;
        FAlphaTestEnable := true;
        FSrcBlendFunc := sbf_SRC_ALPHA;
        FDstBlendFunc := dbf_ONE_MINUS_SRC_ALPHA;
        FAlphaFunc := af_GREATER;
        FAlphaThreshold := 0;
      end;
    bmAdditive:
      begin
        BlendEnable := true;
        FAlphaTestEnable := true;
        FSrcBlendFunc := sbf_SRC_ALPHA;
        FDstBlendFunc := dbf_ONE;
        FAlphaFunc := af_GREATER;
        FAlphaThreshold := 0;
      end;
    bmAlphaTest50:
      begin
        BlendEnable := false;
        FAlphaTestEnable := true;
        FAlphaFunc := af_GEQUAL;
        FAlphaThreshold := 0.5;
      end;
    bmAlphaTest100:
      begin
        BlendEnable := false;
        FAlphaTestEnable := true;
        FAlphaFunc := af_GEQUAL;
        FAlphaThreshold := 1;
      end;
    bmModulate:
      begin
        BlendEnable := true;
        FAlphaTestEnable := true;
        FSrcBlendFunc := sbf_DST_COLOR;
        FDstBlendFunc := dbf_ZERO;
        FAlphaFunc := af_GREATER;
        FAlphaThreshold := 0;
      end;
    bmCustom:
      begin
        BlendEnable := false;
        FAlphaTestEnable := false;
        FSrcBlendFunc := sbf_ONE;
        FDstBlendFunc := dbf_ZERO;
        FAlphaFunc := af_ALWAYS;
        FAlphaThreshold := 0;
      end;
  end;
end;

{ TMaterialObject }

procedure TMaterialObject.AddExTextures(tex: TTexture);
begin
  TextureSlot[FLastAddTex + 1] := tex;
end;

function TMaterialObject.AddNewMaterial(aName: string): TMaterial;
begin
  if assigned(FMaterial) and (FMaterial.Owner = Self) then FMaterial.Free;
  FMaterial := TMaterial.CreateOwned(Self);
  FMaterial.Name := aName;
  FUseMaterial := true;
  result := FMaterial;
  FActive := true;
end;

function TMaterialObject.AddNewShader(aName: string): TShaderProgram;
begin
  if assigned(FShader) and (FShader.Owner = Self) then
    FShader.Free;
  FShader := TShaderProgram.CreateOwned(Self);
  FShader.Name := aName;
  FUseShader := true;
  result := FShader;
  FActive := true;
end;

function TMaterialObject.AddNewTexture(aName: string): TTexture;
begin
  if assigned(FTexture) and (FTexture.Owner = Self) then
    FTexture.Free;
  FTexture := TTexture.CreateOwned(nil, Self);
  FTexture.Name := aName;
  FUseTexture := true;
  result := FTexture;
  FActive := true;
end;

procedure TMaterialObject.Assign(MaterialObject: TMaterialObject);
var
  i: integer;
begin
  FBlending.Assign(MaterialObject.FBlending);
  FTexture := MaterialObject.FTexture;
  FMaterial := MaterialObject.FMaterial;
  FShader := MaterialObject.FShader;
  FUseTexture := MaterialObject.FUseTexture;
  FUseMaterial := MaterialObject.FUseMaterial;
  FUseShader := assigned(FShader);
  FActive := MaterialObject.Active;
  for i := 0 to MaterialObject.FLastAddTex - 1 do
    FAdditionalTextures[i] := MaterialObject.FAdditionalTextures[i];
  FLastAddTex := MaterialObject.FLastAddTex;
  UseAddinionalTextures := MaterialObject.UseAddinionalTextures;
end;

procedure TMaterialObject.AttachMaterial(mat: TMaterial);
begin
  if assigned(FMaterial) and (FMaterial.Owner = Self) then
    FMaterial.Free;
  FMaterial := mat;
  FHashKey := -1;
  if assigned(mat) and (mat.UseMaterial) then
    FUseMaterial := true
  else
    FUseMaterial := false;
  FActive := true;
end;

procedure TMaterialObject.AttachSampler(aSampler: TTextureSampler);
begin
  if assigned(FSampler) and (FSampler.Owner = Self) then
    FSampler.Free;
  FSampler := aSampler;
  FHashKey := -1;
end;

procedure TMaterialObject.AttachShader(Shader: TShaderProgram);
begin
  if assigned(FShader) and (FShader.Owner = Self) then
    FShader.Free;
  FShader := Shader;
  FHashKey := -1;
  if assigned(FShader) then
    FUseShader := true;
  FActive := true;
end;

procedure TMaterialObject.AttachTexture(tex: TTexture);
begin
  if assigned(FTexture) and (FTexture.Owner = Self) then
    FTexture.Free;
  FTexture := tex;
  FHashKey := -1;
  if assigned(tex) and (not tex.Disabled) then
    FUseTexture := true
  else
    FUseTexture := false;
  FActive := true;
end;

function TMaterialObject.CheckTransparency: boolean;
var
  Alpha: single;
begin
  if assigned(FMaterial) then
    Alpha := Material.Properties.DiffuseColor.Alpha
  else
    Alpha := 1;
  if FBlending.FBlendEnable or FBlending.FAlphaTestEnable or (Alpha < 1) then
    result := true
  else
    result := false;
end;

constructor TMaterialObject.Create;
var
  mtc, i: integer;
begin
  inherited;
  FBlending := TCustomBlending.Create;
  FspId := 0;
  FHashKey := -1;
  FUseTexture := false;
  FUseMaterial := false;
  FUseShader := false;
  FName := 'MeterialObject';
  FNameChanged := true;
  setlength(FAdditionalTextures, 32);
  FUseAddTex := false;
  for i := 0 to Length(FAdditionalTextures) - 1 do
    FAdditionalTextures[i] := nil;
  FLastAddTex := 0;
  FActive := false;
  FTexture := nil;
  FMaterial := nil;
  FShader := nil;
  FTwoSideLighting := false;
  FIgnoreLighting := false;

end;

destructor TMaterialObject.Destroy;
begin
  FBlending.Free;
  if assigned(FMaterial) and (FMaterial.Owner = Self) then
    FMaterial.Free;
  if assigned(FTexture) and (FTexture.Owner = Self) then
    FTexture.Free;
  if assigned(FShader) and (FShader.Owner = Self) then
    FShader.Free;

  inherited;
end;

function TMaterialObject.getAddTex(Index: integer): TTexture;
begin
  assert(index < Length(FAdditionalTextures), 'Not enough texture units');
  assert(index > 0, 'Use Main texture for slot 0');
  result := FAdditionalTextures[index - 1];
end;

function TMaterialObject.GetHash: integer;
var
  hash: array [0 .. 4] of integer;
begin
  if FHashKey <> -1 then
  begin
    result := FHashKey;
    exit;
  end;
  hash[0] := integer(FTexture);
  hash[1] := integer(FMaterial);
  hash[2] := FBlending.HashKey;
  hash[3] := FspId;
  hash[4] := integer(FShader);
  FHashKey := GetHashFromBuff(hash, SizeOf(hash));
  result := FHashKey;
end;

function TMaterialObject.getTexCount: integer;
begin
  result := FLastAddTex;
  if assigned(FTexture) then
    inc(result);
end;

procedure TMaterialObject.setAddTex(Index: integer; const Value: TTexture);
begin
  assert(index < Length(FAdditionalTextures), 'Not enough texture units');
  assert(index > 0, 'Use Main texture for slot 0');
  FAdditionalTextures[index - 1] := Value;
  if (Value <> nil) and (index > FLastAddTex) then
    FLastAddTex := Index;
end;

procedure TMaterialObject.setIgnoreLighting(const Value: boolean);
begin
  FIgnoreLighting := Value;
  FActive := true;
end;

procedure TMaterialObject.SetName(const Value: string);
begin
  FName := Value;
  FNameChanged := true;
  FHashKey := StringHashKey(FName);
end;

procedure TMaterialObject.setTwoSideLighting(const Value: boolean);
begin
  FTwoSideLighting := Value;
  FActive := true;
end;

procedure TMaterialObject.setUseMaterial(const Value: boolean);
begin
  if assigned(FMaterial) then
    FUseMaterial := Value
  else
    FUseMaterial := false;
end;

procedure TMaterialObject.setUseShader(const Value: boolean);
begin
  if assigned(FShader) then
    FUseShader := Value
  else
    FUseShader := false;
end;

procedure TMaterialObject.setUseTexture(const Value: boolean);
begin
  if assigned(FTexture) then
    FUseTexture := Value
  else
    FUseTexture := false;
end;

function TMaterialObject.TextureByMapTarget(Map: TMapTarget): TTexture;
var
  i: integer;
begin
  if Map = mtDiffuse then
  begin
    result := FTexture;
    exit;
  end;
  for i := 0 to FLastAddTex - 1 do
  begin
    if assigned(FAdditionalTextures[i]) then
      if Map in FAdditionalTextures[i].MapTargets then
      begin
        result := FAdditionalTextures[i];
        exit;
      end;
  end;
  result := nil;
end;

{ TMeshObjectsList }

function TMeshObjectsList.AddMeshObject(const aMeshObject: TMeshObject;
  aCapture: boolean): integer;
begin
  result := AddKey(aMeshObject.GUID, aMeshObject);
  if aCapture then aMeshObject.Owner:=self;
end;

destructor TMeshObjectsList.Destroy;
var
  i: integer;
  mo: TMeshObject;
begin
  { TODO : Find Items Leaks }
  exit;
  for i := 0 to FCount - 1 do begin
    if assigned(FItems[i].Value) then begin
      mo := TMeshObject(FItems[i].Value);
      if mo.Owner=self then FreeAndNil(mo);
    end;
  end;

  inherited;
end;

function TMeshObjectsList.getMeshObj(Index: integer): TMeshObject;
begin
  result := TMeshObject(FItems[Index].Value);
end;

function TMeshObjectsList.GetMeshObject(aFriendlyName: string): TMeshObject;
var
  i: integer;
  mo: TMeshObject;
begin
  for i := 0 to FCount - 1 do
  begin
    mo := TMeshObject(FItems[i].Value);
    if mo.FriendlyName = aFriendlyName then
    begin
      result := mo;
      exit;
    end;
  end;
  result := nil;
end;

procedure TMeshObjectsList.RemoveMeshObject(const aMeshObject: TMeshObject);
var
  i: integer;
  mo: TMeshObject;
begin
  for i := 0 to FCount - 1 do begin
    mo := TMeshObject(FItems[i].Value);
    if mo = aMeshObject then begin
      if mo.Owner=self then FreeAndNil(mo);
      FItems[i].Key := -1;
      FItems[i].KeyName := '';
      FItems[i].KeyGUID := TGUIDEx.Empty;
      FItems[i].Value := nil;
      exit;
    end;
  end;

end;

function TMeshObjectsList.GetMeshObject(aKey: TGUID): TMeshObject;
begin
  result := TMeshObject(GetValue(aKey));
end;

{ TSceneObject }

constructor TSceneObject.Create;
begin
  inherited;
  FMeshObjects := TMeshObjectsList.Create;
end;

destructor TSceneObject.Destroy;
begin
  FMeshObjects.Free;
  inherited;
end;

{ TAttribList }

function TAttribList.ExtractAttrib(aSemantic: TAttribType): Boolean;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if (FItems[i].FSemantic = aSemantic) then
    begin
      FItems[i] := nil;
      Pack;
      Exit(True);
    end;
  Result := False;
end;

destructor TAttribList.Destroy;
begin
  inherited;
end;

function TAttribList.GetAttribBuffer(aSemantic: TAttribType): TAttribBuffer;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if (FItems[i].FSemantic = aSemantic) and (FItems[i] is TAttribBuffer) then
      exit(TAttribBuffer(FItems[i]));
  result := nil;
end;

function TAttribList.GetAttribObject(aSemantic: TAttribType): TAttribObject;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if FItems[i].FSemantic = aSemantic then
      exit(FItems[i]);
  result := nil;
end;

procedure TAttribList.Pack;
var
  PackedCount: integer;
  StartIndex: integer;
  EndIndex: integer;
begin
  if FCount = 0 then
    exit;

  PackedCount := 0;
  StartIndex := 0;
  repeat
    while (StartIndex < FCount) and (FItems[StartIndex] = nil) do
      inc(StartIndex);

    if StartIndex < FCount then
    begin
      EndIndex := StartIndex;
      while (EndIndex < FCount) and (FItems[EndIndex] <> nil) do
        inc(EndIndex);
      Dec(EndIndex);

      if StartIndex > PackedCount then
        System.Move(FItems[StartIndex], FItems[PackedCount],
          (EndIndex - StartIndex + 1) * SizeOf(Pointer));

      inc(PackedCount, EndIndex - StartIndex + 1);

      StartIndex := EndIndex + 1;
    end;
  until StartIndex >= FCount;

  FCount := PackedCount;
end;

{ TImageSampler }

class function TImageSampler.CreateBitmap(aFormatCode: cardinal; aWidth, aHeight: integer; aMipmapping: boolean): TImageHolder;
begin
  result:=TImageHolder.Create(aFormatCode, itBitmap);
  result.Width:=aWidth;
  result.Height:=aHeight;
  result.Depth:=1;
  result.Allocate(aMipmapping);
end;

class function TImageSampler.CreateBitmapArray(aFormatCode: cardinal; aWidth, aHeight,
  aDepth: integer; aMipmapping: boolean): TImageHolder;
begin
  result:=TImageHolder.Create(aFormatCode, itBitmapArray);
  result.Width:=aWidth;
  result.Height:=aHeight;
  result.Depth:=aDepth;
  result.Allocate(aMipmapping);
end;

class function TImageSampler.CreateCubeMap(aFormatCode: cardinal; aWidth, aHeight: integer;
  aMipmapping: boolean): TImageHolder;
begin
  result:=TImageHolder.Create(aFormatCode, itCubemapArray);
  result.Width:=aWidth;
  result.Height:=aHeight;
  result.Depth:=6;
  result.Allocate(aMipmapping);
end;

class function TImageSampler.CreateCubeMapArray(aFormatCode: cardinal; aWidth,
  aHeight, aDepth: integer; aMipmapping: boolean): TImageHolder;
begin
  result:=TImageHolder.Create(aFormatCode, itCubemapArray);
  result.Width:=aWidth;
  result.Height:=aHeight;
  result.Depth:=6*aDepth;
  result.Allocate(aMipmapping);
end;

class function TImageSampler.CreateDepth32Texture(aWidth, aHeight: integer;
  aMipmapping: boolean): TImageHolder;
begin
  result := CreateBitmap(IF_Depth32, aWidth, aHeight, aMipmapping);
end;

class function TImageSampler.CreateDepthStencilTexture(aWidth, aHeight: integer;
  aMipmapping: boolean): TImageHolder;
begin
  result := CreateBitmap(IF_Depth24Stencil8, aWidth, aHeight, aMipmapping);
end;

class function TImageSampler.CreateFloat32Texture2D(aWidth, aHeight: integer;
  aMipmapping: boolean): TImageHolder;
begin
  result := CreateBitmap(IF_Red32F, aWidth, aHeight, aMipmapping);
end;

class function TImageSampler.CreateRGBA32fTexture2D(aWidth, aHeight: integer;
  aMipmapping: boolean): TImageHolder;
begin
  result := CreateBitmap(IF_RGBA32F, aWidth, aHeight, aMipmapping);
end;

class function TImageSampler.CreateRGBA8Texture2D(aWidth, aHeight: integer;
  aMipmapping: boolean): TImageHolder;
begin
  result := CreateBitmap(IF_RGBA8UI, aWidth, aHeight, aMipmapping);
end;

class function TImageSampler.CreateVolume(aFormatCode: cardinal; aWidth, aHeight,
  aDepth: integer; aMipmapping: boolean): TImageHolder;
begin
  result:=TImageHolder.Create(aFormatCode, itVolume);
  result.Width:=aWidth;
  result.Height:=aHeight;
  result.Depth:=aDepth;
  result.Allocate(aMipmapping);
end;

{ TImageHolder }

procedure TImageHolder.Allocate(aWithMipmaps: boolean);
var size: cardinal;
begin
  if assigned(FData) then exit;
  if FImageFormat = $FFFFFFFF then exit;

  size := TImageFormatSelector.GetMemSize(FImageFormat, FWidth, FHeight, FDepth, FLevels>1);
  FDataSize := size;
  FReservedMem := size;

  getmem(FData,FReservedMem);
{  if aWithMipmaps then
    FillLodsStructure(FImageFormat,FWidth, FHeight,FDepth,isTextureArray)
  else
    DiscardLods;
}
  if aWithMipmaps then
    FillLodsStructure(FImageFormat,FWidth, FHeight,FDepth,isTextureArray)
  else
    FillZeroLod(FImageFormat,FWidth, FHeight,FDepth,isTextureArray);
end;

constructor TImageHolder.Create;
begin
  inherited;
  FImageFormat:=$FFFFFFFF;
  FImageType:=itBitmap;

  FElementSize:=-1;
  FDataSize:=-1;
  FReservedMem:=-1;
  FData:=nil;
  FWidth:=-1;
  FHeight:=-1;
  FDepth:=-1;
  FLevels:=1;
  FCompressed:=false;

  FLayers := TImageLayers.Create;
  FLayers.Add(@FLODS);
end;

constructor TImageHolder.Create(aFormatCode: cardinal; aImageType: TImageType);
var size: cardinal;
begin
  Create;
  FImageFormat := aFormatCode;
  setImageType(aImageType);
end;

procedure TImageHolder.Deallocate;
begin
  if assigned(FData) then begin
    FreeMem(FData); FData := nil;
    FElementSize:=-1;
    FDataSize:=-1;
    FReservedMem:=-1;
  end;

end;

destructor TImageHolder.Destroy;
begin
  Deallocate;
  FLayers.Destroy;
  inherited;
end;

procedure TImageHolder.DiscardLods;
begin
  FLevels := 1;
  FLODS[0].Width := FWidth;
  FLODS[0].Height := FHeight;
  FLODS[0].Depth := FDepth;
  FLODS[0].Offset := 0;
  FLODS[0].Size := FDataSize;
end;

procedure TImageHolder.FillLodsStructure(aFormatCode: cardinal; aWidth, aHeight,
  aDepth: integer; aArray: boolean);
var i,j, offs, size: integer;
    w,h,d,layers: integer;
begin
  assert(assigned(FData), 'Allocate image memory first!');
  FLevels := TImageFormatSelector.GetMipmapsCount(max(aWidth, max(aHeight, aDepth)));
  offs := 0; w := aWidth; h := aHeight; d := aDepth;
  if aArray then layers := aDepth else layers := 1;

  for i := 0 to FLevels - 1 do begin
    FLODS[i].Width := w;
    FLODS[i].Height := h;
    if not aArray then begin
      FLODS[i].Depth := d;
      FLODS[i].Offset := offs;
      FLODS[i].Size := TImageFormatSelector.GetMemSize(aFormatCode, w, h, d, false);
//      setlength(FLODS[i].LayersOffset, 0);
    end else begin
      FLODS[i].Depth := 1;
      FLODS[i].Offset := offs;
      Size := TImageFormatSelector.GetMemSize(aFormatCode, w, h, 1, false);
      FLODS[i].Size := Size * Layers;
//      setlength(FLODS[i].LayersOffset, layers);
//      for j := 0 to layers-1 do FLODS[i].LayersOffset[j] := offs + Size * j;
    end;

    offs := offs + FLODS[i].Size;
    w := max(1,w shr 1); h := max(1, h shr 1); d := max(1, d shr 1);
  end;
end;

procedure TImageHolder.FillZeroLod(aFormatCode: cardinal; aWidth, aHeight,
  aDepth: integer; aArray: boolean);
var i,j, offs, size: integer;
    w,h,d,layers: integer;
begin
  assert(assigned(FData), 'Allocate image memory first!');
  FLevels := 1;
  offs := 0; w := aWidth; h := aHeight; d := aDepth;
  if aArray then layers := aDepth else layers := 1;

  i := 0;
  FLODS[i].Width := w;
  FLODS[i].Height := h;
  if not aArray then begin
    FLODS[i].Depth := d;
    FLODS[i].Offset := offs;
    FLODS[i].Size := TImageFormatSelector.GetMemSize(aFormatCode, w, h, d, false);
  end else begin
    FLODS[i].Depth := 1;
    FLODS[i].Offset := offs;
    Size := TImageFormatSelector.GetMemSize(aFormatCode, w, h, 1, false);
    FLODS[i].Size := Size * Layers;
  end;
end;

function TImageHolder.getBitmapState: boolean;
begin
   if FImageType in [itBitmap, itBitmapArray]
   then result:=true else result:=false;
end;

function TImageHolder.getCubeMapState: boolean;
begin
   if FImageType in [itCubemap, itCubemapArray]
   then result:=true else result:=false;
end;

function TImageHolder.getDataSize: integer;
begin
  if FImageFormat<>$FFFFFFFF then result:=FDataSize else result:=-1;
end;

function TImageHolder.getImageLod(Index: integer): PImageLevelDesc;
begin
  assert((Index>=0) and (Index<FLevels), 'Lod Index out of range');
  result := @FLODS[Index];
end;

function TImageHolder.getTextureArrayState: boolean;
begin
   if FImageType in [itBitmapArray, itCubemapArray]
   then result:=true else result:=false;
end;

function TImageHolder.getVolumeState: boolean;
begin
   if FImageType in [itVolume]
   then result:=true else result:=false;
end;

procedure TImageHolder.LoadFromStream(aStream: TStream);
begin
  assert(false, 'Not implemented yet :(');
end;

procedure TImageHolder.LoadImageFromFile(aFileName: string);
var stream: TFileSTream;
begin
  stream:=TFileSTream.Create(aFileName, fmOpenRead);
  LoadImageFromStream(stream);
  stream.free;
end;

procedure TImageHolder.LoadImageFromStream(aStream: TStream);
begin
  assert(false, 'No one image format supported!');
end;

procedure TImageHolder.SaveImageToFile(aFileName, ImageFormat: string);
var stream: TFileSTream;
begin
  stream:=TFileSTream.Create(aFileName, fmOpenWrite);
  SaveImageToStream(stream);
  stream.free;
end;

procedure TImageHolder.SaveImageToStream(aStream: TStream; ImageFormat: string);
begin
  assert(false, 'No one image format supported!');
end;

procedure TImageHolder.SaveToStream(aStream: TStream);
begin
  assert(false, 'Not implemented yet :(');
end;

procedure TImageHolder.setDepth(const Value: integer);
begin
  FDepth := Value; Deallocate;
end;

procedure TImageHolder.setHeight(const Value: integer);
begin
  FHeight := Value; Deallocate;
end;

procedure TImageHolder.setImageFormat(const Value: cardinal);
begin
  FImageFormat := Value;
  Deallocate;
  if FImageFormat <> $FFFFFFFF then begin
    FElementSize := TImageFormatSelector.GetPixelSize(FImageFormat);
    FCompressed := TImageFormatBits.isCompressedFormat(FImageFormat);
  end else begin
    FCompressed := false;
    FElementSize := -1;
  end;
end;

procedure TImageHolder.setImageType(const Value: TImageType);
begin
  if FImageType <> Value then Deallocate;
  FImageType := Value;
  case Value of
    itVolume: FLevels := 1;
    itBitmap: FDepth := 1;
    itBitmapArray: FLevels := 1;
    itCubemap: FDepth := 6;
  end;

end;

procedure TImageHolder.setWidth(const Value: integer);
begin
  FWidth := Value; Deallocate;
end;

procedure TImageHolder.VolumeToArray;
begin
  if FImageType = itVolume then
  begin
    FImageType := itBitmapArray;
  end;
end;

constructor TImageHolder.CreateFromStream(aStream: TStream);
begin
  assert(false, 'Not implemented yet :(');
end;

function TImageHolder.CreateTexture: TTexture;
begin
  assert(FDataSize > 0, 'Allocate image first!');
  result := TTexture.CreateOwned(self);
end;

{ TMaterialList }

function TMaterialList.AddMaterial(const aItem: TMaterialObject): integer;
begin
  result:=AddKey(aItem.GUID, aItem);
end;

destructor TMaterialList.Destroy;
var i: integer;
    obj: TMaterialObject;
begin
  for i:=0 to Count-1 do begin
    obj := getItemObj(i);
    if obj.Owner = self then  obj.Free;
  end;

  inherited;
end;

function TMaterialList.getItemObj(index: integer): TMaterialObject;
begin
  result:=TMaterialObject(FItems[Index].Value);
end;

function TMaterialList.GetMaterial(aKey: TGUID): TMaterialObject;
begin
   result:=TMaterialObject(GetValue(aKey));
end;

function TMaterialList.GetMaterial(aName: string): TMaterialObject;
var i: integer;
    mi: TMaterialObject;
begin
  for i:=0 to FCount-1 do begin
    mi:=TMaterialObject(FItems[i].Value);
    if mi.FName=aName then begin
      result:=mi; exit;
    end;
  end; result:=nil;
end;

procedure TMaterialList.RemoveMaterial(const aItem: TMaterialObject);
var i: integer;
    mi: TMaterialObject;
begin
  for i:=0 to FCount-1 do begin
    mi:=TMaterialObject(FItems[i].Value);
    if mi=aItem then begin
      FItems[i].Key:=-1; FItems[i].KeyName:='';
      FItems[i].KeyGUID := TGUIDEx.Empty;
      FItems[i].Value:=nil; exit;
    end;
  end;
end;

{ TLightsList }

function TLightsList.AddLight(const aItem: TLightSource): integer;
begin
  result:=AddKey(aItem.GUID, aItem);
end;

destructor TLightsList.Destroy;
var i: integer;
    obj: TLightSource;
begin
  for i:=0 to Count-1 do begin
    obj := getItemObj(i);
    if obj.Owner = self then  obj.Free;
  end;
  inherited;
end;

function TLightsList.getItemObj(index: integer): TLightSource;
begin
  result:=TLightSource(FItems[Index].Value);
end;

function TLightsList.GetLight(aName: string): TLightSource;
var i: integer;
    li: TLightSource;
begin
  for i:=0 to FCount-1 do begin
    li:=TLightSource(FItems[i].Value);
    if li.FriendlyName=aName then begin
      result:=li; exit;
    end;
  end; result:=nil;
end;

function TLightsList.GetLight(aKey: TGUID): TLightSource;
begin
  result:=TLightSource(GetValue(aKey));
end;

procedure TLightsList.RemoveLight(const aItem: TLightSource);
var i: integer;
    li: TLightSource;
begin
  for i:=0 to FCount-1 do begin
    li:=TLightSource(FItems[i].Value);
    if li=aItem then begin
      FItems[i].Key:=-1; FItems[i].KeyName:='';
      FItems[i].KeyGUID := TGUIDEx.Empty;
      FItems[i].Value:=nil; exit;
    end;
  end;
end;

{ TSceneCamera }

procedure TSceneCamera.AdjustDistanceToTarget(distanceRatio: Single);
var
  v: TVector;
begin
  if Assigned(FViewTarget) then
  begin
    UpdateWorldMatrix;
    // calculate vector from target to camera in absolute coordinates
    v := {Absolute}Position - FViewTarget.AbsolutePosition;
    // ratio -> translation vector
    v.SetScale(-(1 - distanceRatio));
    v := v + {Absolute}Position;
    MoveObject(v);
  end;
end;

constructor TSceneCamera.Create;
begin
  inherited;
  FProjMatrix := TMatrix.IdentityMatrix;
  Direction := TVector.MakeTmp(vtY);
  FRenderTarget := nil;
  FViewPortSize[0]:=256;
  FViewPortSize[1]:=256;
  FFoV := 60;
  FzNear := 0.1;
  FzFar := 100;
  FViewTarget := nil;
  RebuildProjMatrix;
  RebuildViewMatrix;
end;

function TSceneCamera.GetViewMatrix: TMatrix;
begin
  RebuildViewMatrix;
  Result := FViewMatrix;
end;

procedure TSceneCamera.Notify(Sender: TObject; Msg: Cardinal; Params: pointer);
begin
  if sender = FViewTarget then
    case Msg of
      NM_WorldMatrixChanged: RebuildViewMatrix;
      NM_ObjectDestroyed: FViewTarget := nil;
    end;
  if sender = Owner then
    case Msg of
      NM_WorldMatrixChanged: begin
        UpdateWorldMatrix; RebuildViewMatrix;
      end;
      NM_ObjectDestroyed: UpdateWorldMatrix;
    end;
  inherited;
end;

procedure TSceneCamera.RebuildProjMatrix;
begin
  if FFoV = 0 then
    FProjMatrix := TMatrix.OrthoMatrix(0,FViewPortSize[0],0,FViewPortSize[1],FzNear, FzFar)
  else
    FProjMatrix := TMatrix.PerspectiveMatrix(FFov,FViewPortSize[0]/FViewPortSize[1],FzNear, FzFar);
end;

procedure TSceneCamera.RebuildViewMatrix;
begin
  if assigned(FViewTarget) then
    FViewMatrix := TMatrix.LookAtMatrix(Position,FViewTarget.Position,Up)
  else
    FViewMatrix := TMatrix.LookAtMatrix(Position,Position + Direction,Up);
end;

procedure TSceneCamera.SetFoV(const Value: single);
begin
  FFoV := Value;
  RebuildProjMatrix;
end;

procedure TSceneCamera.SetProjMatrix(const Value: TMatrix);
begin
  FProjMatrix := Value;
end;

procedure TSceneCamera.SetRenderTarget(const Value: TFrameBuffer);
begin
  if assigned(FRenderTarget) and (FRenderTarget.Owner = self)
  then FRenderTarget.Free;

  FRenderTarget := Value;
  if assigned(FRenderTarget) then FRenderTarget.SetSize(FViewPortSize);
end;

procedure TSceneCamera.SetViewMatrix(const Value: TMatrix);
begin
  FViewMatrix := Value;
end;

procedure TSceneCamera.SetViewPortSize(const Value: vec2i);
begin
  FViewPortSize := Value;
  if assigned(FRenderTarget) then FRenderTarget.SetSize(Value);
end;

procedure TSceneCamera.SetViewTarget(const Value: TMovableObject);
begin
  if Value <> FViewTarget then
  begin
    if Assigned(FViewTarget) then
      UnSubscribe(FViewTarget);
    FViewTarget := Value;
    if assigned(FViewTarget) then begin
      FViewTarget.Subscribe(self);
    end;
    RebuildViewMatrix;
  end;
end;

procedure TSceneCamera.SetzFar(const Value: single);
begin
  FzFar := Value;
  RebuildProjMatrix;
end;

procedure TSceneCamera.SetzNear(const Value: single);
begin
  FzNear := Value;
  RebuildProjMatrix;
end;

procedure TSceneCamera.UpdateWorldMatrix(UseMatrix: TTransforms);
begin
  RebuildViewMatrix;
  inherited;
end;

{ TFrameBuffer }

procedure TFrameBuffer.AttachColor(aTexture: TTexture);
begin
  FColorAttachments.Add(aTexture);
  DispatchMessage(NM_ResourceChanged);
end;

procedure TFrameBuffer.AttachRenderBuffer(aFormat: TRenderBufferFormat; aMode: TBufferMode);
var buffptr: ^TRenderBufferAttachment;
begin
  case aFormat of
    rbDepth16, rbDepth24, rbDepth32: begin
        buffptr := @FDepthBuffer;
          if aMode<>bmNone then include(FRenderBuffers,rbDepth)
          else exclude(FRenderBuffers,rbDepth);
      end;
    rbStencil1b, rbStencil4b, rbStencil8b, rbStencil16b: begin
        buffptr := @FStencilBuffer;
          if aMode<>bmNone then include(FRenderBuffers,rbStencil)
          else exclude(FRenderBuffers,rbStencil);
      end;
    rbDepth24Stencil8, rbDepth32FStencil8: begin
        buffptr := @FDepthStencilBuffer;
          if aMode<>bmNone then include(FRenderBuffers,rbDepthStencil)
          else exclude(FRenderBuffers,rbDepthStencil);
      end;
    else assert(false, 'Unknown render buffer format!');
  end;

  buffptr.Mode := aMode;
  buffptr.Format := aFormat;
  if assigned(buffptr.Texture) then buffptr.Texture.Free;
  buffptr.Texture := nil;

  DispatchMessage(NM_ResourceChanged);
end;

constructor TFrameBuffer.Create;
begin
    inherited Create;

    FReadBackBuffers := TImageHolders.Create;
    FRenderBuffers := [];
    FDepthBuffer.Mode := bmNone;
    FDepthBuffer.Texture := nil;
    FStencilBuffer.Mode := bmNone;
    FStencilBuffer.Texture := nil;
    FDepthStencilBuffer.Mode := bmNone;
    FDepthStencilBuffer.Texture :=nil;
    FColorAttachments := TTexturesList.Create;

    FActive := false;
    FMultisample := MSNone;

end;

destructor TFrameBuffer.Destroy;
begin
  ResetColorAttachments;
  ResetRenderBuffers;
  ResetReedBackBuffers;

  FColorAttachments.Free;
  FReadBackBuffers.Free;
  inherited;
end;

function TFrameBuffer.GetColorCount: Integer;
begin
  Result := FColorAttachments.Count;
end;

function TFrameBuffer.GetRenderBuffer(aRenderBuffer: TRenderBuffer): TTexture;
begin
  case aRenderBuffer of
    rbDepth: if FDepthBuffer.Mode = bmTexture
      then result := FDepthBuffer.Texture
      else result := nil;
    rbStencil: if FStencilBuffer.Mode = bmTexture
      then result := FStencilBuffer.Texture
      else result := nil;
    rbDepthStencil: if FDepthStencilBuffer.Mode = bmTexture
      then result := FDepthStencilBuffer.Texture
      else result := nil;
    else result := nil;
  end;
end;

function TFrameBuffer.GetTexture(aAttachmentSlot: cardinal): TTexture;
begin
  if (aAttachmentSlot < FColorAttachments.Count)
  then result := FColorAttachments[aAttachmentSlot]
  else result := nil;
end;

class function TFrameBuffer.IsInner: boolean;
begin
  Result := true;
end;

procedure TFrameBuffer.ResetColorAttachments;
begin
  FColorAttachments.Clear;
  DispatchMessage(NM_ResourceChanged);
end;

procedure TFrameBuffer.ResetReedBackBuffers;
begin
  FReadBackBuffers.Clear;
  DispatchMessage(NM_ResourceChanged);
end;

procedure TFrameBuffer.ResetRenderBuffers;
begin
  FRenderBuffers:=[];
  FDepthBuffer.Mode:=bmNone;
  if assigned(FDepthBuffer.Texture) then FDepthBuffer.Texture.Free;
  FStencilBuffer.Mode:=bmNone;
  if assigned(FStencilBuffer.Texture) then FStencilBuffer.Texture.Free;
  FDepthStencilBuffer.Mode:=bmNone;
  if assigned(FDepthStencilBuffer.Texture) then FDepthStencilBuffer.Texture.Free;
  DispatchMessage(NM_ResourceChanged);
end;

procedure TFrameBuffer.SetActive(const Value: boolean);
begin
  FActive := Value;
end;

procedure TFrameBuffer.SetMultisample(const Value: TMultisampleFormat);
begin
  FMultisample := Value;
  DispatchMessage(NM_ResourceChanged);
end;

procedure TFrameBuffer.SetSize(aSize: vec2i);
begin
  FSize := aSize;
  DispatchMessage(NM_ResourceChanged);
end;

{ TLODsController }

function TLODsController.AddLod(aLoD: TMeshList; aDistance: single): integer;
begin
  result := FList.Add(TLod.Create(aLod,aDistance));
end;

function TLODsController.AddLod(aLoD: TLoD): integer;
begin
  result := FList.Add(aLod);
end;

constructor TLODsController.Create(aBaseLod: TMeshList);
begin
  inherited Create;
  FList := TLodsDataList.Create;
  FList.Add(TLod.Create(aBaseLod, 0));
  FMinLod := 0; FMaxLod := high(Integer);
end;

constructor TLODsController.CreateOwner(aBaseLod: TMeshList; aOwner: TObject);
begin
  Create(aBaseLod);
  Owner := aOwner;
end;

destructor TLODsController.Destroy;
begin
  FList.Free;
  inherited;
end;

function TLODsController.getCount: integer;
begin
  result := FList.Count;
end;

function TLODsController.getLod(Index: integer): TLod;
begin
  result := FList[min(Index,FList.Count-1)];
end;

function TLODsController.GetMeshLod(const aLod: integer): TMeshList;
var n: integer;
begin
  n := max(aLod, FMinLod);
  n := min(n,FMaxLod);
  if n<FList.Count then result := FList[n].LoD
  else result:=nil;
end;

function TLODsController.GetMeshLod(const aDistance: single): TMeshList;
var i: integer;
begin
  if assigned(FLodFunction) then begin
    i:=FLodFunction(aDistance);
    i:=max(FMinLod,i); i:=min(FMaxLod,i);
    result:=FList[i].LoD;
  end else begin
    for i:= FList.Count-1 downto 0 do begin
      if FList[i].Distance <= aDistance then begin
        result:=FList[i].LoD; exit;
      end;
    end;
    result:=FList[0].LoD;
  end;
end;

procedure TLODsController.SetLodEquation(aConstant, aLinear, aQuadric: single);
begin
  FConstant:=aConstant; FLinear:=aLinear; FQuadric:=aQuadric;
end;

procedure TLODsController.SetLodRange(aMinLod, aMaxLod: integer);
begin
  FMinLod := aMinLod; FMaxLod := aMaxLod;
end;

{ TLoD }

constructor TLoD.Create(aLoD: TMeshList; aDistance: single);
begin
  LoD:=aLod; Distance:=aDistance;
end;

{ TBaseBuiltinUniform }

class function TBaseBuiltinUniform.IsInner: boolean;
begin
  Result := true;
end;

{ TBuiltinUniformLightNumber }

class function TBuiltinUniformLightNumber.Name: ansistring;
begin
  Result := 'LightNumber';
end;

{ TCamerasList }

function TCamerasList.AddCamera(const aItem: TSceneCamera): integer;
begin
  result:=AddKey(aItem.GUID, aItem);
end;

destructor TCamerasList.Destroy;
var i: integer;
    obj: TSceneCamera;
begin
  for i:=0 to Count-1 do begin
    obj := getItemObj(i);
    if obj.Owner = self then  obj.Free;
  end;
  inherited;
end;

function TCamerasList.GetCamera(aKey: TGUID): TSceneCamera;
begin
  result:=TSceneCamera(GetValue(aKey));
end;

function TCamerasList.GetCamera(aName: string): TSceneCamera;
var i: integer;
    si: TSceneCamera;
begin
  for i:=0 to FCount-1 do begin
    si:=TSceneCamera(FItems[i].Value);
    if si.FName=aName then begin
      result:=si; exit;
    end;
  end; result:=nil;

end;

function TCamerasList.getItemObj(index: integer): TSceneCamera;
begin
  result:=TSceneCamera(FItems[Index].Value);
end;

procedure TCamerasList.RemoveCamera(const aItem: TSceneCamera);
var i: integer;
    si: TSceneCamera;
begin
  for i:=0 to FCount-1 do begin
    si:=TSceneCamera(FItems[i].Value);
    if si=aItem then begin
      FItems[i].Key:=-1; FItems[i].KeyName:='';
      FItems[i].KeyGUID := TGUIDEx.Empty;
      FItems[i].Value:=nil; exit;
    end;
  end;
end;

initialization

vRegisteredResource := TRegisteredResource.Create;

finalization

vRegisteredResource.Free;

end.
