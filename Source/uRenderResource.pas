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
  uGenericsRBTree, uPersistentClasses;

const
  cDiffuseColor: vec4 = (0.8, 0.8, 0.8, 1);
  cAmbientColor: vec4 = (0.2, 0.2, 0.2, 1);
  cSpecularColor: vec4 = (0, 0, 0, 1);
  cEmissiveColor: vec4 = (0, 0, 0, 1);

Type

  TColor = record
    Red, Green, Blue, Alpha: single;
  end;

  TLightModels = (lmNone, lmGouraud, lmPhong, lmBlinn, lmLambert, lmDeferred);
  TColorReplacing = (crDisable, crEmission, crAmbient, crDiffuse, crSpecular,
    crAmbientAndDiffuse);

  TMaterialType = (mtFFP, mtShader);
  TLightStyle = (lsSpot, lsOmni, lsParallel, lsDirectional);

  TRegisteredResource = class(TNotifiableObject)
  private
    FResource: TList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterResource(const aResource: TBaseRenderResource);
    procedure UnRegisterResource(const aResource: TBaseRenderResource);
  end;

  TShaderProgram = class(TBaseRenderResource)
  private
    FOwner: TObject;
    FShaderText: array [stVertex .. stCompute] of ansistring;
    FBinary: TBinaryData;
    FFragDataBindPos: TAttribSemantic;
    FAttribsBindPos: array of TAttribSemantic;

    function getShaderText(ShaderType: TShaderType): ansistring;
    procedure setShaderText(ShaderType: TShaderType; const Value: ansistring);
    function getShaderBinary: TBinaryData;
    procedure setShaderBinary(const Value: TBinaryData);
    function getAttr(Index: integer): TAttribSemantic;
    function getAttrCount: integer;
  public
    Name: string;

    function SetAttribBindPos(const aName: ansistring;
      const aLocation: integer): integer;

    property ShaderText[ShaderType: TShaderType]: ansistring read getShaderText
      write setShaderText; default;
    property Binary: TBinaryData read getShaderBinary write setShaderBinary;

    property FragDataBindPos: TAttribSemantic read FFragDataBindPos
      write FFragDataBindPos;
    property AttribBinds[Index: integer]: TAttribSemantic read getAttr;
    property AttribBindsCount: integer read getAttrCount;

    property Owner: TObject read FOwner;

    constructor CreateOwned(aOwner: TObject = nil);
  end;

  TColorVectorClass = class;

  TLightSource = class(TBaseRenderResource)
  private
    FEnabled: boolean;
    FLightStyle: TLightStyle;
    FLightModel: TLightModels;
    FSpotDirection: TVector;
    FSpotCutOff: single;
    FSpotExponent: single;
    FPosition: TVector;
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
    property Position: TVector read FPosition write FPosition;
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
    FOwner: TObject;

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
    constructor CreateOwned(aOwner: TObject = nil);
    destructor Destroy; override;

    property Owner: TObject read FOwner;
    property Light: TLightSource read FLightProperties;
    property Properties: TMaterialProperties read FMaterialProperties;
    property ColorReplacing: TColorReplacing read FColorReplacing
      write FColorReplacing;
    property MaterialType: TMaterialType read FMaterialType write FMaterialType;
    property Name: string read FName write SetName;
    property UseMaterial: boolean read FUseMaterial write FUseMaterial;
  end;

  TTexture = class(TBaseRenderResource)
  private
    FOwner: TObject;

    FTextureDescriptor: TTextureDesc;
    FImageDescriptor: TImageDesc;
    FReady: boolean;
    FName: string;
    FUpdates: TTextureUpdates;
    FDisabled: boolean;
    FMapTargets: TMapTargets;
    FTextureMode: TTextureMode;
    FTexMatrix: TMatrix;
    FTexMatrixChanged: boolean;
    FTwoSides: boolean;

    function getCFormat: cardinal;
    function getCompressed: boolean;
    function getCubeMap: boolean;
    function getData: Pointer;
    function getDataSize: integer;
    function getDType: cardinal;
    function getElmSize: integer;
    function getFormat: cardinal;
    function getGenMipMaps: boolean;
    function getIFormat: cardinal;
    function getImgLod(Index: integer): TImageLevelDesc;
    function getLevels: integer;
    function getHeight: integer;
    function getDepth: integer;
    function getWidth: integer;

    function getMagFilter: TMagFilter;
    function getMinFilter: TMinFilter;
    function getResMem: integer;
    function getTarget: TTexTarget;
    function getTexArray: boolean;
    function getTexGenR: TTexGens;
    function getTexGenS: TTexGens;
    function getTexGenT: TTexGens;
    function getWrapR: TTextureWraps;
    function getWrapS: TTextureWraps;
    function getWrapT: TTextureWraps;
    function getAnisotropyLevel: single;
    procedure SetAnisotropyLevel(const Value: single);
    procedure setGenMipMaps(const Value: boolean);
    procedure setMagFilter(const Value: TMagFilter);
    procedure setMinFilter(const Value: TMinFilter);
    procedure setTexGenR(const Value: TTexGens);
    procedure setTexGenS(const Value: TTexGens);
    procedure setTexGenT(const Value: TTexGens);
    procedure setWrapR(const Value: TTextureWraps);
    procedure setWrapS(const Value: TTextureWraps);
    procedure setWrapT(const Value: TTextureWraps);
    procedure SetName(const Value: string);
    function getImgDescr: TImageDesc;
    function getTexDescr: TTextureDesc;
    procedure setTexMatrix(const Value: TMatrix);
    procedure setData(const Value: Pointer);
  public

    constructor CreateOwned(aOwner: TObject = nil);

    property Owner: TObject read FOwner;

    property ImageDescriptor: TImageDesc read getImgDescr;
    property TextureDescriptor: TTextureDesc read getTexDescr;
    property Updates: TTextureUpdates read FUpdates write FUpdates;

    property Disabled: boolean read FDisabled write FDisabled;
    property Name: string read FName write SetName;
    property MapTargets: TMapTargets read FMapTargets write FMapTargets;
    property TextureMode: TTextureMode read FTextureMode write FTextureMode;
    property Matrix: TMatrix read FTexMatrix write setTexMatrix;
    property MatrixChanged: boolean read FTexMatrixChanged;
    property TwoSides: boolean read FTwoSides write FTwoSides;

    // Texture Descriptors
    property WrapS: TTextureWraps read getWrapS write setWrapS;
    property WrapT: TTextureWraps read getWrapT write setWrapT;
    property WrapR: TTextureWraps read getWrapR write setWrapR;
    property minFilter: TMinFilter read getMinFilter write setMinFilter;
    property magFilter: TMagFilter read getMagFilter write setMagFilter;
    property TextureGenS: TTexGens read getTexGenS write setTexGenS;
    property TextureGenT: TTexGens read getTexGenT write setTexGenT;
    property TextureGenR: TTexGens read getTexGenR write setTexGenR;
    property GenerateMipMaps: boolean read getGenMipMaps write setGenMipMaps;
    property AnisotropyLevel: single read getAnisotropyLevel
      write SetAnisotropyLevel;

    // Image Descriptors
    property Target: TTexTarget read getTarget;
    property Format: cardinal read getFormat;
    property InternalFormat: cardinal read getIFormat;
    property ColorFormat: cardinal read getCFormat;

    property DataType: cardinal read getDType;
    property Width: integer read getWidth;
    property Height: integer read getHeight;
    property Depth: integer read getDepth;

    property ElementSize: integer read getElmSize;
    property DataSize: integer read getDataSize;
    property ReservedMem: integer read getResMem;
    property Data: Pointer read getData write setData;
    property Levels: integer read getLevels;
    property Compressed: boolean read getCompressed;
    property CubeMap: boolean read getCubeMap;
    property TextureArray: boolean read getTexArray;
    property LODS[Index: integer]: TImageLevelDesc read getImgLod;
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
    FBuffer: TBufferObject;
    FIteratorPtr: Pointer;

    procedure SetName(const Value: ansistring);
    procedure setSize(const Value: integer);
    procedure setStride(const Value: integer);
    procedure setType(const Value: TValueType);
  protected
    FSettedParam: set of (apAttrName, apAttrSize, apAttrType, apAttrStride);
    FAttribName: ansistring;
    FSize: integer;
    FType: TValueType;
    FStride: integer;
    FNormalized: boolean;
    FElementSize: integer;
    FSemantic: TAttribType;
    FTagObject: TObject;
  public
    constructor Create; override;
    constructor CreateAndSetup(AttrName: ansistring; aSize: integer;
      AType: TValueType = vtFloat; AStride: integer = 0;
      BuffType: TBufferType = btArray); virtual;
    constructor CreateClone(ASample: TAttribObject);

    procedure Notify(Sender: TObject; Msg: Cardinal;
      Params: pointer = nil); override;

    procedure AssignBuffer(aBuffer: TBufferObject);
    procedure SetAttribSemantic(aSemantic: TAttribType);
    function IsEqual(AnObject: TAttribObject): Boolean;

    property Normalized: boolean read FNormalized write FNormalized;
    property Buffer: TBufferObject read FBuffer;
    property ElementSize: integer read FElementSize;

    property AttrSize: integer read FSize write setSize;
    property AttrType: TValueType read FType write setType;
    property AttrName: ansistring read FAttribName write SetName;
    property AttrStride: integer read FStride write setStride;
    property Semantic: TAttribType read FSemantic;
    property TagObject: TObject read FTagObject write FTagObject;
  end;

  TAttribObjectClass = class of TAttribObject;

  TAttribBuffer = class(TAttribObject)
  public
    constructor CreateAndSetup(AttrName: ansistring; aSize: integer;
      AType: TValueType = vtFloat; AStride: integer = 0;
      BuffType: TBufferType = btArray); override;
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
    function MakeTrianglesIndices(): TIntegerArray;
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
    procedure SetAdjacencyIndices(const AnIndices: TIntegerArray);

    function AddSubMeshArrays(APrimType: TFaceType; AVertexCount: integer;
      AMaterialName: string; AStartingVertex: integer = 0; aName: string = '')
      : PSubMesh;
    function AddSubMeshElements(APrimType: TFaceType; APrimitiveCount: integer;
      AMaterialName: string; AStartingIndex: integer = 0; aName: string = '')
      : PSubMesh;

    // Удаление атрибутов и индексов
    procedure Clear;
    // Присоединение геометрии другого TVertexObject трансформирую её матрицей
    procedure Join(AnObject: TVertexObject; const AMatrix: TMatrix);
    // Сварка вершин
    procedure WeldVertices;
    // Каждая вершина становится уникальной, значения индексов не повторяется
    procedure UnWeldVertices;
    // Конвертирует полосы и вееры треугольников в отдельные треугольники
    procedure Triangulate;
    // Конвертирует полилинии в сегменты
    procedure LineSegmentation;
    // Создает нормали
    procedure ComputeNormals(ASmooth: boolean);
    // Создает касательные
    procedure ComputeTangents(ATexCoord: TAttribType = atTexCoord0);
    // Создает текстурные координаты
    procedure ComputeTexCoords(ATexCoord: TAttribType = atTexCoord0);
    // Создает индексы смежных треугольников
    procedure ComputeTriangleAdjacency;
    // Трансформирует вершины, нормали и касательные
    procedure Transform(const AMatrix: TMatrix);

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
  public
    Visible: boolean;
    LocalMatrix: TMatrix;

    FriendlyName: string;

    MaterialObject: TMaterialObject;

    constructor CreateFrom(const aVertexObject: TVertexObject;
      const aGUID: TGUID); overload;
    constructor CreateFrom(const aVertexObject: TVertexObject); overload;
    constructor CreateFrom(const aVertexObject: TVertexObject;
      aName: string); overload;

    property VertexObject: TVertexObject read FVertexObject;
    property Extents: TExtents read FExtents;
    property bbPosition: vec3 read FbbPosition;
    property bbRadius: single read FbbRadius;
  end;

  TMeshList = class
  private
    FList: TList;
    function getMesh(Index: integer): TMesh;
    function getCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function AddNewMesh(aVertexObject: TVertexObject): TMesh;

    property Items[Index: integer]: TMesh read getMesh; default;
    property Count: integer read getCount;
  end;

  TListOfMeshList =  TDataList<TMeshList>;

  { TODO : Заменить "Lods: TDataList<TMeshList>" на библиотеку ЛОДов
    с возможностью задавать закон смены ЛОДов, события на выбор ЛОДа и ограничения
    на минимальный/максимальный ЛОД.
    - Реализовать функцию GetMeshLod с выборкой по расстоянию из библиотеки ЛОДов.
  }
  TMeshObject = class(TBaseRenderResource)
  private
    FLods: TListOfMeshList;
    FOccluder: TMeshList;
    FCollider: TTriangleList;
    FExtents: TExtents;
  public
    FriendlyName: string;

    constructor Create; override;
    destructor Destroy; override;
    function GetMeshLod(const aLod: integer): TMeshList; overload;
    // function GetMeshLod(const aDistance: single): TMeshList; overload;
    // Вся работа с мешами через список ЛОДов
    property LODS: TListOfMeshList read FLods;
    property Occluder: TMeshList read FOccluder;
    property Collider: TTriangleList read FCollider;
  end;

  TMeshObjectsList = class(TObjectsDictionary)
  private
    function getMeshObj(Index: integer): TMeshObject;
  public
    function AddMeshObject(const aMeshObject: TMeshObject): integer;
    function GetMeshObject(aKey: TGUID): TMeshObject; overload;
    function GetMeshObject(aFriendlyName: string): TMeshObject; overload;

    procedure RemoveMeshObject(const aMeshObject: TMeshObject);

    property MeshObjects[index: integer]: TMeshObject read getMeshObj; default;
  end;

  { TODO : Доработать структуру отображаемого объекта сцены.
    Вопрос: Есть ли смысл делать Лоды на уровне объектов сцены?
    Переделать работу с MeshObjects на запрос ЛОДа
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

  { TODO : Заменить TList'ы на адекватные библиотки (материалов, источников света, объектов) }
  TSceneGraph = class(TSceneItemList)
    // Решить вопрос с камерой (списком камер) - где хранятся, как используются
    // Решить вопрос со скриптовым рендером - добавить вместе с камерами в корень графа?
    // Или передавать их в рендер через ProcessScene(Scene, Camera, Target, Script)?
    // Реализовать удаление используемых объектом ресурсов при уничтожении графа, подсчет ссылок?
  private
    FItemsList: TList;
    // List of TBaseSceneItem and TSceneObject(TBaseSceneItem)
    FLights: TList; // List of TLightSource
    FMaterials: TList; // List of TMaterialObjct
    function getItem(Index: integer): TBaseSceneItem;
    function getCount: integer;
    function getLight(Index: integer): TLightSource;
    function getLightCount: integer;
    function getMatCount: integer;
    function getMaterial(Index: integer): TMaterialObject;
  public
    constructor Create;
    destructor Destroy; override;

    function AddItem(aItem: TBaseSceneItem): integer;
    function AddLight(aLight: TLightSource): integer;

    function AddMaterial(aMat: TMaterialObject): integer;
    function AddNewMaterial(aName: string): TMaterialObject;

    property Items[index: integer]: TBaseSceneItem read getItem; default;
    property Lights[index: integer]: TLightSource read getLight;
    property LightsCount: integer read getLightCount;

    property Materials[index: integer]: TMaterialObject read getMaterial;
    property MaterialsCount: integer read getMatCount;

    property Count: integer read getCount;
  end;

  { TODO : Перенести класс TWorldSpace в модуль uWorldSpace }
  TWorldSpace = class(TBaseRenderResource)
  private
    FCameras: TList; // List of Cameras
    FScripts: TList; // List of lua scripts?
    FGraphs: TList; // List of TSceneGraph
    FTimerActions: TList; // List of timer events

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
  FShininess := 0;
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
  FAmbient := TColorVectorClass.Create;
  FDiffuse := TColorVectorClass.Create;
  FSpecular := TColorVectorClass.Create;
  FSceneColor := TColorVectorClass.Create;
  FAmbient.ColorVector := Vector(0, 0, 0, 1);
  FDiffuse.ColorVector := Vector(1, 1, 1, 1);
  FSpecular.ColorVector := Vector(1, 1, 1, 1);
  FSceneColor.ColorVector := Vector(0.2, 0.2, 0.2, 1);
  FPosition := Vector(0, 0, 1, 0);
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
  Create;
  FOwner := aOwner;
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

// TImageDescriptors

function TTexture.getCFormat: cardinal;
begin
  result := FImageDescriptor.ColorFormat;
end;

function TTexture.getCompressed: boolean;
begin
  result := FImageDescriptor.Compressed;
end;

function TTexture.getCubeMap: boolean;
begin
  result := FImageDescriptor.CubeMap;
end;

function TTexture.getData: Pointer;
begin
  result := FImageDescriptor.Data;
end;

function TTexture.getDataSize: integer;
begin
  result := FImageDescriptor.DataSize
end;

function TTexture.getDType: cardinal;
begin
  result := FImageDescriptor.DataType;
end;

function TTexture.getElmSize: integer;
begin
  result := FImageDescriptor.ElementSize;
end;

function TTexture.getFormat: cardinal;
begin
  result := FImageDescriptor.ColorFormat;
end;

function TTexture.getDepth: integer;
begin
  result := FImageDescriptor.Depth;
end;

function TTexture.getHeight: integer;
begin
  result := FImageDescriptor.Height;
end;

function TTexture.getWidth: integer;
begin
  result := FImageDescriptor.Width;
end;

function TTexture.getIFormat: cardinal;
begin
  result := FImageDescriptor.InternalFormat;
end;

function TTexture.getImgDescr: TImageDesc;
begin
  result := FImageDescriptor;
end;

function TTexture.getImgLod(Index: integer): TImageLevelDesc;
begin
  assert((index >= 0) and (index <= high(FImageDescriptor.LODS)),
    'Lod index [' + inttostr(Index) + '] out of Range');
  result := FImageDescriptor.LODS[index];
end;

function TTexture.getLevels: integer;
begin
  result := FImageDescriptor.Levels;
end;

function TTexture.getResMem: integer;
begin
  result := FImageDescriptor.ReservedMem;
end;

function TTexture.getTexArray: boolean;
begin
  result := FImageDescriptor.TextureArray;
end;

function TTexture.getTexDescr: TTextureDesc;
begin
  result := FTextureDescriptor;
end;

function TTexture.getTarget: TTexTarget;
begin
  result := FTextureDescriptor.Target;
end;

// TextureDescriptors

function TTexture.getAnisotropyLevel: single;
begin
  result := FTextureDescriptor.AnisotropyLevel;
end;

function TTexture.getGenMipMaps: boolean;
begin
  result := FTextureDescriptor.GenerateMipMaps;
end;

function TTexture.getMagFilter: TMagFilter;
begin
  result := FTextureDescriptor.magFilter;
end;

function TTexture.getMinFilter: TMinFilter;
begin
  result := FTextureDescriptor.minFilter;
end;

function TTexture.getTexGenR: TTexGens;
begin
  result := FTextureDescriptor.TextureGenR;
end;

function TTexture.getTexGenS: TTexGens;
begin
  result := FTextureDescriptor.TextureGenS;
end;

function TTexture.getTexGenT: TTexGens;
begin
  result := FTextureDescriptor.TextureGenT;
end;

function TTexture.getWrapR: TTextureWraps;
begin
  result := FTextureDescriptor.WrapR;
end;

function TTexture.getWrapS: TTextureWraps;
begin
  result := FTextureDescriptor.WrapS;
end;

function TTexture.getWrapT: TTextureWraps;
begin
  result := FTextureDescriptor.WrapT;
end;

procedure TTexture.SetAnisotropyLevel(const Value: single);
begin
  include(FUpdates, tuAnisotropyLevel);
  FTextureDescriptor.AnisotropyLevel := Value;
end;

procedure TTexture.setData(const Value: Pointer);
begin
  FImageDescriptor.Data := Value;
end;

procedure TTexture.setGenMipMaps(const Value: boolean);
begin
  include(FUpdates, tuGenMipMaps);
  FTextureDescriptor.GenerateMipMaps := Value;
end;

procedure TTexture.setMagFilter(const Value: TMagFilter);
begin
  include(FUpdates, tuMagFilter);
  FTextureDescriptor.magFilter := Value;
end;

procedure TTexture.setMinFilter(const Value: TMinFilter);
begin
  include(FUpdates, tuMinFilter);
  FTextureDescriptor.minFilter := Value;
end;

procedure TTexture.setTexGenR(const Value: TTexGens);
begin
  include(FUpdates, tuTextureGenR);
  FTextureDescriptor.TextureGenR := Value;
end;

procedure TTexture.setTexGenS(const Value: TTexGens);
begin
  include(FUpdates, tuTextureGenS);
  FTextureDescriptor.TextureGenS := Value;
end;

procedure TTexture.setTexGenT(const Value: TTexGens);
begin
  include(FUpdates, tuTextureGenT);
  FTextureDescriptor.TextureGenT := Value;
end;

procedure TTexture.setTexMatrix(const Value: TMatrix);
begin
  FTexMatrix := Value;
  FTexMatrixChanged := true;
end;

procedure TTexture.setWrapR(const Value: TTextureWraps);
begin
  include(FUpdates, tuWrapR);
  FTextureDescriptor.WrapR := Value;
end;

procedure TTexture.setWrapS(const Value: TTextureWraps);
begin
  include(FUpdates, tuWrapS);
  FTextureDescriptor.WrapS := Value;
end;

procedure TTexture.setWrapT(const Value: TTextureWraps);
begin
  include(FUpdates, tuWrapT);
  FTextureDescriptor.WrapT := Value;
end;

constructor TTexture.CreateOwned(aOwner: TObject);
begin
  Create;
  FTexMatrixChanged := false;
  FOwner := aOwner;
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
    FDataHandler.UnSubscribe(Self);
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
  if Assigned(FDataHandler) then
  begin
    FDataHandler.UnSubscribe(Self);
    if FHandlerOwned then
      FreeAndNil(FDataHandler);
  end;

  FDataHandler := aDataHandler;
  FHandlerOwned := aOwned;

  if Assigned(FDataHandler) then
    FDataHandler.Subscribe(Self);
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

constructor TAttribObject.CreateAndSetup(AttrName: ansistring; aSize: integer;
      AType: TValueType = vtFloat; AStride: integer = 0;
      BuffType: TBufferType = btArray);
begin
  Create;
  FSize := aSize;
  FAttribName := AttrName;
  FType := AType;
  FStride := AStride;
  if AStride = 0 then
    FElementSize := aSize * CValueSizes[AType]
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

procedure TAttribObject.setSize(const Value: integer);
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

{ TAttribBuffer }

constructor TAttribBuffer.CreateAndSetup(AttrName: ansistring; aSize: integer;
      AType: TValueType = vtFloat; AStride: integer = 0;
      BuffType: TBufferType = btArray);
begin
  inherited CreateAndSetup(AttrName, aSize, AType, AStride, BuffType);
  FBuffer := TBufferObject.Create(BuffType);
  FBuffer.Subscribe(Self);
end;

destructor TAttribBuffer.Destroy;
begin
  if Assigned(FBuffer) then
  begin
    FBuffer.UnSubscribe(Self);
    FBuffer.Destroy;
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

procedure TVertexObject.ComputeNormals(ASmooth: boolean);
var
  i, J, E, E_, T, EJ: integer;
  Vertices: TAbstractDataList;
  PBIndices: TIntegerArray;
  p0, p1, p2, dp0, dp1, fNormal, nNormal, cNormal: TVector;
  NewNormals, Normals: TVec3List;
  NewNormalIndices: TIntegerList;
  collisionMap: TIntIntRBTree;
  Agrees: boolean;
  NormalBuffer: TAttribBuffer;

begin
  if not(FFaceType in [ftTriangleStrip, ftTriangleFan, ftTriangles]) then
    exit;

  if FVertexAttribIndex = -1 then
    exit;

  BeginUpdate;

  try

    Vertices := Attribs[FVertexAttribIndex].FBuffer.DataHandler;

    FAttribs.ExtractAttrib(atNormal);

    Triangulate;

    if ASmooth then
    begin
      // Делаем сваривание вершим по равенству их позиций
      setlength(PBIndices, FIndiceCount);
      for i := 1 to FIndiceCount - 1 do
      begin
        E := FIndices[i];
        PBIndices[i] := E;
        for J := 0 to i - 1 do
        begin
          E_ := PBIndices[J];
          if E = E_ then
            continue;
          if Vertices.IsItemsEqual(E, E_) then
          begin
            PBIndices[i] := E_;
            break;
          end;
        end;
      end;
    end
    else
    begin
      UnWeldVertices;
      PBIndices := Copy(FIndices, 0, FIndiceCount);
    end;

    NewNormals := TVec3List.Create;
    NewNormals.Count := FIndiceCount;
    NewNormalIndices := TIntegerList.Create;

    // The collision map records any alternate locations for the normals
    if ASmooth then
    begin
      collisionMap := TIntIntRBTree.Create(CompareInteger, CompareInteger_);
      collisionMap.DuplicateKeys := true;
    end
    else
      collisionMap := nil;

    // Iterate over the faces, computing the face normal and summing it them
    for T := 0 to Length(PBIndices) div 3 - 1 do
    begin
      E := 3 * T;
      p0 := Vertices.GetItemAsVector(PBIndices[E]);
      p1 := Vertices.GetItemAsVector(PBIndices[E + 1]);
      p2 := Vertices.GetItemAsVector(PBIndices[E + 2]);

      // Compute the edge vectors
      dp0 := p1 - p0;
      dp1 := p2 - p0;

      // Compute the face normal
      fNormal := dp0.Cross(dp1);

      if not ASmooth then
      begin
        NewNormals[E] := fNormal.vec3;
        NewNormalIndices.Add(E);
        inc(E);
        NewNormals[E] := fNormal.vec3;
        NewNormalIndices.Add(E);
        inc(E);
        NewNormals[E] := fNormal.vec3;
        NewNormalIndices.Add(E);
        continue;
      end;

      // Compute a normalized normal
      nNormal := fNormal.Normalize;

      // Iterate over the vertices, adding the face normal influence to each
      for J := 0 to 2 do
      begin
        // Get the current normal from the default location (index shared with position)
        EJ := PBIndices[E + J];
        cNormal := NewNormals[EJ];

        // Check to see if this normal has not yet been touched
        if cNormal.IsNull then
        begin
          // First instance of this index, just store it as is
          NewNormals[EJ] := fNormal.vec3;
          NewNormalIndices.Add(EJ);
        end
        else
        begin
          // Check for agreement

          if cNormal.Normalize.Dot(nNormal) >= cos(3.1415926 * 0.333333) then
          begin
            // Normal agrees, so add it
            cNormal := cNormal + fNormal;
            NewNormals[EJ] := cNormal.vec3;
            NewNormalIndices.Add(EJ);
          end
          else
          begin
            // Normals disagree, this vertex must be along a facet edge
            Agrees := false;
            if collisionMap.Find(EJ, E_) then
            begin
              // Loop through all hits on this index, until one agrees
              repeat
                cNormal := NewNormals[E_];
                if cNormal.Normalize.Dot(nNormal) >= cos(3.1415926 * 0.333333)
                then
                begin
                  Agrees := true;
                  break;
                end;
              until not collisionMap.NextDublicate(E_);
            end;

            // Check for agreement with an earlier collision
            if Agrees then
            begin
              // Found agreement with an earlier collision, use that one
              cNormal := cNormal + fNormal;
              NewNormals[E_] := cNormal.vec3;
              NewNormalIndices.Add(E_);
            end
            else
            begin
              // We have a new collision, create a new normal
              NewNormalIndices.Add(NewNormals.Count);
              collisionMap.Add(EJ, NewNormals.Count);
              NewNormals.Add(fNormal.vec3);
            end;
          end; // else ( if normal agrees)
        end; // else (if normal is uninitialized)
      end; // for each vertex in triangle
    end; // for each face

    if ASmooth then
      UnWeldVertices;

    Normals := TVec3List.Create;
    Normals.Count := NewNormalIndices.Count;
    for I := 0 to NewNormalIndices.Count - 1 do
    begin
      E := NewNormalIndices[I];
      cNormal.Vec3 := NewNormals[E];
      cNormal.SetNormalize;
      E_ := FIndices[I];
      Normals[E_] := cNormal.Vec3;
    end;

    NormalBuffer := TAttribBuffer.CreateAndSetup(
      CAttribSematics[atNormal].Name, 3, vtFloat, 0, btArray);
    with NormalBuffer do
    begin
      Buffer.SetDataHandler(Normals);
      SetAttribSemantic(atNormal);
    end;
    AddAttrib(NormalBuffer, false);

    NewNormals.Destroy;
    NewNormalIndices.Destroy;
    collisionMap.Free;
    WeldVertices;

  finally
    EndUpdate;
  end;
end;

procedure TVertexObject.ComputeTangents(ATexCoord: TAttribType);
var
  a, T, i, J, E, EJ, E_: integer;
  p0, p1, p2, dp0, dp1, st0, st1, st2, dst0, dst1, fTangent, nTangent,
    cTangent: TVector;
  factor: single;
  Vertices, TexCoords, Normals, Binormals, Tangents: TAttribBuffer;
  newTangents, newBinormals, tempList: TVec3List;
  newTangentIndices: TIntegerList;
  collisionMap: TIntIntRBTree;
  Agrees: boolean;

begin
  if FVertexAttribIndex = -1 then
    exit;
  if not(Attribs[FVertexAttribIndex] is TAttribBuffer) then
    exit;

  Vertices := TAttribBuffer(Attribs[FVertexAttribIndex]);
  if Attribs[FVertexAttribIndex].FType <> vtFloat then
    exit;

  TexCoords := FAttribs.GetAttribBuffer(ATexCoord);
  if not assigned(TexCoords) then
    exit;
  if TexCoords.FType <> vtFloat then
    exit;

  BeginUpdate;

  try

    Triangulate;

    newTangents := TVec3List.Create;
    newTangentIndices := TIntegerList.Create;

    // The collision map records any alternate locations for the tangent
    collisionMap := TIntIntRBTree.Create(CompareInteger, CompareInteger_);
    collisionMap.DuplicateKeys := true;
    fTangent.vec4 := VecNull;

    // Iterate over the faces, computing the face normal and summing it them
    for T := 0 to FIndiceCount div 3 - 1 do
    begin
      E := 3 * T;
      p0 := Vertices.Buffer.DataHandler.GetItemAsVector(FIndices[E]);
      p1 := Vertices.Buffer.DataHandler.GetItemAsVector(FIndices[E + 1]);
      p2 := Vertices.Buffer.DataHandler.GetItemAsVector(FIndices[E + 2]);
      st0 := Vertices.Buffer.DataHandler.GetItemAsVector(FIndices[E]);
      st1 := Vertices.Buffer.DataHandler.GetItemAsVector(FIndices[E + 1]);
      st2 := Vertices.Buffer.DataHandler.GetItemAsVector(FIndices[E + 2]);

      // Compute the edge and tc differentials
      dp0 := p1 - p0;
      dp1 := p2 - p0;
      dst0 := st1 - st0;
      dst1 := st2 - st0;

      factor := 1.0 / (dst0[0] * dst1[1] - dst1[0] * dst0[1]);

      // compute fTangent
      fTangent[0] := dp0[0] * dst1[1] - dp1[0] * dst0[1];
      fTangent[1] := dp0[1] * dst1[1] - dp1[1] * dst0[1];
      fTangent[2] := dp0[2] * dst1[1] - dp1[2] * dst0[1];
      fTangent := fTangent.Scale(factor);

      // should this really renormalize?
      nTangent := fTangent.Normalize;

      // Iterate over the vertices, adding the face normal influence to each
      for J := 0 to 2 do
      begin
        // Get the current normal from the default location (index shared with position)
        EJ := FIndices[E + J];
        cTangent.vec3 := newTangents[EJ];

        // Check to see if this normal has not yet been touched
        if cTangent.IsNull then
        begin
          // First instance of this index, just store it as is
          newTangents[EJ] := fTangent.vec3;
          newTangentIndices.Add(EJ);
        end
        else
        begin
          // Check for agreement
          if cTangent.Normalize.Dot(nTangent) >= cos(3.1415926 * 0.333333) then
          begin
            // Normal agrees, so add it
            cTangent := cTangent + fTangent;
            newTangents[EJ] := cTangent.vec3;
            newTangentIndices.Add(EJ);
          end
          else
          begin
            // Normals disagree, this vertex must be along a facet edge
            Agrees := false;
            if collisionMap.Find(EJ, E_) then
            begin
              // Loop through all hits on this index, until one agrees
              repeat
                cTangent := newTangents[E_];
                if cTangent.Normalize.Dot(nTangent) >= cos(3.1415926 * 0.333333)
                then
                begin
                  Agrees := true;
                  break;
                end;
              until not collisionMap.NextDublicate(E_);
            end;

            // Check for agreement with an earlier collision
            if Agrees then
            begin
              // Found agreement with an earlier collision, use that one
              cTangent := cTangent + fTangent;
              newTangents[E_] := cTangent.vec3;
              newTangentIndices.Add(E_);
            end
            else
            begin
              // We have a new collision, create a new normal
              newTangentIndices.Add(newTangents.Count);
              collisionMap.Add(EJ, newTangents.Count);
              newTangents.Add(fTangent.vec3);
            end;
          end; // else ( if tangent agrees)
        end; // else (if tangent is uninitialized)
      end; // for each vertex in triangle
    end; // for each face

    // now normalize all the normals
    newTangents.SetNormalize;

    // Place new tangent
    UnWeldVertices;

    FAttribs.ExtractAttrib(atTangent);
    FAttribs.ExtractAttrib(atBinormal);

    tempList := TVec3List.Create;
    tempList.Count := newTangentIndices.Count;
    for i := 0 to newTangentIndices.Count - 1 do
    begin
      E := newTangentIndices[i];
      E_ := FIndices[i];
      tempList[E_] := newTangents[E];
    end;
    newTangents.Destroy;
    newTangents := tempList;

    Normals := FAttribs.GetAttribBuffer(atNormal);
    if assigned(Normals) then
    begin
      if Normals.Buffer.DataHandler is TVec3List then
      begin
        tempList := TVec3List(Normals.Buffer.DataHandler);
        newBinormals := tempList.Cross(newTangents);
        Binormals := TAttribBuffer.CreateAndSetup(
          CAttribSematics[atBinormal].Name, 3, vtFloat, 0, btArray);
        with Binormals do
        begin
          Buffer.SetDataHandler(newBinormals);
          SetAttribSemantic(atBinormal);
        end;
        AddAttrib(Binormals, false);
      end;
    end;

    Tangents := TAttribBuffer.CreateAndSetup(CAttribSematics[atTangent].Name, 3,
      vtFloat, 0, btArray);
    with Tangents do
    begin
      Buffer.SetDataHandler(newTangents);
      SetAttribSemantic(atTangent);
    end;
    AddAttrib(Tangents, false);

    newTangentIndices.Destroy;
    collisionMap.Destroy;

  finally
    EndUpdate;
  end;
end;

procedure TVertexObject.ComputeTexCoords(ATexCoord: TAttribType);
var
  Vertices: TAbstractDataList;
  NewTexCoords: TVec2List;
  T, E, EJ: integer;
  p0, p1, p2, dp0, dp1, fNormal, fTangent, fBinormal: TVector;
  TBN: TMatrix;
  TexCoords: TAttribBuffer;
begin
  if FVertexAttribIndex = -1 then
    exit;
  Vertices := Attribs[FVertexAttribIndex].Buffer.DataHandler;

  BeginUpdate;

  try
    FAttribs.ExtractAttrib(ATexCoord);

    Triangulate;

    NewTexCoords := TVec2List.Create;
    NewTexCoords.Count := FIndiceCount;

    EJ := 0;
    for T := 0 to FIndiceCount div 3 - 1 do
    begin
      E := 3 * T;
      p0 := Vertices.GetItemAsVector(E);
      p1 := Vertices.GetItemAsVector(E + 1);
      p2 := Vertices.GetItemAsVector(E + 2);

      // Compute the edge vectors
      dp0 := p1 - p0;
      dp1 := p2 - p0;

      // Compute the face TBN
      fNormal := dp0.Cross(dp1);
      fNormal := fNormal.Normalize;
      fTangent := dp0;
      fTangent := fTangent.Normalize;
      fBinormal := fNormal.Cross(fTangent);
      TBN.Row[0] := fTangent.vec4;
      TBN.Row[1] := fBinormal.vec4;
      TBN.Row[2] := fNormal.vec4;
      TBN := TBN.Invert;

      p0 := TBN.Transform(p0);
      NewTexCoords[EJ] := p0.Vec2;
      inc(EJ);

      p1 := TBN.Transform(p1);
      NewTexCoords[EJ] := p1.Vec2;
      inc(EJ);

      p2 := TBN.Transform(p2);
      NewTexCoords[EJ] := p2.Vec2;
      inc(EJ);
    end;

    // Place new texture coordinates
    UnWeldVertices;

    TexCoords := TAttribBuffer.CreateAndSetup(
      CAttribSematics[ATexCoord].Name, 2, vtFloat, 0, btArray);
    with TexCoords do
    begin
      Buffer.SetDataHandler(NewTexCoords);
      SetAttribSemantic(ATexCoord);
    end;
    AddAttrib(TexCoords, false);
  finally
    EndUpdate;
  end;
end;

procedure TVertexObject.ComputeTriangleAdjacency;
type
  Vec4ui = array [0 .. 3] of LongInt;
  PVec4ui = ^Vec4ui;
var
  edgeInfo: TTriangleEdgeInfoArray;
  triangleNum: integer;
  NewIndices: TIntegerList;

  procedure joinTriangles(tri1: integer; edge1: cardinal; tri2: integer;
    edge2: cardinal);
  begin
    assert((edge1 < 3) and (edge2 < 3),
      'joinTriangles: Multiple edge detected.');

    edgeInfo[tri1].adjacentTriangle[edge1] := tri2;
    edgeInfo[tri1].adjacentTriangleEdges := edgeInfo[tri1]
      .adjacentTriangleEdges and not(3 shl (2 * edge1));
    edgeInfo[tri1].adjacentTriangleEdges := edgeInfo[tri1]
      .adjacentTriangleEdges or (edge2 shl (2 * edge1));

    edgeInfo[tri2].adjacentTriangle[edge2] := tri1;
    edgeInfo[tri2].adjacentTriangleEdges := edgeInfo[tri2]
      .adjacentTriangleEdges and not(3 shl (2 * edge2));
    edgeInfo[tri2].adjacentTriangleEdges := edgeInfo[tri2]
      .adjacentTriangleEdges or (edge1 shl (2 * edge2));
  end;

  procedure matchWithTriangleSharingEdge(triangle, edge, v0, v1,
    otherv: Integer);

  var
    i: integer;
    doubleTri: integer;
    otherEdge: integer;
    vertexIndex: PVec4ui;
  begin
    doubleTri := -1;
    otherEdge := 0;
    // Match shared edges based on vertex numbers (relatively fast).
    for i := triangle + 1 to triangleNum - 1 do
    begin
      vertexIndex := NewIndices.GetItemAddr(i * 3);

      if vertexIndex[0] = v0 then
        if vertexIndex[2] = v1 then
          if edgeInfo[i].adjacentTriangle[2] = $FFFFFFFF then
            if vertexIndex[1] = otherv then
            begin
              if (doubleTri < 0) then
              begin
                doubleTri := i;
                otherEdge := 2;
              end;
            end
            else
            begin
              joinTriangles(i, 2, triangle, edge);
              exit;
            end;

      if vertexIndex[1] = v0 then
        if vertexIndex[0] = v1 then
          if edgeInfo[i].adjacentTriangle[0] = $FFFFFFFF then
            if vertexIndex[2] = otherv then
            begin
              if doubleTri < 0 then
              begin
                doubleTri := i;
                otherEdge := 0;
              end;
            end
            else
            begin
              joinTriangles(i, 0, triangle, edge);
              exit;
            end;

      if vertexIndex[2] = v0 then
        if vertexIndex[1] = v1 then
          if edgeInfo[i].adjacentTriangle[1] = $FFFFFFFF then
            if vertexIndex[0] = otherv then
            begin
              if doubleTri < 0 then
              begin
                doubleTri := i;
                otherEdge := 1;
              end;
            end
            else
            begin
              joinTriangles(i, 1, triangle, edge);
              exit;
            end;
    end;

    // Only connect a triangle to a triangle with the exact
    // same three vertices as a last resort.
    if doubleTri >= 0 then
      joinTriangles(doubleTri, otherEdge, triangle, edge);
  end;

  procedure CheckForBogusAdjacency;

    function AdjacentEdge(x, n: integer): integer;
    begin
      result := (x shr (2 * n)) and 3;
    end;

  var
    i, J: integer;
    adjacentTriangle, adjacentTriangleSharedEdge: Integer;
  begin
    for i := 0 to triangleNum - 1 do
      for J := 0 to 2 do
      begin
        adjacentTriangleSharedEdge :=
          AdjacentEdge(edgeInfo[i].adjacentTriangleEdges, J);
        adjacentTriangle := edgeInfo[i].adjacentTriangle[J];
        if adjacentTriangle <> -1 then
        begin
          assert(adjacentTriangleSharedEdge < 3);
          assert(edgeInfo[adjacentTriangle].adjacentTriangle
            [adjacentTriangleSharedEdge] = LongWord(i));
          assert(AdjacentEdge(edgeInfo[adjacentTriangle].adjacentTriangleEdges,
            adjacentTriangleSharedEdge) = J);
        end
        else
          assert(adjacentTriangleSharedEdge = 3);
      end;
  end;

  function AdjacentEdge(x, n: integer): integer;
  begin
    result := (x shr (2 * n)) and 3;
  end;

var
  Vertices: TAbstractDataList;
  i, J, K: integer;
  vertexIndex, tri, adjtri: PVec4ui;
  n, ii, jj: Integer;
begin
  assert(FFaceType in [ftTriangleStrip, ftTriangleFan, ftTriangles]);

  if FVertexAttribIndex = -1 then
    exit;
  Vertices := Attribs[FVertexAttribIndex].Buffer.DataHandler;

  BeginUpdate;

  NewIndices := TIntegerList.Create;

  try

    Triangulate;
    for i := 1 to FIndiceCount - 1 do
    begin
      ii := FIndices[i];
      for J := 0 to i - 1 do
      begin
        jj := FIndices[J];
        if ii = jj then
          continue;
        if Vertices.IsItemsEqual(ii, jj) then
        begin
          FIndices[i] := jj;
          break;
        end;
      end;
    end;

    // Remove degenerate triangles
    triangleNum := 0;
    for i := 0 to FIndiceCount div 3 - 1 do
    begin
      vertexIndex := @FIndices[i * 3];
      if (vertexIndex[0] = vertexIndex[1]) or (vertexIndex[0] = vertexIndex[2])
        or (vertexIndex[1] = vertexIndex[2]) then
        continue;
      NewIndices.Add(vertexIndex[0]);
      NewIndices.Add(vertexIndex[1]);
      NewIndices.Add(vertexIndex[2]);
      inc(triangleNum);
    end;

    // Initialize edge information as if all triangles are fully disconnected.
    setlength(edgeInfo, triangleNum);
    for i := 0 to triangleNum - 1 do
    begin
      edgeInfo[i].adjacentTriangle[0] := $FFFFFFFF; // Vertex 0,1 edge
      edgeInfo[i].adjacentTriangle[1] := $FFFFFFFF; // Vertex 1,2 edge
      edgeInfo[i].adjacentTriangle[2] := $FFFFFFFF; // Vertex 2,0 edge
      edgeInfo[i].adjacentTriangleEdges := (3 shl 0) or (3 shl 2) or (3 shl 4);
      edgeInfo[i].openEdgeMask := 0;
    end;

    try
      for i := 0 to triangleNum - 1 do
      begin
        vertexIndex := NewIndices.GetItemAddr(i * 3);
        if edgeInfo[i].adjacentTriangle[0] = $FFFFFFFF then
          matchWithTriangleSharingEdge(i, 0, vertexIndex[0], vertexIndex[1],
            vertexIndex[2]);
        if edgeInfo[i].adjacentTriangle[1] = $FFFFFFFF then
          matchWithTriangleSharingEdge(i, 1, vertexIndex[1], vertexIndex[2],
            vertexIndex[0]);
        if edgeInfo[i].adjacentTriangle[2] = $FFFFFFFF then
          matchWithTriangleSharingEdge(i, 2, vertexIndex[2], vertexIndex[0],
            vertexIndex[1]);
      end;

      CheckForBogusAdjacency;

      setlength(FAdjacencyIndices, 2 * NewIndices.Count);
      K := 0;

      for i := 0 to triangleNum - 1 do
      begin
        n := 3 * i;
        tri := NewIndices.GetItemAddr(n);
        for J := 0 to 2 do
        begin
          FAdjacencyIndices[K] := tri^[J];
          inc(K);
          n := edgeInfo[i].adjacentTriangle[J];
          if n = -1 then
          begin
            jj := (J + 2) mod 3;
            FAdjacencyIndices[K] := tri^[jj];
            inc(K);
          end
          else
          begin
            n := 3 * n;
            adjtri := NewIndices.GetItemAddr(n);
            ii := (AdjacentEdge(edgeInfo[i].adjacentTriangleEdges, J) +
              2) mod 3;
            FAdjacencyIndices[K] := adjtri^[ii];
            inc(K);
          end;
        end;
      end;
    except
      FAdjacencyIndices := nil;
    end;
    NewIndices.Destroy;
  finally
    EndUpdate;
  end;
end;

constructor TVertexObject.Create;
begin
  inherited;
  FAttribs := TAttribList.Create;
  FAttribs.FreeingBehavior := fbManual;
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

procedure TVertexObject.Join(AnObject: TVertexObject; const AMatrix: TMatrix);
var
  I, J, IndexOffset, len: Integer;
  attr: TAttribBuffer;
  NMat: TMatrix;
  IsIdent: Boolean;
begin
  Assert(AnObject.FaceType = FaceType);
  if AnObject.FAttribs.Count = 0 then
    exit;

  IsIdent := AMatrix.IsIdentity;
  if not IsIdent then
    NMat := AMatrix.Normalize;

  BeginUpdate;
  try

    if FAttribs.Count = 0 then
    begin
      FVertexAttribIndex := AnObject.FVertexAttribIndex;
      for I := 0 to AnObject.FAttribs.Count - 1 do
      begin
        attr := TAttribBuffer.CreateClone(AnObject.Attribs[I]);
        FAttribs.Add(attr);
        attr.Subscribe(Self);
        if not IsIdent then
          case attr.FSemantic of
            atVertex: attr.Buffer.DataHandler.Transform(AMatrix);
            atNormal,
            atTangent,
            atBinormal: attr.Buffer.DataHandler.Transform(NMat);
          end;
      end;
      FIndiceCount := AnObject.IndiceCount;
      if FIndiceCount > 0 then
        FIndices := Copy(AnObject.FIndices, 0, FIndiceCount);
      FRestartIndex := AnObject.FRestartIndex;
      FAdjacencyIndices := nil;
      exit;
    end
    else
    begin
      for I := 0 to FAttribs.Count - 1 do
      begin
        attr := AnObject.FAttribs.GetAttribBuffer(Attribs[I].Semantic);
        if Assigned(attr) and Attribs[I].IsEqual(attr) then
           Attribs[I].TagObject := attr
        else
        begin
          // Удаляем несовпадающие атрибуты
          Attribs[I].UnSubscribe(Self);
          FAttribs[I] := nil;
        end;
      end;
      FAttribs.Pack;
    end;

    if FAttribs.Count > 0 then
    begin
      IndexOffset := FAttribs.First.FBuffer.DataHandler.Count;

      for I := 0 to FAttribs.Count - 1 do
      begin
        attr := TAttribBuffer(Attribs[I].TagObject);
        if not IsIdent then
        begin
          case attr.FSemantic of
            atVertex: Attribs[I].FBuffer.DataHandler.Join(attr.FBuffer.DataHandler, AMatrix);
            atNormal,
            atTangent,
            atBinormal: Attribs[I].FBuffer.DataHandler.Join(attr.Buffer.DataHandler, NMat);
          else
            Attribs[I].FBuffer.DataHandler.Join(attr.FBuffer.DataHandler, TMatrix.IdentityMatrix);
          end;
        end
        else
          Attribs[I].FBuffer.DataHandler.Join(attr.FBuffer.DataHandler, TMatrix.IdentityMatrix);
      end;

      len := Length(AnObject.FIndices);
      if Length(FIndices) <= FIndiceCount + len then
        setlength(FIndices, FIndiceCount + len + 10);
      if len > 0 then
        Move(AnObject.FIndices[0], FIndices[FIndiceCount], SizeOf(Integer)*len);
      for J := 0 to High(AnObject.FIndices) do
        if AnObject.FIndices[J] = AnObject.FRestartIndex then
          FIndices[J+FIndiceCount] := FRestartIndex
        else
          Inc(FIndices[J+FIndiceCount], IndexOffset);
      FIndiceCount := FIndiceCount + AnObject.FIndiceCount;
    end;

    FAdjacencyIndices := nil;
  finally
    EndUpdate;
  end;
end;

procedure TVertexObject.LineSegmentation;
begin
  Assert(False);
end;

function TVertexObject.MakeTrianglesIndices: TIntegerArray;
var
  NewElements: TIntegerList;
  J, E: integer;
  stripCount, prevIndex1, prevIndex2: integer;
  prevIndex, centerIndex, fansCount: integer;
  degenerate: boolean;
begin
  case FFaceType of
    ftTriangleStrip:
      begin
        NewElements := TIntegerList.Create;
        stripCount := 0;
        prevIndex1 := 0;
        prevIndex2 := 0;
        for J := 0 to FIndiceCount - 1 do
        begin
          E := FIndices[J];
          if stripCount > 2 then
          begin
            // Check for restart index
            if J = FRestartIndex then
            begin
              stripCount := 0;
              continue;
            end
            // Check for degenerate triangles
            else if E = prevIndex1 then
            begin
              continue;
            end
            else if prevIndex1 = prevIndex2 then
            begin
              stripCount := 0;
              continue;
            end;
            if (stripCount and 1) = 0 then
            begin
              NewElements.Add(prevIndex2);
              NewElements.Add(prevIndex1);
            end
            else
            begin
              NewElements.Add(prevIndex1);
              NewElements.Add(prevIndex2);
            end;
          end
          else if stripCount = 2 then
          begin
            NewElements.Add(E);
            NewElements[NewElements.Count - 2] := prevIndex1;
            prevIndex2 := prevIndex1;
            prevIndex1 := E;
            inc(stripCount);
            continue;
          end;
          NewElements.Add(E);
          prevIndex2 := prevIndex1;
          prevIndex1 := E;
          inc(stripCount);
        end;
        NewElements.ToArray(result);
        NewElements.Destroy;
      end;
    ftTriangleFan:
      begin
        NewElements := TIntegerList.Create;
        fansCount := 0;
        prevIndex := 0;
        degenerate := false;
        centerIndex := FIndices[0];
        for J := 0 to FIndiceCount - 1 do
        begin
          E := FIndices[J];
          if fansCount > 2 then
          begin
            // Check for restart index
            if E = FRestartIndex then
            begin
              fansCount := 0;
              continue;
            end
            // Check for degenerate triangles
            else if E = prevIndex then
            begin
              degenerate := true;
              continue;
            end
            else if degenerate then
            begin
              degenerate := false;
              fansCount := 0;
              continue;
            end;
            NewElements.Add(centerIndex);
            NewElements.Add(prevIndex);
          end
          else if fansCount = 0 then
            centerIndex := E;
          NewElements.Add(E);
          prevIndex := E;
          inc(fansCount);
        end;
        NewElements.ToArray(result);
        NewElements.Destroy;
      end;
    ftTriangles:
      result := FIndices;
  end;
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
  FAttribs.UnSubscribe(Self);
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

procedure TVertexObject.Transform(const AMatrix: TMatrix);
var
  attr: TAttribBuffer;
  NMat: TMatrix;
begin
  BeginUpdate;
  try
    NMat := AMatrix.Normalize;
    attr := FAttribs.GetAttribBuffer(atVertex);
    if Assigned(attr) then
      attr.Buffer.DataHandler.Transform(AMatrix);

    attr := FAttribs.GetAttribBuffer(atNormal);
    if Assigned(attr) then
      attr.Buffer.DataHandler.Transform(NMat);

    attr := FAttribs.GetAttribBuffer(atTangent);
    if Assigned(attr) then
      attr.Buffer.DataHandler.Transform(NMat);

    attr := FAttribs.GetAttribBuffer(atBinormal);
    if Assigned(attr) then
      attr.Buffer.DataHandler.Transform(NMat);
  finally
    EndUpdate;
  end;
end;

procedure TVertexObject.Triangulate;
begin
  if FFaceType in [ftTriangleStrip, ftTriangleFan] then
  begin
    FIndices := MakeTrianglesIndices();
    FIndiceCount := Length(FIndices);
    FFaceType := ftTriangles;
    FIndiceChanged := True;
    DispatchMessage(NM_ResourceChanged);
  end;
end;

procedure TVertexObject.UnWeldVertices;
var
  NewBuffers: array of TAbstractDataList;
  i, a, E: integer;
begin
  setlength(NewBuffers, FAttribs.Count);
  for a := 0 to FAttribs.Count - 1 do
    NewBuffers[a] := TAbstractDataListClass(Attribs[a].Buffer.DataHandler.ClassType).Create;

  for i := 0 to FIndiceCount - 1 do
  begin
    E := FIndices[i];
    for a := 0 to FAttribs.Count - 1 do
      NewBuffers[a].AddRaw(Attribs[a].Buffer.DataHandler.GetItemAddr(E));
    FIndices[i] := i;
  end;

  for a := 0 to AttribsCount - 1 do
    Attribs[a].Buffer.SetDataHandler(NewBuffers[a]);

  FStructureChanged := True;
  FIndiceChanged := True;
  DispatchMessage(NM_ResourceChanged);
end;

function CompareVertexKey(const Item1, Item2: Double): integer;
begin
  if Item1 < Item2 then
    exit(-1)
  else if Item1 = Item2 then
    exit(0)
  else
    result := 1;
end;

var
  vVertexObject: TVertexObject;

function CompareVertex(const Item1, Item2: integer): boolean;
var
  a: integer;
  Idx1, Idx2: integer;
begin
  if Item1 <> Item2 then
  begin
    Idx1 := vVertexObject.FIndices[Item1];
    Idx2 := vVertexObject.FIndices[Item2];
    for a := 0 to vVertexObject.AttribsCount - 1 do
      if not vVertexObject.Attribs[a].FBuffer.DataHandler.IsItemsEqual(Idx1,
        Idx2) then
        exit(false);
  end;
  result := true;
end;

procedure TVertexObject.WeldVertices;
var
  i, J, len: integer;
  E, E_, Index: integer;
  vertexKey: Double;
  VertexHashMap: TVertexHashMap;
  VertexHashKey: TDoubleList;
  bFind: boolean;
  StoreBuffers: array of TAbstractDataList;
  StoreIndices: TIntegerArray;

  procedure CalcHashKay;
  var
    pKey, pVertex: PByteArray;
    K, P, W: Integer;
  begin
    vertexKey := 0;
    pKey := @vertexKey;
    P := 0;
    pVertex := StoreBuffers[FVertexAttribIndex].GetItemAddr(E);
    for K := len - 1 downto 0 do
    begin
      W := pKey[P] + pVertex[K];
      pKey[P] := W and 255;
      inc(P);
      P := P and 7;
    end;
    VertexHashKey[i] := vertexKey;
  end;

  function IsVertexEqual(const Index1, Index2: integer): boolean;
  var
    a: integer;
  begin
    if Index1 <> Index2 then
    begin
      for a := 0 to High(StoreBuffers) do
        if not StoreBuffers[a].IsItemsEqual(Index1, Index2) then
          exit(false);
    end;
    result := true;
  end;

  procedure CopyVertex(n: integer);
  var
    a: integer;
  begin
    for a := 0 to AttribsCount - 1 do
      Attribs[a].Buffer.DataHandler.AddRaw(StoreBuffers[a].GetItemAddr(n));
    inc(Index);
  end;

var
  HasHash: boolean;
  a: integer;

begin
  // Calculate hash keys
  VertexHashMap := TVertexHashMap.Create(CompareVertexKey, CompareVertex);
  VertexHashKey := TDoubleList.Create;
  VertexHashKey.Count := FIndiceCount;
  vVertexObject := Self;
  E_ := 0; // Drop compilator warning
  len := Attribs[FVertexAttribIndex].FSize * CValueSizes[Attribs[FVertexAttribIndex].FType];
  SetLength(StoreBuffers, AttribsCount);
  for a := 0 to AttribsCount - 1 do
    StoreBuffers[a] := Attribs[a].Buffer.DataHandler;
  StoreIndices := Copy(FIndices, 0, FIndiceCount);

  for i := 0 to FIndiceCount - 1 do
  begin
    E := FIndices[i];
    if E = FRestartIndex then
      continue;
    CalcHashKay;
    if VertexHashMap.Find(vertexKey, J) then
    begin
      E_ := FIndices[J];
      HasHash := (E_ >= E) or not IsVertexEqual(E, E_);
    end
    else
      HasHash := true;
    if HasHash then
      VertexHashMap.Add(vertexKey, i);
  end;

  for a := 0 to AttribsCount - 1 do
    Attribs[a].Buffer.SetDataHandler(TAbstractDataListClass
      (Attribs[a].Buffer.DataHandler.ClassType).Create);

  // Remap element buffer, fill new attributes list
  Index := 0;
  for i := 0 to FIndiceCount - 1 do
  begin
    E := FIndices[i];
    if E = FRestartIndex then
      continue;

    bFind := false;
    vertexKey := VertexHashKey[i];
    if VertexHashMap.Find(vertexKey, J) then
    begin
      repeat
        E_ := StoreIndices[J];
        bFind := IsVertexEqual(E, E_);
        if bFind then
          break;
      until not VertexHashMap.NextDublicate(J);
    end;
    if not bFind then
      E_ := E;

    if E_ >= E then
    begin
      FIndices[i] := Index;
      CopyVertex(E);
    end
    else
    begin
      FIndices[i] := FIndices[J];
    end;
  end;

  // Free unpacked arrays
  for a := 0 to High(StoreBuffers) do
    StoreBuffers[a].UnSubscribe(Self);
  FIndiceCount := Length(FIndices);

  VertexHashMap.Destroy;
  VertexHashKey.Destroy;
  FStructureChanged := True;
  FIndiceChanged := True;
  DispatchMessage(NM_ResourceChanged);
end;

{ TMeshObject }

constructor TMeshObject.Create;
begin
  inherited;
  FLods := TListOfMeshList.Create;
  FOccluder := TMeshList.Create;
  FCollider := TTriangleList.Create;
end;

destructor TMeshObject.Destroy;
begin
  FLods.Free;
  FOccluder.Free;
  FCollider.Free;
  inherited;
end;

function TMeshObject.GetMeshLod(const aLod: integer): TMeshList;
begin
  if (FLods.Count = 0) or (aLod < 0) then
  begin
    result := nil;
    exit;
  end;
  if aLod < FLods.Count then
    result := FLods[aLod]
  else
    result := FLods[FLods.Count - 1];
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
  result := TMesh.CreateFrom(aVertexObject);
  FList.Add(result);
end;

constructor TMeshList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TMeshList.Destroy;
begin
  FreeList(FList);
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
  GUID := aGUID;
  FriendlyName := '';
end;

constructor TMesh.CreateFrom(const aVertexObject: TVertexObject);
begin
  Create;
  CreateGUID(GUID);
  FriendlyName := '';
end;

constructor TMesh.CreateFrom(const aVertexObject: TVertexObject; aName: string);
begin
  Create;
  CreateGUID(GUID);
  FriendlyName := aName;
end;

{ TShaderProgram }

constructor TShaderProgram.CreateOwned(aOwner: TObject);
var
  st: TShaderType;
begin
  inherited Create;
  FOwner := aOwner;
  for st := Low(FShaderText) to high(FShaderText) do
    FShaderText[st] := '';
  FBinary.Size := 0;
  FFragDataBindPos.Name := '';
  FFragDataBindPos.Location := -1;
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
  if assigned(FMaterial) and (FMaterial.Owner = Self) then
    FMaterial.Free;
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
  FTexture := TTexture.CreateOwned(Self);
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

function TMeshObjectsList.AddMeshObject(const aMeshObject: TMeshObject)
  : integer;
begin
  result := AddKey(aMeshObject.GUID, aMeshObject);
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
  for i := 0 to FCount - 1 do
  begin
    mo := TMeshObject(FItems[i].Value);
    if mo = aMeshObject then
    begin
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

{ TSceneGraph }

function TSceneGraph.AddItem(aItem: TBaseSceneItem): integer;
begin
  result := FItemsList.Add(aItem);
end;

function TSceneGraph.AddLight(aLight: TLightSource): integer;
begin
  result := FLights.Add(aLight);
end;

function TSceneGraph.AddMaterial(aMat: TMaterialObject): integer;
begin
  result := FMaterials.Add(aMat);
end;

function TSceneGraph.AddNewMaterial(aName: string): TMaterialObject;
var
  aMat: TMaterialObject;
begin
  { TODO : Проверить на существование в библиотеке материала с таким именем }
  aMat := TMaterialObject.Create;
  aMat.Name := aName;
  result := aMat;
end;

constructor TSceneGraph.Create;
begin
  FItemsList := TList.Create;
  FLights := TList.Create;
end;

destructor TSceneGraph.Destroy;
begin
  FreeObjectList(FItemsList);
  inherited;
end;

function TSceneGraph.getCount: integer;
begin
  result := FItemsList.Count;
end;

function TSceneGraph.getItem(Index: integer): TBaseSceneItem;
begin
  result := TBaseSceneItem(FItemsList[index]);
end;

function TSceneGraph.getLight(Index: integer): TLightSource;
begin
  result := FLights[Index];
end;

function TSceneGraph.getLightCount: integer;
begin
  result := FLights.Count;
end;

function TSceneGraph.getMatCount: integer;
begin
  result := FMaterials.Count;
end;

function TSceneGraph.getMaterial(Index: integer): TMaterialObject;
begin
  result := TMaterialObject(FMaterials[index]);
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

initialization

vRegisteredResource := TRegisteredResource.Create;

finalization

vRegisteredResource.Free;

end.
