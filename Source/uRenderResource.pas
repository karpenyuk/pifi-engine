﻿{ TODO :
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
  uGenericsRBTree, uPersistentClasses, uDataAccess;

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
      BuffType: TBufferType = btArray); virtual;
    constructor CreateClone(ASample: TAttribObject);

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
      AType: TValueType = vtFloat; AStride: integer = 0;
      BuffType: TBufferType = btArray);
begin
  Create;
  FSize := aSize;
  FAttribName := AttrName;
  FType := AType;
  FStride := AStride;
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
