unit uFileFormatDAE;

interface

uses
  uRenderResource, uStorage;

type

  FileFormatDAE = class
    class function LoadAndCreateVertexObject(const aFileName: string): TVertexObject;
  end;

implementation

uses
  Types,
  uXmlParser,
  uDAESchema,
  uMiscUtils,
  uBaseTypes,
  uLists;

type

  TInputSemantic = (
    isPOSITION,   //Geometric coordinate vector.
    isVERTEX,     //Mesh vertex
    isTANGENT,    //Geometric tangent vector
    isBINORMAL,   //Geometric binormal (bitangent) vector
    isNORMAL,     //Normal vector
    isCOLOR,      //Color coordinate vector. Color inputs are RGB (float3)
    isTEXCOORD,   //Texture coordinate vector

    isIMAGE,      // Raster or MIP-level input.

    //See “Curve Interpolation” in Chapter 4: Programming Guide.
    isINPUT,        //Sampler input.
    isIN_TANGENT,   //Tangent vector for preceding control point.
    isCONTINUITY,   //Continuity constraint at the control vertex (CV)
    isLINEAR_STEPS, //Number of steps to use for the spline segment that follows this CV.
    isINTERPOLATION,//Sampler interpolation type.
    isOUTPUT,       // Sampler output.
    isOUT_TANGENT,  //Tangent vector for succeeding control point.

    isINV_BIND_MATRIX,//Inverse of local-to-world matrix.
    isJOINT,          // Skin influence identifier
    isMORPH_TARGET,   //Morph targets for mesh morphing
    isMORPH_WEIGHT,   //Weights for mesh morphing
    isWEIGHT,         //Skin influence weighting value

    isTEXBINORMAL,    //Texture binormal (bitangent) vector
    isTEXTANGENT,     //Texture tangent vector
    isUV              //Generic parameter vector
  );

  TDAEVertexSource = record
    Id: String;
    Data: TSingleDynArray;
    List: TAbstractDataList;
    Type_: TValueType;
    Components: TValueComponent;
    Stride: Integer;
    procedure Init(aComponents: integer; aList: TAbstractDataList);
  end;

  TDAEVertexSourceDynArray = array of TDAEVertexSource;

  TSourceSemantic = record
    AttribType: TAttribType;
    Source: TDAEVertexSource;
    Offset: Integer;
  end;

  TSourceSemanticDynArray = array of TSourceSemantic;

  TDAESourceRecord = record
    ID: string;
    Accessor: record
      SourceId: string;
      Count, Offset, Stride: integer;
      ParamIndex: array of integer; //Skip unnamed index
    end;
    Value: TSingleDynArray;
    procedure Init(aSource: IXMLSource_type);
  end;
  PDAESourceRecord = ^TDAESourceRecord;

  procedure TDAESourceRecord.Init(aSource: IXMLSource_type);
  var i,n: integer;
  begin
    ID := aSource.Id; Value := nil;
    Accessor.SourceId := aSource.Technique_common.Accessor.Source;
    Accessor.Offset := aSource.Technique_common.Accessor.Offset;
    Accessor.Stride := aSource.Technique_common.Accessor.Stride;
    n:=0; //counting all named params
    for i:=0 to aSource.Technique_common.Accessor.Count-1 do
      if aSource.Technique_common.Accessor.Param[i].name<>'' then inc(n);
    //extract params index
    Accessor.Count := n; n:=0;
    setlength(Accessor.ParamIndex, Accessor.Count);
    for i:=0 to aSource.Technique_common.Accessor.Count-1 do begin
      if aSource.Technique_common.Accessor.Param[i].name<>'' then begin
        Accessor.ParamIndex[n]:=i; inc(n);
      end;
    end;
    if sizeof(aSource.Float_array.Content.Value) > 0
    then Value := FloatStringsToSingleDynArray(aSource.Float_array.Content.Value);
  end;

  TDAESources = class(TDataList<TDAESource>)
  private
    function getSourceById(Id: string): PDAESourceRecord;
  public
    function SourceValue(ID: string): TFloatDynArray;
    property Source[Id: string]: PDAESourceRecord read getSourceById; default;
  end;

  function TDAESources.SourceValue(ID: string): TFloatDynArray;
  var i: integer;
      sId: string;
      S: PDAESourceRecord;
  begin
    S := Source[Id]; result := nil;
    assert(assigned(S), 'Source with ID "'+ID+'" not found in mesh');
    sId := S.Accessor.SourceId;
    for i:=0 to Count-1 do
      if Items[i].Id = sId then
        if assigned(Items[i].Value) then exit(Items[i].Value);
  end;

(*
  TDAEMeshSources = class
  private
    FSources: TSourceSemanticDynArray;
    FCount: integer;
    FMesh: IXMLMesh_type;
    function getSourceById(ID: string): TSourceSemantic;
    function LoadFloatSource(aSource: IXMLSource_type): TDAEVertexSource;
    function GetId(const AName: String): String;
    function CreateAttribBySemantic(aSemantic: string): TAttribObject;
    function CreateSubMesh(aSubMesh: IXMLTriangles_type): TSubMesh; overload;
  public
    constructor Create(aMesh: IXMLMesh_type);
    property Count: integer read FCount;
    property Source[ID: string]: TSourceSemantic read getSourceById;
  end;

  TSemanticsArray  = record
    Name: string; Supported: boolean; AttrType: TAttribType;
  end;

const
  cInputSemantic: array[isPOSITION..isUV] of TSemanticsArray = (
    (Name:'POSITION'; Supported: true; AttrType: atVertex),
    (Name:'VERTEX'; Supported: false; AttrType: atVertex),
    (Name:'TANGENT'; Supported: true; AttrType: atTangent),
    (Name:'BINORMAL'; Supported: true; AttrType: atBinormal),
    (Name:'NORMAL'; Supported: true; AttrType: atNormal),
    (Name:'COLOR'; Supported:  true; AttrType: atColor),
    (Name:'TEXCOORD'; Supported: true; AttrType: atTexCoord0),
    (Name:'IMAGE'; Supported: false; AttrType: atUserAttrib),
    (Name:'INPUT'; Supported: false; AttrType: atUserAttrib),
    (Name:'IN_TANGENT'; Supported: false; AttrType: atUserAttrib),
    (Name:'CONTINUITY'; Supported: false; AttrType: atUserAttrib),
    (Name:'LINEAR_STEPS'; Supported: false; AttrType: atUserAttrib),
    (Name:'INTERPOLATION'; Supported: false; AttrType: atUserAttrib),
    (Name:'OUTPUT'; Supported: false; AttrType: atUserAttrib),
    (Name:'OUT_TANGENT'; Supported: false; AttrType: atUserAttrib),
    (Name:'INV_BIND_MATRIX'; Supported: false; AttrType: atUserAttrib),
    (Name:'JOINT'; Supported: false; AttrType: atUserAttrib),
    (Name:'MORPH_TARGET'; Supported: false; AttrType: atUserAttrib),
    (Name:'MORPH_WEIGHT'; Supported: false; AttrType: atUserAttrib),
    (Name:'WEIGHT'; Supported: false; AttrType: atUserAttrib),
    (Name:'TEXBINORMAL'; Supported: false; AttrType: atUserAttrib),
    (Name:'TEXTANGENT'; Supported: false; AttrType: atUserAttrib),
    (Name:'UV'; Supported: false; AttrType: atUserAttrib)
  );

function isSupportedSemantic(aSemantic: string): boolean;
var i: TInputSemantic;
    s: string;
begin
  s:=uppercase(aSemantic);
  for i :=  low(TInputSemantic) to high(TInputSemantic) do
    if cInputSemantic[i].Name=s then exit(cInputSemantic[i].Supported);
  result := false;
end;

function GetSemantic(const Input: IXMLInput_local_offset_typeList;
  const ASources: TDAEVertexSourceDynArray): TSourceSemanticDynArray;
var j: Integer;
  function FindSource(ASourceName: String): TDAEVertexSource;
  var i: Integer;
  begin
    for i := 0 to High(ASources) do begin
      if (ASourceName = '#' + ASources[i].Id) or (ASourceName = ASources[i].Id)
      then exit(ASources[i]);
    end;
    Result := ASources[0]; // if not found Result POSITION attribute
  end;

begin
  SetLength(Result, Input.Count);
  for j := 0 to Input.Count - 1 do begin
    Result[j].Offset := Input[j].Offset;
    Result[j].Source := FindSource(Input[j].Source);
    if Input[j].Semantic = 'VERTEX' then
      Result[j].AttribType := TAttribType.atVertex
    else if Input[j].Semantic = 'NORMAL' then
      Result[j].AttribType := TAttribType.atNormal
    else if Input[j].Semantic = 'COLOR' then
      Result[j].AttribType := TAttribType.atColor
    else if Input[j].Semantic = 'TEXCOORD' then
      Result[j].AttribType := TAttribType.atTexCoord0;
  end;
end;

class function FileFormatDAE.LoadAndCreateVertexObject(const aFileName: string): TVertexObject;
var
  LVO: TVertexObject;
  LCOLLADA: IXMLCOLLADA;
  LGeometries: IXMLLibrary_geometries_type;
  LGeometry: IXMLGeometry_type;
  LMeshType: IXMLMesh_type;
  LAccessor: IXMLAccessor_type;
  LSource: IXMLSource_type;
  LSources: TDAEMeshSources;
  LIndices: TIntegerDynArray;
  LSemantics: TSourceSemanticDynArray;
  LSubMesh: IXMLTriangles_type;
  sType: string;
  attr: TAttribBuffer;
  i, j, k, Offset, NumSemantic, Index: Integer;

  function GetId(const AName: String): String;
  var n: Integer;
  begin
    Result := AName;
    for n := 0 to LMeshType.Vertices.Input.Count - 1 do
      if LMeshType.Vertices.Input[n].Source = '#' + AName then
        Result := LMeshType.Vertices.Id;
  end;

//  function LoadTriangles(aTriangles: IXMLTriangles_type): TIndices

begin
  LVO := TVertexObject.Create;

  try
    LCOLLADA := TXMLCOLLADA.Load(aFileName) as IXMLCOLLADA;
    if (LCOLLADA.Library_geometries.Count > 0) then begin
      LGeometries := LCOLLADA.Library_geometries[0];
      LGeometry :=  LGeometries.Geometry[0];
      LMeshType := LGeometry.Mesh;

      //Get Mesh Sources (buffer values)
      LSources:=TDAEMeshSources.Create(LMeshType);

//PrimitiveTypes
      if LMeshType.Triangles.Count > 0 then begin
        LSubMesh := LMeshType.Triangles[0];
        LIndices := IntStringsToIntegerDynArray(LSubMesh.P.Content.Value);
        LSemantics := GetSemantic(LSubMesh.Input, LSources);

//Add triangle vertices
        Offset := 0;
        NumSemantic := LSubMesh.Input.Count;
        Index := 0;
        while (Offset < Length(LIndices)) do begin
          for k := 0 to High(LSemantics) do begin
            for j := 0 to 2 do begin
              i := LIndices[Offset + j * NumSemantic + LSemantics[k].Offset];
              i := i * LSemantics[k].Source.Stride;
              LSemantics[k].Source.List.AddRaw(@LSemantics[k].Source.Data[i]);
            end;
          end;
          Inc(Offset, NumSemantic * 3);
          LVO.AddTriangle(Index, Index + 1, Index + 2);
          Inc(Index, 3);
        end;
//=========================
        for k := 0 to High(LSemantics) do
          with LSemantics[k].Source do begin
            attr := TAttribBuffer.CreateAndSetup
              (CAttribSematics[LSemantics[k].AttribType].Name, Ord(Components));
            attr.SetAttribSemantic(LSemantics[k].AttribType);
            attr.Buffer.Allocate(List.Count * List.ItemSize,
              List.GetItemAddr(0));
            attr.Buffer.SetDataHandler(List, True);
            LVO.AddAttrib(attr);
          end;
      end;
    end;

  except
    LVO.Destroy;
    raise;
  end;

  Result := LVO;
end;

function CreateBufferFromSource(aSource: IXMLSource_type; aId: string): TBufferObject;
var data: TSingleDynArray;
begin
  if sizeof(aSource.Float_array.Content.Value)=0 then exit(nil);
  result:=TBufferObject.Create;
  result.Name:=aId;
  data:=FloatStringsToSingleDynArray(aSource.Float_array.Content.Value);

  //
end;


{ TDAEVertexSource }

procedure TDAEVertexSource.Init(aComponents: integer; aList: TAbstractDataList);
begin
  Components := aComponents;
  List := aList;
end;

{ TDAEMeshSources }

function TDAEMeshSources.CreateAttribBySemantic(aSemantic: string): TAttribObject;
begin
  if not isSupportedSemantic(aSemantic) then result := nil;
  result := Storage.CreateAttribObject;
end;

function TDAEMeshSources.CreateSubMesh(aSubMesh: IXMLTriangles_type): TSubMesh;
var i: integer;
    ao: TAttribObject;
    Indices: TIntegerDynArray;
begin
  Indices := IntStringsToIntegerDynArray(aSubMesh.P.Content.Value);
  for i:=0 to aSubMesh.Input.Count-1 do begin
    ao:=CreateAttribBySemantic(aSubMesh.Input[i].Semantic);
    if assigned(ao) then begin
    end;
  end;

end;

function TDAEMeshSources.GetId(const AName: String): String;
var n: Integer;
begin
  Result := AName;
  for n := 0 to FMesh.Vertices.Input.Count - 1 do
    if FMesh.Vertices.Input[n].Source = '#' + AName then
      Result := FMesh.Vertices.Id;
end;

function TDAEMeshSources.LoadFloatSource(aSource: IXMLSource_type): TDAEVertexSource;
var j: integer;
begin
  assert(sizeof(aSource.Float_array.Content.Value)>0,
    #13+#10+'Incorrect source type: '+aSource.Id+#13+#10);
  for j := 0 to aSource.Technique_common.Accessor.Count - 1 do
    Assert(LowerCase(string(aSource.Technique_common.Accessor.Param[j].Type_)) = 'float' );

  result.Id := GetId(aSource.Id);
  result.Data := FloatStringsToSingleDynArray(aSource.Float_array.Content.Value);
  result.Components := aSource.Technique_common.Accessor.Count;
  result.Type_ := vtFloat;
  result.Stride := aSource.Technique_common.Accessor.Stride;
end;

constructor TDAEMeshSources.Create(aMesh: IXMLMesh_type);
var i: integer;
begin
  FMesh := aMesh;
  FCount := FMesh.Source.Count;
  setlength(FSources, FCount);
  for i:=0 to FCount-1 do FSources[i] := LoadFloatSource(FMesh.Source[i]);
end;

function TDAEMeshSources.getSourceById(ID: string): TSourceSemantic;
var i: integer;
begin
  for i:=0 to FCount-1 do begin
    if FSources[i].Source.Id = ID then exit(FSources[i]);
  end;
  result:=nil;
end;
*)
end.

{
        case LSource.Technique_common.Accessor.Count of
          1: LSources[i].Init(LSource.Technique_common.Accessor.Count, TSingleList.Create);
          2: LSources[i].Init(LSource.Technique_common.Accessor.Count, TVec2List.Create);
          3: LSources[i].Init(LSource.Technique_common.Accessor.Count, TVec3List.Create);
          4: LSources[i].Init(LSource.Technique_common.Accessor.Count, TVec4List.Create);
        else
          Assert(False);
        end;
}

