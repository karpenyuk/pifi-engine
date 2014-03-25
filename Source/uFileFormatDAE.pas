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

  TDAESubMesh = record
    PrimitiveType: TFaceType;
    Attribs: array of record
      Semantic: string;
      SourceId: string;
      Offset: int64;
      SetIndex: integer;
    end;
    Indices: TIntegerDynArray;
    procedure Init(aTriangles: IXMLTriangles_type); overload;
    procedure Init(aLines: IXMLLines_type); overload;
    procedure Init(aLineStrips: IXMLLineStrips_type); overload;
    procedure Init(aPolygons: IXMLPolygons_type); overload;
    //procedure Init(aPolyList: IXMLPolyList_type); overload;
    procedure Init(aTriFans: IXMLTriFans_type); overload;
    procedure Init(aTriStrips: IXMLTriStrips_type); overload;
  end;

  TDAESourceRecord = record
    ID: string;
    Accessor: record
      SourceId: string;
      Count, Offset, Stride: integer;
      ParamIndex: array of integer; //Skip unnamed index
    end;
    Value: TSingleDynArray;
    References: array of ^TDAESubMesh;
    procedure Init(aSource: IXMLSource_type);
  end;
  PDAESourceRecord = ^TDAESourceRecord;

  TDAESources = class(TDataList<TDAESourceRecord>)
  private
    FSources: IXMLSource_typeList;
    function getSourceById(Id: string): PDAESourceRecord;
  public
    constructor Create(aSources: IXMLSource_typeList);
    function SourceValue(ID: string): TSingleDynArray;
    property Source[Id: string]: PDAESourceRecord read getSourceById; default;
  end;

  TDAEMesh = class
  private
    FSources: TDAESources;
    FMesh: IXMLMesh_type;
    FSubMeshes: TDataList<TDAESubMesh>;
    procedure AddSubMesh(const sm: TDAESubMesh);
  public
    constructor Create(aMesh: IXMLMesh_type);
  end;

  procedure TDAEMesh.AddSubMesh(const sm: TDAESubMesh);
  var i,n,mi: integer;
      SourceRec: PDAESourceRecord;
  begin
    mi := FSubMeshes.Add(sm);
    for i:=0 to length(sm.Attribs)-1 do begin
      SourceRec := FSources[sm.Attribs[i].SourceId];
      if assigned(SourceRec) then begin
        n:=length(SourceRec.References);
        setlength(SourceRec.References,n+1);
        SourceRec.References[n]:=@FSubMeshes[mi];
      end;
    end;
  end;

  constructor TDAEMesh.Create(aMesh: IXMLMesh_type);
  var i: integer;
      sm: TDAESubMesh;
  begin
    FMesh := aMesh;
    FSources:=TDAESources.Create(FMesh.Source);
    FSubMeshes:=TDataList<TDAESubMesh>.Create;
    for i:=0 to aMesh.Triangles.Count-1 do begin
      sm.Init(aMesh.Triangles[i]); AddSubMesh(sm);
    end;

    for i:=0 to aMesh.Lines.Count-1 do begin
      sm.Init(aMesh.Lines[i]); AddSubMesh(sm);
    end;

    //Repeat for left primitive types
  end;

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
    if length(aSource.Float_array.Content.Value) > 0
    then Value := FloatStringsToSingleDynArray(aSource.Float_array.Content.Value);
    References := nil;
  end;

  function TDAESources.SourceValue(ID: string): TSingleDynArray;
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

  function TDAESources.getSourceById(Id: string): PDAESourceRecord;
  var i: integer;
  begin
    result := nil;
    for i:=0 to Count-1 do
      if Items[i].ID = Id then exit(@Items[i]);
  end;

  constructor TDAESources.Create(aSources: IXMLSource_typeList);
  var i: integer;
      SourceRec: TDAESourceRecord;
  begin
    FSources := aSources;
    for i:=0 to FSources.Count - 1 do begin
      SourceRec.Init(FSources[i]);
      Add(SourceRec);
    end;
  end;

  procedure TDAESubMesh.Init(aTriangles: IXMLTriangles_type);
  var i: integer;
  begin
    PrimitiveType:=ftTriangles;
    setlength(Attribs,aTriangles.Input.Count);
    for i:=0 to aTriangles.Input.Count-1 do begin
      Attribs[i].Semantic:=aTriangles.Input[i].Semantic;
      Attribs[i].SourceId:=aTriangles.Input[i].Source;
      Attribs[i].Offset:=aTriangles.Input[i].Offset;
      Attribs[i].SetIndex:=aTriangles.Input[i].Set_;
    end;
    if length(aTriangles.P.Content.Value)>0 then
      Indices:=IntStringsToIntegerDynArray(aTriangles.P.Content.Value)
    else Indices:=nil;
  end;

  procedure TDAESubMesh.Init(aLines: IXMLLines_type);
  var i: integer;
  begin
    PrimitiveType:=ftLines;
    setlength(Attribs,aLines.Input.Count);
    for i:=0 to aLines.Input.Count-1 do begin
      Attribs[i].Semantic:=aLines.Input[i].Semantic;
      Attribs[i].SourceId:=aLines.Input[i].Source;
      Attribs[i].Offset:=aLines.Input[i].Offset;
      Attribs[i].SetIndex:=aLines.Input[i].Set_;
    end;
    if length(aLines.P.Content.Value)>0 then
      Indices:=IntStringsToIntegerDynArray(aLines.P.Content.Value);
  end;
  procedure TDAESubMesh.Init(aLineStrips: IXMLLineStrips_type);
  var i: integer;
  begin
    PrimitiveType:=ftLineStrip;
    setlength(Attribs,aLineStrips.Input.Count);
    for i:=0 to aLineStrips.Input.Count-1 do begin
      Attribs[i].Semantic:=aLineStrips.Input[i].Semantic;
      Attribs[i].SourceId:=aLineStrips.Input[i].Source;
      Attribs[i].Offset:=aLineStrips.Input[i].Offset;
      Attribs[i].SetIndex:=aLineStrips.Input[i].Set_;
    end;
    if length(aLineStrips.P.Content.Value)>0 then
      Indices:=IntStringsToIntegerDynArray(aLineStrips.P.Content.Value);
  end;
  procedure TDAESubMesh.Init(aPolygons: IXMLPolygons_type);
  var i: integer;
  begin
    PrimitiveType:=ftTriangles; //pars polygons to triangles
    setlength(Attribs,aPolygons.Input.Count);
    for i:=0 to aPolygons.Input.Count-1 do begin
      Attribs[i].Semantic:=aPolygons.Input[i].Semantic;
      Attribs[i].SourceId:=aPolygons.Input[i].Source;
      Attribs[i].Offset:=aPolygons.Input[i].Offset;
      Attribs[i].SetIndex:=aPolygons.Input[i].Set_;
    end;
    //Not completed
    assert(false, 'parser is not comleted');
  end;
  procedure TDAESubMesh.Init(aTriFans: IXMLTriFans_type);
  var i: integer;
  begin
    PrimitiveType:=ftTriangleFan;
    setlength(Attribs,aTriFans.Input.Count);
    for i:=0 to aTriFans.Input.Count-1 do begin
      Attribs[i].Semantic:=aTriFans.Input[i].Semantic;
      Attribs[i].SourceId:=aTriFans.Input[i].Source;
      Attribs[i].Offset:=aTriFans.Input[i].Offset;
      Attribs[i].SetIndex:=aTriFans.Input[i].Set_;
    end;
    if length(aTriFans.P.Content.Value)>0 then
      Indices:=IntStringsToIntegerDynArray(aTriFans.P.Content.Value);
  end;
  procedure TDAESubMesh.Init(aTriStrips: IXMLTriStrips_type);
  var i: integer;
  begin
    PrimitiveType:=ftTriangleStrip;
    setlength(Attribs,aTriStrips.Input.Count);
    for i:=0 to aTriStrips.Input.Count-1 do begin
      Attribs[i].Semantic:=aTriStrips.Input[i].Semantic;
      Attribs[i].SourceId:=aTriStrips.Input[i].Source;
      Attribs[i].Offset:=aTriStrips.Input[i].Offset;
      Attribs[i].SetIndex:=aTriStrips.Input[i].Set_;
    end;
    if length(aTriStrips.P.Content.Value)>0 then
      Indices:=IntStringsToIntegerDynArray(aTriStrips.P.Content.Value);
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
  if length(aSource.Float_array.Content.Value)=0 then exit(nil);
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
  assert(length(aSource.Float_array.Content.Value)>0,
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

