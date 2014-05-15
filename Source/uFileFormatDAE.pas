unit uFileFormatDAE;

interface

uses
  uRenderResource, uStorage, uDAEHelpers,
  Types, uXmlParser, uDAESchema, uMiscUtils,
  uBaseTypes, uLists;

type

  FileFormatDAE = class
    class function LoadAndCreateVertexObject(const aFileName: string): TVertexObject;
  end;

implementation

type

  TMeshBuffer = record
    Values: TSingleList;
    Attrib: TDAEAttrib;
    Source: PDAESourceRecord;
  end;
  TMeshBuffers = array of TMeshBuffer;

procedure FillBuffer(var aBuffer: TMeshBuffer; const aSubMesh: TDAESubMesh;
  const aSource: PDAESourceRecord);
var i,j,n,offs,index,stride,count,pcount: integer;
begin
  offs := aBuffer.Attrib.Offset;
  pcount := length(aBuffer.Source.Accessor.ParamIndex);
  stride := aBuffer.Source.Accessor.Stride;
  count := aBuffer.Source.Accessor.Count;
  n := aSubMesh.AttribsCount;
  aBuffer.Values.Capacity:=count*pcount;
  for i:= 0 to count-1 do begin
    index := stride * aSubMesh.Indices[offs+i*n];
    for j:=0 to pcount-1 do
      aBuffer.Values.Add(aBuffer.Source.Value[index+aBuffer.Source.Accessor.ParamIndex[j]]);
  end;
end;

function LoadGeometry(aMesh: TDAEMesh): TObject;
var sm: TDAESubMesh;
    i,j: integer;
    buffers: array of TMeshBuffers;
begin
  setlength(buffers, aMesh.SubMeshCount);
  for i:=0 to aMesh.SubMeshCount-1 do begin
    sm:=aMesh[i]; setlength(buffers[i], sm.AttribsCount);
    for j:=0 to sm.AttribsCount-1 do begin
      buffers[i][j].Values := TSingleList.Create;
      buffers[i][j].Attrib := sm.Attribs[j];
      buffers[i][j].Source := aMesh.Sources[sm.Attribs[j].SourceId];
    end;
  end;
end;

class function FileFormatDAE.LoadAndCreateVertexObject(const aFileName: string): TVertexObject;
var
  LCOLLADA: IXMLCOLLADA;
  LGeometries: IXMLLibrary_geometries_type;
  LGeometry: IXMLGeometry_type;
  LMeshType: IXMLMesh_type;
  LAccessor: IXMLAccessor_type;
  LSource: IXMLSource_type;
  LIndices: TIntegerDynArray;
  LSubMesh: IXMLTriangles_type;
  Geometries: TObjectList;
  sType: string;
  attr: TAttribBuffer;
  i, j, k, Offset: Integer;

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

  try
    LCOLLADA := TXMLCOLLADA.Load(aFileName) as IXMLCOLLADA;
    //Parse geometry library
    Geometries:=TObjectList.Create;
    for i:=0 to LCOLLADA.Library_geometries.Count-1 do begin
      LGeometries := LCOLLADA.Library_geometries[i];
      for j:=0 to LGeometries.Count-1 do begin
        //Geometries.Add(TDAEMesh.Create(LGeometries.Geometry[j].Mesh));
        LoadGeometry(TDAEMesh.Create(LGeometries.Geometry[j].Mesh));
      end;
    end;


{
    if (LCOLLADA.Library_geometries.Count > 0) then begin
      LGeometries := LCOLLADA.Library_geometries[0];
      LGeometry :=  LGeometries.Geometry[0];
      LMeshType := LGeometry.Mesh;


//PrimitiveTypes
      if LMeshType.Triangles.Count > 0 then begin
        LSubMesh := LMeshType.Triangles[0];
        LIndices := IntStringsToIntegerDynArray(LSubMesh.P.Content.Value);

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
}
  except
    raise;
  end;
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

