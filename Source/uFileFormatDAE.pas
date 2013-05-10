unit uFileFormatDAE;

interface

uses
  uRenderResource;

type

  FileFormatDAE = class
    class function LoadAndCreateVertexObject(const aFileName: string)
      : TVertexObject;
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
  TDAEVertexSource = record
    Id: String;
    Data: TSingleDynArray;
    List: TAbstractDataList;
    Type_: TValueType;
    Components: TValueComponent;
    Stride: Integer;
  end;

  TDAEVertexSourceDynArray = array of TDAEVertexSource;

  TSourceSemantic = record
    AttribType: TAttribType;
    Source: TDAEVertexSource;
    Offset: Integer;
  end;

  TSourceSemanticDynArray = array of TSourceSemantic;

function GetSemantic(const Input: IXMLInput_local_offset_typeList;
  const ASources: TDAEVertexSourceDynArray): TSourceSemanticDynArray;
var
  j: Integer;
  function FindSource(ASourceName: String): TDAEVertexSource;
  var
    i: Integer;
  begin
    for i := 0 to High(ASources) do
    begin
      if (ASourceName = '#' + ASources[i].Id) or (ASourceName = ASources[i].Id)
      then
        exit(ASources[i]);
    end;
    Result := ASources[0]; // if not found Result POSITION attribute
  end;

begin
  SetLength(Result, Input.Count);
  for j := 0 to Input.Count - 1 do
  begin
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

class function FileFormatDAE.LoadAndCreateVertexObject(const aFileName: string)
  : TVertexObject;
var
  LVO: TVertexObject;
  LCOLLADA: IXMLCOLLADA;
  LGeometry: IXMLGeometry_type;
  LMeshType: IXMLMesh_type;
  LAccessor: IXMLAccessor_type;
  LSource: IXMLSource_type;
  LSources: TDAEVertexSourceDynArray;
  LIndices: TIntegerDynArray;
  LSemantics: TSourceSemanticDynArray;
  LSubMesh: IXMLTriangles_type;
  sType: string;
  attr: TAttribBuffer;
  i, j, k, Offset, NumSemantic, Index: Integer;

  function GetId(const AName: String): String;
  var
    n: Integer;
  begin
    Result := AName;
    for n := 0 to LMeshType.Vertices.Input.Count - 1 do
      if LMeshType.Vertices.Input[n].Source = '#' + AName then
        Result := LMeshType.Vertices.Id;
  end;

begin
  LVO := TVertexObject.Create;

  try

    LCOLLADA := TXMLCOLLADA.Load(aFileName) as IXMLCOLLADA;
    if (LCOLLADA.Library_geometries.Count > 0) then
    begin
      LGeometry := LCOLLADA.Library_geometries[0].Geometry[0];
      LMeshType := LGeometry.Mesh;

      SetLength(LSources, LMeshType.Source.Count);
      for i := 0 to LMeshType.Source.Count - 1 do
      begin
        LSource := LMeshType.Source[i];
        LSources[i].Id := GetId(LSource.Id);
        LSources[i].Data := FloatStringsToSingleDynArray
          (LSource.Float_array.Content.Value);
        case LSource.Technique_common.Accessor.Count of
          1:
            begin
              LSources[i].Components := 1;
              LSources[i].List := TSingleList.Create;
            end;
          2:
            begin
              LSources[i].Components := 2;
              LSources[i].List := TVec2List.Create;
            end;
          3:
            begin
              LSources[i].Components := 3;
              LSources[i].List := TVec3List.Create;
            end;
          4:
            begin
              LSources[i].Components := 4;
              LSources[i].List := TVec4List.Create;
            end;
        else
          Assert(False);
        end;
        LAccessor := LSource.Technique_common.Accessor;
        for j := 0 to LSource.Technique_common.Accessor.Count - 1 do
        begin
          sType := LowerCase(string(LAccessor.Param[j].Type_));
          Assert(sType = 'float');
        end;
        LSources[i].Type_ := vtFloat;
        LSources[i].Stride := LSource.Technique_common.Accessor.Stride;
      end;

      if LMeshType.Triangles.Count > 0 then
      begin
        LSubMesh := LMeshType.Triangles[0];
        LIndices := IntStringsToIntegerDynArray(LSubMesh.P.Content.Value);
        LSemantics := GetSemantic(LSubMesh.Input, LSources);

        Offset := 0;
        NumSemantic := LSubMesh.Input.Count;
        Index := 0;
        while (Offset < Length(LIndices)) do
        begin
          for k := 0 to High(LSemantics) do
          begin
            for j := 0 to 2 do
            begin
              i := LIndices[Offset + j * NumSemantic + LSemantics[k].Offset];
              i := i * LSemantics[k].Source.Stride;
              LSemantics[k].Source.List.AddRaw(@LSemantics[k].Source.Data[i]);
            end;
          end;
          Inc(Offset, NumSemantic * 3);
          LVO.AddTriangle(Index, Index + 1, Index + 2);
          Inc(Index, 3);
        end;

        for k := 0 to High(LSemantics) do
          with LSemantics[k].Source do
          begin
            attr := TAttribBuffer.CreateAndSetup
              (CAttribSematics[LSemantics[k].AttribType].Name, Ord(Components));
            attr.SetAttribSemantic(LSemantics[k].AttribType);
            attr.Buffer.Allocate(List.Count * List.ItemSize,
              List.GetItemAddr(0));
            attr.Buffer.SetDataHandler(List);
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

end.
