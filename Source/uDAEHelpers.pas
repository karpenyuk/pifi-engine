unit uDAEHelpers;

interface

uses
  uRenderResource, 
  uStorage,
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


  TFloatValuesHelper = class helper for TFloatValues
    procedure ValuesFromString(const AStr: string);
  end;

  TIntValuesHelper = class helper for TIntegerValues
    procedure ValuesFromString(const AStr: string);
  end;

  TDAEAttrib = record
    Semantic: string;
    SourceId: string;
    Offset: int64;
    SetIndex: integer;
  end;

  TDAESubMesh = record
  private
    function getAttribsCount: integer;
  public
    PrimitiveType: TFaceType;
    Attribs: array of TDAEAttrib;
    Indices: TIntegerValues;

    function BuildVertexObject: TVertexObject;
    procedure Init(aTriangles: IXMLTriangles_type); overload;
    procedure Init(aLines: IXMLLines_type); overload;
    procedure Init(aLineStrips: IXMLLineStrips_type); overload;
    procedure Init(aPolygons: IXMLPolygons_type); overload;
    //procedure Init(aPolyList: IXMLPolyList_type); overload;
    procedure Init(aTriFans: IXMLTriFans_type); overload;
    procedure Init(aTriStrips: IXMLTriStrips_type); overload;

    property AttribsCount: integer read getAttribsCount;
  end;

  TDAESourceRecord = record
    ID: string;
    Accessor: record
      SourceId: string;
      Count, Offset, Stride: integer;
      ParamIndex: array of integer; //Skip unnamed index
    end;
    Value: TFloatValues;
    References: array of ^TDAESubMesh;
    procedure Init(aSource: IXMLSource_type);
    function CreateAttribute: TAttribObject;
  end;
  PDAESourceRecord = ^TDAESourceRecord;

  TDAESources = class(TDataList<TDAESourceRecord>)
  private
    FSources: IXMLSource_typeList;
    function getSourceById(Id: string): PDAESourceRecord;
  public
    procedure Initialize(aSources: IXMLSource_typeList);
    function SourceValue(ID: string): TFloatValues;
    property Source[Id: string]: PDAESourceRecord read getSourceById; default;
  end;

  TDAEMesh = class
  private
    FSources: TDAESources;
    FMesh: IXMLMesh_type;
    FSubMeshes: TDataList<TDAESubMesh>;
    procedure AddSubMesh(const sm: TDAESubMesh);
    function getSubMesh(Index: integer): TDAESubMesh;
    function getSMCount: integer;
  public
    constructor Create(aMesh: IXMLMesh_type);
    property SubMesh[index: integer]: TDAESubMesh read getSubMesh; default;
    property Sources: TDAESources read FSources;
    property SubMeshCount: integer read getSMCount;
  end;


implementation

Type
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


  { TIntValuesHelper }

  procedure TIntValuesHelper.ValuesFromString(const AStr: string);
  var
    str: string;
    c: PChar;
  begin
    Reset;
    if AStr = '' then exit;

    c := @AStr[1];
    while c^ <> #0 do begin
      case c^ of
        #1..#32: if str<>'' then begin
                   Add(StrToInt(str));
                   str:='';
                 end;
      else str := str + c^;
      end;
      Inc(c);
    end;
    if str<>'' then Add(StrToInt(str));
    Clamp;
  end;

  { TFloatValuesHelper }

  procedure TFloatValuesHelper.ValuesFromString(const AStr: string);
  var
    str: string;
    c: PChar;
  begin
    Reset;
    if AStr = '' then exit;

    c := @AStr[1];
    while c^ <> #0 do begin
      case c^ of
        #1..#32: if str<>'' then begin
                   Add(StrToFloat(str));
                   str:='';
                 end;
      else str := str + c^;
      end;
      Inc(c);
    end;
    if str<>'' then Add(StrToFloat(str));
    Clamp;
  end;


  procedure TDAESources.Initialize(aSources: IXMLSource_typeList);
  var i: integer;
    SourceRec: TDAESourceRecord;
  begin
    FSources := aSources;
    for i:=0 to FSources.Count - 1 do begin
      SourceRec.Init(FSources[i]);
      Add(SourceRec);
    end;
    for i:=0 to FSources.Count - 1 do begin
      if not assigned(Items[i].Value) then begin
        Items[i].Value.InitializeFromSource(Source[Items[i].Accessor.SourceId]^.Value);
      end;
    end;
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
        SourceRec.References[n]:=FSubMeshes.GetItemAddr(mi);
      end;
    end;
  end;

  function TDAEMesh.getSMCount: integer;
  begin
    result := FSubMeshes.Count;
  end;

  function TDAEMesh.getSubMesh(Index: integer): TDAESubMesh;
  begin
    result := FSubMeshes[index];
  end;

  constructor TDAEMesh.Create(aMesh: IXMLMesh_type);
  var i: integer;
      sm: TDAESubMesh;
  begin
    FMesh := aMesh;
    FSources:=TDAESources.Create;
    FSources.Initialize(FMesh.Source);
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
    then begin
      Value := TFloatValues.Create; Value.InitializeAsSource;
      Value.ValuesFromString(aSource.Float_array.Content.Value);
    end else begin
      Value:=nil;
    end;
    References := nil;
  end;

  function TDAESourceRecord.CreateAttribute: TAttribObject;
  begin

  end;

  function TDAESources.SourceValue(ID: string): TFloatValues;
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
      if Items[i].ID = Id then exit(GetItemAddr(i));
  end;

  function TDAESubMesh.getAttribsCount: integer;
  begin
    result := length(Attribs);
  end;

  function TDAESubMesh.BuildVertexObject: TVertexObject;
  begin {
    1. Create TAttribObject for each of Attrib
    2.
  }end;


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
    if length(aTriangles.P.Content.Value)>0 then begin
      Indices:=TIntegerValues.Create; Indices.InitializeAsSource;
      Indices.ValuesFromString(aTriangles.P.Content.Value);
    end else Indices:=nil;
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
    if length(aLines.P.Content.Value)>0 then begin
      Indices:=TIntegerValues.Create; Indices.InitializeAsSource;
      Indices.ValuesFromString(aLines.P.Content.Value);
    end else Indices:=nil;
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
    if length(aLineStrips.P.Content.Value)>0 then begin
      Indices:=TIntegerValues.Create; Indices.InitializeAsSource;
      Indices.ValuesFromString(aLineStrips.P.Content.Value);
    end else indices:=nil;
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
    if length(aTriFans.P.Content.Value)>0 then begin
      Indices:=TIntegerValues.Create; Indices.InitializeAsSource;
      Indices.ValuesFromString(aTriFans.P.Content.Value);
    end else Indices:=nil;
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
    if length(aTriStrips.P.Content.Value)>0 then begin
      Indices:=TIntegerValues.Create; Indices.InitializeAsSource;
      Indices.ValuesFromString(aTriStrips.P.Content.Value);
    end else Indices:=nil;
  end;

end.