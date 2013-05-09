unit uMainUnit;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Types;

type
  TForm5 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}

uses
    uXmlParser, uDAESchema, uMiscUtils, uRenderResource, uBaseTypes;

type
  TDAEVertexSource = record
    Id: String;
    Data: TSingleDynArray;
    FType: TValueType;
    FComponents: TValueComponent;
  end;
  TDAEVertexSourceDynArray = array of TDAEVertexSource;

  TSourceSemantic = record
    FAttribType: TAttribType;
    FSource: TDAEVertexSource;
    FOffset: Integer;
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
        if (ASourceName = '#' + ASources[i].Id) or
           (ASourceName = ASources[i].Id) then
          exit(ASources[i]);
      end;
      Result := ASources[0]; // if not found Result POSITION attribute
    end;

  begin
    SetLength(Result, Input.Count);
    for j := 0 to Input.Count - 1 do
    begin
      Result[j].FOffset := Input[j].Offset;
      Result[j].FSource := FindSource(Input[j].Source);
      if Input[j].Semantic = 'VERTEX' then
        Result[j].FAttribType := TAttribType.atVertex
      else if Input[j].Semantic = 'NORMAL' then
        Result[j].FAttribType := TAttribType.atNormal
      else if Input[j].Semantic = 'VCOLOR' then
        Result[j].FAttribType := TAttribType.atColor
      else if Input[j].Semantic = 'TEXCOORD' then
        Result[j].FAttribType := TAttribType.atTexCoord0;
    end;
  end;

procedure TForm5.FormCreate(Sender: TObject);
var
  path: string;
  LDOC: IXML;
  LCOLLADA: IXMLCOLLADA;
  LGeometry: IXMLGeometry_type;
  LMeshType: IXMLMesh_type;
  LSource: IXMLSource_type;
  LSources: TDAEVertexSourceDynArray;
  LIndices: TIntegerDynArray;
  LSemantics: TSourceSemanticDynArray;
  LSubMesh: IXMLTriangles_type;
  attr: TAttribBuffer;
  I, J, T: Integer;

  function GetId(const AName: String):String;
  var
    n :Integer;
  begin
    result := AName;
    for n := 0 to LMeshType.Vertices.Input.Count - 1 do
       if LMeshType.Vertices.Input[n].Source = '#'+ AName then
           result := LMeshType.Vertices.Id;
  end;

begin
  {$IFDEF MSWindows}
  path := '..\..\Source\Media\'; { :-\ }
  {$ENDIF}
  {$IFDEF Linux}
  path := '../../Source/Media/'; { :-/ }
  {$ENDIF}

  LCOLLADA := TXMLCOLLADA.Load(path + 'bunny.dae') as IXMLCOLLADA;
  if (LCOLLADA.Library_geometries.Count > 0) then
  begin
    LGeometry := LCOLLADA.Library_geometries[0].Geometry[I];
    LMeshType := LGeometry.Mesh;

    SetLength(LSources, LMeshType.Source.Count);
    for I := 0 to LMeshType.Source.Count - 1 do
    begin
      LSource := LMeshType.Source[I];
      LSources[i].Id := GetId(LSource.Id);
      LSources[i].Data := FloatStringsToSingleDynArray(LSource.Float_array.Content.Value);
      case LSource.Technique_common.Accessor.Count of
        1: LSources[i].FComponents := 1;
        2: LSources[i].FComponents := 2;
        3: LSources[i].FComponents := 3;
        4: LSources[i].FComponents := 4;
        else Assert(False);
      end;
      for J := 0 to LSource.Technique_common.Accessor.Count - 1 do
        Assert(LSource.Technique_common.Accessor.Param[J].Type_ = 'float');
      LSources[i].FType := vtFloat;
    end;

    if LMeshType.Triangles.Count > 0 then
    begin
      LIndices := IntStringsToIntegerDynArray(LMeshType.Triangles[0].P.Content.Value);
      LSemantics := GetSemantic(LMeshType.Triangles[0].Input, LSources);

    end;

  end;
end;

end.
