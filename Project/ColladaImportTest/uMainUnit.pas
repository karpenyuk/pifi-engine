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
    uXmlParser, uDAESchema, uMiscUtils, uRenderResource
    ,XML.xmldom, XML.XMLDoc, XML.XMLIntf
    ;

procedure TForm5.FormCreate(Sender: TObject);
var
  path: string;
  LCOLLADA: IXMLCOLLADA;
  LGeometry: IXMLGeometry_type;
  LMeshType: IXMLMesh_type;
  LSource: IXMLSource_type;
  LSubMesh: IXMLTriangles_type;
  FloatArray: TSingleDynArray;
  attr: TAttribBuffer;
  I, J, T: Integer;
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
    for I := 0 to LCOLLADA.Library_geometries[0].Count - 1 do
    begin
      LGeometry := LCOLLADA.Library_geometries[0].Geometry[I];
      LMeshType := LGeometry.Mesh;
      if LMeshType.Triangles.Count > 0 then
      begin
        for J := 0 to LMeshType.Source.Count - 1 do
        begin
          LSource := LMeshType.Source[I];
          FloatArray := FloatStringsToSingleDynArray(LSource.Float_array.Content.Value);
          for T := 0 to LMeshType.Triangles.Count - 1 do
          begin

          end;
        end;
      end;
    end;

  end;
end;

end.
