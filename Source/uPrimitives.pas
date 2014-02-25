unit uPrimitives;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses uMath, uVMath, uBaseClasses, uLists, uBaseTypes, uRenderResource, uStorage;

//Реализовать основные примитивы - цилиндр, труба, конус, сетка и т.д.

function CreatePlane(aWidth, aHeight: Float): TVertexObject;
function CreateSphere(Radius: Float; VSegments, HSegments: integer;
                  TileS: Float=1; TileT: Float=1; InvertNormal: boolean = false): TVertexObject;
function CreateBox(aWidth, aHeight, aDepth: Float): TVertexObject;
function CreateCube(aSize: Float): TVertexObject;
function CreateCylinder(aTopRadius, aBottomRadius, aHeight: Float; VSegments, HSegments: Integer): TVertexObject;
function CreateTeapod(aDivision: Integer = 9): TVertexObject;
function CreateSprite(aWidth, aHeight: Float): TVertexObject;

implementation

function CreatePlane(aWidth, aHeight: Float): TVertexObject;
var attr: TAttribBuffer;
    Vertices: TVec3List;
    TexCoords: TVec2List;
    w2,h2: Float;
begin
  result:=Storage.CreateVertexObject;
  Vertices:=TVec3List.Create;
  TexCoords:=TVec2List.Create;
  w2:=aWidth/2; h2:=aHeight/2;
  Vertices.Add( Vec3Make( -w2, -h2 )); TexCoords.Add( Vec2Make( 0, 0 ));
  Vertices.Add( Vec3Make( w2, -h2 ));  TexCoords.Add( Vec2Make( 1, 0 ));
  Vertices.Add( Vec3Make( w2, h2 ));   TexCoords.Add( Vec2Make( 1, 1 ));
  Vertices.Add( Vec3Make( -w2, h2 ));  TexCoords.Add( Vec2Make( 0, 1 ));

  attr:=Storage.CreateAttribBuffer(CAttribSematics[atVertex].Name,3,vtFloat,0);
  attr.Buffer.Allocate(Vertices.Size,Vertices.Data);
  attr.Buffer.SetDataHandler(Vertices);
  attr.SetAttribSemantic(atVertex);
  //attr.SetAttribLocation(CAttribSematics[atVertex].Location);
  result.AddAttrib(attr,true);
  attr:=Storage.CreateAttribBuffer(CAttribSematics[atTexCoord0].Name,2,vtFloat,0);
  attr.Buffer.Allocate(TexCoords.Size,TexCoords.Data);
  attr.Buffer.SetDataHandler(TexCoords);
  attr.SetAttribSemantic(atTexCoord0);
  //attr.SetAttribLocation(CAttribSematics[atTexCoord0].Location);
  result.AddAttrib(attr);
  result.AddQuad(0,1,2,3);
  result.FaceType:=ftTriangles;
end;

function CreateSphere(Radius: Float; VSegments, HSegments: integer;
                  TileS: Float=1; TileT: Float=1; InvertNormal: boolean = false): TVertexObject;
var attr: TAttribBuffer;
    Vertices: TVec3List;
    Normals: TVec3List;
    TexCoords: TVec2List;
    v: vec3; t: vec2;
    norm: TVector;
    w2,h2: Float;

    a,b: Float;
    i,j,si,vi: integer;
    da,db,rx,rz,rxz,ry: Float;
    ks,kt:Float;
begin
  Assert(Vsegments*HSegments<>0,'Segments count must be more than "0".');

  result:=Storage.CreateVertexObject;
  Vertices:=TVec3List.Create;
  Normals:=TVec3List.Create;
  TexCoords:=TVec2List.Create;

  da:=pi/VSegments; db:=2*pi/(HSegments-1);
  ks:=1/(HSegments-1);kt:=1/(VSegments);

  for i:=0 to VSegments do begin
    if InvertNormal then a:=pi/2-i*da else a:=i*da-pi/2;
    TMath.SinCos( a, Radius, ry, rxz ); si:=Result.IndiceCount;
    for j:=0 to HSegments-1 do begin
      b:=j*db; rx:=rxz*cos(b); rz:=rxz*sin(b); vi:=i*(HSegments)+j;
      v[0]:=rx; v[1]:=ry; v[2]:=rz; Vertices.Add(v);
      t[0]:=(1-j*ks)*TileS; t[1]:=(i*kt)*TileT; TexCoords.Add(t);
      norm := TVector.Make( rx, ry, rz ).Scale( 1 / Radius );
      if InvertNormal then norm.SetNegate; //NegateVector(norm);
      Normals.Add(norm.Vec3);
      if i<VSegments then Result.AddLine(vi,vi+HSegments);
    end;
    Result.AddLine(Result.Indices[si],Result.Indices[si+1]);
  end;
  Result.IndiceCount:=Result.IndiceCount-2;

  attr:=Storage.CreateAttribBuffer(CAttribSematics[atVertex].Name,3,vtFloat,0);
  attr.Buffer.Allocate(Vertices.Size,Vertices.Data);
  attr.Buffer.SetDataHandler(Vertices);
  attr.SetAttribSemantic(atVertex);
  //attr.SetAttribLocation(CAttribSematics[atVertex].Location);
  result.AddAttrib(attr,true);

  attr:=Storage.CreateAttribBuffer(CAttribSematics[atNormal].Name,3,vtFloat,0);
  attr.Buffer.Allocate(Normals.Size,Normals.Data);
  attr.Buffer.SetDataHandler(Normals);
  attr.SetAttribSemantic(atNormal);
  //attr.SetAttribLocation(CAttribSematics[atNormal].Location);
  result.AddAttrib(attr);

  attr:=Storage.CreateAttribBuffer(CAttribSematics[atTexCoord0].Name,2,vtFloat,0);
  attr.Buffer.Allocate(TexCoords.Size,TexCoords.Data);
  attr.Buffer.SetDataHandler(TexCoords);
  attr.SetAttribSemantic(atTexCoord0);
  //attr.SetAttribLocation(CAttribSematics[atTexCoord0].Location);
  result.AddAttrib(attr);

  result.FaceType:=ftTriangleStrip;
end;

function CreateBox(aWidth, aHeight, aDepth: Float): TVertexObject;
var attr: TAttribBuffer;
    Vertices: TVec3List;
    TexCoords: TVec2List;
    Normals: TVec3List;
    x, y, z: Float;
    n: TVector;
begin
  result:=Storage.CreateVertexObject;
  Vertices:=TVec3List.Create;
  Normals:=TVec3List.Create;
  TexCoords:=TVec2List.Create;
  x := aWidth / 2; y := aHeight / 2; z := aDepth / 2;
  // front face
  n := Vec3Make( 0, 0, 1 );
  Vertices.Add( Vec3Make( -x, -y, z )); TexCoords.Add( Vec2Make( 0, 0 )); Normals.Add(n.Vec3);
  Vertices.Add( Vec3Make( -x, y, z ));  TexCoords.Add( Vec2Make( 0, 1 )); Normals.Add(n.Vec3);
  Vertices.Add( Vec3Make( x, y, z ));   TexCoords.Add( Vec2Make( 1, 1 )); Normals.Add(n.Vec3);
  Vertices.Add( Vec3Make( x, -y, z ));  TexCoords.Add( Vec2Make( 1, 0 )); Normals.Add(n.Vec3);
  // back face
  n := Vec3Make( 0, 0, -1 );
  Vertices.Add( Vec3Make( x, -y, -z )); TexCoords.Add( Vec2Make( 0, 0 )); Normals.Add(n.Vec3);
  Vertices.Add( Vec3Make( x, y, -z ));  TexCoords.Add( Vec2Make( 0, 1 )); Normals.Add(n.Vec3);
  Vertices.Add( Vec3Make( -x, y, -z ));   TexCoords.Add( Vec2Make( 1, 1 )); Normals.Add(n.Vec3);
  Vertices.Add( Vec3Make( -x, -y, -z ));  TexCoords.Add( Vec2Make( 1, 0 )); Normals.Add(n.Vec3);
  // right face
  n := Vec3Make( 1, 0, 0 );
  Vertices.Add( Vec3Make( x, -y, z )); TexCoords.Add( Vec2Make( 0, 0 )); Normals.Add(n.Vec3);
  Vertices.Add( Vec3Make( x, y, z ));  TexCoords.Add( Vec2Make( 0, 1 )); Normals.Add(n.Vec3);
  Vertices.Add( Vec3Make( x, y, -z ));   TexCoords.Add( Vec2Make( 1, 1 )); Normals.Add(n.Vec3);
  Vertices.Add( Vec3Make( x, -y, -z ));  TexCoords.Add( Vec2Make( 1, 0 )); Normals.Add(n.Vec3);
  // left face
  n := Vec3Make( -1, 0, 0 );
  Vertices.Add( Vec3Make( -x, -y, -z )); TexCoords.Add( Vec2Make( 0, 0 )); Normals.Add(n.Vec3);
  Vertices.Add( Vec3Make( -x, y, -z ));  TexCoords.Add( Vec2Make( 0, 1 )); Normals.Add(n.Vec3);
  Vertices.Add( Vec3Make( -x, y, z ));   TexCoords.Add( Vec2Make( 1, 1 )); Normals.Add(n.Vec3);
  Vertices.Add( Vec3Make( -x, -y, z ));  TexCoords.Add( Vec2Make( 1, 0 )); Normals.Add(n.Vec3);
  // top face
  n := Vec3Make( 0, 1, 0 );
  Vertices.Add( Vec3Make( -x, y, z )); TexCoords.Add( Vec2Make( 0, 0 )); Normals.Add(n.Vec3);
  Vertices.Add( Vec3Make( -x, y, -z ));  TexCoords.Add( Vec2Make( 0, 1 )); Normals.Add(n.Vec3);
  Vertices.Add( Vec3Make( x, y, -z ));   TexCoords.Add( Vec2Make( 1, 1 )); Normals.Add(n.Vec3);
  Vertices.Add( Vec3Make( x, y, z ));  TexCoords.Add( Vec2Make( 1, 0 )); Normals.Add(n.Vec3);
  // bottom face
  n := Vec3Make( 0, -1, 0 );
  Vertices.Add( Vec3Make( x, -y, z )); TexCoords.Add( Vec2Make( 0, 0 )); Normals.Add(n.Vec3);
  Vertices.Add( Vec3Make( x, -y, -z ));  TexCoords.Add( Vec2Make( 0, 1 )); Normals.Add(n.Vec3);
  Vertices.Add( Vec3Make( -x, -y, -z ));   TexCoords.Add( Vec2Make( 1, 1 )); Normals.Add(n.Vec3);
  Vertices.Add( Vec3Make( -x, -y, z ));  TexCoords.Add( Vec2Make( 1, 0 )); Normals.Add(n.Vec3);

  attr:=Storage.CreateAttribBuffer(CAttribSematics[atVertex].Name,3,vtFloat,0);
  attr.Buffer.Allocate(Vertices.Size,Vertices.Data);
  attr.Buffer.SetDataHandler(Vertices);
  attr.SetAttribSemantic(atVertex);
  //attr.SetAttribLocation(CAttribSematics[atVertex].Location);
  result.AddAttrib(attr,true);

  attr:=Storage.CreateAttribBuffer(CAttribSematics[atNormal].Name,3,vtFloat,0);
  attr.Buffer.Allocate(Normals.Size,Normals.Data);
  attr.Buffer.SetDataHandler(Normals);
  attr.SetAttribSemantic(atNormal);
  //attr.SetAttribLocation(CAttribSematics[atNormal].Location);
  result.AddAttrib(attr);

  attr:=Storage.CreateAttribBuffer(CAttribSematics[atTexCoord0].Name,2,vtFloat,0);
  attr.Buffer.Allocate(TexCoords.Size,TexCoords.Data);
  attr.Buffer.SetDataHandler(TexCoords);
  attr.SetAttribSemantic(atTexCoord0);
  //attr.SetAttribLocation(CAttribSematics[atTexCoord0].Location);
  result.AddAttrib(attr);
  with result do begin
    AddQuad(0,1,2,3); AddQuad(4,5,6,7);
    AddQuad(8,9,10,11); AddQuad(12,13,14,15);
    AddQuad(16,17,18,19); AddQuad(20,21,22,23);
  end;
  result.FaceType:=ftTriangles;
end;

function CreateCube(aSize: Float): TVertexObject;
begin
  result:=CreateBox(aSize,aSize,aSize);
end;

function CreateTeapod(aDivision: Integer): TVertexObject;
{$REGION TEAPODCONST}
const
  CTeapotIndices: array[0..511] of Integer =
    (
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
    4, 17, 18, 19, 8, 20, 21, 22, 12, 23, 24, 25, 16, 26, 27, 28,
    19, 29, 30, 31, 22, 32, 33, 34, 25, 35, 36, 37, 28, 38, 39, 40,
    31, 41, 42, 1, 34, 43, 44, 5, 37, 45, 46, 9, 40, 47, 48, 13,
    13, 14, 15, 16, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
    16, 26, 27, 28, 52, 61, 62, 63, 56, 64, 65, 66, 60, 67, 68, 69,
    28, 38, 39, 40, 63, 70, 71, 72, 66, 73, 74, 75, 69, 76, 77, 78,
    40, 47, 48, 13, 72, 79, 80, 49, 75, 81, 82, 53, 78, 83, 84, 57,
    57, 58, 59, 60, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96,
    60, 67, 68, 69, 88, 97, 98, 99, 92, 100, 101, 102, 96, 103, 104, 105,
    69, 76, 77, 78, 99, 106, 107, 108, 102, 109, 110, 111, 105, 112, 113, 114,
    78, 83, 84, 57, 108, 115, 116, 85, 111, 117, 118, 89, 114, 119, 120, 93,
    121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136,
    124, 137, 138, 121, 128, 139, 140, 125, 132, 141, 142, 129, 136, 143, 144, 133,
    133, 134, 135, 136, 145, 146, 147, 148, 149, 150, 151, 152, 69, 153, 154, 155,
    136, 143, 144, 133, 148, 156, 157, 145, 152, 158, 159, 149, 155, 160, 161, 69,
    162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177,
    165, 178, 179, 162, 169, 180, 181, 166, 173, 182, 183, 170, 177, 184, 185, 174,
    174, 175, 176, 177, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197,
    177, 184, 185, 174, 189, 198, 199, 186, 193, 200, 201, 190, 197, 202, 203, 194,
    204, 204, 204, 204, 207, 208, 209, 210, 211, 211, 211, 211, 212, 213, 214, 215,
    204, 204, 204, 204, 210, 217, 218, 219, 211, 211, 211, 211, 215, 220, 221, 222,
    204, 204, 204, 204, 219, 224, 225, 226, 211, 211, 211, 211, 222, 227, 228, 229,
    204, 204, 204, 204, 226, 230, 231, 207, 211, 211, 211, 211, 229, 232, 233, 212,
    212, 213, 214, 215, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245,
    215, 220, 221, 222, 237, 246, 247, 248, 241, 249, 250, 251, 245, 252, 253, 254,
    222, 227, 228, 229, 248, 255, 256, 257, 251, 258, 259, 260, 254, 261, 262, 263,
    229, 232, 233, 212, 257, 264, 265, 234, 260, 266, 267, 238, 263, 268, 269, 242,
    270, 270, 270, 270, 279, 280, 281, 282, 275, 276, 277, 278, 271, 272, 273, 274,
    270, 270, 270, 270, 282, 289, 290, 291, 278, 286, 287, 288, 274, 283, 284, 285,
    270, 270, 270, 270, 291, 298, 299, 300, 288, 295, 296, 297, 285, 292, 293, 294,
    270, 270, 270, 270, 300, 305, 306, 279, 297, 303, 304, 275, 294, 301, 302, 271
    );

  cTeapotVertices: array[0..305] of Vec3 =
    (
    (1.4000, 2.400, 0.0000),
    (1.4000, 2.400, -0.7840),
    (0.7840, 2.400, -1.4000),
    (0.0000, 2.400, -1.4000),
    (1.3375, 2.5312, 0.0000),
    (1.3375, 2.5312, -0.7490),
    (0.7490, 2.5312, -1.3375),
    (0.0000, 2.5312, -1.3375),
    (1.4375, 2.5312, 0.0000),
    (1.4375, 2.5312, -0.8050),
    (0.8050, 2.5312, -1.4375),
    (0.0000, 2.5312, -1.4375),
    (1.5000, 2.400, 0.0000),
    (1.5000, 2.400, -0.8400),
    (0.8400, 2.400, -1.5000),
    (0.0000, 2.400, -1.5000),
    (-0.7840, 2.400, -1.4000),
    (-1.4000, 2.400, -0.7840),
    (-1.4000, 2.400, 0.0000),
    (-0.7490, 2.5312, -1.3375),
    (-1.3375, 2.5312, -0.7490),
    (-1.3375, 2.5312, 0.0000),
    (-0.8050, 2.5312, -1.4375),
    (-1.4375, 2.5312, -0.8050),
    (-1.4375, 2.5312, 0.0000),
    (-0.8400, 2.400, -1.5000),
    (-1.5000, 2.400, -0.8400),
    (-1.5000, 2.400, 0.0000),
    (-1.4000, 2.400, 0.7840),
    (-0.7840, 2.400, 1.4000),
    (0.0000, 2.400, 1.4000),
    (-1.3375, 2.5312, 0.7490),
    (-0.7490, 2.5312, 1.3375),
    (0.0000, 2.5312, 1.3375),
    (-1.4375, 2.5312, 0.8050),
    (-0.8050, 2.5312, 1.4375),
    (0.0000, 2.5312, 1.4375),
    (-1.5000, 2.400, 0.8400),
    (-0.8400, 2.400, 1.5000),
    (0.0000, 2.400, 1.5000),
    (0.7840, 2.400, 1.4000),
    (1.4000, 2.400, 0.7840),
    (0.7490, 2.5312, 1.3375),
    (1.3375, 2.5312, 0.7490),
    (0.8050, 2.5312, 1.4375),
    (1.4375, 2.5312, 0.8050),
    (0.8400, 2.400, 1.5000),
    (1.5000, 2.400, 0.8400),
    (1.7500, 1.875, 0.0000),
    (1.7500, 1.875, -0.9800),
    (0.9800, 1.875, -1.7500),
    (0.0000, 1.875, -1.7500),
    (2.0000, 1.350, 0.0000),
    (2.0000, 1.350, -1.1200),
    (1.1200, 1.350, -2.0000),
    (0.0000, 1.350, -2.0000),
    (2.0000, 0.900, 0.0000),
    (2.0000, 0.900, -1.1200),
    (1.1200, 0.900, -2.0000),
    (0.0000, 0.900, -2.0000),
    (-0.9800, 1.875, -1.7500),
    (-1.7500, 1.875, -0.9800),
    (-1.7500, 1.875, 0.0000),
    (-1.1200, 1.350, -2.0000),
    (-2.0000, 1.350, -1.1200),
    (-2.0000, 1.350, 0.0000),
    (-1.1200, 0.900, -2.0000),
    (-2.0000, 0.900, -1.1200),
    (-2.0000, 0.900, 0.0000),
    (-1.7500, 1.875, 0.9800),
    (-0.9800, 1.875, 1.7500),
    (0.0000, 1.875, 1.7500),
    (-2.0000, 1.350, 1.1200),
    (-1.1200, 1.350, 2.0000),
    (0.0000, 1.350, 2.0000),
    (-2.0000, 0.900, 1.1200),
    (-1.1200, 0.900, 2.0000),
    (0.0000, 0.900, 2.0000),
    (0.9800, 1.875, 1.7500),
    (1.7500, 1.875, 0.9800),
    (1.1200, 1.350, 2.0000),
    (2.0000, 1.350, 1.1200),
    (1.1200, 0.900, 2.0000),
    (2.0000, 0.900, 1.1200),
    (2.0000, 0.450, 0.0000),
    (2.0000, 0.450, -1.1200),
    (1.1200, 0.450, -2.0000),
    (0.0000, 0.450, -2.0000),
    (1.5000, 0.225, 0.0000),
    (1.5000, 0.225, -0.8400),
    (0.8400, 0.225, -1.5000),
    (0.0000, 0.225, -1.5000),
    (1.5000, 0.150, 0.0000),
    (1.5000, 0.150, -0.8400),
    (0.8400, 0.150, -1.5000),
    (0.0000, 0.150, -1.5000),
    (-1.1200, 0.450, -2.0000),
    (-2.0000, 0.450, -1.1200),
    (-2.0000, 0.450, 0.0000),
    (-0.8400, 0.225, -1.5000),
    (-1.5000, 0.225, -0.8400),
    (-1.5000, 0.225, 0.0000),
    (-0.8400, 0.150, -1.5000),
    (-1.5000, 0.150, -0.8400),
    (-1.5000, 0.150, 0.0000),
    (-2.0000, 0.450, 1.1200),
    (-1.1200, 0.450, 2.0000),
    (0.0000, 0.450, 2.0000),
    (-1.5000, 0.225, 0.8400),
    (-0.8400, 0.225, 1.5000),
    (0.0000, 0.225, 1.5000),
    (-1.5000, 0.150, 0.8400),
    (-0.8400, 0.150, 1.5000),
    (0.0000, 0.150, 1.5000),
    (1.1200, 0.450, 2.0000),
    (2.0000, 0.450, 1.1200),
    (0.8400, 0.225, 1.5000),
    (1.5000, 0.2250, 0.8400),
    (0.8400, 0.150, 1.5000),
    (1.5000, 0.150, 0.8400),
    (-1.6000, 2.025, 0.0000),
    (-1.6000, 2.025, -0.3000),
    (-1.5000, 2.250, -0.3000),
    (-1.5000, 2.250, 0.0000),
    (-2.3000, 2.025, 0.0000),
    (-2.3000, 2.025, -0.3000),
    (-2.5000, 2.250, -0.3000),
    (-2.5000, 2.250, 0.0000),
    (-2.7000, 2.025, 0.0000),
    (-2.7000, 2.025, -0.3000),
    (-3.0000, 2.250, -0.3000),
    (-3.0000, 2.250, 0.0000),
    (-2.7000, 1.800, 0.0000),
    (-2.7000, 1.800, -0.3000),
    (-3.0000, 1.800, -0.3000),
    (-3.0000, 1.800, 0.0000),
    (-1.5000, 2.250, 0.3000),
    (-1.6000, 2.025, 0.3000),
    (-2.5000, 2.250, 0.3000),
    (-2.3000, 2.025, 0.3000),
    (-3.0000, 2.250, 0.3000),
    (-2.7000, 2.025, 0.3000),
    (-3.0000, 1.800, 0.3000),
    (-2.7000, 1.800, 0.3000),
    (-2.7000, 1.575, 0.0000),
    (-2.7000, 1.575, -0.3000),
    (-3.0000, 1.350, -0.3000),
    (-3.0000, 1.350, 0.0000),
    (-2.5000, 1.125, 0.0000),
    (-2.5000, 1.125, -0.3000),
    (-2.6500, 0.937, -0.3000),
    (-2.6500, 0.937, 0.0000),
    (-2.0000, 0.900, -0.3000),
    (-1.9000, 0.600, -0.3000),
    (-1.9000, 0.600, 0.0000),
    (-3.0000, 1.350, 0.3000),
    (-2.7000, 1.575, 0.3000),
    (-2.6500, 0.937, 0.3000),
    (-2.5000, 1.125, 0.3000),
    (-1.9000, 0.600, 0.3000),
    (-2.0000, 0.900, 0.3000),
    (1.7000, 1.425, 0.0000),
    (1.7000, 1.425, -0.6600),
    (1.7000, 0.600, -0.6600),
    (1.7000, 0.600, 0.0000),
    (2.6000, 1.425, 0.0000),
    (2.6000, 1.425, -0.6600),
    (3.1000, 0.825, -0.6600),
    (3.1000, 0.825, 0.0000),
    (2.3000, 2.100, 0.0000),
    (2.3000, 2.100, -0.2500),
    (2.4000, 2.025, -0.2500),
    (2.4000, 2.025, 0.0000),
    (2.7000, 2.400, 0.0000),
    (2.7000, 2.400, -0.2500),
    (3.3000, 2.400, -0.2500),
    (3.3000, 2.400, 0.0000),
    (1.7000, 0.600, 0.6600),
    (1.7000, 1.425, 0.6600),
    (3.1000, 0.825, 0.6600),
    (2.6000, 1.425, 0.6600),
    (2.4000, 2.025, 0.2500),
    (2.3000, 2.100, 0.2500),
    (3.3000, 2.400, 0.2500),
    (2.7000, 2.400, 0.2500),
    (2.8000, 2.475, 0.0000),
    (2.8000, 2.475, -0.2500),
    (3.5250, 2.4937, -0.2500),
    (3.5250, 2.4937, 0.0000),
    (2.9000, 2.475, 0.0000),
    (2.9000, 2.475, -0.1500),
    (3.4500, 2.512, -0.1500),
    (3.4500, 2.512, 0.0000),
    (2.8000, 2.400, 0.0000),
    (2.8000, 2.400, -0.1500),
    (3.2000, 2.400, -0.1500),
    (3.2000, 2.400, 0.0000),
    (3.5250, 2.4937, 0.2500),
    (2.8000, 2.475, 0.2500),
    (3.4500, 2.512, 0.1500),
    (2.9000, 2.475, 0.1500),
    (3.2000, 2.400, 0.1500),
    (2.8000, 2.400, 0.1500),
    (0.0000, 3.150, 0.0000),
    (0.0000, 3.150, -0.0020),
    (0.0020, 3.150, 0.0000),
    (0.8000, 3.150, 0.0000),
    (0.8000, 3.150, -0.4500),
    (0.4500, 3.150, -0.8000),
    (0.0000, 3.150, -0.8000),
    (0.0000, 2.850, 0.0000),
    (0.2000, 2.700, 0.0000),
    (0.2000, 2.700, -0.1120),
    (0.1120, 2.700, -0.2000),
    (0.0000, 2.700, -0.2000),
    (-0.0020, 3.150, 0.0000),
    (-0.4500, 3.150, -0.8000),
    (-0.8000, 3.150, -0.4500),
    (-0.8000, 3.150, 0.0000),
    (-0.1120, 2.700, -0.2000),
    (-0.2000, 2.700, -0.1120),
    (-0.2000, 2.700, 0.0000),
    (0.0000, 3.150, 0.0020),
    (-0.8000, 3.150, 0.4500),
    (-0.4500, 3.150, 0.8000),
    (0.0000, 3.150, 0.8000),
    (-0.2000, 2.700, 0.1120),
    (-0.1120, 2.700, 0.2000),
    (0.0000, 2.700, 0.2000),
    (0.4500, 3.150, 0.8000),
    (0.8000, 3.150, 0.4500),
    (0.1120, 2.700, 0.2000),
    (0.2000, 2.700, 0.1120),
    (0.4000, 2.550, 0.0000),
    (0.4000, 2.550, -0.2240),
    (0.2240, 2.550, -0.4000),
    (0.0000, 2.550, -0.4000),
    (1.3000, 2.550, 0.0000),
    (1.3000, 2.550, -0.7280),
    (0.7280, 2.550, -1.3000),
    (0.0000, 2.550, -1.3000),
    (1.3000, 2.400, 0.0000),
    (1.3000, 2.400, -0.7280),
    (0.7280, 2.400, -1.3000),
    (0.0000, 2.400, -1.3000),
    (-0.2240, 2.550, -0.4000),
    (-0.4000, 2.550, -0.2240),
    (-0.4000, 2.550, 0.0000),
    (-0.7280, 2.550, -1.3000),
    (-1.3000, 2.550, -0.7280),
    (-1.3000, 2.550, 0.0000),
    (-0.7280, 2.400, -1.3000),
    (-1.3000, 2.400, -0.7280),
    (-1.3000, 2.400, 0.0000),
    (-0.4000, 2.550, 0.2240),
    (-0.2240, 2.550, 0.4000),
    (0.0000, 2.550, 0.4000),
    (-1.3000, 2.550, 0.7280),
    (-0.7280, 2.550, 1.3000),
    (0.0000, 2.550, 1.3000),
    (-1.3000, 2.400, 0.7280),
    (-0.7280, 2.400, 1.3000),
    (0.0000, 2.400, 1.3000),
    (0.2240, 2.550, 0.4000),
    (0.4000, 2.550, 0.2240),
    (0.7280, 2.550, 1.3000),
    (1.3000, 2.550, 0.7280),
    (0.7280, 2.400, 1.3000),
    (1.3000, 2.400, 0.7280),
    (0.0000, 0.000, 0.0000),
    (1.5000, 0.150, 0.0000),
    (1.5000, 0.150, 0.8400),
    (0.8400, 0.150, 1.5000),
    (0.0000, 0.150, 1.5000),
    (1.5000, 0.075, 0.0000),
    (1.5000, 0.075, 0.8400),
    (0.8400, 0.075, 1.5000),
    (0.0000, 0.075, 1.5000),
    (1.4250, 0.000, 0.0000),
    (1.4250, 0.000, 0.7980),
    (0.7980, 0.000, 1.4250),
    (0.0000, 0.000, 1.4250),
    (-0.8400, 0.150, 1.5000),
    (-1.5000, 0.150, 0.8400),
    (-1.5000, 0.150, 0.0000),
    (-0.8400, 0.075, 1.5000),
    (-1.5000, 0.075, 0.8400),
    (-1.5000, 0.075, 0.0000),
    (-0.7980, 0.000, 1.4250),
    (-1.4250, 0.000, 0.7980),
    (-1.4250, 0.000, 0.0000),
    (-1.5000, 0.150, -0.8400),
    (-0.8400, 0.150, -1.5000),
    (0.0000, 0.150, -1.5000),
    (-1.5000, 0.075, -0.8400),
    (-0.8400, 0.075, -1.5000),
    (0.0000, 0.075, -1.5000),
    (-1.4250, 0.000, -0.7980),
    (-0.7980, 0.000, -1.4250),
    (0.0000, 0.000, -1.4250),
    (0.8400, 0.150, -1.5000),
    (1.5000, 0.150, -0.8400),
    (0.8400, 0.075, -1.5000),
    (1.5000, 0.075, -0.8400),
    (0.7980, 0.000, -1.4250),
    (1.4250, 0.000, -0.7980)
    );
function GetPatchValue(N: Integer): TVector;
begin
  Result.Vec3 := cTeapotVertices[CTeapotIndices[N]-1];
end;
{$ENDREGION}
var
  attr: TAttribBuffer;
  Vertices: TVec3List;
  TexCoords: TVec2List;
  Normals, Tangents, Binormals: TVec3List;
  x: TVector;
  P, P1, P2, I, J, L: Integer;
  invDiv, u, v, u1, v1: Float;
  B0, B1, B2, B3, T0, T1, T2: TVector;
  vp, dv, du: array[0..3] of TVector;
  function GetIndex(x, y: Integer): Integer;
  begin
    Result := x + y*(aDivision+1) + p *(aDivision+1)*(aDivision+1);
  end;
begin
  result:=Storage.CreateVertexObject;
  Vertices:=TVec3List.Create;
  Normals:=TVec3List.Create;
  Tangents:=TVec3List.Create;
  Binormals:=TVec3List.Create;
  TexCoords:=TVec2List.Create;

  invDiv := 1 / aDivision;
  for P := 0 to 31 do begin
    P1 := P * 16;
    for I := 0 to aDivision do begin
      v := I * invDiv;
      v1 := 1 - v;
      for J := 0 to aDivision do begin
        u := J * invDiv;
        u1 := 1 - u;
        B0.SetVector(u1 * u1 * u1, v1 * v1 * v1);
        B1.SetVector(3 * u * u1 * u1, 3 * v * v1 * v1);
        B2.SetVector(3 * u * u * u1, 3 * v * v * v1);
        B3.SetVector(u * u * u, v * v * v);

        for L := 0 to 3 do begin
          P2 := P1 + L * 4;
          vp[L] := GetPatchValue(P2 + 0) * B0.X;
          vp[L] := vp[L] + GetPatchValue(P2 + 1) * B1.X;
          vp[L] := vp[L] + GetPatchValue(P2 + 2) * B2.X;
          vp[L] := vp[L] + GetPatchValue(P2 + 3) * B3.X;
        end;
        vp[0].SetScale(B0.Y);
        vp[1].SetScale(B1.Y);
        vp[2].SetScale(B2.Y);
        vp[3].SetScale(B3.Y);
        vp[0] := vp[0] + vp[1] + vp[2]+  vp[3];
        Vertices.Add(vp[0].Scale(0.5).Vec3);

        T0.SetVector(u1 * u1, v1 * v1);
        T1.SetVector(2 * u * u1, 2 * v * v1);
        T2.SetVector( u * u, v * v);

        x := GetPatchValue(P1 + 4) - GetPatchValue(P1 + 0);
        dv[0] := x.Scale(B0.X);
        x := GetPatchValue(P1 + 5) - GetPatchValue(P1 + 1);
        dv[0] := dv[0] + x.Scale(B1.X);
        x := GetPatchValue(P1 + 6) - GetPatchValue(P1 + 2);
        dv[0] := dv[0] + x.Scale(B2.X);
        x := GetPatchValue(P1 + 7) - GetPatchValue(P1 + 3);
        dv[0] := dv[0] + x.Scale(B3.X);
        dv[0].SetScale(T0.Y);

        x := GetPatchValue(P1 + 8) - GetPatchValue(P1 + 4);
        dv[1] := x.Scale(B0.X);
        x := GetPatchValue(P1 + 9) - GetPatchValue(P1 + 5);
        dv[1] := dv[1] + x.Scale(B1.X);
        x := GetPatchValue(P1 + 10) - GetPatchValue(P1 + 6);
        dv[1] := dv[1] + x.Scale(B2.X);
        x := GetPatchValue(P1 + 11) - GetPatchValue(P1 + 7);
        dv[1] := dv[1] + x.Scale(B3.X);
        dv[1].SetScale(T1.Y);

        x := GetPatchValue(P1 + 12) - GetPatchValue(P1 + 8);
        dv[2] := x.Scale(B0.X);
        x := GetPatchValue(P1 + 13) - GetPatchValue(P1 + 9);
        dv[2] := dv[2] + x.Scale(B1.X);
        x := GetPatchValue(P1 + 14) - GetPatchValue(P1 + 10);
        dv[2] := dv[2] + x.Scale(B2.X);
        x := GetPatchValue(P1 + 15) - GetPatchValue(P1 + 11);
        dv[2] := dv[2] + x.Scale(B3.X);
        dv[2].SetScale(T2.Y);

        dv[0] := dv[0] + dv[1] + dv[2];
        Tangents.Add(dv[0].Normalize().Vec3);

        x := GetPatchValue(P1 + 1) - GetPatchValue(P1 + 0);
        du[0] := x.Scale(T0.X);
        x := GetPatchValue(P1 + 2) - GetPatchValue(P1 + 1);
        du[0] := du[0] + x.Scale(T1.X);
        x := GetPatchValue(P1 + 3) - GetPatchValue(P1 + 2);
        du[0] := du[0] + x.Scale(T2.X);
        du[0].SetScale(B0.Y);

        x := GetPatchValue(P1 + 5) - GetPatchValue(P1 + 4);
        du[1] := x.Scale(T0.X);
        x := GetPatchValue(P1 + 6) - GetPatchValue(P1 + 5);
        du[1] := du[1] + x.Scale(T1.X);
        x := GetPatchValue(P1 + 7) - GetPatchValue(P1 + 6);
        du[1] := du[1] + x.Scale(T2.X);
        du[1].SetScale(B1.Y);

        x := GetPatchValue(P1 + 9) - GetPatchValue(P1 + 8);
        du[2] := x.Scale(T0.X);
        x := GetPatchValue(P1 + 10) - GetPatchValue(P1 + 9);
        du[2] := du[2] + x.Scale(T1.X);
        x := GetPatchValue(P1 + 11) - GetPatchValue(P1 + 10);
        du[2] := du[2] + x.Scale(T2.X);
        du[2].SetScale(B2.Y);

        x := GetPatchValue(P1 + 13) - GetPatchValue(P1 + 12);
        du[3] := x.Scale(T0.X);
        x := GetPatchValue(P1 + 14) - GetPatchValue(P1 + 13);
        du[3] := du[3] + x.Scale(T1.X);
        x := GetPatchValue(P1 + 15) - GetPatchValue(P1 + 14);
        du[3] := du[3] + x.Scale(T2.X);
        du[3].SetScale(B3.Y);

        du[0] := du[0] + du[1] + du[2] + du[3];
        Binormals.Add(du[0].Normalize().Vec3);
        Normals.Add(dv[0].Cross(du[0]).Vec3);
        TexCoords.Add(TVector.Make(u, v).Vec2);
      end;
    end;
  end;

  attr:=Storage.CreateAttribBuffer(CAttribSematics[atVertex].Name,3,vtFloat,0);
  attr.Buffer.Allocate(Vertices.Size,Vertices.Data);
  attr.Buffer.SetDataHandler(Vertices);
  attr.SetAttribSemantic(atVertex);
  //attr.SetAttribLocation(CAttribSematics[atVertex].Location);
  result.AddAttrib(attr,true);

  attr:=Storage.CreateAttribBuffer(CAttribSematics[atNormal].Name,3,vtFloat,0);
  attr.Buffer.Allocate(Normals.Size,Normals.Data);
  attr.Buffer.SetDataHandler(Normals);
  attr.SetAttribSemantic(atNormal);
  //attr.SetAttribLocation(CAttribSematics[atNormal].Location);
  result.AddAttrib(attr);

  attr:=Storage.CreateAttribBuffer(CAttribSematics[atTangent].Name,3,vtFloat,0);
  attr.Buffer.Allocate(Tangents.Size,Tangents.Data);
  attr.Buffer.SetDataHandler(Tangents);
  attr.SetAttribSemantic(atTangent);
  //attr.SetAttribLocation(CAttribSematics[atTangent].Location);
  result.AddAttrib(attr);

  attr:=Storage.CreateAttribBuffer(CAttribSematics[atBinormal].Name,3,vtFloat,0);
  attr.Buffer.Allocate(Binormals.Size,Binormals.Data);
  attr.Buffer.SetDataHandler(Binormals);
  attr.SetAttribSemantic(atBinormal);
  //attr.SetAttribLocation(CAttribSematics[atBinormal].Location);
  result.AddAttrib(attr);

  attr:=Storage.CreateAttribBuffer(CAttribSematics[atTexCoord0].Name,2,vtFloat,0);
  attr.Buffer.Allocate(TexCoords.Size,TexCoords.Data);
  attr.Buffer.SetDataHandler(TexCoords);
  attr.SetAttribSemantic(atTexCoord0);
  //attr.SetAttribLocation(CAttribSematics[atTexCoord0].Location);
  result.AddAttrib(attr);

  result.FaceType:=ftTriangles;
  for P := 0 to 31 do begin
    for I := 0 to aDivision - 1 do
      for J := 0 to aDivision - 1 do
      begin
        result.AddTriangle(GetIndex(J+1, I), GetIndex(J, I), GetIndex(J, I+1));
        result.AddTriangle(GetIndex(J, I+1), GetIndex(J+1, I+1), GetIndex(J+1, I));
      end;
    end;
end;

function CreateSprite(aWidth, aHeight: Float): TVertexObject;
var attr: TAttribBuffer;
    Vertices: TVec2List;
    TexCoords: TVec2List;
begin
  result:=Storage.CreateVertexObject;
  Vertices:=TVec2List.Create;
  TexCoords:=TVec2List.Create;
  aWidth := aWidth / 2;
  aHeight := aHeight / 2;

  Vertices.Add( Vec2Make( -aWidth, -aHeight));  TexCoords.Add( Vec2Make( 0, 0 ));
  Vertices.Add( Vec2Make( -aWidth, aHeight ));  TexCoords.Add( Vec2Make( 0, 1 ));
  Vertices.Add( Vec2Make( aWidth, aHeight ));   TexCoords.Add( Vec2Make( 1, 1 ));
  Vertices.Add( Vec2Make( aWidth, -aHeight ));  TexCoords.Add( Vec2Make( 1, 0 ));

  attr:=Storage.CreateAttribBuffer(CAttribSematics[atVertex].Name,2,vtFloat,0);
  attr.Buffer.Allocate(Vertices.Size, Vertices.Data);
  attr.Buffer.SetDataHandler(Vertices);
  attr.SetAttribSemantic(atVertex);
  result.AddAttrib(attr,true);

  attr:=Storage.CreateAttribBuffer(CAttribSematics[atTexCoord0].Name,2,vtFloat,0);
  attr.Buffer.Allocate(TexCoords.Size,TexCoords.Data);
  attr.Buffer.SetDataHandler(TexCoords);
  attr.SetAttribSemantic(atTexCoord0);
  result.AddAttrib(attr);
  with result do begin
    AddTriangle(0,1,3);
    AddTriangle(3,1,2);
  end;
  result.FaceType:=ftTriangles;
end;

function CreateCylinder(aTopRadius, aBottomRadius, aHeight: Float; VSegments, HSegments: Integer): TVertexObject;
var attr: TAttribBuffer;
    Vertices: TVec3List;
    TexCoords: TVec2List;
    Normals: TVec3List;
    i, j, c: integer;
    xt, y, zt, xb, zb, tx, ty, a, step: Float;
    pt, pb, p: TVector;
    nt, nb, nm, n: TVector;
begin
  result:=Storage.CreateVertexObject;
  Vertices:=TVec3List.Create;
  Normals:=TVec3List.Create;
  TexCoords:=TVec2List.Create;
  y := aHeight / 2;


  step := 2*pi/VSegments; a := 0;
  c := 0;
  nt := Vec3Make( 0, 1, 0 );
  nb := Vec3Make( 0, -1, 0 );

  tx := cos(a);
  ty := sin(a);
  xt := tx * aTopRadius;
  zt := ty * aTopRadius;
  for i:= 1 to VSegments do begin
    Vertices.Add( Vec3Make( 0, y, 0 )); TexCoords.Add( Vec2Make( 0.5, 0.5 )); Normals.Add(nt.Vec3);
    Vertices.Add( Vec3Make( xt, y, zt )); TexCoords.Add( Vec2Make( tx*0.5+0.5, ty*0.5+0.5 )); Normals.Add(nt.Vec3);
    a := a + step;
    tx := cos(a);
    ty := sin(a);
    xt := tx * aTopRadius;
    zt := ty * aTopRadius;
    Vertices.Add( Vec3Make( xt, y, zt )); TexCoords.Add( Vec2Make( tx*0.5+0.5, ty*0.5+0.5 )); Normals.Add(nt.Vec3);
    Result.AddTriangle(c,c+1,c+2); Inc(c, 3);
  end;

  a := 0;
  tx := cos(a);
  ty := sin(a);
  xb := tx * aBottomRadius;
  zb := ty * aBottomRadius;
  for i:= 1 to VSegments do begin
    Vertices.Add( Vec3Make( 0, -y, 0 )); TexCoords.Add( Vec2Make( 0.5, 0.5 )); Normals.Add(nb.Vec3);
    Vertices.Add( Vec3Make( xb, -y, zb )); TexCoords.Add( Vec2Make( -tx*0.5+0.5, ty*0.5+0.5 )); Normals.Add(nb.Vec3);
    a := a + step;
    tx := cos(a);
    ty := sin(a);
    xb := tx * aBottomRadius;
    zb := ty * aBottomRadius;
    Vertices.Add( Vec3Make( xb, -y, zb )); TexCoords.Add( Vec2Make( -tx*0.5+0.5, ty*0.5+0.5 )); Normals.Add(nb.Vec3);
    Result.AddTriangle(c,c+1,c+2); Inc(c, 3);
  end;

  a := 0;
  for i:= 0 to VSegments do begin
    tx := cos(a);
    ty := sin(a);
    xt := tx * aTopRadius;
    zt := ty * aTopRadius;
    xb := tx * aBottomRadius;
    zb := ty * aBottomRadius;
    pt := Vector(xt, y, zt);
    pb := Vector(xb, -y, zb);
    nm := Vector(tx, 0, ty);
    for j := 0 to HSegments do begin
      p := pb.Lerp(pt, j / HSegments);
      n := nt.Cross(nm);
      n := n.Cross((pt - pb).Normalize);
      Vertices.Add( p.Vec3); TexCoords.Add( Vec2Make( i / VSegments, j / HSegments )); Normals.Add(n.Vec3);
    end;
    a := a + step;
  end;

  for i:= 1 to VSegments do begin
    for j := 1 to HSegments do begin
      Result.AddTriangle(c,c+1,c+HSegments+1);
      Result.AddTriangle(c+HSegments+1,c+1,c+HSegments+2); Inc(c);
    end;
    Inc(c);
  end;

  attr:=Storage.CreateAttribBuffer(CAttribSematics[atVertex].Name,3,vtFloat,0);
  attr.Buffer.Allocate(Vertices.Size,Vertices.Data);
  attr.Buffer.SetDataHandler(Vertices);
  attr.SetAttribSemantic(atVertex);
  result.AddAttrib(attr,true);

  attr:=Storage.CreateAttribBuffer(CAttribSematics[atNormal].Name,3,vtFloat,0);
  attr.Buffer.Allocate(Normals.Size,Normals.Data);
  attr.Buffer.SetDataHandler(Normals);
  attr.SetAttribSemantic(atNormal);
  result.AddAttrib(attr);

  attr:=Storage.CreateAttribBuffer(CAttribSematics[atTexCoord0].Name,2,vtFloat,0);
  attr.Buffer.Allocate(TexCoords.Size,TexCoords.Data);
  attr.Buffer.SetDataHandler(TexCoords);
  attr.SetAttribSemantic(atTexCoord0);
  result.AddAttrib(attr);

  Result.FaceType:=ftTriangles;
end;

end.
