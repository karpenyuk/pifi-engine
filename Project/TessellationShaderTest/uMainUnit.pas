unit uMainUnit;

interface

uses
  Classes, Graphics,
  Controls, Forms, Dialogs, uGLViewer;

const
  DECAL_RADIUS_SCALE = 0.25;
  DECAL_RADIUS = 1.0;
  MAX_DECALS = 50;
  DISPLACE_NORMAL_DIRECTION = true;

type
  TForm1 = class(TForm)
    GLViewer1: TGLViewer;
    procedure GLViewer1ContextReady(Sender: TObject);
    procedure GLViewer1CanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure GLViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure GLViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLViewer1Render(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    MX, MY: Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


uses
  uBaseRenders,
  uBaseTypes,
  uBaseGL,
  uRenderResource,
  uVMath,
  dglOpenGL,
  uLists,
  ImageLoader,
  uWall,
  uMeshUtils,
  uDataAccess;

var
  Mesh: TVertexObject;
  Extents: TExtents;
  Render: TBaseRender;
  Shader1: TGLSLShaderProgram;
  Drawer: TGLVertexObject;
  cameraPos: TVector;
  Model, View, Proj: TMatrix;
  WallTexId, StructTexId, DisplaceTexId, NormalTexId: GLuint;
  VDA1, VDA2: IVectorDataAccess;

  DecalPositonSize: array [0 .. MAX_DECALS - 1] of TVector;
  DecalNormal: array [0 .. MAX_DECALS - 1] of TVector;
  DecalBinormal: array [0 .. MAX_DECALS - 1] of TVector;
  DecalTangent: array [0 .. MAX_DECALS - 1] of TVector;
  decalIndex: Integer = 0;
  upadateDamage: Boolean;

function BuildWall: TVertexObject;
var
  I, Index: Integer;
  v: Vec3;
  p, tc, n: TVec3List;
  attr: TAttribBuffer;
begin
  Result := TVertexObject.Create;
  p := TVec3List.Create;
  tc := TVec3List.Create;
  n := TVec3List.Create;

  for I := 0 to High(WALLVERTICES) do
  begin
    p.Add(WALLVERTICES[I].pos);
    v := WALLVERTICES[I].tc;
    v[0] := v[0] * 2;
    v[1] := v[1] * 2;
    tc.Add(v);
    n.Add(WALLVERTICES[I].norm);
  end;

  attr := TAttribBuffer.CreateAndSetup(CAttribSematics[atVertex].Name, 3);
  attr.SetAttribSemantic(atVertex);
  attr.Buffer.SetDataHandler(p, true);
  Result.AddAttrib(attr, true);
  VDA1 := TVectorDataAccess.Create(p.Data, vtFloat, 3, p.ItemSize, p.Count);

  attr := TAttribBuffer.CreateAndSetup(CAttribSematics[atNormal].Name, 3);
  attr.SetAttribSemantic(atNormal);
  attr.Buffer.SetDataHandler(n, true);
  Result.AddAttrib(attr);
  VDA2 := TVectorDataAccess.Create(n.Data, vtFloat, 3, p.ItemSize, n.Count);

  attr := TAttribBuffer.CreateAndSetup(CAttribSematics[atTexCoord0].Name, 3);
  attr.SetAttribSemantic(atTexCoord0);
  attr.Buffer.SetDataHandler(tc, true);
  Result.AddAttrib(attr);

  Index := 0;
  for I := 0 to High(WALLVERTICES) div 4 do
  begin
    Result.AddTriangle(Index, Index + 1, Index + 2);
    Result.AddTriangle(Index + 2, Index + 3, Index + 1);
    Inc(Index, 4);
  end;
  Result.FaceType := ftPatches;
  Extents := Result.Extents;
end;

procedure MakeDamage;
var
  Origin, Direct: TVector;
  InvModel: TMatrix;
  MSOrigin, MSDirect: TVector;
  Intersection, Normal, decalLocation, vIntToDecal: TVector;
  decalRadius, minRadius, decalSize, distance: Single;
  vHitDirection, vNormal, vTangent, vBinormal: TVector;
  I: Integer;
begin
  Origin := TVector.Make(1, 1, -2, 1);
  Direct := TVector.Make(0, 0, 1);

  InvModel := TMatrix.TranslationMatrix(Extents.eMid);//Model.Invert;
  MSOrigin := InvModel.Transform(Origin);
  MSDirect := InvModel.Transform(Direct);

  if MeshUtils.RayCastIntersect(VDA1, VDA2, Mesh.GetIndices, MSOrigin, MSDirect,
    Intersection, Normal) then
  begin
    Intersection := Model.Transform(Intersection);
    Normal := Model.Transform(Normal);

    decalRadius := DECAL_RADIUS * DECAL_RADIUS_SCALE;
    minRadius := decalRadius;

    // see if this decal will overlap with any existing decals
    for I := 0 to MAX_DECALS - 1 do
    begin
      if DecalNormal[I].IsNull then
          break;
      decalLocation.Vec3 := DecalPositonSize[I].Vec3;
      decalSize := DecalPositonSize[I].W;
      vIntToDecal := decalLocation - Intersection;
      distance := vIntToDecal.Length;
      if distance < (decalRadius + decalSize) then
      begin
        // adjust the size of the decal to fit
        decalRadius := distance - decalSize;
        decalRadius := 0.9 * decalRadius;
        if decalRadius < minRadius then
            minRadius := decalRadius;
      end;
    end;

    if DISPLACE_NORMAL_DIRECTION then
    begin
      vHitDirection := Normal.Negate;
    end
    else
    begin
      vHitDirection := Intersection - Origin;
      vHitDirection.SetNormalize;
    end;
    vHitDirection.CreateOrthonormalBasis(vNormal, vTangent, vBinormal);
    Intersection.W := minRadius;
    DecalPositonSize[decalIndex] := Intersection;
    DecalNormal[decalIndex] := vNormal;
    DecalBinormal[decalIndex] := vBinormal;
    DecalTangent[decalIndex] := vTangent;
    Inc(decalIndex);
    if decalIndex >= MAX_DECALS then
        decalIndex := 0;

    upadateDamage := true;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  glFinish;
  glDeleteTextures(1, @WallTexId);
  glDeleteTextures(1, @StructTexId);
  glDeleteTextures(1, @DisplaceTexId);
  glDeleteTextures(1, @NormalTexId);
  GLViewer1.OnRender := nil;
  GLViewer1.Context.Deactivate;
  Mesh.Attribs[0].Destroy;
  Mesh.Attribs[0].Destroy;
  Mesh.Attribs[0].Destroy;
  Mesh.Destroy;
  Shader1.Destroy;
  Drawer.Destroy;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLViewer1.Context.DebugContext := true;
end;

procedure TForm1.GLViewer1CanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  Proj := TMatrix.PerspectiveMatrix(60, NewWidth / NewHeight, 0.1, 100);
end;

procedure TForm1.GLViewer1ContextReady(Sender: TObject);
var
  path: string;
  ver: TApiVersion;
  img: TImageDesc;
  I: Integer;
  WallStruct: array [0 .. 531] of GLUbyte;
begin
  ver.GAPI := avGL;
  ver.Version := 420;
  Render := vRegisteredRenders.GetCompatibleRender(ver);

{$IFDEF MSWindows}
  path := '..\..\Source\Media\'; { :-\ }
{$ENDIF}
{$IFDEF Linux}
  path := '../../Source/Media/'; { :-/ }
{$ENDIF}
  Mesh := BuildWall;

  img := LoadImage(path + 'Wall.dds');

  glGenTextures(1, @WallTexId);
  glBindTexture(GL_TEXTURE_2D_ARRAY, WallTexId);
  glTextureParameterfEXT(
    WallTexId,
    GL_TEXTURE_2D_ARRAY,
    GL_GENERATE_MIPMAP_SGIS,
    GL_TRUE);
  glTextureParameterfEXT(WallTexId, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_S,
    GL_REPEAT);
  glTextureParameterfEXT(WallTexId, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_T,
    GL_REPEAT);
  glTextureParameterfEXT(WallTexId, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MAG_FILTER,
    GL_LINEAR);
  glTextureParameterfEXT(WallTexId, GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MIN_FILTER,
    GL_LINEAR_MIPMAP_LINEAR);
  glTextureImage3DEXT(WallTexId, GL_TEXTURE_2D_ARRAY, 0, img.InternalFormat,
    img.Width, img.Height, img.Depth, 0, img.ColorFormat, img.DataType,
    img.Data);
  FreeMem(img.Data);

  WallStruct[0] := 0; // Wallpaper
  for I := 1 to 20 do
      WallStruct[I] := 1 + I and 1; // Plaster
  for I := 21 to 21 + 120 do // Halfbreak 1
      WallStruct[I] := 3;
  for I := 22 + 120 to 21 + 130 do // Seam
      WallStruct[I] := 4;
  for I := 22 + 130 to 21 + 250 do // Halfbreak 2
      WallStruct[I] := 3;
  for I := 22 + 250 to 21 + 260 do // Seam
      WallStruct[I] := 5;
  for I := 22 + 260 to 21 + 380 do // Halfbreak 3
      WallStruct[I] := 3;
  for I := 22 + 380 to 21 + 390 do // Seam
      WallStruct[I] := 4;
  for I := 22 + 390 to 21 + 510 do // Halfbreak 4
      WallStruct[I] := 3;

  glGenTextures(1, @StructTexId);
  glBindTexture(GL_TEXTURE_1D, StructTexId);
  glTextureParameterfEXT(StructTexId, GL_TEXTURE_1D, GL_TEXTURE_WRAP_S,
    GL_CLAMP_TO_EDGE);
  glTextureParameterfEXT(StructTexId, GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER,
    GL_NEAREST);
  glTextureParameterfEXT(StructTexId, GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER,
    GL_NEAREST);
  glTextureImage1DEXT(StructTexId, GL_TEXTURE_1D, 0, GL_R8, Length(WallStruct),
    0, GL_RED, GL_UNSIGNED_BYTE, @WallStruct[0]);

  img := LoadImage(path + 'Damage_Displacement.dds');

  glGenTextures(1, @DisplaceTexId);
  glBindTexture(GL_TEXTURE_2D, DisplaceTexId);
  glTextureParameterfEXT(DisplaceTexId, GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,
    GL_CLAMP_TO_EDGE);
  glTextureParameterfEXT(DisplaceTexId, GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,
    GL_CLAMP_TO_EDGE);
  glTextureParameterfEXT(DisplaceTexId, GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,
    GL_LINEAR);
  glTextureParameterfEXT(DisplaceTexId, GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
    GL_LINEAR);
  glTextureImage2DEXT(DisplaceTexId, GL_TEXTURE_2D, 0, img.InternalFormat,
    img.Width, img.Height, 0, img.ColorFormat, img.DataType,
    img.Data);
  FreeMem(img.Data);

  img := LoadImage(path + 'Damage_Normal.dds');

  glGenTextures(1, @NormalTexId);
  glBindTexture(GL_TEXTURE_2D, NormalTexId);
  glTextureParameterfEXT(
    WallTexId,
    GL_TEXTURE_2D_ARRAY,
    GL_GENERATE_MIPMAP_SGIS,
    GL_TRUE);
  glTextureParameterfEXT(NormalTexId, GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,
    GL_CLAMP_TO_EDGE);
  glTextureParameterfEXT(NormalTexId, GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,
    GL_CLAMP_TO_EDGE);
  glTextureParameterfEXT(NormalTexId, GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,
    GL_LINEAR);
  glTextureParameterfEXT(NormalTexId, GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
    GL_LINEAR_MIPMAP_LINEAR);
  glTextureImage2DEXT(NormalTexId, GL_TEXTURE_2D, 0, img.InternalFormat,
    img.Width, img.Height, 0, img.ColorFormat, img.DataType,
    img.Data);
  FreeMem(img.Data);

  Shader1 := TGLSLShaderProgram.Create;

  Shader1.AttachShaderFromFile(stVertex, path + 'WallShader.Vert');
  Shader1.AttachShaderFromFile(stTessControl, path + 'WallShader.Ctrl');
  Shader1.AttachShaderFromFile(stTessEval, path + 'WallShader.Eval');
  Shader1.AttachShaderFromFile(stFragment, path + 'WallShader.Frag');

  Shader1.LinkShader;
  if Shader1.Error then
  begin
    showmessage(Shader1.Log);
    Halt(0);
  end;

  Proj := TMatrix.PerspectiveMatrix(60, GLViewer1.Width / GLViewer1.Height,
    0.1, 100);
  cameraPos := TVector.Make(0, 0, 6);
  View := TMatrix.LookAtMatrix(cameraPos, VecNull, vecY);
  Model := TMatrix.TranslationMatrix(Extents.eMid.Negate);

  Drawer := TGLVertexObject.CreateFrom(Mesh);
  Drawer.Shader := Shader1;

  glDisable(GL_CULL_FACE);
//  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

  MakeDamage;
end;

procedure TForm1.GLViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MX := X;
  MY := Y;
end;

procedure TForm1.GLViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Shift = [ssLeft] then
  begin
    cameraPos.RotateAround(VecNull, vecY, MY - Y, MX - X);
    View := TMatrix.LookAtMatrix(cameraPos, VecNull, vecY);
  end;
  MX := X;
  MY := Y;
end;

procedure TForm1.GLViewer1Render(Sender: TObject);
var
  VP: TMatrix;
begin
  GLViewer1.Context.ClearDevice;

  VP := View * Proj;
  Shader1.Apply;
  Shader1.SetUniform('ModelMatrix', Model.Matrix4);
  Shader1.SetUniform('ViewProjection', VP.Matrix4);
  Shader1.SetUniform('LightPosition', cameraPos.Vec3);
  Shader1.SetUniform('EyePosition', cameraPos.Vec3);
  Shader1.SetUniform('ScreenSize', TVector.Make(GLViewer1.Width,
    GLViewer1.Height).Vec2);
  Shader1.SetUniform('TessellationFactor', 3.0);
  Shader1.SetUniform('DisplacementScaleBias', TVector.Make(-0.3848734,
    0.1968906).Vec2);

  if upadateDamage then
  begin
    Shader1.SetUniform('DecalPositionSize', DecalPositonSize);
    Shader1.SetUniform('DecalNormal', DecalNormal);
    Shader1.SetUniform('DecalBinormal', DecalBinormal);
    Shader1.SetUniform('DecalTangent', DecalTangent);
    upadateDamage := false;
  end;

  glActiveTexture(GL_TEXTURE3);
  glBindTexture(GL_TEXTURE_2D, DisplaceTexId);
  glActiveTexture(GL_TEXTURE2);
  glBindTexture(GL_TEXTURE_2D, NormalTexId);
  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D_ARRAY, WallTexId);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_1D, StructTexId);

  glPatchParameteri(GL_PATCH_VERTICES, 3);
  Drawer.RenderVO();
  Shader1.UnApply;
end;

end.
