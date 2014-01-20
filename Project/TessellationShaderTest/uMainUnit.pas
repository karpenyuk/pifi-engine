unit uMainUnit;

interface

uses
  Classes, Graphics,
  Controls, Forms, Dialogs, uGLViewer,{$IFDEF FPC}ExtCtrls{$ELSE}Vcl.ExtCtrls{$ENDIF};

{$POINTERMATH ON}

const
  DECAL_RADIUS_SCALE = 1.0;
  DECAL_RADIUS = 0.5;
  MAX_DECALS = 50;
  DISPLACE_NORMAL_DIRECTION = false;

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
  uImageLoader,
  uWall,
  uMeshUtils,
  uDataAccess,
  uImageFormats;

var
  Mesh: TVertexObject;
  Extents: TExtents;
  Render: TBaseRender;
  Shader1: TGLSLShaderProgram;
  Drawer: TGLVertexObject;
  cameraPos: TVector;
  Model, View, Proj: TMatrix;
  WallSampler, StructSampler, DisplaceSampler: TTextureSampler;
  glWallSampler, glStructSampler, glDisplaceSampler: TGLTextureSampler;
  WallTex, StructTex, DisplaceTex, NormalTex: TTexture;
  glWallTex, glStructTex, glDisplaceTex, glNormalTex: TGLTextureObject;
  VDA1, VDA2: IVectorDataAccess;

  LeftOff: TDataList<TImageHolder>;

  DecalPositonSize: array [0 .. MAX_DECALS - 1] of TVector;
  DecalNormal: array [0 .. MAX_DECALS - 1] of TVector;
  DecalBinormal: array [0 .. MAX_DECALS - 1] of TVector;
  DecalTangent: array [0 .. MAX_DECALS - 1] of TVector;
  decalIndex: Integer = 0;
  upadateDamage: Boolean;

function BuildWall: TVertexObject;
var
  I, Index: Integer;
  v1, v2, v3, dp1, dp2: TVector;
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
    v1.Vec3 := WALLVERTICES[I].tc;
    v1[0] := v1[0] * 2;
    v1[1] := v1[1] * 2;
    tc.Add(v1.Vec3);
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
    v1.Vec3 := p[Index];
    v2.Vec3 := p[Index+1];
    v3.Vec3 := p[Index+2];
    dp1 := v2 - v1;
    dp2 := v3 - v1;
    v1 := dp1.Cross(dp2).Normalize;
    v2.Vec3 := n[Index];
    if v1 = v2 then
    begin
      Result.AddTriangle(Index, Index + 2, Index + 1);
      Result.AddTriangle(Index + 2, Index + 3, Index + 1);
    end
    else
    begin
      Result.AddTriangle(Index, Index + 1, Index + 2);
      Result.AddTriangle(Index + 3, Index + 2, Index + 1);
    end;
    Inc(Index, 4);
  end;
  Result.FaceType := ftPatches;
  Extents := Result.Extents;
end;

procedure MakeDamage(Origin, Direct: TVector);
var
  InvModel: TMatrix;
  MSOrigin, MSDirect: TVector;
  Intersection, Normal, decalLocation, vIntToDecal: TVector;
  decalRadius, minRadius, decalSize, distance: Single;
  vHitDirection, vNormal, vTangent, vBinormal: TVector;
  I: Integer;
begin
  Origin.W := 1;
  Direct.SetNormalize;

  InvModel := Model.Invert;
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
var
  i: integer;
begin
  glFinish;
  GLViewer1.OnRender := nil;
  glWallSampler.Destroy;
  glStructSampler.Destroy;
  glDisplaceSampler.Destroy;
  glWallTex.Destroy;
  glStructTex.Destroy;
  glDisplaceTex.Destroy;
  glNormalTex.Destroy;

  GLViewer1.Context.Deactivate;
  Mesh.Attribs[0].Destroy;
  Mesh.Attribs[0].Destroy;
  Mesh.Attribs[0].Destroy;
  Mesh.Destroy;
  Shader1.Destroy;
  Drawer.Destroy;
  WallTex.Destroy;
  StructTex.Destroy;
  DisplaceTex.Destroy;
  NormalTex.Destroy;
  WallSampler.Destroy;
  StructSampler.Destroy;
  DisplaceSampler.Destroy;

  for I := 0 to LeftOff.Count-1 do
      LeftOff[i].Destroy;
  LeftOff.Destroy;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLViewer1.Context.DebugContext := true;
  LeftOff := TDataList<TImageHolder>.Create;
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
  imgLoader: TImageLoader;
  r8Image: TImageHolder;
  I: Integer;
  WallStruct: PGLUbyte;
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

  WallSampler:=TTextureSampler.Create;
  WallSampler.WrapS := twRepeat;
  WallSampler.WrapT := twRepeat;
  WallSampler.minFilter := mnLinearMipmapLinear;
  WallSampler.magFilter := mgLinear;
  glWallSampler:=TGLTextureSampler.CreateFrom(WallSampler);

  imgLoader := TImageLoader.Create();
  LeftOff.Add(imgLoader);
  imgLoader.LoadImageFromFile(path + 'Wall.dds');
  imgLoader.VolumeToArray;

  WallTex := imgLoader.CreateTexture();
  WallTex.GenerateMipMaps := true;

  glWallTex := TGLTextureObject.CreateFrom(WallTex);
  glWallTex.TextureSampler := WallSampler;
  glWallTex.UploadTexture;


  StructSampler:=TTextureSampler.Create;
  StructSampler.WrapS := twClampToEdge;
  StructSampler.minFilter := mnNearest;
  StructSampler.magFilter := mgNearest;
  glStructSampler:=TGLTextureSampler.CreateFrom(StructSampler);

  r8Image:=TImageSampler.CreateBitmap(
    TImageFormatSelector.CreateUInt8(bfRed), 532, 0, false);
  LeftOff.Add(r8Image);
  StructTex := TTexture.CreateOwned(r8Image);
  StructTex.GenerateMipMaps := false;
  WallStruct :=  StructTex.ImageHolder.Data;
  WallStruct[0] := 0; // Wallpaper
  for I := 1 to 20 do              // Plaster
      WallStruct[I] := 1 + I and 1;
  for I := 21 to 21 + 120 do       // Halfbreak 1
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

  glStructTex := TGLTextureObject.CreateFrom(StructTex);
  glStructTex.TextureSampler := StructSampler;
  glStructTex.UploadTexture;

  DisplaceSampler:=TTextureSampler.Create;
  DisplaceSampler.WrapS := twRepeat;
  DisplaceSampler.WrapT := twRepeat;
  DisplaceSampler.minFilter := mnLinear;
  DisplaceSampler.magFilter := mgLinear;
  glDisplaceSampler:=TGLTextureSampler.CreateFrom(DisplaceSampler);

  imgLoader := TImageLoader.Create();
  LeftOff.Add(imgLoader);
  imgLoader.LoadImageFromFile(path + 'Damage_Displacement.dds');

  DisplaceTex := imgLoader.CreateTexture();
  DisplaceTex.GenerateMipMaps := false;

  glDisplaceTex := TGLTextureObject.CreateFrom(DisplaceTex);
  glDisplaceTex.TextureSampler := DisplaceSampler;
  glDisplaceTex.UploadTexture;


  imgLoader := TImageLoader.Create();
  LeftOff.Add(imgLoader);
  imgLoader.LoadImageFromFile(path + 'Damage_Normal.dds');

  NormalTex := imgLoader.CreateTexture();
  NormalTex.GenerateMipMaps := true;

  glNormalTex:= TGLTextureObject.CreateFrom(NormalTex);
  glNormalTex.TextureSampler := WallSampler;
  glNormalTex.UploadTexture;

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
  cameraPos := TVector.Make(0, 0, -6);
  View := TMatrix.LookAtMatrix(cameraPos, VecNull, vecY);
  Model := TMatrix.TranslationMatrix(Extents.eMid.Negate);

  Drawer := TGLVertexObject.CreateFrom(Mesh);
  Drawer.Shader := Shader1;

  glDisable(GL_CULL_FACE);
//  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
end;

procedure TForm1.GLViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  VP: TMatrix;
  ScreenPos, ViewPort, WorldPos: TVector;
begin
  MX := X;
  MY := Y;
  if Shift = [ssRight] then
  begin
    VP := View * Proj;
    ScreenPos.SetVector(MX, GLViewer1.Height - MY);
    ViewPort.SetVector(0, 0, GLViewer1.Width, GLViewer1.Height);
    if VP.UnProject(ScreenPos, ViewPort, WorldPos) then
      MakeDamage(cameraPos, WorldPos - cameraPos);
  end;
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
  Shader1.SetUniform('TessellationFactor', 16.0);
  Shader1.SetUniform('DisplacementScaleBias', TVector.Make(-0.2, 0.3).Vec2);

  if upadateDamage then
  begin
    Shader1.SetUniform('DecalPositionSize', DecalPositonSize);
    Shader1.SetUniform('DecalNormal', DecalNormal);
    Shader1.SetUniform('DecalBinormal', DecalBinormal);
    Shader1.SetUniform('DecalTangent', DecalTangent);
    upadateDamage := false;
  end;

  glActiveTexture(GL_TEXTURE3);
  glDisplaceSampler.Bind(3);
  glBindTexture(GL_TEXTURE_2D, glDisplaceTex.Id);

  glActiveTexture(GL_TEXTURE2);
  glWallSampler.Bind(2);
  glBindTexture(GL_TEXTURE_2D, glNormalTex.Id);

  glActiveTexture(GL_TEXTURE1);
  glWallSampler.Bind(1);
  glBindTexture(GL_TEXTURE_2D_ARRAY, glWallTex.Id);

  glActiveTexture(GL_TEXTURE0);
  glStructSampler.Bind(0);
  glBindTexture(GL_TEXTURE_1D, glStructTex.Id);

  glPatchParameteri(GL_PATCH_VERTICES, 3);
  Drawer.RenderVO();
  Shader1.UnApply;
end;

end.
