unit uStorage;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, Types, uBaseTypes, uMiscUtils, uLists, uGenericsRBTree,
  uPersistentClasses, uRenderResource;

type

  TResourceList = GRedBlackTree < TGUID, TPersistentResource >;

  TStorage = class
  private
    class var FStorageHandle: TPersistentResource;
    class var FResources: TResourceList;
    class constructor Create;
    class destructor Destroy;
  public
    class function CreateProgram: TShaderProgram;
    class function CreateMaterial: TMaterial;
    class function CreateTextureSample: TTextureSampler;
    class function CreateImageHolder: TImageHolder; overload;
    class function CreateImageHolder(aFormatCode: cardinal;
                     aImageType: TImageType = itBitmap): TImageHolder; overload;
    class function CreateTexture(aImageHolder: TImageHolder): TTexture;
    class function CreateBlending: TCustomBlending;
    class function CreateMaterialObject: TMaterialObject;
    class function CreateBufferObject: TBufferObject;
    class function CreateAttribObject: TAttribObject; overload;
    class function CreateAttribObject(AttrName: ansistring; aSize: TValueComponent;
                     AType: TValueType = vtFloat; AStride: integer = 0;
                     BuffType: TBufferType = btArray): TAttribObject; overload;
    class function CreateAttribBuffer(AttrName: ansistring; aSize: TValueComponent;
                     AType: TValueType = vtFloat; AStride: integer = 0;
                     BuffType: TBufferType = btArray): TAttribBuffer;
    class function CreateVertexObject: TVertexObject;
    class function CreateMesh: TMesh;
    class function CreateLODsController: TLODsController;
    class function CreateMeshObject: TMeshObject;
    class function CreateSceneObject: TSceneObject;
    class function CreateFrameBuffer: TFrameBuffer;
    class function CreateCamera: TSceneCamera;
    class function CreateLight: TLightSource;



  end;

implementation

function ResourceComparer(const Item1, Item2: TGUID): Integer;
begin
  result := CompareGUID(Item1, Item2);
end;

{ TStorage }

class constructor TStorage.Create;
begin
  FStorageHandle:=TPersistentResource.Create;
  FResources:=TResourceList.Create(ResourceComparer, nil);
end;

class function TStorage.CreateAttribBuffer(AttrName: ansistring;
  aSize: TValueComponent; AType: TValueType; AStride: integer;
  BuffType: TBufferType): TAttribBuffer;
begin
  result := TAttribBuffer.CreateAndSetup(AttrName, aSize, AType,
    AStride, BuffType, FStorageHandle);
  FResources.Add(result.GUID, result);
end;

class function TStorage.CreateAttribObject(AttrName: ansistring;
  aSize: TValueComponent; AType: TValueType; AStride: integer;
  BuffType: TBufferType): TAttribObject;
begin
  result := TAttribBuffer.CreateAndSetup(AttrName, aSize, AType,
    AStride, BuffType, FStorageHandle);
  FResources.Add(result.GUID, result);
end;

class function TStorage.CreateAttribObject: TAttribObject;
begin
  result := TAttribObject.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
end;

class function TStorage.CreateBlending: TCustomBlending;
begin
  result := TCustomBlending.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
end;

class function TStorage.CreateBufferObject: TBufferObject;
begin
  result := TBufferObject.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
end;

class function TStorage.CreateCamera: TSceneCamera;
begin
  result := TSceneCamera.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
end;

class function TStorage.CreateFrameBuffer: TFrameBuffer;
begin
  result := TFrameBuffer.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
end;

class function TStorage.CreateImageHolder(aFormatCode: cardinal;
  aImageType: TImageType): TImageHolder;
begin
  result := TImageHolder.Create(aFormatCode,aImageType);
  result.Owner := FStorageHandle;
  FResources.Add(result.GUID, result);
end;

class function TStorage.CreateImageHolder: TImageHolder;
begin
  result := CreateImageHolder.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
end;

class function TStorage.CreateLight: TLightSource;
begin
  result := TLightSource.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
end;

class function TStorage.CreateLODsController: TLODsController;
begin
  result := TLODsController.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
end;

class function TStorage.CreateMaterial: TMaterial;
begin
  result := TMaterial.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
end;

class function TStorage.CreateMaterialObject: TMaterialObject;
begin
  result := TMaterialObject.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
end;

class function TStorage.CreateMesh: TMesh;
begin
  result := TMesh.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
end;

class function TStorage.CreateMeshObject: TMeshObject;
begin
  result := TMeshObject.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
end;

class function TStorage.CreateProgram: TShaderProgram;
begin
  result:=TShaderProgram.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
end;

class function TStorage.CreateSceneObject: TSceneObject;
begin
  result := TSceneObject.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
end;

class function TStorage.CreateTexture(aImageHolder: TImageHolder): TTexture;
begin
  result := TTexture.CreateOwned(aImageHolder, FStorageHandle);
  FResources.Add(result.GUID, result);
end;

class function TStorage.CreateTextureSample: TTextureSampler;
begin
  result := TTextureSampler.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
end;

class function TStorage.CreateVertexObject: TVertexObject;
begin
  result := TVertexObject.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
end;

class destructor TStorage.Destroy;
var i: integer;
begin
  FResources.Clear;
  FResources.Free;
  FStorageHandle.Free;
end;

initialization

finalization

end.
