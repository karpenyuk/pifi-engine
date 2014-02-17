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
    Type
      TStorageHandler = class (TPersistentResource)
      public
        procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;
      end;
    class var FStorageHandle: TStorageHandler;
    class var FResources: TResourceList;
    class constructor Create;
    class destructor Destroy;
  public
    class procedure Delete(Resource: TPersistentResource);

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
  FStorageHandle:=TStorageHandler.Create;
  FResources:=TResourceList.Create(ResourceComparer, nil);
end;

class function TStorage.CreateAttribBuffer(AttrName: ansistring;
  aSize: TValueComponent; AType: TValueType; AStride: integer;
  BuffType: TBufferType): TAttribBuffer;
begin
  result := TAttribBuffer.CreateAndSetup(AttrName, aSize, AType,
    AStride, BuffType, FStorageHandle);
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class function TStorage.CreateAttribObject(AttrName: ansistring;
  aSize: TValueComponent; AType: TValueType; AStride: integer;
  BuffType: TBufferType): TAttribObject;
begin
  result := TAttribBuffer.CreateAndSetup(AttrName, aSize, AType,
    AStride, BuffType, FStorageHandle);
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class function TStorage.CreateAttribObject: TAttribObject;
begin
  result := TAttribObject.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class function TStorage.CreateBlending: TCustomBlending;
begin
  result := TCustomBlending.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class function TStorage.CreateBufferObject: TBufferObject;
begin
  result := TBufferObject.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class function TStorage.CreateCamera: TSceneCamera;
begin
  result := TSceneCamera.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class function TStorage.CreateFrameBuffer: TFrameBuffer;
begin
  result := TFrameBuffer.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class function TStorage.CreateImageHolder(aFormatCode: cardinal;
  aImageType: TImageType): TImageHolder;
begin
  result := TImageHolder.Create(aFormatCode,aImageType);
  result.Owner := FStorageHandle;
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class function TStorage.CreateImageHolder: TImageHolder;
begin
  result := CreateImageHolder.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class function TStorage.CreateLight: TLightSource;
begin
  result := TLightSource.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class function TStorage.CreateLODsController: TLODsController;
begin
  result := TLODsController.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class function TStorage.CreateMaterial: TMaterial;
begin
  result := TMaterial.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class function TStorage.CreateMaterialObject: TMaterialObject;
begin
  result := TMaterialObject.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class function TStorage.CreateMesh: TMesh;
begin
  result := TMesh.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class function TStorage.CreateMeshObject: TMeshObject;
begin
  result := TMeshObject.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class function TStorage.CreateProgram: TShaderProgram;
begin
  result:=TShaderProgram.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class function TStorage.CreateSceneObject: TSceneObject;
begin
  result := TSceneObject.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class function TStorage.CreateTexture(aImageHolder: TImageHolder): TTexture;
begin
  result := TTexture.CreateOwned(aImageHolder, FStorageHandle);
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class function TStorage.CreateTextureSample: TTextureSampler;
begin
  result := TTextureSampler.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class function TStorage.CreateVertexObject: TVertexObject;
begin
  result := TVertexObject.CreateOwned(FStorageHandle);
  FResources.Add(result.GUID, result);
  result.Subscribe(FStorageHandle);
end;

class procedure TStorage.Delete(Resource: TPersistentResource);
begin
  if Resource is TPersistentResource then
    FResources.Delete(Resource.GUID);
end;

class destructor TStorage.Destroy;
begin
  FResources.Clear;
  FResources.Free;
  FStorageHandle.Free;
end;

{ TStorage.TStorageHandler }

procedure TStorage.TStorageHandler.Notify(Sender: TObject; Msg: Cardinal;
  Params: pointer);
begin
  inherited;
  if assigned(Sender) then begin
    case Msg of
      NM_ObjectDestroyed:
        if Sender is TPersistentResource then begin
          FStorageHandle.UnSubscribe(TNotifiableObject(Sender));
          TStorage.Delete(TPersistentResource(Sender));
        end;
    end;
  end;
end;

initialization

finalization

end.
