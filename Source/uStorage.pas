unit uStorage;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, Types, uBaseTypes, uMiscUtils, uLists, uGenericsRBTree,
  uPersistentClasses, uRenderResource;

type
  TPersistentRef = TReference<TPersistentResource>;
  TPersistentResourceRef = TResourceReference<TPersistentResource>;

  TResourceList = class(GRedBlackTree < TGUID, TPersistentRef>)
  private
    procedure FreeResources(aOwner: TPersistentResource);
  public
    destructor Destroy; override;
  end;

  Storage = class
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
    class procedure AddResource(const aRes: TPersistentResource);
  public
    class procedure PutResource(Resource: TPersistentResource);
    class procedure Delete(Resource: TPersistentResource);
    class procedure RemoveReference(aResource: TPersistentResource);
    class function GetResource(const aGUID: TGUID): TPersistentResource;
    class function GetReference<T: class>(const aGUID: TGUID): IReference<T>; overload;

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
                     AType: TValueType = vtFloat; AOffset: integer = 0; AStride: integer = 0;
                     BuffType: TBufferType = btArray): TAttribObject; overload;
    class function CreateAttribBuffer(AttrName: ansistring; aSize: TValueComponent;
                     AType: TValueType = vtFloat; AOffset: integer = 0; AStride: integer = 0;
                     BuffType: TBufferType = btArray): TAttribBuffer;
    class function CreateVertexObject: TVertexObject;
    class function CreateMesh(vo: TVertexObject = nil): TMesh;
    class function CreateMeshObject: TMeshObject; overload;
    class function CreateMeshObject(aMesh: TMeshAssembly): TMeshObject; overload;
    class function CreateMeshObject(aMesh: TMesh): TMeshObject; overload;
    class function CreateSceneObject: TSceneObject;
    class function CreateFrameBuffer: TFrameBuffer;
    class function CreateCamera: TSceneCamera;
    class function CreateLight: TLightSource;
    class function CreateEffectPipeline: TEffectPipeline;

  end;

implementation

function ResourceComparer(const Item1, Item2: TGUID): Integer;
begin
  result := CompareGUID(Item1, Item2);
end;

procedure FreeGenericResource(AKey: TGUID; AValue: TPersistentResource; out AContinue: Boolean); inline;
begin
  if assigned(AValue) then AValue.Free; 
  AContinue:=true;
end;

{ TStorage }

//class procedure Storage.AddResource(const aRes: TPersistentResource);
class procedure Storage.AddResource(const aRes: TPersistentResource);
begin
  FResources.Add(aRes.GUID, TReference<TPersistentResource>.Create(aRes));
  aRes.Subscribe(FStorageHandle);
end;

class constructor Storage.Create;
begin
  FStorageHandle:=TStorageHandler.Create;
  FResources:=TResourceList.Create(ResourceComparer, nil);
end;

class function Storage.CreateAttribBuffer(AttrName: ansistring;
  aSize: TValueComponent; AType: TValueType; AOffset,AStride: integer;
  BuffType: TBufferType): TAttribBuffer;
begin
  result := TAttribBuffer.CreateAndSetup(AttrName, aSize, AType,
    AOffset, AStride, BuffType, FStorageHandle);
  AddResource(result);
end;

class function Storage.CreateAttribObject(AttrName: ansistring;
  aSize: TValueComponent; AType: TValueType; AOffset,AStride: integer;
  BuffType: TBufferType): TAttribObject;
begin
  result := TAttribBuffer.CreateAndSetup(AttrName, aSize, AType,
    AOffset, AStride, BuffType, FStorageHandle);
  AddResource(result);
end;

class function Storage.CreateAttribObject: TAttribObject;
begin
  result := TAttribObject.CreateOwned(FStorageHandle);
  AddResource(result);
end;

class function Storage.CreateBlending: TCustomBlending;
begin
  result := TCustomBlending.CreateOwned(FStorageHandle);
  AddResource(result);
end;

class function Storage.CreateBufferObject: TBufferObject;
begin
  result := TBufferObject.CreateOwned(FStorageHandle);
  AddResource(result);
end;

class function Storage.CreateCamera: TSceneCamera;
begin
  result := TSceneCamera.CreateOwned(FStorageHandle);
  AddResource(result);
end;

class function Storage.CreateEffectPipeline: TEffectPipeline;
begin
  result := TEffectPipeline.CreateOwned(FStorageHandle);
  AddResource(result);
end;

class function Storage.CreateFrameBuffer: TFrameBuffer;
begin
  result := TFrameBuffer.CreateOwned(FStorageHandle);
  AddResource(result);
end;

class function Storage.CreateImageHolder(aFormatCode: cardinal;
  aImageType: TImageType): TImageHolder;
begin
  result := TImageHolder.Create(aFormatCode,aImageType);
  result.Owner := FStorageHandle;
  AddResource(result);
end;

class function Storage.CreateImageHolder: TImageHolder;
begin
  result := CreateImageHolder.CreateOwned(FStorageHandle);
  AddResource(result);
end;

class function Storage.CreateLight: TLightSource;
begin
  result := TLightSource.CreateOwned(FStorageHandle);
  AddResource(result);
end;

class function Storage.CreateMaterial: TMaterial;
begin
  result := TMaterial.CreateOwned(FStorageHandle);
  AddResource(result);
end;

class function Storage.CreateMaterialObject: TMaterialObject;
begin
  result := TMaterialObject.CreateOwned(FStorageHandle);
  AddResource(result);
end;

class function Storage.CreateMesh(vo: TVertexObject): TMesh;
begin
  if assigned(vo) then begin
    result := TMesh.CreateFrom(vo);
    result.Owner := FStorageHandle;
  end else
    result := TMesh.CreateOwned(FStorageHandle);
  AddResource(result);
end;

class function Storage.CreateMeshObject(aMesh: TMeshAssembly): TMeshObject;
begin
  result := TMeshObject.CreateFrom(aMesh);
  result.Owner:=FStorageHandle;
  AddResource(result);
end;

class function Storage.CreateMeshObject(aMesh: TMesh): TMeshObject;
begin
  result := TMeshObject.CreateFrom(aMesh);
  result.Owner:=FStorageHandle;
  AddResource(result);
end;

class function Storage.CreateMeshObject: TMeshObject;
begin
  result := TMeshObject.CreateOwned(FStorageHandle);
  AddResource(result);
end;

class function Storage.CreateProgram: TShaderProgram;
begin
  result:=TShaderProgram.CreateOwned(FStorageHandle);
  AddResource(result);
end;

class function Storage.CreateSceneObject: TSceneObject;
begin
  result := TSceneObject.CreateOwned(FStorageHandle);
  AddResource(result);
end;

class function Storage.CreateTexture(aImageHolder: TImageHolder): TTexture;
begin
  result := TTexture.CreateFrom(aImageHolder, FStorageHandle);
  AddResource(result);
end;

class function Storage.CreateTextureSample: TTextureSampler;
begin
  result := TTextureSampler.CreateOwned(FStorageHandle);
  AddResource(result);
end;

class function Storage.CreateVertexObject: TVertexObject;
begin
  result := TVertexObject.CreateOwned(FStorageHandle);
  AddResource(result);
end;

class procedure Storage.Delete(Resource: TPersistentResource);
begin
  if Resource is TPersistentResource then begin
    FResources.Delete(Resource.GUID);
  end;
end;

class destructor Storage.Destroy;
begin
  FResources.FreeResources(FStorageHandle);
  FResources.Free;
  FResources := nil;
  FStorageHandle.Free;
end;

class function Storage.GetReference<T>(const aGUID: TGUID): IReference<T>;
begin
  result := TReference<T>(FResources.FindNode(aGUID).Value);
end;

class function Storage.GetResource(const aGUID: TGUID): TPersistentResource;
begin
  result := FResources.FindNode(aGUID).Value.Reference;
end;

class procedure Storage.PutResource(Resource: TPersistentResource);
begin
  if Resource is TPersistentResource then AddResource(Resource);
end;

class procedure Storage.RemoveReference(aResource: TPersistentResource);
begin
  GetReference<TPersistentResource>(aResource.GUID).Reference := nil;
end;

{ Storage.TStorageHandler }

procedure Storage.TStorageHandler.Notify(Sender: TObject; Msg: Cardinal;
  Params: pointer);
begin
  inherited;
  if assigned(Sender) then begin
    case Msg of
      NM_ObjectDestroyed:
        if Sender is TPersistentResource then begin
          Storage.Delete(TPersistentResource(Sender));
        end;
    end;
  end;
end;

{ TResourceList }

destructor TResourceList.Destroy;
begin
  inherited;
end;

procedure TResourceList.FreeResources(aOwner: TPersistentResource);
var
  x, y, z: TRBNode;
  cont: Boolean;
  temp: TReference<TPersistentResource>;
begin
  if Assigned(FLeftMost) then begin
    x := FLeftMost;
    repeat
      z := x;
      repeat
        z.Value:=nil;
        z := z.Twin;
      until z = nil;
      // Next node
      if (x.right <> nil) then begin
        x := x.right;
        while (x.left <> nil) do x := x.left;
      end
      else if (x.parent <> nil) then begin
        y := x.parent;
        while (x = y.right) do begin
          x := y;
          y := y.parent;
        end;
        if (x.right <> y) then x := y;
      end else
        x := FRoot;
    until x = FRightMost;
    if (FLeftMost <> FRightMost) and assigned(x) then begin
      if assigned(x.Value) then begin
        x.Value := nil;
      end;
    end;
  end;
end;

initialization

finalization

end.
