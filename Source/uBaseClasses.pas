 { TODO : Переписать функцию UpdateWorldMatrix
          на использование внешней процедуры из модуля рендеров;
        - зарегистрировать доступные рендеры;
   }

unit uBaseClasses;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, uPersistentClasses, uVMath, uBaseTypes, uLists, uMiscUtils;
Type

  TTransformsTypes = (ttPosition, ttScale, ttRotation, ttModel, ttParent, ttFollow, ttAll);
  TTransforms = set of TTransformsTypes;

  TBaseRenderResource = TPersistentResource;
  TRenderResourceClass = class of TBaseRenderResource;

  TBaseSceneItem = class;

  TSceneItemList = class (TObjectsDictionary)
  private
    function getItemObj(index: integer): TBaseSceneItem;
  public
    function AddSceneItem(const aItem: TBaseSceneItem): integer;
    function GetSceneItem(aKey: TGUID): TBaseSceneItem; overload;
    function GetSceneItem(aFriendlyName: string): TBaseSceneItem; overload;

    procedure RemoveSceneItem(const aItem: TBaseSceneItem);

    property SceneItems[index: integer]: TBaseSceneItem read getItemObj; default;
  end;


  { TODO : Решить что нужно вынести в базовый объект сцены }
  TBaseSceneItem = class (TPersistentResource)
  protected
    FChilds: TSceneItemList;
  public
    Active: boolean;
    FriendlyName: string;

    constructor Create; override;
    destructor Destroy; override;
    property Childs: TSceneItemList read FChilds;
  end;

 { TODO : Реализовать подписку объектов TMovableObject для получения уведомлений
          о изменении родительской матрицы.
 }

  TMovableObject = class (TBaseSceneItem)
  private
    //координатный базис
    FAbsolutePosition: TVector;
    FPosition: TVector; // глобальные координаты объекта
    FScale: TVector;    // масштаб объекта, совместно с положением - только для чтения
    FUp: TVector;       // OY
    FDirection: TVector;// OZ
    FLeft: TVector;
    procedure setModelMatrix(const Value: TMatrix);
    procedure setRotMatrix(const Value: TMatrix);
    procedure setScaleMatrix(const Value: TMatrix);
    procedure setTranslMatrix(const Value: TMatrix);
    procedure setWorldMatrix(const Value: TMatrix);
    function getDirection: TVector;
    function getLeftVector: TVector;
    function getUpVector: TVector;

  protected
    FParent: TMovableObject;

    FRollAngle: single;
    FTurnAngle: single;
    FPitchAngle: single;
    FXRotationAngle: single;
    FYRotationAngle: single;
    FZRotationAngle: single;

    //Матрицы трансформации
    FModelMatrix: TMatrix; //модельная матрица, хранит базовые трансформации объекта
    FScaleMatrix: TMatrix; //масштабная матрица
    FRotationMatrix: TMatrix; //матрица поворота
    FTranslationMatrix: TMatrix; //матрица переноса
    FWorldMatrix: TMatrix; //результирующая мировая матрица
    FWorldMatrixT: TMatrix; //транспонированная мировая матрица
    FInvWorldMatrix: TMatrix;//обратная мировая матрица
    FParentMatrix: TMatrix;

    function getParent: TMovableObject;
    procedure SetParent(const Value: TMovableObject);
    procedure SetPosition(const Value: TVector);
    procedure SetScale(const Value: TVector);
    //Ориентирует объект в заданном направлении
    procedure SetDirection(const aDirection: TVector);

  public
    FriendlyName: string; //храните любой текст или комментарии тут
    Tag: integer; //для нужд пользователя
    DirectingAxis: TVector; //Хранит направляющую ось Axis

    WorldMatrixUpdated: boolean; //false=требуется перестроить мировую матрицу

    Constructor Create; override;
    Destructor Destroy; override;

    //установка родителя, из которого будет браться базовая матрица трансформаций
    Property Parent: TMovableObject read getParent write SetParent;
    //Прямая установка родительской матрицы
    Property ParentMatrix: TMatrix read FParentMatrix write FParentMatrix;

    //модельная матрица, хранит локальные/базовые трансформации объекта
    Property ModelMatrix: TMatrix read FModelMatrix write setModelMatrix;
    //масштабная матрица
    Property ScaleMatrix: TMatrix read FScaleMatrix write setScaleMatrix;
    //матрица поворота
    Property RotationMatrix: TMatrix read FRotationMatrix write setRotMatrix;
    //матрица переноса
    Property TranslationMatrix: TMatrix read FTranslationMatrix write setTranslMatrix;
    //результирующая мировая матрица
    Property WorldMatrix: TMatrix read FWorldMatrix write setWorldMatrix;
    //транспонированная мировая матрица
    Property WorldMatrixT: TMatrix read FWorldMatrixT;
    //обратная мировая матрица
    Property InvWorldMatrix: TMatrix read FInvWorldMatrix;


    //Установка/чтение локального положения
    Property Position: TVector read FPosition write SetPosition;
    //Чтение абсолютного положения
    Property AbsolutePosition: TVector read FAbsolutePosition;
    //Установка/чтение масштаба объекта
    Property Scale: TVector read FScale write SetScale;
    //Угол поворота в плоскости экрана
    Property RollAngle: single read FRollAngle write FRollAngle;
    //Установка/чтение ориентации объекта
    Property Direction: TVector read getDirection write SetDirection;
    Property Left: TVector read getLeftVector;
    Property Up: TVector read getUpVector;

    //Вращение относительно локальных осей
    Procedure TurnObject(Angle:single);  //Вокруг локальной оси Y
    Procedure RollObject(Angle:single);  //Вокруг локальной оси Z
    Procedure PitchObject(Angle:single); //Вокруг локальной оси X
    //Передвигает объект вдоль оси Direction
    Procedure MoveForward(Step:single);
    //Передвигает объект вдоль оси Left
    Procedure MoveLeft(Step:single);
    //Передвигает объект вдоль оси Up
    Procedure MoveUp(Step:single);
    //формирует матрицу поворота, при AbsoluteRotation=false модифицируется существующая
    Procedure RotateObject(const Axis: TVector; Angle: single; AbsoluteRotation: boolean=true);
    Procedure RotateAround(const Pivot, Axis: TVector; Angle: single);
    Procedure RotateAroundX(Angle: single; AbsoluteRotation: boolean=true);
    Procedure RotateAroundY(Angle: single; AbsoluteRotation: boolean=true);
    Procedure RotateAroundZ(Angle: single; AbsoluteRotation: boolean=true);
    //Накопленные углы при абсолютном повороте
    property XRotationAngle: single read FXRotationAngle;
    property YRotationAngle: single read FYRotationAngle;
    property ZRotationAngle: single read FZRotationAngle;

    //формирует матрицу масштабирования, при AbsoluteScale=false модифицируется существующая
    Procedure ScaleObject(Scale: TVector; AbsoluteScale: boolean=true);overload;
    Procedure ScaleObject(ScaleX,ScaleY,ScaleZ: single; AbsoluteScale: boolean=true);overload;
    //формирует матрицу переноса, при AbsolutePos=false модифицируется существующая
    Procedure MoveObject(Pos: TVector; AbsolutePos: boolean=true);overload;
    Procedure MoveObject(x,y,z: single; AbsolutePos: boolean=true);overload;
    //перестраивается мировая матрица
    Procedure UpdateWorldMatrix(UseMatrix: TTransforms=[ttAll]);virtual;
    //Заменяет все матрицы трансформаций на единичные
    Procedure ResetMatrices;
    //Заменяет модельную матрицу текущей мировой матрицей
    Procedure StoreTransforms(ToStore: TTransforms);
    //Переводит точку из глобальной системы координат в систему координат объекта
    Function AbsoluteToLocal(P: TVector):TVector;
    //Переводит вектор из глобальной системы координат в локальную
    Function VectorToLocal(V: TVector; Norm: boolean=true): TVector;
    //Переводит точку из локальной системы координат в глобальную
    Function LocalToAbsolute(P: TVector): TVector;
  end;

  { TODO :
      Добавить к RebuildTriangles работу с ftTriangleFan, ftTriangleStripAdjacency,
        ftTriangleAdjacency и ftPatches;
  }
  //Список треугольников для расчета колизии, рейкаста и прочих целей.
  TTriangleList = class (TPersistentResource)
  private
    FVertices: TVec3List;
    FIndices: TIntegerList;
    FTriangles: TVec3List;
    FExtents: TExtents;
  public
    constructor Create; override;
    constructor CreateFrom(const aVertices: TVec3List; aIndices: TIntegerList);
    destructor Destroy; override;

    procedure RebuildTriangles(aSourceFaceType: TFaceType = ftTriangles);
    procedure Clear(aTriangles: boolean = true; aVertices: boolean = true;
      aIndices: boolean = true);
    function UpdateExtents: TExtents;

    property Vertices: TVec3List read FVertices;
    property Indices: TIntegerList read FIndices;
    property Triangles: TVec3List read FTriangles;
    property Extents: TExtents read FExtents;

  end;

implementation

{ TMovableObject }

function TMovableObject.AbsoluteToLocal(P: TVector): TVector;
begin
    if not WorldMatrixUpdated then UpdateWorldMatrix;p[3]:=1;
    Result:=P*FInvWorldMatrix.Matrix4;
end;

function TMovableObject.VectorToLocal(V: TVector; Norm: boolean=true): TVector;
var rv: TVector;
begin
    if not WorldMatrixUpdated then UpdateWorldMatrix;
    rv := V.Affine * FInvWorldMatrix.Matrix4;
    if Norm then Result:=rv.Normalize else Result:=rv;
end;

constructor TMovableObject.Create;
begin
  inherited Create;
  FModelMatrix.SetIdentity;
  FScaleMatrix.SetIdentity;
  FRotationMatrix.SetIdentity;
  FTranslationMatrix.SetIdentity;
  FWorldMatrix.SetIdentity;
  FWorldMatrixT.SetIdentity;
  FInvWorldMatrix.SetIdentity;
  FParentMatrix.SetIdentity;

  FRollAngle:=0;
  FTurnAngle:=0;
  FPitchAngle:=0;
  FXRotationAngle:=0;
  FYRotationAngle:=0;
  FZRotationAngle:=0;

  FPosition:= vtW;
  FScale:= 1;
  Parent:=nil;
  UpdateWorldMatrix;
end;

destructor TMovableObject.Destroy;
begin
  inherited;
end;

function TMovableObject.getDirection: TVector;
begin
  result:=FWorldMatrix.Row[2];
end;

function TMovableObject.getLeftVector: TVector;
begin
  result:=FWorldMatrix.Row[0];
end;

function TMovableObject.getParent: TMovableObject;
begin
  result:=FParent as TMovableObject;
end;

function TMovableObject.getUpVector: TVector;
begin
  result:=FWorldMatrix.Row[1];
end;

procedure TMovableObject.MoveObject(Pos: TVector; AbsolutePos:boolean=true);
var mt: TMatrix;
begin
  mt:=TMatrix.TranslationMatrix( Pos );
  if AbsolutePos then FTranslationMatrix:=mt
  else FTranslationMatrix:=TranslationMatrix*mt;

  UpdateWorldMatrix;
end;

procedure TMovableObject.ResetMatrices;
begin
  FModelMatrix.SetIdentity;
  FScaleMatrix.SetIdentity;
  FRotationMatrix.SetIdentity;
  FTranslationMatrix.SetIdentity;
  FWorldMatrix.SetIdentity;

  UpdateWorldMatrix;
end;

procedure TMovableObject.RotateObject(const Axis: TVector; Angle: single;
  AbsoluteRotation: boolean);
var mr: TMatrix;
begin
  mr:=TMatrix.RotationMatrix( Axis.Affine, Angle );
  if AbsoluteRotation then RotationMatrix:=mr
  else RotationMatrix:=RotationMatrix*mr;
  UpdateWorldMatrix;
end;

procedure TMovableObject.ScaleObject(Scale: TVector; AbsoluteScale:boolean=true);
var ms: TMatrix;
begin
  ms:=TMatrix.ScaleMatrix( Scale );
  if AbsoluteScale then begin
     FScaleMatrix:=ms;
     FScale:=Scale;
  end else begin
     FScale:=Scale*FScaleMatrix.Matrix4;
     FScaleMatrix:=FScaleMatrix*ms;
  end;
  UpdateWorldMatrix;
end;


procedure TMovableObject.UpdateWorldMatrix;
var wm: TMatrix;
begin
{ //Код перенесен в процедуру UpdateWorldMatrix базового рендера
  if (FParent<>nil) and ((ttParent in UseMatrix) or (ttAll in UseMatrix)) then begin
   if not Parent.WorldMatrixUpdated then parent.UpdateWorldMatrix;
   FParentMatrix:=parent.WorldMatrix;
  end;

  wm.SetIdentity;
  if (FParent<>nil) and ((ttParent in UseMatrix) or (ttAll in UseMatrix)) then begin
     if not Parent.WorldMatrixUpdated then parent.UpdateWorldMatrix;
     wm:=parent.WorldMatrix; wm:=wm * FModelMatrix;
  end else wm := FModelMatrix;

  if (not (ttModel in UseMatrix)) and (not(ttAll in UseMatrix)) then wm.SetIdentity;

  if (ttScale in UseMatrix) or (ttAll in UseMatrix) then wm := wm * ScaleMatrix;
  if (ttRotation in UseMatrix) or (ttAll in UseMatrix) then wm := wm * RotationMatrix;
  if (ttPosition in UseMatrix) or (ttAll in UseMatrix) then wm := wm * TranslationMatrix;

  wm:=wm * FParentMatrix;

  FWorldMatrix:=wm;
}

  FAbsolutePosition:=FWorldMatrix.Row[3];
  FPosition:=FTranslationMatrix.Row[3];
  FWorldMatrixT:=wm.Transpose;
  FInvWorldMatrix:=FWorldMatrix.Invert;
  DirectingAxis:=Vector(WorldMatrix[0,0],WorldMatrix[1,1],WorldMatrix[2,2],0);
  DirectingAxis.SetNormalize;
  WorldMatrixUpdated:=true;

end;

procedure TMovableObject.PitchObject(Angle: single);
begin
  //вокруг оси X в YZ
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  FRotationMatrix.Pitch(Angle);
  UpdateWorldMatrix;
  FPitchAngle:=FPitchAngle+Angle;
end;

procedure TMovableObject.RollObject(Angle: single);
begin
  //вокруг оси Z в XY
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  FRotationMatrix.Roll(Angle);
  UpdateWorldMatrix;
  FRollAngle:=FRollAngle+Angle;
end;

procedure TMovableObject.TurnObject(Angle: single);
begin
  //вокруг оси Y в XZ
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  FRotationMatrix.Turn(Angle);
  UpdateWorldMatrix;
  FTurnAngle:=FTurnAngle+Angle;
end;

procedure TMovableObject.StoreTransforms(ToStore: TTransforms);
var wm,mm: TMatrix;
begin
    if ttModel in toStore then wm := FModelMatrix else wm.SetIdentity;
    if ttScale in toStore then begin wm := wm*FScaleMatrix; FScaleMatrix.SetIdentity; end;
    if ttRotation in toStore then begin wm := wm*FRotationMatrix; FRotationMatrix.SetIdentity; end;
    if ttposition in toStore then begin wm := wm*FTranslationMatrix; FTranslationMatrix.SetIdentity; end;
    mm:=FModelMatrix; ResetMatrices;
    FModelMatrix:=mm*wm;
    UpdateWorldMatrix;
end;

procedure TMovableObject.RotateAroundX(Angle: single;
  AbsoluteRotation: boolean);
var rm: TMatrix;
begin
  //вокруг глобальной оси X
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  if AbsoluteRotation then begin
     FRotationMatrix:=TMatrix.RotationMatrix( TVector( vtX ), Angle);
  end else begin
     FXRotationAngle:=FXRotationAngle+Angle;
     rm:=TMatrix.RotationMatrix( TVector( vtX ), Angle);
     FRotationMatrix:=FRotationMatrix * rm;
  end;
  UpdateWorldMatrix;
end;

procedure TMovableObject.RotateAroundY(Angle: single;
  AbsoluteRotation: boolean);
var rm: TMatrix;
begin
  //вокруг глобальной оси Y
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  if AbsoluteRotation then begin
     FRotationMatrix:=TMatrix.RotationMatrix( TVector( vtY ),Angle);
  end else begin
     FYRotationAngle:=FYRotationAngle+Angle;
     rm:=TMatrix.RotationMatrix(TVector( vtY ), Angle);
     FRotationMatrix:=FRotationMatrix*rm;
  end;
  UpdateWorldMatrix;
end;

procedure TMovableObject.RotateAroundZ(Angle: single;
  AbsoluteRotation: boolean);
var rm: TMatrix;
begin
  //вокруг глобальной оси Z
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  if AbsoluteRotation then begin
     FRotationMatrix:=TMatrix.RotationMatrix(TVector( vtZ ), Angle);
  end else begin
     FZRotationAngle:=FZRotationAngle+Angle;
     rm:=TMatrix.RotationMatrix(TVector( vtZ ), Angle);
     FRotationMatrix:=FRotationMatrix * rm;
  end;
  UpdateWorldMatrix;
end;

procedure TMovableObject.RotateAround(const Pivot, Axis: TVector; Angle: single);
var np: TVector;
    mr,mp,mnp,m: TMatrix;
begin
  mr:=TMatrix.RotationMatrix(Axis, Angle);

  np:=-Pivot; np[3]:=1;
  mp:=TMatrix.TranslationMatrix(Pivot);
  mnp:=TMatrix.TranslationMatrix(np);

  //Поворот вокруг заданной точки
  m:=FModelMatrix*mp; m:=m*mr; m:=m*mnp;
  FModelMatrix:=m;
  UpdateWorldMatrix;
end;

procedure TMovableObject.SetParent(const Value: TMovableObject);
begin
  FParent:=Value;
end;

procedure TMovableObject.SetPosition(const Value: TVector);
begin
  MoveObject(Value);
end;

procedure TMovableObject.setRotMatrix(const Value: TMatrix);
begin
  FRotationMatrix := Value;
end;

procedure TMovableObject.SetScale(const Value: TVector);
begin
  ScaleObject(Value);
end;

procedure TMovableObject.setScaleMatrix(const Value: TMatrix);
begin
  FScaleMatrix := Value;
end;

procedure TMovableObject.setTranslMatrix(const Value: TMatrix);
begin
  FTranslationMatrix := Value;
end;

procedure TMovableObject.setWorldMatrix(const Value: TMatrix);
begin
  FWorldMatrix := Value;
end;

procedure TMovableObject.MoveForward(Step: single);
begin
  FTranslationMatrix[3,0]:=FTranslationMatrix[3,0]+FDirection[0]*Step;
  FTranslationMatrix[3,1]:=FTranslationMatrix[3,1]+FDirection[1]*Step;
  FTranslationMatrix[3,2]:=FTranslationMatrix[3,2]+FDirection[2]*Step;
  UpdateWorldMatrix;
end;

procedure TMovableObject.MoveLeft(Step: single);
begin
  FTranslationMatrix[3,0]:=FTranslationMatrix[3,0]+FLeft[0]*Step;
  FTranslationMatrix[3,1]:=FTranslationMatrix[3,1]+FLeft[1]*Step;
  FTranslationMatrix[3,2]:=FTranslationMatrix[3,2]+FLeft[2]*Step;
  UpdateWorldMatrix;
end;

procedure TMovableObject.MoveUp(Step: single);
begin
  FTranslationMatrix[3,0]:=FTranslationMatrix[3,0]+FUp[0]*Step;
  FTranslationMatrix[3,1]:=FTranslationMatrix[3,1]+FUp[1]*Step;
  FTranslationMatrix[3,2]:=FTranslationMatrix[3,2]+FUp[2]*Step;
  UpdateWorldMatrix;
end;

procedure TMovableObject.MoveObject(x, y, z: single; AbsolutePos: boolean);
begin
   MoveObject(Vector(x,y,z,1),AbsolutePos);
end;

procedure TMovableObject.ScaleObject(ScaleX, ScaleY, ScaleZ: single;
  AbsoluteScale: boolean);
begin
  ScaleObject(Vector(ScaleX, ScaleY, ScaleZ, 0),AbsoluteScale);
end;

procedure TMovableObject.SetDirection(const aDirection: TVector);
var up,left,right,dir,t: TVector;
begin
  up:=FModelMatrix.Row[1]; up.SetNormalize;
  dir:=aDirection.Normalize;
  right:=Dir.Cross(Up);
  if right.Length<1e-5  then begin
     t:= vtZ;
     right:=t.Cross(Up);
     if right.Length<1e-5 then begin
        t:= vtX;
        right:=t.Cross(Up);
     end;
  end;
  right.SetNormalize;
  Up:=right.Cross(Dir); Up.SetNormalize;
  Left:=Up.Cross(Dir); Left.SetNormalize;
  FModelMatrix.Row[0]:=Left.Vec4;
  FModelMatrix.Row[1]:=Up.Vec4;
  FModelMatrix.Row[2]:=Dir.Vec4;
  FRotationMatrix.SetIdentity;
  UpdateWorldMatrix;
end;

procedure TMovableObject.setModelMatrix(const Value: TMatrix);
begin
  FModelMatrix := Value;
end;

function TMovableObject.LocalToAbsolute(P: TVector): TVector;
begin
  if not WorldMatrixUpdated then UpdateWorldMatrix;
  Result:=P*FWorldMatrix.Matrix4;
end;

{ TTriangleList }

constructor TTriangleList.Create;
begin
  inherited;
  FVertices:=TVec3List.Create;
  FIndices:=TIntegerList.Create;
  FTriangles:=TVec3List.Create;
  FExtents.Empty:=true;
end;

procedure TTriangleList.Clear(aTriangles, aVertices, aIndices: boolean);
begin
  if aTriangles then begin FTriangles.Clear; FTriangles.Capacity:=4; end;
  if aVertices then begin FVertices.Clear; FVertices.Capacity:=4; end;
  if aIndices then begin FIndices.Clear; FIndices.Capacity:=4; end;
end;

constructor TTriangleList.CreateFrom(const aVertices: TVec3List;
  aIndices: TIntegerList);
begin
  Create;
  if assigned(aVertices) then begin
    FVertices.Count:=aVertices.Count;
    Move(aVertices.Data^,FVertices.Data^,aVertices.Size);
  end;

  if assigned(aIndices) then begin
    FIndices.Count:=aIndices.Count;
    Move(aIndices.Data^,FIndices.Data^,aIndices.Size);
  end;

end;

destructor TTriangleList.Destroy;
begin
  FVertices.Free; FIndices.Free; FTriangles.Free;
  inherited;
end;

procedure TTriangleList.RebuildTriangles(aSourceFaceType: TFaceType);
var i: integer;
    v,v1,v2,v3: vec3;
begin
  FTriangles.Clear;
  if FIndices.Count>0 then begin
    case aSourceFaceType of
      ftTriangles, ftPoints: begin
        FTriangles.Count:=FIndices.Count;
        for i:=0 to FIndices.Count-1 do
          FTriangles[i]:=FVertices[FIndices[i]];
      end;

      ftTriangleStrip: begin
        FTriangles.Capacity:=FIndices.Count;
        v1 := FVertices[FIndices[0]]; v2 := FVertices[FIndices[1]];
        for i := 2 to FIndices.Count - 1 do begin
          v := FVertices[FIndices[i]];
          if odd(i) then begin
            FTriangles.Add(v2); FTriangles.Add(v1); FTriangles.Add(v);
          end else begin
            FTriangles.Add(v1); FTriangles.Add(v2); FTriangles.Add(v);
          end;
          v1 := v2; v2 := v;
        end;
      end;

      ftQuads: begin
        FTriangles.Capacity:=(FIndices.Count div 4)*2;
        for i := 0 to (FIndices.Count div 4) - 1 do begin
          v  := FVertices[FIndices[i*4]];
          v1 := FVertices[FIndices[i*4+1]];
          v2 := FVertices[FIndices[i*4+2]];
          v3 := FVertices[FIndices[i*4+3]];
          FTriangles.Add(v);  FTriangles.Add(v1);
          FTriangles.Add(v2); FTriangles.Add(v2);
          FTriangles.Add(v3); FTriangles.Add(v);
        end;
      end;

      Else assert(false, 'Unsupported face type!');
    end;
  end else begin
    case aSourceFaceType of
      ftTriangles, ftPoints: begin
        FTriangles.Count:=FVertices.Count;
        Move(FVertices.Data^,FTriangles.Data^,FVertices.Size);
      end;

      ftTriangleStrip: begin
        FTriangles.Capacity:=FVertices.Count;
        v1 := FVertices[0]; v2 := FVertices[1];
        for i := 2 to FVertices.Count - 1 do begin
          v := FVertices[i];
          if odd(i) then begin
            FTriangles.Add(v2); FTriangles.Add(v1); FTriangles.Add(v);
          end else begin
            FTriangles.Add(v1); FTriangles.Add(v2); FTriangles.Add(v);
          end;
          v1 := v2; v2 := v;
        end;
      end;

      ftQuads: begin
        FTriangles.Capacity:=(FVertices.Count div 4)*2;
        for i := 0 to (FVertices.Count div 4) - 1 do begin
          v  := FVertices[i*4];   v1 := FVertices[i*4+1];
          v2 := FVertices[i*4+2]; v3 := FVertices[i*4+3];
          FTriangles.Add(v);  FTriangles.Add(v1);
          FTriangles.Add(v2); FTriangles.Add(v2);
          FTriangles.Add(v3); FTriangles.Add(v);
        end;
      end;

      Else assert(false, 'Unsupported face type!');
    end;
  end;

end;

function TTriangleList.UpdateExtents: TExtents;
var i: integer;
begin
  FExtents.Empty:=true;
  for i:=0 to FVertices.Count-1 do FExtents.Include(FVertices[i]);
  result:=FExtents;
end;

{ TBaseSceneItem }

constructor TBaseSceneItem.Create;
begin
  inherited;
  FChilds:=TSceneItemList.Create;
end;

destructor TBaseSceneItem.Destroy;
begin
  FChilds.Free;
  inherited;
end;

{ TSceneItemList }

function TSceneItemList.AddSceneItem(
  const aItem: TBaseSceneItem): integer;
begin
  result:=AddKey(aItem.GUID, aItem);
end;

function TSceneItemList.getItemObj(index: integer): TBaseSceneItem;
begin
  result:=TBaseSceneItem(FItems[Index].Value);
end;

function TSceneItemList.GetSceneItem(aFriendlyName: string): TBaseSceneItem;
var i: integer;
    si: TBaseSceneItem;
begin
  for i:=0 to FCount-1 do begin
    si:=TBaseSceneItem(FItems[i].Value);
    if si.FriendlyName=aFriendlyName then begin
      result:=si; exit;
    end;
  end; result:=nil;
end;

function TSceneItemList.GetSceneItem(aKey: TGUID): TBaseSceneItem;
begin
  result:=TBaseSceneItem(GetValue(aKey));
end;

procedure TSceneItemList.RemoveSceneItem(const aItem: TBaseSceneItem);
var i: integer;
    si: TBaseSceneItem;
begin
  for i:=0 to FCount-1 do begin
    si:=TBaseSceneItem(FItems[i].Value);
    if si=aItem then begin
      FItems[i].Key:=-1; FItems[i].KeyName:='';
      FItems[i].KeyGUID := TGUIDEx.Empty;
      FItems[i].Value:=nil; exit;
    end;
  end;
end;

end.
