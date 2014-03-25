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

type

  TTransformsTypes = (ttPosition, ttScale, ttRotation, ttModel, ttPivot, ttFollow);
  TTransforms = set of TTransformsTypes;

const
  ALL_TRANSFORM = [ttPosition, ttScale, ttRotation, ttModel, ttPivot, ttFollow];

type

  TBaseRenderResource = TPersistentResource;
  TRenderResourceClass = class of TBaseRenderResource;

  TBaseSceneItem = class;

  TSceneItemListChanges = (chAdd, chRemove);
  TSceneItemListOnChange = procedure(anItem: TBaseSceneItem; aChnages: TSceneItemListChanges) of object;

  TSceneItemList = class (TObjectsDictionary)
  private
    FOnChange: TSceneItemListOnChange;
    function getItemObj(index: integer): TBaseSceneItem;
  public
    destructor Destroy; override;

    function AddSceneItem(const aItem: TBaseSceneItem): integer;
    function GetSceneItem(aKey: TGUID): TBaseSceneItem; overload;
    function GetSceneItem(aFriendlyName: string): TBaseSceneItem; overload;

    procedure RemoveSceneItem(const aItem: TBaseSceneItem);

    property SceneItems[index: integer]: TBaseSceneItem read getItemObj; default;
    property OnChange: TSceneItemListOnChange read FOnChange write FOnChange;
  end;


  { TODO : Решить что нужно вынести в базовый объект сцены }
  TBaseSceneItem = class (TPersistentResource)
  protected
    FParent: TBaseSceneItem;
    FChilds: TSceneItemList;
    FNestingDepth: integer;
    procedure OnItemsChanged(anItem: TBaseSceneItem; aChnages: TSceneItemListChanges);
    procedure SetParent(const Value: TBaseSceneItem); virtual;
  public
    Active: boolean;
    FriendlyName: string;

    constructor Create; override;
    destructor Destroy; override;

    procedure RecalcNestingDepth;

    property Parent: TBaseSceneItem read FParent write SetParent;
    property Childs: TSceneItemList read FChilds;
    property NestingDepth: integer read FNestingDepth;
  end;

  TDirectionBehavior = (dbNone, dbSphericalSprite, dbCylindricalSprite);

  TMovableObjectClass = class of TMovableObject;

  TMovableObject = class (TBaseSceneItem)
  private
    FDirBehavior: TDirectionBehavior;
    FWorldMatrixUpdated: Boolean;
    procedure SetModelMatrix(const Value: TMatrix);
    procedure SetRotMatrix(const Value: TMatrix);
    procedure SetScaleMatrix(const Value: TMatrix);
    procedure SetTranslMatrix(const Value: TMatrix);
    function GetDirection: TVector;
    function GetLeftVector: TVector;
    function GetUpVector: TVector;
    procedure SetDirBehavior(const Value: TDirectionBehavior);
    function GetPivot: TMovableObject;
    procedure SetPivot(const Value: TMovableObject);
    procedure SetPitchAngle(const Value: single);
    procedure SetRollAngle(const Value: single);
    procedure SetTurnAngle(const Value: single);
    function GetWorldMatrix: TMatrix;
    function GetAbsoluteDirection: TVector;
    function GetAbsoluteLeft: TVector;
    function GetAbsolutePosition: TVector;
    function GetAbsoluteUp: TVector;
    function GetInvWorldMatrix: TMatrix;
    function GetPivotMatrix: TMatrix;
    function GetPosition: TVector;
    function GetScale: TVector;
    function GetWorldMatrixT: TMatrix;
    procedure SetXRotationAngle(const Value: single);
    procedure SetYRotationAngle(const Value: single);
    procedure SetZRotationAngle(const Value: single);
    function GetInvPivotMatrix: TMatrix;
    procedure SetAbsoluteDirection(const Value: TVector);
    procedure SetAbsoluteLeft(const Value: TVector);
    procedure SetAbsolutePosition(const Value: TVector);
    procedure SetAbsoluteUp(const Value: TVector);
    procedure SetLeftVector(const Value: TVector);
    procedure SetUpVector(const Value: TVector);
    procedure SetDirection(const aDirection: TVector);
    function GetNormalMatrix: TMatrix;
  protected
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
    FNormalMatrix: TMatrix;
    FPivotMatrix: TMatrix; //мировая матрица для дочерних объектов (без учета модельной)
    FInvPivotMatrix: TMatrix;
    FWorldMatrixUpdateCounter: Integer;

    procedure SetPosition(const Value: TVector);
    procedure SetScale(const Value: TVector);
    procedure NotifyWorldMatrixChanged; virtual;
    procedure SetParent(const Value: TBaseSceneItem); override;
  public
    Tag: integer; //для нужд пользователя
    DirectingAxis: TVector; //Хранит направляющую ось Axis

    constructor Create; override;
    destructor Destroy; override;
    function MakeClone(aWithChildren: Boolean): TMovableObject; virtual;

    procedure Notify(Sender: TObject; Msg: Cardinal; Params: pointer = nil); override;

    // Parent as TMovableObject
    property Pivot: TMovableObject read GetPivot write SetPivot;
    // Мировая матрица для дочерних объектов
    property PivotMatrix: TMatrix read GetPivotMatrix;
    property InvPivotMatrix: TMatrix read GetInvPivotMatrix;

    //модельная матрица, хранит локальные/базовые трансформации объекта
    property ModelMatrix: TMatrix read FModelMatrix write SetModelMatrix;
    //масштабная матрица
    property ScaleMatrix: TMatrix read FScaleMatrix write SetScaleMatrix;
    //матрица поворота
    property RotationMatrix: TMatrix read FRotationMatrix write setRotMatrix;
    //матрица переноса
    property TranslationMatrix: TMatrix read FTranslationMatrix write setTranslMatrix;
    //результирующая мировая матрица
    property WorldMatrix: TMatrix read GetWorldMatrix;
    //нормированая матрица
    property NormalMatrix: TMatrix read GetNormalMatrix;
    //транспонированная мировая матрица
    property WorldMatrixT: TMatrix read GetWorldMatrixT;
    //обратная мировая матрица
    property InvWorldMatrix: TMatrix read GetInvWorldMatrix;
    //счеткик изменений мировой матрицы
    property WorldMatrixUpdateCounter: Integer read FWorldMatrixUpdateCounter;
    //Чтение абсолютного положения
    property AbsolutePosition: TVector read GetAbsolutePosition write SetAbsolutePosition;
    property AbsoluteDirection: TVector read GetAbsoluteDirection write SetAbsoluteDirection;
    property AbsoluteLeft: TVector read GetAbsoluteLeft write SetAbsoluteLeft;
    property AbsoluteUp: TVector read GetAbsoluteUp write SetAbsoluteUp;

    //Установка/чтение локального положения
    property Position: TVector read GetPosition write SetPosition;
    //Установка/чтение масштаба объекта
    property Scale: TVector read GetScale write SetScale;
    //Угол поворота в плоскости экрана
    property RollAngle: single read FRollAngle write SetRollAngle;
    property TurnAngle: single read FTurnAngle write SetTurnAngle;
    property PitchAngle: single read FPitchAngle write SetPitchAngle;
    //Установка/чтение ориентации объекта
    property Direction: TVector read GetDirection write SetDirection;
    property Left: TVector read GetLeftVector write SetLeftVector;
    property Up: TVector read GetUpVector write SetUpVector;
    property DirectionBehavior: TDirectionBehavior read FDirBehavior write SetDirBehavior;

    //Вращение относительно локальных осей
    procedure TurnObject(Angle:single);  //Вокруг локальной оси Y
    procedure RollObject(Angle:single);  //Вокруг локальной оси Z
    procedure PitchObject(Angle:single); //Вокруг локальной оси X
    //Передвигает объект вдоль оси Direction
    procedure MoveForward(Step:single);
    //Передвигает объект вдоль оси Left
    procedure MoveLeft(Step:single);
    //Передвигает объект вдоль оси Up
    procedure MoveUp(Step:single);
    //формирует матрицу поворота, при AbsoluteRotation=false модифицируется существующая
    procedure RotateObject(const Axis: TVector; Angle: single; AbsoluteRotation: boolean=true);
    procedure RotateAround(const aPivot, Axis: TVector; Angle: single); overload;
    procedure RotateAround(const aPivot, anUp: TVector; aPitchDelta, aTurnDelta: single); overload;
    procedure RotateAroundX(Angle: Single);
    procedure RotateAroundY(Angle: Single);
    procedure RotateAroundZ(Angle: Single);
    //Накопленные углы при абсолютном повороте
    property XRotationAngle: Single read FXRotationAngle write SetXRotationAngle;
    property YRotationAngle: Single read FYRotationAngle write SetYRotationAngle;
    property ZRotationAngle: Single read FZRotationAngle write SetZRotationAngle;

    //формирует матрицу масштабирования
    procedure RescaleObject(const aNewScale: TVector); overload;
    procedure RescaleObject(aNewScaleX, aNewScaleY, aNewScaleZ: Single); overload;
    procedure ScaleObject(const aScale: TVector); overload;
    procedure ScaleObject(aScaleX, aScaleY, aScaleZ: Single); overload;
    //формирует матрицу переноса
    procedure MoveObject(const aPosition: TVector); overload;
    procedure MoveObject(x, y, z: Single); overload;
    procedure ShiftObject(const aPosition: TVector); overload;
    procedure ShiftObject(x, y, z: Single); overload;
    //перестраивается мировая матрица
    procedure UpdateWorldMatrix(UseMatrix: TTransforms=ALL_TRANSFORM); overload; virtual;
    //прямая установка матриц (после внешнего расчета)
    procedure UpdateWorldMatrix(const aWorld, aNormalWorld, anInvWorld, aWorldT, aPivot, anInvPivot: TMatrix); overload;
    //Заменяет все матрицы трансформаций на единичные
    procedure ResetMatrices;
    //Заменяет модельную матрицу текущей мировой матрицей
    procedure StoreTransforms(const aTransforms: TTransforms);
    //Переводит точку из глобальной системы координат в систему координат объекта
    function AbsoluteToLocal(const aPosition: TVector):TVector;
    //Переводит вектор из глобальной системы координат в локальную
    function VectorToLocal(aVector: TVector; Norm: boolean=true): TVector;
    //Переводит точку из локальной системы координат в глобальную
    function LocalToAbsolute(const aPosition: TVector): TVector;
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

function ResourceComparer(const Item1, Item2: TBaseRenderResource): Integer;

implementation

uses
  Math, uMath;

function ResourceComparer(const Item1, Item2: TBaseRenderResource): Integer;
begin
  if Item1.Order < Item2.Order then
    exit(-1)
  else if (Item1.Order = Item2.Order) then
    exit(0)
  else
    Result := 1;
end;

{ TMovableObject }

function TMovableObject.AbsoluteToLocal(const aPosition: TVector): TVector;
begin
  Result := InvPivotMatrix.Transform(aPosition);
end;

function TMovableObject.VectorToLocal(aVector: TVector; Norm: boolean=true): TVector;
var rv: TVector;
begin
  aVector.MakeAffine;
  Result := InvPivotMatrix.Transform(aVector);
  if Norm then Result.SetNormalize;
end;

procedure TMovableObject.NotifyWorldMatrixChanged;
begin
  FWorldMatrixUpdated := false;
  DispatchMessage(NM_WorldMatrixChanged);
end;

function TMovableObject.MakeClone(aWithChildren: Boolean): TMovableObject;
var
  ctype: TMovableObjectClass;
  clone, childclone: TMovableObject;
  I: Integer;
begin
  ctype := TMovableObjectClass(ClassType);
  clone := TMovableObject(ctype.Create);
  clone.FDirBehavior := FDirBehavior;
  clone.FRollAngle := FRollAngle;
  clone.FTurnAngle := FTurnAngle;
  clone.FPitchAngle := FPitchAngle;
  clone.FXRotationAngle := FXRotationAngle;
  clone.FYRotationAngle := FYRotationAngle;
  clone.FZRotationAngle := FZRotationAngle;
  clone.FModelMatrix := FModelMatrix;
  clone.FScaleMatrix := FScaleMatrix;
  clone.FRotationMatrix := FRotationMatrix;
  clone.FTranslationMatrix := FTranslationMatrix;
  clone.Parent := Parent;
  clone.FriendlyName := FriendlyName + '@';
  Result := clone;
  if aWithChildren then
    for I := 0 to Childs.Count - 1 do if Assigned(Childs[i]) then begin
      childclone := TMovableObject(Childs[i]).MakeClone(aWithChildren);
      clone.Childs.AddSceneItem(childclone);
    end;
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
  FPivotMatrix.SetIdentity;

  FRollAngle:=0;
  FTurnAngle:=0;
  FPitchAngle:=0;
  FXRotationAngle:=0;
  FYRotationAngle:=0;
  FZRotationAngle:=0;

  FWorldMatrixUpdated := false;
end;

destructor TMovableObject.Destroy;
begin
  if assigned(FParent) then FParent.UnSubscribe(self);
  inherited;
end;

function TMovableObject.GetAbsoluteDirection: TVector;
begin
  Result := PivotMatrix.Row[2];
  Result.SetNormalize;
end;

function TMovableObject.GetAbsoluteLeft: TVector;
begin
  Result := PivotMatrix.Row[0];
  Result.SetNormalize;
end;

function TMovableObject.GetAbsolutePosition: TVector;
begin
  Result := PivotMatrix.Row[3];
end;

function TMovableObject.GetAbsoluteUp: TVector;
begin
  Result := PivotMatrix.Row[1];
  Result.SetNormalize;
end;

function TMovableObject.GetDirection: TVector;
begin
  Result := FRotationMatrix.Row[2];
  Result.SetNormalize;
end;

function TMovableObject.GetInvPivotMatrix: TMatrix;
begin
  if not FWorldMatrixUpdated then UpdateWorldMatrix();
  Result := FInvPivotMatrix;
end;

function TMovableObject.GetInvWorldMatrix: TMatrix;
begin
  if not FWorldMatrixUpdated then UpdateWorldMatrix();
  Result := FInvWorldMatrix;
end;

function TMovableObject.GetLeftVector: TVector;
begin
  Result := FRotationMatrix.Row[0];
  Result.SetNormalize;
end;

function TMovableObject.GetNormalMatrix: TMatrix;
begin
  if not FWorldMatrixUpdated then UpdateWorldMatrix();
  Result := FNormalMatrix;
end;

function TMovableObject.GetPivot: TMovableObject;
begin
  Result := Parent as TMovableObject;
end;

function TMovableObject.GetPivotMatrix: TMatrix;
begin
  if not FWorldMatrixUpdated then UpdateWorldMatrix();
  Result := FPivotMatrix;
end;

function TMovableObject.GetPosition: TVector;
begin
  Result := FTranslationMatrix.Row[3];
end;

function TMovableObject.GetScale: TVector;
var
  m: mat3;
begin
  m := FScaleMatrix.Matrix3;
  Result := Vector(m[0][0], m[1][1], m[2][2]);
end;

function TMovableObject.GetUpVector: TVector;
begin
  Result := FRotationMatrix.Row[1];
  Result.SetNormalize;
end;

function TMovableObject.GetWorldMatrix: TMatrix;
begin
  if not FWorldMatrixUpdated then UpdateWorldMatrix();
  Result := FWorldMatrix;
end;

function TMovableObject.GetWorldMatrixT: TMatrix;
begin
  if not FWorldMatrixUpdated then UpdateWorldMatrix();
  Result := FWorldMatrixT;
end;

procedure TMovableObject.MoveObject(const aPosition: TVector);
begin
  FTranslationMatrix := TMatrix.TranslationMatrix( aPosition );
  NotifyWorldMatrixChanged;
end;

procedure TMovableObject.RescaleObject(const aNewScale: TVector);
begin
  FScaleMatrix := TMatrix.ScaleMatrix( Scale );
  NotifyWorldMatrixChanged;
end;

procedure TMovableObject.RescaleObject(aNewScaleX, aNewScaleY,
  aNewScaleZ: Single);
begin
  RescaleObject(Vector(aNewScaleX, aNewScaleY, aNewScaleZ, 0));
end;

procedure TMovableObject.ResetMatrices;
begin
  FModelMatrix.SetIdentity;
  FScaleMatrix.SetIdentity;
  FRotationMatrix.SetIdentity;
  FTranslationMatrix.SetIdentity;
  FRollAngle:=0;
  FTurnAngle:=0;
  FPitchAngle:=0;
  FXRotationAngle:=0;
  FYRotationAngle:=0;
  FZRotationAngle:=0;
  NotifyWorldMatrixChanged;
end;

procedure TMovableObject.RotateObject(const Axis: TVector; Angle: single;
  AbsoluteRotation: boolean);
var mr: TMatrix;
begin
  mr:=TMatrix.RotationMatrix( Axis.Affine, Angle );
  if AbsoluteRotation then RotationMatrix:=mr
  else RotationMatrix:=RotationMatrix*mr;
  NotifyWorldMatrixChanged;
end;

procedure TMovableObject.ScaleObject(const aScale: TVector);
begin
  FScaleMatrix := FScaleMatrix * TMatrix.ScaleMatrix( Scale );
  NotifyWorldMatrixChanged;
end;

procedure TMovableObject.ScaleObject(aScaleX, aScaleY, aScaleZ: Single);
begin
  ScaleObject(Vector(aScaleX, aScaleY, aScaleZ, 0));
end;


procedure TMovableObject.UpdateWorldMatrix(UseMatrix: TTransforms=ALL_TRANSFORM);
var wm, pm, srp: TMatrix;
begin
  wm.SetIdentity;
  if (Pivot<>nil) and (ttPivot in UseMatrix)
    then pm := Pivot.PivotMatrix
    else pm.SetIdentity;

  if ttModel in UseMatrix
    then wm := ModelMatrix
    else wm.SetIdentity;

  srp.SetIdentity;
  if ttScale in UseMatrix then srp := srp * ScaleMatrix;
  if ttRotation in UseMatrix then srp := srp * RotationMatrix;
  if ttPosition in UseMatrix then srp := srp * TranslationMatrix;

  pm := srp * pm;
  wm := wm * pm;

  FWorldMatrix := wm;
  FPivotMatrix := pm;

  FNormalMatrix := FWorldMatrix.Normalize;
  FWorldMatrixT := FWorldMatrix.Transpose;
  FInvWorldMatrix := FWorldMatrix.Invert;
  FInvPivotMatrix := FPivotMatrix.Invert;
  DirectingAxis := Vector(FWorldMatrix[0,0], FWorldMatrix[1,1], FWorldMatrix[2,2]);
  DirectingAxis.SetNormalize;
  FWorldMatrixUpdated := true;
  Inc(FWorldMatrixUpdateCounter);
end;

procedure TMovableObject.PitchObject(Angle: single);
begin
  //вокруг оси X в YZ
  if not FWorldMatrixUpdated then UpdateWorldMatrix;
  FRotationMatrix := FRotationMatrix.Pitch(TMath.DegToRad(Angle));
  FPitchAngle := FPitchAngle + Angle;
  NotifyWorldMatrixChanged;
end;

procedure TMovableObject.RollObject(Angle: single);
begin
  //вокруг оси Z в XY
  FRotationMatrix := FRotationMatrix.Roll(TMath.DegToRad(Angle));
  FRollAngle := FRollAngle + Angle;
  NotifyWorldMatrixChanged;
end;

procedure TMovableObject.TurnObject(Angle: single);
begin
  //вокруг оси Y в XZ
  if not FWorldMatrixUpdated then UpdateWorldMatrix;
  FRotationMatrix := FRotationMatrix.Turn(TMath.DegToRad(Angle));
  FTurnAngle := FTurnAngle + Angle;
  NotifyWorldMatrixChanged;
end;

procedure TMovableObject.UpdateWorldMatrix(const aWorld, aNormalWorld,
  anInvWorld, aWorldT, aPivot, anInvPivot: TMatrix);
begin
  FWorldMatrix := aWorld;
  FInvWorldMatrix := anInvWorld;
  FWorldMatrixT := aWorldT;
  FNormalMatrix := aNormalWorld;
  FPivotMAtrix := aPivot;
  FInvPivotMatrix := anInvPivot;
  DirectingAxis := Vector(FWorldMatrix[0,0], FWorldMatrix[1,1], FWorldMatrix[2,2]);
  DirectingAxis.SetNormalize;
  FWorldMatrixUpdated := true;
  Inc(FWorldMatrixUpdateCounter);
end;

procedure TMovableObject.ShiftObject(const aPosition: TVector);
begin
  FTranslationMatrix := FTranslationMatrix * TMatrix.TranslationMatrix( aPosition );
  NotifyWorldMatrixChanged;
end;

procedure TMovableObject.ShiftObject(x, y, z: Single);
begin
  ShiftObject(Vector(x, y, z, 1));
end;

procedure TMovableObject.StoreTransforms(const aTransforms: TTransforms);
var
  wm, mm: TMatrix;
begin
  if ttModel in aTransforms
    then wm := FModelMatrix
    else wm.SetIdentity;

  if ttScale in aTransforms then begin
    wm := wm * FScaleMatrix;
    FScaleMatrix.SetIdentity;
  end;

  if ttRotation in aTransforms then begin
    wm := wm * FRotationMatrix;
    FRotationMatrix.SetIdentity;
  end;

  if ttPosition in aTransforms then begin
    wm := wm*FTranslationMatrix;
    FTranslationMatrix.SetIdentity;
  end;

  mm:=FModelMatrix;
  ResetMatrices;
  FModelMatrix:=mm*wm;
  NotifyWorldMatrixChanged;
end;

procedure TMovableObject.RotateAround(const aPivot, anUp: TVector; aPitchDelta,
  aTurnDelta: single);
var p: TVector;
begin
  p := AbsolutePosition;
  p.RotateAround(aPivot, anUp, aPitchDelta, aTurnDelta);
  AbsolutePosition := p;
end;

procedure TMovableObject.RotateAroundX(Angle: Single);
var rm: TMatrix;
begin
  //вокруг глобальной оси X
  FXRotationAngle := FXRotationAngle + Angle;
  rm:=TMatrix.RotationMatrix( TVector( vtX ), TMath.DegToRad(Angle));
  FRotationMatrix := FRotationMatrix * rm;
  NotifyWorldMatrixChanged;
end;

procedure TMovableObject.RotateAroundY(Angle: Single);
var rm: TMatrix;
begin
  //вокруг глобальной оси Y
  FYRotationAngle := FYRotationAngle + Angle;
  rm := TMatrix.RotationMatrix(TVector( vtY ), TMath.DegToRad(Angle));
  FRotationMatrix := FRotationMatrix * rm;
  NotifyWorldMatrixChanged;
end;

procedure TMovableObject.RotateAroundZ(Angle: Single);
var rm: TMatrix;
begin
  //вокруг глобальной оси Z
  FZRotationAngle := FZRotationAngle + Angle;
  rm := TMatrix.RotationMatrix(TVector( vtZ ), TMath.DegToRad(Angle));
  FRotationMatrix := FRotationMatrix * rm;
  NotifyWorldMatrixChanged;
end;

procedure TMovableObject.RotateAround(const aPivot, Axis: TVector; Angle: Single);
var p: TVector;
begin
  p := AbsolutePosition;
  p := TMatrix.RotationMatrix(Axis, Angle).Transform(p);
  AbsolutePosition := p;
end;

procedure TMovableObject.SetParent(const Value: TBaseSceneItem);
begin
  if Parent <> Value then begin
    inherited SetParent(Value);
    NotifyWorldMatrixChanged;
  end;
end;

procedure TMovableObject.SetPitchAngle(const Value: single);
begin
  if not TMath.SameValue(FPitchAngle, Value) then
    PitchObject(Value - FPitchAngle);
end;

procedure TMovableObject.SetPivot(const Value: TMovableObject);
begin
  Parent := Value;
end;

procedure TMovableObject.SetPosition(const Value: TVector);
begin
  MoveObject(Value);
end;

procedure TMovableObject.SetRollAngle(const Value: single);
begin
  if not TMath.SameValue(FRollAngle, Value) then
    RollObject(Value - FRollAngle);
end;

procedure TMovableObject.setRotMatrix(const Value: TMatrix);
begin
  if FRotationMatrix <> Value then begin
    FRotationMatrix := Value;
    NotifyWorldMatrixChanged;
  end;
end;

procedure TMovableObject.SetScale(const Value: TVector);
begin
  RescaleObject(Value);
end;

procedure TMovableObject.SetScaleMatrix(const Value: TMatrix);
begin
  if FScaleMatrix = Value then begin
    FScaleMatrix := Value;
    NotifyWorldMatrixChanged;
  end;
end;

procedure TMovableObject.SetTranslMatrix(const Value: TMatrix);
begin
  if FTranslationMatrix <> Value then begin
    FTranslationMatrix := Value;
    NotifyWorldMatrixChanged;
  end;
end;

procedure TMovableObject.SetTurnAngle(const Value: single);
begin
  if not TMath.SameValue(FTurnAngle, Value) then
    TurnObject(Value - FTurnAngle);
end;

procedure TMovableObject.SetUpVector(const Value: TVector);
begin
  FRotationMatrix := TMatrix.LookAtMatrix(TVector.Null, Direction, Value);
  FRotationMatrix.SetNormalize;
  NotifyWorldMatrixChanged;
end;

procedure TMovableObject.SetXRotationAngle(const Value: single);
begin
  if not TMath.SameValue(FXRotationAngle, Value) then
    RotateAroundX(Value - FXRotationAngle);
end;

procedure TMovableObject.SetYRotationAngle(const Value: single);
begin
  if not TMath.SameValue(FYRotationAngle, Value) then
    RotateAroundY(Value - FYRotationAngle);
end;

procedure TMovableObject.SetZRotationAngle(const Value: single);
begin
  if not TMath.SameValue(FZRotationAngle, Value) then
    RotateAroundZ(Value - FZRotationAngle);
end;

procedure TMovableObject.MoveForward(Step: single);
var
  dir: vec3;
begin
  dir := Direction.Vec3;
  FTranslationMatrix[3,0]:=FTranslationMatrix[3,0]+dir[0]*Step;
  FTranslationMatrix[3,1]:=FTranslationMatrix[3,1]+dir[1]*Step;
  FTranslationMatrix[3,2]:=FTranslationMatrix[3,2]+dir[2]*Step;
  NotifyWorldMatrixChanged;
end;

procedure TMovableObject.MoveLeft(Step: single);
var
  left_: vec3;
begin
  left_ := Left.Vec3;
  FTranslationMatrix[3,0]:=FTranslationMatrix[3,0]+left_[0]*Step;
  FTranslationMatrix[3,1]:=FTranslationMatrix[3,1]+left_[1]*Step;
  FTranslationMatrix[3,2]:=FTranslationMatrix[3,2]+left_[2]*Step;
  NotifyWorldMatrixChanged;
end;

procedure TMovableObject.MoveUp(Step: single);
var
  up_: vec3;
begin
  up_ := Up.Vec3;
  FTranslationMatrix[3,0]:=FTranslationMatrix[3,0]+up_[0]*Step;
  FTranslationMatrix[3,1]:=FTranslationMatrix[3,1]+up_[1]*Step;
  FTranslationMatrix[3,2]:=FTranslationMatrix[3,2]+up_[2]*Step;
  NotifyWorldMatrixChanged;
end;

procedure TMovableObject.Notify(Sender: TObject; Msg: Cardinal;
  Params: pointer);
begin
  if Sender = FParent then
  case Msg of
    NM_WorldMatrixChanged: begin
//      if Childs.inList(Sender) then
        NotifyWorldMatrixChanged;
    end;
    NM_ObjectDestroyed: begin
      FParent := nil; NotifyWorldMatrixChanged;
    end;
  end;

  inherited;
end;

procedure TMovableObject.MoveObject(x, y, z: Single);
begin
  MoveObject(Vector(x, y, z, 1));
end;

procedure TMovableObject.SetAbsoluteDirection(const Value: TVector);
begin
  Direction := VectorToLocal(Value);
end;

procedure TMovableObject.SetAbsoluteLeft(const Value: TVector);
begin
  Left := VectorToLocal(Value);
end;

procedure TMovableObject.SetAbsolutePosition(const Value: TVector);
begin
  MoveObject(AbsoluteToLocal(Value));
end;

procedure TMovableObject.SetAbsoluteUp(const Value: TVector);
begin
  Up := VectorToLocal(Value);
end;

procedure TMovableObject.SetDirBehavior(const Value: TDirectionBehavior);
begin
  if FDirBehavior <> Value then
  begin
    FDirBehavior := Value;
  end;
end;

procedure TMovableObject.SetDirection(const aDirection: TVector);
begin
  FRotationMatrix := TMatrix.LookAtMatrix(TVector.Null, aDirection, Vector(0, 1, 0));
  FRotationMatrix.SetNormalize;
  NotifyWorldMatrixChanged;
end;

procedure TMovableObject.SetLeftVector(const Value: TVector);
var
  newUp: TVector;
begin
  newUp := Direction.Cross(Value);
  FRotationMatrix := TMatrix.LookAtMatrix(TVector.Null, Direction, newUp);
  FRotationMatrix.SetNormalize;
  NotifyWorldMatrixChanged;
end;

procedure TMovableObject.SetModelMatrix(const Value: TMatrix);
begin
  if FModelMatrix <> Value then begin
    FModelMatrix := Value;
    NotifyWorldMatrixChanged;
  end;
end;

function TMovableObject.LocalToAbsolute(const aPosition: TVector): TVector;
begin
  Result := PivotMatrix.Transform(aPosition);
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
  FChilds.OnChange := OnItemsChanged;
  FNestingDepth := -1;
end;

destructor TBaseSceneItem.Destroy;
begin
  FChilds.Free;
  inherited;
end;


procedure TBaseSceneItem.SetParent(const Value: TBaseSceneItem);
begin
  if FParent = Value then exit;

  if assigned(FParent) then FParent.Childs.RemoveSceneItem(Self);
  if assigned(Value) then begin
    Value.Childs.AddSceneItem(Self);
  end else FNestingDepth := 0;
end;

procedure TBaseSceneItem.OnItemsChanged(anItem: TBaseSceneItem; aChnages: TSceneItemListChanges);
begin
  case aChnages of
    chAdd: begin
      if Assigned(anItem.Parent) then
        anItem.Parent.Childs.RemoveSceneItem(anItem);
      anItem.FParent := Self;
      Subscribe(anItem);
    end;
    chRemove: begin
      if anItem.Parent = Self then
      begin
        UnSubscribe(anItem);
        anItem.FParent := nil;
      end;
    end;
  end;
end;

procedure TBaseSceneItem.RecalcNestingDepth;

  procedure GoDeep(aChilds: TSceneItemList; aDeep: integer);
  var i: integer;
  begin
    for I := 0 to aChilds.Count - 1 do if Assigned(aChilds[i]) then begin
      aChilds[i].FNestingDepth := aDeep;
      GoDeep(aChilds[i].FChilds, aDeep + 1);
    end;
  end;

begin
  Assert(FParent = nil);
  GoDeep(FChilds, 0);
end;

{ TSceneItemList }

function TSceneItemList.AddSceneItem(
  const aItem: TBaseSceneItem): integer;
begin
  result:=AddKey(aItem.GUID, aItem);
  if Assigned(FOnChange) then FOnChange(aItem, chAdd);
end;

destructor TSceneItemList.Destroy;
var i: integer;
    obj: TBaseSceneItem;
begin
  for i:=0 to Count-1 do begin
    obj := getItemObj(i);
    if assigned(obj) then begin
      if obj.Owner = self then  obj.Free;
    end;
  end;

  inherited;
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
      FItems[i].Value:=nil;
      if Assigned(FOnChange) then FOnChange(aItem, chRemove);
      exit;
    end;
  end;
end;

end.
