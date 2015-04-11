# Свойства #

<table cellpadding='5' width='840px' border='1' cellspacing='0'><th align='left'><b>Property</b></th><th width='100px' align='left'><b>Defined By</b></th>

<tr><td><div><b><a href='TVector#Value.md'>Value</a></b> : <a href='uVMath#Vec4.md'>Vec4</a></div>Массив компонентов вектора<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Vector4.md'>Vector4</a></b> : <a href='uVMath#Vec4.md'>Vec4</a></div>Четырехкомпонентный вектор<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Vector3.md'>Vector3</a></b> : <a href='uVMath#Vec3.md'>Vec3</a></div>Трехкомпонентный вектор<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Vector2.md'>Vector2</a></b> : <a href='uVMath#Vec2.md'>Vec2</a></div>Двухкомпонентный вектор<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#X.md'>X</a></b> : Single</div>Компонент X вектора<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Y.md'>Y</a></b> : Single</div>Компонент Y вектора<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Z.md'>Z</a></b> : Single</div>Компонент Z вектора<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#W.md'>W</a></b> : Single</div>Компонент W вектора<br>
</td><td>uVMath</td></tr>

</table>

<br />
# Methods #

<table cellpadding='5' width='840px' border='1' cellspacing='0'><th align='left'><b>Method</b></th><th width='100px' align='left'><b>Defined By</b></th>

<tr><td><div><b><a href='TVector#Abs.md'>Abs</a></b> : <a href='TVector.md'>TVector</a></div>Модуль компонентов из исходного вектора<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Affine.md'>Affine</a></b> : <a href='TVector.md'>TVector</a></div>Обнуление компонента W из исходного вектора для аффинных преобразований<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Combine.md'>Combine</a></b> ( aVector: <a href='TVector.md'>TVector</a>; aF1,aF2: Single ): <a href='TVector.md'>TVector</a></div>Комбинация двух векторов<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#CombineEx.md'>CombineEx</a></b> ( aV1,aV2: <a href='TVector.md'>TVector</a>; aF1,aF2,aF3: Single ): <a href='TVector.md'>TVector</a></div>Комбинация трёх векторов<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Cross.md'>Cross</a></b> ( aVector: <a href='TVector.md'>TVector</a> ): <a href='TVector.md'>TVector</a></div>Векторное произведение<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Distance.md'>Distance</a></b> ( aPoint: <a href='TVector.md'>TVector</a> ): Single</div>Расстояние до точки<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#DistanceSqr.md'>DistanceSqr</a></b> ( aPoint: <a href='TVector.md'>TVector</a> ): Single</div>Квадрат расстояния до точки<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Dot.md'>Dot</a></b> ( aVector: <a href='TVector.md'>TVector</a> ): Single</div>Скалярное произведение<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Floor.md'>Floor</a></b> : <a href='TVector.md'>TVector</a></div>Отсечение дробной части компонентов из исходного вектора<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Fract.md'>Fract</a></b> : <a href='TVector.md'>TVector</a></div>Отсечение целой части компонентов из исходного вектора<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#GetAddr.md'>GetAddr</a></b> : <a href='TVector#PVec4.md'>PVec4</a></div>Ссылка на вектор<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Length.md'>Length</a></b> : Single</div>Длина вектора<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#LengthSqr.md'>LengthSqr</a></b> : Single</div>Квадрат длины вектора<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Lerp.md'>Lerp</a></b> ( aVector: <a href='TVector.md'>TVector</a>; aRatio: Single ) : <a href='TVector.md'>TVector</a></div>Интерполяция исходного вектора в aVector<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#LerpSmooth.md'>LerpSmooth</a></b> ( aVector: <a href='TVector.md'>TVector</a>; aRatio: Single ) : <a href='TVector.md'>TVector</a></div>Плавная интерполяция исходного вектора в aVector<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Make.md'>Make</a></b> ( aX,aY: Single; aZ: Single = 0; aW: Single = 0 ): <a href='TVector.md'>TVector</a>;</div>Покомпонентное заполнение нового вектора<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#MakeTmp.md'>MakeTmp</a></b> ( aVecType: <a href='uVMath#TVecTemp.md'>TVecTemp</a> ): <a href='TVector.md'>TVector</a>;</div>Заполнение нового вектора из шаблона<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Negate.md'>Negate</a></b> : <a href='TVector.md'>TVector</a></div>Противоположный вектор<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Normalize.md'>Normalize</a></b> : <a href='TVector.md'>TVector</a></div>Нормализованный вектор из исходного<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Perpendicular.md'>Perpendicular</a></b> ( aNVector: <a href='TVector.md'>TVector</a> ): <a href='TVector.md'>TVector</a></div>Перпендикуляр<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Point.md'>Point</a></b> : <a href='TVector.md'>TVector</a>;</div>Подготовка вектора для матричных преобразований, W = 1.<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#PointProject.md'>PointProject</a></b> ( aPos,aDir: <a href='TVector.md'>TVector</a> ): Single</div>Проекция точки на луч<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Reflect.md'>Reflect</a></b> ( aNVector: <a href='TVector.md'>TVector</a> ): <a href='TVector.md'>TVector</a></div>Отражение от aNVector<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Rotate.md'>Rotate</a></b> ( aAxis: <a href='TVector.md'>TVector</a>; aAngle: Single ): <a href='TVector.md'>TVector</a></div>Вращение вокруг оси aAxis<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Scale.md'>Scale</a></b> ( aFactor: Single ): <a href='TVector.md'>TVector</a></div>Размер вектора<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#SetAbs.md'>SetAbs</a></b></div>Модуль компонентов исходного вектора<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Methods.md'>SetAffine</a></b>;</div>Обнуление компонента W исходного вектора для аффинных преобразований<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#SetCombine.md'>SetCombine</a></b> ( aVector: <a href='TVector.md'>TVector</a>; aF1,aF2: Single )</div>Комбинация двух векторов, результат в исходном векторе<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#SetCombineEx.md'>SetCombineEx</a></b> ( aV1,aV2: <a href='TVector.md'>TVector</a>; aF1,aF2,aF3: Single )</div>Комбинация трёх векторов, результат в исходном векторе<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#SetCross.md'>SetCross</a></b> ( aVector: <a href='TVector.md'>TVector</a> )</div>Векторное умножение, результат в исходном векторе<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#SetFloor.md'>SetFloor</a></b></div>Отбрасывание дробной части компонентов иходного вектора<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#SetFract.md'>SetFract</a></b></div>Сохранение только дробной части компонентов исходного вектора<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#SetLerp.md'>SetLerp</a></b> ( aVector: <a href='TVector.md'>TVector</a>; aRatio: Single )</div>Интерполяция исходного вектора в aVector<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#SetLerpSmooth.md'>SetLerpSmooth</a></b> ( aVector: <a href='TVector.md'>TVector</a>; aRatio: Single )</div>Плавная интерполяция исходного вектора в aVector<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#SetNegate.md'>SetNegate</a></b></div>Вектор, противоположный от исходного<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#SetNorm.md'>SetNormalize</a></b></div>Нормализация исходного вектора<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#SetPerp.md'>SetPerperpendicular</a></b> ( aNVector: <a href='TVector.md'>TVector</a> )</div>Перпендикуляр к aNVector<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Methods.md'>SetPoint</a></b>;</div>Подготовка вектора для матричных преобразований, W = 1.<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#SetReflect.md'>SetReflect</a></b> ( aNVector: <a href='TVector.md'>TVector</a> )</div>Отражение от aNVector<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#SetRotate.md'>SetRotate</a></b> ( aAxis: <a href='TVector.md'>TVector</a>; aAngle: Single )</div>Вращение вокруг оси aAxis<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#SetScale.md'>SetScale</a></b> ( aFactor: Single )</div>Размер вектора<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#SetVec2.md'>SetVec2</a></b>;</div>Обнуление компонентов Z и W исходного вектора.<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#SetVector.md'>SetVector</a></b> ( aX,aY:Single; aZ:Single = 0; aW:Single = 0 )</div>Покомпонентное заполнение вектора<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#SetVectorTmp.md'>SetVectorTmp</a></b> ( aVecType:<a href='uVMath#TVecTemp.md'>TVecTemp</a> )</div>Заполнение вектора из шаблона<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#Squeeze.md'>Squeeze</a></b> ( aA: <a href='uVMath#TVecTemp.md'>TVecTemp</a>; aB: <a href='uVMath#TVecTemp.md'>TVecTemp</a> = vt0; aC: <a href='uVMath#TVecTemp.md'>TVecTemp</a> = vt0; aD: <a href='uVMath#TVecTemp.md'>TVecTemp</a> = vt0 ): <a href='TVector.md'>TVector</a>;</div>Смешивание компонентов вектора<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#SwapWith.md'>SwapWith</a></b> ( <b>var</b> aVector:<a href='TVector.md'>TVector</a> )</div>Обмен векторов<br>
</td><td>uVMath</td></tr>
<tr><td><div><b><a href='TVector#ToVec2.md'>ToVec2</a></b>;</div>Обнуление компонентов Z и W из исходного вектора<br>
</td><td>uVMath</td></tr>

</table>

<br />
# Операторы #


<br /><br />
![http://img72.imageshack.us/img72/7286/ttlmethod.png](http://img72.imageshack.us/img72/7286/ttlmethod.png)

## Abs ##
function **[Abs](TVector#Methods.md)** : [TVector](TVector.md);

Модуль компонентов из исходного вектора:
```
 result.X := Abs( self.X );
 result.Y := Abs( self.Y );
 result.Z := Abs( self.Z );
 result.W := Abs( self.W );
```

<div>Returns</div>*** [TVector](TVector.md) - вектор с компонентами >= 0**


---

## Affine ##
function **[Affine](TVector#Methods.md)** : [TVector](TVector.md);

Обнуление компонента W из исходного вектора для аффинных преобразований.

**Returns**
> <div><a href='TVector.md'>TVector</a> - вектор, где W = 0</div>


---

## Combine ##
function **[Combine](TVector#Methods.md)** ( aVector: [TVector](TVector.md); aF1,aF2: Single ) : [TVector](TVector.md);

Комбинация двух векторов:
```
 result := self * aF1 + aVector * aF2;
```

**Parameters**
> <div><b>aVector: <a href='TVector.md'>TVector</a></b> - второй вектор после исходного, участвующий в комбинировании</div>
> <div><b>aF1: Single</b> - коэффициент исходного вектора в комбинировании</div>
> <div><b>aF2: Single</b> - коэффициент для aVector</div>

**Returns**
> <div><a href='TVector.md'>TVector</a> - вектор, результат комбинирования</div>


---

## CombineEx ##
function **[CombineEx](TVector#Methods.md)** ( aV2,aV3: [TVector](TVector.md); aF1,aF2,aF3: Single ): [TVector](TVector.md);

Комбинация трех векторов:
```
 result := self * aF1 + aV2 * aF2 + aV3 * aF3;
```

**Parameters**
> <div><b>aV2: <a href='TVector.md'>TVector</a></b> - второй вектор участвующий в комбинировании</div>
> <div><b>aV3: <a href='TVector.md'>TVector</a></b> - третий вектор</div>
> <div><b>aF1: Single</b> - коэффициент исходного вектора в комбинировании</div>
> <div><b>aF2: Single</b> - коэффициент для aV2</div>
> <div><b>aF3: Single</b> - коэффициент для aV3</div>

**Returns**
> <div><a href='TVector.md'>TVector</a> - вектор, результат комбинирования</div>


---

## Cross ##
function **[Cross](TVector#Methods.md)** ( aVector: [TVector](TVector.md) ): [TVector](TVector.md);

Векторное произведение:
```
 result := self * aVector;
```

**Parameters**
> <div><b>aVector: <a href='TVector.md'>TVector</a></b> - вектор, участвующий в векторном произведении</div>

**Returns**
> <div><a href='TVector.md'>TVector</a> - вектор, результат векторного произведения</div>


---

## Distance ##
function **[Distance](TVector#Methods.md)** ( aPoint: [TVector](TVector.md) ): Single;

Расстояние до точки aPoint.

**Parameters**
> <div><b>aPoint: <a href='TVector.md'>TVector</a></b> - точка, до которой расчитывается расстояние</div>

**Returns**
> <div>Single - расстояние</div>


---

## DistanceSqr ##
function **[DistanceSqr](TVector#Methods.md)** ( aPoint: [TVector](TVector.md) ): Single;

Квадрат расстояния до точки aPoint.

**Parameters**
> <div><b>aPoint: <a href='TVector.md'>TVector</a></b> - точка, до которой расчитывается расстояние</div>

**Returns**
> <div>Single - квадрат расстояния</div>


---

## Dot ##
function **[Dot](TVector#Methods.md)** ( aVector: [TVector](TVector.md) ): Single;

Скалярное произведение:
```
 result := self.X * aVector.X + self.Y * aVector.Y + self.Z * aVector.Z + self.W * aVector.W;
```

**Parameters**
> <div><b>aVector: <a href='TVector.md'>TVector</a></b> - вектор, участвующий в скалярном произведении</div>

**Returns**
> <div>Single - скалярное произведение</div>


---

## Floor ##
function **[Floor](TVector#Methods.md)** : [TVector](TVector.md);

Отсечение дробной части компонентов из исходного вектора.

**Returns**
> <div><a href='TVector.md'>TVector</a> - вектор с компонентами без дробной части</div>


---

## Fract ##
function **[Fract](TVector#Methods.md)** : [TVector](TVector.md);

Отсечение целой части компонентов из исходного вектора.

**Returns**
> <div><a href='TVector.md'>TVector</a> - вектор с компонентами без целой части</div>


---

## GetAddr ##
function **[GetAddr](TVector#Methods.md)** : [PVec4](uVMath#PVec4.md);

Ссылка на вектора.

**Returns**
> <div><a href='uVMath#PVec4.md'>PVec4</a> - ссылка на вектор</div>


---

## Length ##
function **[Length](TVector#Methods.md)** : Single;

Длина вектора.

**Returns**
> <div>Single - длина</div>


---

## LengthSqr ##
function **[LengthSqr](TVector#Methods.md)** : Single;

Квадрат длины вектора.

**Returns**
> <div>Single - квадрат длины</div>


---

## Lerp ##
function **[Lerp](TVector#Methods.md)** ( aVector: [TVector](TVector.md); aRatio: Single ): [TVector](TVector.md);

Интерполяция исходного вектора в aVector.

**Parameters**
> <div><b>aVector: <a href='TVector.md'>TVector</a></b> - вектор, к которому интерполидуется исходный</div>
> <div><b>aRatio: Single</b> - степень интерполяции [0..1]</div>

**Returns**
> <div><a href='TVector.md'>TVector</a> - интерполяция</div>


---

## LerpSmooth ##
function **[LerpSmooth](TVector#Methods.md)** ( aVector: [TVector](TVector.md); aRatio: Single ): [TVector](TVector.md);

Плавная интерполяция исходного вектора в aVector.

**Parameters**
> <div><b>aVector: <a href='TVector.md'>TVector</a></b> - вектор, к которому интерполидуется исходный</div>
> <div><b>aRatio: Single</b> - степень интерполяции [0..1]</div>

**Returns**
> <div><a href='TVector.md'>TVector</a> - интерполяция</div>


---

## Make ##
`[static]` function **[Make](TVector#Methods.md)** ( aX,aY: Single; aZ: Single = 0; aW: Single = 0 ): [TVector](TVector.md);

Покомпонентное заполнение нового вектора.

**Parameters**
> <div><b>aX: Single</b> - первый X компонент вектора</div>
> <div><b>aY: Single</b> - второй Y компонент вектора</div>
> <div><b>aZ: Single</b> - третий Z компонент вектора <code>[def = 0]</code></div>
> <div><b>aW: Single</b> - четвертый W компонент вектора <code>[def = 0]</code></div>

**Returns**
> <div><a href='TVector.md'>TVector</a> - новый вектор</div>


---

## MakeTmp ##
`[static]` function **[MakeTmp](TVector#Methods.md)** ( aVecType: [TVecTemp](uVMath#TVecTemp.md) ): [TVector](TVector.md);

Заполнение нового вектора из шаблона.

**Parameters**
> <div><b>aVecType: <a href='uVMath#TVecTemp.md'>TVecTemp</a></b> - вектор из набора шаблонов векторов</div>

**Returns**
> <div><a href='TVector.md'>TVector</a> - новый вектор</div>


---

## Negate ##
function **[Negate](TVector#Methods.md)** : [TVector](TVector.md);

Противоположный вектор.

**Returns**
> <div><a href='TVector.md'>TVector</a></div>


---

## Normalize ##
function **[Normalize](TVector#Methods.md)** : [TVector](TVector.md);

Нормализованный вектор из исходного.

**Returns**
> <div><a href='TVector.md'>TVector</a></div>


---

## Perpendicular ##
function **[Perpendicular](TVector#Methods.md)** ( aNVector: [TVector](TVector.md) ): [TVector](TVector.md);

Угол между исходным вектором и aNVector выставляется равным 90°.

**Parameters**
> <div><b>aNVector: <a href='TVector.md'>TVector</a></b> - вектор, относительно которго нужно получить перпендикуляр</div>

**Returns**
> <div><a href='TVector.md'>TVector</a> - перпендикуляр к вектору aNVector</div>


---

## Point ##
function **[Point](TVector#Methods.md)** : [TVector](TVector.md);

Подготовка вектора для матричных преобразований, W = 1.

**Returns**
> <div><a href='TVector.md'>TVector</a></div>


---

## PointProject ##
function **[PointProject](TVector#Methods.md)** ( aPos,aDir: [TVector](TVector.md) ): Single;

Проекция исходного вектора на луч, заданный aPos и aDir.

**Parameters**
> <div><b>aPos: <a href='TVector.md'>TVector</a></b> - начало луча</div>
> <div><b>aDir: <a href='TVector.md'>TVector</a></b> - направление луча</div>

**Returns**
> <div>Single - проекция</div>


---

## Reflect ##
function **[Reflect](TVector#Methods.md)** ( aNVector: [TVector](TVector.md) ): [TVector](TVector.md);

Отражение исходного вектора от aNVector.

**Parameters**
> <div><b>aNVector: <a href='TVector.md'>TVector</a></b> - вектор, относительно которого нужно получить отражение</div>

**Returns**
> <div><a href='TVector.md'>TVector</a> - отраженный вектор</div>


---

## Rotate ##
function **[Rotate](TVector#Methods.md)** ( aAxis: [TVector](TVector.md); aAngle: Single ): [TVector](TVector.md);

Вращение исходного вектора вокруг оси aAxis на веричину aAngle.

**Parameters**
> <div><b>aAxis: <a href='TVector.md'>TVector</a></b> - ось вращения</div>
> <div><b>aAngle: Single</b> - угол</div>

**Returns**
> <div><a href='TVector.md'>TVector</a> - повернутый вектор</div>


---

## Scale ##
function **[Scale](TVector#Methods.md)** ( aFactor: Single ): [TVector](TVector.md);

Изменение размера вектора из исходного:
```
 result.X := self.X * aFactor;
 result.Y := self.Y * aFactor;
 result.Z := self.Z * aFactor;
 result.W := self.W * aFactor;
```

**Parameters**
> <div><b>aFactor: Single</b> - коэффициент увеличения вектора</div>

**Returns**
> <div><a href='TVector.md'>TVector</a></div>


---

## SetAbs ##
procedure **[SetAbs](TVector#Methods.md)**;

Модуль компонентов исходного вектора:
```
 self.X := Abs( self.X );
 self.Y := Abs( self.Y );
 self.Z := Abs( self.Z );
 self.W := Abs( self.W );
```


---

## SetAffine ##
procedure **[SetAffine](TVector#Methods.md)**;

Вектор для афинных преобразований, `[W = 0]`.


---

## SetCombine ##
procedure **[SetCombine](TVector#Methods.md)** ( aVector: [TVector](TVector.md); aF1,aF2: Single );

Комбинация двух векторов:
```
 self := self * aF1 + aVector * aF2;
```


---

## SetCombineEx ##
procedure **[SetCombineEx](TVector#Methods.md)** ( aV2,aV3: [TVector](TVector.md); aF1,aF2,aF3: Single );

Комбинация трех векторов:
```
 self := self * aF1 + aV2 * aF2 + aV3 * aF3;
```

**Parameters**
> <div><b>aV2: <a href='TVector.md'>TVector</a></b> - второй вектор участвующий в комбинировании</div>
> <div><b>aV3: <a href='TVector.md'>TVector</a></b> - третий вектор</div>
> <div><b>aF1: Single</b> - коэффициент исходного вектора в комбинировании</div>
> <div><b>aF2: Single</b> - коэффициент для aV2</div>
> <div><b>aF3: Single</b> - коэффициент для aV3</div>


---

## SetCross ##
procedure **[SetCross](TVector#Methods.md)** ( aVector: [TVector](TVector.md) );

Векторное произведение:
```
 self := self * aVector;
```

**Parameters**
> <div><b>aVector: <a href='TVector.md'>TVector</a></b> - вектор, участвующий в векторном произведении</div>


---

## SetFloor ##
procedure **[SetFloor](TVector#Methods.md)**;

Отсечение дробной части компонентов исходного вектора.


---

## SetFract ##
procedure **[SetFract](TVector#Methods.md)**;

Отсечение целой части компонентов исходного вектора.


---

## SetLerp ##
procedure **[SetLerp](TVector#Methods.md)** ( aVector: [TVector](TVector.md); aRatio: Single );

Интерполяция исходного вектора в aVector.

**Parameters**
> <div><b>aVector: <a href='TVector.md'>TVector</a></b> - вектор, к которому интерполидуется исходный</div>
> <div><b>aRatio: Single</b> - степень интерполяции <code>[0..1]</code></div>


---

## SetLerpSmooth ##
procedure **[SetLerpSmooth](TVector#Methods.md)** ( aVector: [TVector](TVector.md); aRatio: Single );

Плавная интерполяция исходного вектора в aVector.

**Parameters**
> <div><b>aVector: <a href='TVector.md'>TVector</a></b> - вектор, к которому интерполидуется исходный</div>
> <div><b>aRatio: Single</b> - степень интерполяции <code>[0..1]</code></div>


---

## SetNegate ##
procedure **[SetNegate](TVector#Methods.md)**;

Вектор, обратный исходному.


---

## SetNormalize ##
procedure **[SetNormalize](TVector#Methods.md)**;

Нормализованный исходный вектор.


---

## SetPoint ##
procedure **[SetPoint](TVector#Methods.md)**;

Подготовка вектора для матричных преобразований `[W = 1]`.


---

## SetReflect ##
procedure **[SetReflect](TVector#Methods.md)** ( aNVector: [TVector](TVector.md) );

Отражение исходного вектора от aNVector.

**Parameters**
> <div><b>aNVector: <a href='TVector.md'>TVector</a></b> - вектор, относительно которого нужно получить отражение</div>


---

## SetRotate ##
procedure **[SetRotate](TVector#Methods.md)** ( aAxis: [TVector](TVector.md); aAngle: Single );

Вращение исходного вектора вокруг оси aAxis на веричину aAngle.

**Parameters**
> <div><b>aAxis: <a href='TVector.md'>TVector</a></b> - ось вращения</div>
> <div><b>aAngle: Single</b> - угол</div>


---

## SetPerpendicular ##
procedure **[SetPerpendicular](TVector#Methods.md)** ( aNVector: [TVector](TVector.md) );

Угол между исходным вектором и aNVector выставляется равным 90°.

**Parameters**
> <div><b>aNVector: <a href='TVector.md'>TVector</a></b> - вектор, относительно которго нужно получить перпендикуляр</div>


---

## SetScale ##
procedure **[SetScale](TVector#Methods.md)** ( aFactor: Single );

Изменение размера исходного вектора:
```
 self.X := self.X * aFactor;
 self.Y := self.Y * aFactor;
 self.Z := self.Z * aFactor;
 self.W := self.W * aFactor;
```

**Parameters**
> <div><b>aFactor: Single</b> - коэффициент увеличения вектора</div>


---

## SetVec2 ##
procedure **[SetVec2](TVector#Methods.md)**;

Обнуление компонентов Z и W исходного вектора.


---

## SetVector ##
procedure **[SetVector](TVector#Methods.md)** ( aX,aY: Single; aZ: Single = 0; aW: Single = 0 );

Покомпонентное заполнение вектора

**Parameters**
> <div><b>aX: Single</b> - первый X компонент вектора</div>
> <div><b>aY: Single</b> - второй Y компонент вектора</div>
> <div><b>aZ: Single</b> - третий Z компонент вектора, <code>[def = 0]</code></div>
> <div><b>aW: Single</b> - четвертый W компонент вектора, <code>[def = 0]</code></div>


---

## SetVectorTmp ##
procedure **[SetVectorTmp](TVector#Methods.md)** ( aVecType: [TVecTemp](uVMath#TVecTemp.md) );

Заполнение вектора из шаблона

**Parameters**
> <div><b>aVecType: <a href='uVMath#TVecTemp.md'>TVecTemp</a></b> - вектор из набора шаблонов векторов</div>


---

## Squeeze ##
function **[Squeeze](TVector#Methods.md)** ( aA: [TVecTemp](uVMath#TVecTemp.md); aB: [TVecTemp](uVMath#TVecTemp.md) = vt0; aC: [TVecTemp](uVMath#TVecTemp.md) = vt0; aD: [TVecTemp](uVMath#TVecTemp.md) = vt0 ): [TVector](TVector.md);

Смешивание компонентов вектора:
```
 examples:
 v1 := Vector( 0.1, 4, -2 ); // v1 = ( 0.1, 4, -2, 0 )
 v2 := v1.Squeeze( vtZ, vt0, vtY, vtX ); // v2 = ( -2, 0, 4, 0.1 )
 v3 := v1.Squeeze( vtX, vtX, vtX, vtY ); // v3 = ( 0.1, 0.1, 0.1, 4 )
```

**Parameters**
> <div><b>aA: <a href='uVMath#TVecTemp.md'>TVecTemp</a></b> - шаблон для компонента X вектора</div>
> <div><b>aB: <a href='uVMath#TVecTemp.md'>TVecTemp</a></b> - шаблон для компонента Y вектора, <code>[def = 0]</code></div>
> <div><b>aC: <a href='uVMath#TVecTemp.md'>TVecTemp</a></b> - шаблон для компонента Z вектора, <code>[def = 0]</code></div>
> <div><b>aD: <a href='uVMath#TVecTemp.md'>TVecTemp</a></b> - шаблон для компонента W вектора, <code>[def = 0]</code></div>

**Returns**
> <div><a href='TVector.md'>TVector</a> - результат смешивания</div>


---

## SwapWith ##
procedure **[SwapWith](TVector#Methods.md)** ( var aVector: [TVector](TVector.md) );

Копирование векторов друг в друга.

**Parameters**
> <div><b>aVector: <a href='TVector.md'>TVector</a></b> - вектор, с которым обменивается исходный</div>


---

## ToVec2 ##
function **[ToVec2](TVector#Methods.md)** : [TVector](TVector.md);

Обнуление компонентов Z и W исходного вектора.

**Returns**
> <div><a href='TVector.md'>TVector</a></div>


---
