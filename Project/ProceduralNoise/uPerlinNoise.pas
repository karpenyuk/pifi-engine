unit uPerlinNoise;

interface

uses SysUtils;

Type TByteArray2d = array of array of byte;


Procedure GenNoise(var pNoise: TByteArray2d; size: integer);
Procedure GenNoise1D(var pNoise: TByteArray2d; size: integer);
Procedure PermTextureData(var pixels: PByteArray);

const
  perm: array[0..255] of byte = (151,160,137,91,90,15,
  131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
  190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
  88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
  77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
  102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
  135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
  5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
  223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
  129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
  251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
  49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
  138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180);

  grad3: array[0..15, 0..2] of integer =
                  ((0,1,1),(0,1,-1),(0,-1,1),(0,-1,-1),
                   (1,0,1),(1,0,-1),(-1,0,1),(-1,0,-1),
                   (1,1,0),(1,-1,0),(-1,1,0),(-1,-1,0), // 12 cube edges
                   (1,0,-1),(-1,0,-1),(0,-1,1),(0,1,1)); // 4 more to make 16

  grad4: array[0..31,0..3] of integer = ((0,1,1,1), (0,1,1,-1), (0,1,-1,1), (0,1,-1,-1), // 32 tesseract edges
                   (0,-1,1,1), (0,-1,1,-1), (0,-1,-1,1), (0,-1,-1,-1),
                   (1,0,1,1), (1,0,1,-1), (1,0,-1,1), (1,0,-1,-1),
                   (-1,0,1,1), (-1,0,1,-1), (-1,0,-1,1), (-1,0,-1,-1),
                   (1,1,0,1), (1,1,0,-1), (1,-1,0,1), (1,-1,0,-1),
                   (-1,1,0,1), (-1,1,0,-1), (-1,-1,0,1), (-1,-1,0,-1),
                   (1,1,1,0), (1,1,-1,0), (1,-1,1,0), (1,-1,-1,0),
                   (-1,1,1,0), (-1,1,-1,0), (-1,-1,1,0), (-1,-1,-1,0));

implementation

Function Noise2D(x,y:integer):single;
var n:integer;
begin
  n := x + y * 57;
  n := (n shl 13) xor n;
  result := ( 1.0 - ( (n * (n * n * 15731 + 789221) + 1376312589) and $7fffffff) /
      1073741824.0);
end;

Function Cosine_Interpolate(a,b,x:single):single;
var f,ft:single;
begin
  ft := x * Pi;
  f := (1 - cos(ft)) * 0.5;
  result := a*(1-f) + b*f;
end;

Function SmoothedNoise2D(x,y:integer):single;
var corners, sides, center : single;
begin
    corners := ( Noise2D(x-1, y-1)+Noise2D(x+1, y-1)+
         Noise2D(x-1, y+1)+Noise2D(x+1, y+1) ) / 16;
    sides   := ( Noise2D(x-1, y)  +Noise2D(x+1, y)  +
         Noise2D(x, y-1)  +Noise2D(x, y+1) ) /  8;
    center  :=  Noise2D(x, y) / 4;
    Result  := corners + sides + center;
end;

Function CompileNoise(x,y:single):single;
var int_X,int_Y :integer;
    fractional_X,fractional_Y:single;
    v1,v2,v3,v4:single;
    i1,i2:single;
begin
      int_X    := trunc(x);//цела€ часть х
      fractional_X := x - int_X;//дробь от х
//аналогично у
      int_Y    := trunc(y);
      fractional_Y := y - int_Y;
  //получаем 4 сглаженных значени€
     v1 := SmoothedNoise2D(int_X,     int_Y);
     v2 := SmoothedNoise2D(int_X + 1, int_Y);
     v3 := SmoothedNoise2D(int_X,     int_Y + 1);
     v4 := SmoothedNoise2D(int_X + 1, int_Y + 1);
  //интерполируем значени€ 1 и 2 пары и производим интерпол€цию между ними
     i1 := Cosine_Interpolate(v1 , v2 , fractional_X);
     i2 := Cosine_Interpolate(v3 , v4 , fractional_X);
     Result := Cosine_Interpolate(i1 , i2 , fractional_Y);
end;

Function PerlinNoise_2D(x,y:single;factor:single):integer;
const NUM_OCTAVES=10;
var total:single;
    persistence:single;
    frequency:single;
    amplitude:single;
    i:integer;
begin
   total := 0;
   persistence := 0.5;
   frequency := 0.02;
   amplitude := 1;//амплитуда, в пр€мой зависимости от значени€ настойчивости

   // вводим фактор случайности, чтобы облака не были всегда одинаковыми
   // (ћы ведь помним что ф-ци€ шума когерентна?)

   x:= x+factor; y:= y+factor;

   // NUM_OCTAVES - переменна€, котора€ обозначает число октав,
   // чем больше октав, тем лучше получаетс€ шум
   for i:=0 to NUM_OCTAVES-1 do begin
       total := total+CompileNoise(x*frequency, y*frequency) * amplitude;
       amplitude := amplitude*persistence;
       frequency := frequency*2;
    end;
    total:=abs(total);
    result:=trunc(total*255.0);//приводим цвет к значению 0-255Е
end;

Procedure GenNoise(var pNoise: TByteArray2d; size: integer);
var fac:single;
    i,j:integer;
begin
//  randomize;
  setlength(pNoise,size,size);
  fac := Random(size*size);
  for i:=0 to size-1 do begin
    for j:=0 to size-1 do begin
       //проходим по всем элементам массива и заполн€ем их значени€ми
       pNoise[i,j]:=PerlinNoise_2D(i,j,fac);
    end;
  end;
end;

Procedure GenNoise1D(var pNoise:TByteArray2d; size:integer);
var fac:single;
    i,j:integer;
begin
//  randomize;
  setlength(pNoise,size,1);
  fac := Random(size*size);
  for i:=0 to 0 do begin
    for j:=0 to size-1 do begin
       //проходим по всем элементам массива и заполн€ем их значени€ми
       pNoise[i,j]:=PerlinNoise_2D(i,j,fac);
    end;
  end;
end;

procedure PermTextureData(var pixels: PByteArray);
var i,j: integer;
    offset: integer;
    value: byte;
begin
  Getmem(pixels, 256*256*4);
  for i := 0 to 255 do for j := 0 to 255 do begin
      offset := (i*256+j)*4;
      value := perm[(j+perm[i]) and $FF];
      pixels[offset] := grad3[value and $0F][0] * 64 + 64;   // Gradient x
      pixels[offset+1] := grad3[value and $0F][1] * 64 + 64; // Gradient y
      pixels[offset+2] := grad3[value and $0F][2] * 64 + 64; // Gradient z
      pixels[offset+3] := value;                     // Permuted index
  end;
end;
end.
