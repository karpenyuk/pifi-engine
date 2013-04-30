{$REGION 'MatrixTest'}
  (*
  mvp, wm: mat4;
  m,v,p: mat4;
  r,t,s: mat4;
  u,x: vec4;

    m:=IdentityMatrix;
    v:=CreateLookAt(Vector(-7,-8,-5),Vector(3,5,1));
    p:=CreatePerspective(60,426/289,0.1,100);
    r:=CreateRotationMatrix(Vector(1,0.5,0.8),30);
    t:=CreateTranslationMatrix(Vector(-1,2,3,1));
    s:=CreateScaleMatrix(Vector(1,2,3,1));

    wm:=m*s*r*t;
    mvp:=wm*v*p;

    u:=SetVector(3,7,11,1);
    x:=u*mvp;
    d:=x.Dot(u);

    x.Normalize;
    u.Normalize;
    nd:=x.Dot(u);

  Model Matrix:
  M[0]: (  1.000000;   0.000000;   0.000000;   0.000000)
  M[1]: (  0.000000;   1.000000;   0.000000;   0.000000)
  M[2]: (  0.000000;   0.000000;   1.000000;   0.000000)
  M[3]: (  0.000000;   0.000000;   0.000000;   1.000000)

  Scale Matrix:
  S[0]: (  1.000000;   0.000000;   0.000000;   0.000000)
  S[1]: (  0.000000;   2.000000;   0.000000;   0.000000)
  S[2]: (  0.000000;   0.000000;   3.000000;   0.000000)
  S[3]: (  0.000000;   0.000000;   0.000000;   1.000000)

  Translation Matrix:
  T[0]: (  1.000000;   0.000000;   0.000000;   0.000000)
  T[1]: (  0.000000;   1.000000;   0.000000;   0.000000)
  T[2]: (  0.000000;   0.000000;   1.000000;   0.000000)
  T[3]: ( -1.000000;   2.000000;   3.000000;   1.000000)

  Rotation Matrix:
  R[0]: (  0.601737;   0.798693;  -0.001355;   0.000000)
  R[1]: ( -0.351207;   0.266123;   0.897682;   0.000000)
  R[2]: (  0.717332;  -0.539693;   0.440643;   0.000000)
  R[3]: (  0.000000;   0.000000;   0.000000;   1.000000)

  View Matrix:
  View[0]: ( -0.514496;  -0.638299;  -0.572598;   0.000000)
  View[1]: (  0.000000;   0.667759;  -0.744378;   0.000000)
  View[2]: (  0.857493;  -0.382979;  -0.343559;   0.000000)
  View[3]: (  0.685994;  -1.040918; -11.681006;   1.000000)

  Projection Matrix:
  Proj[0]: (  1.175030;   0.000000;   0.000000;   0.000000)
  Proj[1]: (  0.000000;   1.732051;   0.000000;   0.000000)
  Proj[2]: (  0.000000;   0.000000;  -1.002002;  -1.000000)
  Proj[3]: (  0.000000;   0.000000;  -0.200200;   0.000000)

  World Matrix:
  WM[0]: (  0.601737;   0.798693;  -0.001355;   0.000000)
  WM[1]: ( -0.702414;   0.532246;   1.795363;   0.000000)
  WM[2]: (  2.151998;  -1.619078;   1.321928;   0.000000)
  WM[3]: ( -1.000000;   2.000000;   3.000000;   1.000000)

  MVP Matrix:
  MVP[0]: ( -0.365144;   0.259400;   0.940497;   0.938618)
  MVP[1]: (  2.233614;   0.201221;   0.612027;   0.610804)
  MVP[2]: (  0.030962;  -5.128675;   0.482148;   0.481184)
  MVP[3]: (  4.433351;  -0.374192;  13.454924;  13.627841)

  Source Vector: (  3.000000;   7.000000;  11.000000;   1.000000)

  Result Vector: ( 19.313801; -54.602875;  25.864227;  26.012352)

  Result dot Source = -13.759869

  Normalized Source Vector: (  0.223607;   0.521749;   0.819892;   0.074536)

  Normalized Result Vector: (  0.281718;  -0.796457;   0.377265;   0.379425)

  Normalized: Result dot Source =  -0.014960

  const eps = 0.000001;

  *)

  function TForm2.Test: boolean;
  const eps = 0.0001;
  var mvp,wm,m,v,p: TMatrix;
      r,t,s: TMatrix;
      u,x: TVector;
      i: integer;
      d,nd: double;
      sl: TStringList;
  begin
    result:=true;

    m:=TMatrix.IdentityMatrix;
    v:=TMatrix.LookAtMatrix(TVector.Make(-7,-8,-5),TVector.Make(3,5,1),VecY);
    p:=TMatrix.PerspectiveMatrix(60,426/289,0.1,100);
    r:=TMatrix.RotationMatrix(TVector.Make(1,0.5,0.8),30);
    t:=TMatrix.TranslationMatrix(TVector.Make(-1,2,3,1));
    s:=TMatrix.ScaleMatrix(TVector.Make(1,2,3,1));

    wm:=m*(s*(r*t));
    mvp:=wm*(v*p);
    u:=TVector.Make(3,7,11,1);
    x:=mvp.Transform(u);
    d:=x.Dot(u);

    //Identity test
    result:=Result and (abs(m[0,0]-1.000000)<eps);
    result:=Result and (abs(m[0,1]-0.000000)<eps);
    result:=Result and (abs(m[0,2]-0.000000)<eps);
    result:=Result and (abs(m[0,3]-0.000000)<eps);
    result:=Result and (abs(m[1,0]-0.000000)<eps);
    result:=Result and (abs(m[1,1]-1.000000)<eps);
    result:=Result and (abs(m[1,2]-0.000000)<eps);
    result:=Result and (abs(m[1,3]-0.000000)<eps);
    result:=Result and (abs(m[2,0]-0.000000)<eps);
    result:=Result and (abs(m[2,1]-0.000000)<eps);
    result:=Result and (abs(m[2,2]-1.000000)<eps);
    result:=Result and (abs(m[2,3]-0.000000)<eps);
    result:=Result and (abs(m[3,0]-0.000000)<eps);
    result:=Result and (abs(m[3,1]-0.000000)<eps);
    result:=Result and (abs(m[3,2]-0.000000)<eps);
    result:=Result and (abs(m[3,3]-1.000000)<eps);

    assert(result, 'Identity Matrix failed');

    //Scale Matrix:
    result:=Result and (abs(s[0,0]-1.000000)<eps);
    result:=Result and (abs(s[0,1]-0.000000)<eps);
    result:=Result and (abs(s[0,2]-0.000000)<eps);
    result:=Result and (abs(s[0,3]-0.000000)<eps);

    result:=Result and (abs(s[1,0]-0.000000)<eps);
    result:=Result and (abs(s[1,1]-2.000000)<eps);
    result:=Result and (abs(s[1,2]-0.000000)<eps);
    result:=Result and (abs(s[1,3]-0.000000)<eps);

    result:=Result and (abs(s[2,0]-0.000000)<eps);
    result:=Result and (abs(s[2,1]-0.000000)<eps);
    result:=Result and (abs(s[2,2]-3.000000)<eps);
    result:=Result and (abs(s[2,3]-0.000000)<eps);

    result:=Result and (abs(s[3,0]-0.000000)<eps);
    result:=Result and (abs(s[3,1]-0.000000)<eps);
    result:=Result and (abs(s[3,2]-0.000000)<eps);
    result:=Result and (abs(s[3,3]-1.000000)<eps);

    assert(result, 'Scale Matrix creation failed');

    //Translation Matrix:
    result:=Result and (abs(t[0,0]-1.000000)<eps);
    result:=Result and (abs(t[0,1]-0.000000)<eps);
    result:=Result and (abs(t[0,2]-0.000000)<eps);
    result:=Result and (abs(t[0,3]-0.000000)<eps);
    result:=Result and (abs(t[1,0]-0.000000)<eps);
    result:=Result and (abs(t[1,1]-1.000000)<eps);
    result:=Result and (abs(t[1,2]-0.000000)<eps);
    result:=Result and (abs(t[1,3]-0.000000)<eps);
    result:=Result and (abs(t[2,0]-0.000000)<eps);
    result:=Result and (abs(t[2,1]-0.000000)<eps);
    result:=Result and (abs(t[2,2]-1.000000)<eps);
    result:=Result and (abs(t[2,3]-0.000000)<eps);
    result:=Result and (abs(t[3,0]+1.000000)<eps);
    result:=Result and (abs(t[3,1]-2.000000)<eps);
    result:=Result and (abs(t[3,2]-3.000000)<eps);
    result:=Result and (abs(t[3,3]-1.000000)<eps);

    assert(result, 'Translation Matrix creation failed');

    //Rotation Matrix:

    result:=Result and (abs(R[0,0]-0.601737)<eps);
    result:=Result and (abs(R[0,1]-0.798693)<eps);
    result:=Result and (abs(R[0,2]+0.001355)<eps);
    result:=Result and (abs(R[0,3]-0.000000)<eps);

    result:=Result and (abs(R[1,0]+0.351207)<eps);
    result:=Result and (abs(R[1,1]-0.266123)<eps);
    result:=Result and (abs(R[1,2]-0.897682)<eps);
    result:=Result and (abs(R[1,3]-0.000000)<eps);

    result:=Result and (abs(R[2,0]-0.717332)<eps);
    result:=Result and (abs(R[2,1]+0.539693)<eps);
    result:=Result and (abs(R[2,2]-0.440643)<eps);
    result:=Result and (abs(R[2,3]-0.000000)<eps);

    result:=Result and (abs(R[3,0]-0.000000)<eps);
    result:=Result and (abs(R[3,1]-0.000000)<eps);
    result:=Result and (abs(R[3,2]-0.000000)<eps);
    result:=Result and (abs(R[3,3]-1.000000)<eps);

    assert(result, 'Rotation Matrix creation failed');

    //View Matrix:

    result:=Result and (abs(v[0,0]+0.514496)<eps);
    result:=Result and (abs(v[0,1]+0.638299)<eps);
    result:=Result and (abs(v[0,2]+0.572598)<eps);
    result:=Result and (abs(v[0,3]-0.000000)<eps);

    result:=Result and (abs(v[1,0]-0.000000)<eps);
    result:=Result and (abs(v[1,1]-0.667759)<eps);
    result:=Result and (abs(v[1,2]+0.744378)<eps);
    result:=Result and (abs(v[1,3]-0.000000)<eps);

    result:=Result and (abs(v[2,0]-0.857493)<eps);
    result:=Result and (abs(v[2,1]+0.382979)<eps);
    result:=Result and (abs(v[2,2]+0.343559)<eps);
    result:=Result and (abs(v[2,3]-0.000000)<eps);

    result:=Result and (abs(v[3,0]-0.685994)<eps);
    result:=Result and (abs(v[3,1]+1.040918)<eps);
    result:=Result and (abs(v[3,2]+11.681006)<eps);
    result:=Result and (abs(v[3,3]-1.000000)<eps);

    assert(result, 'LookAt Matrix creation failed');


    //Projection Matrix:
    result:=Result and (abs(p[0,0]-1.175030)<eps);
    result:=Result and (abs(p[0,1]-0.000000)<eps);
    result:=Result and (abs(p[0,2]-0.000000)<eps);
    result:=Result and (abs(p[0,3]-0.000000)<eps);

    result:=Result and (abs(p[1,0]-0.000000)<eps);
    result:=Result and (abs(p[1,1]-1.732051)<eps);
    result:=Result and (abs(p[1,2]-0.000000)<eps);
    result:=Result and (abs(p[1,3]-0.000000)<eps);

    result:=Result and (abs(p[2,0]-0.000000)<eps);
    result:=Result and (abs(p[2,1]-0.000000)<eps);
    result:=Result and (abs(p[2,2]+1.002002)<eps);
    result:=Result and (abs(p[2,3]+1.000000)<eps);

    result:=Result and (abs(p[3,0]-0.000000)<eps);
    result:=Result and (abs(p[3,1]-0.000000)<eps);
    result:=Result and (abs(p[3,2]+0.200200)<eps);
    result:=Result and (abs(p[3,3]-0.000000)<eps);

    assert(result, 'Projection Matrix creation failed');

    //World Matrix:
    result:=Result and (abs(wm[0,0]-0.601737)<eps);
    result:=Result and (abs(wm[0,1]-0.798693)<eps);
    result:=Result and (abs(wm[0,2]+0.001355)<eps);
    result:=Result and (abs(wm[0,3]-0.000000)<eps);

    result:=Result and (abs(wm[1,0]+0.702414)<eps);
    result:=Result and (abs(wm[1,1]-0.532246)<eps);
    result:=Result and (abs(wm[1,2]-1.795363)<eps);
    result:=Result and (abs(wm[1,3]-0.000000)<eps);

    result:=Result and (abs(wm[2,0]-2.151998)<eps);
    result:=Result and (abs(wm[2,1]+1.619078)<eps);
    result:=Result and (abs(wm[2,2]-1.321928)<eps);
    result:=Result and (abs(wm[2,3]-0.000000)<eps);

    result:=Result and (abs(wm[3,0]+1.000000)<eps);
    result:=Result and (abs(wm[3,1]-2.000000)<eps);
    result:=Result and (abs(wm[3,2]-3.000000)<eps);
    result:=Result and (abs(wm[3,3]-1.000000)<eps);

    assert(result, 'World Matrix Multiplication (wm = m*r*t*s) failed');

    //MVP Matrix:

    result:=Result and (abs(mvp[0,0]+0.365144)<eps);
    result:=Result and (abs(mvp[0,1]-0.259400)<eps);
    result:=Result and (abs(mvp[0,2]-0.940497)<eps);
    result:=Result and (abs(mvp[0,3]-0.938618)<eps);

    result:=Result and (abs(mvp[1,0]-2.233614)<eps);
    result:=Result and (abs(mvp[1,1]-0.201221)<eps);
    result:=Result and (abs(mvp[1,2]-0.612027)<eps);
    result:=Result and (abs(mvp[1,3]-0.610804)<eps);

    result:=Result and (abs(mvp[2,0]-0.030962)<eps);
    result:=Result and (abs(mvp[2,1]+5.128675)<eps);
    result:=Result and (abs(mvp[2,2]-0.482148)<eps);
    result:=Result and (abs(mvp[2,3]-0.481184)<eps);

    result:=Result and (abs(mvp[3,0]-4.433351)<eps);
    result:=Result and (abs(mvp[3,1]+0.374192)<eps);
    result:=Result and (abs(mvp[3,2]-13.454924)<eps);
    result:=Result and (abs(mvp[3,3]-13.627841)<eps);

    assert(result, 'MVP Matrix Multiplication (MVP = m*v*p) failed');

    result:=Result and (abs(3.000000 - u[0])<eps);
    result:=Result and (abs(7.000000 - u[1])<eps);
    result:=Result and (abs(11.000000 - u[2])<eps);
    result:=Result and (abs(1.000000 - u[3])<eps);

    assert(result, 'Source vector Creation failed');

    result:=Result and (abs(19.313801 - x[0])<eps);
    result:=Result and (abs(-54.602875 - x[1])<eps);
    result:=Result and (abs(25.864227 - x[2])<eps);
    result:=Result and (abs(26.012352 - x[3])<eps);

    assert(result, 'Result vector Creation failed');

    result:=Result and (abs(-13.759869-d)<eps);
    assert(result, 'Result.dot.Source failed');

    x.SetNormalize; u.SetNormalize;

    result:=Result and (abs(0.223607 - u[0])<eps);
    result:=Result and (abs(0.521749 - u[1])<eps);
    result:=Result and (abs(0.819892 - u[2])<eps);
    result:=Result and (abs(0.074536 - u[3])<eps);

    assert(result, 'Source vector Normalization failed');

    result:=Result and (abs(0.281718 - x[0])<eps);
    result:=Result and (abs(-0.796457 - x[1])<eps);
    result:=Result and (abs(0.377265 - x[2])<eps);
    result:=Result and (abs(0.379425 - x[3])<eps);

    assert(result, 'Result vector Normalization failed');

    nd:=x.Dot(u);

    result:=Result and (abs(-0.014960-nd)<eps);
    assert(result, 'Result.dot.Source for Normalized vectors failed');
  end;

{$ENDREGION}