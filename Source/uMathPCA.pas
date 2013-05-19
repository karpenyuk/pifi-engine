//
// Principal components analysis
//
// BuildBasis  builds  orthogonal  basis  where  first  axis  corresponds  to
// direction with maximum variance, second axis maximizes variance in subspace
// orthogonal to first axis and so on.

unit uMathPCA;

interface

uses
  Math;

const
  MACHINE_EPSILON = 5E-16;
  MAX_REAL_NUMBER = 1E300;
  MIN_REAL_NUMBER = 1E-300;

type
  TDouble1DArray = array of Double;
  TDouble2DArray = array of TDouble1DArray;

  PrincipalComponentsAnalysis = class
  public
    class procedure BuildBasis(
      const X: TDouble2DArray;
      NPoints: Integer;
      NVars: Integer;
      var Info: Integer;
      var S2: TDouble1DArray;
      var V: TDouble2DArray); static;
  private
    class procedure Move(
      VDst: PDouble; I11, I12: Integer;
      VSrc: PDouble; I21, I22: Integer); overload; static;

    class procedure Move(
      VDst: PDouble; I11, I12: Integer;
      VSrc: PDouble; I21, I22: Integer;
      S: Double); overload; static;

    class procedure Add(
      VDst: PDouble; I11, I12: Integer;
      VSrc: PDouble; I21, I22: Integer;
      S: Real); static;

    class procedure Sub(
      VDst: PDouble; I11, I12: Integer;
      VSrc: PDouble; I21, I22: Integer); overload; static;

    class procedure Sub(
      VDst: PDouble; I11, I12: Integer;
      VSrc: PDouble; I21, I22: Integer;
      S: Double); overload; static;

    class procedure Mul(
      VOp: PDouble; I1, I2: Integer;
      S: Double); static;

    class function DotProduct(
      V1: PDouble; I11, I12: Integer;
      V2: PDouble; I21, I22: Integer): Double; static;

    class procedure CopyMatrix(const A: TDouble2DArray;
      IS1: Integer;
      IS2: Integer;
      JS1: Integer;
      JS2: Integer;
      var B: TDouble2DArray;
      ID1: Integer;
      ID2: Integer;
      JD1: Integer;
      JD2: Integer); static;

    class procedure CopyAndTranspose(const A: TDouble2DArray;
      IS1: Integer;
      IS2: Integer;
      JS1: Integer;
      JS2: Integer;
      var B: TDouble2DArray;
      ID1: Integer;
      ID2: Integer;
      JD1: Integer;
      JD2: Integer); static;

    class procedure InplaceTranspose(var A: TDouble2DArray;
      I1: Integer;
      I2: Integer;
      J1: Integer;
      J2: Integer;
      var WORK: TDouble1DArray); static;

    class procedure MatrixMatrixMultiply(const A: TDouble2DArray;
      AI1: Integer;
      AI2: Integer;
      AJ1: Integer;
      AJ2: Integer;
      TransA: Boolean;
      const B: TDouble2DArray;
      BI1: Integer;
      BI2: Integer;
      BJ1: Integer;
      BJ2: Integer;
      TransB: Boolean;
      Alpha: Double;
      var C: TDouble2DArray;
      CI1: Integer;
      CI2: Integer;
      CJ1: Integer;
      CJ2: Integer;
      Beta: Double;
      var WORK: TDouble1DArray); static;

    // QR decomposition of a rectangular matrix of size MxN
    class procedure RectMatrixQR(var A: TDouble2DArray;
      M: Integer;
      N: Integer;
      var Tau: TDouble1DArray); static;

    // Partial unpacking of matrix Q from the QR decomposition of a matrix A
    class procedure RectMatrixQRUnpackQ(const A: TDouble2DArray;
      M: Integer;
      N: Integer;
      const Tau: TDouble1DArray;
      QColumns: Integer;
      var Q: TDouble2DArray); static;

    // Reduction of a rectangular matrix to  bidiagonal form
    class procedure RectMatrixBD(var A: TDouble2DArray;
      M: Integer;
      N: Integer;
      var TauQ: TDouble1DArray;
      var TauP: TDouble1DArray); static;

    // Unpacking matrix P which reduces matrix A to bidiagonal form.
    // The subrvarine returns transposed matrix P.
    class procedure RectMatrixBDUnpackPT(const QP: TDouble2DArray;
      M: Integer;
      N: Integer;
      const TauP: TDouble1DArray;
      PTRows: Integer;
      var PT: TDouble2DArray); static;

    // Multiplication by matrix P which reduces matrix A to  bidiagonal form.
    class procedure RectMatrixBDMultiplyByP(const QP: TDouble2DArray;
      M: Integer;
      N: Integer;
      const TauP: TDouble1DArray;
      var Z: TDouble2DArray;
      ZRows: Integer;
      ZColumns: Integer;
      FromTheRight: Boolean;
      DoTranspose: Boolean); static;

    // Unpacking matrix Q which reduces a matrix to bidiagonal form.
    class procedure RectMatrixBDUnpackQ(const QP: TDouble2DArray;
      M: Integer;
      N: Integer;
      const TauQ: TDouble1DArray;
      QColumns: Integer;
      var Q: TDouble2DArray); static;

    //  Multiplication by matrix Q which reduces matrix A to  bidiagonal form.
    class procedure RectMatrixBDMultiplyByQ(const QP: TDouble2DArray;
      M: Integer;
      N: Integer;
      const TauQ: TDouble1DArray;
      var Z: TDouble2DArray;
      ZRows: Integer;
      ZColumns: Integer;
      FromTheRight: Boolean;
      DoTranspose: Boolean); static;

    //  Unpacking of the main and secondary diagonals
    // of bidiagonal decomposition of matrix A.
    class procedure RectMatrixBDUnpackDiagonals(const B: TDouble2DArray;
      M: Integer;
      N: Integer;
      var IsUpper: Boolean;
      var D: TDouble1DArray;
      var E: TDouble1DArray); static;

    // LQ decomposition of a rectangular matrix of size MxN
    class procedure RectMatrixLQ(var A: TDouble2DArray;
      M: Integer;
      N: Integer;
      var Tau: TDouble1DArray); static;

    // Partial unpacking of matrix Q from the LQ decomposition of a matrix A
    class procedure RectMatrixLQUnpackQ(const A: TDouble2DArray;
      M: Integer;
      N: Integer;
      const Tau: TDouble1DArray;
      QRows: Integer;
      var Q: TDouble2DArray); static;

    // Singular value decomposition of a rectangular matrix
    class function RectMatrixSVD(A: TDouble2DArray;
      M: Integer;
      N: Integer;
      UNeeded: Integer;
      VTNeeded: Integer;
      AdditionalMemory: Integer;
      var W: TDouble1DArray;
      var U: TDouble2DArray;
      var VT: TDouble2DArray): Boolean; static;

    // Internal working subrvarine for bidiagonal decomposition
    class function BidiagonalSVDDecompositionInternal(var D: TDouble1DArray;
      E: TDouble1DArray;
      N: Integer;
      IsUpper: Boolean;
      IsFractionalAccuracyRequired: Boolean;
      var U: TDouble2DArray;
      UStart: Integer;
      NRU: Integer;
      var C: TDouble2DArray;
      CStart: Integer;
      NCC: Integer;
      var VT: TDouble2DArray;
      VStart: Integer;
      NCVT: Integer): Boolean; static;

    class function ExtSignBDSQR(A: Double; B: Double): Double; static;

    class procedure Svd2X2(F: Double;
      G: Double;
      H: Double;
      var SSMIN: Double;
      var SSMAX: Double); static;

    class procedure SvdV2X2(F: Double;
      G: Double;
      H: Double;
      var SSMIN: Double;
      var SSMAX: Double;
      var SNR: Double;
      var CSR: Double;
      var SNL: Double;
      var CSL: Double); static;

    // Singular value decomposition of a bidiagonal matrix
    class function RectMatrixBDSVD(var D: TDouble1DArray;
      E: TDouble1DArray;
      N: Integer;
      IsUpper: Boolean;
      IsFractionalAccuracyRequired: Boolean;
      var U: TDouble2DArray;
      NRU: Integer;
      var C: TDouble2DArray;
      NCC: Integer;
      var VT: TDouble2DArray;
      NCVT: Integer): Boolean; static;

    // Generation of an elementary reflection transformation
    class procedure GenerateReflection(var X: TDouble1DArray;
      N: Integer;
      var Tau: Double); static;

    // Application of an elementary reflection to a rectangular matrix of size MxN
    class procedure ApplyReflectionFromTheLeft(var C: TDouble2DArray;
      Tau: Double;
      const V: TDouble1DArray;
      M1: Integer;
      M2: Integer;
      N1: Integer;
      N2: Integer;
      var WORK: TDouble1DArray); static;

    // Application of an elementary reflection to a rectangular matrix of size MxN
    class procedure ApplyReflectionFromTheRight(var C: TDouble2DArray;
      Tau: Double;
      const V: TDouble1DArray;
      M1: Integer;
      M2: Integer;
      N1: Integer;
      N2: Integer;
      var WORK: TDouble1DArray); static;

    // Application of a sequence of  elementary rotations to a matrix
    class procedure ApplyRotationsFromTheLeft(IsForward: Boolean;
      M1: Integer;
      M2: Integer;
      N1: Integer;
      N2: Integer;
      const C: TDouble1DArray;
      const S: TDouble1DArray;
      var A: TDouble2DArray;
      var WORK: TDouble1DArray); static;

    // Application of a sequence of  elementary rotations to a matrix
    class procedure ApplyRotationsFromTheRight(IsForward: Boolean;
      M1: Integer;
      M2: Integer;
      N1: Integer;
      N2: Integer;
      const C: TDouble1DArray;
      const S: TDouble1DArray;
      var A: TDouble2DArray;
      var WORK: TDouble1DArray); static;

    // The subrvarine generates the elementary rotation
    class procedure GenerateRotation(F: Double;
      G: Double;
      var CS: Double;
      var SN: Double;
      var R: Double); static;

    class procedure CalculateMoments(
      const X: TDouble1DArray;
      N: Integer;
      var Mean: Double;
      var Variance: Double;
      var Skewness: Double;
      var Kurtosis: Double); static;
  end;

implementation

class procedure PrincipalComponentsAnalysis.Add(VDst: PDouble; I11,
  I12: Integer; VSrc: PDouble; I21, I22: Integer; S: Real);
var
  I, C: LongInt;
begin
  Assert(I12 - I11 = I22 - I21, 'PrincipalComponentsAnalysis.Add: arrays of different size!');
  Inc(VDst, I11);
  Inc(VSrc, I21);

  // Generic pascal code
  C := I12 - I11;
  for I := 0 to C do
  begin
    VDst^ := VDst^ + S * VSrc^;
    Inc(VDst);
    Inc(VSrc);
  end;
end;

class procedure PrincipalComponentsAnalysis.ApplyReflectionFromTheLeft(
  var C: TDouble2DArray; Tau: Double; const V: TDouble1DArray; M1, M2, N1,
  N2: Integer; var WORK: TDouble1DArray);
var
  T: Double;
  I: Integer;
begin
  if (Tau = 0) or (N1 > N2) or (M1 > M2) then
    Exit;

  // w := C' * v
  I := N1;
  while I <= N2 do
  begin
    WORK[I] := 0;
    Inc(I);
  end;
  I := M1;
  while I <= M2 do
  begin
    T := V[I + 1 - M1];
    Add(@WORK[0], N1, N2, @C[I][0], N1, N2, T);
    Inc(I);
  end;

  // C := C - tau * v * w'
  I := M1;
  while I <= M2 do
  begin
    T := V[I - M1 + 1] * Tau;
    Sub(@C[I][0], N1, N2, @WORK[0], N1, N2, T);
    Inc(I);
  end;
end;

class procedure PrincipalComponentsAnalysis.ApplyReflectionFromTheRight(
  var C: TDouble2DArray; Tau: Double; const V: TDouble1DArray; M1, M2, N1,
  N2: Integer; var WORK: TDouble1DArray);
var
  T: Double;
  I: Integer;
  VM: Integer;
begin
  if (Tau = 0) or (N1 > N2) or (M1 > M2) then
      Exit;

  // w := C * v
  VM := N2 - N1 + 1;
  I := M1;
  while I <= M2 do
  begin
    T := DotProduct(@C[I][0], N1, N2, @V[0], 1, VM);
    WORK[I] := T;
    Inc(I);
  end;

  // C := C - w * v'
  I := M1;
  while I <= M2 do
  begin
    T := WORK[I] * Tau;
    Sub(@C[I][0], N1, N2, @V[0], 1, VM, T);
    Inc(I);
  end;
end;

class procedure PrincipalComponentsAnalysis.ApplyRotationsFromTheLeft(
  IsForward: Boolean; M1, M2, N1, N2: Integer; const C, S: TDouble1DArray;
  var A: TDouble2DArray; var WORK: TDouble1DArray);
var
  J: Integer;
  JP1: Integer;
  CTEMP: Double;
  STEMP: Double;
  TEMP: Double;
begin
  if (M1 > M2) or (N1 > N2) then
    Exit;

  // Form  P * A
  if IsForward then
  begin
    if N1 <> N2 then
    begin
      // Common case: N1<>N2
      J := M1;
      while J <= M2 - 1 do
      begin
        CTEMP := C[J - M1 + 1];
        STEMP := S[J - M1 + 1];
        if (CTEMP <> 1) or (STEMP <> 0) then
        begin
          JP1 := J + 1;
          Move(@WORK[0], N1, N2, @A[JP1][0], N1, N2, CTEMP);
          Sub(@WORK[0], N1, N2, @A[J][0], N1, N2, STEMP);
          Mul(@A[J][0], N1, N2, CTEMP);
          Add(@A[J][0], N1, N2, @A[JP1][0], N1, N2, STEMP);
          Move(@A[JP1][0], N1, N2, @WORK[0], N1, N2);
        end;
        Inc(J);
      end;
    end
    else
    begin
      // Special case: N1=N2
      J := M1;
      while J <= M2 - 1 do
      begin
        CTEMP := C[J - M1 + 1];
        STEMP := S[J - M1 + 1];
        if (CTEMP <> 1) or (STEMP <> 0) then
        begin
          TEMP := A[J + 1, N1];
          A[J + 1, N1] := CTEMP * TEMP - STEMP * A[J, N1];
          A[J, N1] := STEMP * TEMP + CTEMP * A[J, N1];
        end;
        Inc(J);
      end;
    end;
  end
  else
  begin
    if N1 <> N2 then
    begin
      // Common case: N1<>N2
      J := M2 - 1;
      while J >= M1 do
      begin
        CTEMP := C[J - M1 + 1];
        STEMP := S[J - M1 + 1];
        if (CTEMP <> 1) or (STEMP <> 0) then
        begin
          JP1 := J + 1;
          Move(@WORK[0], N1, N2, @A[JP1][0], N1, N2, CTEMP);
          Sub(@WORK[0], N1, N2, @A[J][0], N1, N2, STEMP);
          Mul(@A[J][0], N1, N2, CTEMP);
          Add(@A[J][0], N1, N2, @A[JP1][0], N1, N2, STEMP);
          Move(@A[JP1][0], N1, N2, @WORK[0], N1, N2);
        end;
        Dec(J);
      end;
    end
    else
    begin
      // Special case: N1=N2
      J := M2 - 1;
      while J >= M1 do
      begin
        CTEMP := C[J - M1 + 1];
        STEMP := S[J - M1 + 1];
        if (CTEMP <> 1) or (STEMP <> 0) then
        begin
          TEMP := A[J + 1, N1];
          A[J + 1, N1] := CTEMP * TEMP - STEMP * A[J, N1];
          A[J, N1] := STEMP * TEMP + CTEMP * A[J, N1];
        end;
        Dec(J);
      end;
    end;
  end;
end;

class procedure PrincipalComponentsAnalysis.ApplyRotationsFromTheRight(
  IsForward: Boolean; M1, M2, N1, N2: Integer; const C, S: TDouble1DArray;
  var A: TDouble2DArray; var WORK: TDouble1DArray);
var
  J: Integer;
  JP1: Integer;
  CTEMP: Double;
  STEMP: Double;
  TEMP: Double;
  i_: Integer;
begin
  // Form A * P'
  if IsForward then
  begin
    if M1 <> M2 then
    begin
      // Common case: M1<>M2
      J := N1;
      while J <= N2 - 1 do
      begin
        CTEMP := C[J - N1 + 1];
        STEMP := S[J - N1 + 1];
        if (CTEMP <> 1) or (STEMP <> 0) then
        begin
          JP1 := J + 1;
          for i_ := M1 to M2 do
              WORK[i_] := CTEMP * A[i_, JP1];
          for i_ := M1 to M2 do
              WORK[i_] := WORK[i_] - STEMP * A[i_, J];
          for i_ := M1 to M2 do
              A[i_, J] := CTEMP * A[i_, J];
          for i_ := M1 to M2 do
              A[i_, J] := A[i_, J] + STEMP * A[i_, JP1];
          for i_ := M1 to M2 do
              A[i_, JP1] := WORK[i_];
        end;
        Inc(J);
      end;
    end
    else
    begin
      // Special case: M1=M2
      J := N1;
      while J <= N2 - 1 do
      begin
        CTEMP := C[J - N1 + 1];
        STEMP := S[J - N1 + 1];
        if (CTEMP <> 1) or (STEMP <> 0) then
        begin
          TEMP := A[M1, J + 1];
          A[M1, J + 1] := CTEMP * TEMP - STEMP * A[M1, J];
          A[M1, J] := STEMP * TEMP + CTEMP * A[M1, J];
        end;
        Inc(J);
      end;
    end;
  end
  else
  begin
    if M1 <> M2 then
    begin
      // Common case: M1<>M2
      J := N2 - 1;
      while J >= N1 do
      begin
        CTEMP := C[J - N1 + 1];
        STEMP := S[J - N1 + 1];
        if (CTEMP <> 1) or (STEMP <> 0) then
        begin
          JP1 := J + 1;
          for i_ := M1 to M2 do
              WORK[i_] := CTEMP * A[i_, JP1];
          for i_ := M1 to M2 do
              WORK[i_] := WORK[i_] - STEMP * A[i_, J];
          for i_ := M1 to M2 do
              A[i_, J] := CTEMP * A[i_, J];
          for i_ := M1 to M2 do
              A[i_, J] := A[i_, J] + STEMP * A[i_, JP1];
          for i_ := M1 to M2 do
              A[i_, JP1] := WORK[i_];
        end;
        Dec(J);
      end;
    end
    else
    begin
      // Special case: M1=M2
      J := N2 - 1;
      while J >= N1 do
      begin
        CTEMP := C[J - N1 + 1];
        STEMP := S[J - N1 + 1];
        if (CTEMP <> 1) or (STEMP <> 0) then
        begin
          TEMP := A[M1, J + 1];
          A[M1, J + 1] := CTEMP * TEMP - STEMP * A[M1, J];
          A[M1, J] := STEMP * TEMP + CTEMP * A[M1, J];
        end;
        Dec(J);
      end;
    end;
  end;
end;

class function PrincipalComponentsAnalysis.BidiagonalSVDDecompositionInternal(
  var D: TDouble1DArray; E: TDouble1DArray; N: Integer; IsUpper,
  IsFractionalAccuracyRequired: Boolean; var U: TDouble2DArray; UStart,
  NRU: Integer; var C: TDouble2DArray; CStart, NCC: Integer;
  var VT: TDouble2DArray; VStart, NCVT: Integer): Boolean;
var
  I: Integer;
  IDIR: Integer;
  ISUB: Integer;
  ITER: Integer;
  J: Integer;
  LL: Integer;
  LLL: Integer;
  M: Integer;
  MAXIT: Integer;
  OLDLL: Integer;
  OLDM: Integer;
  ABSE: Double;
  ABSS: Double;
  COSL: Double;
  COSR: Double;
  CS: Double;
  EPS: Double;
  F: Double;
  G: Double;
  H: Double;
  MU: Double;
  OLDCS: Double;
  OLDSN: Double;
  R: Double;
  SHIFT: Double;
  SIGMN: Double;
  SIGMX: Double;
  SINL: Double;
  SINR: Double;
  SLL: Double;
  SMAX: Double;
  SMIN: Double;
  SMINL: Double;
  SMINLO: Double;
  SMINOA: Double;
  SN: Double;
  THRESH: Double;
  TOL: Double;
  TOLMUL: Double;
  UNFL: Double;
  WORK0: TDouble1DArray;
  WORK1: TDouble1DArray;
  WORK2: TDouble1DArray;
  WORK3: TDouble1DArray;
  MAXITR: Integer;
  MatrixSplitFlag: Boolean;
  IterFlag: Boolean;
  UTemp: TDouble1DArray;
  VTTemp: TDouble1DArray;
  CTemp: TDouble1DArray;
  ETemp: TDouble1DArray;
  RightSide: Boolean;
  FwdDir: Boolean;
  Tmp: Double;
  MM1: Integer;
  MM0: Integer;
  BChangeDir: Boolean;
  UEnd: Integer;
  CEnd: Integer;
  VEnd: Integer;
  i_: Integer;
begin
  E := Copy(E, 0, Length(E));
  Result := True;
  if N = 0 then
      Exit;
  if N = 1 then
  begin
    if D[1] < 0 then
    begin
      D[1] := -D[1];
      if NCVT > 0 then
          Mul(@VT[VStart][0], VStart, VStart + NCVT - 1, -1);
    end;
    Exit;
  end;

  // init
  SetLength(WORK0, N - 1 + 1);
  SetLength(WORK1, N - 1 + 1);
  SetLength(WORK2, N - 1 + 1);
  SetLength(WORK3, N - 1 + 1);
  UEnd := UStart + Max(NRU - 1, 0);
  VEnd := VStart + Max(NCVT - 1, 0);
  CEnd := CStart + Max(NCC - 1, 0);
  SetLength(UTemp, UEnd + 1);
  SetLength(VTTemp, VEnd + 1);
  SetLength(CTemp, CEnd + 1);
  MAXITR := 12;
  RightSide := True;
  FwdDir := True;

  // resize E from N-1 to N
  SetLength(ETemp, N + 1);
  I := 1;
  while I <= N - 1 do
  begin
    ETemp[I] := E[I];
    Inc(I);
  end;
  SetLength(E, N + 1);
  I := 1;
  while I <= N - 1 do
  begin
    E[I] := ETemp[I];
    Inc(I);
  end;
  E[N] := 0;
  IDIR := 0;

  // Get machine constants
  EPS := MACHINE_EPSILON;
  UNFL := MIN_REAL_NUMBER;

  // If matrix lower bidiagonal, rotate to be upper bidiagonal
  // by applying Givens rotations on the left
  if not IsUpper then
  begin
    I := 1;
    while I <= N - 1 do
    begin
      GenerateRotation(D[I], E[I], CS, SN, R);
      D[I] := R;
      E[I] := SN * D[I + 1];
      D[I + 1] := CS * D[I + 1];
      WORK0[I] := CS;
      WORK1[I] := SN;
      Inc(I);
    end;

    // Update singular vectors if desired
    if NRU > 0 then
        ApplyRotationsFromTheRight(FwdDir, UStart, UEnd, 1 + UStart - 1,
        N + UStart - 1, WORK0, WORK1, U, UTemp);
    if NCC > 0 then
        ApplyRotationsFromTheLeft(FwdDir, 1 + CStart - 1, N + CStart - 1,
        CStart, CEnd, WORK0, WORK1, C, CTemp);
  end;

  // Compute singular values to relative accuracy TOL
  // (By setting TOL to be negative, algorithm will compute
  // singular values to absolute accuracy ABS(TOL)*norm(input matrix))
  TOLMUL := Max(10, Min(100, Power(EPS, -0.125)));
  TOL := TOLMUL * EPS;
  if not IsFractionalAccuracyRequired then
      TOL := -TOL;

  // Compute approximate maximum, minimum singular values
  SMAX := 0;
  I := 1;
  while I <= N do
  begin
    SMAX := Max(SMAX, ABS(D[I]));
    Inc(I);
  end;
  I := 1;
  while I <= N - 1 do
  begin
    SMAX := Max(SMAX, ABS(E[I]));
    Inc(I);
  end;
  SMINL := 0;
  if TOL >= 0 then
  begin
    // Relative accuracy desired
    SMINOA := ABS(D[1]);
    if SMINOA <> 0 then
    begin
      MU := SMINOA;
      I := 2;
      while I <= N do
      begin
        MU := ABS(D[I]) * (MU / (MU + ABS(E[I - 1])));
        SMINOA := Min(SMINOA, MU);
        if SMINOA = 0 then
            Break;
        Inc(I);
      end;
    end;
    SMINOA := SMINOA / SQRT(N);
    THRESH := Max(TOL * SMINOA, MAXITR * N * N * UNFL);
  end
  else
  begin
    // Absolute accuracy desired
    THRESH := Max(ABS(TOL) * SMAX, MAXITR * N * N * UNFL);
  end;

  // Prepare for main iteration loop for the singular values
  // (MAXIT is the maximum number of passes through the inner
  // loop permitted before nonconvergence signalled.)
  MAXIT := MAXITR * N * N;
  ITER := 0;
  OLDLL := -1;
  OLDM := -1;

  // M points to last element of unconverged part of matrix
  M := N;

  // Begin main iteration loop
  while True do
  begin
    // Check for convergence or exceeding iteration count
    if M <= 1 then
    begin
      Break;
    end;
    if ITER > MAXIT then
        Exit(False);

    // Find diagonal block of matrix to work on
    if (TOL < 0) and (ABS(D[M]) <= THRESH) then
    begin
      D[M] := 0;
    end;
    SMAX := ABS(D[M]);
    SMIN := SMAX;
    MatrixSplitFlag := False;
    LLL := 1;
    LL := 0;
    while LLL <= M - 1 do
    begin
      LL := M - LLL;
      ABSS := ABS(D[LL]);
      ABSE := ABS(E[LL]);
      if (TOL < 0) and (ABSS <= THRESH) then
      begin
        D[LL] := 0;
      end;
      if ABSE <= THRESH then
      begin
        MatrixSplitFlag := True;
        Break;
      end;
      SMIN := Min(SMIN, ABSS);
      SMAX := Max(SMAX, Max(ABSS, ABSE));
      Inc(LLL);
    end;
    if not MatrixSplitFlag then
      LL := 0
    else
    begin
      // Matrix splits since E(LL) = 0
      E[LL] := 0;
      if LL = M - 1 then
      begin
        // Convergence of bottom singular value, return to top of loop
        M := M - 1;
        Continue;
      end;
    end;
    LL := LL + 1;

    // E(LL) through E(M-1) are nonzero, E(LL-1) is zero
    if LL = M - 1 then
    begin
      // 2 by 2 block, handle separately
      SvdV2X2(D[M - 1], E[M - 1], D[M], SIGMN, SIGMX, SINR, COSR, SINL, COSL);
      D[M - 1] := SIGMX;
      E[M - 1] := 0;
      D[M] := SIGMN;

      // Compute singular vectors, if desired
      if NCVT > 0 then
      begin
        MM0 := M + (VStart - 1);
        MM1 := M - 1 + (VStart - 1);
        Move(@VTTemp[0], VStart, VEnd, @VT[MM1][0], VStart, VEnd, COSR);
        Add(@VTTemp[0], VStart, VEnd, @VT[MM0][0], VStart, VEnd, SINR);
        Mul(@VT[MM0][0], VStart, VEnd, COSR);
        Sub(@VT[MM0][0], VStart, VEnd, @VT[MM1][0], VStart, VEnd, SINR);
        Move(@VT[MM1][0], VStart, VEnd, @VTTemp[0], VStart, VEnd);
      end;
      if NRU > 0 then
      begin
        MM0 := M + UStart - 1;
        MM1 := M - 1 + UStart - 1;
        for i_ := UStart to UEnd do
            UTemp[i_] := COSL * U[i_, MM1];
        for i_ := UStart to UEnd do
            UTemp[i_] := UTemp[i_] + SINL * U[i_, MM0];
        for i_ := UStart to UEnd do
            U[i_, MM0] := COSL * U[i_, MM0];
        for i_ := UStart to UEnd do
            U[i_, MM0] := U[i_, MM0] - SINL * U[i_, MM1];
        for i_ := UStart to UEnd do
            U[i_, MM1] := UTemp[i_];
      end;
      if NCC > 0 then
      begin
        MM0 := M + CStart - 1;
        MM1 := M - 1 + CStart - 1;
        Move(@CTemp[0], CStart, CEnd, @C[MM1][0], CStart, CEnd, COSL);
        Add(@CTemp[0], CStart, CEnd, @C[MM0][0], CStart, CEnd, SINL);
        Mul(@C[MM0][0], CStart, CEnd, COSL);
        Sub(@C[MM0][0], CStart, CEnd, @C[MM1][0], CStart, CEnd, SINL);
        Move(@C[MM1][0], CStart, CEnd, @CTemp[0], CStart, CEnd);
      end;
      M := M - 2;
      Continue;
    end;

    // If working on new submatrix, choose shift direction
    // (from larger end diagonal element towards smaller)
    //
    // Previously was
    // "if (LL>OLDM) or (M<OLDLL) then"
    // fixed thanks to Michael Rolle < m@rolle.name >
    // Very strange that LAPACK still contains it.
    BChangeDir := False;
    if (IDIR = 1) and (ABS(D[LL]) < 1.0E-3 * ABS(D[M])) then
    begin
      BChangeDir := True;
    end;
    if (IDIR = 2) and (ABS(D[M]) < 1.0E-3 * ABS(D[LL])) then
    begin
      BChangeDir := True;
    end;
    if (LL <> OLDLL) or (M <> OLDM) or BChangeDir then
    begin
      if ABS(D[LL]) >= ABS(D[M]) then
      begin
        // Chase bulge from top (big end) to bottom (small end)
        IDIR := 1;
      end
      else
      begin
        // Chase bulge from bottom (big end) to top (small end)
        IDIR := 2;
      end;
    end;

    // Apply convergence tests
    if IDIR = 1 then
    begin
      // Run convergence test in forward direction
      // First apply standard test to bottom of matrix
      if (ABS(E[M - 1]) <= ABS(TOL) * ABS(D[M])) or (TOL < 0) and
        (ABS(E[M - 1]) <= THRESH) then
      begin
        E[M - 1] := 0;
        Continue;
      end;
      if TOL >= 0 then
      begin
        // If relative accuracy desired,
        // apply convergence criterion forward
        MU := ABS(D[LL]);
        SMINL := MU;
        IterFlag := False;
        LLL := LL;
        while LLL <= M - 1 do
        begin
          if ABS(E[LLL]) <= TOL * MU then
          begin
            E[LLL] := 0;
            IterFlag := True;
            Break;
          end;
          SMINLO := SMINL;
          MU := ABS(D[LLL + 1]) * (MU / (MU + ABS(E[LLL])));
          SMINL := Min(SMINL, MU);
          Inc(LLL);
        end;
        if IterFlag then
        begin
          Continue;
        end;
      end;
    end
    else
    begin
      // Run convergence test in backward direction
      // First apply standard test to top of matrix
      if (ABS(E[LL]) <= ABS(TOL) * ABS(D[LL])) or (TOL < 0) and
        (ABS(E[LL]) <= THRESH) then
      begin
        E[LL] := 0;
        Continue;
      end;
      if TOL >= 0 then
      begin
        // If relative accuracy desired,
        // apply convergence criterion backward
        MU := ABS(D[M]);
        SMINL := MU;
        IterFlag := False;
        LLL := M - 1;
        while LLL >= LL do
        begin
          if ABS(E[LLL]) <= TOL * MU then
          begin
            E[LLL] := 0;
            IterFlag := True;
            Break;
          end;
          SMINLO := SMINL;
          MU := ABS(D[LLL]) * (MU / (MU + ABS(E[LLL])));
          SMINL := Min(SMINL, MU);
          Dec(LLL);
        end;
        if IterFlag then
        begin
          Continue;
        end;
      end;
    end;
    OLDLL := LL;
    OLDM := M;

    // Compute shift.  First, test if shifting would ruin relative
    // accuracy, and if so set the shift to zero.
    if (TOL >= 0) and (N * TOL * (SMINL / SMAX) <= Max(EPS, 0.01 * TOL)) then
    begin
      // Use a zero shift to avoid loss of relative accuracy
      SHIFT := 0;
    end
    else
    begin
      // Compute the shift from 2-by-2 block at end of matrix
      if IDIR = 1 then
      begin
        SLL := ABS(D[LL]);
        Svd2X2(D[M - 1], E[M - 1], D[M], SHIFT, R);
      end
      else
      begin
        SLL := ABS(D[M]);
        Svd2X2(D[LL], E[LL], D[LL + 1], SHIFT, R);
      end;

      // Test if shift negligible, and if so set to zero
      if SLL > 0 then
      begin
        if Sqr(SHIFT / SLL) < EPS then
        begin
          SHIFT := 0;
        end;
      end;
    end;

    // Increment iteration count
    ITER := ITER + M - LL;

    // If SHIFT = 0, do simplified QR iteration
    if SHIFT = 0 then
    begin
      if IDIR = 1 then
      begin
        // Chase bulge from top to bottom
        // Save cosines and sines for later singular vector updates
        CS := 1;
        OLDCS := 1;
        I := LL;
        while I <= M - 1 do
        begin
          GenerateRotation(D[I] * CS, E[I], CS, SN, R);
          if I > LL then
          begin
            E[I - 1] := OLDSN * R;
          end;
          GenerateRotation(OLDCS * R, D[I + 1] * SN, OLDCS, OLDSN, Tmp);
          D[I] := Tmp;
          WORK0[I - LL + 1] := CS;
          WORK1[I - LL + 1] := SN;
          WORK2[I - LL + 1] := OLDCS;
          WORK3[I - LL + 1] := OLDSN;
          Inc(I);
        end;
        H := D[M] * CS;
        D[M] := H * OLDCS;
        E[M - 1] := H * OLDSN;

        // Update singular vectors
        if NCVT > 0 then
            ApplyRotationsFromTheLeft(FwdDir, LL + VStart - 1, M + VStart - 1,
            VStart, VEnd, WORK0, WORK1, VT, VTTemp);
        if NRU > 0 then
            ApplyRotationsFromTheRight(FwdDir, UStart, UEnd, LL + UStart - 1,
            M + UStart - 1, WORK2, WORK3, U, UTemp);
        if NCC > 0 then
            ApplyRotationsFromTheLeft(FwdDir, LL + CStart - 1, M + CStart - 1,
            CStart, CEnd, WORK2, WORK3, C, CTemp);

        // Test convergence
        if ABS(E[M - 1]) <= THRESH then
            E[M - 1] := 0;
      end
      else
      begin
        // Chase bulge from bottom to top
        // Save cosines and sines for later singular vector updates
        CS := 1;
        OLDCS := 1;
        I := M;
        while I >= LL + 1 do
        begin
          GenerateRotation(D[I] * CS, E[I - 1], CS, SN, R);
          if I < M then
              E[I] := OLDSN * R;
          GenerateRotation(OLDCS * R, D[I - 1] * SN, OLDCS, OLDSN, Tmp);
          D[I] := Tmp;
          WORK0[I - LL] := CS;
          WORK1[I - LL] := -SN;
          WORK2[I - LL] := OLDCS;
          WORK3[I - LL] := -OLDSN;
          Dec(I);
        end;
        H := D[LL] * CS;
        D[LL] := H * OLDCS;
        E[LL] := H * OLDSN;

        // Update singular vectors
        if NCVT > 0 then
            ApplyRotationsFromTheLeft(not FwdDir, LL + VStart - 1,
            M + VStart - 1, VStart, VEnd, WORK2, WORK3, VT, VTTemp);
        if NRU > 0 then
            ApplyRotationsFromTheRight(not FwdDir, UStart, UEnd,
            LL + UStart - 1, M + UStart - 1, WORK0, WORK1, U, UTemp);
        if NCC > 0 then
            ApplyRotationsFromTheLeft(not FwdDir, LL + CStart - 1,
            M + CStart - 1, CStart, CEnd, WORK0, WORK1, C, CTemp);

        // Test convergence
        if ABS(E[LL]) <= THRESH then
            E[LL] := 0;
      end;
    end
    else
    begin
      // Use nonzero shift
      if IDIR = 1 then
      begin
        // Chase bulge from top to bottom
        // Save cosines and sines for later singular vector updates
        F := (ABS(D[LL]) - SHIFT) * (ExtSignBDSQR(1, D[LL]) + SHIFT / D[LL]);
        G := E[LL];
        I := LL;
        while I <= M - 1 do
        begin
          GenerateRotation(F, G, COSR, SINR, R);
          if I > LL then
              E[I - 1] := R;
          F := COSR * D[I] + SINR * E[I];
          E[I] := COSR * E[I] - SINR * D[I];
          G := SINR * D[I + 1];
          D[I + 1] := COSR * D[I + 1];
          GenerateRotation(F, G, COSL, SINL, R);
          D[I] := R;
          F := COSL * E[I] + SINL * D[I + 1];
          D[I + 1] := COSL * D[I + 1] - SINL * E[I];
          if I < M - 1 then
          begin
            G := SINL * E[I + 1];
            E[I + 1] := COSL * E[I + 1];
          end;
          WORK0[I - LL + 1] := COSR;
          WORK1[I - LL + 1] := SINR;
          WORK2[I - LL + 1] := COSL;
          WORK3[I - LL + 1] := SINL;
          Inc(I);
        end;
        E[M - 1] := F;

        // Update singular vectors
        if NCVT > 0 then
            ApplyRotationsFromTheLeft(FwdDir, LL + VStart - 1, M + VStart - 1,
            VStart, VEnd, WORK0, WORK1, VT, VTTemp);
        if NRU > 0 then
            ApplyRotationsFromTheRight(FwdDir, UStart, UEnd, LL + UStart - 1,
            M + UStart - 1, WORK2, WORK3, U, UTemp);
        if NCC > 0 then
            ApplyRotationsFromTheLeft(FwdDir, LL + CStart - 1, M + CStart - 1,
            CStart, CEnd, WORK2, WORK3, C, CTemp);

        // Test convergence
        if ABS(E[M - 1]) <= THRESH then
            E[M - 1] := 0;
      end
      else
      begin
        // Chase bulge from bottom to top
        // Save cosines and sines for later singular vector updates
        F := (ABS(D[M]) - SHIFT) * (ExtSignBDSQR(1, D[M]) + SHIFT / D[M]);
        G := E[M - 1];
        I := M;
        while I >= LL + 1 do
        begin
          GenerateRotation(F, G, COSR, SINR, R);
          if I < M then
          begin
            E[I] := R;
          end;
          F := COSR * D[I] + SINR * E[I - 1];
          E[I - 1] := COSR * E[I - 1] - SINR * D[I];
          G := SINR * D[I - 1];
          D[I - 1] := COSR * D[I - 1];
          GenerateRotation(F, G, COSL, SINL, R);
          D[I] := R;
          F := COSL * E[I - 1] + SINL * D[I - 1];
          D[I - 1] := COSL * D[I - 1] - SINL * E[I - 1];
          if I > LL + 1 then
          begin
            G := SINL * E[I - 2];
            E[I - 2] := COSL * E[I - 2];
          end;
          WORK0[I - LL] := COSR;
          WORK1[I - LL] := -SINR;
          WORK2[I - LL] := COSL;
          WORK3[I - LL] := -SINL;
          Dec(I);
        end;
        E[LL] := F;

        // Test convergence
        if ABS(E[LL]) <= THRESH then
            E[LL] := 0;

        // Update singular vectors if desired
        if NCVT > 0 then
            ApplyRotationsFromTheLeft(not FwdDir, LL + VStart - 1,
            M + VStart - 1, VStart, VEnd, WORK2, WORK3, VT, VTTemp);
        if NRU > 0 then
            ApplyRotationsFromTheRight(not FwdDir, UStart, UEnd,
            LL + UStart - 1, M + UStart - 1, WORK0, WORK1, U, UTemp);
        if NCC > 0 then
            ApplyRotationsFromTheLeft(not FwdDir, LL + CStart - 1,
            M + CStart - 1, CStart, CEnd, WORK0, WORK1, C, CTemp);
      end;
    end;

    // QR iteration finished, go back and check convergence
    Continue;
  end;

  // All singular values converged, so make them positive
  I := 1;
  while I <= N do
  begin
    if D[I] < 0 then
    begin
      D[I] := -D[I];

      // Change sign of singular vectors, if desired
      if NCVT > 0 then
          Mul(@VT[I + VStart - 1][0], VStart, VEnd, -1);
    end;
    Inc(I);
  end;

  // Sort the singular values into decreasing order (insertion sort on
  // singular values, but only one transposition per singular vector)
  I := 1;
  while I <= N - 1 do
  begin
    // Scan for smallest D(I)
    ISUB := 1;
    SMIN := D[1];
    J := 2;
    while J <= N + 1 - I do
    begin
      if D[J] <= SMIN then
      begin
        ISUB := J;
        SMIN := D[J];
      end;
      Inc(J);
    end;
    if ISUB <> N + 1 - I then
    begin
      // Swap singular values and vectors
      D[ISUB] := D[N + 1 - I];
      D[N + 1 - I] := SMIN;
      if NCVT > 0 then
      begin
        J := N + 1 - I;
        Move(@VTTemp[0], VStart, VEnd, @VT[ISUB + VStart - 1][0], VStart, VEnd);
        Move(@VT[ISUB + VStart - 1][0], VStart, VEnd, @VT[J + VStart - 1][0],
          VStart, VEnd);
        Move(@VT[J + VStart - 1][0], VStart, VEnd, @VTTemp[0], VStart, VEnd);
      end;
      if NRU > 0 then
      begin
        J := N + 1 - I;
        for i_ := UStart to UEnd do
            UTemp[i_] := U[i_, ISUB + UStart - 1];
        for i_ := UStart to UEnd do
            U[i_, ISUB + UStart - 1] := U[i_, J + UStart - 1];
        for i_ := UStart to UEnd do
            U[i_, J + UStart - 1] := UTemp[i_];
      end;
      if NCC > 0 then
          J := N + 1 - I;
      Move(@CTemp[0], CStart, CEnd, @C[ISUB + CStart - 1][0], CStart, CEnd);
      Move(@C[ISUB + CStart - 1][0], CStart, CEnd, @C[J + CStart - 1][0],
        CStart, CEnd);
      Move(@C[J + CStart - 1][0], CStart, CEnd, @CTemp[0], CStart, CEnd);
    end;
    Inc(I);
  end;
end;

class procedure PrincipalComponentsAnalysis.BuildBasis(const X: TDouble2DArray;
  NPoints: Integer;
  NVars: Integer;
  var Info: Integer;
  var S2: TDouble1DArray;
  var V: TDouble2DArray);
var
  A: TDouble2DArray;
  U: TDouble2DArray;
  VT: TDouble2DArray;
  M: TDouble1DArray;
  T: TDouble1DArray;
  I: Integer;
  J: Integer;
  Mean: Double;
  Variance: Double;
  Skewness: Double;
  Kurtosis: Double;
  i_: Integer;
begin

  //
  // Check input data
  //
  if (NPoints < 0) or (NVars < 1) then
  begin
    Info := -1;
    Exit;
  end;
  Info := 1;

  //
  // Special case: NPoints=0
  //
  if NPoints = 0 then
  begin
    SetLength(S2, NVars - 1 + 1);
    SetLength(V, NVars - 1 + 1, NVars - 1 + 1);
    I := 0;
    while I <= NVars - 1 do
    begin
      S2[I] := 0;
      Inc(I);
    end;
    I := 0;
    while I <= NVars - 1 do
    begin
      J := 0;
      while J <= NVars - 1 do
      begin
        if I = J then
        begin
          V[I, J] := 1;
        end
        else
        begin
          V[I, J] := 0;
        end;
        Inc(J);
      end;
      Inc(I);
    end;
    Exit;
  end;

  //
  // Calculate means
  //
  SetLength(M, NVars - 1 + 1);
  SetLength(T, NPoints - 1 + 1);
  J := 0;
  while J <= NVars - 1 do
  begin
    for i_ := 0 to NPoints - 1 do
    begin
      T[i_] := X[i_, J];
    end;
    CalculateMoments(T, NPoints, Mean, Variance, Skewness, Kurtosis);
    M[J] := Mean;
    Inc(J);
  end;

  //
  // Center, apply SVD, prepare varput
  //
  SetLength(A, Max(NPoints, NVars) - 1 + 1, NVars - 1 + 1);
  I := 0;
  while I <= NPoints - 1 do
  begin
    PrincipalComponentsAnalysis.Move(@A[I][0], 0, NVars - 1, @X[I][0], 0,
      NVars - 1);
    PrincipalComponentsAnalysis.Sub(@A[I][0], 0, NVars - 1, @M[0], 0,
      NVars - 1);
    Inc(I);
  end;
  I := NPoints;
  while I <= NVars - 1 do
  begin
    J := 0;
    while J <= NVars - 1 do
    begin
      A[I, J] := 0;
      Inc(J);
    end;
    Inc(I);
  end;
  if not RectMatrixSVD(A, Max(NPoints, NVars), NVars, 0, 1, 2, S2, U, VT) then
  begin
    Info := -4;
    Exit;
  end;
  if NPoints <> 1 then
  begin
    I := 0;
    while I <= NVars - 1 do
    begin
      S2[I] := Sqr(S2[I]) / (NPoints - 1);
      Inc(I);
    end;
  end;
  SetLength(V, NVars - 1 + 1, NVars - 1 + 1);
  CopyAndTranspose(VT, 0, NVars - 1, 0, NVars - 1, V, 0, NVars - 1, 0,
    NVars - 1);
end;

class procedure PrincipalComponentsAnalysis.CalculateMoments
  (const X: TDouble1DArray; N: Integer;
  var Mean, Variance, Skewness, Kurtosis: Double);
var
  I: Integer;
  V: Double;
  V1: Double;
  V2: Double;
  StdDev: Double;
begin
  Mean := 0;
  Variance := 0;
  Skewness := 0;
  Kurtosis := 0;
  StdDev := 0;
  if N <= 0 then
  begin
    Exit;
  end;

  //
  // Mean
  //
  I := 0;
  while I <= N - 1 do
  begin
    Mean := Mean + X[I];
    Inc(I);
  end;
  Mean := Mean / N;

  //
  // Variance (using corrected two-pass algorithm)
  //
  if N <> 1 then
  begin
    V1 := 0;
    I := 0;
    while I <= N - 1 do
    begin
      V1 := V1 + Sqr(X[I] - Mean);
      Inc(I);
    end;
    V2 := 0;
    I := 0;
    while I <= N - 1 do
    begin
      V2 := V2 + (X[I] - Mean);
      Inc(I);
    end;
    V2 := Sqr(V2) / N;
    Variance := (V1 - V2) / (N - 1);
    if Variance < 0 then
    begin
      Variance := 0;
    end;
    StdDev := Sqrt(Variance);
  end;

  //
  // Skewness and kurtosis
  //
  if StdDev <> 0 then
  begin
    I := 0;
    while I <= N - 1 do
    begin
      V := (X[I] - Mean) / StdDev;
      V2 := Sqr(V);
      Skewness := Skewness + V2 * V;
      Kurtosis := Kurtosis + Sqr(V2);
      Inc(I);
    end;
    Skewness := Skewness / N;
    Kurtosis := Kurtosis / N - 3;
  end;
end;

class procedure PrincipalComponentsAnalysis.CopyAndTranspose
  (const A: TDouble2DArray; IS1, IS2, JS1,
  JS2: Integer; var B: TDouble2DArray; ID1, ID2, JD1, JD2: Integer);
var
  ISRC: Integer;
  JDST: Integer;
  i_: Integer;
  i1_: Integer;
begin
  if (IS1 > IS2) or (JS1 > JS2) then
  begin
    Exit;
  end;
  Assert(IS2 - IS1 = JD2 - JD1,
    'PrincipalComponentsAnalysis.CopyAndTranspose: different sizes!');
  Assert(JS2 - JS1 = ID2 - ID1,
    'PrincipalComponentsAnalysis.CopyAndTranspose: different sizes!');
  ISRC := IS1;
  while ISRC <= IS2 do
  begin
    JDST := ISRC - IS1 + JD1;
    i1_ := (JS1) - (ID1);
    for i_ := ID1 to ID2 do
        B[i_, JDST] := A[ISRC, i_ + i1_];
    Inc(ISRC);
  end;
end;

class procedure PrincipalComponentsAnalysis.CopyMatrix(const A: TDouble2DArray;
  IS1, IS2, JS1, JS2: Integer; var B: TDouble2DArray; ID1, ID2, JD1,
  JD2: Integer);
var
  ISRC: Integer;
  IDST: Integer;
begin
  if (IS1 > IS2) or (JS1 > JS2) then
      Exit;
  Assert(IS2 - IS1 = ID2 - ID1,
    'PrincipalComponentsAnalysis.CopyMatrix: different sizes!');
  Assert(JS2 - JS1 = JD2 - JD1,
    'PrincipalComponentsAnalysis.CopyMatrix: different sizes!');
  ISRC := IS1;
  while ISRC <= IS2 do
  begin
    IDST := ISRC - IS1 + ID1;
    Move(@B[IDST][0], JD1, JD2, @A[ISRC][0], JS1, JS2);
    Inc(ISRC);
  end;
end;

class function PrincipalComponentsAnalysis.DotProduct(V1: PDouble; I11,
  I12: Integer; V2: PDouble; I21, I22: Integer): Double;
var
  I, C: LongInt;
begin
  Assert(I12 - I11 = I22 - I21,
    'PrincipalComponentsAnalysis.DotProduct: arrays of different size!');
  Inc(V1, I11);
  Inc(V2, I21);

  // Generic pascal code
  C := I12 - I11;
  Result := 0;
  for I := 0 to C do
  begin
    Result := Result + V1^ * V2^;
    Inc(V1);
    Inc(V2);
  end;
end;

class function PrincipalComponentsAnalysis.ExtSignBDSQR(A, B: Double): Double;
begin
  if B >= 0 then
    Result := Abs(A)
  else
    Result := -Abs(A);
end;

class procedure PrincipalComponentsAnalysis.GenerateReflection(
  var X: TDouble1DArray; N: Integer; var Tau: Double);
var
  J: Integer;
  Alpha: Double;
  XNORM: Double;
  V: Double;
  Beta: Double;
  MX: Double;
begin
  // Executable Statements ..
  if N <= 1 then
  begin
    Tau := 0;
    Exit;
  end;

  // XNORM = DNRM2( N-1, X, INCX )
  Alpha := X[1];
  MX := 0;
  J := 2;
  while J <= N do
  begin
    MX := Max(Abs(X[J]), MX);
    Inc(J);
  end;
  XNORM := 0;
  if MX <> 0 then
  begin
    J := 2;
    while J <= N do
    begin
      XNORM := XNORM + Sqr(X[J] / MX);
      Inc(J);
    end;
    XNORM := Sqrt(XNORM) * MX;
  end;
  if XNORM = 0 then
  begin
    // H  =  I
    Tau := 0;
    Exit;
  end;

  // general case
  MX := Max(Abs(Alpha), Abs(XNORM));
  Beta := -MX * Sqrt(Sqr(Alpha / MX) + Sqr(XNORM / MX));
  if Alpha < 0 then
      Beta := -Beta;
  Tau := (Beta - Alpha) / Beta;
  V := 1 / (Alpha - Beta);
  Mul(@X[0], 2, N, V);
  X[1] := Beta;
end;

class procedure PrincipalComponentsAnalysis.GenerateRotation(F, G: Double;
  var CS, SN, R: Double);
var
  F1: Double;
  G1: Double;
begin
  if G = 0 then
  begin
    CS := 1;
    SN := 0;
    R := F;
  end
  else
  begin
    if F = 0 then
    begin
      CS := 0;
      SN := 1;
      R := G;
    end
    else
    begin
      F1 := F;
      G1 := G;
      R := SQRT(Sqr(F1) + Sqr(G1));
      CS := F1 / R;
      SN := G1 / R;
      if (ABS(F) > ABS(G)) and (CS < 0) then
      begin
        CS := -CS;
        SN := -SN;
        R := -R;
      end;
    end;
  end;
end;

class procedure PrincipalComponentsAnalysis.InplaceTranspose(
  var A: TDouble2DArray; I1, I2, J1, J2: Integer; var WORK: TDouble1DArray);
var
  I: Integer;
  J: Integer;
  IPS: Integer;
  JPS: Integer;
  L: Integer;
  i_: Integer;
  i1_: Integer;
begin
  if (I1 > I2) or (J1 > J2) then
      Exit;
  Assert(I1 - I2 = J1 - J2,
    'PrincipalComponentsAnalysis.InplaceTranspose: incorrect array size!');
  I := I1;
  while I <= I2 - 1 do
  begin
    J := J1 + I - I1;
    IPS := I + 1;
    JPS := J1 + IPS - I1;
    L := I2 - I;
    i1_ := (IPS) - (1);
    for i_ := 1 to L do
        WORK[i_] := A[i_ + i1_, J];
    i1_ := (JPS) - (IPS);
    for i_ := IPS to I2 do
        A[i_, J] := A[I, i_ + i1_];
    Move(@A[I][0], JPS, J2, @WORK[0], 1, L);
    Inc(I);
  end;
end;

class procedure PrincipalComponentsAnalysis.MatrixMatrixMultiply(
  const A: TDouble2DArray; AI1, AI2, AJ1, AJ2: Integer; TransA: Boolean;
  const B: TDouble2DArray; BI1, BI2, BJ1, BJ2: Integer; TransB: Boolean;
  Alpha: Double; var C: TDouble2DArray; CI1, CI2, CJ1, CJ2: Integer;
  Beta: Double; var WORK: TDouble1DArray);
var
  ARows: Integer;
  ACols: Integer;
  BRows: Integer;
  BCols: Integer;
  CRows: Integer;
  CCols: Integer;
  I: Integer;
  J: Integer;
  K: Integer;
  L: Integer;
  R: Integer;
  V: Double;
  i_: Integer;
  i1_: Integer;
begin
  // Setup
  if not TransA then
  begin
    ARows := AI2 - AI1 + 1;
    ACols := AJ2 - AJ1 + 1;
  end
  else
  begin
    ARows := AJ2 - AJ1 + 1;
    ACols := AI2 - AI1 + 1;
  end;
  if not TransB then
  begin
    BRows := BI2 - BI1 + 1;
    BCols := BJ2 - BJ1 + 1;
  end
  else
  begin
    BRows := BJ2 - BJ1 + 1;
    BCols := BI2 - BI1 + 1;
  end;
  Assert(ACols = BRows,
    'PrincipalComponentsAnalysis.MatrixMatrixMultiply: incorrect matrix sizes!');
  if (ARows <= 0) or (ACols <= 0) or (BRows <= 0) or (BCols <= 0) then
  begin
    Exit;
  end;
  CRows := ARows;
  CCols := BCols;

  // Test WORK
  I := Max(ARows, ACols);
  I := Max(BRows, I);
  I := Max(I, BCols);
  WORK[1] := 0;
  WORK[I] := 0;

  // Prepare C
  if Beta = 0 then
  begin
    I := CI1;
    while I <= CI2 do
    begin
      J := CJ1;
      while J <= CJ2 do
      begin
        C[I, J] := 0;
        Inc(J);
      end;
      Inc(I);
    end;
  end
  else
  begin
    I := CI1;
    while I <= CI2 do
    begin
      Mul(@C[I][0], CJ1, CJ2, Beta);
      Inc(I);
    end;
  end;

  // A*B
  if not TransA and not TransB then
  begin
    L := AI1;
    while L <= AI2 do
    begin
      R := BI1;
      while R <= BI2 do
      begin
        V := Alpha * A[L, AJ1 + R - BI1];
        K := CI1 + L - AI1;
        Add(@C[K][0], CJ1, CJ2, @B[R][0], BJ1, BJ2, V);
        Inc(R);
      end;
      Inc(L);
    end;
    Exit;
  end;

  // A*B'
  if not TransA and TransB then
  begin
    if ARows * ACols < BRows * BCols then
    begin
      R := BI1;
      while R <= BI2 do
      begin
        L := AI1;
        while L <= AI2 do
        begin
          V := DotProduct(@A[L][0], AJ1, AJ2, @B[R][0], BJ1, BJ2);
          C[CI1 + L - AI1, CJ1 + R - BI1] := C[CI1 + L - AI1, CJ1 + R - BI1]
            + Alpha * V;
          Inc(L);
        end;
        Inc(R);
      end;
      Exit;
    end
    else
    begin
      L := AI1;
      while L <= AI2 do
      begin
        R := BI1;
        while R <= BI2 do
        begin
          V := DotProduct(@A[L][0], AJ1, AJ2, @B[R][0], BJ1, BJ2);
          C[CI1 + L - AI1, CJ1 + R - BI1] := C[CI1 + L - AI1, CJ1 + R - BI1]
            + Alpha * V;
          Inc(R);
        end;
        Inc(L);
      end;
      Exit;
    end;
  end;

  // A'*B
  if TransA and not TransB then
  begin
    L := AJ1;
    while L <= AJ2 do
    begin
      R := BI1;
      while R <= BI2 do
      begin
        V := Alpha * A[AI1 + R - BI1, L];
        K := CI1 + L - AJ1;
        Add(@C[K][0], CJ1, CJ2, @B[R][0], BJ1, BJ2, V);
        Inc(R);
      end;
      Inc(L);
    end;
    Exit;
  end;

  // A'*B'
  if TransA and TransB then
  begin
    if ARows * ACols < BRows * BCols then
    begin
      R := BI1;
      while R <= BI2 do
      begin
        I := 1;
        while I <= CRows do
        begin
          WORK[I] := 0.0;
          Inc(I);
        end;
        L := AI1;
        K := 0;
        while L <= AI2 do
        begin
          V := Alpha * B[R, BJ1 + L - AI1];
          K := CJ1 + R - BI1;
          Add(@WORK[0], 1, CRows, @A[L][0], AJ1, AJ2, V);
          Inc(L);
        end;
        i1_ := (1) - (CI1);
        for i_ := CI1 to CI2 do
          C[i_, K] := C[i_, K] + WORK[i_ + i1_];
        Inc(R);
      end;
      Exit;
    end
    else
    begin
      L := AJ1;
      while L <= AJ2 do
      begin
        K := AI2 - AI1 + 1;
        i1_ := (AI1) - (1);
        for i_ := 1 to K do
        begin
          WORK[i_] := A[i_ + i1_, L];
        end;
        R := BI1;
        while R <= BI2 do
        begin
          V := DotProduct(@WORK[0], 1, K, @B[R][0], BJ1, BJ2);
          C[CI1 + L - AJ1, CJ1 + R - BI1] := C[CI1 + L - AJ1, CJ1 + R - BI1]
            + Alpha * V;
          Inc(R);
        end;
        Inc(L);
      end;
      Exit;
    end;
  end;
end;

class procedure PrincipalComponentsAnalysis.Move(VDst: PDouble; I11,
  I12: Integer; VSrc: PDouble; I21, I22: Integer; S: Double);
var
    I, C: LongInt;
begin
  Assert(I12 - I11 = I22 - I21,
    'PrincipalComponentsAnalysis.Move: arrays of different size!');
  Inc(VDst, I11);
  Inc(VSrc, I21);

  // Generic pascal code
  C := I12 - I11;
  for I := 0 to C do
  begin
    VDst^ := S * VSrc^;
    Inc(VDst);
    Inc(VSrc);
  end;
end;

class procedure PrincipalComponentsAnalysis.Move(VDst: PDouble;
  I11, I12: Integer; VSrc: PDouble;
  I21,
  I22: Integer);
var
  C: LongInt;
begin
  Assert(I12 - I11 = I22 - I21,
    'PrincipalComponentsAnalysis.Move: arrays of different size!');
  Inc(VDst, I11);
  Inc(VSrc, I21);

  C := I12 - I11 + 1;
  System.Move(VSrc^, VDst^, SizeOf(Double) * C);
end;

class procedure PrincipalComponentsAnalysis.Mul(VOp: PDouble; I1, I2: Integer;
  S: Double);
var
  I, C: LongInt;
begin
  Inc(VOp, I1);
  C := I2 - I1;
  for I := 0 to C do
  begin
    VOp^ := S * VOp^;
    Inc(VOp);
  end;
end;

class procedure PrincipalComponentsAnalysis.RectMatrixBD(var A: TDouble2DArray;
  M, N: Integer; var TauQ, TauP: TDouble1DArray);
var
  WORK: TDouble1DArray;
  T: TDouble1DArray;
  MaxMN: Integer;
  I: Integer;
  LTau: Double;
  i_: Integer;
  i1_: Integer;
begin
  // Prepare
  if (N <= 0) or (M <= 0) then
      Exit;
  MaxMN := Max(M, N);
  SetLength(WORK, MaxMN + 1);
  SetLength(T, MaxMN + 1);
  if M >= N then
  begin
    SetLength(TauQ, N - 1 + 1);
    SetLength(TauP, N - 1 + 1);
  end
  else
  begin
    SetLength(TauQ, M - 1 + 1);
    SetLength(TauP, M - 1 + 1);
  end;
  if M >= N then
  begin
    // Reduce to upper bidiagonal form
    I := 0;
    while I <= N - 1 do
    begin
      // Generate elementary reflector H(i) to annihilate A(i+1:m-1,i)
      i1_ := (I) - (1);
      for i_ := 1 to M - I do
          T[i_] := A[i_ + i1_, I];
      GenerateReflection(T, M - I, LTau);
      TauQ[I] := LTau;
      i1_ := (1) - (I);
      for i_ := I to M - 1 do
          A[i_, I] := T[i_ + i1_];
      T[1] := 1;

      // Apply H(i) to A(i:m-1,i+1:n-1) from the left
      ApplyReflectionFromTheLeft(A, LTau, T, I, M - 1, I + 1, N - 1, WORK);
      if I < N - 1 then
      begin
        // Generate elementary reflector G(i) to annihilate
        // A(i,i+2:n-1)
        Move(@T[0], 1, N - I - 1, @A[I][0], I + 1, N - 1);
        GenerateReflection(T, N - 1 - I, LTau);
        TauP[I] := LTau;
        Move(@A[I][0], I + 1, N - 1, @T[0], 1, N - 1 - I);
        T[1] := 1;

        //
        // Apply G(i) to A(i+1:m-1,i+1:n-1) from the right
        //
        ApplyReflectionFromTheRight(A, LTau, T, I + 1, M - 1, I + 1,
          N - 1, WORK);
      end
      else
      begin
        TauP[I] := 0;
      end;
      Inc(I);
    end;
  end
  else
  begin

    //
    // Reduce to lower bidiagonal form
    //
    I := 0;
    while I <= M - 1 do
    begin

      // Generate elementary reflector G(i) to annihilate A(i,i+1:n-1)
      Move(@T[0], 1, N - I, @A[I][0], I, N - 1);
      GenerateReflection(T, N - I, LTau);
      TauP[I] := LTau;
      Move(@A[I][0], I, N - 1, @T[0], 1, N - I);
      T[1] := 1;

      // Apply G(i) to A(i+1:m-1,i:n-1) from the right
      ApplyReflectionFromTheRight(A, LTau, T, I + 1, M - 1, I, N - 1, WORK);
      if I < M - 1 then
      begin

        // Generate elementary reflector H(i) to annihilate
        // A(i+2:m-1,i)
        i1_ := (I + 1) - (1);
        for i_ := 1 to M - 1 - I do
        begin
          T[i_] := A[i_ + i1_, I];
        end;
        GenerateReflection(T, M - 1 - I, LTau);
        TauQ[I] := LTau;
        i1_ := (1) - (I + 1);
        for i_ := I + 1 to M - 1 do
        begin
          A[i_, I] := T[i_ + i1_];
        end;
        T[1] := 1;

        // Apply H(i) to A(i+1:m-1,i+1:n-1) from the left
        ApplyReflectionFromTheLeft(A, LTau, T, I + 1, M - 1, I + 1,
          N - 1, WORK);
      end
      else
      begin
        TauQ[I] := 0;
      end;
      Inc(I);
    end;
  end;
end;

class procedure PrincipalComponentsAnalysis.RectMatrixBDMultiplyByP(
  const QP: TDouble2DArray; M, N: Integer; const TauP: TDouble1DArray;
  var Z: TDouble2DArray; ZRows, ZColumns: Integer; FromTheRight,
  DoTranspose: Boolean);
var
  I: Integer;
  V: TDouble1DArray;
  WORK: TDouble1DArray;
  MX: Integer;
  I1: Integer;
  I2: Integer;
  IStep: Integer;
begin
  if (M <= 0) or (N <= 0) or (ZRows <= 0) or (ZColumns <= 0) then
    Exit;
  Assert(FromTheRight and (ZColumns = N) or not FromTheRight and (ZRows = N),
    'PrincipalComponentsAnalysis.RectMatrixBDMultiplyByP: incorrect Z size!');

  // init
  MX := Max(M, N);
  MX := Max(MX, ZRows);
  MX := Max(MX, ZColumns);
  SetLength(V, MX + 1);
  SetLength(WORK, MX + 1);
  SetLength(V, MX + 1);
  SetLength(WORK, MX + 1);
  if M >= N then
  begin

    // setup
    if FromTheRight then
    begin
      I1 := N - 2;
      I2 := 0;
      IStep := -1;
    end
    else
    begin
      I1 := 0;
      I2 := N - 2;
      IStep := +1;
    end;
    if not DoTranspose then
    begin
      I := I1;
      I1 := I2;
      I2 := I;
      IStep := -IStep;
    end;

    // Process
    if N - 1 > 0 then
    begin
      I := I1;
      repeat
        Move(@V[0], 1, N - 1 - I, @QP[I][0], I + 1, N - 1);
        V[1] := 1;
        if FromTheRight then
        begin
          ApplyReflectionFromTheRight(Z, TauP[I], V, 0, ZRows - 1, I + 1,
            N - 1, WORK);
        end
        else
        begin
          ApplyReflectionFromTheLeft(Z, TauP[I], V, I + 1, N - 1, 0,
            ZColumns - 1, WORK);
        end;
        I := I + IStep;
      until I = I2 + IStep;
    end;
  end
  else
  begin
    // setup
    if FromTheRight then
    begin
      I1 := M - 1;
      I2 := 0;
      IStep := -1;
    end
    else
    begin
      I1 := 0;
      I2 := M - 1;
      IStep := +1;
    end;
    if not DoTranspose then
    begin
      I := I1;
      I1 := I2;
      I2 := I;
      IStep := -IStep;
    end;

    // Process
    I := I1;
    repeat
      Move(@V[0], 1, N - I, @QP[I][0], I, N - 1);
      V[1] := 1;
      if FromTheRight then
      begin
        ApplyReflectionFromTheRight(Z, TauP[I], V, 0, ZRows - 1, I,
          N - 1, WORK);
      end
      else
      begin
        ApplyReflectionFromTheLeft(Z, TauP[I], V, I, N - 1, 0,
          ZColumns - 1, WORK);
      end;
      I := I + IStep;
    until I = I2 + IStep;
  end;
end;

class procedure PrincipalComponentsAnalysis.RectMatrixBDMultiplyByQ(
  const QP: TDouble2DArray; M, N: Integer; const TauQ: TDouble1DArray;
  var Z: TDouble2DArray; ZRows, ZColumns: Integer; FromTheRight,
  DoTranspose: Boolean);
var
  I: Integer;
  I1: Integer;
  I2: Integer;
  IStep: Integer;
  V: TDouble1DArray;
  WORK: TDouble1DArray;
  MX: Integer;
  i_: Integer;
  i1_: Integer;
begin
  if (M <= 0) or (N <= 0) or (ZRows <= 0) or (ZColumns <= 0) then
      Exit;
  Assert(FromTheRight and (ZColumns = M) or not FromTheRight and (ZRows = M),
    'PrincipalComponentsAnalysis.RectMatrixBDMultiplyByQ: incorrect Z size!');

  // init
  MX := Max(M, N);
  MX := Max(MX, ZRows);
  MX := Max(MX, ZColumns);
  SetLength(V, MX + 1);
  SetLength(WORK, MX + 1);
  if M >= N then
  begin
    // setup
    if FromTheRight then
    begin
      I1 := 0;
      I2 := N - 1;
      IStep := +1;
    end
    else
    begin
      I1 := N - 1;
      I2 := 0;
      IStep := -1;
    end;
    if DoTranspose then
    begin
      I := I1;
      I1 := I2;
      I2 := I;
      IStep := -IStep;
    end;

    // Process
    I := I1;
    repeat
      i1_ := (I) - (1);
      for i_ := 1 to M - I do
      begin
        V[i_] := QP[i_ + i1_, I];
      end;
      V[1] := 1;
      if FromTheRight then
      begin
        ApplyReflectionFromTheRight(Z, TauQ[I], V, 0, ZRows - 1, I,
          M - 1, WORK);
      end
      else
      begin
        ApplyReflectionFromTheLeft(Z, TauQ[I], V, I, M - 1, 0,
          ZColumns - 1, WORK);
      end;
      I := I + IStep;
    until I = I2 + IStep;
  end
  else
  begin

    // setup
    if FromTheRight then
    begin
      I1 := 0;
      I2 := M - 2;
      IStep := +1;
    end
    else
    begin
      I1 := M - 2;
      I2 := 0;
      IStep := -1;
    end;
    if DoTranspose then
    begin
      I := I1;
      I1 := I2;
      I2 := I;
      IStep := -IStep;
    end;

    // Process
    if M - 1 > 0 then
    begin
      I := I1;
      repeat
        i1_ := (I + 1) - (1);
        for i_ := 1 to M - I - 1 do
        begin
          V[i_] := QP[i_ + i1_, I];
        end;
        V[1] := 1;
        if FromTheRight then
            ApplyReflectionFromTheRight(Z, TauQ[I], V, 0, ZRows - 1, I + 1,
            M - 1, WORK)
        else
            ApplyReflectionFromTheLeft(Z, TauQ[I], V, I + 1, M - 1, 0,
            ZColumns - 1, WORK);
        I := I + IStep;
      until I = I2 + IStep;
    end;
  end;
end;

class function PrincipalComponentsAnalysis.RectMatrixBDSVD(var D: TDouble1DArray;
  E: TDouble1DArray; N: Integer; IsUpper, IsFractionalAccuracyRequired: Boolean;
  var U: TDouble2DArray; NRU: Integer; var C: TDouble2DArray; NCC: Integer;
  var VT: TDouble2DArray; NCVT: Integer): Boolean;
var
    D1 : TDouble1DArray;
    E1 : TDouble1DArray;
begin
    E := Copy(E, 0, Length(E));
    SetLength(D1, N+1);
    Move(@D1[0], 1, N, @D[0], 0, N-1);
    if N>1 then
    begin
        SetLength(E1, N-1+1);
        Move(@E1[0], 1, N-1, @E[0], 0, N-2);
    end;
    Result := BidiagonalSVDDecompositionInternal(D1, E1, N, IsUpper, IsFractionalAccuracyRequired, U, 0, NRU, C, 0, NCC, VT, 0, NCVT);
    Move(@D[0], 0, N-1, @D1[0], 1, N);
end;

class procedure PrincipalComponentsAnalysis.RectMatrixBDUnpackDiagonals(
  const B: TDouble2DArray; M, N: Integer; var IsUpper: Boolean; var D,
  E: TDouble1DArray);
var
  I: Integer;
begin
  IsUpper := M >= N;
  if (M <= 0) or (N <= 0) then
  begin
    Exit;
  end;
  if IsUpper then
  begin
    SetLength(D, N - 1 + 1);
    SetLength(E, N - 1 + 1);
    I := 0;
    while I <= N - 2 do
    begin
      D[I] := B[I, I];
      E[I] := B[I, I + 1];
      Inc(I);
    end;
    D[N - 1] := B[N - 1, N - 1];
  end
  else
  begin
    SetLength(D, M - 1 + 1);
    SetLength(E, M - 1 + 1);
    I := 0;
    while I <= M - 2 do
    begin
      D[I] := B[I, I];
      E[I] := B[I + 1, I];
      Inc(I);
    end;
    D[M - 1] := B[M - 1, M - 1];
  end;
end;

class procedure PrincipalComponentsAnalysis.RectMatrixBDUnpackPT(
  const QP: TDouble2DArray; M, N: Integer; const TauP: TDouble1DArray;
  PTRows: Integer; var PT: TDouble2DArray);
var
  I: Integer;
  J: Integer;
begin
  Assert(PTRows <= N,
    'PrincipalComponentsAnalysis.RectMatrixBDUnpackPT: PTRows>N!');
  Assert(PTRows >= 0,
    'PrincipalComponentsAnalysis.RectMatrixBDUnpackPT: PTRows<0!');
  if (M = 0) or (N = 0) or (PTRows = 0) then
  begin
    Exit;
  end;

  // prepare PT
  SetLength(PT, PTRows - 1 + 1, N - 1 + 1);
  I := 0;
  while I <= PTRows - 1 do
  begin
    J := 0;
    while J <= N - 1 do
    begin
      if I = J then
      begin
        PT[I, J] := 1;
      end
      else
      begin
        PT[I, J] := 0;
      end;
      Inc(J);
    end;
    Inc(I);
  end;

  // Calculate
  RectMatrixBDMultiplyByP(QP, M, N, TauP, PT, PTRows, N, True, True);
end;

class procedure PrincipalComponentsAnalysis.RectMatrixBDUnpackQ(
  const QP: TDouble2DArray; M, N: Integer; const TauQ: TDouble1DArray;
  QColumns: Integer; var Q: TDouble2DArray);
var
  I: Integer;
  J: Integer;
begin
  Assert(QColumns <= M,
    'PrincipalComponentsAnalysis.RectMatrixBDUnpackQ: QColumns>M!');
  Assert(QColumns >= 0,
    'PrincipalComponentsAnalysis.RectMatrixBDUnpackQ: QColumns<0!');
  if (M = 0) or (N = 0) or (QColumns = 0) then
      Exit;

  // prepare Q
  SetLength(Q, M - 1 + 1, QColumns - 1 + 1);
  I := 0;
  while I <= M - 1 do
  begin
    J := 0;
    while J <= QColumns - 1 do
    begin
      if I = J then
      begin
        Q[I, J] := 1;
      end
      else
      begin
        Q[I, J] := 0;
      end;
      Inc(J);
    end;
    Inc(I);
  end;

  // Calculate
  RectMatrixBDMultiplyByQ(QP, M, N, TauQ, Q, M, QColumns, False, False);
end;

class procedure PrincipalComponentsAnalysis.RectMatrixLQ(var A: TDouble2DArray;
  M, N: Integer; var Tau: TDouble1DArray);
var
  WORK: TDouble1DArray;
  T: TDouble1DArray;
  I: Integer;
  K: Integer;
  MinMN: Integer;
  Tmp: Double;
begin
  MinMN := Min(M, N);
  SetLength(WORK, M + 1);
  SetLength(T, N + 1);
  SetLength(Tau, MinMN - 1 + 1);
  K := Min(M, N);
  I := 0;
  while I <= K - 1 do
  begin
    // Generate elementary reflector H(i) to annihilate A(i,i+1:n-1)
    Move(@T[0], 1, N - I, @A[I][0], I, N - 1);
    GenerateReflection(T, N - I, Tmp);
    Tau[I] := Tmp;
    Move(@A[I][0], I, N - 1, @T[0], 1, N - I);
    T[1] := 1;
    if I < N then
    begin
      // Apply H(i) to A(i+1:m,i:n) from the right
      ApplyReflectionFromTheRight(A, Tau[I], T, I + 1, M - 1, I, N - 1, WORK);
    end;
    Inc(I);
  end;
end;

class procedure PrincipalComponentsAnalysis.RectMatrixLQUnpackQ(
  const A: TDouble2DArray; M, N: Integer; const Tau: TDouble1DArray;
  QRows: Integer; var Q: TDouble2DArray);
var
  I: Integer;
  J: Integer;
  K: Integer;
  MinMN: Integer;
  V: TDouble1DArray;
  WORK: TDouble1DArray;
begin
  Assert(QRows <= N,
    'PrincipalComponentsAnalysis.RectMatrixLQUnpackQ: QRows>N!');
  if (M <= 0) or (N <= 0) or (QRows <= 0) then
      Exit;

  // init
  MinMN := Min(M, N);
  K := Min(MinMN, QRows);
  SetLength(Q, QRows - 1 + 1, N - 1 + 1);
  SetLength(V, N + 1);
  SetLength(WORK, QRows + 1);
  I := 0;
  while I <= QRows - 1 do
  begin
    J := 0;
    while J <= N - 1 do
    begin
      if I = J then
      begin
        Q[I, J] := 1;
      end
      else
      begin
        Q[I, J] := 0;
      end;
      Inc(J);
    end;
    Inc(I);
  end;

  // unpack Q
  I := K - 1;
  while I >= 0 do
  begin
    // Apply H(i)
    Move(@V[0], 1, N - I, @A[I][0], I, N - 1);
    V[1] := 1;
    ApplyReflectionFromTheRight(Q, Tau[I], V, 0, QRows - 1, I, N - 1, WORK);
    Dec(I);
  end;
end;

class procedure PrincipalComponentsAnalysis.RectMatrixQR(var A: TDouble2DArray;
  M, N: Integer; var Tau: TDouble1DArray);
var
  WORK: TDouble1DArray;
  T: TDouble1DArray;
  I: Integer;
  K: Integer;
  MinMN: Integer;
  Tmp: Double;
  i_: Integer;
  i1_: Integer;
begin
  if (M <= 0) or (N <= 0) then
  begin
    Exit;
  end;
  MinMN := Min(M, N);
  SetLength(WORK, N - 1 + 1);
  SetLength(T, M + 1);
  SetLength(Tau, MinMN - 1 + 1);

  // Test the input arguments
  K := MinMN;
  I := 0;
  while I <= K - 1 do
  begin
    // Generate elementary reflector H(i) to annihilate A(i+1:m,i)
    i1_ := (I) - (1);
    for i_ := 1 to M - I do
        T[i_] := A[i_ + i1_, I];
    GenerateReflection(T, M - I, Tmp);
    Tau[I] := Tmp;
    i1_ := (1) - (I);
    for i_ := I to M - 1 do
        A[i_, I] := T[i_ + i1_];
    T[1] := 1;
    if I < N then
    begin
      // Apply H(i) to A(i:m-1,i+1:n-1) from the left
      ApplyReflectionFromTheLeft(A, Tau[I], T, I, M - 1, I + 1, N - 1, WORK);
    end;
    Inc(I);
  end;
end;

class procedure PrincipalComponentsAnalysis.RectMatrixQRUnpackQ(
  const A: TDouble2DArray; M, N: Integer; const Tau: TDouble1DArray;
  QColumns: Integer; var Q: TDouble2DArray);
var
  I: Integer;
  J: Integer;
  K: Integer;
  MinMN: Integer;
  V: TDouble1DArray;
  WORK: TDouble1DArray;
  i_: Integer;
  i1_: Integer;
begin
  Assert(QColumns <= M,
    'PrincipalComponentsAnalysis.RectMatrixQRUnpackQ: QColumns>M!');
  if (M <= 0) or (N <= 0) or (QColumns <= 0) then
  begin
    Exit;
  end;

  // init
  MinMN := Min(M, N);
  K := Min(MinMN, QColumns);
  SetLength(Q, M - 1 + 1, QColumns - 1 + 1);
  SetLength(V, M + 1);
  SetLength(WORK, QColumns - 1 + 1);
  I := 0;
  while I <= M - 1 do
  begin
    J := 0;
    while J <= QColumns - 1 do
    begin
      if I = J then
      begin
        Q[I, J] := 1;
      end
      else
      begin
        Q[I, J] := 0;
      end;
      Inc(J);
    end;
    Inc(I);
  end;

  // unpack Q
  I := K - 1;
  while I >= 0 do
  begin
    // Apply H(i)
    i1_ := (I) - (1);
    for i_ := 1 to M - I do
    begin
      V[i_] := A[i_ + i1_, I];
    end;
    V[1] := 1;
    ApplyReflectionFromTheLeft(Q, Tau[I], V, I, M - 1, 0, QColumns - 1, WORK);
    Dec(I);
  end;
end;

class function PrincipalComponentsAnalysis.RectMatrixSVD(A: TDouble2DArray;
  M, N, UNeeded, VTNeeded,
  AdditionalMemory: Integer; var W: TDouble1DArray; var U,
  VT: TDouble2DArray): Boolean;
var
  TauQ: TDouble1DArray;
  TauP: TDouble1DArray;
  Tau: TDouble1DArray;
  E: TDouble1DArray;
  WORK: TDouble1DArray;
  T2: TDouble2DArray;
  IsUpper: Boolean;
  MinMN: Integer;
  NCU: Integer;
  NRVT: Integer;
  NRU: Integer;
  NCVT: Integer;
  I: Integer;
  J: Integer;
begin
  A := Copy(A, 0, Length(A));
  Result := True;
  if (M = 0) or (N = 0) then
      Exit;
  Assert((UNeeded >= 0) and (UNeeded <= 2),
    'PrincipalComponentsAnalysis.RectMatrixSVD: wrong parameters!');
  Assert((VTNeeded >= 0) and (VTNeeded <= 2),
    'PrincipalComponentsAnalysis.RectMatrixSVD: wrong parameters!');
  Assert((AdditionalMemory >= 0) and (AdditionalMemory <= 2),
    'PrincipalComponentsAnalysis.RectMatrixSVD: wrong parameters!');

  // initialize
  MinMN := Min(M, N);
  SetLength(W, MinMN + 1);
  NCU := 0;
  NRU := 0;
  if UNeeded = 1 then
  begin
    NRU := M;
    NCU := MinMN;
    SetLength(U, NRU - 1 + 1, NCU - 1 + 1);
  end;
  if UNeeded = 2 then
  begin
    NRU := M;
    NCU := M;
    SetLength(U, NRU - 1 + 1, NCU - 1 + 1);
  end;
  NRVT := 0;
  NCVT := 0;
  if VTNeeded = 1 then
  begin
    NRVT := MinMN;
    NCVT := N;
    SetLength(VT, NRVT - 1 + 1, NCVT - 1 + 1);
  end;
  if VTNeeded = 2 then
  begin
    NRVT := N;
    NCVT := N;
    SetLength(VT, NRVT - 1 + 1, NCVT - 1 + 1);
  end;

  // M much larger than N
  // Use bidiagonal reduction with QR-decomposition
  if M > 1.6 * N then
  begin
    if UNeeded = 0 then
    begin
      // No left singular vectors to be computed
      RectMatrixQR(A, M, N, Tau);
      I := 0;
      while I <= N - 1 do
      begin
        J := 0;
        while J <= I - 1 do
        begin
          A[I, J] := 0;
          Inc(J);
        end;
        Inc(I);
      end;
      RectMatrixBD(A, N, N, TauQ, TauP);
      RectMatrixBDUnpackPT(A, N, N, TauP, NRVT, VT);
      RectMatrixBDUnpackDiagonals(A, N, N, IsUpper, W, E);
      Result := RectMatrixBDSVD(W, E, N, IsUpper, False, U, 0, A, 0, VT, NCVT);
      Exit;
    end
    else
    begin
      // Left singular vectors (may be full matrix U) to be computed
      RectMatrixQR(A, M, N, Tau);
      RectMatrixQRUnpackQ(A, M, N, Tau, NCU, U);
      I := 0;
      while I <= N - 1 do
      begin
        J := 0;
        while J <= I - 1 do
        begin
          A[I, J] := 0;
          Inc(J);
        end;
        Inc(I);
      end;
      RectMatrixBD(A, N, N, TauQ, TauP);
      RectMatrixBDUnpackPT(A, N, N, TauP, NRVT, VT);
      RectMatrixBDUnpackDiagonals(A, N, N, IsUpper, W, E);
      if AdditionalMemory < 1 then
      begin
        // No additional memory can be used
        RectMatrixBDMultiplyByQ(A, N, N, TauQ, U, M, N, True, False);
        Result := RectMatrixBDSVD(W, E, N, IsUpper, False, U, M, A, 0,
          VT, NCVT);
      end
      else
      begin
        // Large U. Transforming intermediate matrix T2
        SetLength(WORK, Max(M, N) + 1);
        RectMatrixBDUnpackQ(A, N, N, TauQ, N, T2);
        CopyMatrix(U, 0, M - 1, 0, N - 1, A, 0, M - 1, 0, N - 1);
        InplaceTranspose(T2, 0, N - 1, 0, N - 1, WORK);
        Result := RectMatrixBDSVD(W, E, N, IsUpper, False, U, 0, T2, N,
          VT, NCVT);
        MatrixMatrixMultiply(A, 0, M - 1, 0, N - 1, False, T2, 0, N - 1, 0,
          N - 1, True, 1.0, U, 0, M - 1, 0, N - 1, 0.0, WORK);
      end;
      Exit;
    end;
  end;

  // N much larger than M
  // Use bidiagonal reduction with LQ-decomposition
  if N > 1.6 * M then
  begin
    if VTNeeded = 0 then
    begin
      // No right singular vectors to be computed
      RectMatrixLQ(A, M, N, Tau);
      I := 0;
      while I <= M - 1 do
      begin
        J := I + 1;
        while J <= M - 1 do
        begin
          A[I, J] := 0;
          Inc(J);
        end;
        Inc(I);
      end;
      RectMatrixBD(A, M, M, TauQ, TauP);
      RectMatrixBDUnpackQ(A, M, M, TauQ, NCU, U);
      RectMatrixBDUnpackDiagonals(A, M, M, IsUpper, W, E);
      SetLength(WORK, M + 1);
      InplaceTranspose(U, 0, NRU - 1, 0, NCU - 1, WORK);
      Result := RectMatrixBDSVD(W, E, M, IsUpper, False, A, 0, U, NRU, VT, 0);
      InplaceTranspose(U, 0, NRU - 1, 0, NCU - 1, WORK);
      Exit;
    end
    else
    begin
      // Right singular vectors (may be full matrix VT) to be computed
      RectMatrixLQ(A, M, N, Tau);
      RectMatrixLQUnpackQ(A, M, N, Tau, NRVT, VT);
      I := 0;
      while I <= M - 1 do
      begin
        J := I + 1;
        while J <= M - 1 do
        begin
          A[I, J] := 0;
          Inc(J);
        end;
        Inc(I);
      end;
      RectMatrixBD(A, M, M, TauQ, TauP);
      RectMatrixBDUnpackQ(A, M, M, TauQ, NCU, U);
      RectMatrixBDUnpackDiagonals(A, M, M, IsUpper, W, E);
      SetLength(WORK, Max(M, N) + 1);
      InplaceTranspose(U, 0, NRU - 1, 0, NCU - 1, WORK);
      if AdditionalMemory < 1 then
      begin

        // No additional memory available
        RectMatrixBDMultiplyByP(A, M, M, TauP, VT, M, N, False, True);
        Result := RectMatrixBDSVD(W, E, M, IsUpper, False, A, 0, U, NRU, VT, N);
      end
      else
      begin

        // Large VT. Transforming intermediate matrix T2
        RectMatrixBDUnpackPT(A, M, M, TauP, M, T2);
        Result := RectMatrixBDSVD(W, E, M, IsUpper, False, A, 0, U, NRU, T2, M);
        CopyMatrix(VT, 0, M - 1, 0, N - 1, A, 0, M - 1, 0, N - 1);
        MatrixMatrixMultiply(T2, 0, M - 1, 0, M - 1, False, A, 0, M - 1, 0,
          N - 1, False, 1.0, VT, 0, M - 1, 0, N - 1, 0.0, WORK);
      end;
      InplaceTranspose(U, 0, NRU - 1, 0, NCU - 1, WORK);
      Exit;
    end;
  end;

  // M<=N
  // We can use inplace transposition of U to get rid of columnwise operations
  if M <= N then
  begin
    RectMatrixBD(A, M, N, TauQ, TauP);
    RectMatrixBDUnpackQ(A, M, N, TauQ, NCU, U);
    RectMatrixBDUnpackPT(A, M, N, TauP, NRVT, VT);
    RectMatrixBDUnpackDiagonals(A, M, N, IsUpper, W, E);
    SetLength(WORK, M + 1);
    InplaceTranspose(U, 0, NRU - 1, 0, NCU - 1, WORK);
    Result := RectMatrixBDSVD(W, E, MinMN, IsUpper, False, A, 0, U, NRU,
      VT, NCVT);
    InplaceTranspose(U, 0, NRU - 1, 0, NCU - 1, WORK);
    Exit;
  end;

  // Simple bidiagonal reduction
  RectMatrixBD(A, M, N, TauQ, TauP);
  RectMatrixBDUnpackQ(A, M, N, TauQ, NCU, U);
  RectMatrixBDUnpackPT(A, M, N, TauP, NRVT, VT);
  RectMatrixBDUnpackDiagonals(A, M, N, IsUpper, W, E);
  if (AdditionalMemory < 2) or (UNeeded = 0) then
  begin

    // We cant use additional memory or there is no need in such operations
    Result := RectMatrixBDSVD(W, E, MinMN, IsUpper, False, U, NRU, A, 0,
      VT, NCVT);
  end
  else
  begin

    // We can use additional memory
    SetLength(T2, MinMN - 1 + 1, M - 1 + 1);
    CopyAndTranspose(U, 0, M - 1, 0, MinMN - 1, T2, 0, MinMN - 1, 0, M - 1);
    Result := RectMatrixBDSVD(W, E, MinMN, IsUpper, False, U, 0, T2, M,
      VT, NCVT);
    CopyAndTranspose(T2, 0, MinMN - 1, 0, M - 1, U, 0, M - 1, 0, MinMN - 1);
  end;
end;

class procedure PrincipalComponentsAnalysis.Sub(VDst: PDouble; I11, I12: Integer;
  VSrc: PDouble; I21,
  I22: Integer);
var
  I, C: LongInt;
begin
  Assert(I12 - I11 = I22 - I21,
    'PrincipalComponentsAnalysis.Sub arrays of different size!');
  Inc(VDst, I11);
  Inc(VSrc, I21);

  C := I12 - I11;
  for I := 0 to C do
  begin
    VDst^ := VDst^ - VSrc^;
    Inc(VDst);
    Inc(VSrc);
  end;
end;

class procedure PrincipalComponentsAnalysis.Sub(VDst: PDouble; I11, I12: Integer; VSrc: PDouble;
  I21, I22: Integer; S: Double);
begin
  Assert(I12 - I11 = I22 - I21,
    'PrincipalComponentsAnalysis.Sub: arrays of different size!');
  Add(VDst, I11, I12, VSrc, I21, I22, -S);
end;

class procedure PrincipalComponentsAnalysis.Svd2X2(F, G, H: Double; var SSMIN,
  SSMAX: Double);
var
  AAS: Double;
  AT: Double;
  AU: Double;
  C: Double;
  FA: Double;
  FHMN: Double;
  FHMX: Double;
  GA: Double;
  HA: Double;
begin
  FA := ABS(F);
  GA := ABS(G);
  HA := ABS(H);
  FHMN := Min(FA, HA);
  FHMX := Max(FA, HA);
  if FHMN = 0 then
  begin
    SSMIN := 0;
    if FHMX = 0 then
    begin
      SSMAX := GA;
    end
    else
    begin
      SSMAX := Max(FHMX, GA) * SQRT(1 + Sqr(Min(FHMX, GA) / Max(FHMX, GA)));
    end;
  end
  else
  begin
    if GA < FHMX then
    begin
      AAS := 1 + FHMN / FHMX;
      AT := (FHMX - FHMN) / FHMX;
      AU := Sqr(GA / FHMX);
      C := 2 / (SQRT(AAS * AAS + AU) + SQRT(AT * AT + AU));
      SSMIN := FHMN * C;
      SSMAX := FHMX / C;
    end
    else
    begin
      AU := FHMX / GA;
      if AU = 0 then
      begin
        // Avoid possible harmful underflow if exponent range
        // asymmetric (true SSMIN may not underflow even if
        // AU underflows)
        SSMIN := FHMN * FHMX / GA;
        SSMAX := GA;
      end
      else
      begin
        AAS := 1 + FHMN / FHMX;
        AT := (FHMX - FHMN) / FHMX;
        C := 1 / (SQRT(1 + Sqr(AAS * AU)) + SQRT(1 + Sqr(AT * AU)));
        SSMIN := FHMN * C * AU;
        SSMIN := SSMIN + SSMIN;
        SSMAX := GA / (C + C);
      end;
    end;
  end;
end;

class procedure PrincipalComponentsAnalysis.SvdV2X2(F, G, H: Double; var SSMIN,
  SSMAX, SNR, CSR, SNL, CSL: Double);
var
  GASMAL: Boolean;
  SWP: Boolean;
  PMAX: Integer;
  A: Double;
  CLT: Double;
  CRT: Double;
  D: Double;
  FA: Double;
  FT: Double;
  GA: Double;
  GT: Double;
  HA: Double;
  HT: Double;
  L: Double;
  M: Double;
  MM: Double;
  R: Double;
  S: Double;
  SLT: Double;
  SRT: Double;
  T: Double;
  TEMP: Double;
  TSIGN: Double;
  TT: Double;
  V: Double;
begin
  FT := F;
  FA := ABS(FT);
  HT := H;
  HA := ABS(H);
  SRT := 0;
  CRT := 0;
  SLT := 0;
  CLT := 0;

  // PMAX points to the maximum absolute element of matrix
  // PMAX = 1 if F largest in absolute values
  // PMAX = 2 if G largest in absolute values
  // PMAX = 3 if H largest in absolute values
  PMAX := 1;
  SWP := HA > FA;
  if SWP then
  begin
    // Now FA .ge. HA
    PMAX := 3;
    TEMP := FT;
    FT := HT;
    HT := TEMP;
    TEMP := FA;
    FA := HA;
    HA := TEMP;
  end;
  GT := G;
  GA := ABS(GT);
  if GA = 0 then
  begin
    // Diagonal matrix
    SSMIN := HA;
    SSMAX := FA;
    CLT := 1;
    CRT := 1;
    SLT := 0;
    SRT := 0;
  end
  else
  begin
    GASMAL := True;
    if GA > FA then
    begin
      PMAX := 2;
      if FA / GA < MACHINE_EPSILON then
      begin

        //
        // Case of very large GA
        //
        GASMAL := False;
        SSMAX := GA;
        if HA > 1 then
        begin
          V := GA / HA;
          SSMIN := FA / V;
        end
        else
        begin
          V := FA / GA;
          SSMIN := V * HA;
        end;
        CLT := 1;
        SLT := HT / GT;
        SRT := 1;
        CRT := FT / GT;
      end;
    end;
    if GASMAL then
    begin
      // Normal case
      D := FA - HA;
      if D = FA then
          L := 1
      else
          L := D / FA;
      M := GT / FT;
      T := 2 - L;
      MM := M * M;
      TT := T * T;
      S := SQRT(TT + MM);
      if L = 0 then
          R := ABS(M)
      else
          R := SQRT(L * L + MM);
      A := 0.5 * (S + R);
      SSMIN := HA / A;
      SSMAX := FA * A;
      if MM = 0 then
      begin
        // Note that M is very tiny
        if L = 0 then
            T := ExtSignBDSQR(2, FT) * ExtSignBDSQR(1, GT)
        else
            T := GT / ExtSignBDSQR(D, FT) + M / T;
      end
      else
      begin
        T := (M / (S + T) + M / (R + L)) * (1 + A);
      end;
      L := SQRT(T * T + 4);
      CRT := 2 / L;
      SRT := T / L;
      CLT := (CRT + SRT * M) / A;
      V := HT / FT;
      SLT := V * SRT / A;
    end;
  end;
  if SWP then
  begin
    CSL := SRT;
    SNL := CRT;
    CSR := SLT;
    SNR := CLT;
  end
  else
  begin
    CSL := CLT;
    SNL := SLT;
    CSR := CRT;
    SNR := SRT;
  end;

  // Correct signs of SSMAX and SSMIN
  if PMAX = 1 then
      TSIGN := ExtSignBDSQR(1, CSR) * ExtSignBDSQR(1, CSL) * ExtSignBDSQR(1, F)
  else if PMAX = 2 then
      TSIGN := ExtSignBDSQR(1, SNR) * ExtSignBDSQR(1, CSL) * ExtSignBDSQR(1, G)
  else if PMAX = 3 then
      TSIGN := ExtSignBDSQR(1, SNR) * ExtSignBDSQR(1, SNL) * ExtSignBDSQR(1, H)
  else TSIGN := 0;
  SSMAX := ExtSignBDSQR(SSMAX, TSIGN);
  SSMIN := ExtSignBDSQR(SSMIN, TSIGN * ExtSignBDSQR(1, F) * ExtSignBDSQR(1, H));
end;

end.
