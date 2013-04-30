unit uMeshUtils;

interface

uses
  uBaseTypes, uLists, uVMath;

type

  MeshUtils = class
  public
    class var RestartIndex: Integer;
    // Сварка вершин
    class procedure WeldVertices(var anAttribs: array of TAbstractDataList;
      var anIndices: TIntegerArray);
    // Каждая вершина становится уникальной, значения индексов не повторяется
    class procedure UnWeldVertices(var anAttribs: array of TAbstractDataList;
      var anIndices: TIntegerArray; aVertexAttribIndex: Integer);
    // Конвертирует полосы и вееры треугольников в отдельные треугольники
    class procedure Triangulate(aFaceType: TFaceType;
      var anIndices: TIntegerArray);
    // Создает нормали
    class function ComputeTriangleNormals(ASmooth: Boolean;
      aVertices: TAbstractDataList; anIndices: TIntegerArray): TVec3List;
    // Создает касательные
    class procedure ComputeTriangleTangents(aVertices, aTexCoors,
      aNormals: TAbstractDataList; anIndices: TIntegerArray;
      var aTangens, aBinormal: TAbstractDataList);
    // Создает текстурные координаты
    class function ComputeTriangleTexCoords(aVertices: TAbstractDataList)
      : TVec2List; overload;
    class function ComputeTriangleTexCoords(aVertices: TAbstractDataList;
      anIndices: TIntegerArray): TVec2List; overload;
    // Создает индексы смежных треугольников
    class procedure ComputeTriangleAdjacency(Vertices: TAbstractDataList;
      anIndices: TIntegerArray;
      var anAdjacencyIndices: TIntegerArray);
  end;

implementation

uses
  uGenericsRBTree;

type
  TIntIntRBTree = GRedBlackTree<Integer, Integer>;
  TVertexHashMap = GRedBlackTree<Double, Integer>;

  { MeshUtils }

function CompareVertexKey(const Item1, Item2: Double): Integer;
begin
  if Item1 < Item2 then
    exit(-1)
  else if Item1 = Item2 then
    exit(0)
  else
    result := 1;
end;

threadvar
vAttribs: array of TAbstractDataList;
vIndices:
TIntegerArray;

function CompareVertex(const Item1, Item2: Integer): Boolean;
var
  a: Integer;
  Idx1, Idx2: Integer;
begin
  if Item1 <> Item2 then
  begin
    Idx1 := vIndices[Item1];
    Idx2 := vIndices[Item2];
    for a := 0 to High(vAttribs) do
      if not vAttribs[a].IsItemsEqual(Idx1, Idx2) then
        exit(false);
  end;
  result := true;
end;

function CompareIntegerValue(const Item1, Item2: Integer): Boolean;
begin
  result := Item1 = Item2;
end;

class procedure MeshUtils.ComputeTriangleAdjacency(Vertices: TAbstractDataList;
  anIndices: TIntegerArray; var anAdjacencyIndices: TIntegerArray);
type
  Vec4ui = array [0 .. 3] of LongInt;
  PVec4ui = ^Vec4ui;
var
  edgeInfo: TTriangleEdgeInfoArray;
  triangleNum: integer;
  NewIndices: TIntegerList;

  procedure joinTriangles(tri1: integer; edge1: cardinal; tri2: integer;
    edge2: cardinal);
  begin
    assert((edge1 < 3) and (edge2 < 3),
      'joinTriangles: Multiple edge detected.');

    edgeInfo[tri1].adjacentTriangle[edge1] := tri2;
    edgeInfo[tri1].adjacentTriangleEdges := edgeInfo[tri1]
      .adjacentTriangleEdges and not(3 shl (2 * edge1));
    edgeInfo[tri1].adjacentTriangleEdges := edgeInfo[tri1]
      .adjacentTriangleEdges or (edge2 shl (2 * edge1));

    edgeInfo[tri2].adjacentTriangle[edge2] := tri1;
    edgeInfo[tri2].adjacentTriangleEdges := edgeInfo[tri2]
      .adjacentTriangleEdges and not(3 shl (2 * edge2));
    edgeInfo[tri2].adjacentTriangleEdges := edgeInfo[tri2]
      .adjacentTriangleEdges or (edge1 shl (2 * edge2));
  end;

  procedure matchWithTriangleSharingEdge(triangle, edge, v0, v1,
    otherv: Integer);

  var
    i: integer;
    doubleTri: integer;
    otherEdge: integer;
    vertexIndex: PVec4ui;
  begin
    doubleTri := -1;
    otherEdge := 0;
    // Match shared edges based on vertex numbers (relatively fast).
    for i := triangle + 1 to triangleNum - 1 do
    begin
      vertexIndex := NewIndices.GetItemAddr(i * 3);

      if vertexIndex[0] = v0 then
        if vertexIndex[2] = v1 then
          if edgeInfo[i].adjacentTriangle[2] = $FFFFFFFF then
            if vertexIndex[1] = otherv then
            begin
              if (doubleTri < 0) then
              begin
                doubleTri := i;
                otherEdge := 2;
              end;
            end
            else
            begin
              joinTriangles(i, 2, triangle, edge);
              exit;
            end;

      if vertexIndex[1] = v0 then
        if vertexIndex[0] = v1 then
          if edgeInfo[i].adjacentTriangle[0] = $FFFFFFFF then
            if vertexIndex[2] = otherv then
            begin
              if doubleTri < 0 then
              begin
                doubleTri := i;
                otherEdge := 0;
              end;
            end
            else
            begin
              joinTriangles(i, 0, triangle, edge);
              exit;
            end;

      if vertexIndex[2] = v0 then
        if vertexIndex[1] = v1 then
          if edgeInfo[i].adjacentTriangle[1] = $FFFFFFFF then
            if vertexIndex[0] = otherv then
            begin
              if doubleTri < 0 then
              begin
                doubleTri := i;
                otherEdge := 1;
              end;
            end
            else
            begin
              joinTriangles(i, 1, triangle, edge);
              exit;
            end;
    end;

    // Only connect a triangle to a triangle with the exact
    // same three vertices as a last resort.
    if doubleTri >= 0 then
      joinTriangles(doubleTri, otherEdge, triangle, edge);
  end;

  procedure CheckForBogusAdjacency;

    function AdjacentEdge(x, n: integer): integer;
    begin
      result := (x shr (2 * n)) and 3;
    end;

  var
    i, J: integer;
    adjacentTriangle, adjacentTriangleSharedEdge: Integer;
  begin
    for i := 0 to triangleNum - 1 do
      for J := 0 to 2 do
      begin
        adjacentTriangleSharedEdge :=
          AdjacentEdge(edgeInfo[i].adjacentTriangleEdges, J);
        adjacentTriangle := edgeInfo[i].adjacentTriangle[J];
        if adjacentTriangle <> -1 then
        begin
          assert(adjacentTriangleSharedEdge < 3);
          assert(edgeInfo[adjacentTriangle].adjacentTriangle
            [adjacentTriangleSharedEdge] = LongWord(i));
          assert(AdjacentEdge(edgeInfo[adjacentTriangle].adjacentTriangleEdges,
            adjacentTriangleSharedEdge) = J);
        end
        else
          assert(adjacentTriangleSharedEdge = 3);
      end;
  end;

  function AdjacentEdge(x, n: integer): integer;
  begin
    result := (x shr (2 * n)) and 3;
  end;

var
  i, J, K: integer;
  vertexIndex, tri, adjtri: PVec4ui;
  n, ii, jj: Integer;
begin
  Assert(Length(anIndices) mod 3  = 0);

  NewIndices := nil;

  try
    NewIndices := TIntegerList.Create;
    for i := 1 to High(anIndices) do
    begin
      ii := anIndices[i];
      for J := 0 to i - 1 do
      begin
        jj := anIndices[J];
        if ii = jj then
          continue;
        if Vertices.IsItemsEqual(ii, jj) then
        begin
          anIndices[i] := jj;
          break;
        end;
      end;
    end;

    // Remove degenerate triangles
    triangleNum := 0;
    for i := 0 to Length(anIndices) div 3 - 1 do
    begin
      vertexIndex := @anIndices[i * 3];
      if (vertexIndex[0] = vertexIndex[1]) or (vertexIndex[0] = vertexIndex[2])
        or (vertexIndex[1] = vertexIndex[2]) then
        continue;
      NewIndices.Add(vertexIndex[0]);
      NewIndices.Add(vertexIndex[1]);
      NewIndices.Add(vertexIndex[2]);
      inc(triangleNum);
    end;

    // Initialize edge information as if all triangles are fully disconnected.
    setlength(edgeInfo, triangleNum);
    for i := 0 to triangleNum - 1 do
    begin
      edgeInfo[i].adjacentTriangle[0] := $FFFFFFFF; // Vertex 0,1 edge
      edgeInfo[i].adjacentTriangle[1] := $FFFFFFFF; // Vertex 1,2 edge
      edgeInfo[i].adjacentTriangle[2] := $FFFFFFFF; // Vertex 2,0 edge
      edgeInfo[i].adjacentTriangleEdges := (3 shl 0) or (3 shl 2) or (3 shl 4);
      edgeInfo[i].openEdgeMask := 0;
    end;

    try
      for i := 0 to triangleNum - 1 do
      begin
        vertexIndex := NewIndices.GetItemAddr(i * 3);
        if edgeInfo[i].adjacentTriangle[0] = $FFFFFFFF then
          matchWithTriangleSharingEdge(i, 0, vertexIndex[0], vertexIndex[1],
            vertexIndex[2]);
        if edgeInfo[i].adjacentTriangle[1] = $FFFFFFFF then
          matchWithTriangleSharingEdge(i, 1, vertexIndex[1], vertexIndex[2],
            vertexIndex[0]);
        if edgeInfo[i].adjacentTriangle[2] = $FFFFFFFF then
          matchWithTriangleSharingEdge(i, 2, vertexIndex[2], vertexIndex[0],
            vertexIndex[1]);
      end;

      CheckForBogusAdjacency;

      SetLength(anAdjacencyIndices, 2 * NewIndices.Count);
      K := 0;

      for i := 0 to triangleNum - 1 do
      begin
        n := 3 * i;
        tri := NewIndices.GetItemAddr(n);
        for J := 0 to 2 do
        begin
          anAdjacencyIndices[K] := tri^[J];
          inc(K);
          n := edgeInfo[i].adjacentTriangle[J];
          if n = -1 then
          begin
            jj := (J + 2) mod 3;
            anAdjacencyIndices[K] := tri^[jj];
            inc(K);
          end
          else
          begin
            n := 3 * n;
            adjtri := NewIndices.GetItemAddr(n);
            ii := (AdjacentEdge(edgeInfo[i].adjacentTriangleEdges, J) +
              2) mod 3;
            anAdjacencyIndices[K] := adjtri^[ii];
            inc(K);
          end;
        end;
      end;
    except
      SetLength(anAdjacencyIndices, 0);
    end;

  finally
    NewIndices.Free;
  end;
end;

class function MeshUtils.ComputeTriangleNormals(ASmooth: Boolean;
  aVertices: TAbstractDataList; anIndices: TIntegerArray): TVec3List;
var
  I, J, E, E_, T, EJ: Integer;
  PBIndices: TIntegerArray;
  p0, p1, p2, dp0, dp1, fNormal, nNormal, cNormal: TVector;
  NewNormals, Normals: TVec3List;
  NewNormalIndices: TIntegerList;
  collisionMap: TIntIntRBTree;
  Agrees: Boolean;
begin
  Assert(Length(anIndices) mod 3 = 0);
  Assert((ASmooth and (Length(anIndices) = aVertices.Count)) or
    (Length(anIndices) = 0));

  NewNormals := nil;
  NewNormalIndices := nil;
  collisionMap := nil;

  try
    if ASmooth then
    begin
      // Делаем сваривание вершим по равенству их позиций
      SetLength(PBIndices, Length(anIndices));
      for I := 1 to High(anIndices) do
      begin
        E := anIndices[I];
        PBIndices[I] := E;
        for J := 0 to I - 1 do
        begin
          E_ := PBIndices[J];
          if E = E_ then
            continue;
          if aVertices.IsItemsEqual(E, E_) then
          begin
            PBIndices[I] := E_;
            break;
          end;
        end;
      end;
    end
    else
      PBIndices := Copy(anIndices, 0, Length(anIndices));

    if Length(PBIndices) = 0 then
    begin
      // Если индексов нет, делаем считая что вершины расположены по порядку
      SetLength(PBIndices, aVertices.Count);
      SetLength(anIndices, aVertices.Count);
      for I := 0 to aVertices.Count - 1 do
      begin
        PBIndices[I] := I;
        anIndices[I] := I;
      end;
    end;

    NewNormals := TVec3List.Create;
    NewNormals.Count := Length(anIndices);
    NewNormalIndices := TIntegerList.Create;

    // The collision map records any alternate locations for the normals
    if ASmooth then
    begin
      collisionMap := TIntIntRBTree.Create(CompareInteger, CompareIntegerValue);
      collisionMap.DuplicateKeys := true;
    end;

    // Iterate over the faces, computing the face normal and summing it them
    for T := 0 to Length(PBIndices) div 3 - 1 do
    begin
      E := 3 * T;
      p0 := aVertices.GetItemAsVector(PBIndices[E]);
      p1 := aVertices.GetItemAsVector(PBIndices[E + 1]);
      p2 := aVertices.GetItemAsVector(PBIndices[E + 2]);

      // Compute the edge vectors
      dp0 := p1 - p0;
      dp1 := p2 - p0;

      // Compute the face normal
      fNormal := dp0.Cross(dp1);

      if not ASmooth then
      begin
        NewNormals[E] := fNormal.vec3;
        NewNormalIndices.Add(E);
        inc(E);
        NewNormals[E] := fNormal.vec3;
        NewNormalIndices.Add(E);
        inc(E);
        NewNormals[E] := fNormal.vec3;
        NewNormalIndices.Add(E);
        continue;
      end;

      // Compute a normalized normal
      nNormal := fNormal.Normalize;

      // Iterate over the vertices, adding the face normal influence to each
      for J := 0 to 2 do
      begin
        // Get the current normal from the default location (index shared with position)
        EJ := PBIndices[E + J];
        cNormal := NewNormals[EJ];

        // Check to see if this normal has not yet been touched
        if cNormal.IsNull then
        begin
          // First instance of this index, just store it as is
          NewNormals[EJ] := fNormal.vec3;
          NewNormalIndices.Add(EJ);
        end
        else
        begin
          // Check for agreement

          if cNormal.Normalize.Dot(nNormal) >= cos(3.1415926 * 0.333333) then
          begin
            // Normal agrees, so add it
            cNormal := cNormal + fNormal;
            NewNormals[EJ] := cNormal.vec3;
            NewNormalIndices.Add(EJ);
          end
          else
          begin
            // Normals disagree, this vertex must be along a facet edge
            Agrees := false;
            if collisionMap.Find(EJ, E_) then
            begin
              // Loop through all hits on this index, until one agrees
              repeat
                cNormal := NewNormals[E_];
                if cNormal.Normalize.Dot(nNormal) >= cos(3.1415926 * 0.333333)
                then
                begin
                  Agrees := true;
                  break;
                end;
              until not collisionMap.NextDublicate(E_);
            end;

            // Check for agreement with an earlier collision
            if Agrees then
            begin
              // Found agreement with an earlier collision, use that one
              cNormal := cNormal + fNormal;
              NewNormals[E_] := cNormal.vec3;
              NewNormalIndices.Add(E_);
            end
            else
            begin
              // We have a new collision, create a new normal
              NewNormalIndices.Add(NewNormals.Count);
              collisionMap.Add(EJ, NewNormals.Count);
              NewNormals.Add(fNormal.vec3);
            end;
          end; // else ( if normal agrees)
        end; // else (if normal is uninitialized)
      end; // for each vertex in triangle
    end; // for each face

    Normals := TVec3List.Create;
    Normals.Count := NewNormalIndices.Count;
    for I := 0 to NewNormalIndices.Count - 1 do
    begin
      E := NewNormalIndices[I];
      cNormal.vec3 := NewNormals[E];
      cNormal.SetNormalize;
      E_ := anIndices[I];
      Normals[E_] := cNormal.vec3;
    end;

  finally
    NewNormals.Free;
    NewNormalIndices.Free;
    collisionMap.Free;
  end;

  result := Normals;
end;

class procedure MeshUtils.ComputeTriangleTangents(aVertices, aTexCoors,
  aNormals: TAbstractDataList; anIndices: TIntegerArray;
  var aTangens, aBinormal: TAbstractDataList);
var
  a, T, I, J, E, EJ, E_: Integer;
  p0, p1, p2, dp0, dp1, st0, st1, st2, dst0, dst1, fTangent, nTangent,
    cTangent: TVector;
  factor: single;
  newTangents: TVec3List;
  newTangentIndices: TIntegerList;
  collisionMap: TIntIntRBTree;
  Agrees: Boolean;
begin
  Assert(Length(anIndices) mod 3 = 0);
  Assert((aVertices.Count = aTexCoors.Count));
  Assert((Length(anIndices) = aVertices.Count) or (Length(anIndices) = 0));

  newTangents := nil;
  collisionMap := nil;
  newTangentIndices := nil;

  try
    newTangents := TVec3List.Create;
    newTangentIndices := TIntegerList.Create;

    // The collision map records any alternate locations for the tangent
    collisionMap := TIntIntRBTree.Create(CompareInteger, CompareIntegerValue);
    collisionMap.DuplicateKeys := true;
    fTangent.vec4 := VecNull;

    // Iterate over the faces, computing the face normal and summing it them
    for T := 0 to Length(anIndices) div 3 - 1 do
    begin
      E := 3 * T;
      p0 := aVertices.GetItemAsVector(anIndices[E]);
      p1 := aVertices.GetItemAsVector(anIndices[E + 1]);
      p2 := aVertices.GetItemAsVector(anIndices[E + 2]);
      st0 := aVertices.GetItemAsVector(anIndices[E]);
      st1 := aVertices.GetItemAsVector(anIndices[E + 1]);
      st2 := aVertices.GetItemAsVector(anIndices[E + 2]);

      // Compute the edge and tc differentials
      dp0 := p1 - p0;
      dp1 := p2 - p0;
      dst0 := st1 - st0;
      dst1 := st2 - st0;

      factor := 1.0 / (dst0[0] * dst1[1] - dst1[0] * dst0[1]);

      // compute fTangent
      fTangent[0] := dp0[0] * dst1[1] - dp1[0] * dst0[1];
      fTangent[1] := dp0[1] * dst1[1] - dp1[1] * dst0[1];
      fTangent[2] := dp0[2] * dst1[1] - dp1[2] * dst0[1];
      fTangent := fTangent.Scale(factor);

      // should this really renormalize?
      nTangent := fTangent.Normalize;

      // Iterate over the vertices, adding the face normal influence to each
      for J := 0 to 2 do
      begin
        // Get the current normal from the default location (index shared with position)
        EJ := anIndices[E + J];
        cTangent.vec3 := newTangents[EJ];

        // Check to see if this normal has not yet been touched
        if cTangent.IsNull then
        begin
          // First instance of this index, just store it as is
          newTangents[EJ] := fTangent.vec3;
          newTangentIndices.Add(EJ);
        end
        else
        begin
          // Check for agreement
          if cTangent.Normalize.Dot(nTangent) >= cos(3.1415926 * 0.333333) then
          begin
            // Normal agrees, so add it
            cTangent := cTangent + fTangent;
            newTangents[EJ] := cTangent.vec3;
            newTangentIndices.Add(EJ);
          end
          else
          begin
            // Normals disagree, this vertex must be along a facet edge
            Agrees := false;
            if collisionMap.Find(EJ, E_) then
            begin
              // Loop through all hits on this index, until one agrees
              repeat
                cTangent := newTangents[E_];
                if cTangent.Normalize.Dot(nTangent) >= cos(3.1415926 * 0.333333)
                then
                begin
                  Agrees := true;
                  break;
                end;
              until not collisionMap.NextDublicate(E_);
            end;

            // Check for agreement with an earlier collision
            if Agrees then
            begin
              // Found agreement with an earlier collision, use that one
              cTangent := cTangent + fTangent;
              newTangents[E_] := cTangent.vec3;
              newTangentIndices.Add(E_);
            end
            else
            begin
              // We have a new collision, create a new normal
              newTangentIndices.Add(newTangents.Count);
              collisionMap.Add(EJ, newTangents.Count);
              newTangents.Add(fTangent.vec3);
            end;
          end; // else ( if tangent agrees)
        end; // else (if tangent is uninitialized)
      end; // for each vertex in triangle
    end; // for each face

    // now normalize all the normals
    newTangents.SetNormalize;

    if not Assigned(aTangens) then
      aTangens := TVec3List.Create;
    aTangens.Count := newTangentIndices.Count;

    for I := 0 to newTangentIndices.Count - 1 do
    begin
      E := newTangentIndices[I];
      E_ := anIndices[I];
      cTangent.vec3 := newTangents[E];
      aTangens.SetItemAsVector(E_, cTangent);
    end;

    if Assigned(aNormals) then
    begin
      if not Assigned(aBinormal) then
        aBinormal := TVec3List.Create;
      aBinormal.Count := aNormals.Count;
      for I := 0 to aNormals.Count - 1 do
      begin
        cTangent := aTangens.GetItemAsVector(I);
        nTangent := aNormals.GetItemAsVector(I);
        fTangent := nTangent.Cross(cTangent);
        aBinormal.SetItemAsVector(I, fTangent);
      end;
    end;

  finally
    newTangents.Free;
    newTangentIndices.Free;
    collisionMap.Free;
  end;
end;

class function MeshUtils.ComputeTriangleTexCoords(
  aVertices: TAbstractDataList): TVec2List;
var
  NewTexCoords: TVec2List;
  T, E3T: integer;
  p0, p1, p2, dp0, dp1, fNormal, fTangent, fBinormal: TVector;
  TBN: TMatrix;
begin
  NewTexCoords := TVec2List.Create;
  NewTexCoords.Count := aVertices.Count;

  for T := 0 to aVertices.Count div 3 - 1 do
  begin
    E3T := 3 * T;
    p0 := aVertices.GetItemAsVector(E3T);
    p1 := aVertices.GetItemAsVector(E3T+1);
    p2 := aVertices.GetItemAsVector(E3T+2);

    // Compute the edge vectors
    dp0 := p1 - p0;
    dp1 := p2 - p0;

    // Compute the face TBN
    fNormal := dp0.Cross(dp1);
    fNormal := fNormal.Normalize;
    fTangent := dp0;
    fTangent := fTangent.Normalize;
    fBinormal := fNormal.Cross(fTangent);
    TBN.Row[0] := fTangent.vec4;
    TBN.Row[1] := fBinormal.vec4;
    TBN.Row[2] := fNormal.vec4;
    TBN := TBN.Invert;

    p0 := TBN.Transform(p0);
    NewTexCoords[E3T] := p0.Vec2;

    p1 := TBN.Transform(p1);
    NewTexCoords[E3T+1] := p1.Vec2;

    p2 := TBN.Transform(p2);
    NewTexCoords[E3T+2] := p2.Vec2;
  end;
  Result := NewTexCoords;
end;

class function MeshUtils.ComputeTriangleTexCoords(
  aVertices: TAbstractDataList; anIndices: TIntegerArray): TVec2List;
var
  NewTexCoords: TVec2List;
  T, E1, E2, E3, E3T: integer;
  p0, p1, p2, dp0, dp1, fNormal, fTangent, fBinormal: TVector;
  TBN: TMatrix;
begin
  if Length(anIndices) = 0 then
    exit(ComputeTriangleTexCoords(aVertices));
  Assert(Length(anIndices) = aVertices.Count);
  NewTexCoords := nil;

  try
    NewTexCoords := TVec2List.Create;
    NewTexCoords.Count := Length(anIndices);

    for T := 0 to Length(anIndices) div 3 - 1 do
    begin
      E3T := 3 * T;
      E1 := anIndices[E3T];
      E2 := anIndices[E3T+1];
      E3 := anIndices[E3T+2];
      p0 := aVertices.GetItemAsVector(E1);
      p1 := aVertices.GetItemAsVector(E2);
      p2 := aVertices.GetItemAsVector(E3);

      // Compute the edge vectors
      dp0 := p1 - p0;
      dp1 := p2 - p0;

      // Compute the face TBN
      fNormal := dp0.Cross(dp1);
      fNormal := fNormal.Normalize;
      fTangent := dp0;
      fTangent := fTangent.Normalize;
      fBinormal := fNormal.Cross(fTangent);
      TBN.Row[0] := fTangent.vec4;
      TBN.Row[1] := fBinormal.vec4;
      TBN.Row[2] := fNormal.vec4;
      TBN := TBN.Invert;

      p0 := TBN.Transform(p0);
      NewTexCoords[E1] := p0.Vec2;

      p1 := TBN.Transform(p1);
      NewTexCoords[E2] := p1.Vec2;

      p2 := TBN.Transform(p2);
      NewTexCoords[E3] := p2.Vec2;
    end;
  except
    NewTexCoords.Free;
    raise;
  end;

  Result := NewTexCoords;
end;

class procedure MeshUtils.Triangulate(aFaceType: TFaceType;
  var anIndices: TIntegerArray);
var
  NewElements: TIntegerList;
  J, E: Integer;
  stripCount, prevIndex1, prevIndex2: Integer;
  prevIndex, centerIndex, fansCount: Integer;
  degenerate: Boolean;

  procedure GiveResult;
  begin
    SetLength(anIndices, NewElements.Count);
    Move(NewElements.GetItemAddr(0)^, anIndices[0],
      SizeOf(Integer) * NewElements.Count);
    NewElements.Destroy;
  end;

begin
  Assert(aFaceType in [ftTriangles, ftTriangleStrip, ftTriangleFan]);

  case aFaceType of

    ftTriangleStrip:

      begin
        NewElements := TIntegerList.Create;
        stripCount := 0;
        prevIndex1 := 0;
        prevIndex2 := 0;
        for J := 0 to High(anIndices) do
        begin
          E := anIndices[J];
          if stripCount > 2 then
          begin
            // Check for restart index
            if J = RestartIndex then
            begin
              stripCount := 0;
              continue;
            end
            // Check for degenerate triangles
            else if E = prevIndex1 then
            begin
              continue;
            end
            else if prevIndex1 = prevIndex2 then
            begin
              stripCount := 0;
              continue;
            end;
            if (stripCount and 1) = 0 then
            begin
              NewElements.Add(prevIndex2);
              NewElements.Add(prevIndex1);
            end
            else
            begin
              NewElements.Add(prevIndex1);
              NewElements.Add(prevIndex2);
            end;
          end
          else if stripCount = 2 then
          begin
            NewElements.Add(E);
            NewElements[NewElements.Count - 2] := prevIndex1;
            prevIndex2 := prevIndex1;
            prevIndex1 := E;
            inc(stripCount);
            continue;
          end;
          NewElements.Add(E);
          prevIndex2 := prevIndex1;
          prevIndex1 := E;
          inc(stripCount);
        end;
        GiveResult
      end;

    ftTriangleFan:

      begin
        NewElements := TIntegerList.Create;
        fansCount := 0;
        prevIndex := 0;
        degenerate := false;
        centerIndex := anIndices[0];
        for J := 0 to High(anIndices) do
        begin
          E := anIndices[J];
          if fansCount > 2 then
          begin
            // Check for restart index
            if E = RestartIndex then
            begin
              fansCount := 0;
              continue;
            end
            // Check for degenerate triangles
            else if E = prevIndex then
            begin
              degenerate := true;
              continue;
            end
            else if degenerate then
            begin
              degenerate := false;
              fansCount := 0;
              continue;
            end;
            NewElements.Add(centerIndex);
            NewElements.Add(prevIndex);
          end
          else if fansCount = 0 then
            centerIndex := E;
          NewElements.Add(E);
          prevIndex := E;
          inc(fansCount);
        end;
        GiveResult
      end;
  end;
end;

class procedure MeshUtils.UnWeldVertices(var anAttribs
  : array of TAbstractDataList; var anIndices: TIntegerArray;
  aVertexAttribIndex: Integer);
var
  NewAttribs: array of TAbstractDataList;
  I, a, E: Integer;
begin
  SetLength(NewAttribs, Length(anAttribs));
  for a := 0 to High(anAttribs) do
    NewAttribs[a] := TAbstractDataListClass(anAttribs[a].ClassType).Create;

  for I := 0 to High(anIndices) do
  begin
    E := anIndices[I];
    for a := 0 to High(anAttribs) do
      NewAttribs[a].AddRaw(anAttribs[a].GetItemAddr(E));
    anIndices[I] := I;
  end;
end;

class procedure MeshUtils.WeldVertices(var anAttribs
  : array of TAbstractDataList; var anIndices: TIntegerArray);
var
  I, J, ItemSize: Integer;
  E, E_, Index: Integer;
  vertexKey: Double;
  VertexHashMap: TVertexHashMap;
  VertexHashKey: TDoubleList;
  bFind: Boolean;
  StoreBuffers: array of TAbstractDataList;
  StoreIndices: TIntegerArray;

  procedure CalcHashKay;
  var
    pKey, pVertex: PByteArray;
    K, P, W: Integer;
  begin
    vertexKey := 0;
    pKey := @vertexKey;
    P := 0;
    pVertex := StoreBuffers[0].GetItemAddr(E);
    for K := ItemSize - 1 downto 0 do
    begin
      W := pKey[P] + pVertex[K];
      pKey[P] := W and 255;
      inc(P);
      P := P and 7;
    end;
    VertexHashKey[I] := vertexKey;
  end;

  function IsVertexEqual(const Index1, Index2: Integer): Boolean;
  var
    a: Integer;
  begin
    if Index1 <> Index2 then
    begin
      for a := 0 to High(StoreBuffers) do
        if not StoreBuffers[a].IsItemsEqual(Index1, Index2) then
          exit(false);
    end;
    result := true;
  end;

  procedure CopyVertex(n: Integer);
  var
    a: Integer;
  begin
    for a := 0 to High(anAttribs) do
      anAttribs[a].AddRaw(StoreBuffers[a].GetItemAddr(n));
    inc(Index);
  end;

var
  HasHash: Boolean;
  a: Integer;
begin
  // Calculate hash keys
  VertexHashMap := TVertexHashMap.Create(CompareVertexKey, CompareVertex);
  VertexHashKey := TDoubleList.Create;

  try
    VertexHashKey.Count := Length(anIndices);
    SetLength(vAttribs, Length(anAttribs));
    for a := 0 to High(anAttribs) do
      vAttribs[A] := anAttribs[A];
    E_ := 0; // Drop compilator warning
    ItemSize := anAttribs[0].ItemSize;
    SetLength(StoreBuffers, Length(anAttribs));
    for a := 0 to High(anAttribs) do
      StoreBuffers[a] := anAttribs[a];
    StoreIndices := Copy(anIndices, 0, Length(anIndices));

    for I := 0 to High(anIndices) do
    begin
      E := anIndices[I];
      if E = RestartIndex then
        continue;
      CalcHashKay;
      if VertexHashMap.Find(vertexKey, J) then
      begin
        E_ := anIndices[J];
        HasHash := (E_ >= E) or not IsVertexEqual(E, E_);
      end
      else
        HasHash := true;
      if HasHash then
        VertexHashMap.Add(vertexKey, I);
    end;

    for a := 0 to High(anAttribs) do
      anAttribs[a] := TAbstractDataListClass(anAttribs[a].ClassType).Create;

    // Remap element buffer, fill new attributes list
    Index := 0;
    for I := 0 to High(anAttribs) do
    begin
      E := anIndices[I];
      if E = RestartIndex then
        continue;

      bFind := false;
      vertexKey := VertexHashKey[I];
      if VertexHashMap.Find(vertexKey, J) then
      begin
        repeat
          E_ := StoreIndices[J];
          bFind := IsVertexEqual(E, E_);
          if bFind then
            break;
        until not VertexHashMap.NextDublicate(J);
      end;
      if not bFind then
        E_ := E;

      if E_ >= E then
      begin
        anIndices[I] := Index;
        CopyVertex(E);
      end
      else
      begin
        anIndices[I] := anIndices[J];
      end;
    end;

  finally
    VertexHashMap.Destroy;
    VertexHashKey.Destroy;
  end;
end;

initialization

MeshUtils.RestartIndex := -1;

end.
