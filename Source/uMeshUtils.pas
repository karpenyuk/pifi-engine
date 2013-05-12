unit uMeshUtils;

interface

uses
  uBaseTypes, uLists, uDataAccess, uVMath;

type

  MeshUtils = class
  public
    class var RestartIndex: Integer;
    // Vertices welding
    class procedure WeldVertices(const anInAttribs: array of IVectorDataAccess;
      out anOutAttribs: TAbstractDataListArray;
      var anIndices: TIntegerArray);
    // Makes all vertices unique, indices is just non repeated enumerate
    class procedure UnWeldVertices(const anInAttribs: array of TAbstractDataList;
      out anOutAttribs: TAbstractDataListArray;
      var anIndices: TIntegerArray);
    // Converts strips and fans to single triangles
    class procedure Triangulate(aFaceType: TFaceType;
      var anIndices: TIntegerArray);
    // Makes normals
    class function ComputeTriangleNormals(ASmooth: Boolean;
      aVertices: IVectorDataAccess; anIndices: TIntegerArray): TVec3List;
    // Makes tangents
    class procedure ComputeTriangleTangents(aVertices, aTexCoors,
      aNormals: IVectorDataAccess; anIndices: TIntegerArray;
      var aTangens, aBinormal: TAbstractDataList);
    // Makes texture coordinates for triangles
    class function ComputeTriangleTexCoords(aVertices: IVectorDataAccess)
      : TVec2List; overload;
    class function ComputeTriangleTexCoords(aVertices: IVectorDataAccess;
      anIndices: TIntegerArray): TVec2List; overload;
    // Makes indices for adjancency triangles
    class procedure ComputeTriangleAdjacency(Vertices: IVectorDataAccess;
      anIndices: TIntegerArray;
      var anAdjacencyIndices: TIntegerArray);
    // Join the geometry fromn income to storage
    class procedure Join(var aStorageAttribs: TAbstractDataListArray;
      const anIncomeAttribs: TAbstractDataListArray;
      var StorageIndices: TIntegerArray;
      const anIncomeIndices: TIntegerArray); overload;
    class procedure Join(var StorageIndices: TIntegerArray;
      const anIncomeIndices: TIntegerArray; anIndexOffset: Integer); overload;
    // Bruteforce ray cast inteersect for triangles
    class function RayCastIntersect(const aVertices: IVectorDataAccess;
      const aNormals: IVectorDataAccess;
      const anIndices: TIntegerArray;
      const aRayStart, aRayVector: TVector;
      out anIntersectPoint, anIntersectNormal: TVector): Boolean;
  end;

implementation

uses
  uMath, uGenericsRBTree;

type
  TIntIntRBTree = GRedBlackTree<Integer, Integer>;
  TVertexHashMap = GRedBlackTree<Double, Integer>;
  Vec4ui = array [0 .. 3] of LongInt;
  PVec4ui = ^Vec4ui;

function RayCastTriangleIntersect(const rayStart, rayVector: TVector;
  const p1, p2, p3: TVector;
  intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): Boolean;
var
  pvec: TVector;
  v1, v2, qvec, tvec: TVector;
  t, u, v, det, invDet: Single;
begin
  v1 := p2 - p1;
  v2 := p3 - p1;
  pvec := rayVector.Cross(v2);
  det := v1.Dot(pvec);
  if ((det < 1E-30) and (det > -1E-30)) then
    Exit(False);
  invDet := 1.0 / det;
  tvec := rayStart - p1;
  u := tvec.Dot(pvec) * invDet;
  if (u < 0) or (u > 1) then
    Exit(False)
  else
  begin
    qvec := tvec.Cross(v1);
    v := rayVector.Dot(qvec) * invDet;
    Result := (v >= 0) and (u + v <= 1);
    if Result then
    begin
      t := v2.Dot(qvec) * invDet;
      if t > 0 then
      begin
        if intersectPoint <> nil then
            intersectPoint^ := rayStart.Combine(rayVector, 1, t);
        if intersectNormal <> nil then
            intersectNormal^ := v1.Cross(v2);
      end
      else Result := False;
    end;
  end;
end;

function CompareVertexKey(const Item1, Item2: Double): Integer;
begin
  if Item1 < Item2 then
      Exit(-1)
  else if Item1 = Item2 then
      Exit(0)
  else
      Result := 1;
end;

threadvar
  vAttribs: array of IVectorDataAccess;
vIndices:
^TIntegerArray;

function CompareVertex(const Item1, Item2: Integer): Boolean;
var
  a: Integer;
  Idx1, Idx2: Integer;
begin
  if Item1 <> Item2 then
  begin
    Idx1 := vIndices^[Item1];
    Idx2 := vIndices^[Item2];
    for a := 0 to High(vAttribs) do
      if not vAttribs[a].IsItemsEqual(Idx1, Idx2) then
          Exit(False);
  end;
  Result := true;
end;

function CompareIntegerValue(const Item1, Item2: Integer): Boolean;
begin
  Result := Item1 = Item2;
end;

class procedure MeshUtils.ComputeTriangleAdjacency(Vertices: IVectorDataAccess;
  anIndices: TIntegerArray;
  var anAdjacencyIndices: TIntegerArray);
var
  edgeInfo: TTriangleEdgeInfoArray;
  triangleNum: Integer;
  NewIndices: TIntegerList;

  procedure joinTriangles(tri1: Integer; edge1: cardinal; tri2: Integer;
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
    i: Integer;
    doubleTri: Integer;
    otherEdge: Integer;
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
              Exit;
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
              Exit;
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
              Exit;
            end;
    end;

    // Only connect a triangle to a triangle with the exact
    // same three vertices as a last resort.
    if doubleTri >= 0 then
        joinTriangles(doubleTri, otherEdge, triangle, edge);
  end;

  procedure CheckForBogusAdjacency;

    function AdjacentEdge(x, n: Integer): Integer;
    begin
      Result := (x shr (2 * n)) and 3;
    end;

  var
    i, J: Integer;
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

  function AdjacentEdge(x, n: Integer): Integer;
  begin
    Result := (x shr (2 * n)) and 3;
  end;

var
  i, J, K: Integer;
  vertexIndex, tri, adjtri: PVec4ui;
  n, ii, jj: Integer;
begin
  assert(Length(anIndices) mod 3 = 0);

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

      setlength(anAdjacencyIndices, 2 * NewIndices.Count);
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
      setlength(anAdjacencyIndices, 0);
    end;

  finally
    NewIndices.Free;
  end;
end;

class function MeshUtils.ComputeTriangleNormals(ASmooth: Boolean;
  aVertices: IVectorDataAccess; anIndices: TIntegerArray): TVec3List;
var
  i, J, E, E_, t, EJ: Integer;
  PBIndices: TIntegerArray;
  p0, p1, p2, dp0, dp1, fNormal, nNormal, cNormal: TVector;
  NewNormals, Normals: TVec3List;
  NewNormalIndices: TIntegerList;
  collisionMap: TIntIntRBTree;
  Agrees: Boolean;
begin
  assert(Length(anIndices) mod 3 = 0);
  assert((ASmooth and (Length(anIndices) = aVertices.Count)) or
    (Length(anIndices) = 0));

  NewNormals := nil;
  NewNormalIndices := nil;
  collisionMap := nil;

  try
    if ASmooth then
    begin
      // Делаем сваривание вершим по равенству их позиций
      setlength(PBIndices, Length(anIndices));
      for i := 1 to High(anIndices) do
      begin
        E := anIndices[i];
        PBIndices[i] := E;
        for J := 0 to i - 1 do
        begin
          E_ := PBIndices[J];
          if E = E_ then
              continue;
          if aVertices.IsItemsEqual(E, E_) then
          begin
            PBIndices[i] := E_;
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
      setlength(PBIndices, aVertices.Count);
      setlength(anIndices, aVertices.Count);
      for i := 0 to aVertices.Count - 1 do
      begin
        PBIndices[i] := i;
        anIndices[i] := i;
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
    for t := 0 to Length(PBIndices) div 3 - 1 do
    begin
      E := 3 * t;
      p0 := aVertices.Items[PBIndices[E]];
      p1 := aVertices.Items[PBIndices[E + 1]];
      p2 := aVertices.Items[PBIndices[E + 2]];

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
            Agrees := False;
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
    for i := 0 to NewNormalIndices.Count - 1 do
    begin
      E := NewNormalIndices[i];
      cNormal.vec3 := NewNormals[E];
      cNormal.SetNormalize;
      E_ := anIndices[i];
      Normals[E_] := cNormal.vec3;
    end;

  finally
    NewNormals.Free;
    NewNormalIndices.Free;
    collisionMap.Free;
  end;

  Result := Normals;
end;

class procedure MeshUtils.ComputeTriangleTangents(aVertices, aTexCoors,
  aNormals: IVectorDataAccess; anIndices: TIntegerArray;
  var aTangens, aBinormal: TAbstractDataList);
var
  a, t, i, J, E, EJ, E_: Integer;
  p0, p1, p2, dp0, dp1, st0, st1, st2, dst0, dst1, fTangent, nTangent,
    cTangent: TVector;
  factor: Single;
  newTangents: TVec3List;
  newTangentIndices: TIntegerList;
  collisionMap: TIntIntRBTree;
  Agrees: Boolean;
begin
  assert(Length(anIndices) mod 3 = 0);
  assert((aVertices.Count = aTexCoors.Count));
  assert((Length(anIndices) = aVertices.Count) or (Length(anIndices) = 0));

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
    for t := 0 to Length(anIndices) div 3 - 1 do
    begin
      E := 3 * t;
      p0 := aVertices.Items[anIndices[E]];
      p1 := aVertices.Items[anIndices[E + 1]];
      p2 := aVertices.Items[anIndices[E + 2]];
      st0 := aVertices.Items[anIndices[E]];
      st1 := aVertices.Items[anIndices[E + 1]];
      st2 := aVertices.Items[anIndices[E + 2]];

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
            Agrees := False;
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

    for i := 0 to newTangentIndices.Count - 1 do
    begin
      E := newTangentIndices[i];
      E_ := anIndices[i];
      cTangent.vec3 := newTangents[E];
      aTangens.SetItemAsVector(E_, cTangent);
    end;

    if Assigned(aNormals) then
    begin
      if not Assigned(aBinormal) then
          aBinormal := TVec3List.Create;
      aBinormal.Count := aNormals.Count;
      for i := 0 to aNormals.Count - 1 do
      begin
        cTangent := aTangens.GetItemAsVector(i);
        nTangent := aNormals.Items[i];
        fTangent := nTangent.Cross(cTangent);
        aBinormal.SetItemAsVector(i, fTangent);
      end;
    end;

  finally
    newTangents.Free;
    newTangentIndices.Free;
    collisionMap.Free;
  end;
end;

const
  cubeNormals: array [0 .. 5] of vec3 = (
    (1, 0, 0),
    (0, 1, 0),
    (0, 0, 1),
    (-1, 0, 0),
    (0, -1, 0),
    (0, 0, -1));
  cubeTangents: array [0 .. 5] of vec3 = (
    (0, -1, 0),
    (1, 0, 0),
    (1, 0, 0),
    (0, 1, 0),
    (-1, 0, 0),
    (-1, 0, 0));

class function MeshUtils.ComputeTriangleTexCoords(
  aVertices: IVectorDataAccess): TVec2List;
var
  extent: TExtents;
  K, W, maxW: Single;
  NewTexCoords: TVec2List;
  t, E3T, n, M: Integer;
  p0, p1, p2, dp0, dp1, fNormal, fTangent, fBinormal: TVector;
  TBN: TMatrix;
begin
  NewTexCoords := TVec2List.Create;
  NewTexCoords.Count := aVertices.Count;
  extent.Reset;
  for t := 0 to aVertices.Count - 1 do
      extent.Include(aVertices.Items[t]);
  p0 := extent.eMax - extent.eMin;
  K := TMath.Max(p0[0], p0[1]);
  K := TMath.Max(K, p0[2]);
  K := 1 / K;

  for t := 0 to aVertices.Count div 3 - 1 do
  begin
    E3T := 3 * t;
    p0 := aVertices.Items[E3T] - extent.eMin;
    p1 := aVertices.Items[E3T + 1] - extent.eMin;
    p2 := aVertices.Items[E3T + 2] - extent.eMin;
    p0.SetScale(K);
    p1.SetScale(K);
    p2.SetScale(K);

    // Compute the edge vectors
    dp0 := p1 - p0;
    dp1 := p2 - p0;

    fNormal := dp0.Cross(dp1);
    fNormal := fNormal.Normalize;
    maxW := 0;
    M := 0;
    for n := 0 to 5 do
    begin
      W := fNormal.Dot(cubeNormals[n]);
      if W > maxW then
      begin
        maxW := W;
        M := n;
      end;
    end;

    fNormal := cubeNormals[M];
    fTangent := cubeTangents[M];
    fBinormal := fNormal.Cross(fTangent);
    TBN.Row[0] := fTangent.vec4;
    TBN.Row[1] := fBinormal.vec4;
    TBN.Row[2] := fNormal.vec4;
    TBN := TBN.Invert;

    p0 := TBN.Transform(p0);
    NewTexCoords[E3T] := p0.Vec2;

    p1 := TBN.Transform(p1);
    NewTexCoords[E3T + 1] := p1.Vec2;

    p2 := TBN.Transform(p2);
    NewTexCoords[E3T + 2] := p2.Vec2;
  end;
  Result := NewTexCoords;
end;

class function MeshUtils.ComputeTriangleTexCoords(
  aVertices: IVectorDataAccess; anIndices: TIntegerArray): TVec2List;
var
  extent: TExtents;
  K, W, maxW: Single;
  NewTexCoords: TVec2List;
  t, E1, E2, E3, E3T, n, M: Integer;
  p0, p1, p2, dp0, dp1, fNormal, fTangent, fBinormal: TVector;
  TBN: TMatrix;
begin
  if Length(anIndices) = 0 then
      Exit(ComputeTriangleTexCoords(aVertices));
  assert(Length(anIndices) = aVertices.Count);
  NewTexCoords := TVec2List.Create;
  NewTexCoords.Count := Length(anIndices);

  try
    extent.Reset;
    for t := 0 to aVertices.Count - 1 do
        extent.Include(aVertices.Items[t]);
    p0 := extent.eMax - extent.eMin;
    K := TMath.Max(p0[0], p0[1]);
    K := TMath.Max(K, p0[2]);
    K := 1 / K;

    for t := 0 to Length(anIndices) div 3 - 1 do
    begin
      E3T := 3 * t;
      E1 := anIndices[E3T];
      E2 := anIndices[E3T + 1];
      E3 := anIndices[E3T + 2];
      p0 := aVertices.Items[E1] - extent.eMin;
      p1 := aVertices.Items[E2] - extent.eMin;
      p2 := aVertices.Items[E3] - extent.eMin;
      p0.SetScale(K);
      p1.SetScale(K);
      p2.SetScale(K);

      // Compute the edge vectors
      dp0 := p1 - p0;
      dp1 := p2 - p0;

      fNormal := dp0.Cross(dp1);
      fNormal := fNormal.Normalize;
      maxW := 0;
      M := 0;
      for n := 0 to 5 do
      begin
        W := fNormal.Dot(cubeNormals[n]);
        if W > maxW then
        begin
          maxW := W;
          M := n;
        end;
      end;

      fNormal := cubeNormals[M];
      fTangent := cubeTangents[M];
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

class procedure MeshUtils.Join(var StorageIndices: TIntegerArray;
  const anIncomeIndices: TIntegerArray; anIndexOffset: Integer);
var
  i, iLen, iStart: Integer;
begin
  iLen := Length(anIncomeIndices);
  if iLen > 0 then
  begin
    iStart := Length(StorageIndices);
    setlength(StorageIndices, iStart + iLen);
    Move(anIncomeIndices[0], StorageIndices[iStart], SizeOf(Integer) * iLen);
    if anIndexOffset > 0 then
      for i := iStart to High(StorageIndices) do
        if StorageIndices[i] <> RestartIndex then
            inc(StorageIndices[i], anIndexOffset);
  end;
end;

class function MeshUtils.RayCastIntersect(const aVertices: IVectorDataAccess;
  const aNormals: IVectorDataAccess;
  const anIndices: TIntegerArray; const aRayStart, aRayVector: TVector;
  out anIntersectPoint, anIntersectNormal: TVector): Boolean;
var
  i: Integer;
  pE: PVec4ui;
  Dis, minDis: Single;
  V1, V2, V3: TVector;
  iPoint, iNormal: TVector;

begin
  minDis := -1;

  for i := 0 to High(anIndices) div 3 - 1 do
  begin
    pE := @anIndices[i * 3];
    V1 := aVertices.Items[pE[0]];
    V2 := aVertices.Items[pE[1]];
    V3 := aVertices.Items[pE[2]];
    if RayCastTriangleIntersect(
      aRayStart, aRayVector,
      V1, V2, V3,
      @iPoint, @iNormal) then
    begin
      Dis := aRayStart.DistanceSqr(iPoint);
      if (Dis < minDis) or (minDis < 0) then
      begin
        minDis := Dis;
        anIntersectPoint := iPoint;
        if Assigned(aNormals) then
          anIntersectNormal := aNormals.Items[pE[0]]
        else
          anIntersectNormal := iNormal;
      end;
    end;
  end;

  Result := (minDis >= 0);
end;

class procedure MeshUtils.Join(var aStorageAttribs: TAbstractDataListArray;
  const anIncomeAttribs: TAbstractDataListArray;
  var StorageIndices: TIntegerArray; const anIncomeIndices: TIntegerArray);
var
  i, IndexOffset, iLen, iStart: Integer;
begin
  iLen := Length(anIncomeIndices);
  if Length(aStorageAttribs) = 0 then
  begin
    setlength(aStorageAttribs, Length(aStorageAttribs));
    for i := 0 to High(anIncomeAttribs) do
    begin
      aStorageAttribs[i] :=
        TAbstractDataListClass(anIncomeAttribs[i].ClassType).Create;
      aStorageAttribs[i].Join(anIncomeAttribs[i], TMatrix.IdentityMatrix);
    end;

    if iLen > 0 then
        StorageIndices := Copy(anIncomeIndices, 0, iLen)
    else
        setlength(StorageIndices, 0);
    Exit;
  end
  else
  begin
    assert(Length(aStorageAttribs) = Length(anIncomeAttribs));
    IndexOffset := aStorageAttribs[0].Count;
    for i := 0 to High(anIncomeAttribs) do
    begin
      assert(IndexOffset = aStorageAttribs[i].Count);
      aStorageAttribs[i].Join(anIncomeAttribs[i], TMatrix.IdentityMatrix);
    end;
    if iLen > 0 then
    begin
      iStart := Length(StorageIndices);
      setlength(StorageIndices, iStart + iLen);
      Move(anIncomeIndices[0], StorageIndices[iStart], SizeOf(Integer) * iLen);
      if IndexOffset > 0 then
        for i := iStart to High(StorageIndices) do
          if StorageIndices[i] <> RestartIndex then
              inc(StorageIndices[i], IndexOffset);
    end;
  end;
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
    setlength(anIndices, NewElements.Count);
    Move(NewElements.GetItemAddr(0)^, anIndices[0],
      SizeOf(Integer) * NewElements.Count);
    NewElements.Destroy;
  end;

begin
  assert(aFaceType in [ftTriangles, ftTriangleStrip, ftTriangleFan]);

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
        degenerate := False;
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
              degenerate := False;
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

class procedure MeshUtils.UnWeldVertices(
  const anInAttribs: array of TAbstractDataList;
  out anOutAttribs: TAbstractDataListArray;
  var anIndices: TIntegerArray);
var
  i, a, E: Integer;
begin
  setlength(anOutAttribs, Length(anInAttribs));
  for a := 0 to High(anInAttribs) do
      anOutAttribs[a] := TAbstractDataListClass
      (anInAttribs[a].ClassType).Create;

  for i := 0 to High(anIndices) do
  begin
    E := anIndices[i];
    for a := 0 to High(anInAttribs) do
        anOutAttribs[a].AddRaw(anInAttribs[a].GetItemAddr(E));
    anIndices[i] := i;
  end;
end;

class procedure MeshUtils.WeldVertices(
  const anInAttribs: array of IVectorDataAccess;
  out anOutAttribs: TAbstractDataListArray;
  var anIndices: TIntegerArray);
var
  i, J, ItemSize: Integer;
  E, E_, Index: Integer;
  vertexKey: Double;
  VertexHashMap: TVertexHashMap;
  VertexHashKey: TDoubleList;
  bFind: Boolean;
  WorkBuffers: array of IVectorDataAccess;
  StoreIndices: TIntegerArray;

  procedure CalcHashKay;
  var
    pKey, pVertex: PByteArray;
    K, P, W: Integer;
  begin
    vertexKey := 0;
    pKey := @vertexKey;
    P := 0;
    pVertex := WorkBuffers[0].GetItemAddress(E);
    for K := ItemSize - 1 downto 0 do
    begin
      W := pKey[P] + pVertex[K];
      pKey[P] := W and 255;
      inc(P);
      P := P and 7;
    end;
    VertexHashKey[i] := vertexKey;
  end;

  function IsVertexEqual(const Index1, Index2: Integer): Boolean;
  var
    a: Integer;
  begin
    if Index1 <> Index2 then
    begin
      for a := 0 to High(WorkBuffers) do
        if not WorkBuffers[a].IsItemsEqual(Index1, Index2) then
            Exit(False);
    end;
    Result := true;
  end;

  procedure CopyVertex(n: Integer);
  var
    a: Integer;
  begin
    for a := 0 to High(anOutAttribs) do
        anOutAttribs[a].AddRaw(WorkBuffers[a].GetItemAddress(n));
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
    setlength(vAttribs, Length(anInAttribs));
    setlength(WorkBuffers, Length(anInAttribs));
    for a := 0 to High(anInAttribs) do
    begin
      vAttribs[a] := anInAttribs[a];
      WorkBuffers[a] := anInAttribs[a];
    end;
    E_ := 0; // Drop compilator warning
    ItemSize := anInAttribs[0].ItemSize;
    vIndices := @anIndices;

    for i := 0 to High(anIndices) do
    begin
      E := anIndices[i];
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
          VertexHashMap.Add(vertexKey, i);
    end;

    StoreIndices := Copy(anIndices, 0, Length(anIndices));
    setlength(anOutAttribs, Length(anInAttribs));
    for a := 0 to High(anOutAttribs) do
    begin
      assert(anInAttribs[a].ItemType = vtFloat);
      case anInAttribs[a].ItemComponent of
        1: anOutAttribs[a] := TSingleList.Create;
        2: anOutAttribs[a] := TVec2List.Create;
        3: anOutAttribs[a] := TVec3List.Create;
        4: anOutAttribs[a] := TVec4List.Create;
      end;
      vAttribs[a] := TVectorDataAccess.Create(
        anOutAttribs[a].GetItemAddr(0),
        anInAttribs[a].ItemType,
        anInAttribs[a].ItemComponent,
        anInAttribs[a].ItemSize,
        anInAttribs[a].Count);
    end;

    // Remap element buffer, fill new attributes list
    Index := 0;
    for i := 0 to High(anIndices) do
    begin
      E := anIndices[i];
      if E = RestartIndex then
          continue;

      bFind := False;
      vertexKey := VertexHashKey[i];
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
        anIndices[i] := Index;
        CopyVertex(E);
      end
      else
      begin
        anIndices[i] := anIndices[J];
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
