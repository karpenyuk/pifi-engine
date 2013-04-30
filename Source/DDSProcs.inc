uses dglOpenGL;

type
{$I DDSTypes.inc}

function MAKEFOURCC(s: ansistring): cardinal;
begin
  result:=ord(s[1]) or (ord(s[2]) shl 8) or (ord(s[3]) shl 16) or (ord(s[4]) shl 24);
end;
{$IFNDEF FPC}
{$if CompilerVersion>17.0}{$REGION 'FlipBlocks'}{$ifend}
{$ENDIF}
  // flip a DXT1 color block
  ////////////////////////////////////////////////////////////
  procedure flip_blocks_dxtc1( data : PByte; numBlocks: integer);
  var
    curblock : PDXTColBlock;
    temp : byte;
    i : integer;
  begin
    curblock := PDXTColBlock( data );
    for i := 0 to  numBlocks-1 do begin
      temp := curblock.row[0];
      curblock.row[0] := curblock.row[3];
      curblock.row[3] := temp;
      temp := curblock.row[1];
      curblock.row[1] := curblock.row[2];
      curblock.row[2] := temp;

      Inc( curblock );
    end;
  end;

  // flip a DXT3 color block
  ////////////////////////////////////////////////////////////
  procedure flip_blocks_dxtc3( data: PByte; numBlocks: integer );
  var
    curblock : PDXTColBlock;
    alphablock : PDXT3AlphaBlock;
    tempS : word;
    tempB : byte;
    i : integer;
  begin
    curblock := PDXTColBlock( data );
    for i := 0 to numBlocks-1 do
    begin
      alphablock := PDXT3AlphaBlock( curblock );

      tempS := alphablock.row[0];
      alphablock.row[0] := alphablock.row[3];
      alphablock.row[3] := tempS;
      tempS := alphablock.row[1];
      alphablock.row[1] := alphablock.row[2];
      alphablock.row[2] := tempS;

      Inc( curblock );

      tempB := curblock.row[0];
      curblock.row[0] := curblock.row[3];
      curblock.row[3] := tempB;
      tempB := curblock.row[1];
      curblock.row[1] := curblock.row[2];
      curblock.row[2] := tempB;

      Inc( curblock );
    end;
  end;

  //
  // flip a DXT5 alpha block
  ////////////////////////////////////////////////////////////
  procedure flip_dxt5_alpha( block : PDXT5AlphaBlock);
  const
    mask = $00000007;          // bits = 00 00 01 11
  var
    gBits : array[0..3, 0..3] of byte;
    bits  : Integer;
  begin
    bits := 0;
    Move(block.row[0], bits, sizeof(byte) * 3);

    gBits[0][0] := byte(bits and mask);
    bits := bits shr 3;
    gBits[0][1] := byte(bits and mask);
    bits := bits shr 3;
    gBits[0][2] := byte(bits and mask);
    bits := bits shr 3;
    gBits[0][3] := byte(bits and mask);
    bits := bits shr 3;
    gBits[1][0] := byte(bits and mask);
    bits := bits shr 3;
    gBits[1][1] := byte(bits and mask);
    bits := bits shr 3;
    gBits[1][2] := byte(bits and mask);
    bits := bits shr 3;
    gBits[1][3] := byte(bits and mask);

    bits := 0;
    Move(block.row[3], bits, sizeof(byte) * 3);

    gBits[2][0] := byte(bits and mask);
    bits := bits shr 3;
    gBits[2][1] := byte(bits and mask);
    bits := bits shr 3;
    gBits[2][2] := byte(bits and mask);
    bits := bits shr 3;
    gBits[2][3] := byte(bits and mask);
    bits := bits shr 3;
    gBits[3][0] := byte(bits and mask);
    bits := bits shr 3;
    gBits[3][1] := byte(bits and mask);
    bits := bits shr 3;
    gBits[3][2] := byte(bits and mask);
    bits := bits shr 3;
    gBits[3][3] := byte(bits and mask);

    // clear existing alpha bits
    FillChar( block.row, sizeof(byte) * 6, 0);

    bits := block.row[0]+block.row[1]*$100+block.row[2]*$10000;

    bits := bits or (gBits[3][0] shl 0);
    bits := bits or (gBits[3][1] shl 3);
    bits := bits or (gBits[3][2] shl 6);
    bits := bits or (gBits[3][3] shl 9);

    bits := bits or (gBits[2][0] shl 12);
    bits := bits or (gBits[2][1] shl 15);
    bits := bits or (gBits[2][2] shl 18);
    bits := bits or (gBits[2][3] shl 21);

    block.row[0] := bits and $FF;
    block.row[1] := (bits shr 8) and $FF;
    block.row[2] := (bits shr 16) and $FF;

    bits := block.row[3]+block.row[4]*$100+block.row[5]*$10000;

    bits := bits or (gBits[1][0] shl 0);
    bits := bits or (gBits[1][1] shl 3);
    bits := bits or (gBits[1][2] shl 6);
    bits := bits or (gBits[1][3] shl 9);

    bits := bits or (gBits[0][0] shl 12);
    bits := bits or (gBits[0][1] shl 15);
    bits := bits or (gBits[0][2] shl 18);
    bits := bits or (gBits[0][3] shl 21);

    block.row[3] := bits and $FF;
    block.row[4] := (bits shr 8) and $FF;
    block.row[5] := (bits shr 16) and $FF;
  end;

  //
  // flip a DXT5 color block
  ////////////////////////////////////////////////////////////
  procedure flip_blocks_dxtc5( data: PByte; numBlocks: integer );
  var
    curblock : PDXTColBlock;
    temp : byte;
    i : integer;
  begin
    curblock := PDXTColBlock( data );
    for i := 0 to numBlocks-1 do
    begin
      flip_dxt5_alpha( PDXT5AlphaBlock( curblock ) );
      Inc( curblock );
      temp := curblock.row[0];
      curblock.row[0] := curblock.row[3];
      curblock.row[3] := temp;
      temp := curblock.row[1];
      curblock.row[1] := curblock.row[2];
      curblock.row[2] := temp;
      Inc( curblock );
    end;
  end;
{$IFNDEF FPC}
{$if CompilerVersion>17.0}{$ENDREGION}{$ifend}
{$ENDIF}

procedure flipSurface(chgData: Pbyte; w, h, d: integer; DDSDesc: TImageDesc); overload;
var
  lineSize: integer;
  sliceSize: integer;
  tempBuf: Pbyte;
  i, j: integer;
  top, bottom: Pbyte;
  flipblocks: procedure(data: Pbyte; size: integer);

begin
  if d = 0 then d := 1;

  if not DDSDesc.Compressed then begin
    lineSize := DDSDesc.ElementSize * w;
    sliceSize := lineSize * h;
    GetMem(tempBuf, lineSize);

    for i := 0 to d - 1 do begin
      top := chgData; Inc(top, i * sliceSize);
      bottom := top;  Inc(bottom, sliceSize - lineSize);

      for j := 0 to (h div 2) - 1 do begin
        Move(top^, tempBuf^, lineSize);
        Move(bottom^, top^, lineSize);
        Move(tempBuf^, bottom^, lineSize);
        Inc(top, lineSize);
        Dec(bottom, lineSize);
      end;
    end;
    FreeMem(tempBuf);
  end else begin
    w := (w + 3) div 4; h := (h + 3) div 4;
    case DDSDesc.ColorFormat of
      GL_COMPRESSED_RGBA_S3TC_DXT1_EXT: flipblocks := flip_blocks_dxtc1;
      GL_COMPRESSED_RGBA_S3TC_DXT3_EXT: flipblocks := flip_blocks_dxtc3;
      GL_COMPRESSED_RGBA_S3TC_DXT5_EXT: flipblocks := flip_blocks_dxtc5;
    else
      exit;
    end;

    lineSize := DDSDesc.ElementSize * w;
    sliceSize := lineSize * h;
    GetMem(tempBuf, lineSize);
    for i := 0 to d - 1 do begin
      top := chgData; Inc(top, i * sliceSize);
      bottom := top;  Inc(bottom, sliceSize - lineSize);

      for j := 0 to (h div 2) - 1 do begin
        if top = bottom then begin flipblocks(top, w); break; end;

        flipblocks(top, w); flipblocks(bottom, w);

        Move(top^, tempBuf^, lineSize);
        Move(bottom^, top^, lineSize);
        Move(tempBuf^, bottom^, lineSize);

        Inc(top, lineSize);
        Dec(bottom, lineSize);
      end;
    end;
    FreeMem(tempBuf);
  end;
end;

function ReservCompMem(bs: integer; var desc: TImageDesc): integer;
var i,s: integer;
    mw,mh,ms,offset: integer;
begin
  with desc do begin
    mw:=width; mh:=height;
    offset:=0; s:=0;
    for i:=0 to Levels-1 do begin
      if mw=0 then mw:=1; if mh=0 then mh:=1;
      ms:=trunc(max(1, mw / 4) * max(1, mh / 4)*bs); s:=s+ms;
      LODS[i].Offset:=offset; offset := offset + ms;
      LODS[i].Width:=mw; LODS[i].Height:=mh;
      LODS[i].Size:=ms; LODS[i].Depth:=1;
      mw:=mw shr 1; mh:=mh shr 1;
    end; result:=s;
    getmem(Data,s); DataSize:=s;
  end;
end;

procedure CompleatLods(var desc: PImageDesc; LodsCount: integer);
var i,j,w,h,x,y,b: integer;
    pb: PByteArray;
    c: integer;
begin
  b:=desc.ElementSize;
  pb:=PByteArray(desc.Data);
  with desc^ do begin
    for i:=Levels to LodsCount-1 do begin
      w:=LODS[i].Width; h:=LODS[i].Height;
      for y:=0 to h-1 do for x:=0 to w-1 do begin
        for j:=0 to b-1 do begin
          c:=
            pb[LODS[i-1].Offset+y*2*w*b+x*2*b+j]+
            pb[LODS[i-1].Offset+(y*2+1)*w*b+x*2*b+j]+
            pb[LODS[i-1].Offset+y*2*w*b+(x*2+1)*b+j]+
            pb[LODS[i-1].Offset+(y*2+1)*w*b+(x*2+1)*b+j];
          c:=c div 4;
          pb[LODS[i].Offset+y*w*b+x*b+j]:=c and $FF;
        end;
      end;
    end;
  end;
end;

function ReservUncompMem(bpp: byte; var desc: TImageDesc): integer;
var i,s: integer;
    mw,mh,ms,offset: integer;
    b: byte;
begin
  with desc do begin
    mw:=width; mh:=height;
    offset:=0; s:=0; b:=(bpp); i:=0;
    repeat
      if mw=0 then mw:=1; if mh=0 then mh:=1;
      ms:=mw*mh*b; s:=s+ms;
      LODS[i].Offset:=offset; offset := offset + ms;
      LODS[i].Width:=mw; LODS[i].Height:=mh;
      LODS[i].Size:=ms; LODS[i].Depth:=1;
      mw:=mw shr 1; mh:=mh shr 1; inc(i);
    until (i=Levels) or (mw+mh=0);
    if desc.CubeMap then getmem(Data,s*6)
    //else if desc.TextureArray then getmem(Data,s*Depth)
    else getmem(Data,s);
    DataSize:=s;

{    if (i<>Levels) and (Levels>1) then begin
      result:=LODS[Levels].Offset;
      CompleatLods(desc,i); Levels:=i;
    end else
}
    Levels:=1; result:=s;

{    for i:=0 to Levels-1 do begin
      if mw=0 then mw:=1; if mh=0 then mh:=1;
      ms:=mw*mh*b; s:=s+ms;
      LODS[i].Offset:=offset; offset := offset + ms;
      LODS[i].Width:=mw; LODS[i].Height:=mh;
      LODS[i].Size:=ms; LODS[i].Depth:=1;
      mw:=mw shr 1; mh:=mh shr 1;
    end;
}
//    result:=s;
  end;
end;

procedure SwapARGB(data: PInteger; count: integer);assembler;
asm
@loop:
  mov ecx, dword ptr [eax];
  mov byte ptr [eax], cl;
  shr ecx, 24
  mov byte ptr [eax+3], cl;
  add eax, 4;
  dec edx;
  jnz @loop;
end;

procedure SwapARGB16(data: PInteger; count: integer);assembler;
asm
@loop:
  mov cx, word ptr [eax];
  xchg cx, word ptr [eax+6];
  mov word ptr [eax], cx;
  add eax, 8;
  dec edx;
  jnz @loop;
end;

{$I DDSFormats.inc}

function DDSLoadFromStream(aStream: TStream): TImageDesc;
var dds: TDDSImage;
    i,mw,mh: integer;
    glFormat: cardinal;
    buffSize: cardinal;
    bs,ms: cardinal;
    offset: cardinal;
    p: pointer;
    f,FaceCount: integer;
begin
  aStream.Read(dds.dwMagic,4);
  assert(dds.dwMagic='DDS ','Invalid DDS file');
  aStream.Read(dds.header, Sizeof(TDDSHeader));
  assert((dds.header.dwSize=sizeof(DDS_HEADER)) and
    (dds.header.ddspf.dwSize=sizeof(DDS_PIXELFORMAT)),'Invalid DDS file');
  if (dds.header.ddspf.dwFlags and DDPF_FOURCC <> 0)
  and (dds.header.ddspf.dwFourCC = FOURCC_DX10)
  then aStream.Read(dds.header10, Sizeof(DDS_HEADER_DXT10));
  //new(result);
  with dds.header,result do begin
    Width:=dwWidth; Height:=dwHeight; Depth:=dwDepth;
    if ((dwCaps2 and DDSCAPS2_VOLUME) <> 0) and (dwDepth > 0)
    then Depth := dwDepth else Depth := 0;
    if (dwFlags and DDSD_MIPMAPCOUNT) > 0
    then Levels := dwMipMapCount else Levels:= 1;
    CubeMap:=((dwCaps2 and DDSCAPS2_CUBEMAP_ALLFACES)=DDSCAPS2_CUBEMAP_ALLFACES)
      or (dds.header10.miscFlag=DDS_RESOURCE_MISC_TEXTURECUBE);
    if CubeMap then assert(Width=Height,'Invalid cubemap');
    TextureArray:=(dds.header10.arraySize>1) and (not CubeMap);
    //Compressed:=isCompressedFormat(dds);
    if CubeMap then FaceCount:=6 else FaceCount:=1;

    DDSToGLFormats(dds,result);

    if Compressed then
      buffsize:=ReservCompMem(result.ElementSize,result)
    else buffsize:=ReservUncompMem(result.ElementSize,Result);
    aStream.Read(Result.Data^,buffSize*FaceCount);

    ReservedMem:=buffSize*FaceCount;
    if (not Compressed) and ((ddspf.dwRGBBitCount=32) or (result.ElementSize=4))
    then SwapARGB(data,buffSize div 4);
    //if not Cubemap then
    for f:=0 to FaceCount-1 do
    for i:=0 to result.Levels-1 do begin
      p:=pointer(integer(result.Data)+result.LODS[i].Offset+buffsize*f);
      flipSurface(p,result.LODS[i].width,result.LODS[i].height,1,result);
    end;
  end;
end;

function DDSLoadFromFile(FileName: string): TImageDesc;
var Stream: TFileStream;
begin
  Stream:=TFileStream.Create(FileName,fmOpenRead);
  result:=DDSLoadFromStream(Stream);
end;
