unit uImageLoader;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}LCLType,LCLIntf,{$ENDIF}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF} Graphics, Classes,{$IFNDEF FPC} JPEG,{$ENDIF} SysUtils,
  {$IFNDEF FPC} jpegdec,{$ENDIF}
  uMiscUtils, uBaseTypes, uRenderResource, uImageFormats;

Type
  {$I DDSTypes.inc}
Type

  TImageLoader = class(TImageHolder)
  private
    procedure SetDDSFormat(const dds: TDDSImage);
  public
    procedure LoadImageFromStream(aStream: TStream); override;
  end;

implementation

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

procedure flipSurface(chgData: Pbyte; w, h, d: integer; DDSDesc: TImageHolder); overload;
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
    case DDSDesc.ImageFormat of
      IF_RGBA_DXT1: flipblocks := flip_blocks_dxtc1;
      IF_RGBA_DXT3: flipblocks := flip_blocks_dxtc3;
      IF_RGBA_DXT5: flipblocks := flip_blocks_dxtc5;
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

function ReservCompMem(bs: integer; const desc: TImageHolder): integer;
var i,s: integer;
    mw,mh,md,ms,offset: integer;
begin
  with desc do begin
    mw:=Width; mh:=Height; md:=max(1, Depth);
    offset:=0; s:=0;
    for i:=0 to LevelsCount-1 do begin
      if mw=0 then mw:=1; if mh=0 then mh:=1; if md=0 then md:=1;
      ms:=trunc(max(1, mw / 4) * max(1, mh / 4)*bs)*md; s:=s+ms;
      LODS[i].Offset:=offset; offset := offset + ms;
      LODS[i].Width:=mw; LODS[i].Height:=mh;
      LODS[i].Size:=ms; LODS[i].Depth:=md;
      mw:=mw shr 1; mh:=mh shr 1;
      if not isTextureArray then md:=md shr 1;
    end; result:=s;
  end;
end;

function ReservUncompMem(bpp: byte; const desc: TImageHolder): integer;
var i,s: integer;
    mw,mh,md,ms,offset: integer;
    b: byte;
begin
  with desc do begin
    mw:=width; mh:=height; md:=max(1, depth);
    offset:=0; s:=0; b:=(bpp); i:=0;
    repeat
      if mw=0 then mw:=1; if mh=0 then mh:=1;  if md=0 then md:=1;
      ms:=mw*mh*md*b; s:=s+ms;
      LODS[i].Offset:=offset; offset := offset + ms;
      LODS[i].Width:=mw; LODS[i].Height:=mh; LODS[i].Depth:=md;
      LODS[i].Size:=ms; LODS[i].Depth:=1;
      mw:=mw shr 1; mh:=mh shr 1;
      if not isTextureArray then md:=md shr 1;
      inc(i);
    until (i=LevelsCount) or (mw+mh=0);
    if isCubeMap then result := s*6
    else if isTextureArray then result := s*Depth
    else result := s;
    DiscardLods;
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

{ TImageLoader }

procedure TImageLoader.LoadImageFromStream(aStream: TStream);
var dds: TDDSImage;
    i: integer;
    buffSize: cardinal;
    p: pointer;
    f,FaceCount: cardinal;
    CubeMap: boolean;
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
  with dds.header do begin
    FWidth:=dwWidth; FHeight:=dwHeight; FDepth:=dwDepth;
    if ((dwCaps2 and DDSCAPS2_VOLUME) <> 0) and (dwDepth > 0)
    then FDepth := dwDepth else FDepth := 0;
    if (dwFlags and DDSD_MIPMAPCOUNT) > 0
    then FLevels := dwMipMapCount else FLevels:= 1;
    FLevels := 1;
    CubeMap:=((dwCaps2 and DDSCAPS2_CUBEMAP_ALLFACES)=DDSCAPS2_CUBEMAP_ALLFACES)
      or (dds.header10.miscFlag=DDS_RESOURCE_MISC_TEXTURECUBE);
    if CubeMap then assert(FWidth=FHeight,'Invalid cubemap');
    //TextureArray:=(dds.header10.arraySize>1) and (not CubeMap);
    //Compressed:=isCompressedFormat(dds);
    if CubeMap then FaceCount:=6 else FaceCount:=1;

    SetDDSFormat(dds);
    assert(FImageFormat <> IF_UNKNOWN, 'Unsupported dds format!');
    ImageFormat := FImageFormat;

    if Compressed then
      buffsize:=ReservCompMem(ElementSize,self)
    else buffsize:=ReservUncompMem(ElementSize,self);
    getmem(FData, buffsize);
    aStream.Read(FData^,buffSize*FaceCount);

    FReservedMem:=buffSize*FaceCount;
    FDataSize := FReservedMem;
    if (not Compressed) and ((ddspf.dwRGBBitCount=32) or (ElementSize=4))
    then SwapARGB(FData,buffSize div 4);
    //if not Cubemap then
    for f:=0 to FaceCount-1 do
      for i:=0 to FLevels-1 do begin
        p:=pointer(cardinal(FData)+FLODS[i].Offset+buffsize*f);
        flipSurface(p,FLODS[i].width,FLODS[i].height,1,self);
      end;
  end;
end;

procedure TImageLoader.SetDDSFormat(const dds: TDDSImage);
begin
  with dds.header do begin
    // figure out what the image format is
    if (ddspf.dwFlags and DDPF_FOURCC<>0) then begin
        case ddspf.dwFourCC of
            FOURCC_DXT1: FImageFormat := IF_RGBA_DXT1;
            FOURCC_DXT2: FImageFormat := IF_UNKNOWN;
            FOURCC_DXT3: FImageFormat := IF_RGBA_DXT3;
            FOURCC_DXT4: FImageFormat := IF_UNKNOWN;
            FOURCC_DXT5: FImageFormat := IF_RGBA_DXT5;

	    FOURCC_ATI1: FImageFormat := IF_UNKNOWN;
	    FOURCC_ATI2: FImageFormat := IF_UNKNOWN;

            FOURCC_R8G8B8: FImageFormat := IF_BGR8UI;
            FOURCC_A8R8G8B8: FImageFormat := IF_BGRA8UI;
            FOURCC_X8R8G8B8: FImageFormat := IF_BGR8UI;

            FOURCC_R5G6B5: FImageFormat := IF_RGB565;
            FOURCC_A8: FImageFormat := IF_Red8UI;

            FOURCC_A2B10G10R10: FImageFormat := IF_RGB10A2UI;
            FOURCC_A8B8G8R8: FImageFormat := IF_RGBA8UI;
            FOURCC_X8B8G8R8: FImageFormat := IF_RGB8UI;
            FOURCC_A2R10G10B10: begin
                FImageFormat := IF_RGB10A2UI;
                TImageFormatBits.SetReversFormat(FImageFormat, true);
                end;

            FOURCC_A16B16G16R16: FImageFormat := IF_RGBA16UI;
            FOURCC_L8: FImageFormat := IF_Red8UI;
            FOURCC_A8L8: FImageFormat := IF_RG8UI;
            FOURCC_L16: FImageFormat := IF_Red16UI;
            FOURCC_R16F: FImageFormat := IF_Red16F;

            FOURCC_A16B16G16R16F: FImageFormat := IF_RGBA16F;
            FOURCC_R32F: FImageFormat := IF_Red32F;
            FOURCC_A32B32G32R32F: FImageFormat := IF_RGBA32F;

            FOURCC_UNKNOWN: FImageFormat := IF_UNKNOWN;
            FOURCC_X1R5G5B5: FImageFormat := IF_UNKNOWN;
            FOURCC_A1R5G5B5: FImageFormat := IF_UNKNOWN;
            FOURCC_A4R4G4B4: FImageFormat := IF_UNKNOWN;
            FOURCC_R3G3B2: FImageFormat := IF_UNKNOWN;
            FOURCC_A8R3G3B2: FImageFormat := IF_UNKNOWN;
            FOURCC_X4R4G4B4: FImageFormat := IF_UNKNOWN;
            FOURCC_A4L4: FImageFormat := IF_UNKNOWN;
            FOURCC_D16_LOCKABLE: FImageFormat := IF_Depth16;
            FOURCC_D32: FImageFormat := IF_Depth32;
            FOURCC_D24X8: FImageFormat := IF_Depth24;
            FOURCC_D16: FImageFormat := IF_Depth16;
            FOURCC_D32F_LOCKABLE: TImageFormatBits.SetBitsDepth(FImageFormat, 32, true);
            FOURCC_G16R16: FImageFormat := IF_RG16UI;
            FOURCC_G16R16F: FImageFormat := IF_RG16F;
            FOURCC_G32R32F: FImageFormat := IF_RG32F;
            FOURCC_Q16W16V16U16: FImageFormat := IF_RGBA16I;
                //these are unsupported for now
            else assert(false,'Unsupported format');
        end;
    end else begin
      if ddspf.dwFourCC=0 then begin
        case ddspf.dwRGBBitCount of
          32: begin
            if ddspf.dwRBitMask<ddspf.dwBBitMask
            then FImageFormat:=IF_RGBA8UI
            else FImageFormat:=IF_BGRA8UI;
          end;
          24: begin
            if ddspf.dwRBitMask<ddspf.dwBBitMask
            then FImageFormat:=IF_RGB8UI
            else FImageFormat:=IF_BGR8UI;
          end;
          16: begin
            FImageFormat:=IF_RG8UI;
            if ddspf.dwRBitMask>ddspf.dwBBitMask
            then TImageFormatBits.SetReversFormat(FImageFormat, true);
          end;
          8: begin
            FImageFormat:=IF_Red8UI;
          end;
        end;
      end;
    end;
    if FImageFormat <> IF_UNKNOWN then exit;
    if (ddspf.dwFlags = DDPF_RGBA) and (ddspf.dwRGBBitCount = 32) then begin
        FImageFormat:=IF_BGRA8UI;
    end else if (ddspf.dwFlags = DDPF_RGB) and (ddspf.dwRGBBitCount = 32) then begin
        FImageFormat:=IF_RGBA8UI;
    end else if (ddspf.dwFlags = DDPF_RGB) and (ddspf.dwRGBBitCount = 24) then begin
        FImageFormat:=IF_BGR8UI;
    end else if (ddspf.dwRGBBitCount = 8) then begin
	FImageFormat:=IF_Red8UI;
    end else assert(false,'Unsupported format');
  end;
end;

end.
