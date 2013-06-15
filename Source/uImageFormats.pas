unit uImageFormats;


interface


uses uMiscUtils;


Type


  TBaseImageFormat = (bfRed, bfRG, bfRGB, bfBGR, bfRGBA, bfBGRA, bfDepth,
    bfDepthStencil, bfCompressed, bfSpecial);


  TImagePixelFormat = (pfUByte, pfByte, pfUShort, pfShort, pfUInt, pfInt, pfFloat16, pfFloat,
    pfUB332, pfUS565, pfUS4444, pfUS5551, pfUI8888, pfUI1010102, pfI1010102,
    pfUS1555Rev, pfUB233Rev, pfUS565Rev, pfUS4444Rev, pfUI8888Rev, pfUI2101010Rev,
    pfI2101010Rev, pfI5999, pfI5999Rev, pfUI24_8, pfI10F11F11F, pfI10F11F11FRev, pfF32UB8);


  TDepthStencilFormat = (dfDepth16, dfDepth24, dfDepth32, dfDepth32F,
    dfStencilIndex8, dfDepth24Stencil8, dfDepth32FStencil8);


  TImageSpecialFormat = (sfR3G3B2, sfRGB565, sfRGB5A1, sfRGB10A2, sfRGB10A2UI,
    sfR11FG11FB10F, sfRGB9E5);


  TS3TCCompressedFormats = (cfRGB_DXT1, cfSRGB_DXT1, cfRGBA_DXT1,
    cfSRGBA_DXT1, cfRGBA_DXT3, cfSRGBA_DXT3, cfRGBA_DXT5, cfSRGBA_DXT5);


  TAbstractPixelFormatSelector<T> = class
    class function CreateInt8(aPixelFormat: TBaseImageFormat): T; virtual; abstract;
    class function CreateInt16(aPixelFormat: TBaseImageFormat): T; virtual; abstract;
    class function CreateInt32(aPixelFormat: TBaseImageFormat): T; virtual; abstract;
    class function CreateUInt8(aPixelFormat: TBaseImageFormat): T; virtual; abstract;
    class function CreateUInt16(aPixelFormat: TBaseImageFormat): T; virtual; abstract;
    class function CreateUInt32(aPixelFormat: TBaseImageFormat): T; virtual; abstract;
    class function CreateFloat16(aPixelFormat: TBaseImageFormat): T; virtual; abstract;
    class function CreateFloat32(aPixelFormat: TBaseImageFormat): T; virtual; abstract;


    class function CreateCompressed(aPixelFormat: TS3TCCompressedFormats): T; virtual; abstract;


    class function CreateDepthStencil(aDepthBit: byte; aStencil: boolean = false): T; virtual; abstract;


    class function CreateSpecial(aPixelFormat: TImageSpecialFormat): T; virtual; abstract;


    class function GetMemSize(aPixelFormat: TS3TCCompressedFormats;
      aWidth: integer; aHeight: integer = 1; aDepth: integer = 1): cardinal; overload;
    class function GetMemSize(aPixelFormat: TImagePixelFormat;
      aWidth: integer; aHeight: integer = 1; aDepth: integer = 1): cardinal; overload;
    class function GetMemSize(aPixelFormat: TDepthStencilFormat;
      aWidth: integer; aHeight: integer = 1; aDepth: integer = 1): cardinal; overload;
    class function GetMemSize(aPixelFormat: TImageSpecialFormat;
      aWidth: integer; aHeight: integer = 1; aDepth: integer = 1): cardinal; overload;
    class function GetMemSize(aFormatBits: cardinal; aWidth: integer;
      aHeight: integer = 1; aDepth: integer = 1; aMipMapping: boolean = false): cardinal; overload;


    class function GetPixelSize(aPixelFormat: TImagePixelFormat): cardinal; overload;
    class function GetPixelSize(aPixelFormat: TDepthStencilFormat): cardinal; overload;
    class function GetPixelSize(aPixelFormat: TS3TCCompressedFormats): cardinal; overload;
    class function GetPixelSize(aPixelFormat: TImageSpecialFormat): cardinal; overload;
    class function GetPixelSize(aPixelFormatBits: cardinal): cardinal; overload;


    class function GetMipmapsCount(Size: integer): byte;
  end;


  TImageFormatSelector = class (TAbstractPixelFormatSelector<cardinal>)
  public
    class function CreateInt8(aPixelFormat: TBaseImageFormat): cardinal; override;
    class function CreateInt16(aPixelFormat: TBaseImageFormat): cardinal; override;
    class function CreateInt32(aPixelFormat: TBaseImageFormat): cardinal; override;
    class function CreateUInt8(aPixelFormat: TBaseImageFormat): cardinal; override;
    class function CreateUInt16(aPixelFormat: TBaseImageFormat): cardinal; override;
    class function CreateUInt32(aPixelFormat: TBaseImageFormat): cardinal; override;
    class function CreateFloat16(aPixelFormat: TBaseImageFormat): cardinal; override;
    class function CreateFloat32(aPixelFormat: TBaseImageFormat): cardinal; override;


    class function CreateCompressed(aPixelFormat: TS3TCCompressedFormats): cardinal; override;


    class function CreateDepthStencil(aDepthBit: byte; aStencil: boolean = false): cardinal; override;


    class function CreateSpecial(aPixelFormat: TImageSpecialFormat): cardinal; override;


  end;


  //Bits 0,1 - Base format: 0 - Red, 1 - RG, 2 - RGB, 3 - RGBA
  //Bits 2 - Revers format flag: RGB -> BGR
  //Bits 3,4 - Value type: 0 - uint, 1 - int, 2 - float, 3 - depth/stencil
  //Bits 5,6 - Bits per component: 0 - 8, 1 - 16, 2 - 32, 3 - extended
  //Bits 7 - float flag bit (for bit 5,6)
  //Bits 7 = 1, 5,6 = 0 - compressed format, Bits 5,6,7 = 1 - special format flag
  //Extended bits
  //Bits 8 - stencil flag, Bits 5,6 set depth format as:
  //Bit 8 = 0, bits 5-6: 0 - Depth16, 1 - Depth24, 2 - Depth32(F), 3 - reserved
  //Bit 8 = 1, bits 5-6: 0 - StencilIndex8, 1 - Depth24Stencil8, 2 - Depth32FStencil8, 3 - reserved
  //Bits 10-12 - Special format index (0..7)
  //Bits 13-15 - Compressed format index (0..7)
  TImageFormatBits = class
    //Return Base Format flag
    class function isBaseFormat(aFormat: cardinal): boolean;
    //Return Float format flag
    class function isFloatFormat(aFormat: cardinal): boolean;
    //Return compressed flag
    class function isCompressedFormat(aFormat: cardinal): boolean;
    //Retund Depth/Stencil flag
    class function isDepthStencilFormat(aFormat: cardinal): boolean;
    //Return Special format flag
    class function isSpecialFormat(aFormat: cardinal): boolean;


    //Return Base BaseImageFormat from format bits
    class function GetBaseFormat(aFormat: cardinal): TBaseImageFormat;
    //Return Pixel Format from format bits
    class function GetPixelFormat(aFormat: cardinal): TImagePixelFormat;
    //Return Bits count per pixel from format bits
    class function GetBPP(aFormat: cardinal): byte;
    //Return Depth/Stencil format
    class function GetDepthStencilFormat(aFormat: cardinal): TDepthStencilFormat;
    //Return Special format
    class function GetSpecialFormat(aFormat: cardinal): TImageSpecialFormat;
    //Return compressed format
    class function GetCompressedFormat(aFormat: cardinal): TS3TCCompressedFormats;


    //Set base format bits
    class procedure SetBaseFormat(var aFormat: cardinal; aValue: TBaseImageFormat);
    //Set pixel format bits
    class procedure SetPixelFormat(var aFormat: cardinal; aValue: TImagePixelFormat);
    //Set Bits count per component
    class procedure SetBitsDepth(var aFormat: cardinal; aValue: byte);
    //Set depth/stencil format bits
    class procedure SetDepthStencilFormat(var aFormat: cardinal; aValue: TDepthStencilFormat);
    //Set special format bits
    class procedure SetSpecialFormat(var aFormat: cardinal; aValue: TImageSpecialFormat);
    //Set compressed format bits
    class procedure SetCompressedFormat(var aFormat: cardinal; aValue: TS3TCCompressedFormats);
  end;


const
  CComponentsCount: array[TBaseImageFormat] of byte =
    (1,2,3,3,4,4,1,1,1,1);
  CBasePixelSize: array[TImagePixelFormat] of byte =
    (1,1,2,2,4,4,2,4,1,2,2,2,4,4,4,2,1,2,2,4,4,4,4,4,4,4,4,5);
  CDepthStencilSize: array[TDepthStencilFormat] of byte =
    (2,4,4,4,1,4,5);
  CCompressedPixelSize: array[TS3TCCompressedFormats] of byte =
    (8,8,8,8,16,16,16,16);
  CSpecialFormatPixelSize: array[TImageSpecialFormat] of byte =
    (1, 2, 2, 4, 4, 4, 4);


//Format constants
  IF_Red8I		 = 8;
  IF_Red8UI		 = 0;
  IF_Red16I		 = 40;
  IF_Red16UI		 = 32;
  IF_Red32I		 = 72;
  IF_Red32UI		 = 64;
  IF_Red16F		 = 48;
  IF_Red32F		 = 80;
  IF_RG8I		 = 9;
  IF_RG8UI		 = 1;
  IF_RG16I		 = 41;
  IF_RG16UI		 = 33;
  IF_RG32I		 = 73;
  IF_RG32UI		 = 65;
  IF_RG16F		 = 49;
  IF_RG32F		 = 81;
  IF_RGB8I		 = 10;
  IF_RGB8UI		 = 2;
  IF_RGB16I		 = 42;
  IF_RGB16UI		 = 34;
  IF_RGB32I		 = 74;
  IF_RGB32UI		 = 66;
  IF_RGB16F		 = 50;
  IF_RGB32F		 = 82;
  IF_BGR8I		 = 14;
  IF_BGR8UI		 = 6;
  IF_BGR16I		 = 46;
  IF_BGR16UI		 = 38;
  IF_BGR32I		 = 78;
  IF_BGR32UI		 = 70;
  IF_BGR16F		 = 54;
  IF_BGR32F		 = 86;
  IF_RGBA8I		 = 11;
  IF_RGBA8UI		 = 3;
  IF_RGBA16I		 = 43;
  IF_RGBA16UI		 = 35;
  IF_RGBA32I		 = 75;
  IF_RGBA32UI		 = 67;
  IF_RGBA16F		 = 51;
  IF_RGBA32F		 = 83;
  IF_BGRA8I		 = 15;
  IF_BGRA8UI		 = 7;
  IF_BGRA16I		 = 47;
  IF_BGRA16UI		 = 39;
  IF_BGRA32I		 = 79;
  IF_BGRA32UI		 = 71;
  IF_BGRA16F		 = 55;
  IF_BGRA32F		 = 87;
  IF_Depth16		 = 24;
  IF_Depth24		 = 56;
  IF_Depth32		 = 88;
  IF_StencilIndex8	 = 280;
  IF_Depth24Stencil8	 = 312;
  IF_Depth32FStencil8	 = 472;
  IF_R3G3B2		 = 224;
  IF_RGB565		 = 1248;
  IF_RGB5A1		 = 2272;
  IF_RGB10A2		 = 3296;
  IF_RGB10A2UI		 = 4320;
  IF_R11FG11FB10F	 = 5344;
  IF_RGB9E5		 = 6368;
  IF_RGB_DXT1		 = 128;
  IF_SRGB_DXT1		 = 8320;
  IF_RGBA_DXT1		 = 16512;
  IF_SRGBA_DXT1		 = 24704;
  IF_RGBA_DXT3		 = 32896;
  IF_SRGBA_DXT3		 = 41088;
  IF_RGBA_DXT5		 = 49280;
  IF_SRGBA_DXT5		 = 57472;


implementation


{ TImageFormatSelector }


class function TImageFormatSelector.CreateCompressed(
  aPixelFormat: TS3TCCompressedFormats): cardinal;
var Format: cardinal;
begin
  Format := 0;
  TImageFormatBits.SetBaseFormat(Format, bfCompressed);
  TImageFormatBits.SetCompressedFormat(Format, aPixelFormat);
  result := format;
end;


class function TImageFormatSelector.CreateDepthStencil(aDepthBit: byte;
  aStencil: boolean): cardinal;
var Format: cardinal;
    dsFormat: TDepthStencilFormat;
begin
  Format := 0; dsFormat := dfDepth24;
  if aStencil then TImageFormatBits.SetBaseFormat(Format, bfDepthStencil)
  else TImageFormatBits.SetBaseFormat(Format, bfDepth);
  if not aStencil then begin
    case aDepthBit of
      16: dsFormat := dfDepth16;
      24: dsFormat := dfDepth24;
      32: dsFormat := dfDepth32F;
      else assert(false, 'Unsupported DepthStencil format!');
    end;
  end else begin
    case aDepthBit of
       0: dsFormat := dfStencilIndex8;
      24: dsFormat := dfDepth24Stencil8;
      32: dsFormat := dfDepth32FStencil8;
      else assert(false, 'Unsupported DepthStencil format!');
    end;
  end;
  TImageFormatBits.SetDepthStencilFormat(Format, dsFormat);
  result := format;
end;


class function TImageFormatSelector.CreateFloat16(
  aPixelFormat: TBaseImageFormat): cardinal;
var Format: cardinal;
begin
  Format := 0;
  TImageFormatBits.SetBaseFormat(Format, aPixelFormat);
  TImageFormatBits.SetPixelFormat(Format, pfFloat16);
  TImageFormatBits.SetBitsDepth(Format, 16);
  result := format;
end;


class function TImageFormatSelector.CreateFloat32(
  aPixelFormat: TBaseImageFormat): cardinal;
var Format: cardinal;
begin
  Format := 0;
  TImageFormatBits.SetBaseFormat(Format, aPixelFormat);
  TImageFormatBits.SetPixelFormat(Format, pfFloat);
  TImageFormatBits.SetBitsDepth(Format, 32);
  result := format;
end;


class function TImageFormatSelector.CreateInt16(
  aPixelFormat: TBaseImageFormat): cardinal;
var Format: cardinal;
begin
  Format := 0;
  TImageFormatBits.SetBaseFormat(Format, aPixelFormat);
  TImageFormatBits.SetPixelFormat(Format, pfShort);
  TImageFormatBits.SetBitsDepth(Format, 16);
  result := format;
end;


class function TImageFormatSelector.CreateInt32(
  aPixelFormat: TBaseImageFormat): cardinal;
var Format: cardinal;
begin
  Format := 0;
  TImageFormatBits.SetBaseFormat(Format, aPixelFormat);
  TImageFormatBits.SetPixelFormat(Format, pfInt);
  TImageFormatBits.SetBitsDepth(Format, 32);
  result := format;
end;


class function TImageFormatSelector.CreateInt8(
  aPixelFormat: TBaseImageFormat): cardinal;
var Format: cardinal;
begin
  Format := 0;
  TImageFormatBits.SetBaseFormat(Format, aPixelFormat);
  TImageFormatBits.SetPixelFormat(Format, pfByte);
  TImageFormatBits.SetBitsDepth(Format, 8);
  result := format;
end;


class function TImageFormatSelector.CreateUInt8(
  aPixelFormat: TBaseImageFormat): cardinal;
var Format: cardinal;
begin
  Format := 0;
  TImageFormatBits.SetBaseFormat(Format, aPixelFormat);
  TImageFormatBits.SetPixelFormat(Format, pfUByte);
  TImageFormatBits.SetBitsDepth(Format, 8);
  result := format;
end;


class function TImageFormatSelector.CreateUInt16(
  aPixelFormat: TBaseImageFormat): cardinal;
var Format: cardinal;
begin
  Format := 0;
  TImageFormatBits.SetBaseFormat(Format, aPixelFormat);
  TImageFormatBits.SetPixelFormat(Format, pfUShort);
  TImageFormatBits.SetBitsDepth(Format, 16);
  result := format;
end;


class function TImageFormatSelector.CreateUInt32(
  aPixelFormat: TBaseImageFormat): cardinal;
var Format: cardinal;
begin
  Format := 0;
  TImageFormatBits.SetBaseFormat(Format, aPixelFormat);
  TImageFormatBits.SetPixelFormat(Format, pfUInt);
  TImageFormatBits.SetBitsDepth(Format, 32);
  result := format;
end;


class function TImageFormatSelector.CreateSpecial(
  aPixelFormat: TImageSpecialFormat): cardinal;
var Format: cardinal;
begin
  Format := 0;
  TImageFormatBits.SetBaseFormat(Format, bfSpecial);
  TImageFormatBits.SetSpecialFormat(Format, aPixelFormat);
  result := format;
end;




{ TImageFormatBits }


class function TImageFormatBits.GetBaseFormat(
  aFormat: cardinal): TBaseImageFormat;
var rev: boolean;
begin
  result := bfRed;
  rev := (aFormat and 4) <> 0;
  if isDepthStencilFormat(aFormat) then begin
    if (aFormat and 256) <> 0 then result := bfDepthStencil
    else result := bfDepth;
    exit;
  end;


  if isCompressedFormat(aFormat) then begin
    result := bfCompressed;
    exit;
  end;


  if isSpecialFormat(aFormat) then begin
    result := bfSpecial;
    exit;
  end;


  //Normal color format
  case aFormat and 3 of
    0: result := bfRed;
    1: result := bfRG;
    2: if rev then result := bfBGR else result := bfRGB;
    3: if rev then result := bfBGRA else result := bfRGBA;
    else assert(false, 'Unknown color format');
  end;
end;


class procedure TImageFormatBits.SetBitsDepth(var aFormat: cardinal;
  aValue: byte);
begin
  if not isBaseFormat(aFormat) then exit;
  //Reset Bits
  aFormat := aFormat and $FFFFFF9F;
  //Bits 5,6 - Bits per component: 0 - 8, 1 - 16, 2 - 32, 3 - extended
  case aValue of
    8: aFormat := aFormat + 0;
    16: aFormat := aFormat + 32;
    32: aFormat := aFormat + 64;
    else assert(false, 'Unsupported bits depth');
  end;
end;


class function TImageFormatBits.GetBPP(aFormat: cardinal): byte;
const
   bpñ: array[0..3] of byte = (8, 16, 32, 0);
begin
  //Bits 5,6 - Bits per component: 0 - 8, 1 - 16, 2 - 32, 3 - extended
  if isDepthStencilFormat(aFormat) then
    result := CDepthStencilSize[GetDepthStencilFormat(aFormat)] * 8
  else if isSpecialFormat(aFormat) then
    result := CSpecialFormatPixelSize[GetSpecialFormat(aFormat)] * 8
  else if isCompressedFormat(aFormat) then
    result := CCompressedPixelSize[GetCompressedFormat(aFormat)] * 8
  else
    result := bpñ[(aFormat and 96) shr 5] *
      CComponentsCount[GetBaseFormat(aFormat)];
end;


class function TImageFormatBits.GetCompressedFormat(
  aFormat: cardinal): TS3TCCompressedFormats;
var temp: cardinal;
begin
  assert(isCompressedFormat(aFormat),'It is not Compressed Format');
  temp := (aFormat and $E000) shr 13;
  result := TS3TCCompressedFormats(temp);
end;


class function TImageFormatBits.GetDepthStencilFormat(
  aFormat: cardinal): TDepthStencilFormat;
var temp: cardinal;
    fb: boolean;
begin
  result := dfDepth24;
  assert(isDepthStencilFormat(aFormat),'It is not Depth/Stencil Format');
  temp := (aFormat and 96) shr 5;
  fb := (aFormat and 128) <> 0;
  if GetBaseFormat(aFormat) = bfDepth then begin
    case temp of
      0: result := dfDepth16;
      1: result := dfDepth24;
      2: if fb then result := dfDepth32F else result := dfDepth32;
      3: assert(false, 'Unsupported format');
    end;
  end else begin
    case temp of
      0: result := dfStencilIndex8;
      1: result := dfDepth24Stencil8;
      2: result := dfDepth32FStencil8;
      3: assert(false, 'Unsupported format');
    end;
  end;
end;


class function TImageFormatBits.GetPixelFormat(
  aFormat: cardinal): TImagePixelFormat;
var temp, bpc: cardinal;
    fb, rev: boolean;
begin
  fb := (aFormat and 128) <> 0;
  rev := (aFormat and 4) <> 0;
  result := pfUByte;
  case GetBaseFormat(aFormat) of
    bfDepth: begin
       //bits 5-6: 0 - Depth16, 1 - Depth24, 2 - Depth32F, 3 - reserved
        temp := (aFormat and 96) shr 5;
        case temp of
          0: result := pfUShort;
          1: result := pfUInt;
          2: if fb then result := pfFloat else result := pfUInt;
          else assert(false, 'Unsupported pixel format!');
        end;
      end;
    bfDepthStencil: begin
        //bits 5-6: 0 - StencilIndex8, 1 - Depth24Stencil8, 2 - Depth32FStencil8, 3 - reserved
        temp := (aFormat and 96) shr 5;
        case temp of
          0: result := pfUByte;
          1: result := pfUI24_8;
          2: result := pfF32UB8;
          else assert(false, 'Unsupported pixel format!');
        end;
      end;
    bfCompressed: begin
        //Pixel format not used
        result := pfUByte;
      end;
    bfSpecial: begin
        case GetSpecialFormat(aFormat) of
          sfR3G3B2: if rev then result := pfUB233Rev else result := pfUB332;
          sfRGB565: if rev then result := pfUS565Rev else result := pfUS565;
          sfRGB5A1: if rev then result := pfUS1555Rev else result := pfUS5551;
          sfRGB10A2: if rev then result := pfI2101010Rev else result := pfI1010102;
          sfRGB10A2UI: if rev then result := pfUI2101010Rev else result := pfUI1010102;
          sfR11FG11FB10F: if rev then result := pfI10F11F11FRev else result := pfFloat;
          sfRGB9E5: if rev then result := pfI5999Rev else result := pfI5999;
          else assert(false, 'Unsupported pixel format!');
        end;
      end;
    else begin
      //Bits 3,4 - Value type: 0 - uint, 1 - int, 2 - float, 3 - depth/stencil
      //Bits 5,6 - Bits per component: 0 - 8, 1 - 16, 2 - 32, 3 - extended
      temp := (aFormat and 24) shr 3;
      bpc := (aFormat and 96) shr 5;
      case temp  of
        0: begin
          case bpc of
            0: result := pfUByte;
            1: result := pfUShort;
            2: result := pfUInt;
          end;
        end;
        1: begin
          case bpc of
            0: result := pfByte;
            1: result := pfShort;
            2: result := pfInt;
          end;
        end;
        2: begin
          case bpc of
            0: assert (false, 'Float 8 is not avaible format!');
            1: result := pfFloat16;
            2: result := pfFloat;
          end;
        end;
      end;
    end;
  end;
end;


class function TImageFormatBits.GetSpecialFormat(
  aFormat: cardinal): TImageSpecialFormat;
var temp: cardinal;
begin
  assert(isSpecialFormat(aFormat),'It is not Special Format');
  temp := (aFormat and $1C00) shr 10;
  result := TImageSpecialFormat(temp);
end;


class function TImageFormatBits.isBaseFormat(aFormat: cardinal): boolean;
begin
  result := false;
  if isCompressedFormat(aFormat) then exit;
  if isDepthStencilFormat(aFormat) then exit;
  if isSpecialFormat(aFormat) then exit;
  result := true;
end;


class function TImageFormatBits.isCompressedFormat(aFormat: cardinal): boolean;
var temp: cardinal;
begin
  temp := aFormat and $E0;
  result := temp = $80;
end;


class function TImageFormatBits.isDepthStencilFormat(
  aFormat: cardinal): boolean;
var temp: cardinal;
begin
  temp := (aFormat and 24) shr 3;
  result := temp = 3;
end;


class function TImageFormatBits.isFloatFormat(aFormat: cardinal): boolean;
begin
  result := ((aFormat and 16) = 16)
    or (((aFormat and 128) = 128) and not isCompressedFormat(aFormat)
        and not isSpecialFormat(aFormat));
end;


class function TImageFormatBits.isSpecialFormat(aFormat: cardinal): boolean;
var temp: cardinal;
begin
  temp := aFormat and $E0;
  result := temp = $E0;
end;


class procedure TImageFormatBits.SetBaseFormat(var aFormat: cardinal;
  aValue: TBaseImageFormat);
begin
  case aValue of
    bfRed: aFormat := (aFormat and $FFFFFFFC) + 0;
    bfRG: aFormat := (aFormat and $FFFFFFFC) + 1;
    bfRGB: aFormat := (aFormat and $FFFFFFFC) + 2;
    bfBGR: aFormat := (aFormat and $FFFFFFF8) + 6;
    bfRGBA: aFormat := (aFormat and $FFFFFFFC) + 3;
    bfBGRA: aFormat := (aFormat and $FFFFFFF8) + 7;
    bfDepth: aFormat := (aFormat and $FFFFFFE7) + 24;
    bfDepthStencil: aFormat := (aFormat and $FFFFFEE7) + 24 + 256;
    bfCompressed: aFormat := (aFormat and $FFFFFF1F) + 128;
    bfSpecial: aFormat := (aFormat and $FFFFFF1F) + 224;
  end;
end;


class procedure TImageFormatBits.SetCompressedFormat(var aFormat: cardinal;
  aValue: TS3TCCompressedFormats);
var temp: cardinal;
begin
  aFormat := (aFormat and $FFFFFF1F) + 128;
  temp := cardinal(aValue) shl 13;
  aFormat := (aFormat and $FFFF1FFF) + temp;
end;


class procedure TImageFormatBits.SetDepthStencilFormat(var aFormat: cardinal;
  aValue: TDepthStencilFormat);
begin
  aFormat := (aFormat and $FFFFFFE7) + 24;
  aFormat := (aFormat and $FFFFFE9F);
  if aValue in [dfStencilIndex8, dfDepth24Stencil8, dfDepth32FStencil8]
  then aFormat := aFormat + 256;
  aFormat := (aFormat and $FFFFFF7F);
  if aValue = dfDepth32FStencil8 then aFormat := aFormat + 128;


  case aValue of
    dfDepth16: aFormat := aFormat + (0 shl 5);
    dfDepth24: aFormat := aFormat + (1 shl 5);
    dfDepth32: aFormat := aFormat + (2 shl 5);
    dfDepth32F: aFormat := aFormat + (2 shl 5);
    dfStencilIndex8: aFormat := aFormat + (0 shl 5);
    dfDepth24Stencil8: aFormat := aFormat + (1 shl 5);
    dfDepth32FStencil8: aFormat := aFormat + (2 shl 5);
  end;
end;


class procedure TImageFormatBits.SetPixelFormat(var aFormat: cardinal;
  aValue: TImagePixelFormat);
begin
  if aValue in [pfUS1555Rev, pfUB233Rev, pfUS565Rev, pfUS4444Rev, pfUI8888Rev,
    pfUI2101010Rev, pfI2101010Rev, pfI5999Rev, pfI10F11F11FRev]
  then aFormat := (aFormat and $FFFFFFFB) + 4;


  aFormat := (aFormat and $FFFFFFE7); //reset type bits
  aFormat := (aFormat and $FFFFFF1F); //reset float and components flag
  //Set special format bits
  case aValue of
    pfUB332, pfUB233Rev: aFormat := aFormat + (ord(sfR3G3B2) shl 5) + 224;
    pfUS565, pfUS565Rev: aFormat := aFormat + (ord(sfRGB565) shl 5) + 224;
    pfUS5551, pfUS1555Rev: aFormat := aFormat + (ord(sfRGB5A1) shl 5) + 224;
    pfUI1010102, pfUI2101010Rev: aFormat := aFormat + (ord(sfRGB10A2UI) shl 5) + 224;
    pfI5999, pfI5999Rev: aFormat := aFormat + (ord(sfRGB9E5) shl 5) + 224;
    pfI10F11F11F, pfI10F11F11FRev: aFormat := aFormat + (ord(sfR11FG11FB10F) shl 5) + 224;
    pfI1010102, pfI2101010Rev: aFormat := aFormat + (ord(sfRGB10A2) shl 5) + 224;
  end;


  if aValue in [pfUByte, pfUShort, pfUInt, pfUS4444Rev,
    pfUB332, pfUB233Rev, pfUS565, pfUS565Rev, pfUS4444, pfUS5551, pfUS1555Rev,
    pfUI8888, pfUI8888Rev, pfUI1010102, pfUI2101010Rev, pfUI24_8]
  then begin
    aFormat := aFormat + 0; //UInt format flag
    //set bits per componet
    if aValue in [pfUByte, pfUB332, pfUB233Rev, pfUI8888, pfUI8888Rev]
    then aFormat := aFormat + 0 //8 bits per component
    else
      if aValue in [pfUShort, pfUS4444Rev, pfUS565, pfUS565Rev,
        pfUS4444, pfUS5551, pfUS1555Rev]
      then aFormat := aFormat + 32 //16 bits per component
      else aFormat := aFormat + 64 //32 bits per component
  end else if aValue in [pfByte, pfShort, pfInt, pfI1010102, pfI2101010Rev, pfI5999,
    pfI5999Rev] then begin
    aFormat := aFormat + 8;
  end else if aValue = pfFloat16 then aFormat :=aFormat + 48
    else begin // in [pfFloat, pfI10F11F11F, pfI10F11F11FRev] then begin
      aFormat := aFormat + 80;
    end;
end;


class procedure TImageFormatBits.SetSpecialFormat(var aFormat: cardinal;
  aValue: TImageSpecialFormat);
var temp: cardinal;
begin
  aFormat := (aFormat and $FFFFFF1F) + 224;
  temp := cardinal(aValue) shl 10;
  aFormat := (aFormat and $FFFFE3FF) + temp;
end;


{ TAbstractPixelFormatSelector<T> }


class function TAbstractPixelFormatSelector<T>.GetMemSize(
  aPixelFormat: TS3TCCompressedFormats; aWidth, aHeight,
  aDepth: integer): cardinal;
var mw, mh, md, bs: integer;
begin
  mw := max(1,aWidth); mh := max(1,aHeight); md := max(1, aDepth);
  bs := CCompressedPixelSize[aPixelFormat];
  result := trunc(max(1, mw / 4) * max(1, mh / 4)*bs)*md;
end;


class function TAbstractPixelFormatSelector<T>.GetMemSize(
  aPixelFormat: TImagePixelFormat; aWidth, aHeight, aDepth: integer): cardinal;
begin
  result := max(1,aWidth) * max(1,aHeight) * max(1, aDepth) *
    CBasePixelSize[aPixelFormat];
end;


class function TAbstractPixelFormatSelector<T>.GetMemSize(
  aPixelFormat: TDepthStencilFormat; aWidth, aHeight,
  aDepth: integer): cardinal;
begin
  result := max(1,aWidth) * max(1,aHeight) * max(1, aDepth) *
    CDepthStencilSize[aPixelFormat];
end;


class function TAbstractPixelFormatSelector<T>.GetMemSize(
  aPixelFormat: TImageSpecialFormat; aWidth, aHeight,
  aDepth: integer): cardinal;
begin
  result := max(1,aWidth) * max(1,aHeight) * max(1, aDepth) *
    CSpecialFormatPixelSize[aPixelFormat];
end;


class function TAbstractPixelFormatSelector<T>.GetPixelSize(
  aPixelFormat: TImagePixelFormat): cardinal;
begin
  result := CBasePixelSize[aPixelFormat];
end;


class function TAbstractPixelFormatSelector<T>.GetPixelSize(
  aPixelFormat: TS3TCCompressedFormats): cardinal;
begin
  result := CCompressedPixelSize[aPixelFormat];
end;


class function TAbstractPixelFormatSelector<T>.GetPixelSize(
  aPixelFormat: TDepthStencilFormat): cardinal;
begin
  result := CDepthStencilSize[aPixelFormat];
end;


class function TAbstractPixelFormatSelector<T>.GetPixelSize(
  aPixelFormat: TImageSpecialFormat): cardinal;
begin
  result := CSpecialFormatPixelSize[aPixelFormat];
end;


class function TAbstractPixelFormatSelector<T>.GetMemSize(aFormatBits: cardinal;
  aWidth, aHeight, aDepth: integer; aMipMapping: boolean): cardinal;
var w,h,d: integer;
    size: cardinal;
begin
  size := 0; w := aWidth; h := aHeight; d := aDepth;
  repeat
    if TImageFormatBits.isCompressedFormat(aFormatBits) then
      size := size + GetMemSize(TImageFormatBits.GetCompressedFormat(aFormatBits),w,h,d)
    else
      if TImageFormatBits.isDepthStencilFormat(aFormatBits) then
        size := size + GetMemSize(TImageFormatBits.GetDepthStencilFormat(aFormatBits),w,h,d)
      else
        if TImageFormatBits.isSpecialFormat(aFormatBits) then
          size := size + GetMemSize(TImageFormatBits.GetSpecialFormat(aFormatBits),w,h,d)
        else
          size := size + GetMemSize(TImageFormatBits.GetPixelFormat(aFormatBits),w,h,d) *
            CComponentsCount[TImageFormatBits.GetBaseFormat(aFormatBits)];
    w := w div 2; h:= h div 2; d:= d div 2;
  until (not aMipMapping) or (w+h+d = 0);
  result := size;
end;


class function TAbstractPixelFormatSelector<T>.GetMipmapsCount(
  Size: integer): byte;
var s: integer;
begin
  result := 0; s := size;
  while s > 0 do begin
    Inc(result);
    s := s shr 1;
  end;
end;


class function TAbstractPixelFormatSelector<T>.GetPixelSize(
  aPixelFormatBits: cardinal): cardinal;
begin
    if TImageFormatBits.isCompressedFormat(aPixelFormatBits) then
      result := GetPixelSize(TImageFormatBits.GetCompressedFormat(aPixelFormatBits))
    else
      if TImageFormatBits.isDepthStencilFormat(aPixelFormatBits) then
        result := GetPixelSize(TImageFormatBits.GetDepthStencilFormat(aPixelFormatBits))
      else
        if TImageFormatBits.isSpecialFormat(aPixelFormatBits) then
          result := GetPixelSize(TImageFormatBits.GetSpecialFormat(aPixelFormatBits))
        else
          result := GetPixelSize(TImageFormatBits.GetPixelFormat(aPixelFormatBits))*
            CComponentsCount[TImageFormatBits.GetBaseFormat(aPixelFormatBits)];
end;


end.
