unit uImageFormats;

interface

Type

  TBaseImageFormat = (bfRed, bfRG, bfRGB, bfBGR, bfRGBA, bfBGRA, bfDepth, bfDepthStencil);

  TImagePixelFormat = (pfUByte, pfByte, pfUShort, pfShort, pfUInt, pfInt, pfFloat,
    pfUB332, pfUS565, pfUS4444, pfUS5551, pfUI8888, pfUI1010102,
    pf1555Rev, pfUB233Rev, pfUS565Rev, pfUS4444Rev, pfUI8888Rev, pfUI2101010Rev);


  TImageSpecialFormat = (sfR3G3B2, sfRGB565, sfRGB5A1, sfRGB10A2, sfRGB10A2UI,
    sfR11FG11FB10F, sfRGB9E5);

  TCompressedImageFormat = (cfRed, cfRG, cfRGB, cfRGBA, cfSRGB,
    cfSRGBA, cfRedRGTC1, cfSRed_RGTC1, cfRG_RGTC2,
    cfSRG_RGTC2, cfRGBA_BPTC_UNORM, cfSRGB_ALPHA_BPTC_UNORM,
    cfRGB_BPTC_SF, cfRGB_BPTC_UF, cfRGB_DXT1, cfSRGB_DXT1, cfRGBA_DXT1,
    cfSRGBA_DXT1, cfRGBA_DXT3, cfSRGBA_DXT3, cfRGBA_DXT5, cfSRGBA_DXT5);


  TImagePixelFormats = set of TImagePixelFormat;

  TInternalImageFormat = (
    ifUnknownFormat,
    //Unsized Internal Formats
    ifRGB, ifRGBA, ifLuminanceAlpha, ifLuminance, ifAlpha,
    //Sized Internal Formats
      //single-component formats:
    ifR8, ifR8_SNORM, ifR16F, ifR32F, ifR8UI, ifR8I, ifR16UI, ifR16I, ifR32UI, ifR32I,
      //two-components formats:
    ifRG8, ifRG8_SNORM, ifRG16F, ifRG32F, ifRG8UI, ifRG8I, ifRG16UI, ifRG16I, ifRG32UI, ifRG32I,
      //three-components formats:
    ifRGB8, ifSRGB8, ifR3G3B2, ifRGB565, ifRGB8_SNORM, ifR11F_G11F_B10F, ifRGB9_E5, ifRGB16F,
    ifRGB32F, ifRGB8UI, ifRGB8I, ifRGB16UI, ifRGB16I, ifRGB32UI, ifRGB32I,
      //four-components formats:
    ifRGBA8, ifSRGB8_ALPHA8, ifRGBA8_SNORM, ifRGB5_A1, ifRGBA4, ifRGB10_A2, ifRGBA16F,
    ifRGBA32F, ifRGBA8UI, ifRGBA8I, ifRGB10_A2UI, ifRGBA16UI, ifRGBA16I, ifRGBA32I, ifRGBA32UI,
    //Depth and stencil
    ifDepth16, ifDepth24, ifDepth32F, ifDepth24Stencil8, ifDepth32FStencil8,
    //Copressed Formats
    ifCompressedRed, ifCompressedRG, ifCompressedRGB, ifCompressedRGBA, ifCompressedSRGB,
    ifCompressedSRGBA, ifCompressedRedRGTC1, ifCompressedSRed_RGTC1, ifCompressedRG_RGTC2,
    ifCompressedSRG_RGTC2, ifCompressedRGBA_BPTC_UNORM, ifCompressed_SRGB_ALPHA_BPTC_UNORM,
    ifCompressedRGB_BPTC_SIGNED_FLOAT, ifCompressedRGB_BPTC_UNSIGNED_FLOAT,
    //S3TC formats
    ifCompressedRGB_S3TC_DXT1, ifCompressedSRGB_S3TC_DXT1, ifCompressedRGBA_S3TC_DXT1,
    ifCompressedSRGB_ALPHA_S3TC_DXT1, ifCompressedRGBA_S3TC_DXT3,
    ifCompressedSRGB_ALPHA_S3TC_DXT3, ifCompressedRGBA_S3TC_DXT5, ifCompressedSRGB_ALPHA_S3TC_DXT5
    );

  TImageFormatSelector = class
  public
    class function CreateInt8(aPixelFormat: TBaseImageFormat): TInternalImageFormat;
    class function CreateInt16(aPixelFormat: TBaseImageFormat): TInternalImageFormat;
    class function CreateInt32(aPixelFormat: TBaseImageFormat): TInternalImageFormat;
    class function CreateUInt8(aPixelFormat: TBaseImageFormat): TInternalImageFormat;
    class function CreateUInt16(aPixelFormat: TBaseImageFormat): TInternalImageFormat;
    class function CreateUInt32(aPixelFormat: TBaseImageFormat): TInternalImageFormat;
    class function CreateFloat16(aPixelFormat: TBaseImageFormat): TInternalImageFormat;
    class function CreateFloat32(aPixelFormat: TBaseImageFormat): TInternalImageFormat;

    class function CreateCompressed(aPixelFormat: TCompressedImageFormat): TInternalImageFormat;

    class function CreateDepthStencil(aDepthBit: byte; aStencil: boolean = false): TInternalImageFormat;

    class function CreateSpecial(aPixelFormat: TImageSpecialFormat): TInternalImageFormat;

  end;
{//Description of TInternalImageFormat
const
  CInternalImageFormats: array[TInternalImageFormat] of record
    PixelFormat: TImagePixelFormat;
    RevPF: TImagePixelFormat; //Revers pixel format
    BaseFormat: TBaseImageFormat;
    RevBF: TBaseImageFormat; //Revers base format
    Bpp: byte; //Bits per Pixel
  end = (
  //ifUnknownFormat
    (PixelFormat: pfUByte; RevPF: pfUByte; BaseFormat: bfRed; RevBF: bfRed; Bpp: 8),
  //ifRGB
    (PixelFormat: pfUByte; RevPF: pfUByte; BaseFormat: bfRGB; RevBF: bfBGR; Bpp: 24),
  //ifRGBA
    (PixelFormat: pfUByte; RevPF: pfUByte; BaseFormat: bfRGBA; RevBF: bfBGRA; Bpp: 32),
  //ifLuminanceAlpha
    (PixelFormat: pfUByte; RevPF: pfUByte; BaseFormat: bfRG; RevBF: bfRG; Bpp: 16),
  //ifLuminance
    (PixelFormat: pfUByte; RevPF: pfUByte; BaseFormat: bfRed; RevBF: bfRed; Bpp: 8),
  //ifAlpha
    (PixelFormat: pfUByte; RevPF: pfUByte; BaseFormat: bfRed; RevBF: bfRed; Bpp: 8),
  );
}
implementation

{ TImageFormatSelector }

class function TImageFormatSelector.CreateCompressed(
  aPixelFormat: TCompressedImageFormat): TInternalImageFormat;
begin
  result := ifUnknownFormat;
  case aPixelFormat of
    cfRed: result := ifCompressedRed;
    cfRG: result := ifCOmpressedRG;
    cfRGB: result := ifCompressedRGB;
    cfRGBA: result := ifCompressedRGBA;
    cfSRGB: result := ifCompressedSRGB;
    cfSRGBA: result := ifCompressedSRGBA;

    cfRedRGTC1: result := ifCompressedRedRGTC1;
    cfSRed_RGTC1: result := ifCompressedSRed_RGTC1;
    cfRG_RGTC2: result := ifCompressedRG_RGTC2;
    cfSRG_RGTC2: result := ifCompressedSRG_RGTC2;
    cfRGBA_BPTC_UNORM: result := ifCompressedRGBA_BPTC_UNORM;
    cfSRGB_ALPHA_BPTC_UNORM: result := ifCompressed_SRGB_ALPHA_BPTC_UNORM;
    cfRGB_BPTC_SF: result := ifCompressedRGB_BPTC_SIGNED_FLOAT;
    cfRGB_BPTC_UF: result := ifCompressedRGB_BPTC_UNSIGNED_FLOAT;
    cfRGB_DXT1: result := ifCompressedRGB_S3TC_DXT1;
    cfSRGB_DXT1: result := ifCompressedSRGB_S3TC_DXT1;
    cfRGBA_DXT1: result := ifCompressedRGBA_S3TC_DXT1;
    cfSRGBA_DXT1: result := ifCompressedSRGB_ALPHA_S3TC_DXT1;
    cfRGBA_DXT3: result := ifCompressedRGBA_S3TC_DXT3;
    cfSRGBA_DXT3: result := ifCompressedSRGB_ALPHA_S3TC_DXT3;
    cfRGBA_DXT5: result := ifCompressedRGBA_S3TC_DXT5;
    cfSRGBA_DXT5: result := ifCompressedSRGB_ALPHA_S3TC_DXT5;
    else assert(false, 'Incompatible pixel format! Try another selector.');
  end;
end;

class function TImageFormatSelector.CreateDepthStencil(aDepthBit: byte;
  aStencil: boolean): TInternalImageFormat;
begin
  result := ifUnknownFormat;
  if not aStencil then begin
    case aDepthBit of
      16: result := ifDepth16;
      24: result := ifDepth24;
      32: result := ifDepth32F;
      else assert(false, 'Unsupported DepthStencil format!');
    end;
  end else begin
    case aDepthBit of
      24: result := ifDepth24Stencil8;
      32: result := ifDepth32FStencil8;
      else assert(false, 'Unsupported DepthStencil format!');
    end;
  end;
end;

class function TImageFormatSelector.CreateFloat16(
  aPixelFormat: TBaseImageFormat): TInternalImageFormat;
begin
  result := ifUnknownFormat;
  case aPixelFormat of
    bfRed: result := ifR16F;
    bfRG: result := ifRG16F;
    bfRGB: result := ifRGB16F;
    bfBGR: result := ifRGB16F;
    bfRGBA: result := ifRGBA16F;
    bfBGRA: result := ifRGBA16F;
    else assert(false, 'Incompatible pixel format! Try another selector.');
  end;
end;

class function TImageFormatSelector.CreateFloat32(
  aPixelFormat: TBaseImageFormat): TInternalImageFormat;
begin
  result := ifUnknownFormat;
  case aPixelFormat of
    bfRed: result := ifR32F;
    bfRG: result := ifRG32F;
    bfRGB: result := ifRGB32F;
    bfBGR: result := ifRGB32F;
    bfRGBA: result := ifRGBA32F;
    bfBGRA: result := ifRGBA32F;
    else assert(false, 'Incompatible pixel format! Try another selector.');
  end;
end;

class function TImageFormatSelector.CreateInt16(
  aPixelFormat: TBaseImageFormat): TInternalImageFormat;
begin
  result := ifUnknownFormat;
  case aPixelFormat of
    bfRed: result := ifR16I;
    bfRG: result := ifRG16I;
    bfRGB: result := ifRGB16I;
    bfBGR: result := ifRGB16I;
    bfRGBA: result := ifRGBA16I;
    bfBGRA: result := ifRGBA16I;
    else assert(false, 'Incompatible pixel format! Try another selector.');
  end;
end;

class function TImageFormatSelector.CreateInt32(
  aPixelFormat: TBaseImageFormat): TInternalImageFormat;
begin
  result := ifUnknownFormat;
  case aPixelFormat of
    bfRed: result := ifR32I;
    bfRG: result := ifRG32I;
    bfRGB: result := ifRGB32I;
    bfBGR: result := ifRGB32I;
    bfRGBA: result := ifRGBA32I;
    bfBGRA: result := ifRGBA32I;
    else assert(false, 'Incompatible pixel format! Try another selector.');
  end;
end;

class function TImageFormatSelector.CreateInt8(
  aPixelFormat: TBaseImageFormat): TInternalImageFormat;
begin
  result := ifUnknownFormat;
  case aPixelFormat of
    bfRed: result := ifR8I;
    bfRG: result := ifRG8I;
    bfRGB: result := ifRGB8I;
    bfBGR: result := ifRGB8I;
    bfRGBA: result := ifRGBA8I;
    bfBGRA: result := ifRGBA8I;
    else assert(false, 'Incompatible pixel format! Try another selector.');
  end;
end;

class function TImageFormatSelector.CreateUInt8(
  aPixelFormat: TBaseImageFormat): TInternalImageFormat;
begin
  result := ifUnknownFormat;
  case aPixelFormat of
    bfRed: result := ifR8UI;
    bfRG: result := ifRG8UI;
    bfRGB: result := ifRGB8UI;
    bfBGR: result := ifRGB8UI;
    bfRGBA: result := ifRGBA8UI;
    bfBGRA: result := ifRGBA8UI;
    else assert(false, 'Incompatible pixel format! Try another selector.');
  end;
end;

class function TImageFormatSelector.CreateUInt16(
  aPixelFormat: TBaseImageFormat): TInternalImageFormat;
begin
  result := ifUnknownFormat;
  case aPixelFormat of
    bfRed: result := ifR16UI;
    bfRG: result := ifRG16UI;
    bfRGB: result := ifRGB16UI;
    bfBGR: result := ifRGB16UI;
    bfRGBA: result := ifRGBA16UI;
    bfBGRA: result := ifRGBA16UI;
    else assert(false, 'Incompatible pixel format! Try another selector.');
  end;
end;

class function TImageFormatSelector.CreateUInt32(
  aPixelFormat: TBaseImageFormat): TInternalImageFormat;
begin
  result := ifUnknownFormat;
  case aPixelFormat of
    bfRed: result := ifR32UI;
    bfRG: result := ifRG32UI;
    bfRGB: result := ifRGB32UI;
    bfBGR: result := ifRGB32UI;
    bfRGBA: result := ifRGBA32UI;
    bfBGRA: result := ifRGBA32UI;
    else assert(false, 'Incompatible pixel format! Try another selector.');
  end;
end;

class function TImageFormatSelector.CreateSpecial(
  aPixelFormat: TImageSpecialFormat): TInternalImageFormat;
begin
  result := ifUnknownFormat;
  case aPixelFormat of
    sfR3G3B2: result := ifR3G3B2;
    sfRGB565: result := ifRGB565;
    sfRGB5A1: result := ifRGB5_A1;
    sfRGB10A2: result := ifRGB10_A2;
    sfRGB10A2UI: result := ifRGB10_A2UI;
    sfR11FG11FB10F: result := ifR11F_G11F_B10F;
    sfRGB9E5: result := ifRGB9_E5;
    else assert(false, 'Incompatible pixel format! Try another selector.');
  end;
end;


end.
