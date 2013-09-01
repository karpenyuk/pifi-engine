{$IFNDEF DDS_TYPES}

{$REGION 'DDS Constants and Types'}
    DWORD = cardinal;
    TFOURCC = array[0..3] of AnsiChar;

    PDxgiFormat = ^TDxgiFormat;
    DXGI_FORMAT                              = (
      DXGI_FORMAT_UNKNOWN                    = 0,
      DXGI_FORMAT_R32G32B32A32_TYPELESS      = 1,
      DXGI_FORMAT_R32G32B32A32_FLOAT         = 2,
      DXGI_FORMAT_R32G32B32A32_UINT          = 3,
      DXGI_FORMAT_R32G32B32A32_SINT          = 4,
      DXGI_FORMAT_R32G32B32_TYPELESS         = 5,
      DXGI_FORMAT_R32G32B32_FLOAT            = 6,
      DXGI_FORMAT_R32G32B32_UINT             = 7,
      DXGI_FORMAT_R32G32B32_SINT             = 8,
      DXGI_FORMAT_R16G16B16A16_TYPELESS      = 9,
      DXGI_FORMAT_R16G16B16A16_FLOAT         = 10,
      DXGI_FORMAT_R16G16B16A16_UNORM         = 11,
      DXGI_FORMAT_R16G16B16A16_UINT          = 12,
      DXGI_FORMAT_R16G16B16A16_SNORM         = 13,
      DXGI_FORMAT_R16G16B16A16_SINT          = 14,
      DXGI_FORMAT_R32G32_TYPELESS            = 15,
      DXGI_FORMAT_R32G32_FLOAT               = 16,
      DXGI_FORMAT_R32G32_UINT                = 17,
      DXGI_FORMAT_R32G32_SINT                = 18,
      DXGI_FORMAT_R32G8X24_TYPELESS          = 19,
      DXGI_FORMAT_D32_FLOAT_S8X24_UINT       = 20,
      DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS   = 21,
      DXGI_FORMAT_X32_TYPELESS_G8X24_UINT    = 22,
      DXGI_FORMAT_R10G10B10A2_TYPELESS       = 23,
      DXGI_FORMAT_R10G10B10A2_UNORM          = 24,
      DXGI_FORMAT_R10G10B10A2_UINT           = 25,
      DXGI_FORMAT_R11G11B10_FLOAT            = 26,
      DXGI_FORMAT_R8G8B8A8_TYPELESS          = 27,
      DXGI_FORMAT_R8G8B8A8_UNORM             = 28,
      DXGI_FORMAT_R8G8B8A8_UNORM_SRGB        = 29,
      DXGI_FORMAT_R8G8B8A8_UINT              = 30,
      DXGI_FORMAT_R8G8B8A8_SNORM             = 31,
      DXGI_FORMAT_R8G8B8A8_SINT              = 32,
      DXGI_FORMAT_R16G16_TYPELESS            = 33,
      DXGI_FORMAT_R16G16_FLOAT               = 34,
      DXGI_FORMAT_R16G16_UNORM               = 35,
      DXGI_FORMAT_R16G16_UINT                = 36,
      DXGI_FORMAT_R16G16_SNORM               = 37,
      DXGI_FORMAT_R16G16_SINT                = 38,
      DXGI_FORMAT_R32_TYPELESS               = 39,
      DXGI_FORMAT_D32_FLOAT                  = 40,
      DXGI_FORMAT_R32_FLOAT                  = 41,
      DXGI_FORMAT_R32_UINT                   = 42,
      DXGI_FORMAT_R32_SINT                   = 43,
      DXGI_FORMAT_R24G8_TYPELESS             = 44,
      DXGI_FORMAT_D24_UNORM_S8_UINT          = 45,
      DXGI_FORMAT_R24_UNORM_X8_TYPELESS      = 46,
      DXGI_FORMAT_X24_TYPELESS_G8_UINT       = 47,
      DXGI_FORMAT_R8G8_TYPELESS              = 48,
      DXGI_FORMAT_R8G8_UNORM                 = 49,
      DXGI_FORMAT_R8G8_UINT                  = 50,
      DXGI_FORMAT_R8G8_SNORM                 = 51,
      DXGI_FORMAT_R8G8_SINT                  = 52,
      DXGI_FORMAT_R16_TYPELESS               = 53,
      DXGI_FORMAT_R16_FLOAT                  = 54,
      DXGI_FORMAT_D16_UNORM                  = 55,
      DXGI_FORMAT_R16_UNORM                  = 56,
      DXGI_FORMAT_R16_UINT                   = 57,
      DXGI_FORMAT_R16_SNORM                  = 58,
      DXGI_FORMAT_R16_SINT                   = 59,
      DXGI_FORMAT_R8_TYPELESS                = 60,
      DXGI_FORMAT_R8_UNORM                   = 61,
      DXGI_FORMAT_R8_UINT                    = 62,
      DXGI_FORMAT_R8_SNORM                   = 63,
      DXGI_FORMAT_R8_SINT                    = 64,
      DXGI_FORMAT_A8_UNORM                   = 65,
      DXGI_FORMAT_R1_UNORM                   = 66,
      DXGI_FORMAT_R9G9B9E5_SHAREDEXP         = 67,
      DXGI_FORMAT_R8G8_B8G8_UNORM            = 68,
      DXGI_FORMAT_G8R8_G8B8_UNORM            = 69,
      DXGI_FORMAT_BC1_TYPELESS               = 70,
      DXGI_FORMAT_BC1_UNORM                  = 71,
      DXGI_FORMAT_BC1_UNORM_SRGB             = 72,
      DXGI_FORMAT_BC2_TYPELESS               = 73,
      DXGI_FORMAT_BC2_UNORM                  = 74,
      DXGI_FORMAT_BC2_UNORM_SRGB             = 75,
      DXGI_FORMAT_BC3_TYPELESS               = 76,
      DXGI_FORMAT_BC3_UNORM                  = 77,
      DXGI_FORMAT_BC3_UNORM_SRGB             = 78,
      DXGI_FORMAT_BC4_TYPELESS               = 79,
      DXGI_FORMAT_BC4_UNORM                  = 80,
      DXGI_FORMAT_BC4_SNORM                  = 81,
      DXGI_FORMAT_BC5_TYPELESS               = 82,
      DXGI_FORMAT_BC5_UNORM                  = 83,
      DXGI_FORMAT_BC5_SNORM                  = 84,
      DXGI_FORMAT_B5G6R5_UNORM               = 85,
      DXGI_FORMAT_B5G5R5A1_UNORM             = 86,
      DXGI_FORMAT_B8G8R8A8_UNORM             = 87,
      DXGI_FORMAT_B8G8R8X8_UNORM             = 88,
      DXGI_FORMAT_R10G10B10_XR_BIAS_A2_UNORM = 89,
      DXGI_FORMAT_B8G8R8A8_TYPELESS          = 90,
      DXGI_FORMAT_B8G8R8A8_UNORM_SRGB        = 91,
      DXGI_FORMAT_B8G8R8X8_TYPELESS          = 92,
      DXGI_FORMAT_B8G8R8X8_UNORM_SRGB        = 93,
      DXGI_FORMAT_BC6H_TYPELESS              = 94,
      DXGI_FORMAT_BC6H_UF16                  = 95,
      DXGI_FORMAT_BC6H_SF16                  = 96,
      DXGI_FORMAT_BC7_TYPELESS               = 97,
      DXGI_FORMAT_BC7_UNORM                  = 98,
      DXGI_FORMAT_BC7_UNORM_SRGB             = 99,
      DXGI_FORMAT_AYUV                       = 100,
      DXGI_FORMAT_Y410                       = 101,
      DXGI_FORMAT_Y416                       = 102,
      DXGI_FORMAT_NV12                       = 103,
      DXGI_FORMAT_P010                       = 104,
      DXGI_FORMAT_P016                       = 105,
      DXGI_FORMAT_420_OPAQUE                 = 106,
      DXGI_FORMAT_YUY2                       = 107,
      DXGI_FORMAT_Y210                       = 108,
      DXGI_FORMAT_Y216                       = 109,
      DXGI_FORMAT_NV11                       = 110,
      DXGI_FORMAT_AI44                       = 111,
      DXGI_FORMAT_IA44                       = 112,
      DXGI_FORMAT_P8                         = 113,
      DXGI_FORMAT_A8P8                       = 114,
      DXGI_FORMAT_B4G4R4A4_UNORM             = 115
  //    DXGI_FORMAT_FORCE_UINT                 = $FFFFFFFF
    );
    TDxgiFormat = DXGI_FORMAT;

    DXTColBlock = record
      col0: word;
      col1: word;
      row: array[0..3] of byte;
    end;
    PDXTColBlock = ^DXTColBlock;

    DXT3AlphaBlock = record
      row: array[0..3] of word;
    end;
    PDXT3AlphaBlock = ^DXT3AlphaBlock;

    DXT5AlphaBlock = record
      alpha0 : byte;
      alpha1 : byte;
      row : array[0..5] of byte;
    end;
    PDXT5AlphaBlock = ^DXT5AlphaBlock;

  //DDS header DXT10 resourceDimension
    PD3d10ResourceDimension = ^TD3d10ResourceDimension;
    D3D10_RESOURCE_DIMENSION             = (
      D3D10_RESOURCE_DIMENSION_UNKNOWN   = 0,
      D3D10_RESOURCE_DIMENSION_BUFFER    = 1,
      D3D10_RESOURCE_DIMENSION_TEXTURE1D = 2,
      D3D10_RESOURCE_DIMENSION_TEXTURE2D = 3,
      D3D10_RESOURCE_DIMENSION_TEXTURE3D = 4
    );
    TD3d10ResourceDimension = D3D10_RESOURCE_DIMENSION;

    PDdsPixelformat = ^TDdsPixelformat;
    DDS_PIXELFORMAT = record
      dwSize: DWORD;
      dwFlags: DWORD;
      dwFourCC: DWORD;
      dwRGBBitCount: DWORD;
      dwRBitMask: DWORD;
      dwGBitMask: DWORD;
      dwBBitMask: DWORD;
      dwABitMask: DWORD;
    end;
    TDdsPixelformat = DDS_PIXELFORMAT;

    PDdsHeader = ^TDdsHeader;
    DDS_HEADER = record
      dwSize: DWORD;
      dwFlags: DWORD;
      dwHeight: DWORD;
      dwWidth: DWORD;
      dwPitchOrLinearSize: DWORD;
      dwDepth: DWORD;
      dwMipMapCount: DWORD;
      dwReserved1: array[0..10] of DWORD;
      ddspf: DDS_PIXELFORMAT;
      dwCaps: DWORD;
      dwCaps2: DWORD;
      dwCaps3: DWORD;
      dwCaps4: DWORD;
      dwReserved2: DWORD;
    end;
    TDdsHeader = DDS_HEADER;

    PDdsHeaderDxt10 = ^TDdsHeaderDxt10;
    DDS_HEADER_DXT10 = record
      dxgiFormat: DXGI_FORMAT;
      resourceDimension: D3D10_RESOURCE_DIMENSION;
      miscFlag: cardinal;
      arraySize: cardinal;
      reserved: cardinal;
    end;
    TDdsHeaderDxt10 = DDS_HEADER_DXT10;

    TDDSImage = record
      dwMagic: array[0..3] of ansichar;
      header: DDS_HEADER;
      header10: DDS_HEADER_DXT10;
      data: PByte;
      data2: PByte;
    end;

const
  //Pixel formats dwFlags
    //Texture contains alpha data; dwRGBAlphaBitMask contains valid data.
    DDPF_ALPHAPIXELS      = 1;
    //Used in some older DDS files for alpha channel only uncompressed data (dwRGBBitCount contains the alpha channel bitcount;
    //dwABitMask contains valid data)
    DDPF_ALPHA            = 2;
    //Texture contains compressed RGB data; dwFourCC contains valid data.
    DDPF_FOURCC           = 4;
    //Texture contains uncompressed RGB data; dwRGBBitCount and the RGB masks (dwRBitMask, dwRBitMask, dwRBitMask) contain valid data.
    DDPF_RGB              = $40;
    DDPF_RGBA             = DDPF_RGB + DDPF_ALPHA;
    //Used in some older DDS files for YUV uncompressed data (dwRGBBitCount contains the YUV bit count; dwRBitMask contains the Y mask,
    //dwGBitMask contains the U mask, dwBBitMask contains the V mask)
    DDPF_YUV              = $200;
    //Used in some older DDS files for single channel color uncompressed data (dwRGBBitCount contains the luminance channel bit count; dwRBitMask contains the channel mask).
    //Can be combined with DDPF_ALPHAPIXELS for a two channel DDS file.
    DDPF_LUMINANCE        = $20000;

  //Pixel formats dwFourCC
    FOURCC_UNKNOWN       = 0;
    FOURCC_R8G8B8        = 20;
    FOURCC_A8R8G8B8      = 21;
    FOURCC_X8R8G8B8      = 22;
    FOURCC_R5G6B5        = 23;
    FOURCC_X1R5G5B5      = 24;
    FOURCC_A1R5G5B5      = 25;
    FOURCC_A4R4G4B4      = 26;
    FOURCC_R3G3B2        = 27;
    FOURCC_A8            = 28;
    FOURCC_A8R3G3B2      = 29;
    FOURCC_X4R4G4B4      = 30;
    FOURCC_A2B10G10R10   = 31;
    FOURCC_A8B8G8R8      = 32;
    FOURCC_X8B8G8R8      = 33;
    FOURCC_G16R16        = 34;
    FOURCC_A2R10G10B10   = 35;
    FOURCC_A16B16G16R16  = 36;

    FOURCC_L8            = 50;
    FOURCC_A8L8          = 51;
    FOURCC_A4L4          = 52;
    FOURCC_DXT1          = $31545844;
    FOURCC_DXT2          = $32545844;
    FOURCC_DXT3          = $33545844;
    FOURCC_DXT4          = $34545844;
    FOURCC_DXT5          = $35545844;
    FOURCC_ATI1          = $31495441;
    FOURCC_ATI2          = $32495441;

    FOURCC_D16_LOCKABLE  = 70;
    FOURCC_D32           = 71;
    FOURCC_D24X8         = 77;
    FOURCC_D16           = 80;

    FOURCC_D32F_LOCKABLE = 82;

    FOURCC_L16           = 81;

  // Floating point surface formats

  // s10e5 formats (16-bits per channel)
    FOURCC_R16F          = 111;
    FOURCC_G16R16F       = 112;
    FOURCC_A16B16G16R16F = 113;

  // IEEE s23e8 formats (32-bits per channel)
    FOURCC_R32F          = 114;
    FOURCC_G32R32F       = 115;
    FOURCC_A32B32G32R32F = 116;

    // DX10 header indicator
    FOURCC_DX10         = $30315844;//$47495844 = DXGI;

  //DDS header dwFlags
    DDSD_CAPS      = 1; //Required in every .dds file.
    DDSD_HEIGHT    = 2; //Required in every .dds file.
    DDSD_WIDTH     = 4; //Required in every .dds file.
    DDSD_PITCH     = 8; //Required when pitch is provided for an uncompressed texture.
    DDSD_PIXELFORMAT = $1000;  //Required in every .dds file.
    DDSD_MIPMAPCOUNT = $20000; //Required in a mipmapped texture.
    DDSD_LINEARSIZE = $80000;  //Required when pitch is provided for a compressed texture.
    DDSD_DEPTH = $800000;      //Required in a depth texture.

  //DDS header dwCaps
    //DDSCAPS_COMPLEX - Optional; must be used on any file that contains more than
    //one surface (a mipmap, a cubic environment map, or mipmapped volume texture).
    DDSCAPS_COMPLEX       = 8;
    DDSCAPS_MIPMAP	= $400000; //Optional; should be used for a mipmap.
    DDSCAPS_TEXTURE	= $1000;   //Required

  //DDS header dwCaps2
    DDSCAPS2_CUBEMAP      = $200; //Required for a cube map.
    //Required when these surfaces are stored in a cube map.
    DDSCAPS2_CUBEMAP_POSITIVEX = $400;
    DDSCAPS2_CUBEMAP_NEGATIVEX = $800;
    DDSCAPS2_CUBEMAP_POSITIVEY = $1000;
    DDSCAPS2_CUBEMAP_NEGATIVEY = $2000;
    DDSCAPS2_CUBEMAP_POSITIVEZ = $4000;
    DDSCAPS2_CUBEMAP_NEGATIVEZ = $8000;
    DDSCAPS2_CUBEMAP_ALLFACES  = $FE00;
    //Required for a volume texture.
    DDSCAPS2_VOLUME            = $200000;

  //DDS header DXT10 miscFlag
    DDS_RESOURCE_MISC_TEXTURECUBE = 4; //Indicates a 2D texture is a cube-map texture.

    CDX10CompressedFormat: array[0..21] of DXGI_FORMAT =
    ( DXGI_FORMAT_R9G9B9E5_SHAREDEXP,
      DXGI_FORMAT_BC1_TYPELESS, DXGI_FORMAT_BC1_UNORM,
      DXGI_FORMAT_BC1_UNORM_SRGB, DXGI_FORMAT_BC2_TYPELESS,
      DXGI_FORMAT_BC2_UNORM, DXGI_FORMAT_BC2_UNORM_SRGB,
      DXGI_FORMAT_BC3_TYPELESS, DXGI_FORMAT_BC3_UNORM,
      DXGI_FORMAT_BC3_UNORM_SRGB, DXGI_FORMAT_BC4_TYPELESS,
      DXGI_FORMAT_BC4_UNORM, DXGI_FORMAT_BC4_SNORM,
      DXGI_FORMAT_BC5_TYPELESS, DXGI_FORMAT_BC5_UNORM,
      DXGI_FORMAT_BC5_SNORM, DXGI_FORMAT_BC6H_TYPELESS,
      DXGI_FORMAT_BC6H_UF16, DXGI_FORMAT_BC6H_SF16,
      DXGI_FORMAT_BC7_TYPELESS, DXGI_FORMAT_BC7_UNORM,
      DXGI_FORMAT_BC7_UNORM_SRGB );

{$ENDREGION}
{$DEFINE DDS_TYPES}
{$ENDIF}
