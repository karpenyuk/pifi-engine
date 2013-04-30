procedure DDSToGLFormats(const dds: TDDSImage; var Descr: TImageDesc);
begin
  with dds.header,Descr do begin
    Compressed := false; ElementSize:=0;
    // figure out what the image format is
    if (ddspf.dwFlags and DDPF_FOURCC<>0) then begin
        case ddspf.dwFourCC of
            FOURCC_DXT1: begin
                ColorFormat := GL_COMPRESSED_RGBA_S3TC_DXT1_EXT;
                InternalFormat := GL_COMPRESSED_RGBA_S3TC_DXT1_EXT;
                DataType := GL_COMPRESSED_RGBA_S3TC_DXT1_EXT;
                ElementSize := 8;
                Compressed := true;
                end;

            FOURCC_DXT2:;
            FOURCC_DXT3: begin
                ColorFormat := GL_COMPRESSED_RGBA_S3TC_DXT3_EXT;
                InternalFormat := GL_COMPRESSED_RGBA_S3TC_DXT3_EXT;
                DataType := GL_COMPRESSED_RGBA_S3TC_DXT3_EXT;
                ElementSize := 16;
                Compressed := true;
                end;

            FOURCC_DXT4:;
            FOURCC_DXT5: begin
                ColorFormat := GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;
                InternalFormat := GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;
                DataType := GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;
                ElementSize := 16;
                Compressed := true;
                end;

	    FOURCC_ATI1: begin
                ColorFormat := GL_COMPRESSED_LUMINANCE_LATC1_EXT;
                InternalFormat := GL_COMPRESSED_LUMINANCE_LATC1_EXT;
                DataType := GL_COMPRESSED_LUMINANCE_LATC1_EXT;
                ElementSize := 8;
                Compressed := true;
                end;

	    FOURCC_ATI2: begin
                ColorFormat := GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT;
                InternalFormat := GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT;
                DataType := GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT;
                ElementSize := 16;
                Compressed := true;
                end;

            FOURCC_R8G8B8: begin
                ColorFormat := GL_BGR;
                InternalFormat := GL_RGB8;
                DataType := GL_UNSIGNED_BYTE;
                ElementSize := 3;
                end;

            FOURCC_A8R8G8B8: begin
                ColorFormat := GL_BGRA;
                InternalFormat := GL_RGBA8;
                DataType := GL_UNSIGNED_BYTE;
                ElementSize := 4;
                end;

            FOURCC_X8R8G8B8: begin
                ColorFormat := GL_BGRA;
                InternalFormat := GL_RGB8;
                DataType := GL_UNSIGNED_INT_8_8_8_8;
                ElementSize := 4;
                end;

            FOURCC_R5G6B5: begin
                ColorFormat := GL_BGR;
                InternalFormat := GL_RGB5;
                DataType := GL_UNSIGNED_SHORT_5_6_5;
                ElementSize := 2;
                end;

            FOURCC_A8: begin
                ColorFormat := GL_ALPHA;
                InternalFormat := GL_ALPHA8;
                DataType := GL_UNSIGNED_BYTE;
                ElementSize := 1;
                end;

            FOURCC_A2B10G10R10: begin
                ColorFormat := GL_RGBA;
                InternalFormat := GL_RGB10_A2;
                DataType := GL_UNSIGNED_INT_10_10_10_2;
                ElementSize := 4;
                end;

            FOURCC_A8B8G8R8: begin
                ColorFormat := GL_RGBA;
                InternalFormat := GL_RGBA8;
                DataType := GL_UNSIGNED_BYTE;
                ElementSize := 4;
                end;

            FOURCC_X8B8G8R8: begin
                ColorFormat := GL_RGBA;
                InternalFormat := GL_RGB8;
                DataType := GL_UNSIGNED_INT_8_8_8_8;
                ElementSize := 4;
                end;

            FOURCC_A2R10G10B10: begin
                ColorFormat := GL_BGRA;
                InternalFormat := GL_RGB10_A2;
                DataType := GL_UNSIGNED_INT_10_10_10_2;
                ElementSize := 4;
                end;

            FOURCC_A16B16G16R16: begin
                ColorFormat := GL_RGBA;
                InternalFormat := GL_RGBA16;
                DataType := GL_UNSIGNED_SHORT;
                ElementSize := 8;
                end;

            FOURCC_L8: begin
                ColorFormat := GL_LUMINANCE;
                InternalFormat := GL_LUMINANCE8;
                DataType := GL_UNSIGNED_BYTE;
                ElementSize := 1;
                end;

            FOURCC_A8L8: begin
                ColorFormat := GL_LUMINANCE_ALPHA;
                InternalFormat := GL_LUMINANCE8_ALPHA8;
                DataType := GL_UNSIGNED_BYTE;
                ElementSize := 2;
                end;

            FOURCC_L16: begin
                ColorFormat := GL_LUMINANCE;
                InternalFormat := GL_LUMINANCE16;
                DataType := GL_UNSIGNED_SHORT;
                ElementSize := 2;
                end;

            FOURCC_R16F: begin
                ColorFormat := GL_LUMINANCE; //should use red, once it is available
                InternalFormat := GL_LUMINANCE16F_ARB; 
                DataType := GL_HALF_FLOAT_ARB;
                ElementSize := 2;
                end;

            FOURCC_A16B16G16R16F: begin
                ColorFormat := GL_RGBA;
                InternalFormat := GL_RGBA16F_ARB;
                DataType := GL_HALF_FLOAT_ARB;
                ElementSize := 8;
                end;

            FOURCC_R32F: begin
                ColorFormat := GL_LUMINANCE; //should use red, once it is available
                InternalFormat := GL_LUMINANCE32F_ARB; 
                DataType := GL_FLOAT;
                ElementSize := 4;
                end;

            FOURCC_A32B32G32R32F: begin
                ColorFormat := GL_RGBA;
                InternalFormat := GL_RGBA32F_ARB;
                DataType := GL_FLOAT;
                ElementSize := 16;
                end;

            FOURCC_UNKNOWN: ;
            FOURCC_X1R5G5B5: ;
            FOURCC_A1R5G5B5: ;
            FOURCC_A4R4G4B4: ;
            FOURCC_R3G3B2: ;
            FOURCC_A8R3G3B2: ;
            FOURCC_X4R4G4B4: ;
            FOURCC_A4L4: ;
            FOURCC_D16_LOCKABLE: ;
            FOURCC_D32: ;
            FOURCC_D24X8: ;
            FOURCC_D16: ;
            FOURCC_D32F_LOCKABLE: ;
            FOURCC_G16R16: ;
            FOURCC_G16R16F: ;
            FOURCC_G32R32F: ;
                //these are unsupported for now
            else assert(false,'Unsupported format');
        end;
    end else begin
      ElementSize := 0;
      if ddspf.dwFourCC=0 then begin
        case ddspf.dwRGBBitCount of
          32: begin
            InternalFormat:=GL_RGBA8;
            if ddspf.dwRBitMask<ddspf.dwBBitMask
            then ColorFormat:=GL_RGBA
            else ColorFormat:=GL_BGRA;
            DataType:=GL_UNSIGNED_BYTE;
            ElementSize := 4;
          end;
          24: begin
            InternalFormat:=GL_RGB8;
            if ddspf.dwRBitMask<ddspf.dwBBitMask
            then ColorFormat:=GL_RGB
            else ColorFormat:=GL_BGR;
            DataType:=GL_UNSIGNED_BYTE;
            ElementSize := 3;
          end;
          8: begin
            if ddspf.dwFlags = DDPF_LUMINANCE then begin
              InternalFormat:=GL_LUMINANCE8; ColorFormat:=GL_LUMINANCE;
              DataType:=GL_UNSIGNED_BYTE;
              ElementSize := 1;
            end;
            if ddspf.dwFlags = DDPF_ALPHA then begin
              InternalFormat:=GL_ALPHA8; ColorFormat:=GL_ALPHA;
              DataType:=GL_UNSIGNED_BYTE;
              ElementSize := 1;
            end;
            if ddspf.dwFlags = DDPF_RGB then begin
              InternalFormat:=GL_LUMINANCE8; ColorFormat:=GL_LUMINANCE;
              DataType:=GL_UNSIGNED_BYTE;
              ElementSize := 1;
            end;
          end;
        end;
      end;
    end;
    if ElementSize>0 then exit;
    if (ddspf.dwFlags = DDPF_RGBA) and (ddspf.dwRGBBitCount = 32) then begin
        ColorFormat := GL_BGRA;
        InternalFormat := GL_RGBA8;
        DataType := GL_UNSIGNED_BYTE;
        ElementSize := 4;
    end else if (ddspf.dwFlags = DDPF_RGB) and (ddspf.dwRGBBitCount = 32) then begin
        ColorFormat := GL_BGR;
        InternalFormat := GL_RGBA8;
        DataType := GL_UNSIGNED_BYTE;
        ElementSize := 4;
    end else if (ddspf.dwFlags = DDPF_RGB) and (ddspf.dwRGBBitCount = 24) then begin
        ColorFormat := GL_BGR;
        InternalFormat := GL_RGB8;
        DataType := GL_UNSIGNED_BYTE;
        ElementSize := 3;
    end else if (ddspf.dwRGBBitCount = 8) then begin
	ColorFormat := GL_LUMINANCE; 
        InternalFormat := GL_LUMINANCE8; 
        DataType := GL_UNSIGNED_BYTE;
	ElementSize := 1;
    end else assert(false,'Unsupported format');
  end;
end;