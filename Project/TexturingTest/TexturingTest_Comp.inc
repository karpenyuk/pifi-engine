
  //RGB_DXT1 Description
  comp_format := cfRGB_DXT1; pixel_format :=  pfUByte; pixel_size := 8;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := false;
  comp_flag := true; depth_flag := false;

  Format := TImageFormatSelector.CreateCompressed(comp_format);
  LogList.Add(LogString('IF_RGB_DXT1', Format, CheckFlags + CheckBaseFormat));

  //SRGB_DXT1 Description
  comp_format := cfSRGB_DXT1; pixel_format := pfUByte; pixel_size := 8;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := false;
  comp_flag := true; depth_flag := false;

  Format := TImageFormatSelector.CreateCompressed(comp_format);
  LogList.Add(LogString('IF_SRGB_DXT1', Format, CheckFlags + CheckBaseFormat));

  //RGBA_DXT1 Description
  comp_format := cfRGBA_DXT1; pixel_format := pfUByte; pixel_size := 8;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := false;
  comp_flag := true; depth_flag := false;

  Format := TImageFormatSelector.CreateCompressed(comp_format);
  LogList.Add(LogString('IF_RGBA_DXT1', Format, CheckFlags + CheckBaseFormat));

  //SRGBA_DXT1 Description
  comp_format := cfSRGBA_DXT1; pixel_format := pfUByte; pixel_size := 8;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := false;
  comp_flag := true; depth_flag := false;

  Format := TImageFormatSelector.CreateCompressed(comp_format);
  LogList.Add(LogString('IF_SRGBA_DXT1', Format, CheckFlags + CheckBaseFormat));

  //RGBA_DXT3 Description
  comp_format := cfRGBA_DXT3; pixel_format := pfUByte; pixel_size := 16;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := false;
  comp_flag := true; depth_flag := false;

  Format := TImageFormatSelector.CreateCompressed(comp_format);
  LogList.Add(LogString('IF_RGBA_DXT3', Format, CheckFlags + CheckBaseFormat));

  //SRGBA_DXT3 Description
  comp_format := cfSRGBA_DXT3; pixel_format := pfUByte; pixel_size := 16;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := false;
  comp_flag := true; depth_flag := false;

  Format := TImageFormatSelector.CreateCompressed(comp_format);
  LogList.Add(LogString('IF_SRGBA_DXT3', Format, CheckFlags + CheckBaseFormat));

  //RGBA_DXT5 Description
  comp_format := cfRGBA_DXT5; pixel_format := pfUByte; pixel_size := 16;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := false;
  comp_flag := true; depth_flag := false;

  Format := TImageFormatSelector.CreateCompressed(comp_format);
  LogList.Add(LogString('IF_RGBA_DXT5', Format, CheckFlags + CheckBaseFormat));

  //SRGBA_DXT5 Description
  comp_format := cfSRGBA_DXT5; pixel_format := pfUByte; pixel_size := 16;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := false;
  comp_flag := true; depth_flag := false;

  Format := TImageFormatSelector.CreateCompressed(comp_format);
  LogList.Add(LogString('IF_SRGBA_DXT5', Format, CheckFlags + CheckBaseFormat));
