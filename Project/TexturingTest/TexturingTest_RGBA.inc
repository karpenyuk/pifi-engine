  //RGBA8I Description
  base_format := bfRGBA; pixel_format := pfByte; comp_count := 4;
  pixel_size := 1 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateInt8(bfRGBA);
  LogList.Add(LogString('IF_RGBA8I', Format, CheckFlags + CheckBaseFormat));

  //RGBA8U Description
  base_format := bfRGBA; pixel_format := pfUByte; comp_count := 4;
  pixel_size := 1 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateUInt8(bfRGBA);
  LogList.Add(LogString('IF_RGBA8UI', Format, CheckFlags + CheckBaseFormat));

  //RGBA16I Description
  base_format := bfRGBA; pixel_format := pfShort; comp_count := 4;
  pixel_size := 2 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateInt16(bfRGBA);
  LogList.Add(LogString('IF_RGBA16I', Format, CheckFlags + CheckBaseFormat));

  //RGBA16U Description
  base_format := bfRGBA; pixel_format := pfUShort; comp_count := 4;
  pixel_size := 2 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateUInt16(bfRGBA);
  LogList.Add(LogString('IF_RGBA16UI', Format, CheckFlags + CheckBaseFormat));

  //RGBA32I Description
  base_format := bfRGBA; pixel_format := pfInt; comp_count := 4;
  pixel_size := 4 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateInt32(bfRGBA);
  LogList.Add(LogString('IF_RGBA32I', Format, CheckFlags + CheckBaseFormat));

  //RGBA32U Description
  base_format := bfRGBA; pixel_format := pfUInt; comp_count := 4;
  pixel_size := 4 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateUInt32(bfRGBA);
  LogList.Add(LogString('IF_RGBA32UI', Format, CheckFlags + CheckBaseFormat));

  //RGBA16F Description
  base_format := bfRGBA; pixel_format := pfFloat16; comp_count := 4;
  pixel_size := 2 * comp_count; mem_size := w*h*pixel_size;
  float_flag := true; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateFloat16(bfRGBA);
  LogList.Add(LogString('IF_RGBA16F', Format, CheckFlags + CheckBaseFormat));

  //RGBA32F Description
  base_format := bfRGBA; pixel_format := pfFloat; comp_count := 4;
  pixel_size := 4 * comp_count; mem_size := w*h*pixel_size;
  float_flag := true; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateFloat32(bfRGBA);
  LogList.Add(LogString('IF_RGBA32F', Format, CheckFlags + CheckBaseFormat));
