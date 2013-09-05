  //RGB8I Description
  base_format := bfRGB; pixel_format := pfByte; comp_count := 3;
  pixel_size := 1 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateInt8(bfRGB);
  LogList.Add(LogString('IF_RGB8I', Format, CheckFlags + CheckBaseFormat));

  //RGB8U Description
  base_format := bfRGB; pixel_format := pfUByte; comp_count := 3;
  pixel_size := 1 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateUInt8(bfRGB);
  LogList.Add(LogString('IF_RGB8UI', Format, CheckFlags + CheckBaseFormat));

  //RGB16I Description
  base_format := bfRGB; pixel_format := pfShort; comp_count := 3;
  pixel_size := 2 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateInt16(bfRGB);
  LogList.Add(LogString('IF_RGB16I', Format, CheckFlags + CheckBaseFormat));

  //RGB16U Description
  base_format := bfRGB; pixel_format := pfUShort; comp_count := 3;
  pixel_size := 2 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateUInt16(bfRGB);
  LogList.Add(LogString('IF_RGB16UI', Format, CheckFlags + CheckBaseFormat));

  //RGB32I Description
  base_format := bfRGB; pixel_format := pfInt; comp_count := 3;
  pixel_size := 4 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateInt32(bfRGB);
  LogList.Add(LogString('IF_RGB32I', Format, CheckFlags + CheckBaseFormat));

  //RGB32U Description
  base_format := bfRGB; pixel_format := pfUInt; comp_count := 3;
  pixel_size := 4 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateUInt32(bfRGB);
  LogList.Add(LogString('IF_RGB32UI', Format, CheckFlags + CheckBaseFormat));

  //RGB16F Description
  base_format := bfRGB; pixel_format := pfFloat16; comp_count := 3;
  pixel_size := 2 * comp_count; mem_size := w*h*pixel_size;
  float_flag := true; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateFloat16(bfRGB);
  LogList.Add(LogString('IF_RGB16F', Format, CheckFlags + CheckBaseFormat));

  //RGB32F Description
  base_format := bfRGB; pixel_format := pfFloat; comp_count := 3;
  pixel_size := 4 * comp_count; mem_size := w*h*pixel_size;
  float_flag := true; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateFloat32(bfRGB);
  LogList.Add(LogString('IF_RGB32F', Format, CheckFlags + CheckBaseFormat));
