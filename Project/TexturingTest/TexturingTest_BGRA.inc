  //BGRA8I Description
  base_format := bfBGRA; pixel_format := pfByte; comp_count := 4;
  pixel_size := 1 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateInt8(bfBGRA);
  LogList.Add(LogString('IF_BGRA8I', Format, CheckFlags + CheckBaseFormat));

  //BGRA8U Description
  base_format := bfBGRA; pixel_format := pfUByte; comp_count := 4;
  pixel_size := 1 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateUInt8(bfBGRA);
  LogList.Add(LogString('IF_BGRA8UI', Format, CheckFlags + CheckBaseFormat));

  //BGRA16I Description
  base_format := bfBGRA; pixel_format := pfShort; comp_count := 4;
  pixel_size := 2 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateInt16(bfBGRA);
  LogList.Add(LogString('IF_BGRA16I', Format, CheckFlags + CheckBaseFormat));

  //BGRA16U Description
  base_format := bfBGRA; pixel_format := pfUShort; comp_count := 4;
  pixel_size := 2 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateUInt16(bfBGRA);
  LogList.Add(LogString('IF_BGRA16UI', Format, CheckFlags + CheckBaseFormat));

  //BGRA32I Description
  base_format := bfBGRA; pixel_format := pfInt; comp_count := 4;
  pixel_size := 4 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateInt32(bfBGRA);
  LogList.Add(LogString('IF_BGRA32I', Format, CheckFlags + CheckBaseFormat));

  //BGRA32U Description
  base_format := bfBGRA; pixel_format := pfUInt; comp_count := 4;
  pixel_size := 4 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateUInt32(bfBGRA);
  LogList.Add(LogString('IF_BGRA32UI', Format, CheckFlags + CheckBaseFormat));

  //BGRA16F Description
  base_format := bfBGRA; pixel_format := pfFloat16; comp_count := 4;
  pixel_size := 2 * comp_count; mem_size := w*h*pixel_size;
  float_flag := true; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateFloat16(bfBGRA);
  LogList.Add(LogString('IF_BGRA16F', Format, CheckFlags + CheckBaseFormat));

  //BGRA32F Description
  base_format := bfBGRA; pixel_format := pfFloat; comp_count := 4;
  pixel_size := 4 * comp_count; mem_size := w*h*pixel_size;
  float_flag := true; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateFloat32(bfBGRA);
  LogList.Add(LogString('IF_BGRA32F', Format, CheckFlags + CheckBaseFormat));
