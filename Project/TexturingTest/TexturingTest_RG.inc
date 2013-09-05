  //RG8I Description
  base_format := bfRG; pixel_format := pfByte; comp_count := 2;
  pixel_size := 1 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateInt8(bfRG);
  LogList.Add(LogString('IF_RG8I', Format, CheckFlags + CheckBaseFormat));

  //RG8U Description
  base_format := bfRG; pixel_format := pfUByte; comp_count := 2;
  pixel_size := 1 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateUInt8(bfRG);
  LogList.Add(LogString('IF_RG8UI', Format, CheckFlags + CheckBaseFormat));

  //RG16I Description
  base_format := bfRG; pixel_format := pfShort; comp_count := 2;
  pixel_size := 2 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateInt16(bfRG);
  LogList.Add(LogString('IF_RG16I', Format, CheckFlags + CheckBaseFormat));

  //RG16U Description
  base_format := bfRG; pixel_format := pfUShort; comp_count := 2;
  pixel_size := 2 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateUInt16(bfRG);
  LogList.Add(LogString('IF_RG16UI', Format, CheckFlags + CheckBaseFormat));

  //RG32I Description
  base_format := bfRG; pixel_format := pfInt; comp_count := 2;
  pixel_size := 4 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateInt32(bfRG);
  LogList.Add(LogString('IF_RG32I', Format, CheckFlags + CheckBaseFormat));

  //RG32U Description
  base_format := bfRG; pixel_format := pfUInt; comp_count := 2;
  pixel_size := 4 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateUInt32(bfRG);
  LogList.Add(LogString('IF_RG32UI', Format, CheckFlags + CheckBaseFormat));

  //RG16F Description
  base_format := bfRG; pixel_format := pfFloat16; comp_count := 2;
  pixel_size := 2 * comp_count; mem_size := w*h*pixel_size;
  float_flag := true; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateFloat16(bfRG);
  LogList.Add(LogString('IF_RG16F', Format, CheckFlags + CheckBaseFormat));

  //RG32F Description
  base_format := bfRG; pixel_format := pfFloat; comp_count := 2;
  pixel_size := 4 * comp_count; mem_size := w*h*pixel_size;
  float_flag := true; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateFloat32(bfRG);
  LogList.Add(LogString('IF_RG32F', Format, CheckFlags + CheckBaseFormat));
