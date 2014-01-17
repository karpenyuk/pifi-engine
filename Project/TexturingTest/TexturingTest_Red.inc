  //Red8I Description
  base_format := bfRed; pixel_format := pfByte; pixel_size := 1;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateInt8(bfRed);
  LogList.Add(LogString('IF_Red8I', Format, CheckFlags + CheckBaseFormat));

  //Red8U Description
  base_format := bfRed; pixel_format := pfUByte; pixel_size := 1;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateUInt8(bfRed);
  LogList.Add(LogString('IF_Red8UI', Format, CheckFlags + CheckBaseFormat));

  //Red16I Description
  base_format := bfRed; pixel_format := pfShort; pixel_size := 2;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateInt16(bfRed);
  LogList.Add(LogString('IF_Red16I', Format, CheckFlags + CheckBaseFormat));

  //Red16U Description
  base_format := bfRed; pixel_format := pfUShort; pixel_size := 2;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateUInt16(bfRed);
  LogList.Add(LogString('IF_Red16UI', Format, CheckFlags + CheckBaseFormat));

  //Red32I Description
  base_format := bfRed; pixel_format := pfInt; pixel_size := 4;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateInt32(bfRed);
  LogList.Add(LogString('IF_Red32I', Format, CheckFlags + CheckBaseFormat));

  //Red32U Description
  base_format := bfRed; pixel_format := pfUInt; pixel_size := 4;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateUInt32(bfRed);
  LogList.Add(LogString('IF_Red32UI', Format, CheckFlags + CheckBaseFormat));

  //Red16F Description
  base_format := bfRed; pixel_format := pfFloat16; pixel_size := 2;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := true; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateFloat16(bfRed);
  LogList.Add(LogString('IF_Red16F', Format, CheckFlags + CheckBaseFormat));

  //Red32F Description
  base_format := bfRed; pixel_format := pfFloat; pixel_size := 4;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := true; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateFloat32(bfRed);
  LogList.Add(LogString('IF_Red32F', Format, CheckFlags + CheckBaseFormat));
