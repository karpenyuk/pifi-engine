  //BGR8I Description
  base_format := bfBGR; pixel_format := pfByte; comp_count := 3;
  pixel_size := 1 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateInt8(bfBGR);
  LogList.Add(LogString('IF_BGR8I', Format, CheckFlags + CheckBaseFormat));

  //BGR8U Description
  base_format := bfBGR; pixel_format := pfUByte; comp_count := 3;
  pixel_size := 1 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateUInt8(bfBGR);
  LogList.Add(LogString('IF_BGR8UI', Format, CheckFlags + CheckBaseFormat));

  //BGR16I Description
  base_format := bfBGR; pixel_format := pfShort; comp_count := 3;
  pixel_size := 2 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateInt16(bfBGR);
  LogList.Add(LogString('IF_BGR16I', Format, CheckFlags + CheckBaseFormat));

  //BGR16U Description
  base_format := bfBGR; pixel_format := pfUShort; comp_count := 3;
  pixel_size := 2 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateUInt16(bfBGR);
  LogList.Add(LogString('IF_BGR16UI', Format, CheckFlags + CheckBaseFormat));

  //BGR32I Description
  base_format := bfBGR; pixel_format := pfInt; comp_count := 3;
  pixel_size := 4 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateInt32(bfBGR);
  LogList.Add(LogString('IF_BGR32I', Format, CheckFlags + CheckBaseFormat));

  //BGR32U Description
  base_format := bfBGR; pixel_format := pfUInt; comp_count := 3;
  pixel_size := 4 * comp_count; mem_size := w*h*pixel_size;
  float_flag := false; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateUInt32(bfBGR);
  LogList.Add(LogString('IF_BGR32UI', Format, CheckFlags + CheckBaseFormat));

  //BGR16F Description
  base_format := bfBGR; pixel_format := pfFloat16; comp_count := 3;
  pixel_size := 2 * comp_count; mem_size := w*h*pixel_size;
  float_flag := true; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateFloat16(bfBGR);
  LogList.Add(LogString('IF_BGR16F', Format, CheckFlags + CheckBaseFormat));

  //BGR32F Description
  base_format := bfBGR; pixel_format := pfFloat; comp_count := 3;
  pixel_size := 4 * comp_count; mem_size := w*h*pixel_size;
  float_flag := true; spec_flag := false;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateFloat32(bfBGR);
  LogList.Add(LogString('IF_BGR32F', Format, CheckFlags + CheckBaseFormat));
