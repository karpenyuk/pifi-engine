
  //Depth16 Description
  ds_format := dfDepth16; pixel_format :=  pfUShort; pixel_size := 2;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := true; spec_flag := false;
  comp_flag := false; depth_flag := true;

  Format := TImageFormatSelector.CreateDepthStencil(16, false);
  LogList.Add(LogString('IF_Depth16', Format, CheckFlags + CheckBaseFormat));

  //Depth24 Description
  ds_format := dfDepth24; pixel_format :=  pfUInt; pixel_size := 4;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := true; spec_flag := false;
  comp_flag := false; depth_flag := true;

  Format := TImageFormatSelector.CreateDepthStencil(24, false);
  LogList.Add(LogString('IF_Depth24', Format, CheckFlags + CheckBaseFormat));

  //Depth32 Description
  ds_format := dfDepth32; pixel_format :=  pfUInt; pixel_size := 4;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := true; spec_flag := false;
  comp_flag := false; depth_flag := true;

  Format := TImageFormatSelector.CreateDepthStencil(32, false);
  LogList.Add(LogString('IF_Depth32', Format, CheckFlags + CheckBaseFormat));

{  //Depth32F Description
  ds_format := dfDepth32F; pixel_format :=  pfFloat; pixel_size := 4;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := true; spec_flag := false;
  comp_flag := false; depth_flag := true;

  Format := TImageFormatSelector.CreateDepthStencil(32, false);
  LogList.Add(LogString('IF_Depth32F', Format, CheckFlags + CheckBaseFormat));
}
  //StencilIndex8 Description
  ds_format := dfStencilIndex8; pixel_format := pfUByte; pixel_size := 1;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := true; spec_flag := false;
  comp_flag := false; depth_flag := true;

  Format := TImageFormatSelector.CreateDepthStencil(0, true);
  LogList.Add(LogString('IF_StencilIndex8', Format, CheckFlags + CheckBaseFormat));

  //Depth24Stencil8 Description
  ds_format := dfDepth24Stencil8; pixel_format := pfUI24_8; pixel_size := 4;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := true; spec_flag := false;
  comp_flag := false; depth_flag := true;

  Format := TImageFormatSelector.CreateDepthStencil(24, true);
  LogList.Add(LogString('IF_Depth24Stencil8', Format, CheckFlags + CheckBaseFormat));

  //Depth32FStencil8 Description
  ds_format := dfDepth32FStencil8; pixel_format := pfF32UB8; pixel_size := 5;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := true; spec_flag := false;
  comp_flag := false; depth_flag := true;

  Format := TImageFormatSelector.CreateDepthStencil(32, true);
  LogList.Add(LogString('IF_Depth32FStencil8', Format, CheckFlags + CheckBaseFormat));
