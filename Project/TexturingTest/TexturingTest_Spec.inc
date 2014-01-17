
  //R3G3B2 Description
  spec_format := sfR3G3B2; pixel_format :=  pfUB332; pixel_size := 1;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := true;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateSpecial(spec_format);
  LogList.Add(LogString('IF_R3G3B2', Format, CheckFlags + CheckBaseFormat));

  //RGB565 Description
  spec_format := sfRGB565; pixel_format := pfUS565; pixel_size := 2;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := true;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateSpecial(spec_format);
  LogList.Add(LogString('IF_RGB565', Format, CheckFlags + CheckBaseFormat));

  //RGB5A1 Description
  spec_format := sfRGB5A1; pixel_format := pfUS5551; pixel_size := 2;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := true;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateSpecial(spec_format);
  LogList.Add(LogString('IF_RGB5A1', Format, CheckFlags + CheckBaseFormat));

  //RGB10A2 Description
  spec_format := sfRGB10A2; pixel_format := pfI1010102; pixel_size := 4;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := true;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateSpecial(spec_format);
  LogList.Add(LogString('IF_RGB10A2', Format, CheckFlags + CheckBaseFormat));

  //RGB10A2UI Description
  spec_format := sfRGB10A2UI; pixel_format := pfUI1010102; pixel_size := 4;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := true;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateSpecial(spec_format);
  LogList.Add(LogString('IF_RGB10A2UI', Format, CheckFlags + CheckBaseFormat));

  //R11FG11FB10F Description
  spec_format := sfR11FG11FB10F; pixel_format := pfFloat; pixel_size := 4;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := true;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateSpecial(spec_format);
  LogList.Add(LogString('IF_R11FG11FB10F', Format, CheckFlags + CheckBaseFormat));

  //RGB9E5 Description
  spec_format := sfRGB9E5; pixel_format := pfI5999; pixel_size := 4;
  mem_size := w*h*pixel_size; comp_count := 1;
  float_flag := false; spec_flag := true;
  comp_flag := false; depth_flag := false;

  Format := TImageFormatSelector.CreateSpecial(spec_format);
  LogList.Add(LogString('IF_RGB9E5', Format, CheckFlags + CheckBaseFormat));
