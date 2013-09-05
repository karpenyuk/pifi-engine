procedure RunTest;
const
    w = 256; h = 256;

type
    TLogString = record
      FormatName: string;
      FormatValue: cardinal;
      FormatDesc: string;
      OutString: string;
    end;
    PLogString = ^TLogString;

var Image: TImageSampler;
    Format, ResFormat: cardinal;
    base_format: TBaseImageFormat;
    pixel_format: TImagePixelFormat;
    ds_format: TDepthStencilFormat;
    spec_format: TImageSpecialFormat;
    comp_format: TS3TCCompressedFormats;
    pixel_size: integer;
    mem_size: integer;
    comp_count: integer;
    float_flag: boolean;
    spec_flag: boolean;
    comp_flag: boolean;
    depth_flag: boolean;

    s: string;
    log: TStringList;
    LogList: TDataList<TLogString>;

    function LogString(aFormatName: string; aFormatValue: cardinal; aDescription: string = ''): TLogString;
    begin
      result.FormatName := aFormatName; result.FormatValue := aFormatValue;
      result.FormatDesc := aDescription;
      result.OutString := aFormatName + #9+#9 + ' = ' + inttostr(aFormatValue) + ';';
      if aDescription <> '' then result.OutString := result.OutString + #9 +' //'+aDescription;
    end;

    function CheckFlags: string;
    var s: string;
    begin
      s := '';
      if TImageFormatBits.isFloatFormat(Format) <> float_flag
      then s := s + 'Float bit: ERROR; ';
      if TImageFormatBits.isCompressedFormat(Format) <> comp_flag
      then s := s + 'Compressed bit: ERROR; ';
      if TImageFormatBits.isDepthStencilFormat(Format) <> depth_flag
      then s := s + 'DepthStencil bit: ERROR; ';
      if TImageFormatBits.isSpecialFormat(Format) <> spec_flag
      then s := s + 'Special bit: ERROR; ';
      result := s;
    end;

    function CheckBaseFormat: string;
    var s: string;
    begin
      s := '';
      if depth_flag then begin
        if TImageFormatBits.GetDepthStencilFormat(Format) <> ds_format
        then s:=s+'DepthStencil format: ERROR; ';
      end else if spec_flag then begin
        if TImageFormatBits.GetSpecialFormat(Format) <> spec_format
        then s:=s+'Special format: ERROR; ';
      end else if comp_flag then begin
        if TImageFormatBits.GetCompressedFormat(Format) <> comp_format
        then s:=s+'Compressed format: ERROR; ';
      end else begin
        if TImageFormatBits.GetBaseFormat(Format) <> base_format
        then s:=s+'Base format: ERROR; ';
      end;

      if TImageFormatBits.GetPixelFormat(Format) <> pixel_format
      then s := s + 'PixelFormat: ERROR; ';

      if TImageFormatSelector.GetPixelSize(Format) <> pixel_size
      then s := s + 'Pixel size: ERROR; ';

      if not TImageFormatBits.isCompressedFormat(Format) then begin
        if TImageFormatSelector.GetMemSize(Format, w,h,1) <> mem_size
        then s := s + 'Mem size: ERROR; ';
      end;

      result := s;
    end;

var i: integer;

begin
  log := TStringList.Create;
  LogList := TDataList<TLogString>.Create;

  //Red Format test
  {$I TexturingTest_Red.inc}
  //RG Format test
  {$I TexturingTest_RG.inc}
  //RGB Format test
  {$I TexturingTest_RGB.inc}
  //BGR Format test
  {$I TexturingTest_BGR.inc}
  //RGBA Format test
  {$I TexturingTest_RGBA.inc}
  //BGRA Format test
  {$I TexturingTest_BGRA.inc}
  //Depth/Stencil Format test
  {$I TexturingTest_DS.inc}
  //Special Format test
  {$I TexturingTest_Spec.inc}
  //Compressed Format test
  {$I TexturingTest_Comp.inc}

  for i:= 0 to LogList.Count-1 do
    Log.add(LogList[i].OutString);
  Log.savetofile('TextureTest.log');
  Log.free;

end;
