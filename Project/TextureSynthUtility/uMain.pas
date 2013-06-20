unit uMain;

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  Spin,
  ExtDlgs,
  ComCtrls,
  Dialogs,
  uGLViewer,
  uImageAnalysisClasses,
  uImageAnalysis,
  uImageSynthesis,
  uGLImageSynthesis,
  uBaseTypes;

const
  VIEW_LEVEL = 0;

type
  TMainForm = class(TForm)
    MainPanel: TPanel;
    Memo1: TMemo;
    Label8: TLabel;
    ComboBox1: TComboBox;
    OpenPictureDialog: TOpenPictureDialog;
    GLViewer1: TGLViewer;
    OpenDataDialog: TOpenDialog;
    SaveDataDialog: TSaveDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label2: TLabel;
    Image1: TImage;
    Label9: TLabel;
    Label15: TLabel;
    AnalyzeButton: TButton;
    SpinEdit1: TSpinEdit;
    SaveDataButton: TButton;
    LoadDataButton: TButton;
    AnalysisProgressBar: TProgressBar;
    TilingComboBox: TComboBox;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Edit1: TEdit;
    SpinEdit2: TSpinEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    SynthesizeButton: TButton;
    SynthProgressBar: TProgressBar;
    Label10: TLabel;
    ComboBox3: TComboBox;
    JitterControllPanel: TPanel;
    Label14: TLabel;
    Label12: TLabel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    TrackBar5: TTrackBar;
    TrackBar6: TTrackBar;
    TrackBar7: TTrackBar;
    Label13: TLabel;
    TrackBar8: TTrackBar;
    LeftRight1: TUpDown;
    UpDown1: TUpDown;
    PerionPanel: TPanel;
    Edit5: TEdit;
    Label7: TLabel;
    Edit4: TEdit;
    Label6: TLabel;
    CheckBox4: TCheckBox;
    ApplyButton: TButton;
    SpinEdit3: TSpinEdit;
    Label11: TLabel;
    procedure ComboBox2Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure SynthesizeButtonClick(Sender: TObject);
    procedure AnalyzeButtonClick(Sender: TObject);

    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure GLViewer1ContextReady(Sender: TObject);
    procedure GLViewer1Render(Sender: TObject);

    procedure SaveDataButtonClick(Sender: TObject);
    procedure LoadDataButtonClick(Sender: TObject);
    procedure GLViewer1ContextDebugMessage(const AMessage: string);
    procedure UpDown1ChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
    procedure LeftRight1ChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
    procedure ApplyButtonClick(Sender: TObject);
  private
    FJTracks: array[0..7] of TTrackBar;
    procedure UpdateJitters;
  public
    AnalysisData: TAnalysisData;
    Analyzer: TAnalyzer;
    Synthesizer: TSynthesizer;
    GLSynthesizer: TGLSynthesizer;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  ImageLoader,
  dglOpenGL,
  uMiscUtils,
  uPrimitives,
  uBaseRenders,
  uBaseGL,
  uRenderResource,
  uImageSynthesisShaderGen,
  uImageFormats,
  uVMath,
  JPEG;

var
  AppDir: string;

  TexWidth: integer = 512;
  TexHeight: integer = 512;
  // one for analyzer, one for synthesizer
  Jitter: single = 25.0;
  tsKappa: single = 1.0;
  PeriodX: integer = 0;
  PeriodY: integer = 0;
  AutoJitter: boolean = true;

  SQ_VO: TVertexObject;
  SQ_Drawer: TGLVertexObject;
  ViewShader1, ViewShader2: TGLSLShaderProgram;
  Texture: TTexture;
  GLSynthTexture: TGLTextureObject;
  Render: TBaseRender;
  ExemplarTextureId: LongInt;
  SynthTextureId: LongInt;
  PatchesTextureId: LongInt;
  SamplerId: LongInt;
  SynthImage: TImageDesc;
  PatchesImage: TImageDesc;

  TextureChaged: array[0..2] of Boolean;

function _GetTime: Double;
var
  Freq, Tick: Int64;
begin
  QueryPerformanceFrequency(Freq);
  QueryPerformanceCounter(Tick);
  result := Tick / Freq;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  glFinish;
  glDeleteTextures(1, @ExemplarTextureId);
  glDeleteTextures(1, @SynthTextureId);
  glDeleteTextures(1, @PatchesTextureId);
  glDeleteSamplers(1, @SamplerId);
  GLViewer1.OnRender := nil;
  GLViewer1.Context.Deactivate;
  while SQ_VO.AttribsCount > 0 do
    SQ_VO.Attribs[0].Destroy;
  SQ_VO.Destroy;
  ViewShader1.Destroy;
  ViewShader2.Destroy;
  SQ_Drawer.Destroy;
  Texture.Descriptor.ImageDescriptor.Free;
  Texture.Descriptor.Destroy;
  Texture.Destroy;
  GLSynthTexture.Destroy;

  if GLSynthesizer.Initialized then
    GLSynthesizer.Finalize;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  x, y, e: integer;
  p: PByte;
  c: Graphics.TColor;
begin
  FJTracks[0] := TrackBar8;
  FJTracks[1] := TrackBar7;
  FJTracks[2] := TrackBar6;
  FJTracks[3] := TrackBar5;
  FJTracks[4] := TrackBar4;
  FJTracks[5] := TrackBar3;
  FJTracks[6] := TrackBar2;
  FJTracks[7] := TrackBar1;

  for e := 3 to 7 do
    FJTracks[e].Position := Round(100 * e / 8);

  AnalysisData := TAnalysisData.Create;
  Analyzer := TAnalyzer.CreateFrom(AnalysisData);
  Synthesizer := TSynthesizer.CreateFrom(AnalysisData);
  GLSynthesizer := TGLSynthesizer.CreateFrom(AnalysisData);

  with AnalysisData.Exemplar^ do
  begin
    FillChar(AnalysisData.Exemplar^, SizeOf(TImageDesc), $00);
    Width := Image1.Picture.Width;
    Height := Image1.Picture.Height;
    InternalFormat := GL_RGBA8;
    ColorFormat := GL_RGBA;
    ElementSize := 4;
    DataType := GL_UNSIGNED_BYTE;
    e := ElementSize;
    DataSize := Width * Height * e;
    GetMem(Data, DataSize);
  end;

  p := PByte(AnalysisData.Exemplar.Data);
  for y := 0 to Image1.Picture.Height - 1 do
    for x := 0 to Image1.Picture.Width - 1 do
    begin
      c := Image1.Picture.Bitmap.Canvas.Pixels[x, y];
      p[0] := c and $FF;
      p[1] := (c shr 8) and $FF;
      p[2] := (c shr 16) and $FF;
      p[3] := $FF;
      Inc(p, 4);
    end;

  for e := 0 to High(TextureChaged) do
      TextureChaged[e] := false;

  GLViewer1.Context.DebugContext := True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Analyzer.Destroy;
  Synthesizer.Destroy;
  GLSynthesizer.Destroy;
  AnalysisData.Destroy;
end;

// Analysis process
procedure TMainForm.AnalyzeButtonClick(Sender: TObject);
var
  p: Single;
  t: Double;
begin
  AnalyzeButton.Enabled := false;
  SynthesizeButton.Enabled := false;

  Analyzer.MaxCPUThreads := SpinEdit1.Value;
  case TilingComboBox.ItemIndex of
    0: AnalysisData.EdgePolicy := epNonRepeat;
    1: AnalysisData.EdgePolicy := epRepeat;
    2: AnalysisData.EdgePolicy := epNonRepeatDbl;
    3: AnalysisData.EdgePolicy := epRepeatDbl;
  end;

  Memo1.Lines.Add('Analysis started');
  t := _GetTime;
  Analyzer.Start;
  p := 0;
  AnalysisProgressBar.Position := 0;
  while p < 1 do
  begin
    Analyzer.Process;
    Sleep(10);
    p := Analyzer.Progress;
    AnalysisProgressBar.Position := Round(100 * p);
    Application.ProcessMessages;
  end;
  if not AnalysisData.IsValid then
    Exit;
  t := _GetTime - t;
  Memo1.Lines.Add('End analysis');
  Memo1.Lines.Add(Format('Expanded time %.2f msec', [1000 * t]));
  AnalyzeButton.Enabled := true;
  SynthesizeButton.Enabled := true;

  if GLSynthesizer.Supported then
  begin
    GLSynthesizer.Initialize;
    ComboBox1.ItemIndex := 3;
  end;
end;

// CPU synthesis process
procedure TMainForm.SynthesizeButtonClick(Sender: TObject);
var
  t: Double;
  p: Single;
begin
  if AnalysisData.IsValid then
  begin
    SynthProgressBar.Position := 0;
    AnalyzeButton.Enabled := false;
    SynthesizeButton.Enabled := false;

    with Synthesizer do
    begin
      Width := TexWidth;
      Height := TexHeight;
      JitterStrength := Jitter;
      Kappa := tsKappa;
      MaxCPUThreads := SpinEdit2.Value;
      if CheckBox4.Checked then
      begin
        JitterPeriodX := PeriodX;
        JitterPeriodY := PeriodY;
      end
    end;

    Memo1.Lines.Add('Start synthesis');
    t := _GetTime;
    p := 0;
    Synthesizer.Start;
    while p < 1 do
    begin
      Synthesizer.Process;
      Sleep(10);
      p := Synthesizer.Progress;
      SynthProgressBar.Position := Round(100 * p);
      Application.ProcessMessages;
    end;
    t := _GetTime - t;
    Memo1.Lines.Add('End synthesis');
    Memo1.Lines.Add(Format('Expanded time %.2f msec', [1000 * t]));
    SynthImage := Synthesizer.SynthImage[VIEW_LEVEL];
    PatchesImage := Synthesizer.PatchesImage[VIEW_LEVEL];
    TextureChaged[1] := True;
    TextureChaged[2] := True;
    ComboBox1.ItemIndex := 1;
    AnalyzeButton.Enabled := true;
    SynthesizeButton.Enabled := true;
  end
  else
    Memo1.Lines.Add('Unable to synthesize. Run analysis first.');
end;

procedure TMainForm.SaveDataButtonClick(Sender: TObject);
begin
  if AnalysisData.IsValid then
  begin
    if SaveDataDialog.Execute then
    begin
      AnalysisData.SaveToFile(SaveDataDialog.FileName);
      Memo1.Lines.Add('Analysis data saved.');
    end;
  end
  else
    Memo1.Lines.Add('Unable to save. Run analysis first.');
end;

procedure TMainForm.LeftRight1ChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  case Direction of
    updUp: GLSynthesizer.Shift(dirRight);
    updDown: GLSynthesizer.Shift(dirLeft);
  end;
end;

procedure TMainForm.LoadDataButtonClick(Sender: TObject);
var
  x, y: integer;
  p: PByte;
  c: Graphics.TColor;
  bmp: TBitmap;
begin
  if OpenDataDialog.Execute then
  begin
    AnalysisData.LoadFromFile(OpenDataDialog.FileName);
    p := PByte(AnalysisData.Exemplar.Data);
    bmp := TBitmap.Create;
    bmp.Width := AnalysisData.Exemplar.Width;
    bmp.Height := AnalysisData.Exemplar.Height;
    for y := 0 to bmp.Height - 1 do
      for x := 0 to bmp.Width - 1 do
      begin
        c := p[0];
        c := c or (p[1] shl 8);
        c := c or (p[2] shl 16);
        bmp.Canvas.Pixels[x, y] := c;
        Inc(p, AnalysisData.Exemplar.ElementSize);
      end;
    Image1.Picture.Assign(bmp);
    bmp.Free;

    case AnalysisData.EdgePolicy of
      epNonRepeat: TilingComboBox.ItemIndex := 0;
      epRepeat: TilingComboBox.ItemIndex := 1;
      epNonRepeatDbl: TilingComboBox.ItemIndex := 2;
      epRepeatDbl: TilingComboBox.ItemIndex := 3;
    end;

    Memo1.Lines.Add('Analysis data loaded.');
    AnalysisProgressBar.Position := 100;
    TextureChaged[0] := True;

    GLSynthesizer.Initialize;
  end;
end;

procedure TMainForm.ApplyButtonClick(Sender: TObject);
begin
  GLSynthesizer.CoherenceWeight := tsKappa;
end;

procedure TMainForm.CheckBox3Click(Sender: TObject);
begin
  // tsAnalyzer.Toroidal := CheckBox3.Checked;
  Memo1.Lines.Add('You must reanalize and resynthesize image');
end;

// Periodic jitter control
procedure TMainForm.CheckBox4Click(Sender: TObject);
begin
  if CheckBox4.Checked then
  begin
    Edit4.Enabled := true;
    Edit4Change(Edit4);
    Edit5.Enabled := true;
    Edit4Change(Edit5);
  end
  else
  begin
    Edit4.Enabled := false;
    Edit4.Color := clBtnFace;
    Edit5.Enabled := false;
    Edit5.Color := clBtnFace;
  end;
end;

// Pass number choise
procedure TMainForm.ComboBox2Change(Sender: TObject);
var
  n: integer;
begin
  n := 0;
  case ComboBox3.ItemIndex of
    1: n := 2;
    2: n := 4;
    3: n := 8;
  end;
  Synthesizer.CorrectionSubpassesCount := n;
  GLSynthesizer.CorrectionSubpassesCount := n;
end;

// Image dimension correct input
procedure TMainForm.Edit1Change(Sender: TObject);
var
  s1, s2: string;
  p, err: integer;
  w, h: integer;
  correct: boolean;
begin
  correct := true;
  w := 0;
  h := 0;
  s1 := Edit1.Text;
  p := Pos('x', s1);
  if p > 0 then
  begin
    s2 := Copy(s1, p + 1, length(s1) - p);
    Delete(s1, p, length(s1) - p + 1);
    val(s1, w, err);
    if err > 0 then
      correct := false;
    val(s2, h, err);
    if err > 0 then
      correct := false;
  end
  else
    correct := false;

  if correct then
  begin
    Edit1.Color := clWindow;
    if (TexWidth <> w) or (TexHeight <> h) then
    begin
      TexWidth := w;
      TexHeight := h;
    end;
  end
  else
    Edit1.Color := clRed;
end;

// Jitter strength correct input
procedure TMainForm.Edit2Change(Sender: TObject);
var
  err: integer;
  j: single;
begin
  val(Edit2.Text, j, err);
  if err > 0 then
    Edit2.Color := clRed
  else
  begin
    Edit2.Color := clWindow;
    Jitter := j;
  end;
end;

// Coherent threshold correct input
procedure TMainForm.Edit3Change(Sender: TObject);
var
  err: integer;
  k: single;
begin
  val(Edit3.Text, k, err);
  if err > 0 then
    Edit3.Color := clRed
  else if k > 0 then
  begin
    Edit3.Color := clWindow;
    tsKappa := k;
  end
  else
    Edit3.Color := clRed;
end;

// Jitter period correct input
procedure TMainForm.Edit4Change(Sender: TObject);
var
  err: integer;
  x: integer;
begin
  val(TEdit(Sender).Text, x, err);
  if err > 0 then
    TEdit(Sender).Color := clRed
  else if x > 0 then
  begin
    TEdit(Sender).Color := clWindow;
    case TEdit(Sender).Tag of
      0:
        PeriodX := x;
      1:
        PeriodY := x;
    end;
  end
  else
    TEdit(Sender).Color := clRed;
end;

// Jitter control
procedure TMainForm.TrackBar1Change(Sender: TObject);
var
  L: integer;
begin
  if Assigned(GLSynthesizer) then
  begin
    L := TTrackBar(Sender).Tag;
    if L < GLSynthesizer.LevelCount then
    begin
      GLSynthesizer.JitterStrength[L] :=
        Jitter * TTrackBar(Sender).Position / TTrackBar(Sender).Max;
    end;
  end;
end;

procedure TMainForm.UpdateJitters;
var
  L: integer;
  TB: TTrackBar;
begin
  for L := 0 to High(FJTracks) do
  begin
    TB := FJTracks[L];
    if L < GLSynthesizer.LevelCount then
    begin
      TB.Enabled := True;
      GLSynthesizer.JitterStrength[L] := Jitter * TB.Position / TB.Max;
    end
    else
      TB.Enabled := False;
  end;
end;

procedure TMainForm.UpDown1ChangingEx(Sender: TObject; var AllowChange: Boolean;
  NewValue: SmallInt; Direction: TUpDownDirection);
begin
  case Direction of
    updUp: GLSynthesizer.Shift(dirUp);
    updDown: GLSynthesizer.Shift(dirDown);
  end;
end;

// Exemplar image loading
procedure TMainForm.Image1DblClick(Sender: TObject);
var
  img: TImageDesc;
begin
  if OpenPictureDialog.Execute then
  begin
    Image1.Picture.LoadFromFile(OpenPictureDialog.FileName);
    img := LoadImage(OpenPictureDialog.FileName);
    AnalysisData.SetExemplar(img);
    img.Free;
    Memo1.Lines.Add(Format('Image %dx%d was loaded', [Image1.Picture.Width,
      Image1.Picture.Height]));
    TextureChaged[0] := true;
    ComboBox1.ItemIndex := 0;
    AnalysisProgressBar.Position := 0;
  end;
end;

procedure TMainForm.GLViewer1ContextDebugMessage(const AMessage: string);
begin
  WriteLn(AMessage);
end;

procedure TMainForm.GLViewer1ContextReady(Sender: TObject);
var
  ver: TApiVersion;
  path: string;
begin
  ver.GAPI := avGL;
  ver.Version := 420;
  Render := vRegisteredRenders.GetCompatibleRender(ver);

{$IFDEF MSWindows}
  path := '..\..\Source\Media\'; { :-\ }
{$ENDIF}
{$IFDEF Linux}
  path := '../../Source/Media/'; { :-/ }
{$ENDIF}

  ViewShader1 := TGLSLShaderProgram.Create;
  ViewShader1.AttachShaderFromFile(stVertex, path + 'ScreenQuadShader.Vert');
  ViewShader1.AttachShaderFromFile(stFragment, path + 'ScreenQuadShader.Frag');
  ViewShader1.LinkShader;
  if ViewShader1.Error then
  begin
    showmessage(ViewShader1.Log);
    Halt(0);
  end;

  ViewShader2 := TGLSLShaderProgram.Create;
  ViewShader2.AttachShaderFromFile(stVertex, path + 'ScreenQuadShader.Vert');
  ViewShader2.AttachShaderFromFile(stFragment, path + 'ScreenQuadShaderInt.Frag');
  ViewShader2.LinkShader;
  if ViewShader2.Error then
  begin
    showmessage(ViewShader2.Log);
    Halt(0);
  end;

  glGenTextures(1, @ExemplarTextureId);
  glBindTexture(GL_TEXTURE_2D, ExemplarTextureId);
  glTextureParameterfEXT(
    ExemplarTextureId,
    GL_TEXTURE_2D,
    GL_GENERATE_MIPMAP_SGIS,
    GL_TRUE);
  glTextureParameterfEXT(ExemplarTextureId, GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,
    GL_REPEAT);
  glTextureParameterfEXT(ExemplarTextureId, GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,
    GL_REPEAT);
  glTextureParameterfEXT(ExemplarTextureId, GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,
    GL_LINEAR);
  glTextureParameterfEXT(ExemplarTextureId, GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
    GL_LINEAR_MIPMAP_LINEAR);
  with AnalysisData.Exemplar^ do
      glTextureImage2DEXT(ExemplarTextureId, GL_TEXTURE_2D, 0, InternalFormat,
      Width, Height, 0, ColorFormat, DataType, Data);

  glGenTextures(1, @SynthTextureId);
  glBindTexture(GL_TEXTURE_2D, SynthTextureId);
  glTextureParameterfEXT(
    SynthTextureId,
    GL_TEXTURE_2D,
    GL_GENERATE_MIPMAP_SGIS,
    GL_TRUE);
  glTextureParameterfEXT(SynthTextureId, GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,
    GL_REPEAT);
  glTextureParameterfEXT(SynthTextureId, GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,
    GL_REPEAT);
  glTextureParameterfEXT(SynthTextureId, GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,
    GL_LINEAR);
  glTextureParameterfEXT(SynthTextureId, GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
    GL_LINEAR_MIPMAP_LINEAR);

  glGenTextures(1, @PatchesTextureId);
  glBindTexture(GL_TEXTURE_2D, PatchesTextureId);
  glTextureParameterfEXT(
    PatchesTextureId,
    GL_TEXTURE_2D,
    GL_GENERATE_MIPMAP_SGIS,
    GL_FALSE);
  glTextureParameterfEXT(PatchesTextureId, GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,
    GL_REPEAT);
  glTextureParameterfEXT(PatchesTextureId, GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,
    GL_REPEAT);
  glTextureParameterfEXT(PatchesTextureId, GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,
    GL_NEAREST);
  glTextureParameterfEXT(PatchesTextureId, GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
    GL_NEAREST);

  glGenSamplers(1, @SamplerId);
  glSamplerParameteri(SamplerId, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glSamplerParameteri(SamplerId, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glSamplerParameteri(SamplerId, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glSamplerParameteri(SamplerId, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  Texture := TTexture.CreateOwned(Self);
  Texture.Descriptor := TImageSampler.CreateBitmap(
    TImageFormatSelector.CreateInt8(bfRGBA),
    TexWidth, TexHeight, True);
  Texture.Descriptor.ImageDescriptor.InternalFormat := GL_RGBA8;
  Texture.Descriptor.ImageDescriptor.ColorFormat := GL_RGBA;
  Texture.Descriptor.ImageDescriptor.DataType := GL_UNSIGNED_BYTE;
  GLSynthTexture := TGLTextureObject.CreateFrom(texture);
  GLSynthesizer.DestinationTexture := GLSynthTexture;
  GLSynthTexture.AllocateStorage;

  SQ_VO := CreatePlane(2, 2);
  SQ_Drawer := TGLVertexObject.CreateFrom(SQ_VO);
end;

procedure TMainForm.GLViewer1Render(Sender: TObject);
var
  ratio: TVector;
  w, h, L: integer;
  shader: TGLSLShaderProgram;
begin
  UpdateJitters;

  if TextureChaged[0] then
  begin
    glBindTexture(GL_TEXTURE_2D, ExemplarTextureId);
    with AnalysisData.Exemplar^ do
      glTexImage2D(GL_TEXTURE_2D, 0, InternalFormat, Width, Height, 0,
        ColorFormat, DataType, Data);
    TextureChaged[0] := false;
  end;

  if TextureChaged[1] then
  begin
    glBindTexture(GL_TEXTURE_2D, SynthTextureId);
    with SynthImage do
    begin
      glTexImage2D(GL_TEXTURE_2D, 0, InternalFormat, Width, Height, 0,
        ColorFormat, DataType, Data);
      Free;
    end;
    TextureChaged[1] := false;
  end;

  if TextureChaged[2] then
  begin
    glBindTexture(GL_TEXTURE_2D, PatchesTextureId);
    with PatchesImage do
    begin
      glTexImage2D(GL_TEXTURE_2D, 0, InternalFormat, Width, Height, 0,
        ColorFormat, DataType, Data);
      Free;
    end;
    TextureChaged[2] := false;
  end;

  if GLSynthesizer.Supported and GLSynthesizer.Initialized then
  begin
    GLSynthesizer.Process;
  end;

  GLViewer1.Context.ClearDevice;

  w := GLViewer1.Width;
  h := GLViewer1.Height;
  shader := ViewShader1;
  glActiveTexture(GL_TEXTURE0);
  case ComboBox1.ItemIndex of
    0:
      begin
        glBindTexture(GL_TEXTURE_2D, ExemplarTextureId);
        w := AnalysisData.Exemplar.Width;
        h := AnalysisData.Exemplar.Height;
      end;
    1:
      begin
        glBindTexture(GL_TEXTURE_2D, SynthTextureId);
        w := SynthImage.Width;
        h := SynthImage.Height;
      end;
    2:
      begin
        glBindTexture(GL_TEXTURE_2D, PatchesTextureId);
        w := PatchesImage.Width;
        h := PatchesImage.Height;
      end;
    3:
      if GLSynthesizer.Initialized then
      begin
        glBindSampler(0, SamplerId);
        L := SpinEdit3.Value;
        if L > -1 then
        begin
          glSamplerParameteri(SamplerId, GL_TEXTURE_MAX_LOD, L);
          glSamplerParameteri(SamplerId, GL_TEXTURE_MIN_LOD, L);
        end
        else
        begin
          glSamplerParameteri(SamplerId, GL_TEXTURE_MAX_LOD, GLSynthTexture.ImageDescriptor.Levels - 1);
          glSamplerParameteri(SamplerId, GL_TEXTURE_MIN_LOD, 0);
        end;
        glBindTexture(GL_TEXTURE_2D, GLSynthTexture.Id);
        w := TexWidth;
        h := TexHeight;
      end;
    4:
      if GLSynthesizer.Initialized then
      begin
        shader := ViewShader2;
        glBindTexture(GL_TEXTURE_2D, GLSynthesizer.PachesTextureIDs[VIEW_LEVEL]);
        w := GLSynthesizer.SideSize;
        h := w;
      end;
    else
    begin
      glBindTexture(GL_TEXTURE_2D, 0);
    end;
  end;

  shader.Apply;

  ratio := TVector.Make(GLViewer1.Width / w, GLViewer1.Height / h);
  if ratio.X < ratio.Y then
    ratio := TVector.Make(1, ratio.X / ratio.Y)
  else
    ratio := TVector.Make(ratio.Y / ratio.X, 1);
  shader.SetUniform('Ratio', ratio.Vec2);

  SQ_Drawer.RenderVO(shader.Id);

  glBindSampler(0, 0);
end;

end.
