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
  uBaseTypes;

type
  TMainForm = class(TForm)
    MainPanel: TPanel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label2: TLabel;
    Image1: TImage;
    Label9: TLabel;
    CheckBox3: TCheckBox;
    Button1: TButton;
    SpinEdit1: TSpinEdit;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label5: TLabel;
    Edit1: TEdit;
    SpinEdit2: TSpinEdit;
    Edit2: TEdit;
    CheckBox4: TCheckBox;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit3: TEdit;
    CheckBox1: TCheckBox;
    Button2: TButton;
    TabSheet3: TTabSheet;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    ComboBox2: TComboBox;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    TrackBar5: TTrackBar;
    TrackBar6: TTrackBar;
    TrackBar7: TTrackBar;
    TrackBar8: TTrackBar;
    Label14: TLabel;
    TrackBar9: TTrackBar;
    Button3: TButton;
    Label8: TLabel;
    ComboBox1: TComboBox;
    OpenPictureDialog: TOpenPictureDialog;
    GLViewer1: TGLViewer;
    procedure TrackBar9Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GLCadencerProgress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure GLViewer1ContextReady(Sender: TObject);
    procedure GLViewer1Render(Sender: TObject);
    procedure GLViewer1CanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
    AnalysisData: TAnalysisData;
    Analyzer: TAnalyzer;
    Synthesizer: TSynthesizer;
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
  uVMath,
  JPEG;

var
  AppDir: string;
  mx, my: integer;
  KeyDeltaTime: Double;

  TexWidth: integer = 512;
  TexHeight: integer = 512;
  ThreadsNumber: array [0 .. 1] of word = (
    2,
    2
  );
  // one for analyzer, one for synthesizer
  Jitter: single = 1.0;
  tsKappa: single = 1.0;
  PeriodX: integer = 0;
  PeriodY: integer = 0;
  AutoJitter: boolean = true;

  SQ_VO: TVertexObject;
  SQ_Drawer: TGLVertexObject;
  ViewShader: TGLSLShaderProgram;
  Render: TBaseRender;
  ExemplarTextureId: LongInt;
  Proj: TMatrix;

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
  GLViewer1.OnRender := nil;
  GLViewer1.Context.Deactivate;
  while SQ_VO.AttribsCount > 0 do
    SQ_VO.Attribs[0].Destroy;
  SQ_VO.Destroy;
  ViewShader.Destroy;
  SQ_Drawer.Destroy;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  img: TImageDesc;
  x, y, e: integer;
  p: PByte;
  c: Graphics.TColor;
begin
  AnalysisData := TAnalysisData.Create;
  Analyzer := TAnalyzer.Create(AnalysisData);
  Synthesizer := TSynthesizer.Create(AnalysisData);

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

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  AnalysisData.Destroy;
  Analyzer.Destroy;
  Synthesizer.Destroy;
end;

// Analysis process
procedure TMainForm.Button1Click(Sender: TObject);
var
  t: Double;
begin
  if Button1.Tag = 0 then
  begin
    Button1.Tag := 1;
    Button1.Caption := 'Break analysis';
    Button2.Enabled := false;

    t := _GetTime;
    Analyzer.Start;
    while not Analyzer.Done do
    begin
      Analyzer.Process;
      Sleep(1);
      Application.ProcessMessages;
    end;
    t := _GetTime - t;
    Memo1.Lines.Add('End analysis');
    Memo1.Lines.Add(Format('Expanded time %.2f msec', [1000 * t]));
  end
  else if Button1.Tag = 1 then
    Analyzer.Stop;

  Button1.Tag := 0;
  Button1.Caption := 'Analyse';
  Button2.Enabled := true;
end;

// CPU synthesis process
procedure TMainForm.Button2Click(Sender: TObject);
begin
  Button1.Enabled := false;
  Button2.Enabled := false;

  // with Synthesizer do begin
  // Analyzer := tsAnalyzer;
  // Width := TexWidth;
  // Height := TexHeight;
  // JitterStrength := Jitter;
  // Kappa := tsKappa;
  // NumThreads := ThreadsNumber[1];
  // end;
  //
  // try
  // Synthesizer.Init;
  // if CheckBox4.Checked then begin
  // Synthesizer.JitterPeriodX := PeriodX;
  // Synthesizer.JitterPeriodY := PeriodY;
  // end
  // else begin
  // Synthesizer.JitterPeriodX := 0;
  // Synthesizer.JitterPeriodY := 0;
  // end;
  // Memo1.Lines.Add('Start synthesis');
  // dir := PWideChar(AppDir+'\'+tsAnalyzer.Name);
  // SetCurrentDirectory(dir);
  // t:=StartPrecisionTimer;
  // while not Synthesizer.Process do begin
  // Sleep(1);
  // GLCube1.Turn(1);
  // Application.ProcessMessages;
  // end;
  //
  // finally
  // exp:=StopPrecisionTimer(t)*1000;
  // Memo1.Lines.Add('End synthesis');
  // Memo1.Lines.Add(Format('Expanded time %.2f msec', [exp]));
  // // Autosave
  // if CheckBox1.Checked then begin
  // bmp := TBitmap.Create;
  // dir := PWideChar(AppDir+'\'+tsAnalyzer.Name);
  // CreateDirectory(dir, @attr);
  // SetCurrentDirectory(dir);
  // for n := 1 to Synthesizer.NumLevels - 1 do begin
  // bmp32 := Synthesizer.SynthesisResult(n);
  // bmp32.AssignToBitmap(bmp);
  // bmp.SaveToFile(Format('%s synth%d.bmp', [tsAnalyzer.Name, n]));
  // bmp32.Free;
  // bmp32 := Synthesizer.ResultPatches(n);
  // bmp32.AssignToBitmap(bmp);
  // bmp.SaveToFile(Format('%s coords%d.bmp', [tsAnalyzer.Name, n]));
  // bmp32.Free;
  // end;
  // bmp.Free;
  // end;
  // with GLMater.LibMaterialByName('CPU synthesis result') do begin
  // bmp32 := Synthesizer.SynthesisResult(Synthesizer.NumLevels-1);
  // Material.Texture.Image.Assign(bmp32);
  // bmp32.Free;
  // end;
  // with GLMater.LibMaterialByName('CPU result patches') do begin
  // bmp32 := Synthesizer.ResultPatches(Synthesizer.NumLevels-1);
  // Material.Texture.Image.Assign(bmp32);
  // bmp32.Free;
  // end;
  //
  // ComboBox1.ItemIndex := 1;
  // ComboBox1Change(Sender);
  //
  // end;

  Button1.Enabled := true;
  Button2.Enabled := true;
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
end;

// Toroidal control
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

// Texture lookup choise
procedure TMainForm.ComboBox1Change(Sender: TObject);
begin
  // case ComboBox1.ItemIndex of
  // 0: GLCube1.Material.LibMaterialName:='exemplar';
  // 1: GLCube1.Material.LibMaterialName:='CPU synthesis result';
  // 2: GLCube1.Material.LibMaterialName:='CPU result patches';
  // 3:
  // begin
  // GLCube1.Material.LibMaterialName:='GPU synthesis result';
  // GPUSynthesizer.GiveAsPatchesMap := false;
  // end;
  // 4:
  // begin
  // GLCube1.Material.LibMaterialName:='GPU result patches';
  // GPUSynthesizer.GiveAsPatchesMap := true;
  // end;
  // end;
end;

// Pass number choise
procedure TMainForm.ComboBox2Change(Sender: TObject);
begin
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

// Threads number correct input
procedure TMainForm.SpinEdit2Change(Sender: TObject);
begin
  if TSpinEdit(Sender).Value > 0 then
  begin
    TSpinEdit(Sender).Color := clWindow;
    ThreadsNumber[TSpinEdit(Sender).Tag] := TSpinEdit(Sender).Value;
  end
  else
    TSpinEdit(Sender).Color := clRed;
end;

// Jitter control
procedure TMainForm.TrackBar1Change(Sender: TObject);
var
  L: integer;
begin
  // L := TTrackBar(Sender).Tag;
  // if L < GPUSynthesizer.NumLevels then begin
  // GPUSynthesizer.JitterControl[L] := TTrackBar(Sender).Position /
  // TTrackBar(Sender).Max;
  // end;
end;

// Coherence control
procedure TMainForm.TrackBar9Change(Sender: TObject);
begin
  // GPUSynthesizer.CoherenceControl := TTrackBar(Sender).Position /
  // TTrackBar(Sender).Max;
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
    ComboBox1.ItemIndex := 0;
    ComboBox1Change(Sender);
  end;
end;

// Init shader synthesizer when at its page
procedure TMainForm.PageControl1Change(Sender: TObject);
begin
  { if tsAnalyzer.Done and (PageControl1.ActivePageIndex = 2) then begin
    if ComboBox1.ItemIndex < 3 then begin
    ComboBox1.ItemIndex := 3;
    ComboBox1Change(Sender);
    end;
    with GPUSynthesizer do begin
    Coherence := tsKappa;
    Width := TexWidth;
    Height := TexHeight;
    JitterStrength := Jitter;
    end;
    end; }
end;

procedure TMainForm.GLCadencerProgress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  L: integer;
begin
  { KeyDeltaTime := KeyDeltaTime + deltaTime;
    if KeyDeltaTime > 0.1 then begin
    KeyDeltaTime := 0;
    // this for keydowning, but all doing by buutons
    end;
    if (PageControl1.TabIndex = 2) and AutoJitter then begin
    // adapt jitter strength per level - arbitrary, ideally should be per-level
    // user control. Overall it is often more desirable to add strong jitter at
    // coarser levels and let synthesis recover at finer resolution levels.
    for L := 0 to GPUSynthesizer.NumLevels - 1 do begin
    if L > GPUSynthesizer.NumLevels - 4 then
    GPUSynthesizer.JitterControl[L] := 0
    else GPUSynthesizer.JitterControl[L] := 1 - (L + 1) /
    tsAnalyzer.Stack.Count;
    end;
    // is not bidlocoding, is very right disigion
    with GPUSynthesizer do begin
    TrackBar1.Position := Round(TrackBar1.Max * JitterControl[0]);
    TrackBar2.Position := Round(TrackBar2.Max * JitterControl[1]);
    TrackBar3.Position := Round(TrackBar3.Max * JitterControl[2]);
    TrackBar4.Position := Round(TrackBar4.Max * JitterControl[3]);
    TrackBar5.Position := Round(TrackBar5.Max * JitterControl[4]);
    TrackBar6.Position := Round(TrackBar6.Max * JitterControl[5]);
    TrackBar7.Position := Round(TrackBar7.Max * JitterControl[6]);
    TrackBar8.Position := Round(TrackBar8.Max * JitterControl[7]);
    end;
    AutoJitter := false;
    end;
  }
end;

procedure TMainForm.GLViewer1CanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  Proj := TMatrix.PerspectiveMatrix(60, NewWidth / NewHeight, 0.1, 100);
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

  ViewShader := TGLSLShaderProgram.Create;

  ViewShader.AttachShaderFromFile(stVertex, path + 'ScreenQuadShader.Vert');
  ViewShader.AttachShaderFromFile(stFragment, path + 'ScreenQuadShader.Frag');

  ViewShader.LinkShader;
  if ViewShader.Error then
  begin
    showmessage(ViewShader.Log);
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

  SQ_VO := CreatePlane(2, 2);
  SQ_Drawer := TGLVertexObject.CreateFrom(SQ_VO);
  SQ_Drawer.Shader := ViewShader;
end;

procedure TMainForm.GLViewer1Render(Sender: TObject);
var
  ratio: TVector;
begin
  glDisable(GL_CULL_FACE);
  glDisable(GL_DEPTH_TEST);

  ratio := TVector.Make(GLViewer1.Width / Image1.Picture.Width,
    GLViewer1.Height / Image1.Picture.Height);
  if ratio.X < ratio.Y then
    ratio := TVector.Make(1, ratio.X / ratio.Y)
  else
    ratio := TVector.Make(ratio.Y / ratio.X, 1);

  GLViewer1.Context.ClearDevice;

  ViewShader.Apply;
  ViewShader.SetUniform('Ration', ratio.Vec2);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, ExemplarTextureId);
  SQ_Drawer.RenderVO;
end;

end.
