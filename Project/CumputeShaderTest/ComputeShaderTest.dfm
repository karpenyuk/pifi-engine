object Form3: TForm3
  Left = 514
  Top = 225
  Caption = 'Form3'
  ClientHeight = 456
  ClientWidth = 746
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLViewer1: TGLViewer
    Left = 0
    Top = 41
    Width = 746
    Height = 415
    OnRender = GLViewer1Render
    OnContextReady = GLViewer1ContextReady
    Align = alClient
    OnMouseDown = GLViewer1MouseDown
    OnMouseMove = GLViewer1MouseMove
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 746
    Height = 41
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 644
      Top = 6
      Width = 46
      Height = 13
      Caption = 'MVPTime:'
      Color = clBtnFace
      ParentColor = False
    end
    object Label3: TLabel
      Left = 639
      Top = 22
      Width = 21
      Height = 13
      Caption = 'NAN'
      Color = clBtnFace
      ParentColor = False
    end
    object RadioButton1: TRadioButton
      Left = 8
      Top = 11
      Width = 49
      Height = 24
      Caption = 'CPU'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButton2: TRadioButton
      Left = 63
      Top = 11
      Width = 42
      Height = 24
      Caption = 'GPU'
      TabOrder = 1
      OnClick = RadioButton2Click
    end
    object rgUsage: TRadioGroup
      Left = 128
      Top = 0
      Width = 505
      Height = 35
      Caption = 'Transform usage:'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'SSBO'
        'UBO'
        'Readback')
      TabOrder = 2
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 32
    Top = 24
  end
end
