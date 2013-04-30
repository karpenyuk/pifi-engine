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
  PixelsPerInch = 96
  TextHeight = 13
  object GLViewer1: TGLViewer
    Left = 0
    Top = 25
    Width = 746
    Height = 431
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
    Height = 25
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 256
      Top = 8
      Width = 46
      Height = 13
      Caption = 'MVPTime:'
      Color = clBtnFace
      ParentColor = False
    end
    object Label2: TLabel
      Left = 312
      Top = 8
      Width = 21
      Height = 13
      Caption = 'NAN'
      Color = clBtnFace
      ParentColor = False
    end
    object Label3: TLabel
      Left = 408
      Top = 6
      Width = 21
      Height = 13
      Caption = 'NAN'
      Color = clBtnFace
      ParentColor = False
    end
    object RadioButton1: TRadioButton
      Left = 8
      Top = 5
      Width = 120
      Height = 24
      Caption = 'Standart MVP'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButton2: TRadioButton
      Left = 125
      Top = 5
      Width = 138
      Height = 24
      Caption = 'Compute Shader'
      TabOrder = 1
      OnClick = RadioButton2Click
    end
    object CheckBox1: TCheckBox
      Left = 568
      Top = -2
      Width = 24
      Height = 24
      TabOrder = 2
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 32
    Top = 24
  end
end
