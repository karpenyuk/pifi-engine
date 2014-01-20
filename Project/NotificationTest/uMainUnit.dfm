object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Notify test'
  ClientHeight = 542
  ClientWidth = 592
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object GLViewer1: TGLViewer
    Left = 8
    Top = 8
    Width = 577
    Height = 489
    OnRender = GLViewer1Render
    onContextReady = GLViewer1ContextReady
  end
  object Button1: TButton
    Left = 8
    Top = 512
    Width = 75
    Height = 25
    Caption = 'Try'
    TabOrder = 1
  end
end
