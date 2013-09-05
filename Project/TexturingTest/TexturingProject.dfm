object Form2: TForm2
  Left = 1034
  Top = 175
  Caption = 'Form2'
  ClientHeight = 425
  ClientWidth = 733
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object GLViewer1: TGLViewer
    Left = 0
    Top = 0
    Width = 733
    Height = 425
    OnRender = GLViewer1Render
    onContextReady = GLViewer1ContextReady
    Align = alClient
    OnMouseDown = GLViewer1MouseDown
    OnMouseMove = GLViewer1MouseMove
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 8
    Top = 8
  end
end
