object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Decal Tesselation'
  ClientHeight = 479
  ClientWidth = 639
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GLViewer1: TGLViewer
    Left = 0
    Top = 0
    Width = 639
    Height = 479
    OnRender = GLViewer1Render
    OnContextReady = GLViewer1ContextReady
    Context.DepthBits = 24
    Context.StencilBits = 8
    Context.AALevel = 0
    Align = alClient
    OnCanResize = GLViewer1CanResize
    OnMouseDown = GLViewer1MouseDown
    OnMouseMove = GLViewer1MouseMove
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 24
    Top = 24
  end
end
