object Form5: TForm5
  Left = 0
  Top = 0
  Caption = 'DAE Import'
  ClientHeight = 480
  ClientWidth = 649
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
    Left = 0
    Top = 0
    Width = 649
    Height = 480
    OnRender = GLViewer1Render
    OnContextReady = GLViewer1ContextReady
    Context.DepthBits = 24
    Context.StencilBits = 8
    Context.AALevel = 0
    Align = alClient
    OnCanResize = GLViewer1CanResize
    OnMouseDown = GLViewer1MouseDown
    OnMouseMove = GLViewer1MouseMove
    ExplicitLeft = 96
    ExplicitTop = 80
    ExplicitWidth = 225
    ExplicitHeight = 129
  end
end
