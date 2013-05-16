object Form1: TForm1
  Left = 0
  Height = 479
  Top = 24
  Width = 639
  Caption = 'Decal Tesselation'
  ClientHeight = 479
  ClientWidth = 639
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  OnClose = FormClose
  OnCreate = FormCreate
  LCLVersion = '1.1'
  object GLViewer1: TGLViewer
    Left = 0
    Height = 479
    Top = 0
    Width = 639
    OnRender = GLViewer1Render
    OnContextReady = GLViewer1ContextReady
    Align = alClient
    OnCanResize = GLViewer1CanResize
    OnMouseDown = GLViewer1MouseDown
    OnMouseMove = GLViewer1MouseMove
  end
end
