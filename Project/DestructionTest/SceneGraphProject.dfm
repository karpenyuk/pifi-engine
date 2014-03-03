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
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  PixelsPerInch = 96
  TextHeight = 13
  object GLViewer1: TGLViewer
    Left = 0
    Top = 0
    Width = 536
    Height = 425
    OnRender = GLViewer1Render
    OnContextReady = GLViewer1ContextReady
    Align = alClient
    OnCanResize = GLViewer1CanResize
    OnMouseDown = GLViewer1MouseDown
    OnMouseMove = GLViewer1MouseMove
  end
  object TreeView1: TTreeView
    Left = 536
    Top = 0
    Width = 197
    Height = 425
    Align = alRight
    Indent = 19
    PopupMenu = AddMesh
    TabOrder = 1
    Items.NodeData = {
      0302000000300000000000000000000000FFFFFFFFFFFFFFFF00000000000000
      000400000001095200650073006F007500720063006500730030000000000000
      0000000000FFFFFFFFFFFFFFFF00000000000000000000000001094D00610074
      0065007200690061006C0073002C0000000000000000000000FFFFFFFFFFFFFF
      FF000000000000000000000000010753006800610064006500720073002A0000
      000000000000000000FFFFFFFFFFFFFFFF00000000000000000000000001064D
      0065007300680065007300340000000000000000000000FFFFFFFFFFFFFFFF00
      0000000000000000000000010B4D006500730068004F0062006A006500630074
      007300340000000000000000000000FFFFFFFFFFFFFFFF000000000000000002
      000000010B5300630065006E0065002000470072006100700068002A00000000
      00000000000000FFFFFFFFFFFFFFFF00000000000000000000000001064C0069
      0067006800740073002C0000000000000000000000FFFFFFFFFFFFFFFF000000
      00000000000000000001074F0062006A006500630074007300}
    ExplicitLeft = 542
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 8
    Top = 8
  end
  object AddMesh: TPopupMenu
    Left = 656
    Top = 8
    object AddMesh1: TMenuItem
      Caption = 'AddMesh'
      OnClick = AddMesh1Click
    end
  end
end
