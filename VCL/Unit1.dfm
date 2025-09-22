object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'ResampleTestVCL'
  ClientHeight = 800
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  DesignSize = (
    800
    800)
  TextHeight = 15
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 800
    Height = 800
    Align = alClient
    ExplicitLeft = 112
    ExplicitTop = 24
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object Button1: TButton
    Left = 717
    Top = 767
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Mitchell'
    TabOrder = 0
    OnClick = Button1Click
  end
  object OpenDialog1: TOpenDialog
    Left = 440
    Top = 184
  end
end
