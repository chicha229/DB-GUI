object BaseFrame: TBaseFrame
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object TopLabel: TcxLabel
    Left = 0
    Top = 0
    Align = alTop
    AutoSize = False
    ParentColor = False
    ParentFont = False
    Style.Color = clMoneyGreen
    Style.Font.Charset = DEFAULT_CHARSET
    Style.Font.Color = clWindowText
    Style.Font.Height = -11
    Style.Font.Name = 'Tahoma'
    Style.Font.Style = [fsBold]
    Style.IsFontAssigned = True
    Properties.Alignment.Horz = taCenter
    Properties.Alignment.Vert = taVCenter
    Height = 39
    Width = 320
    AnchorX = 160
    AnchorY = 20
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 39
    Width = 320
    Height = 201
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
  object FormErrors: TFormErrors
    Items = <>
    Left = 8
    Top = 8
  end
end
