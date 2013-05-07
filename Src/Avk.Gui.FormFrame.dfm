inherited FormFrame: TFormFrame
  Height = 326
  ExplicitHeight = 326
  inherited TopLabel: TcxLabel
    Style.IsFontAssigned = True
    AnchorX = 232
    AnchorY = 20
  end
  inherited ClientPanel: TPanel
    Height = 238
    ExplicitHeight = 238
    inherited ParamsScrollBox: TScrollBox
      Height = 57
      Align = alTop
      ExplicitHeight = 57
    end
    object FramesScrollBox: TScrollBox
      Left = 0
      Top = 57
      Width = 463
      Height = 181
      Align = alClient
      BorderStyle = bsNone
      TabOrder = 1
      ExplicitWidth = 412
    end
  end
  inherited BarManager: TdxBarManager
    DockControlHeights = (
      0
      0
      49
      0)
  end
end
