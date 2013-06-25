inherited BlockFrame: TBlockFrame
  Width = 463
  Height = 254
  Constraints.MinHeight = 200
  Constraints.MinWidth = 200
  ExplicitWidth = 463
  ExplicitHeight = 254
  inherited TopLabel: TcxLabel
    Style.IsFontAssigned = True
    ExplicitWidth = 463
    Width = 463
    AnchorX = 232
    AnchorY = 20
  end
  inherited ClientPanel: TPanel
    Top = 90
    Width = 463
    Height = 164
    ExplicitTop = 90
    ExplicitWidth = 463
    ExplicitHeight = 164
    object ParamsScrollBox: TScrollBox
      Left = 0
      Top = 0
      Width = 463
      Height = 164
      Align = alClient
      BorderStyle = bsNone
      TabOrder = 0
    end
  end
  object BarManager: TdxBarManager
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    CanCustomize = False
    Categories.Strings = (
      'Default'
      #1052#1077#1085#1102
      #1042#1080#1076
      #1044#1077#1081#1089#1090#1074#1080#1103)
    Categories.ItemsVisibles = (
      2
      2
      2
      2)
    Categories.Visibles = (
      True
      True
      True
      True)
    ImageOptions.Images = CustomMainDataModule.Images
    PopupMenuLinks = <>
    UseSystemFont = True
    Left = 40
    Top = 8
    DockControlHeights = (
      0
      0
      51
      0)
    object BarManagerMenuBar: TdxBar
      AllowClose = False
      AllowCustomizing = False
      AllowQuickCustomizing = False
      Caption = 'Custom 1'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 0
      DockingStyle = dsTop
      FloatLeft = 422
      FloatTop = 0
      FloatClientWidth = 0
      FloatClientHeight = 0
      IsMainMenu = True
      ItemLinks = <
        item
          Visible = True
          ItemName = 'ViewBarSubItem'
        end
        item
          Visible = True
          ItemName = 'ActionsBarSubItem'
        end>
      MultiLine = True
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = True
    end
    object BarManagerToolBar: TdxBar
      AllowClose = False
      AllowCustomizing = False
      AllowQuickCustomizing = False
      Caption = 'Custom 2'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 25
      DockingStyle = dsTop
      FloatLeft = 422
      FloatTop = 0
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <>
      OneOnRow = True
      Row = 1
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object ViewBarSubItem: TdxBarSubItem
      Caption = #1042#1080#1076
      Category = 1
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'SaveViewBarButton'
        end>
    end
    object ActionsBarSubItem: TdxBarSubItem
      Caption = #1044#1077#1081#1089#1090#1074#1080#1103
      Category = 1
      Visible = ivAlways
      ItemLinks = <>
    end
    object SaveViewBarButton: TdxBarButton
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      Category = 2
      Hint = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      Visible = ivAlways
      OnClick = SaveViewBarButtonClick
    end
  end
end
