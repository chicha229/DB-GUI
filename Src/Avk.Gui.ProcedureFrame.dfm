inherited ProcedureFrame: TProcedureFrame
  Width = 419
  Height = 321
  ExplicitWidth = 419
  ExplicitHeight = 321
  inherited TopLabel: TcxLabel
    Style.IsFontAssigned = True
    ExplicitWidth = 419
    Width = 419
    AnchorX = 210
    AnchorY = 20
  end
  inherited ClientPanel: TPanel
    Width = 419
    Height = 233
    ExplicitWidth = 419
    ExplicitHeight = 233
    object TreeList: TcxDBTreeList [0]
      Left = 0
      Top = 100
      Width = 419
      Height = 133
      Hint = ''
      Align = alClient
      Bands = <
        item
        end>
      Navigator.Buttons.CustomButtons = <>
      OptionsBehavior.IncSearch = True
      OptionsData.Editing = False
      OptionsData.Deleting = False
      RootValue = -1
      TabOrder = 2
    end
    object Grid: TcxGrid [1]
      Left = 0
      Top = 100
      Width = 419
      Height = 133
      Align = alClient
      TabOrder = 1
      object GridTableView: TcxGridDBBandedTableView
        Navigator.Buttons.CustomButtons = <>
        DataController.DataSource = DataSource
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsBehavior.IncSearch = True
        OptionsData.Deleting = False
        OptionsData.Editing = False
        OptionsData.Inserting = False
        OptionsView.GroupByBox = False
        Bands = <
          item
          end>
      end
      object GridLevel: TcxGridLevel
        GridView = GridTableView
      end
    end
    inherited ParamsScrollBox: TScrollBox
      Width = 419
      Height = 59
      Align = alTop
      ExplicitWidth = 419
      ExplicitHeight = 59
    end
    object SearchPanel: TPanel
      Left = 0
      Top = 59
      Width = 419
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
      DesignSize = (
        419
        41)
      object SearchEdit: TcxButtonEdit
        Left = 45
        Top = 13
        Anchors = [akLeft, akTop, akRight]
        Properties.Buttons = <
          item
            Kind = bkEllipsis
          end
          item
            ImageIndex = 6
            Kind = bkGlyph
          end>
        Properties.Images = CustomMainDataModule.Images
        Properties.OnButtonClick = SearchEditPropertiesButtonClick
        Properties.OnChange = SearchEditPropertiesChange
        TabOrder = 0
        Width = 366
      end
      object cxLabel1: TcxLabel
        Left = 7
        Top = 15
        Caption = #1055#1086#1080#1089#1082
      end
    end
  end
  inherited BarManager: TdxBarManager
    DockControlHeights = (
      0
      0
      49
      0)
    inherited BarManagerToolBar: TdxBar
      ItemLinks = <
        item
          Visible = True
          ItemName = 'RefreshDataBarButton'
        end>
    end
    inherited ViewBarSubItem: TdxBarSubItem
      ItemLinks = <
        item
          Visible = True
          ItemName = 'GridAutoWidthBarButton'
        end
        item
          Visible = True
          ItemName = 'AllRecordsGridModeButton'
        end
        item
          Visible = True
          ItemName = 'GridSummaryRowBarButton'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'SearchFieldsBarButton'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'FullCollapseBarButton'
        end
        item
          Visible = True
          ItemName = 'FullExpandBarButton'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'SaveViewBarButton'
        end>
    end
    inherited ActionsBarSubItem: TdxBarSubItem
      ItemLinks = <
        item
          Visible = True
          ItemName = 'RefreshDataBarButton'
        end>
    end
    object FullExpandBarButton: TdxBarButton
      Caption = #1056#1072#1079#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1077
      Category = 2
      Hint = #1056#1072#1079#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1077
      Visible = ivAlways
      ImageIndex = 2
      OnClick = FullExpandBarButtonClick
    end
    object FullCollapseBarButton: TdxBarButton
      Caption = #1057#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1077
      Category = 2
      Hint = #1057#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1077
      Visible = ivAlways
      ImageIndex = 1
      OnClick = FullCollapseBarButtonClick
    end
    object GridAutoWidthBarButton: TdxBarButton
      Caption = #1050#1086#1083#1086#1085#1082#1080' '#1074#1086' '#1074#1089#1102' '#1090#1072#1073#1083#1080#1094#1091
      Category = 2
      Hint = #1050#1086#1083#1086#1085#1082#1080' '#1074#1086' '#1074#1089#1102' '#1090#1072#1073#1083#1080#1094#1091
      Visible = ivAlways
      ButtonStyle = bsChecked
      Down = True
      ImageIndex = 21
      OnClick = GridAutoWidthBarButtonClick
    end
    object AllRecordsGridModeButton: TdxBarButton
      Caption = #1056#1077#1078#1080#1084' '#1087#1086#1083#1085#1086#1081' '#1079#1072#1075#1088#1091#1079#1082#1080' '#1076#1072#1085#1085#1099#1093
      Category = 2
      Hint = #1056#1077#1078#1080#1084' '#1087#1086#1083#1085#1086#1081' '#1079#1072#1075#1088#1091#1079#1082#1080' '#1076#1072#1085#1085#1099#1093
      Visible = ivAlways
      ButtonStyle = bsChecked
      OnClick = AllRecordsGridModeButtonClick
    end
    object GridSummaryRowBarButton: TdxBarButton
      Caption = #1057#1090#1088#1086#1082#1072' '#1089#1091#1084#1084#1080#1088#1086#1074#1072#1085#1080#1103
      Category = 2
      Hint = #1057#1090#1088#1086#1082#1072' '#1089#1091#1084#1084#1080#1088#1086#1074#1072#1085#1080#1103
      Visible = ivAlways
      ButtonStyle = bsChecked
      OnClick = GridSummaryRowBarButtonClick
    end
    object SearchFieldsBarButton: TdxBarButton
      Caption = #1055#1086#1083#1103' '#1076#1083#1103' '#1087#1086#1080#1089#1082#1072'...'
      Category = 2
      Hint = #1055#1086#1083#1103' '#1076#1083#1103' '#1087#1086#1080#1089#1082#1072
      Visible = ivAlways
      OnClick = SearchFieldsBarButtonClick
    end
    object RefreshDataBarButton: TdxBarButton
      Caption = #1054#1073#1085#1086#1074#1080#1090#1100
      Category = 3
      Hint = #1054#1073#1085#1086#1074#1080#1090#1100
      Visible = ivAlways
      ImageIndex = 7
      OnClick = RefreshDataBarButtonClick
    end
  end
  object Query: TADQuery
    AfterOpen = QueryAfterOpen
    AfterPost = QueryAfterPost
    AfterScroll = QueryAfterPost
    Left = 8
    Top = 144
  end
  object DataSource: TDataSource
    DataSet = Query
    Left = 40
    Top = 144
  end
end
