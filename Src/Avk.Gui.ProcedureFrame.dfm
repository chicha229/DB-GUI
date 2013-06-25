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
    Height = 231
    ExplicitWidth = 419
    ExplicitHeight = 231
    object TreeList: TcxDBTreeList [0]
      Left = 0
      Top = 100
      Width = 267
      Height = 131
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
      Width = 267
      Height = 131
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
    object VerticalGrid: TcxDBVerticalGrid
      Left = 275
      Top = 100
      Width = 144
      Height = 131
      Align = alRight
      OptionsView.CellAutoHeight = True
      OptionsView.ScrollBars = ssVertical
      OptionsView.PaintStyle = psDelphi
      OptionsBehavior.AllowChangeRecord = False
      OptionsData.CancelOnExit = False
      OptionsData.Editing = False
      OptionsData.Appending = False
      OptionsData.Deleting = False
      OptionsData.DeletingConfirmation = False
      OptionsData.Inserting = False
      Navigator.Buttons.CustomButtons = <>
      TabOrder = 4
      Visible = False
      DataController.DataSource = DataSource
      Version = 1
      object VerticalGridCategoryRow1: TcxCategoryRow
        ID = 0
        ParentID = -1
        Index = 0
        Version = 1
      end
    end
    object GridsSplitter: TcxSplitter
      Left = 267
      Top = 100
      Width = 8
      Height = 131
      HotZoneClassName = 'TcxSimpleStyle'
      AlignSplitter = salRight
      Control = VerticalGrid
      Visible = False
    end
  end
  inherited BarManager: TdxBarManager
    Categories.Strings = (
      'Default'
      #1052#1077#1085#1102
      #1042#1080#1076
      #1044#1077#1081#1089#1090#1074#1080#1103
      #1069#1082#1089#1087#1086#1088#1090)
    Categories.ItemsVisibles = (
      2
      2
      2
      2
      2)
    Categories.Visibles = (
      True
      True
      True
      True
      True)
    DockControlHeights = (
      0
      0
      51
      0)
    inherited BarManagerMenuBar: TdxBar
      ItemLinks = <
        item
          Visible = True
          ItemName = 'ViewBarSubItem'
        end
        item
          Visible = True
          ItemName = 'ActionsBarSubItem'
        end
        item
          Visible = True
          ItemName = 'ExportBarSubItem'
        end>
    end
    inherited BarManagerToolBar: TdxBar
      ItemLinks = <
        item
          Visible = True
          ItemName = 'RefreshDataBarButton'
        end>
    end
    object dxBarSeparator1: TdxBarSeparator [2]
      Caption = 'New Separator'
      Category = 0
      Hint = 'New Separator'
      Visible = ivAlways
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
          Visible = True
          ItemName = 'VerticalGridBarButton'
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
    object ExportBarSubItem: TdxBarSubItem [5]
      Caption = #1069#1082#1089#1087#1086#1088#1090
      Category = 1
      Visible = ivAlways
      ItemLinks = <
        item
          Visible = True
          ItemName = 'PrintPreviewBarButton'
        end
        item
          Visible = True
          ItemName = 'PrintBarButton'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'ExcelExportBarButton'
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
    object VerticalGridBarButton: TdxBarButton
      Caption = #1047#1072#1087#1080#1089#1100' '#1074#1077#1088#1090#1080#1082#1072#1083#1100#1085#1086
      Category = 2
      Hint = #1047#1072#1087#1080#1089#1100' '#1074#1077#1088#1090#1080#1082#1072#1083#1100#1085#1086
      Visible = ivAlways
      ButtonStyle = bsChecked
      OnClick = VerticalGridBarButtonClick
    end
    object RefreshDataBarButton: TdxBarButton
      Caption = #1054#1073#1085#1086#1074#1080#1090#1100
      Category = 3
      Hint = #1054#1073#1085#1086#1074#1080#1090#1100
      Visible = ivAlways
      ImageIndex = 7
      OnClick = RefreshDataBarButtonClick
    end
    object PrintPreviewBarButton: TdxBarButton
      Caption = #1055#1088#1077#1076#1074#1072#1088#1080#1090#1077#1083#1100#1085#1099#1081' '#1087#1088#1086#1089#1084#1086#1090#1088'...'
      Category = 4
      Hint = #1055#1088#1077#1076#1074#1072#1088#1080#1090#1077#1083#1100#1085#1099#1081' '#1087#1088#1086#1089#1084#1086#1090#1088
      Visible = ivAlways
      ImageIndex = 8
      OnClick = PrintPreviewBarButtonClick
    end
    object PrintBarButton: TdxBarButton
      Caption = #1055#1077#1095#1072#1090#1100'...'
      Category = 4
      Hint = #1055#1077#1095#1072#1090#1100
      Visible = ivAlways
      ImageIndex = 9
      OnClick = PrintBarButtonClick
    end
    object ExcelExportBarButton: TdxBarButton
      Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074' Excel'
      Category = 4
      Hint = #1069#1082#1089#1087#1086#1088#1090' '#1074' Excel'
      Visible = ivAlways
      ImageIndex = 33
      OnClick = ExcelExportBarButtonClick
    end
  end
  object DataSource: TDataSource
    DataSet = MemTable
    Left = 40
    Top = 144
  end
  object MemTable: TADMemTable
    AfterOpen = MemTableAfterOpen
    AfterPost = QueryAfterPost
    AfterScroll = MemTableAfterScroll
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired]
    UpdateOptions.CheckRequired = False
    Left = 8
    Top = 144
  end
  object cxGridPopupMenu1: TcxGridPopupMenu
    Grid = Grid
    PopupMenus = <>
    Left = 72
    Top = 8
  end
  object ExcelExportSaveDialog: TSaveDialog
    DefaultExt = '.xls'
    Filter = #1060#1072#1081#1083#1099' Microsoft Excel (*.xls)|*.xls'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 104
    Top = 8
  end
  object ComponentPrinter: TdxComponentPrinter
    CurrentLink = TreeListPrinterLink
    Version = 0
    Left = 136
    Top = 8
    object GridPrinterLink: TdxGridReportLink
      Active = True
      Component = Grid
      PrinterPage.DMPaper = 9
      PrinterPage.Footer = 6350
      PrinterPage.Header = 6350
      PrinterPage.Margins.Bottom = 12700
      PrinterPage.Margins.Left = 12700
      PrinterPage.Margins.Right = 12700
      PrinterPage.Margins.Top = 12700
      PrinterPage.PageFooter.Font.Charset = DEFAULT_CHARSET
      PrinterPage.PageFooter.Font.Color = clWindowText
      PrinterPage.PageFooter.Font.Height = -11
      PrinterPage.PageFooter.Font.Name = 'Tahoma'
      PrinterPage.PageFooter.Font.Style = []
      PrinterPage.PageHeader.Font.Charset = DEFAULT_CHARSET
      PrinterPage.PageHeader.Font.Color = clWindowText
      PrinterPage.PageHeader.Font.Height = -11
      PrinterPage.PageHeader.Font.Name = 'Tahoma'
      PrinterPage.PageHeader.Font.Style = []
      PrinterPage.PageSize.X = 215900
      PrinterPage.PageSize.Y = 279400
      PrinterPage._dxMeasurementUnits_ = 0
      PrinterPage._dxLastMU_ = 2
      ReportDocument.CreationDate = 41449.453649548610000000
      ReportTitle.Font.Charset = DEFAULT_CHARSET
      ReportTitle.Font.Color = clWindowText
      ReportTitle.Font.Height = -19
      ReportTitle.Font.Name = 'Times New Roman'
      ReportTitle.Font.Style = [fsBold]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Times New Roman'
      Font.Style = []
      OptionsCards.Shadow.Color = clBlack
      OptionsExpanding.ExpandGroupRows = True
      OptionsExpanding.ExpandMasterRows = True
      OptionsLevels.Unwrap = True
      OptionsOnEveryPage.Footers = False
      OptionsOnEveryPage.Caption = False
      OptionsOnEveryPage.FilterBar = False
      OptionsSize.AutoWidth = True
      OptionsView.Footers = False
      OptionsView.Caption = False
      OptionsView.ExpandButtons = False
      OptionsView.FilterBar = False
      BuiltInReportLink = True
    end
    object TreeListPrinterLink: TcxDBTreeListReportLink
      Component = TreeList
      PrinterPage.DMPaper = 1
      PrinterPage.Footer = 6350
      PrinterPage.Header = 6350
      PrinterPage.Margins.Bottom = 12700
      PrinterPage.Margins.Left = 12700
      PrinterPage.Margins.Right = 12700
      PrinterPage.Margins.Top = 12700
      PrinterPage.PageSize.X = 215900
      PrinterPage.PageSize.Y = 279400
      PrinterPage._dxMeasurementUnits_ = 0
      PrinterPage._dxLastMU_ = 2
      BuiltInReportLink = True
    end
  end
end
