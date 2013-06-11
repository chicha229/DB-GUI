inherited SelectSearchFieldsForm: TSelectSearchFieldsForm
  Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1087#1086#1083#1103' '#1076#1083#1103' '#1087#1086#1080#1089#1082#1072
  ExplicitWidth = 320
  PixelsPerInch = 96
  TextHeight = 13
  inherited BottomPanel: TPanel
    inherited cxBottomLineGroupBox: TcxGroupBox
      Visible = False
    end
  end
  inherited ClientPanel: TPanel
    inherited ScrollBox: TScrollBox
      object FieldsGrid: TcxGrid
        Left = 0
        Top = 0
        Width = 442
        Height = 235
        Align = alClient
        TabOrder = 0
        object FieldsGridTableView: TcxGridTableView
          Navigator.Buttons.CustomButtons = <>
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
          OptionsBehavior.AlwaysShowEditor = True
          OptionsData.Deleting = False
          OptionsData.Inserting = False
          OptionsView.ColumnAutoWidth = True
          OptionsView.GroupByBox = False
          object GridFieldNameColumn: TcxGridColumn
            Visible = False
          end
          object GridFieldColumn: TcxGridColumn
            Caption = #1055#1086#1083#1077
            HeaderAlignmentHorz = taCenter
            Options.Editing = False
            Width = 339
          end
          object GridSearchColumn: TcxGridColumn
            Caption = #1055#1086#1080#1089#1082
            DataBinding.ValueType = 'Boolean'
            PropertiesClassName = 'TcxCheckBoxProperties'
            HeaderAlignmentHorz = taCenter
            Width = 101
          end
        end
        object FieldsGridLevel1: TcxGridLevel
          GridView = FieldsGridTableView
        end
      end
    end
  end
end
