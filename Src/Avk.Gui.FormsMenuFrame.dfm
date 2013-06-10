inherited FormsMenuFrame: TFormsMenuFrame
  inherited TopLabel: TcxLabel
    Style.IsFontAssigned = True
    AnchorX = 210
    AnchorY = 20
  end
  inherited ClientPanel: TPanel
    inherited TreeList: TcxDBTreeList
      OptionsView.ColumnAutoWidth = True
      OnDblClick = TreeListDblClick
    end
    inherited SearchPanel: TPanel
      inherited SearchEdit: TcxButtonEdit
        ExplicitHeight = 22
      end
    end
  end
  inherited BarManager: TdxBarManager
    DockControlHeights = (
      0
      0
      51
      0)
  end
end
