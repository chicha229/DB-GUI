unit Avk.Gui.ProcedureFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Avk.Gui.BaseFrame, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxSkinsCore, cxStyles,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxNavigator, Data.DB, cxDBData,
  cxGridLevel, cxClasses, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGrid, cxScrollBox, cxLabel,

  Avk.Gui.BlockFrame, Vcl.ExtCtrls, cxGroupBox, cxPCdxBarPopupMenu, cxPC,
  cxTextEdit, uFormErrors, uADStanIntf, uADStanOption, uADStanParam,
  uADStanError, uADDatSManager, uADPhysIntf, uADDAptIntf, uADStanAsync,
  uADDAptManager, uADCompDataSet, uADCompClient,

  Avk.Gui.Descriptions, cxGridBandedTableView, cxGridDBBandedTableView,
  dxBar, cxTL, cxTLdxBarBuiltInMenu, cxInplaceContainer, cxTLData, cxDBTL,
  cxMaskEdit, cxCheckBox, cxButtonEdit, dxSkinsDefaultPainters,
  dxSkinsdxBarPainter, dxSkinscxPCPainter, Avk.Gui.Connection;

type
  TProcedureFrame = class (TBlockFrame)
    GridLevel: TcxGridLevel;
    Grid: TcxGrid;
    DataSource: TDataSource;
    GridTableView: TcxGridDBBandedTableView;
    TreeList: TcxDBTreeList;
    RefreshDataBarButton: TdxBarButton;
    FullExpandBarButton: TdxBarButton;
    FullCollapseBarButton: TdxBarButton;
    GridAutoWidthBarButton: TdxBarButton;
    AllRecordsGridModeButton: TdxBarButton;
    GridSummaryRowBarButton: TdxBarButton;
    SearchFieldsBarButton: TdxBarButton;
    SearchPanel: TPanel;
    SearchEdit: TcxButtonEdit;
    cxLabel1: TcxLabel;
    MemTable: TADMemTable;
    procedure FullExpandBarButtonClick(Sender: TObject);
    procedure FullCollapseBarButtonClick(Sender: TObject);
    procedure RefreshDataBarButtonClick(Sender: TObject);
    procedure GridAutoWidthBarButtonClick(Sender: TObject);
    procedure QueryAfterPost(DataSet: TDataSet);
    procedure AllRecordsGridModeButtonClick(Sender: TObject);
    procedure GridSummaryRowBarButtonClick(Sender: TObject);
    procedure SearchFieldsBarButtonClick(Sender: TObject);
    procedure SearchEditPropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure SearchEditPropertiesChange(Sender: TObject);
    procedure QueryAfterOpen(DataSet: TDataSet);
  private
    { Private declarations }
    FRecordsModified: boolean;
    FParamsModified: boolean;

    function GetProcedureDescription: TProcedureDescription;
    procedure ApplyDataSetView;
    function OpenQuery: boolean;
    procedure RefreshData;
    procedure ApplyGridViewSettings;
    procedure SetKeyFieldValues(const Value: variant);
    function GetKeyFieldValues: variant;
    procedure PostFieldsToParamValues;
    procedure SetGridMode(const Value: boolean);
    function GetGridMode: boolean;
    procedure ApplySummaryRow;
    procedure SaveSearchFields;
    procedure ApplySearchVisible;
    function RefreshOneRecord(PV: TParamValues): boolean;
  public
    SearchFields: string;

    { Public declarations }
    procedure EditorsToParamValues; override;
    procedure OnChangeParamValues(Sender: TBlockFrame; AChangeId: Int64); override;

    procedure FillFields;
    procedure Build(AParent: TWinControl); override;
    function Open: boolean; override;
    function Save: boolean; override;
    function Modified: boolean; override;
    function ConfirmCancel: boolean; override;

    procedure SetButtonProperties(AButton: TdxBarItem); override;
    function CallDblClickActionInternal: boolean; override;

    procedure OnAfterAction(A: TBlockAction; PV: TParamValues); override;
    procedure AfterRefresh; virtual;

    procedure SaveFrameSettings; override;
    procedure LoadFrameSettings; override;

    property ProcedureDescription: TProcedureDescription read GetProcedureDescription;

    property KeyFieldValues: variant read GetKeyFieldValues write SetKeyFieldValues;
    property GridMode: boolean read GetGridMode write SetGridMode;
  end;

implementation

{$R *.dfm}

uses
  Generics.Collections,
  JclSysUtils,
  Avk.Gui.CustomMainDM, Avk.Gui.SearchFieldsSelect,
  AVK.Core.Utils, AVK.DX.LookupFilter;

const
  cGridAutoWidth = 'GRID_AUTO_WIDTH';
  cAllRecordsGridMode = 'GRID_MODE';
  cGridSummaryRow = 'GRID_SUMMARY_ROW';
  cSearchFieldsSettingsName = 'GRID_SEARCH_FIELDS';
  cSummaryFooterFormat = '### ### ### ##0.00';

{ TProcedureFrame }

procedure TProcedureFrame.FullCollapseBarButtonClick(Sender: TObject);
begin
  inherited;
  if ProcedureDescription.IsTree then
    TreeList.FullCollapse;
end;

procedure TProcedureFrame.FullExpandBarButtonClick(Sender: TObject);
begin
  inherited;
  if ProcedureDescription.IsTree then
    TreeList.FullExpand;
end;

procedure TProcedureFrame.EditorsToParamValues;
var
  P: TParamDescription;
begin
  inherited;
  if MemTable.Active then
    for P in BlockDescription.Params.Values do
    begin
      if MemTable.Active then
      begin
        if P.ParamDirection = pdField then
          ParamValues.AddOrSetValue(P.Name, MemTable.FieldValues[P.Name])
      end
      else if P.ParamDirection in [pdOut, pdInOut] then
        ParamValues.AddOrSetValue(P.Name, MemTable.ParamByName(P.Name).Value);
    end;
end;

procedure TProcedureFrame.FillFields;
var
  i: integer;
  P: TParamDescription;
  GroupsFound: boolean;
  C: TcxGridDBBandedColumn;
  TLC: TcxDBTreeListColumn;
  LastBandName: string;
  LastBand: TcxGridBand;
  LastTLBand: TcxTreeListBand;
begin
  GroupsFound := false;
  for P in ProcedureDescription.Params.Values do
    if P.Group <> '' then
      GroupsFound := true;

  if ProcedureDescription.IsTree then
  begin
    // заполняем поля дерева
    TreeList.DataController.DataSource := DataSource;
    GridTableView.DataController.DataSource := nil;
    TreeList.DataController.CreateAllItems;
    if GroupsFound then
    begin
      LastTLBand := nil;
      LastBandName := '';
      for P in ProcedureDescription.SortedParams do
        if (P.ParamDirection = pdField) and ((P.Group <> '')) then
        begin
          TLC := TreeList.ColumnByName(P.Name) as TcxDBTreeListColumn;
          if LastBandName <> P.Group then
            LastTLBand := TreeList.Bands.Add;
          TLC.Position.BandIndex := LastTLBand.Index;
        end;
    end;
    TreeList.DataController.KeyField := ProcedureDescription.KeyFieldNames;
    TreeList.DataController.ParentField := ProcedureDescription.ParentFieldNames;
    for i := 0 to TreeList.ColumnCount - 1 do
    begin
      TreeList.Columns[i].Caption.AlignHorz := taCenter;
      TreeList.Columns[i].Styles.Header := CustomMainDM.GridHeaderStyle;
    end;
    TreeList.ApplyBestFit;
    TreeList.BringToFront;
  end
  else
  begin
    // заполняем поля грида
    TreeList.DataController.DataSource := nil;
    GridTableView.DataController.DataSource := DataSource;
    GridTableView.DataController.CreateAllItems();
    if GroupsFound then
    begin
      LastBand := nil;
      LastBandName := '';
      for P in ProcedureDescription.Params.Values do
        if (P.ParamDirection = pdField) and (P.Group <> '') then
        begin
          C := GridTableView.FindItemByName(P.Name) as TcxGridDBBandedColumn;
          if LastBandName <> P.Group then
            LastBand := GridTableView.Bands.Add;
          C.Position.BandIndex := LastBand.Index;
        end;
    end;
    for i := 0 to GridTableView.ColumnCount - 1 do
    begin
      C := GridTableView.Columns[i];
      C.HeaderAlignmentHorz := taCenter;
      C.Styles.Header := CustomMainDM.GridHeaderStyle;
      if ProcedureDescription.Params.ContainsKey(C.DataBinding.FieldName) then
      begin
        P := ProcedureDescription.ParamByName(C.DataBinding.FieldName);
        if P.DataType = ftBoolean then
        begin
          C.PropertiesClass := TcxCheckBoxProperties;
          (C.Properties as TcxCheckBoxProperties).ValueChecked := 1;
          (C.Properties as TcxCheckBoxProperties).ValueUnChecked := 0;
        end;
      end
      else
        C.Visible := false;
    end;
    GridTableView.ApplyBestFit();
    Grid.BringToFront;
  end;
end;

function TProcedureFrame.GetGridMode: boolean;
begin
  Result := not AllRecordsGridModeButton.Down;
end;

function TProcedureFrame.GetKeyFieldValues: variant;
begin
  if not ProcedureDescription.IsDataSet then
    Result := Unassigned
  else
    Result := MemTable[ProcedureDescription.KeyFieldNames];
end;

function TProcedureFrame.GetProcedureDescription: TProcedureDescription;
begin
  Result := BlockDescription as TProcedureDescription;
end;

procedure TProcedureFrame.ApplyGridViewSettings;
begin
  if not ProcedureDescription.IsDataSet then
    Exit;
  if ProcedureDescription.IsTree then
    TreeList.OptionsView.ColumnAutoWidth := GridAutoWidthBarButton.Down
  else
    GridTableView.OptionsView.ColumnAutoWidth := GridAutoWidthBarButton.Down;

  GridTableView.OptionsCustomize.ColumnSorting := not GridMode;
  GridTableView.OptionsCustomize.ColumnGrouping := not GridMode;
  GridTableView.OptionsCustomize.ColumnFiltering := not GridMode;
  GridTableView.DataController.DataModeController.GridMode := GridMode;
  if GridMode then
  begin
    GridTableView.DataController.Filter.Clear;
    GridSummaryRowBarButton.Down := false;
  end;

  ApplySummaryRow;
  ApplySearchVisible;
end;

procedure TProcedureFrame.GridAutoWidthBarButtonClick(Sender: TObject);
begin
  inherited;
  ApplyGridViewSettings;
end;

function IsNumericField(AField: TField): boolean;
begin
  Result := AField.DataType in [
    ftSmallint,
    ftInteger,
    ftWord,
    ftFloat,
    ftCurrency,
    ftBCD,
    ftLargeint,
    ftFMTBcd
  ];
end;

procedure TProcedureFrame.GridSummaryRowBarButtonClick(Sender: TObject);
begin
  inherited;
  if GridSummaryRowBarButton.Down then
    GridMode := false;
  ApplySummaryRow;
end;

procedure TProcedureFrame.AllRecordsGridModeButtonClick(Sender: TObject);
begin
  inherited;
  ApplyGridViewSettings;
end;

procedure TProcedureFrame.LoadFrameSettings;
begin
  inherited;
  GridAutoWidthBarButton.Down := LoadFrameSettingsValue(cGridAutoWidth, true);
  AllRecordsGridModeButton.Down := LoadFrameSettingsValue(cAllRecordsGridMode, true);
  GridSummaryRowBarButton.Down := LoadFrameSettingsValue(cGridSummaryRow, false);
  SearchFields := LoadFrameSettingsValue(cSearchFieldsSettingsName);

  ApplyGridViewSettings;
end;

function TProcedureFrame.Modified: boolean;
begin
  if BlockDescription.IsDataSet then
    Result := FRecordsModified
  else
  begin
    // для удаления - должно быть всегда true!
    EditorsToParamValues;
    Result := FParamsModified or ProcedureDescription.ForceSave;
  end;
end;

procedure TProcedureFrame.ApplySummaryRow;
var
  VT: TcxGridDBTableView;
  VB: TcxGridDBBandedTableView;
  i: Integer;
begin
  if GridLevel.GridView is TcxGridDBTableView then
  begin
    VT := GridLevel.GridView as TcxGridDBTableView;
    VT.OptionsView.Footer := GridSummaryRowBarButton.Down;
    if GridSummaryRowBarButton.Down then
    begin
      for i := 0 to VT.ColumnCount - 1 do
        if (VT.Columns[i].Summary.FooterKind = skNone) and IsNumericField(VT.Columns[i].DataBinding.Field) then
        begin
          VT.Columns[i].Summary.FooterKind := skSum;
          VT.Columns[i].Summary.FooterFormat := cSummaryFooterFormat;
        end;
    end;
  end
  else if GridLevel.GridView is TcxGridDBBandedTableView then
  begin
    VB := GridLevel.GridView as TcxGridDBBandedTableView;
    VB.OptionsView.Footer := GridSummaryRowBarButton.Down;
    if GridSummaryRowBarButton.Down then
    begin
      for i := 0 to VB.ColumnCount - 1 do
        if (VB.Columns[i].Summary.FooterKind = skNone) and IsNumericField(VB.Columns[i].DataBinding.Field) then
        begin
          VB.Columns[i].Summary.FooterKind := skSum;
          VB.Columns[i].Summary.FooterFormat := cSummaryFooterFormat;
        end;
    end;
  end;
end;

function TProcedureFrame.RefreshOneRecord(PV: TParamValues): boolean;
var
  G: IMultiSafeGuard;
  R: TADMemTable;
  I: Integer;
  PV2: TParamValues;
  P: TParamDescription;
begin
  R := TADMemTable.Create(nil);
  Guard(R, G);
  PV2 := TParamValues.Create;
  Guard(PV2, G);
  Result := false;
  for P in ProcedureDescription.Params.Values do
    // TODO: хардкод!
    if (P.IndexInKeyFields <> 0) and (PV.ContainsKey('O_' + P.Name)) then
    begin
      PV2.AddOrSetValue('I_' + P.Name, PV['O_' + P.Name]);
      Result := true;
    end;
  if not Result then
    Exit;

  Transaction.QueryData(ProcedureDescription, PV2, R);
  MemTable.DisableControls;
  try
    MemTable.Edit;
    for I := 0 to MemTable.Fields.Count - 1 do
      MemTable.Fields[i].Value := R.Fields[i].Value;
    MemTable.Post;
  finally
    MemTable.EnableControls;
  end;

  CustomMainDM.OnRefreshProcedure(ProcedureDescription.Name);
  AfterRefresh;
end;

procedure TProcedureFrame.OnAfterAction(A: TBlockAction; PV: TParamValues);
var
  G: IMultiSafeGuard;
  P: TParamDescription;
  UpdateKeyValues: TParamValues;
begin
  inherited;
  case A.RefreshMode of
    rmDelete:
      begin
        MemTable.Delete;
        AfterRefresh;
      end;
    rmInsert:
      begin
        MemTable.Insert;
        if not RefreshOneRecord(PV) then
          RefreshData;
      end;
    rmUpdate:
      begin
        UpdateKeyValues := TParamValues.Create;
        Guard(UpdateKeyValues, G);
        for P in ProcedureDescription.Params.Values do
          if (P.IndexInKeyFields <> 0) then
            UpdateKeyValues.AddOrSetValue('O_' + P.Name, MemTable[P.Name]);
        if not RefreshOneRecord(UpdateKeyValues) then
          RefreshData;
      end;
    rmFull:
      RefreshData;
    rmNone: ;
  end;
  if A.RefreshMode <> rmNone then
    FRecordsModified := true;
end;

procedure TProcedureFrame.OnChangeParamValues(Sender: TBlockFrame;
  AChangeId: Int64);
var
  ParamName: string;
  NeedRefresh: boolean;
begin
  inherited;
  if (Sender = Self) and (not ProcedureDescription.IsDataSet) then
    FParamsModified := true;

  if
    (Sender <> Self) or
    (not ProcedureDescription.IsDataSet) or
    (BlockDescription.ChildId = 0)
  then
    Exit;

  NeedRefresh := false;
  for ParamName in ChangedParams[AChangeId].Keys do
    if ProcedureDescription.Params[ParamName].ParamDirection in [pdIn, pdInOut] then
      NeedRefresh := true;
  if NeedRefresh then
    RefreshData;
end;

function TProcedureFrame.Open: boolean;
begin
  IsOpening := true;
  Result := OpenQuery;
  if Result then
  begin
    FillFields;
    ApplySummaryRow;
  end;
  IsOpening := false;
end;

procedure TProcedureFrame.RefreshData;
var
  CurrentRecordKey: variant;
begin
  CurrentRecordKey := KeyFieldValues;
  MemTable.Close;
  OpenQuery;
  KeyFieldValues := CurrentRecordKey;
  CustomMainDM.OnRefreshProcedure(ProcedureDescription.Name);
  AfterRefresh;
end;

procedure TProcedureFrame.RefreshDataBarButtonClick(Sender: TObject);
begin
  inherited;
  RefreshData;
end;

function TProcedureFrame.Save: boolean;
begin
  Result := not ProcedureDescription.IsDataSet;
  if Result then
  begin
    PostEditorsValues;
    Transaction.ExecuteProcedure(ProcedureDescription, ParamValues);
    if IsTransactionStart then
      Transaction.Commit;
  end;
end;

procedure TProcedureFrame.SaveFrameSettings;
begin
  inherited;
  SaveFrameSettingsValue(cGridAutoWidth, GridAutoWidthBarButton.Down);
  SaveFrameSettingsValue(cAllRecordsGridMode, AllRecordsGridModeButton.Down);
  SaveFrameSettingsValue(cGridSummaryRow, GridSummaryRowBarButton.Down);
end;

function TProcedureFrame.OpenQuery: Boolean;
begin
  PostEditorsValues;
  Result := ProcedureDescription.IsDataSet;
  if Result then
  begin
    Transaction.QueryData(
      ProcedureDescription,
      ParamValues,
      MemTable
    );
    if ProcedureDescription.IsTree then
      TreeList.FullExpand;
  end;
  PostEditorsValues;
  SetButtonsProperties;
//  ApplyUserSettings;
end;

procedure TProcedureFrame.PostFieldsToParamValues;
var
  F: TField;
  ChangeId: integer;
begin
  ChangeId := BeginParamChanging;
  try
    for F in MemTable.Fields do
      if Assigned(ProcedureDescription.FindParam(F.FieldName)) then
        ParamValues.AddOrSetValue(F.FieldName, F.Value);
  finally
    EndParamChanging(ChangeId);
  end;
end;

procedure TProcedureFrame.QueryAfterOpen(DataSet: TDataSet);
begin
  inherited;
  PostFieldsToParamValues;
end;

procedure TProcedureFrame.QueryAfterPost(DataSet: TDataSet);
begin
  inherited;
  PostFieldsToParamValues;
end;

procedure TProcedureFrame.AfterRefresh;
begin
  SetButtonsProperties;
end;

procedure TProcedureFrame.ApplyDataSetView;

  procedure AddToSearchField(AFieldName: string);
  var
    PD: TParamDescription;
  begin
    AFieldName := AnsiUpperCase(AFieldName);
    PD := ProcedureDescription.FindParam(AFieldName);
    if Assigned(PD) and (PD.ParamDirection = pdField) and (Pos(AFieldName, SearchFields) = 0) then
      SearchFields := DelimitedConcat(SearchFields, AFieldName, ';');
  end;

begin
  if ProcedureDescription.IsTree and (not Assigned(TreeList.OnDblClick)) then
    TreeList.OnDblClick := CallDblClickAction
  else
    if not Assigned(GridTableView.OnDblClick) then
      GridTableView.OnDblClick := CallDblClickAction;
  if ProcedureDescription.IsTree then
    Grid.Visible := false
  else
    TreeList.Visible := false;

  AddToSearchField('contract');
  AddToSearchField('code');
  AddToSearchField('name');

  ApplySearchVisible;
end;

procedure TProcedureFrame.SearchEditPropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
begin
  inherited;
  if AButtonIndex = 0 then
    SearchFieldsBarButtonClick(nil)
  else if AButtonIndex = 1 then
  begin
    SearchEdit.SetFocus;
    SearchEdit.Text := '';
    GridTableView.DataController.Filter.Clear;
  end;
end;

procedure TProcedureFrame.SearchEditPropertiesChange(Sender: TObject);
begin
  inherited;
  if (SearchFields = '') or (SearchEdit.Text = '') then
    Exit;

  GridMode := false;
  AllRecordsGridModeButton.Down := true;
  ApplySearchFilter(GridTableView.DataController, SearchFields, SearchEdit.Text);
end;

procedure TProcedureFrame.SaveSearchFields;
begin
  SaveFrameSettingsValue(cSearchFieldsSettingsName, SearchFields);
end;

procedure TProcedureFrame.ApplySearchVisible;
begin
  SearchPanel.Visible :=
    ProcedureDescription.IsDataSet and
    (not ProcedureDescription.IsTree) and
    (SearchFields <> '')
end;

procedure TProcedureFrame.SearchFieldsBarButtonClick(Sender: TObject);
var
  i: integer;
  S: TStrings;
  F: TFieldsDescription;
  FD: TFieldDescription;
  FieldName: string;
begin
  inherited;
  S := TStringList.Create;
  F := TFieldsDescription.Create([doOwnsValues]);
  try
    S.Delimiter := ';';
    SplitDelimitedString(S, SearchFields, ';');
    for i := 0 to MemTable.FieldCount - 1 do
      if MemTable.Fields[i].Visible then
      begin
        FieldName := AnsiUpperCase(MemTable.Fields[i].FieldName);
        FD := TFieldDescription.Create;
        FD.FieldName := FieldName;
        FD.DisplayLabel := MemTable.Fields[i].DisplayLabel;
        FD.SearchEnabled := S.IndexOf(MemTable.Fields[i].FieldName) <> -1;
        F.Add(FieldName, FD);
      end;
    if TSelectSearchFieldsForm.Execute(F) then
    begin
      S.Clear;
      for FD in F.Values do
        if FD.SearchEnabled then
          S.Add(FD.FieldName);
      SearchFields := S.DelimitedText;
    end;
  finally
    S.Free;
    F.Free;
  end;
  ApplySearchVisible;
  SaveSearchFields;
end;

procedure TProcedureFrame.SetButtonProperties(AButton: TdxBarItem);
var
  BA: TBlockAction;
begin
  inherited;
  if
    (not ProcedureDescription.IsDataSet) and
    (
      (AButton = RefreshDataBarButton) or
      (AButton = FullExpandBarButton) or
      (AButton = FullCollapseBarButton) or
      (AButton = GridAutoWidthBarButton) or
      (AButton = AllRecordsGridModeButton) or
      (AButton = GridSummaryRowBarButton) or
      (AButton = SearchFieldsBarButton) or
      (1=0)
    )
  then
    AButton.Visible := ivNever;

   if
    (not ProcedureDescription.IsTree) and
    (
      (AButton = FullExpandBarButton) or
      (AButton = FullCollapseBarButton) or
      (1=0)
    )
  then
    AButton.Visible := ivNever;

   if
    (ProcedureDescription.IsTree) and
    (
      (AButton = AllRecordsGridModeButton) or
      (AButton = GridSummaryRowBarButton) or
      (AButton = SearchFieldsBarButton) or
      (1=0)
    )
  then
    AButton.Visible := ivNever;

  // enabled у действий по их RefreshMode
  if ProcedureDescription.IsDataSet and (AButton.Tag <> 0) then
  begin
    BA := TObject(AButton.Tag) as TBlockAction;
    if BA.RefreshMode in [rmUpdate, rmDelete] then
      AButton.Enabled := not MemTable.IsEmpty;
  end;
end;

procedure TProcedureFrame.SetGridMode(const Value: boolean);
begin
  AllRecordsGridModeButton.Down := not Value;
  ApplyGridViewSettings;
end;

procedure TProcedureFrame.SetKeyFieldValues(const Value: variant);
begin
  if ProcedureDescription.IsDataSet then
    MemTable.Locate(ProcedureDescription.KeyFieldNames, Value, []);
end;

procedure TProcedureFrame.Build(AParent: TWinControl);
begin
  inherited Build(AParent);
//  FillQuery;
  if not ProcedureDescription.IsDataSet then
  begin
    TreeList.Visible := false;
    Grid.Visible := false;
    ParamsScrollBox.Align := alClient;
  end
  else
    ApplyDataSetView;
  ApplySearchVisible;
  SearchEdit.Properties.Images := CustomMainDM.Images;
end;

function TProcedureFrame.CallDblClickActionInternal: boolean;
var
  A, InsertAction, UpdateAction: TBlockAction;
  InsertActionsCount, UpdateActionsCount: integer;
begin
  Result := inherited CallDblClickActionInternal;
  if Result then
    Exit;

  InsertActionsCount := 0;
  UpdateActionsCount := 0;
  InsertAction := nil;
  UpdateAction := nil;
  for A in ProcedureDescription.Actions.Values do
    case A.RefreshMode of
      rmInsert:
        begin
          InsertAction := A;
          Inc(InsertActionsCount);
        end;
      rmUpdate:
        begin
          UpdateAction := A;
          Inc(UpdateActionsCount);
        end;
    end;

  if (InsertActionsCount = 1) and MemTable.IsEmpty then
    CallAction(InsertAction)
  else if (UpdateActionsCount = 1) and (not MemTable.IsEmpty) then
    CallAction(UpdateAction);
end;

function TProcedureFrame.ConfirmCancel: boolean;
begin
  Result := not ProcedureDescription.ForceSave;
end;

end.
