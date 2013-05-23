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
  dxSkinsdxBarPainter, dxSkinscxPCPainter;

type
  TProcedureFrame = class (TBlockFrame)
    GridLevel: TcxGridLevel;
    Grid: TcxGrid;
    Query: TADQuery;
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

    FConnection: TADConnection;

    function GetProcedureDescription: TProcedureDescription;
    procedure SetConnection(const Value: TADConnection);
    procedure ApplyParamsToQuery;
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
  public
    SearchFields: string;

    { Public declarations }
    procedure EditorsToParamValues; override;
    procedure OnChangeParamValues(Sender: TBlockFrame; AChangeId: Int64); override;

    procedure FillFields;
    procedure FillQuery;
    procedure Build(AParent: TWinControl); override;
    function Open: boolean; override;
    function Save: boolean; override;
    function Modified: boolean; override;

    procedure SetButtonProperties(AButton: TdxBarItem); override;
    function CallDblClickActionInternal: boolean; override;

    procedure OnAfterAction(A: TBlockAction; PV: TParamValues); override;
    procedure AfterRefresh; virtual;

    procedure SaveFrameSettings; override;
    procedure LoadFrameSettings; override;

    property ProcedureDescription: TProcedureDescription read GetProcedureDescription;
    property Connection: TADConnection read FConnection write SetConnection;

    property KeyFieldValues: variant read GetKeyFieldValues write SetKeyFieldValues;
    property GridMode: boolean read GetGridMode write SetGridMode;
  end;

implementation

{$R *.dfm}

uses
  Generics.Collections,
  Avk.Gui.CustomMainDM, Avk.Gui.SearchFieldsSelect,
  AVK.Core.Utils, AVK.DX.LookupFilter;

const
  cGridAutoWidth = 'GRID_AUTO_WIDTH';
  cAllRecordsGridMode = 'GRID_MODE';
  cGridSummaryRow = 'GRID_SUMMARY_ROW';
  cSearchFieldsSettingsName = 'GRID_SEARCH_FIELDS';
  cSummaryFooterFormat = '### ### ### ##0.00';

{ TProcedureFrame }

procedure TProcedureFrame.FillQuery;
begin
  CustomMainDM.DBDependend.FillQuery(ProcedureDescription, Query);
  if not Assigned(Query.Connection) then
    Query.Connection := CustomMainDM.MainConnection;
end;

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
  if Query.Active then
    for P in BlockDescription.Params.Values do
    begin
      if Query.Active then
      begin
        if P.ParamDirection = pdField then
          ParamValues.AddOrSetValue(P.Name, Query.FieldValues[P.Name])
      end
      else if P.ParamDirection in [pdOut, pdInOut] then
        ParamValues.AddOrSetValue(P.Name, Query.ParamByName(P.Name).Value);
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
  CustomMainDM.FillQueryFields(Query, ProcedureDescription);

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
      P := ProcedureDescription.ParamByName(C.DataBinding.FieldName);
      if P.DataType = ftBoolean then
      begin
        C.PropertiesClass := TcxCheckBoxProperties;
        (C.Properties as TcxCheckBoxProperties).ValueChecked := 1;
        (C.Properties as TcxCheckBoxProperties).ValueUnChecked := 0;
      end;
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
    Result := Query[ProcedureDescription.KeyFieldNames];
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
    EditorsToParamValues;
    Result := FParamsModified;
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

procedure TProcedureFrame.OnAfterAction(A: TBlockAction; PV: TParamValues);
begin
  inherited;
  case A.RefreshMode of
    rmInsert,
    rmUpdate,
    rmDelete,
    rmFull:
      begin
        RefreshData;
        FRecordsModified := true;
      end;
    rmNone: ;
  end;
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
  if not Assigned(Connection) then
    Connection := CustomMainDM.MainConnection;
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
  Query.Close;
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

procedure TProcedureFrame.ApplyParamsToQuery;
var
  ParamName: string;
  P: TParamDescription;
begin
  for ParamName in ParamValues.Keys do
  begin
    P := ProcedureDescription.ParamByName(ParamName);
    if P.ParamDirection in [pdIn, pdInOut] then
      Query.ParamByName(ParamName).Value := ParamValues[ParamName];
    if P.ParamDirection = pdInOut then
      Query.ParamByName(ParamName).ParamType := ptInputOutput
    else if P.ParamDirection = pdOut then
      Query.ParamByName(ParamName).ParamType := ptOutput;
  end;
end;

function TProcedureFrame.Save: boolean;
begin
  Result := not ProcedureDescription.IsDataSet;
  if Result then
  begin
    PostEditorsValues;
    ApplyParamsToQuery;
    Query.ExecSQL;
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
  ApplyParamsToQuery;
  Result := ProcedureDescription.IsDataSet;
  if Result then
  begin
    Query.Close;
    Query.Open;
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
    for F in Query.Fields do
      if Assigned(ProcedureDescription.FindParam(F.FieldName)) then
        ParamValues.AddOrSetValue(F.FieldName, F.Value);
  finally
    EndParamChanging(ChangeId);
  end;
end;

procedure TProcedureFrame.QueryAfterOpen(DataSet: TDataSet);
begin
  inherited;
  CustomMainDM.LogQueryOpen(DataSet as TADQuery, 'procedure open');
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
    for i := 0 to Query.FieldCount - 1 do
      if Query.Fields[i].Visible then
      begin
        FieldName := AnsiUpperCase(Query.Fields[i].FieldName);
        FD := TFieldDescription.Create;
        FD.FieldName := FieldName;
        FD.DisplayLabel := Query.Fields[i].DisplayLabel;
        FD.SearchEnabled := S.IndexOf(Query.Fields[i].FieldName) <> -1;
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
      AButton.Enabled := not Query.IsEmpty;
  end;
end;

procedure TProcedureFrame.SetConnection(const Value: TADConnection);
begin
  FConnection := Value;
  Query.Connection := Connection;
end;

procedure TProcedureFrame.SetGridMode(const Value: boolean);
begin
  AllRecordsGridModeButton.Down := not Value;
  ApplyGridViewSettings;
end;

procedure TProcedureFrame.SetKeyFieldValues(const Value: variant);
begin
  if ProcedureDescription.IsDataSet then
    Query.Locate(ProcedureDescription.KeyFieldNames, Value, []);
end;

procedure TProcedureFrame.Build(AParent: TWinControl);
begin
  inherited Build(AParent);
  FillQuery;
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

  if (InsertActionsCount = 1) and Query.IsEmpty then
    CallAction(InsertAction)
  else if (UpdateActionsCount = 1) and (not Query.IsEmpty) then
    CallAction(UpdateAction);
end;

end.
