unit Avk.Gui.BlockFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Avk.Gui.BaseFrame, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  cxScrollBox, cxLabel, System.Generics.Collections, Vcl.StdCtrls,

  Avk.Gui.Descriptions, Vcl.ExtCtrls, cxGroupBox, cxPCdxBarPopupMenu, cxPC,
  cxTextEdit, uFormErrors, dxSkinsCore, Avk.Gui.Connection,
  dxBar, cxClasses, dxSkinsDefaultPainters, dxSkinsdxBarPainter;

type
  TParamControls = class (TObject)
  private
    FEditorControl: TcxCustomEdit;
    FLabelControl: TLabel;
    procedure SetEditorControl(const Value: TcxCustomEdit);
    procedure SetLabelControl(const Value: TLabel);
  public
    property EditorControl: TcxCustomEdit read FEditorControl write SetEditorControl;
    property LabelControl: TLabel read FLabelControl write SetLabelControl;

    destructor Destroy; override;
  end;

  TBlockFrame = class (TBaseFrame)
    ParamsScrollBox: TScrollBox;
    BarManager: TdxBarManager;
    BarManagerMenuBar: TdxBar;
    BarManagerToolBar: TdxBar;
    ViewBarSubItem: TdxBarSubItem;
    SaveViewBarButton: TdxBarButton;
    ActionsBarSubItem: TdxBarSubItem;
    procedure SaveViewBarButtonClick(Sender: TObject);
  private
    { Private declarations }
    FBlockDescription: TBlockDescription;
    FParamControls: TObjectDictionary<string, TParamControls>;
    FGroupControls: TObjectDictionary<string, TWinControl>;
    FGroupsPageControl: TcxPageControl;
    FParamValues: TParamValues;
    FRefControls: TObjectDictionary<integer, TParamControls>;
    FOwnerFrame: TBlockFrame;
    FChangedParams: TObjectDictionary<integer, TParamValues>;
    FUnchangedParams: TStrings;
    FRefParamValues: TParamValues;
    FTransaction: ITransaction;
    FIsTransactionStart: boolean;

    procedure SetBlockDescription(const Value: TBlockDescription);
    procedure CreateParamEditors;
    procedure DeleteParamsEditors;
    procedure CreateParamsGroup(AGroup: string);
    procedure CloseParamsGroup(AGroup: string);
    procedure CreatePC;
    procedure AlignLabels;
    procedure OnActionClick(Sender: TObject);
    procedure CreateActions;
    procedure FillFormErrors;
    procedure SetOwnerFrame(const Value: TBlockFrame);
    procedure SetIsTransactionStart(const Value: boolean);
  protected
    procedure SetTransaction(const Value: ITransaction); virtual;
  public
    { Public declarations }
    IsOpened: boolean;
    IsOpening: boolean;

    EditorDrawPoint: TPoint;
    GroupDrawPoint: TPoint;
    LastParamGroup: string;
    CurrentGroupControl: TWinControl;

    procedure ParamValuesToEditors; virtual;
    procedure EditorsToParamValues; virtual;
    procedure ValidateInput; virtual;
    procedure PostEditorsValues;

    procedure SetButtonProperties(AButton: TdxBarItem); virtual;
    procedure SetButtonsProperties;

    function GetBlockRef(AParam: TParamDescription): TBlockRef;
    procedure OpenRefParamLookups;

    function  BeginParamChanging: Int64;
    procedure EndParamChanging(AChangeId: Int64);

    procedure OnChangeParamValues(Sender: TBlockFrame; AChangeId: Int64); virtual;

    procedure OnEditValueChanged(Sender: TObject); virtual;
    procedure OnAfterAction(A: TBlockAction; PV: TParamValues); virtual;
    procedure CallAction(A: TBlockAction); virtual;
    function  CallDblClickActionInternal: boolean; virtual;
    procedure CallDblClickAction(Sender: TObject);

    function FrameSectionName: string; override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property BlockDescription: TBlockDescription read FBlockDescription write SetBlockDescription;

    procedure HideHeader; virtual;
    procedure Build(AParent: TWinControl); virtual;

    function Open: boolean; virtual;
    function Save: boolean; virtual;

    function Modified: boolean; virtual;
    procedure DropChanges; virtual;
    function ConfirmCancel: boolean; virtual;

    function CreateParamEditor(
      P: TParamDescription;
      AEditorClass: TcxCustomEditClass = nil
    ): TParamControls; virtual;
    function CreateRefEditor(R: TBlockRef): TParamControls; virtual;

    property ParamControls: TObjectDictionary<string, TParamControls> read FParamControls;
    property RefControls: TObjectDictionary<integer, TParamControls> read FRefControls;
    property GroupControls: TObjectDictionary<string, TWinControl> read FGroupControls;
    property GroupsPageControl: TcxPageControl read FGroupsPageControl;

    property ParamValues: TParamValues read FParamValues;
    procedure AssignParamValues(AValues: TParamValues);
    procedure SaveParamValues(AValues: TParamValues);

    property OwnerFrame: TBlockFrame read FOwnerFrame write SetOwnerFrame;

    property ChangedParams: TObjectDictionary<integer, TParamValues> read FChangedParams;
    property Transaction: ITransaction read FTransaction write SetTransaction;
    property IsTransactionStart: boolean read FIsTransactionStart write SetIsTransactionStart;
  end;

  TBlockFrameClass = class of TBlockFrame;

implementation

{$R *.dfm}

uses
  Data.DB, UITypes, Vcl.Menus, Math,
  cxSpinEdit, cxCheckBox, cxCalc, cxCurrencyEdit, cxCalendar, cxTimeEdit,
  cxBlobEdit, cxMemo, cxImage,
  cxDBLookupComboBox, uADCompClient,
  AVK.DX.LookupFilter,
  Avk.Gui.CustomMainDM, Avk.Gui.CustomMainForm, AVK.Core.Utils;

type
  TcxCustomEditCrack = class (TcxCustomEdit)

  end;

const
  cIsNull = '_IS_NULL';

var
  gParamChangeId: Int64 = 1;

  AllowedFieldTypes: set of TFieldType = [ftUnknown..ftFMTBcd];
  FieldEditorClasses: array [ftUnknown..ftFMTBcd] of TcxCustomEditClass = (
    TcxTextEdit,        // ftUnknown
    TcxTextEdit,        // ftString
    TcxSpinEdit,        // ftSmallint
    TcxSpinEdit,        // ftInteger
    TcxSpinEdit,        // ftWord
    TcxCheckBox,        // ftBoolean
    TcxCalcEdit,        // ftFloat
    TcxCurrencyEdit,    // ftCurrency
    TcxCalcEdit,        // ftBCD
    TcxDateEdit,        // ftDate
    TcxTimeEdit,        // ftTime
    TcxDateEdit,        // ftDateTime ?!
    nil,                  // ftBytes
    nil,                  // ftVarBytes
    nil,                  // ftAutoInc
    TcxBlobEdit,        // ftBlob
    TcxMemo,            // ftMemo
    TcxImage,           // ftGraphic
    TcxMemo,            // ftFmtMemo
    nil,                  // ftParadoxOle
    nil,                  // ftDBaseOle
    nil,                  // ftTypedBinary
    nil,                  // ftCursor
    nil,                  // ftFixedChar
    TcxTextEdit,        // ftWideString
    TcxCalcEdit,        // ftLargeint
    nil,                  // ftADT
    nil,                  // ftArray
    nil,                  // ftReference
    nil,                  // ftDataSet
    nil,                  // ftOraBlob
    TcxMemo,            // ftOraClob
    nil,                  // ftVariant
    nil,                  // ftInterface
    nil,                  // ftIDispatch
    nil,                  // ftGuid
    TcxDateEdit,        // ftTimeStamp
    TcxCalcEdit         // ftFMTBcd
  );

{ TBlockFrame }

destructor TBlockFrame.Destroy;
begin
  FParamControls.Free;
  FRefControls.Free;
  FGroupControls.Free;
  FParamValues.Free;
  FChangedParams.Free;
  FUnchangedParams.Free;
  FRefParamValues.Free;
  inherited;
end;

procedure TBlockFrame.DropChanges;
begin
  ;
end;

procedure TBlockFrame.EditorsToParamValues;
var
  ParamName: string;
  RefId: integer;
  V: Variant;
  ValueIndex: integer;
begin
  inherited;
  for ParamName in ParamControls.Keys do
  begin
    ParamControls[ParamName].EditorControl.PostEditValue;
    ParamValues.AddOrSetValue(ParamName, ParamControls[ParamName].EditorControl.EditValue);
  end;
  for RefId in RefControls.Keys do
  begin
    RefControls[RefId].FEditorControl.PostEditValue;
    V := RefControls[RefId].FEditorControl.EditValue;
    if VarIsArray(V) then
    begin
      ValueIndex := 0;
      for ParamName in BlockDescription.BlockRefs[RefId].Params do
      begin
        ParamValues.AddOrSetValue(ParamName, V[ValueIndex]);
        Inc(ValueIndex);
      end;
    end
    else
      ParamValues.AddOrSetValue(BlockDescription.BlockRefs[RefId].MainParam, V);
  end;
end;

function TBlockFrame.FrameSectionName: string;
begin
  if not Assigned(FBlockDescription) then
    Result := inherited FrameSectionName
  else
    Result := FBlockDescription.Name + 'Frame';
end;

procedure TBlockFrame.HideHeader;
begin
  TopLabel.Visible := false;
  BarManagerMenuBar.Visible := false;
  BarManagerToolBar.Visible := false;
end;

function TBlockFrame.Modified: boolean;
begin
  Result := false;
end;

procedure TBlockFrame.OnEditValueChanged(Sender: TObject);
var
  P, EditingParam: TParamDescription;
  E: TcxCustomEdit;
  ChangeId: integer;
  Ref: TBlockRef;
  i: integer;
begin
  E := Sender as TcxCustomEdit;
  EditingParam := TObject((Sender as TcxCustomEdit).Tag) as TParamDescription;
  for P in BlockDescription.Params.Values do
    if
      (P.EnablerParamName = EditingParam.Name) and
      (not VarIsNull(E.EditingValue))
    then
      ParamControls[P.Name].FEditorControl.Enabled := E.EditValue;

  ChangeId := BeginParamChanging;
  try
    Ref := GetBlockRef(EditingParam);
    if Assigned(Ref) then
    begin
      if Ref.Params.Count = 1 then
        ParamValues.AddOrSetValue(Ref.Params[0], E.EditingValue)
      else
        for I := 0 to Ref.Params.Count - 1 do
          ParamValues.AddOrSetValue(Ref.Params[i], E.EditingValue[i]);
    end
    else
      ParamValues.AddOrSetValue(EditingParam.Name, E.EditingValue);
  finally
    EndParamChanging(ChangeId);
  end;
end;

constructor TBlockFrame.Create(AOwner: TComponent);
begin
  inherited;
  FRefControls := TObjectDictionary<integer, TParamControls>.Create([doOwnsValues]);
  FParamControls := TObjectDictionary<string, TParamControls>.Create([doOwnsValues]);
  FGroupControls := TObjectDictionary<string, TWinControl>.Create([doOwnsValues]);
  FChangedParams := TObjectDictionary<integer, TParamValues>.Create([doOwnsValues]);
  FRefParamValues := TParamValues.Create;
  FParamValues := TParamValues.Create;
  FUnchangedParams := TStringList.Create;
end;

procedure TBlockFrame.DeleteParamsEditors;
begin
  FRefControls.Clear;
  ParamControls.Clear;
  GroupControls.Clear;
  FreeAndNil(FGroupsPageControl);
end;

procedure TBlockFrame.AlignLabels;
var
  I: integer;
  MaxLabelRight: integer;
  B: TWinControl;
  R: TRect;
begin
  if not Assigned(CurrentGroupControl) then
    B := ParamsScrollBox
  else
    B := CurrentGroupControl;
  MaxLabelRight := 0;
  for I := 0 to B.ControlCount - 1 do
    if B.Controls[i] is TLabel then
    begin
      R := (B.Controls[i] as TLabel).BoundsRect;
      if R.Right > MaxLabelRight then
        MaxLabelRight := R.Right;
    end;

  if MaxLabelRight = 0 then
    Exit;

  for I := 0 to B.ControlCount - 1 do
    if B.Controls[i] is TLabel then
      B.Controls[i].Left := MaxLabelRight - B.Controls[i].Width
    else
    begin
      B.Controls[i].Left := MaxLabelRight + 4;
      B.Controls[i].Width := B.Width - B.Controls[i].Left - 8;
      B.Controls[i].Anchors := B.Controls[i].Anchors + [akRight];
    end;
end;

procedure TBlockFrame.CloseParamsGroup(AGroup: string);
var
  B: TWinControl;
  I: integer;
  C: TControl;
  MaxPoint: TPoint;
begin
  if Assigned(CurrentGroupControl) then
    B := CurrentGroupControl
  else
    B := ParamsScrollBox;
  if BlockDescription.ParamsDrawDirection = ddVertical then
    AlignLabels;
  MaxPoint.X := 0;
  MaxPoint.Y := 0;
  for i := 0 to B.ControlCount - 1 do
  begin
    C := B.Controls[i];
    if C.BoundsRect.Right > MaxPoint.X then
      MaxPoint.X := C.BoundsRect.Right;
    if C.BoundsRect.Bottom > MaxPoint.Y then
      MaxPoint.Y := C.BoundsRect.Bottom;
  end;
  B.Constraints.MinHeight := MaxPoint.Y + 24;
  B.Constraints.MinWidth := MaxPoint.X + 8;

{
  B.Height := MaxPoint.Y + 24;
  B.Width := MaxPoint.X + 8;
  B.Constraints.MinHeight := B.Height;
  B.Constraints.MinWidth := B.Width;
}
  case BlockDescription.ParamGroupsDrawDirection of
    ddHorizontal: Inc(GroupDrawPoint.X, B.Width + 8);
    ddVertical: Inc(GroupDrawPoint.Y, B.Height + 24);
  end;
end;

function TBlockFrame.ConfirmCancel: boolean;
begin
  Result := true;
end;

procedure TBlockFrame.CreateParamsGroup(AGroup: string);
var
  Box: TcxGroupBox;
  Tab: TcxTabSheet;
begin
  Assert(AGroup <> '');
  case BlockDescription.ParamGroupsDrawDirection of
    ddHorizontal, ddVertical:
      begin
        Box := TcxGroupBox.Create(Self);
        EditorDrawPoint.X := 8;
        EditorDrawPoint.Y := 12;
        Box.Caption := AGroup;
        Box.Parent := ParamsScrollBox;
        Box.Left := GroupDrawPoint.X;
        Box.Top := GroupDrawPoint.Y;
        if BlockDescription.ParamGroupsDrawDirection = ddHorizontal then
          Box.Align := alLeft
        else
          Box.Align := alTop;
        CurrentGroupControl := Box;
      end;
    ddTabs:
      begin
        Tab := TcxTabSheet.Create(Self);
        Tab.Caption := AGroup;
        Tab.PageControl := GroupsPageControl;
        EditorDrawPoint.X := 8;
        EditorDrawPoint.Y := 8;
        CurrentGroupControl := Tab;
      end;
  end;
  GroupControls.Add(AGroup, CurrentGroupControl);
end;

function TBlockFrame.CreateParamEditor(P: TParamDescription; AEditorClass: TcxCustomEditClass): TParamControls;
var
  L: TLabel;
  E: TcxCustomEdit;
  CB: TcxCheckBox;
  SP: TcxSpinEditProperties;
  CP: TcxCalcEditProperties;
begin
  // создаем контролы
  L := nil;
  if Assigned(AEditorClass) then
    E := AEditorClass.Create(Self)
  else
    E := FieldEditorClasses[P.DataType].Create(Self);

  E.Tag := Integer(P);
  TcxCustomEditCrack(E).Properties.OnEditValueChanged := Self.OnEditValueChanged;

  if E is TcxSpinEdit then
  begin
    SP := (TcxCustomEditCrack(E).Properties as TcxSpinEditProperties);
    SP.ClearKey := ShortCut(VK_DELETE, [ssCtrl]);
    SP.UseNullString := true;
  end;
  if E is TcxCalcEdit then
  begin
    CP := (TcxCustomEditCrack(E).Properties as TcxCalcEditProperties);
    CP.ClearKey := ShortCut(VK_DELETE, [ssCtrl]);
    CP.UseNullString := true;
  end;

  if P.ReadOnly then
  begin
    TcxCustomEditCrack(E).Properties.ReadOnly := true;
    E.Style.StyleController := CustomMainDM.ReadOnlyEditStyleController;
  end;
  if P.DataType <> ftBoolean then
  begin
    L := TLabel.Create(Self);
    L.Font.Color := clPurple;
    L.Caption := P.DisplayLabel;
    if P.Required then
      L.Font.Style := L.Font.Style + [fsBold];
  end
  else
  begin
    CB := E as TcxCheckBox;
    CB.Caption := P.DisplayLabel;
    CB.Properties.ValueChecked := 1;
    CB.Properties.ValueUnChecked := 0;
    CB.Style.Font.Color := clPurple;
    if P.Required then
      CB.Style.Font.Style := CB.Style.Font.Style + [fsBold];
  end;
  if Assigned(CurrentGroupControl) then
  begin
    E.Parent := CurrentGroupControl;
    if Assigned(L) then
      L.Parent := CurrentGroupControl;
  end
  else
  begin
    E.Parent := ParamsScrollBox;
    if Assigned(L) then
      L.Parent := ParamsScrollBox;
  end;

  // расставляем их
  case BlockDescription.ParamsDrawDirection of
    ddVertical:
      begin
        if Assigned(L) then
        begin
          L.Left := EditorDrawPoint.X;
          L.Top := EditorDrawPoint.Y + 3;
        end;
        if Assigned(L) then
          E.Left := L.BoundsRect.Right + 7
        else
          E.Left := 20;
        E.Top := EditorDrawPoint.Y;
        Inc(EditorDrawPoint.Y, 27);
      end;
    ddHorizontal:
      begin
        if Assigned(L) then
        begin
          L.Left := EditorDrawPoint.X;
          L.Top := EditorDrawPoint.Y;
        end;
        E.Left := EditorDrawPoint.X - 2;
        E.Top := EditorDrawPoint.Y + 15;
        Inc(EditorDrawPoint.X, E.Width + 8);
      end;
  end;

  Result := TParamControls.Create;
  Result.EditorControl := E;
  Result.LabelControl := L;
end;

function TBlockFrame.CreateRefEditor(R: TBlockRef): TParamControls;
var
  L: TcxLookupComboBox;
  RefBlock: TBlockDescription;
begin
  Result := CreateParamEditor(
    BlockDescription.Params[R.MainParam],
    TcxLookupComboBox
  );
  L := Result.EditorControl as TcxLookupComboBox;

  RefBlock := BlocksManager.Blocks[R.RefsTo];
  L.Properties.KeyFieldNames := RefBlock.KeyFieldNames;
  L.Properties.ListFieldNames := RefBlock.NameFieldNames;
  L.Properties.ListSource := CustomMainDM.GetRefInfo(R.RefsTo).DataSource;

  L.Properties.DropDownSizeable := true;
end;

procedure TBlockFrame.CreatePC;
begin
  if BlockDescription.ParamGroupsDrawDirection = ddTabs then
  begin
    FGroupsPageControl := TcxPageControl.Create(Self);
    FGroupsPageControl.Parent := ParamsScrollBox;
    FGroupsPageControl.Align := alClient;
  end;
end;

function TBlockFrame.GetBlockRef(AParam: TParamDescription): TBlockRef;
var
  R: TBlockRef;
begin
  Result := nil;
  for R in BlockDescription.BlockRefs.Values do
    if R.Params.IndexOf(AParam.Name) <> -1 then
    begin
      Result := R;
      Break;
    end;
end;

procedure TBlockFrame.SetButtonProperties(AButton: TdxBarItem);
begin
  ;
end;

procedure TBlockFrame.FillFormErrors;
var
  P: TParamDescription;
  ED: TErrorDescription;
begin
  FormErrors.Items.Clear;
  for P in BlockDescription.Params.Values do
    if P.Required and (P.ParamDirection in [pdIn, pdInOut]) then
    begin
      ED := TErrorDescription.Create(FormErrors.Items);
      ED.ErrorCode := P.Name + cIsNull;
      ED.ErrorText := Format('Поле "%s" не заполнено', [P.DisplayLabel]);
      if ParamControls.ContainsKey(P.Name) then
        ED.FocusControl := ParamControls[P.Name].EditorControl;
    end;
end;

procedure TBlockFrame.CreateParamEditors;
var
  P: TParamDescription;
  R: TBlockRef;
begin
  CreatePC;
  EditorDrawPoint.X := 8;
  EditorDrawPoint.Y := 8;
  LastParamGroup := '';
  for P in BlockDescription.SortedParams do
    if (not (P.ParamDirection in [pdCursor, pdField])) and P.Visible then
    begin
      if P.Group <> LastParamGroup then
      begin
        CloseParamsGroup(LastParamGroup);
        CreateParamsGroup(P.Group);
        LastParamGroup := P.Group;
      end;
      R := GetBlockRef(P);
      if Assigned(R) then
      begin
        if (not RefControls.ContainsKey(R.ID)) and (R.MainParam = P.Name) then
          RefControls.Add(R.ID, CreateRefEditor(R));
      end
      else
        ParamControls.Add(P.Name, CreateParamEditor(P));
    end;
  CloseParamsGroup(LastParamGroup);
  Constraints.MinWidth := Max(EditorDrawPoint.X + 8, Constraints.MinWidth);
  Constraints.MinHeight := Max(EditorDrawPoint.Y + 40, Constraints.MinHeight);
  FillFormErrors;
  ParamsScrollBox.Visible := ParamsScrollBox.ControlCount <> 0;
end;

procedure TBlockFrame.ParamValuesToEditors;
var
  P: TParamDescription;
  R: TBlockRef;
  V: variant;
  i: Integer;
begin
  for P in BlockDescription.Params.Values do
    if ParamValues.ContainsKey(P.Name) and ParamControls.ContainsKey(P.Name) then
      ParamControls[P.Name].FEditorControl.EditValue := ParamValues[P.Name];
  for R in BlockDescription.BlockRefs.Values do
    if RefControls.ContainsKey(R.ID) then
    begin
      if R.Params.Count > 1 then
      begin
        V := VarArrayCreate([0, R.Params.Count - 1], VarVariant);
        for i := 0 to R.Params.Count - 1 do
          if ParamValues.ContainsKey(R.Params[i]) then
            V[i] := ParamValues[R.Params[i]];
        RefControls[R.ID].EditorControl.EditValue := V;
      end
      else
        if ParamValues.ContainsKey(R.MainParam) then
          RefControls[R.ID].EditorControl.EditValue := ParamValues[R.MainParam];
    end;
end;

procedure TBlockFrame.ValidateInput;
var
  ParamName: string;
begin
  for ParamName in ParamValues.Keys do
    if
      VarIsNull(ParamValues[ParamName]) and
      BlockDescription.ParamByName(ParamName).Required
    then
      FormErrors.RegisterError(ParamName + cIsNull);
end;

procedure TBlockFrame.PostEditorsValues;
begin
  EditorsToParamValues;
  if not IsOpening then
  begin
    ValidateInput;
    FormErrors.ShowErrors;
  end;
end;

procedure TBlockFrame.OnActionClick(Sender: TObject);
begin
  CallAction(TObject((Sender as TdxBarButton).Tag) as TBlockAction);
end;

procedure TBlockFrame.OnAfterAction(A: TBlockAction; PV: TParamValues);
begin
  ;
end;

function TBlockFrame.BeginParamChanging: Int64;
var
  PV: TParamValues;
begin
  Inc(gParamChangeId);
  Result := gParamChangeId;

  PV := TParamValues.Create;
  AddParamValues(Self.ParamValues, PV);
  FChangedParams.Add(Result, PV);
end;

procedure TBlockFrame.EndParamChanging(AChangeId: Int64);
var
  ParamName: string;
  ChangedParamValues: TParamValues;
begin
  try
    if IsOpening then
      Exit;

    // удаляем не изменившиеся значения
    ChangedParamValues := FChangedParams[AChangeId];

    FUnchangedParams.Clear;
    for ParamName in ChangedParamValues.Keys do
      if VarsIsEqual(ChangedParamValues[ParamName], ParamValues[ParamName]) then
        FUnchangedParams.Add(ParamName);
    for ParamName in FUnchangedParams do
      ChangedParamValues.Remove(ParamName);

    // вызываем обработку изменений
    if ChangedParamValues.Count <> 0 then
      OnChangeParamValues(Self, AChangeId);
  finally
    FChangedParams.Remove(AChangeId);
  end;
end;

procedure TBlockFrame.OnChangeParamValues(Sender: TBlockFrame; AChangeId: Int64);

procedure RefreshRefQueries;
var
  ParamName: string;
  PV: TParamValues;
  RefId: integer;
  Ref: TBlockRef;
  RefBind: TBlockRefBind;
  RefInfo: TRefInfo;
begin
  if BlockDescription.BlockRefs.Count = 0 then
    Exit;

  PV := ChangedParams[AChangeId];
  for ParamName in PV.Keys do
    // измененное значение. какой из рефов зависит от этого
    for RefId in BlockDescription.BlockRefs.Keys do
    begin
      RefInfo := nil;
      Ref := BlockDescription.BlockRefs[RefId];
      for RefBind in Ref.Binds.Values do
        if (RefBind.SourceBlockId = 0) and (RefBind.SourceParam = ParamName) then
          RefInfo := CustomMainDM.GetRefInfo(Ref.RefsTo);
      if Assigned(RefInfo) then
        RefInfo.RefreshData(ParamValues);
    end;
end;

begin
{
  обновляем свои (процедуры) Ref параметры и запросы
  посылаем инфу наверх об обновлении. там, наверху: наверху
    обновляем свои параметры по дочерним
      при этом записываем информацию в список изменений и отсылаем наверх!
    обновляем Ref параметры и запросы в других блоках по измененным
    обновляем (открываем заново) связанные блоки по измененным параметрам

  сделать все единообразно на уровне изменения ParamValues?
  тогда все рассылки будут по мелким поводам (каждому полю/редактору в отдельности)
  обьединять в блоки по BeginParamChanging/EndParamChanging? и после обработки блока чистить?
  а если пересечения? выдавать id и работать по нему?

  у изменения параметра 4 следствия:
    - Ref этого же блока (вне формы)
    - параметры формы-владельца
      - также вызываем изменения
    - Ref другого блока в этой же форме
    - параметры другого блока в этой же форме
      - группируем по блокам и вызываем изменения этого блока
        - с finally EndParamChanging по последнему id
      - тут могут быть циклы!

    - при переоткрытии Ref-а старых значений (ссылки) там может не найтись
      это означает обнуление всех значений параметров этого рефа (всех ли?!)
      и смену значений параметров также рекурсивно

  если мы не записывам, что изменилось, то обновляем все!
    это переоткрытия запросов без необходимости
  если записываем, то как быть с удалением этих записей?
    могут быть пересечения - т.е. добавление в журнал изменений
      во время обработки других изменений
    эти пересечения не обработаны, но будут удалены как обработанные

  в BeginParamChanging генерируем и отдаем id, запоминаем значения параметров
  в EndParamChanging удаляем не изменившиеся параметры,
    и если они есть, то вызываем OnChangeParamValues с id
    потом чистим изменившиеся параметры

  в OnChangeParamValues
}

  RefreshRefQueries;
  if Assigned(OwnerFrame) then
    OwnerFrame.OnChangeParamValues(Self, AChangeId);
end;

function TBlockFrame.CallDblClickActionInternal: boolean;
var
  A: TBlockAction;
begin
  Result := false;
  for A in BlockDescription.Actions.Values do
    if A.ActionStyle = asGridDblClick then
    begin
      CallAction(A);
      Result := true;
      Break;
    end;
end;

procedure TBlockFrame.OpenRefParamLookups;
var
  R: TBlockRef;
  RB: TBlockRefBind;
  RefInfo: TRefInfo;
begin
  FRefParamValues.Clear;
  for R in BlockDescription.BlockRefs.Values do
    if RefControls.ContainsKey(R.ID) then
    begin
      RefInfo := CustomMainDM.GetRefInfo(R.RefsTo);
      if BlockDescription.ChildId = 0 then
        for RB in R.Binds.Values do
          if RB.SourceBlockId = 0 then
            FRefParamValues.Add(RB.DestinationParam, ParamValues[RB.SourceParam]);
      RefInfo.RefreshData(FRefParamValues);
    end;
end;

function TBlockFrame.Open: boolean;
begin
  Result := false;
  SetButtonsProperties;
end;

procedure TBlockFrame.CreateActions;
var
  A: TBlockAction;
  B: TdxBarButton;
  ButtonIndex: integer;
begin
  ButtonIndex := 0;
  for A in BlockDescription.SortedActions do
    if (A.ActionStyle = asButton) or (A.ImageIndex <> -1) then
    begin
      B := TdxBarButton.Create(BarManager);
      B.Caption := A.Caption;
      B.Category := 3;
      B.OnClick := OnActionClick;
      B.Tag := Integer(A);
      B.ImageIndex := A.ImageIndex;
      B.ShortCut := A.ShortCut;
      ActionsBarSubItem.ItemLinks.Insert(ButtonIndex).Item := B;
      BarManagerToolBar.ItemLinks.Insert(ButtonIndex).Item := B;
      Inc(ButtonIndex);
    end;
  if ButtonIndex > 0 then
    BarManagerToolBar.ItemLinks.Items[ButtonIndex].BeginGroup := true
  else
    if not BlockDescription.IsDataSet then
    begin
      BarManagerMenuBar.Visible := false;
      BarManagerToolBar.Visible := false;
    end;
end;

procedure TBlockFrame.AssignParamValues(AValues: TParamValues);
begin
  AddParamValues(AValues, FParamValues);
  ParamValuesToEditors;
end;

procedure TBlockFrame.Build(AParent: TWinControl);
begin
  Assert(Assigned(BlockDescription));
  BlockDescription.Validate;
  LoadFrameSettings;

  TopLabel.Caption := BlockDescription.DisplayLabel;
  // в runtime не цепляется, т.к. дата модуль создается другим классом
  BarManager.ImageOptions.Images := CustomMainDM.Images;

  DeleteParamsEditors;
  CreateParamEditors;

  Parent := AParent;
  Align := alClient;
  CreateActions;
  OpenRefParamLookups;
  SetFilterToLookups(ParamsScrollBox);
  ParamValuesToEditors;
end;

function TBlockFrame.Save: boolean;
begin
  Result := false;
end;

procedure TBlockFrame.SaveParamValues(AValues: TParamValues);
var
  ParamName: string;
begin
  AValues.Clear;
  for ParamName in ParamValues.Keys do
    if BlockDescription.ParamByName(ParamName).ParamDirection in [pdOut, pdInOut] then
      AValues.AddOrSetValue(ParamName, ParamValues[ParamName]);
end;

procedure TBlockFrame.SaveViewBarButtonClick(Sender: TObject);
begin
  inherited;
  SaveFrameSettings;
end;

procedure TBlockFrame.CallAction(A: TBlockAction);
var
  P: TPair<string, string>;
  PV: TParamValues;
  T: ITransaction;
  TS: boolean;
begin
  if A.LinksToName = '' then
    Exit;
  PV := TParamValues.Create;
  try
    PostEditorsValues;
    for P in A.ParamBinds.ToArray do
      PV.Add(P.Value, ParamValues[P.Key]);

    if Transaction = CustomMainDM.MainTransaction then
    begin
      T := CustomMainDM.Connection.StartTransaction;
      TS := true;
    end
    else
    begin
      T := Transaction;
      TS := false;
    end;

    if CustomMainForm.ShowBlock(A.LinksTo, T, TS, PV) then
      OnAfterAction(A, PV);
  finally
    PV.Free;
  end;
end;

procedure TBlockFrame.CallDblClickAction(Sender: TObject);
begin
  CallDblClickActionInternal;
end;

procedure TBlockFrame.SetBlockDescription(const Value: TBlockDescription);
begin
  FBlockDescription := Value;
end;

procedure TBlockFrame.SetButtonsProperties;
var
  i: integer;
begin
  for i := 0 to BarManager.ItemCount - 1 do
    SetButtonProperties(BarManager.Items[i]);
end;

procedure TBlockFrame.SetIsTransactionStart(const Value: boolean);
begin
  FIsTransactionStart := Value;
end;

procedure TBlockFrame.SetOwnerFrame(const Value: TBlockFrame);
begin
  FOwnerFrame := Value;
end;

procedure TBlockFrame.SetTransaction(const Value: ITransaction);
begin
  FTransaction := Value;
end;

{ TParamControls }

destructor TParamControls.Destroy;
begin
  FEditorControl.Free;
  FLabelControl.Free;
  inherited;
end;

procedure TParamControls.SetEditorControl(const Value: TcxCustomEdit);
begin
  FEditorControl := Value;
end;

procedure TParamControls.SetLabelControl(const Value: TLabel);
begin
  FLabelControl := Value;
end;

end.
