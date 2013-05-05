unit Avk.Gui.Descriptions;

interface

uses
  Generics.Collections, System.Types, System.Classes, Data.DB,
  Vcl.Controls, Generics.Defaults;

type
  TParamValues = TDictionary<string,variant>;

  TParamDirection = (pdIn, pdOut, pdInOut, pdField, pdCursor);

  TDrawDirection = (ddHorizontal, ddVertical, ddTabs);
  TPanelDrawDirection = (pddLeft, pddRight, pddTop, pddBottom, pddTabs);

  TParamDescription = class (TObject)
  private
    FName: string;
    FDisplayLabel: string;
    FParamDirection: TParamDirection;
    FRequired: boolean;
    FVisible: boolean;
    FDataType: TFieldType;
    FEnablerParamName: string;
    FGroup: string;
    FReadOnly: boolean;
    FSourceBlockId: integer;
    FSourceParamName: string;
    FEditorConstraints: TSizeConstraints;
    FIndexInParentFields: integer;
    FIndexInKeyFields: integer;
    FIndexInNameFields: integer;
    FAutoRefresh: boolean;
    FOrderNum: integer;
    procedure SetDataType(const Value: TFieldType);
    procedure SetDisplayLabel(const Value: string);
    procedure SetEnablerParamName(const Value: string);
    procedure SetGroup(const Value: string);
    procedure SetName(const Value: string);
    procedure SetParamDirection(const Value: TParamDirection);
    procedure SetRequired(const Value: boolean);
    procedure SetVisible(const Value: boolean);
    procedure SetReadOnly(const Value: boolean);
    procedure SetSourceParamName(const Value: string);
    procedure SetEditorConstraints(const Value: TSizeConstraints);
    procedure SetIndexInKeyFields(const Value: integer);
    procedure SetIndexInParentFields(const Value: integer);
    procedure SetIndexInNameFields(const Value: integer);
    procedure SetAutoRefresh(const Value: boolean);
    procedure SetOrderNum(const Value: integer);
  public
    property Name: string read FName write SetName;
    property DisplayLabel: string read FDisplayLabel write SetDisplayLabel;
    property Visible: boolean read FVisible write SetVisible;
    property Required: boolean read FRequired write SetRequired;
    property ReadOnly: boolean read FReadOnly write SetReadOnly;
    property DataType: TFieldType read FDataType write SetDataType;
    property ParamDirection: TParamDirection read FParamDirection write SetParamDirection;
    property IndexInKeyFields: integer read FIndexInKeyFields write SetIndexInKeyFields;
    property IndexInParentFields: integer read FIndexInParentFields write SetIndexInParentFields;
    property IndexInNameFields: integer read FIndexInNameFields write SetIndexInNameFields;
    property AutoRefresh: boolean read FAutoRefresh write SetAutoRefresh;
    property OrderNum: integer read FOrderNum write SetOrderNum;
    property CallOrderNum: integer read FOrderNum write SetOrderNum;

    // для связей мастер-деталь внутри формы
    // и привязки параметров формы к параметрам ее блоков
    property SourceBlockId: integer read FSourceBlockId write FSourceBlockId;
    property SourceParamName: string read FSourceParamName write SetSourceParamName;

    property EnablerParamName: string read FEnablerParamName write SetEnablerParamName;
    property Group: string read FGroup write SetGroup;

    property EditorConstraints: TSizeConstraints read FEditorConstraints write SetEditorConstraints;
  end;

  TParamsComparer = class (TComparer<TParamDescription>)
  public
    function Compare(const Left, Right: TParamDescription): Integer; override;
  end;

  TParamsCallComparer = class (TComparer<TParamDescription>)
  public
    function Compare(const Left, Right: TParamDescription): Integer; override;
  end;

  TBlockActionStyle = (asButton, asGridDblClick);
  TRefreshMode = (rmInsert, rmUpdate, rmDelete, rmFull, rmNone);

  TBlockRefBind = class (TObject)
  public
    SourceParam, DestinationParam: string;
    // 0 для ссылки из процедуры, в которой нет child блоков
    SourceBlockId: integer;

    constructor Create(ASourceBlockId: integer; ASourceParam, ADestinationParam: string);
  end;

  // ссылка из параметров блока
  TBlockRef = class (TObject)
  private
    FParams: TStrings;
    FRefsTo: string;
    FID: integer;
    FMainParam: string;
    FBinds: TObjectDictionary<string, TBlockRefBind>;
    procedure SetID(const Value: integer);
    procedure SetRefsTo(const Value: string);
    procedure SetParams(const Value: TStrings);
    procedure SetMainParam(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    property ID: integer read FID write SetID;
    property RefsTo: string read FRefsTo write SetRefsTo;
    property Params: TStrings read FParams write SetParams;
    // ключ - DestinationParam
    property Binds: TObjectDictionary<string, TBlockRefBind> read FBinds;
    property MainParam: string read FMainParam write SetMainParam;
  end;

  TFormDescription = class;

  TBlockAction = class (TObject)
  private
    FLinksToName: string;
    FLinksTo: TFormDescription;
    FActionStyle: TBlockActionStyle;
    FParamBinds: TDictionary<string, string>;
    FCaption: string;
    FRefreshMode: TRefreshMode;
    FImageIndex: integer;
    FName: string;
    FOrderNum: integer;
    procedure SetLinksToName(const Value: string);
    procedure SetActionStyle(const Value: TBlockActionStyle);
    procedure SetCaption(const Value: string);
    procedure SetRefreshMode(const Value: TRefreshMode);
    function GetLinksTo: TFormDescription;
    procedure SetImageIndex(const Value: integer);
    procedure SetName(const Value: string);
    procedure SetOrderNum(const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;

    property Name: string read FName write SetName;
    property Caption: string read FCaption write SetCaption;
    property OrderNum: integer read FOrderNum write SetOrderNum;

    property ActionStyle: TBlockActionStyle read FActionStyle write SetActionStyle;
    property RefreshMode: TRefreshMode read FRefreshMode write SetRefreshMode;

    property LinksToName: string read FLinksToName write SetLinksToName;
    property LinksTo: TFormDescription read GetLinksTo;
    property ParamBinds: TDictionary<string, string> read FParamBinds;
    property ImageIndex: integer read FImageIndex write SetImageIndex;
  end;

  TActionsComparer = class (TComparer<TBlockAction>)
  public
    function Compare(const Left, Right: TBlockAction): Integer; override;
  end;

  TBlockDescription = class (TObject)
  private
    FDisplayLabel: string;
    FCustomClassName: string;
    FParams: TObjectDictionary<string, TParamDescription>;
    FParamsDrawDirection: TDrawDirection;
    FConstraints: TSizeConstraints;
    FParamGroupsDrawDirection: TDrawDirection;
    FName: string;
    FActions: TObjectDictionary<string, TBlockAction>;
    FIsModal: boolean;
    FKeyFieldNames: string;
    FNameFieldNames: string;
    FParentFieldNames: string;
    FChildId: integer;
    FPanelId: integer;
    FIndexOnPanel: integer;
    FVisible: boolean;
    FBlockRefs: TObjectDictionary<integer, TBlockRef>;
    FSortedActions: TArray<TBlockAction>;
    FSortedParams: TArray<TParamDescription>;
    FCallSortedParams: TArray<TParamDescription>;

    procedure SetDisplayLabel(const Value: string);
    procedure SetCustomClassName(const Value: string);
    procedure SetParamsDrawDirection(const Value: TDrawDirection);
    procedure SetParamGroupsDrawDirection(const Value: TDrawDirection);
    procedure SetName(const Value: string);
    procedure SetIsModal(const Value: boolean);
    function GetIsDataSet: boolean;
    function GetIsTree: boolean;
    function GetKeyFieldNames: string;
    function GetParentFieldNames: string;
    procedure FillKeyParentNameFields;
    function GetNameFieldNames: string;
    procedure SetChildId(const Value: integer);
    procedure SetPanelId(const Value: integer);
    procedure SetIndexOnPanel(const Value: integer);
    procedure SetVisible(const Value: boolean);
    procedure OnParamsValueChanged(Sender: TObject; const Item: TParamDescription;
      Action: Generics.Collections.TCollectionNotification);
    procedure OnActionsValueChanged(Sender: TObject; const Item: TBlockAction;
      Action: Generics.Collections.TCollectionNotification);
    function GetSortedParams: TArray<TParamDescription>;
    function GetSortedActions: TArray<TBlockAction>;
    function GetCallSortedParams: TArray<TParamDescription>;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetIsModal: boolean; virtual;

    property DisplayLabel: string read FDisplayLabel write SetDisplayLabel;
    property Name: string read FName write SetName;

    // специальный фрейм
    property CustomClassName: string read FCustomClassName write SetCustomClassName;

    property Params: TObjectDictionary<string, TParamDescription> read FParams;

    function FindParam(AParamName: string): TParamDescription;
    function ParamByName(AParamName: string): TParamDescription;

    property ParamsDrawDirection: TDrawDirection read FParamsDrawDirection write SetParamsDrawDirection;
    property ParamGroupsDrawDirection: TDrawDirection read FParamGroupsDrawDirection write SetParamGroupsDrawDirection;
    property Constraints: TSizeConstraints read FConstraints;

    property KeyFieldNames: string read GetKeyFieldNames;
    property NameFieldNames: string read GetNameFieldNames;
    property ParentFieldNames: string read GetParentFieldNames;

    property IsDataSet: boolean read GetIsDataSet;
    property IsTree: boolean read GetIsTree;

    property Actions: TObjectDictionary<string, TBlockAction> read FActions;
    property IsModal: boolean read FIsModal write SetIsModal;

    property BlockRefs: TObjectDictionary<integer, TBlockRef> read FBlockRefs;

    property ChildId: integer read FChildId write SetChildId;
    property PanelId: integer read FPanelId write SetPanelId;
    property IndexOnPanel: integer read FIndexOnPanel write SetIndexOnPanel;
    property Visible: boolean read FVisible write SetVisible;

    property SortedParams: TArray<TParamDescription> read GetSortedParams;
    property CallSortedParams: TArray<TParamDescription> read GetCallSortedParams;
    property SortedActions: TArray<TBlockAction> read GetSortedActions;

    procedure Validate; virtual;
  end;

  TBlocksManager = class (TObject)
  private
    FBlocks: TObjectDictionary<string, TBlockDescription>;
  public
    constructor Create;
    destructor Destroy; override;

    property Blocks: TObjectDictionary<string, TBlockDescription> read FBlocks;
  end;

  // Блок типа процедура - с процедурой, возможно возвращающей датасет
  TProcedureDescription = class (TBlockDescription)
  private
    FProcedureName: string;
    FParamsToGridDirection: TDrawDirection;

    procedure SetParamsToGridDirection(const Value: TDrawDirection);
    procedure SetProcedureName(const Value: string);
  public
    procedure Validate; override;

    property ProcedureName: string read FProcedureName write SetProcedureName;

    property ParamsToGridDirection: TDrawDirection read FParamsToGridDirection write SetParamsToGridDirection;
  end;

  // панель как группировка блоков
  TPanelDescription = class (TObject)
  private
    FChildPanels: TObjectList<TPanelDescription>;
    FItemsDrawStyle: TPanelDrawDirection;
    FDisplayLabel: string;
    FBlocks: TObjectList<TBlockDescription>;
    FIndexOnParent: integer;
    FParentId: integer;
    FId: integer;
    procedure SetItemsDrawStyle(const Value: TPanelDrawDirection);
    procedure SetDisplayLabel(const Value: string);
    procedure SetIndexOnParent(const Value: integer);
    procedure SetParentId(const Value: integer);
    procedure SetId(const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;

    property Id: integer read FId write SetId;
    property ParentId: integer read FParentId write SetParentId;

    property DisplayLabel: string read FDisplayLabel write SetDisplayLabel;
    property IndexOnParent: integer read FIndexOnParent write SetIndexOnParent;
    property ItemsDrawStyle: TPanelDrawDirection read FItemsDrawStyle write SetItemsDrawStyle;

    property Blocks: TObjectList<TBlockDescription> read FBlocks;
    property ChildPanels: TObjectList<TPanelDescription> read FChildPanels;
  end;

  // описание формы
  TFormDescription = class (TBlockDescription)
  private
    FBlocks: TObjectDictionary<integer, TBlockDescription>;
    FRootPanel: TPanelDescription;
    FMainProcedureName: string;
    FHeaderVisible: boolean;
    FPanels: TObjectDictionary<integer, TPanelDescription>;

    procedure SetRootPanel(const Value: TPanelDescription);
    procedure SetMainProcedureName(const Value: string);
    procedure SetHeaderVisible(const Value: boolean);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Validate; override;

    property Blocks: TObjectDictionary<integer, TBlockDescription> read FBlocks;
    property RootPanel: TPanelDescription read FRootPanel write SetRootPanel;
    property Panels: TObjectDictionary<integer, TPanelDescription> read FPanels;
    property HeaderVisible: boolean read FHeaderVisible write SetHeaderVisible;

    property MainProcedureName: string read FMainProcedureName write SetMainProcedureName;

    function GetIsModal: boolean; override;
  end;

function BlocksManager: TBlocksManager;
procedure AddParamValues(Source, Destination: TParamValues);

implementation

uses
  System.SysUtils, System.Math,
  AVK.Core.Utils;

var
  FBlocksManager: TBlocksManager;

function BlocksManager: TBlocksManager;
begin
  if not Assigned(FBlocksManager) then
    FBlocksManager := TBlocksManager.Create;
  Result := FBlocksManager;
end;

{ TParamDescription }

procedure TParamDescription.SetAutoRefresh(const Value: boolean);
begin
  FAutoRefresh := Value;
end;

procedure TParamDescription.SetDataType(const Value: TFieldType);
begin
  FDataType := Value;
end;

procedure TParamDescription.SetDisplayLabel(const Value: string);
begin
  FDisplayLabel := Value;
end;

procedure TParamDescription.SetEditorConstraints(const Value: TSizeConstraints);
begin
  FEditorConstraints := Value;
end;

procedure TParamDescription.SetEnablerParamName(const Value: string);
begin
  FEnablerParamName := Value;
end;

procedure TParamDescription.SetGroup(const Value: string);
begin
  FGroup := Value;
end;

procedure TParamDescription.SetIndexInKeyFields(const Value: integer);
begin
  FIndexInKeyFields := Value;
end;

procedure TParamDescription.SetIndexInNameFields(const Value: integer);
begin
  FIndexInNameFields := Value;
end;

procedure TParamDescription.SetIndexInParentFields(const Value: integer);
begin
  FIndexInParentFields := Value;
end;

procedure TParamDescription.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TParamDescription.SetOrderNum(const Value: integer);
begin
  FOrderNum := Value;
end;

procedure TParamDescription.SetParamDirection(const Value: TParamDirection);
begin
  FParamDirection := Value;
end;

procedure TParamDescription.SetReadOnly(const Value: boolean);
begin
  FReadOnly := Value;
end;

procedure TParamDescription.SetRequired(const Value: boolean);
begin
  FRequired := Value;
end;

procedure TParamDescription.SetSourceParamName(const Value: string);
begin
  FSourceParamName := Value;
end;

procedure TParamDescription.SetVisible(const Value: boolean);
begin
  FVisible := Value;
end;

{ TBlockDescription }

procedure TBlockDescription.OnActionsValueChanged(Sender: TObject;
  const Item: TBlockAction;
  Action: Generics.Collections.TCollectionNotification);
begin
  SetLength(FSortedActions, 0);
end;

procedure TBlockDescription.OnParamsValueChanged(Sender: TObject; const Item: TParamDescription;
  Action: Generics.Collections.TCollectionNotification);
begin
  // чистим кэшированное
  SetLength(FSortedParams, 0);
  SetLength(FCallSortedParams, 0);
  FKeyFieldNames := '';
  FNameFieldNames := '';
  FParentFieldNames := '';
end;

constructor TBlockDescription.Create;
begin
  FParams := TObjectDictionary<string, TParamDescription>.Create();
  FParams.OnValueNotify := OnParamsValueChanged;
  FActions := TObjectDictionary<string, TBlockAction>.Create();
  FActions.OnValueNotify := OnActionsValueChanged;
  FBlockRefs := TObjectDictionary<integer, TBlockRef>.Create();
end;

destructor TBlockDescription.Destroy;
var
  O: TObject;
begin
  for O in FParams.Values do
    O.Free;
  FParams.Free;

  for O in FActions.Values do
    O.Free;
  FActions.Free;

  for O in FBlockRefs.Values do
    O.Free;
  FBlockRefs.Free;

  inherited;
end;

function TBlockDescription.FindParam(AParamName: string): TParamDescription;
var
  P: TParamDescription;
begin
  for P in Params.Values do
    if P.Name = AParamName then
    begin
      Result := P;
      Exit;
    end;
  Result := nil;
end;

function CompareByObject(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := Sign(integer(List.Objects[Index1]) - integer(List.Objects[Index2]));
end;

procedure TBlockDescription.FillKeyParentNameFields;
var
  P: TParamDescription;
  KeyFields: TStringList;
  NameFields: TStringList;
  ParentFields: TStringList;
begin
  if (FKeyFieldNames <> '') or (FParentFieldNames <> '') or (FNameFieldNames <> '') then
    Exit;
  KeyFields := TStringList.Create;
  NameFields := TStringList.Create;
  ParentFields := TStringList.Create;
  try
    for P in Params.Values do
      if P.ParamDirection = pdField then
      begin
        if P.IndexInKeyFields <> 0 then
          KeyFields.AddObject(P.Name, TObject(P.IndexInKeyFields));
        if P.IndexInNameFields <> 0 then
          NameFields.AddObject(P.Name, TObject(P.IndexInNameFields));
        if P.IndexInParentFields <> 0 then
          ParentFields.AddObject(P.Name, TObject(P.IndexInParentFields));
      end;
    KeyFields.CustomSort(CompareByObject);
    NameFields.CustomSort(CompareByObject);
    ParentFields.CustomSort(CompareByObject);
    FKeyFieldNames := MergeDelimitedString(KeyFields, ';');
    FNameFieldNames := MergeDelimitedString(NameFields, ';');
    FParentFieldNames := MergeDelimitedString(ParentFields, ';');
  finally
    KeyFields.Free;
    NameFields.Free;
    ParentFields.Free;
  end;

end;

function TBlockDescription.GetKeyFieldNames: string;
begin
  FillKeyParentNameFields;
  Result := FKeyFieldNames;
end;

function TBlockDescription.GetNameFieldNames: string;
begin
  FillKeyParentNameFields;
  Result := FNameFieldNames;
end;

function TBlockDescription.GetParentFieldNames: string;
begin
  FillKeyParentNameFields;
  Result := FParentFieldNames;
end;

function TBlockDescription.GetSortedActions: TArray<TBlockAction>;
begin
  if (Length(FSortedActions) = 0) and (FActions.Count > 0) then
  begin
    FSortedActions := FActions.Values.ToArray;
    TArray.Sort<TBlockAction>(FSortedActions, TActionsComparer.Create);
  end;
  Result := FSortedActions;
end;

function TBlockDescription.GetSortedParams: TArray<TParamDescription>;
begin
  if (Length(FSortedParams) = 0) and (Params.Count > 0) then
  begin
    FSortedParams := Params.Values.ToArray;
    TArray.Sort<TParamDescription>(FSortedParams, TParamsComparer.Create);
  end;
  Result := FSortedParams;
end;

function TBlockDescription.GetCallSortedParams: TArray<TParamDescription>;
begin
  if (Length(FCallSortedParams) = 0) and (Params.Count > 0) then
  begin
    FCallSortedParams := Params.Values.ToArray;
    TArray.Sort<TParamDescription>(FCallSortedParams, TParamsCallComparer.Create);
  end;
  Result := FCallSortedParams;
end;

function TBlockDescription.GetIsDataSet: boolean;
begin
  Result := KeyFieldNames <> '';
end;

function TBlockDescription.GetIsModal: boolean;
begin
  Result := FIsModal or (not IsDataSet);
end;

function TBlockDescription.GetIsTree: boolean;
begin
  Result := IsDataSet and (ParentFieldNames <> '');
end;

function TBlockDescription.ParamByName(AParamName: string): TParamDescription;
begin
  Result := FindParam(AParamName);
  if not Assigned(Result) then
    raise Exception.CreateFmt('Параметр "%s" не найден', [AParamName]);
end;

procedure TBlockDescription.SetChildId(const Value: integer);
begin
  FChildId := Value;
end;

procedure TBlockDescription.SetCustomClassName(const Value: string);
begin
  FCustomClassName := Value;
end;

procedure TBlockDescription.SetDisplayLabel(const Value: string);
begin
  FDisplayLabel := Value;
end;

procedure TBlockDescription.SetIndexOnPanel(const Value: integer);
begin
  FIndexOnPanel := Value;
end;

procedure TBlockDescription.SetIsModal(const Value: boolean);
begin
  FIsModal := Value;
end;

procedure TBlockDescription.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TBlockDescription.SetPanelId(const Value: integer);
begin
  FPanelId := Value;
end;

procedure TBlockDescription.SetParamGroupsDrawDirection(
  const Value: TDrawDirection);
begin
  FParamGroupsDrawDirection := Value;
end;

procedure TBlockDescription.SetParamsDrawDirection(const Value: TDrawDirection);
begin
  FParamsDrawDirection := Value;
end;

procedure TBlockDescription.SetVisible(const Value: boolean);
begin
  FVisible := Value;
end;

procedure TBlockDescription.Validate;
var
  S: TStrings;
  LastGroup: string;
  EmptyCount, NonEmptyCount: integer;
  P: TParamDescription;
  EnablerParam: TParamDescription;
  A: TBlockAction;
  DblClickActionFound: boolean;
  R: TBlockRef;
  SourceParam: string;
begin
  S := TStringList.Create;
  try
    LastGroup := '';
    for P in SortedParams do
    begin
      if (P.FParamDirection in [pdIn, pdOut,  pdInOut]) then
        if (P.Group <> P.Group) and (S.IndexOf(P.Group) <> -1) then
          raise Exception.CreateFmt('Группа %s встречается дважды не рядом в параметрах', [P.Group]);
      LastGroup := P.Group;
    end;

    S.Clear;
    LastGroup := '';
    for P in SortedParams do
    begin
      if (P.FParamDirection in [pdField]) then
        if (P.Group <> P.Group) and (S.IndexOf(P.Group) <> -1) then
          raise Exception.CreateFmt('Группа %s встречается дважды не рядом в параметрах', [P.Group]);
      LastGroup := P.Group;
    end;
  finally
    S.Free;
  end;

  EmptyCount := 0;
  NonEmptyCount := 0;
  for P in Params.Values do
    if P.ParamDirection in [pdIn, pdOut, pdInOut] then
      if P.Group = '' then
        Inc(EmptyCount)
      else
        Inc(NonEmptyCount);
  if (EmptyCount > 0) and (NonEmptyCount > 0) then
    raise Exception.Create('Есть параметры без группы и с группой в inout');

  EmptyCount := 0;
  NonEmptyCount := 0;
  for P in Params.Values do
    if P.ParamDirection in [pdField] then
      if P.Group = '' then
        Inc(EmptyCount)
      else
        Inc(NonEmptyCount);
  if (EmptyCount > 0) and (NonEmptyCount > 0) then
    raise Exception.Create('Есть параметры без группы и с группой в fields');

  for P in Params.Values do
    if P.EnablerParamName <> '' then
    begin
      EnablerParam := FindParam(P.EnablerParamName);
      if not Assigned(EnablerParam) then
        raise Exception.CreateFmt('EnablerParam не найден: %s %s', [P.Name, P.EnablerParamName]);
      if not (EnablerParam.ParamDirection in [pdIn, pdInOut]) then
        raise Exception.CreateFmt('У EnablerParam неверный тип: %s %s', [P.Name, P.EnablerParamName]);
    end;

  DblClickActionFound := false;
  for A in FActions.Values do
  begin
    for SourceParam in A.ParamBinds.Keys do
    begin
      if not Assigned(FindParam(SourceParam)) then
        raise Exception.CreateFmt('Не найден SourceParam %s в ссылке %s', [SourceParam, A.Caption]);
    end;
    if A.ActionStyle = asGridDblClick then
      if not DblClickActionFound then
        DblClickActionFound := true
      else
        raise Exception.Create('Найдено более одной ссылки типа asGridDblClick');
  end;

  for R in BlockRefs.Values do
  begin
    if R.MainParam = '' then
      raise Exception.CreateFmt('В ссылке %d нет главного параметра', [R.FID]);
    if R.Params.IndexOf(R.MainParam) = -1 then
      raise Exception.CreateFmt(
        'В ссылке %d главный параметр не из списка параметров',
        [R.FID]
      );
  end;

  // TODO: что еще хотел проверить?
end;

{ TProcedureDescription }

procedure TProcedureDescription.SetParamsToGridDirection(
  const Value: TDrawDirection);
begin
  FParamsToGridDirection := Value;
end;

procedure TProcedureDescription.SetProcedureName(const Value: string);
begin
  FProcedureName := Value;
end;

procedure TProcedureDescription.Validate;
var
  P: TParamDescription;
  FieldsFound: boolean;
  KeyFieldsList: TStrings;
  F: string;
begin
  inherited;
  if ProcedureName = '' then
    raise Exception.CreateFmt('Не указано имя процедуры в блоке %s', [DisplayLabel]);

  FieldsFound := false;
  for P in Params.Values do
    if P.ParamDirection = pdField then
    begin
      FieldsFound := true;
      Break;
    end;
  if (KeyFieldNames = '') and FieldsFound then
    raise Exception.CreateFmt('Нет ключевого поля, но есть поля в блоке %s', [DisplayLabel]);
  KeyFieldsList := TStringList.Create;
  try
    SplitDelimitedString(KeyFieldsList, KeyFieldNames, ';');
    for F in KeyFieldsList do
      if ParamByName(F).FParamDirection <> pdField then
        raise Exception.CreateFmt(
          'Ключевое поле %s не найдено или не является полем в блоке %s',
          [F, DisplayLabel]
        );
  finally
    KeyFieldsList.Free;
  end;

  if
    IsTree and
    (
      not Assigned(FindParam(ParentFieldNames)) or
      (FindParam(ParentFieldNames).ParamDirection <> pdField)
    )
  then
    raise Exception.CreateFmt('Включено дерево, но поле предка не найдено в блоке %s', [DisplayLabel]);
end;

{ TPanelDescription }

constructor TPanelDescription.Create;
begin
  FBlocks := TObjectList<TBlockDescription>.Create;
  FBlocks.OwnsObjects := false;
  FChildPanels := TObjectList<TPanelDescription>.Create;
  FChildPanels.OwnsObjects := false;
end;

destructor TPanelDescription.Destroy;
begin
  FBlocks.Free;
  FChildPanels.Free;
  inherited;
end;

procedure TPanelDescription.SetDisplayLabel(const Value: string);
begin
  FDisplayLabel := Value;
end;

procedure TPanelDescription.SetId(const Value: integer);
begin
  FId := Value;
end;

procedure TPanelDescription.SetIndexOnParent(const Value: integer);
begin
  FIndexOnParent := Value;
end;

procedure TPanelDescription.SetItemsDrawStyle(
  const Value: TPanelDrawDirection);
begin
  FItemsDrawStyle := Value;
end;

procedure TPanelDescription.SetParentId(const Value: integer);
begin
  FParentId := Value;
end;

{ TFormDescription }

constructor TFormDescription.Create;
begin
  inherited;
  FBlocks := TObjectDictionary<integer, TBlockDescription>.Create([]);
  FPanels := TObjectDictionary<integer, TPanelDescription>.Create([])
end;

destructor TFormDescription.Destroy;
var
  V: TObject;
begin
  // вручную, посколько Loader может удалять блоки без их освобождения
  for V in FBlocks.Values do
    V.Free;
  FBlocks.Free;

  for V in FPanels.Values do
    V.Free;
  FPanels.Free;

  inherited;
end;

function TFormDescription.GetIsModal: boolean;

function ModalBlockExists: boolean;
var
  Block: TBlockDescription;
begin
  for Block in Blocks.Values do
    if Block.GetIsModal then
    begin
      Result := true;
      Exit;
    end;
  Result := false;
end;

begin
  Result :=
    inherited GetIsModal or
    ModalBlockExists;
end;

procedure TFormDescription.SetHeaderVisible(const Value: boolean);
begin
  FHeaderVisible := Value;
end;

procedure TFormDescription.SetMainProcedureName(const Value: string);
begin
  FMainProcedureName := Value;
end;

procedure TFormDescription.SetRootPanel(const Value: TPanelDescription);
begin
  FRootPanel := Value;
end;

procedure TFormDescription.Validate;
var
  P: TParamDescription;
  Block: TBlockDescription;
begin
  inherited;
  for P in Params.Values do
  begin
    if (P.SourceBlockId <> 0) and (P.SourceParamName = '') then
      raise Exception.CreateFmt(
        'Параметр %s - задан исходный блок, но не задан исходный параметр',
        [P.Name]
      );
    if (P.SourceBlockId = 0) and (P.SourceParamName <> '') then
      raise Exception.CreateFmt(
        'Параметр %s - задан исходный параметр, но не задан исходный блок',
        [P.Name]
      );
  end;

  for Block in Blocks.Values do
    for P in Block.Params.Values do
      if P.SourceBlockId <> 0 then
      begin
        if not Blocks.ContainsKey(P.SourceBlockId) then
          raise Exception.CreateFmt(
            'Исходный блок %d параметра %s не найден в форме',
            [P.SourceBlockId, P.Name]
          );
        if not Blocks[P.SourceBlockId].IsDataSet then
          raise Exception.CreateFmt(
            'Исходный блок параметра %s (%d) - не DataSet',
            [P.Name, P.SourceBlockId]
          );
      end;
end;

{ TBlockAction }

constructor TBlockAction.Create;
begin
  FParamBinds := TDictionary<string, string>.Create();
end;

destructor TBlockAction.Destroy;
begin
  FParamBinds.Free;
  inherited;
end;

function TBlockAction.GetLinksTo: TFormDescription;
begin
  if not Assigned(FLinksTo) then
    FLinksTo := BlocksManager.Blocks[FLinksToName] as TFormDescription;
  Result := FLinksTo;
end;

procedure TBlockAction.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TBlockAction.SetImageIndex(const Value: integer);
begin
  FImageIndex := Value;
end;

procedure TBlockAction.SetRefreshMode(const Value: TRefreshMode);
begin
  FRefreshMode := Value;
end;

procedure TBlockAction.SetLinksToName(const Value: string);
begin
  FLinksToName := Value;
  FLinksTo := nil;
end;

procedure TBlockAction.SetActionStyle(const Value: TBlockActionStyle);
begin
  FActionStyle := Value;
end;

procedure TBlockAction.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TBlockAction.SetOrderNum(const Value: integer);
begin
  FOrderNum := Value;
end;

{ TBlocksManager }

constructor TBlocksManager.Create;
begin
  if Assigned(FBlocksManager) then
    raise Exception.Create('Block manager already created! One instance allowed only.');
  FBlocks := TObjectDictionary<string, TBlockDescription>.Create();
end;

destructor TBlocksManager.Destroy;
var
  B: TBlockDescription;
begin
  for B in FBlocks.Values do
    B.Free;
  FBlocks.Free;

  inherited;
end;

procedure AddParamValues(Source, Destination: TParamValues);
var
  PV: string;
begin
  for PV in Source.Keys do
    Destination.AddOrSetValue(PV, Source[PV]);
end;

{ TBlockRef }

constructor TBlockRef.Create;
begin
  FParams := TStringList.Create;
  FBinds := TObjectDictionary<string, TBlockRefBind>.Create();
end;

destructor TBlockRef.Destroy;
var
  V: TObject;
begin
  FParams.Free;

  for V in FBinds.Values do
    V.Free;
  FBinds.Free;
  inherited;
end;

procedure TBlockRef.SetID(const Value: integer);
begin
  FID := Value;
end;

procedure TBlockRef.SetMainParam(const Value: string);
begin
  FMainParam := Value;
end;

procedure TBlockRef.SetRefsTo(const Value: string);
begin
  FRefsTo := Value;
end;

procedure TBlockRef.SetParams(const Value: TStrings);
begin
  FParams := Value;
end;

{ TParamsComparer<T> }

function TParamsComparer.Compare(const Left, Right: TParamDescription): Integer;
begin
  Result := Sign(Left.OrderNum - Right.OrderNum);
end;

{ TActionsComparer }

function TActionsComparer.Compare(const Left, Right: TBlockAction): Integer;
begin
  Result := Sign(Left.OrderNum - Right.OrderNum);
end;

{ TParamsCallComparer }

function TParamsCallComparer.Compare(const Left,
  Right: TParamDescription): Integer;
begin
  Result := Sign(Left.CallOrderNum - Right.CallOrderNum);
end;

{ TBlockRefBind }

constructor TBlockRefBind.Create(ASourceBlockId: integer; ASourceParam,
  ADestinationParam: string);
begin
  SourceParam := ASourceParam;
  SourceBlockId := ASourceBlockId;
  DestinationParam := ADestinationParam;
end;

end.
