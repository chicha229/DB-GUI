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
    FCallOrderNum: integer;
    FDefaultValue: string;

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
    procedure SetCallOrderNum(const Value: integer);
    procedure SetDefaultValue(const Value: string);
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
    property CallOrderNum: integer read FCallOrderNum write SetCallOrderNum;
    property DefaultValue: string read FDefaultValue write SetDefaultValue;

    // ��� ������ ������-������ ������ �����
    // � �������� ���������� ����� � ���������� �� ������
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
    // 0 ��� ������ �� ���������, � ������� ��� child ������
    SourceBlockId: integer;

    constructor Create(ASourceBlockId: integer; ASourceParam, ADestinationParam: string);
  end;

  // ������ �� ���������� �����
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
    // ���� - DestinationParam
    property Binds: TObjectDictionary<string, TBlockRefBind> read FBinds;
    property MainParam: string read FMainParam write SetMainParam;
  end;

  TBlockDescription = class;

  TBlockAction = class (TObject)
  private
    FLinksToName: string;
    FLinksTo: TBlockDescription;
    FActionStyle: TBlockActionStyle;
    FParamBinds: TDictionary<string, string>;
    FCaption: string;
    FRefreshMode: TRefreshMode;
    FImageIndex: integer;
    FName: string;
    FOrderNum: integer;
    FShortCut: TShortCut;
    procedure SetLinksToName(const Value: string);
    procedure SetActionStyle(const Value: TBlockActionStyle);
    procedure SetCaption(const Value: string);
    procedure SetRefreshMode(const Value: TRefreshMode);
    function GetLinksTo: TBlockDescription;
    procedure SetImageIndex(const Value: integer);
    procedure SetName(const Value: string);
    procedure SetOrderNum(const Value: integer);
    procedure SetShortCut(const Value: TShortCut);
  public
    constructor Create;
    destructor Destroy; override;

    property Name: string read FName write SetName;
    property Caption: string read FCaption write SetCaption;
    property OrderNum: integer read FOrderNum write SetOrderNum;

    property ActionStyle: TBlockActionStyle read FActionStyle write SetActionStyle;
    property RefreshMode: TRefreshMode read FRefreshMode write SetRefreshMode;

    property LinksToName: string read FLinksToName write SetLinksToName;
    property LinksTo: TBlockDescription read GetLinksTo;
    property ParamBinds: TDictionary<string, string> read FParamBinds;
    property ImageIndex: integer read FImageIndex write SetImageIndex;
    property ShortCut: TShortCut read FShortCut write SetShortCut;
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
    FValidateErrors: TStrings;

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
  protected
    procedure AddValidationError(AErrorText: string);
    procedure AddValidationErrorFmt(AErrorTextFmt: string; AArgs: array of const);

    procedure ValidateInternal; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetIsModal: boolean; virtual;

    property DisplayLabel: string read FDisplayLabel write SetDisplayLabel;
    property Name: string read FName write SetName;

    // ����������� �����
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

    procedure Validate;
    procedure ClearCaches;
  end;

  TBlocksManager = class (TObject)
  private
    FBlocks: TObjectDictionary<string, TBlockDescription>;
  public
    constructor Create;
    destructor Destroy; override;

    property Blocks: TObjectDictionary<string, TBlockDescription> read FBlocks;
  end;

  TGridStyle = (gsRows, gsColumns);

  // ���� ���� ��������� - � ����������, �������� ������������ �������
  TProcedureDescription = class (TBlockDescription)
  private
    FProcedureName: string;
    FParamsToGridDirection: TDrawDirection;
    FProcedureOwner: string;
    FForceSave: boolean;
    FPackageName: string;
    FGridStyle: TGridStyle;

    procedure SetParamsToGridDirection(const Value: TDrawDirection);
    procedure SetProcedureName(const Value: string);
    procedure SetProcedureOwner(const Value: string);
    procedure SetForceSave(const Value: boolean);
    procedure SetPackageName(const Value: string);
    procedure SetGridStyle(const Value: TGridStyle);
  protected
    procedure ValidateInternal; override;
  public

    property ProcedureOwner: string read FProcedureOwner write SetProcedureOwner;
    property PackageName: string read FPackageName write SetPackageName;
    property ProcedureName: string read FProcedureName write SetProcedureName;

    property ParamsToGridDirection: TDrawDirection read FParamsToGridDirection write SetParamsToGridDirection;
    property ForceSave: boolean read FForceSave write SetForceSave;
    property GridStyle: TGridStyle read FGridStyle write SetGridStyle;
  end;

  // ������ ��� ����������� ������
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

  // �������� �����
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
  protected
    procedure ValidateInternal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

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
  FActionsComparer: TActionsComparer;
  FParamsComparer: TParamsComparer;
  FParamsCallComparer: TParamsCallComparer;

function ActionsComparer: TActionsComparer;
begin
  if not Assigned(FActionsComparer) then
    FActionsComparer := TActionsComparer.Create;
  Result := FActionsComparer;
end;

function ParamsComparer: TParamsComparer;
begin
  if not Assigned(FParamsComparer) then
    FParamsComparer := TParamsComparer.Create;
  Result := FParamsComparer;
end;

function ParamsCallComparer: TParamsCallComparer;
begin
  if not Assigned(FParamsCallComparer) then
    FParamsCallComparer := TParamsCallComparer.Create;
  Result := FParamsCallComparer;
end;

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

procedure TParamDescription.SetCallOrderNum(const Value: integer);
begin
  FCallOrderNum := Value;
end;

procedure TParamDescription.SetDataType(const Value: TFieldType);
begin
  FDataType := Value;
end;

procedure TParamDescription.SetDefaultValue(const Value: string);
begin
  FDefaultValue := Value;
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
  // ������ ������������
  SetLength(FSortedParams, 0);
  SetLength(FCallSortedParams, 0);
  FKeyFieldNames := '';
  FNameFieldNames := '';
  FParentFieldNames := '';
end;

procedure TBlockDescription.AddValidationError(AErrorText: string);
begin
  FValidateErrors.Add(AErrorText);
end;

procedure TBlockDescription.AddValidationErrorFmt(AErrorTextFmt: string;
  AArgs: array of const);
begin
  AddValidationError(Format(AErrorTextFmt, AArgs));
end;

procedure TBlockDescription.ClearCaches;
begin
  SetLength(FSortedActions, 0);
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
  FValidateErrors := TStringList.Create;

  FParamsDrawDirection := ddVertical;
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

  FValidateErrors.Free;

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
    TArray.Sort<TBlockAction>(FSortedActions, ActionsComparer);
  end;
  Result := FSortedActions;
end;

function TBlockDescription.GetSortedParams: TArray<TParamDescription>;
begin
  if (Length(FSortedParams) = 0) and (Params.Count > 0) then
  begin
    FSortedParams := Params.Values.ToArray;
    TArray.Sort<TParamDescription>(FSortedParams, ParamsComparer);
  end;
  Result := FSortedParams;
end;

function TBlockDescription.GetCallSortedParams: TArray<TParamDescription>;
begin
  if (Length(FCallSortedParams) = 0) and (Params.Count > 0) then
  begin
    FCallSortedParams := Params.Values.ToArray;
    TArray.Sort<TParamDescription>(FCallSortedParams, ParamsCallComparer);
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
    raise Exception.CreateFmt('�������� "%s" �� ������', [AParamName]);
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
begin
  FValidateErrors.Clear;
  try
    ValidateInternal;
  except
    on Err: Exception do
      raise Exception.CreateFmt(
        '������ �� ����� ��������� ����� %s: %s %s %s %s',
        [Self.Name, CrLf, Err.Message, CrLf, FValidateErrors.Text]
      );
  end;
  if FValidateErrors.Count > 0 then
    raise Exception.CreateFmt(
      '������ ��������� ����� %s: %s %s',
      [Self.Name, CrLf, FValidateErrors.Text]
    );
end;

procedure TBlockDescription.ValidateInternal;
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
  RefBlock: TBlockDescription;
  RefFieldsCount: integer;
begin
  S := TStringList.Create;
  try
    LastGroup := '';
    for P in SortedParams do
    begin
      if (P.FParamDirection in [pdIn, pdOut,  pdInOut]) then
        if (P.Group <> LastGroup) and (S.IndexOf(P.Group) <> -1) then
          AddValidationErrorFmt('������ %s ����������� ������ �� ����� � ����������', [P.Group]);
      if P.Group <> '' then
        S.Add(P.Group);
      LastGroup := P.Group;
    end;

    S.Clear;
    LastGroup := '';
    for P in SortedParams do
    begin
      if (P.FParamDirection in [pdField]) then
        if (P.Group <> LastGroup) and (S.IndexOf(P.Group) <> -1) then
          AddValidationErrorFmt('������ %s ����������� ������ �� ����� � ����������', [P.Group]);
      if P.Group <> '' then
        S.Add(P.Group);
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
    AddValidationError('���� ��������� ��� ������ � � ������� � inout');

  EmptyCount := 0;
  NonEmptyCount := 0;
  for P in Params.Values do
    if P.ParamDirection in [pdField] then
      if P.Group = '' then
        Inc(EmptyCount)
      else
        Inc(NonEmptyCount);
  if (EmptyCount > 0) and (NonEmptyCount > 0) then
    AddValidationError('���� ��������� ��� ������ � � ������� � fields');

  for P in Params.Values do
    if P.EnablerParamName <> '' then
    begin
      EnablerParam := FindParam(P.EnablerParamName);
      if not Assigned(EnablerParam) then
        AddValidationErrorFmt('EnablerParam �� ������: %s %s', [P.Name, P.EnablerParamName]);
      if not (EnablerParam.ParamDirection in [pdIn, pdInOut]) then
        AddValidationErrorFmt('� EnablerParam �������� ���: %s %s', [P.Name, P.EnablerParamName]);
    end;

  DblClickActionFound := false;
  for A in FActions.Values do
  begin
    for SourceParam in A.ParamBinds.Keys do
    begin
      if not Assigned(FindParam(SourceParam)) then
        AddValidationErrorFmt('�� ������ SourceParam %s � ������ %s', [SourceParam, A.Caption]);
      if
        Assigned(A.LinksTo) and
        (not A.LinksTo.Params.ContainsKey(A.ParamBinds[SourceParam]))
      then
        AddValidationErrorFmt(
          '�� ������ DestinationParam %s � ������ %s',
          [A.ParamBinds[SourceParam], A.Caption]
        );
      if
        Assigned(A.LinksTo) and
        (A.LinksTo.Params.ContainsKey(A.ParamBinds[SourceParam])) and
        (not (A.LinksTo.Params[A.ParamBinds[SourceParam]].ParamDirection in [pdIn, pdInOut]))
      then
        AddValidationErrorFmt(
          'DestinationParam %s � ������ %s �� in ��� inout',
          [A.ParamBinds[SourceParam], A.Caption]
        );
    end;
    if A.ActionStyle = asGridDblClick then
      if not DblClickActionFound then
        DblClickActionFound := true
      else
        AddValidationError('������� ����� ����� ������ ���� asGridDblClick');
  end;

  for R in BlockRefs.Values do
  begin
    if R.MainParam = '' then
      AddValidationErrorFmt('� ������ %d ��� �������� ���������', [R.FID]);
    if R.Params.IndexOf(R.MainParam) = -1 then
      AddValidationErrorFmt(
        '� ������ %d ������� �������� �� �� ������ ����������',
        [R.FID]
      );

    RefBlock := BlocksManager.Blocks[R.RefsTo];
    RefFieldsCount := 0;
    for P in RefBlock.Params.Values do
      if P.IndexInKeyFields <> 0 then
        Inc(RefFieldsCount);
    if RefFieldsCount <> R.Params.Count then
      AddValidationErrorFmt(
        '���������� ����� � ������ %d �� ��������� � ����������� ����� � �����',
        [R.FID]
      )
    else
      for P in RefBlock.Params.Values do
        if P.IndexInKeyFields <> 0 then
        begin
          if P.DataType <> Params[R.Params[P.IndexInKeyFields - 1]].DataType then
            AddValidationErrorFmt(
              '��� ��������� � ������ � �� �� ���������: %s %s',
              [R.Params[P.IndexInKeyFields - 1], P.Name]
            );
        end;
  end;
end;

{ TProcedureDescription }

procedure TProcedureDescription.SetForceSave(const Value: boolean);
begin
  FForceSave := Value;
end;

procedure TProcedureDescription.SetGridStyle(const Value: TGridStyle);
begin
  FGridStyle := Value;
end;

procedure TProcedureDescription.SetPackageName(const Value: string);
begin
  FPackageName := Value;
end;

procedure TProcedureDescription.SetParamsToGridDirection(
  const Value: TDrawDirection);
begin
  FParamsToGridDirection := Value;
end;

procedure TProcedureDescription.SetProcedureName(const Value: string);
begin
  FProcedureName := Value;
end;

procedure TProcedureDescription.SetProcedureOwner(const Value: string);
begin
  FProcedureOwner := Value;
end;

procedure TProcedureDescription.ValidateInternal;
var
  P: TParamDescription;
  FieldsFound: boolean;
  KeyFieldsList: TStrings;
  F: string;
  CursorParamsCount: integer;
begin
  inherited;
  if ProcedureName = '' then
    AddValidationError('�� ������� ��� ���������');

  FieldsFound := false;
  for P in Params.Values do
    if P.ParamDirection = pdField then
    begin
      FieldsFound := true;
      Break;
    end;
  if (KeyFieldNames = '') and FieldsFound then
    AddValidationError('��� ��������� ����, �� ���� ����');
  KeyFieldsList := TStringList.Create;
  try
    SplitDelimitedString(KeyFieldsList, KeyFieldNames, ';');
    for F in KeyFieldsList do
      if ParamByName(F).FParamDirection <> pdField then
        AddValidationErrorFmt(
          '�������� ���� %s �� ������� ��� �� �������� �����',
          [F]
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
    AddValidationError('�������� ������, �� ���� ������ �� �������');

  CursorParamsCount := 0;
  for P in Params.Values do
    if P.ParamDirection = pdCursor then
      Inc(CursorParamsCount);
  if CursorParamsCount > 1 then
    AddValidationError('���������� ���������� - �������� > 1');
  if (CursorParamsCount = 1) and (not IsDataSet) then
    AddValidationError('���� ��������-������, �� ��������� - �� �������');
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
  // �������, ��������� Loader ����� ������� ����� ��� �� ������������
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
    FIsModal or
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

procedure TFormDescription.ValidateInternal;
var
  P: TParamDescription;
  Block: TBlockDescription;
  SourceBlock: TBlockDescription;
  R: TBlockRef;
  B: TBlockRefBind;
begin
  inherited;
  for P in Params.Values do
    if (P.SourceBlockId <> 0) and (P.SourceParamName = '') then
      AddValidationErrorFmt(
        '�������� %s - ����� �������� ����, �� �� ����� �������� ��������',
        [P.Name]
      );

  for Block in Blocks.Values do
  begin
    for P in Block.Params.Values do
    begin
      if P.SourceParamName = '' then
        Continue;
      if P.ParamDirection in [pdCursor] then
        AddValidationErrorFmt(
          '�������� %s - ����� ��������, �� ��� cursor',
          [P.Name]
        );
      if P.SourceBlockId <> 0 then
      begin
        if not Blocks.ContainsKey(P.SourceBlockId) then
        begin
          AddValidationErrorFmt(
            '�������� ���� %d ��������� %s �� ������ � �����',
            [P.SourceBlockId, P.Name]
          );
          SourceBlock := nil;
        end
        else
        begin
          if not Blocks[P.SourceBlockId].IsDataSet then
            AddValidationErrorFmt(
              '�������� ���� ��������� %s (%d) - �� DataSet',
              [P.Name, P.SourceBlockId]
            );
          SourceBlock := Blocks[P.SourceBlockId];
        end;
      end
      else
        SourceBlock := Self;
      if
        Assigned(SourceBlock) and
        (not Assigned(SourceBlock.FindParam(P.SourceParamName)))
      then
        AddValidationErrorFmt(
          '�������� ���� ��������� %s (%s) �� ������',
          [P.Name, P.SourceParamName]
        );
    end;

    for R in Block.BlockRefs.Values do
      for B in R.Binds.Values do
        if B.SourceParam <> '' then
        begin
          if B.SourceBlockId = 0 then
            SourceBlock := Self
          else
          begin
            if not Blocks.ContainsKey(B.SourceBlockId) then
            begin
              AddValidationErrorFmt(
                '�������� ���� %d ������ %d ��������� %s �� ������ � �����',
                [B.SourceBlockId, R.ID, B.SourceParam]
              );
            SourceBlock := nil;
            end
            else
              SourceBlock := Blocks[B.SourceBlockId];
          end;
          if
            Assigned(SourceBlock) and
            (not Assigned(SourceBlock.FindParam(B.SourceParam)))
          then
            AddValidationErrorFmt(
              '�������� �������� %s ������ %d �� ������',
              [B.SourceParam, R.ID]
            );
        end;
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

function TBlockAction.GetLinksTo: TBlockDescription;
begin
  if not Assigned(FLinksTo) then
    FLinksTo := BlocksManager.Blocks[FLinksToName];
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

procedure TBlockAction.SetShortCut(const Value: TShortCut);
begin
  FShortCut := Value;
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
  Result := Sign(Left.FCallOrderNum - Right.FCallOrderNum);
end;

{ TBlockRefBind }

constructor TBlockRefBind.Create(ASourceBlockId: integer; ASourceParam,
  ADestinationParam: string);
begin
  SourceParam := ASourceParam;
  SourceBlockId := ASourceBlockId;
  DestinationParam := ADestinationParam;
end;

initialization
  ;

finalization
begin
  FBlocksManager.Free;
  FActionsComparer.Free;
  FParamsComparer.Free;
  FParamsCallComparer.Free;
end;

end.
