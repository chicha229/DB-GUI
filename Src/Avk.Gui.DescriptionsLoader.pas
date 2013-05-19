unit Avk.Gui.DescriptionsLoader;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  uADStanIntf, uADStanOption, uADStanParam,
  uADStanError, uADDatSManager, uADPhysIntf, uADDAptIntf, uADStanAsync,
  uADDAptManager, Data.DB, uADCompDataSet, uADCompClient,
  Avk.Gui.Descriptions;

type
  TDescriptionsLoaderDM = class (TDataModule)
    BlocksQuery: TADQuery;
    ProceduresQuery: TADQuery;
    FormsQuery: TADQuery;
    BlockParamsQuery: TADQuery;
    BlockActionsQuery: TADQuery;
    ActionBindsQuery: TADQuery;
    FormChildsQuery: TADQuery;
    FormChildParamsQuery: TADQuery;
    FormPanelsQuery: TADQuery;
    BlockRefParamsQuery: TADQuery;
    BlockRefBindsQuery: TADQuery;
    ChildRefBindsQuery: TADQuery;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    FLoadedBlocks: TObjectList<TBlockDescription>;
    FLoadedChildBlocks: TObjectList<TBlockDescription>;
    FLoadedParams: TObjectList<TParamDescription>;
    FLoadedBlockActions: TObjectList<TBlockAction>;
    FLoadedPanels: TObjectList<TPanelDescription>;
    FLoadedParamRefs: TObjectList<TBlockRef>;
    FLoadedBlockRefBinds: TObjectList<TBlockRefBind>;

    FTempProcedureDescription: TProcedureDescription;

    procedure InternalExecute(AConnection: TADConnection);
    procedure LoadBlock(AID: string; ADescription: TBlockDescription);
    procedure LoadForm(AID: string; ADescription: TFormDescription);
    procedure LoadProcedure(AID: string; ADescription: TProcedureDescription);
    procedure FillBlockDescription(B: TBlockDescription);

    function CreateBlock: TBlockDescription;
    procedure FillRefBinds(BlockRef: TBlockRef);
    procedure FillChildRefBinds(ChildId: integer; BlockRef: TBlockRef);
  public
    { Public declarations }
    class procedure Execute(AConnection: TADConnection);
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
  System.Variants,
  Avk.Gui.CustomMainDM, Avk.Core.Helpers;

const
  cParamDirectionNames: array[TParamDirection] of string = (
    'in', 'out', 'in_out', 'field', 'cursor'
  );

  cBlockActionStyles: array[TBlockActionStyle] of string = (
    'button', 'dbl_click'
  );

  cDrawDirections: array[TDrawDirection] of string = (
    'horizontal', 'vertical', 'tabs'
  );

  cPanelDrawDirections: array[TPanelDrawDirection] of string = (
    'left', 'right', 'top', 'bottom', 'tabs'
  );

  cRefreshModeNames: array[TRefreshMode] of string = (
    'insert', 'update', 'delete', 'full', 'none'
  );

{ TDescriptionsLoaderDM }

function GetParamDirectionByName(AName: string): TParamDirection;
var
  PD: TParamDirection;
begin
  for PD := Low(TParamDirection) to High(TParamDirection) do
    if cParamDirectionNames[PD] = AName then
    begin
      Result := PD;
      Exit;
    end;
  raise Exception.CreateFmt('Тип параметра "%s" не найден', [AName]);
end;

function GetFieldTypeByName(AName: string): TFieldType;
begin
  if AName = 'date' then
    Result := ftDateTime
  else if AName = 'integer' then
    Result := ftInteger
  else if AName = 'numeric' then
    Result := ftFloat
  else if AName = 'timestamp' then
    Result := ftTimeStamp
  else if AName = 'varchar' then
    Result := ftString
  else if AName = 'boolean' then
    Result := ftBoolean
  else
    raise Exception.CreateFmt('Тип данных "%s" не найден', [AName]);
end;

function GetActionStyleByName(AName: string): TBlockActionStyle;
var
  S: TBlockActionStyle;
begin
  for S := Low(TBlockActionStyle) to High(TBlockActionStyle) do
    if cBlockActionStyles[S] = AName then
    begin
      Result := S;
      Exit;
    end;
  raise Exception.CreateFmt('Тип ссылки "%s" не найден', [AName]);
end;

function GetDrawDirectionByName(AName: string): TDrawDirection;
var
  DD: TDrawDirection;
begin
  for DD := Low(TDrawDirection) to High(TDrawDirection) do
    if cDrawDirections[DD] = AName then
    begin
      Result := DD;
      Exit;
    end;
  raise Exception.CreateFmt('Тип направления (DrawDirection) "%s" не найден', [AName]);
end;

function GetPanelDrawDirectionByName(AName: string): TPanelDrawDirection;
var
  DD: TPanelDrawDirection;
begin
  for DD := Low(TPanelDrawDirection) to High(TPanelDrawDirection) do
    if cPanelDrawDirections[DD] = AName then
    begin
      Result := DD;
      Exit;
    end;
  raise Exception.CreateFmt('Тип направления (PanelDrawDirection) "%s" не найден', [AName]);
end;


function GetRefreshModeByName(AName: string): TRefreshMode;
var
  V: TRefreshMode;
begin
  for V := Low(TRefreshMode) to High(TRefreshMode) do
    if cRefreshModeNames[V] = AName then
    begin
      Result := V;
      Exit;
    end;
  raise Exception.CreateFmt('Способ обновления (TRefreshMode) "%s" не найден', [AName]);
end;


function TDescriptionsLoaderDM.CreateBlock: TBlockDescription;
var
  BlockType: string;
begin
  BlockType := BlocksQuery.FieldByName('block_type').AsString;
  if BlockType = 'form' then
    Result := TFormDescription.Create
  else if BlockType = 'procedure' then
    Result := TProcedureDescription.Create
  else
    raise Exception.CreateFmt(
      'Неизвестный тип блока: %s',
      [BlockType]
    );
end;


procedure TDescriptionsLoaderDM.FillBlockDescription(B: TBlockDescription);
var
  P: TParamDescription;
  BA: TBlockAction;
  LastLinkGroup: integer;
  BlockRef: TBlockRef;
  ParamName, ActionName: string;
  RefID: integer;
begin
  B.Name := BlocksQuery.FieldByName('id').AsString;
  B.DisplayLabel := BlocksQuery.FieldByName('name').AsString;
  B.CustomClassName := BlocksQuery.FieldByName('custom_class').AsString;
  B.IsModal := (BlocksQuery.FieldByName('is_modal').AsInteger = 1);

  // параметры блока
  FLoadedParams.Clear;
  BlockParamsQuery.Locate('block', B.Name, []);
  while
    (BlockParamsQuery.FieldByName('block').AsString = B.Name) and
    (not BlockParamsQuery.EOF)
  do
  begin
    P := B.FindParam(BlockParamsQuery.FieldByName('param').AsString);
    if not Assigned(P) then
    begin
      P := TParamDescription.Create;
      P.Name := BlockParamsQuery.FieldByName('param').AsString;
      B.Params.Add(P.Name, P);
    end;

    P.DisplayLabel := BlockParamsQuery.FieldByName('caption').AsString;
    P.DataType := GetFieldTypeByName(BlockParamsQuery.FieldByName('data_type').AsString);
    P.ParamDirection := GetParamDirectionByName(BlockParamsQuery.FieldByName('param_direction').AsString);
    P.Visible := BlockParamsQuery.FieldByName('visible').AsInteger <> 0;
    P.Required := BlockParamsQuery.FieldByName('required').AsInteger <> 0;
    P.ReadOnly := BlockParamsQuery.FieldByName('read_only').AsInteger <> 0;
    P.IndexInKeyFields := BlockParamsQuery.FieldByName('index_in_key').AsInteger;
    P.IndexInNameFields := BlockParamsQuery.FieldByName('index_in_name').AsInteger;
    P.IndexInParentFields := BlockParamsQuery.FieldByName('index_in_parent').AsInteger;
    P.Group := BlockParamsQuery.FieldByName('group_name').AsString;
    P.EnablerParamName := BlockParamsQuery.FieldByName('enabler_param').AsString;
    P.SourceBlockId := BlockParamsQuery.FieldByName('source_child').AsInteger;
    P.SourceParamName := BlockParamsQuery.FieldByName('source_param').AsString;
    P.OrderNum := BlockParamsQuery.FieldByName('order_num').AsInteger;
    P.CallOrderNum := BlockParamsQuery.FieldByName('call_order_num').AsInteger;

    FLoadedParams.Add(P);
    BlockParamsQuery.Next;
  end;

  // выкидываем удаленные параметры
  for ParamName in B.Params.Keys do
    if FLoadedParams.IndexOf(B.Params[ParamName]) = -1 then
      B.Params.Remove(ParamName);

  // действия блока
  FLoadedBlockActions.Clear;
  BlockActionsQuery.Locate('block', B.Name, []);
  while
    (BlockActionsQuery.FieldByName('block').AsString = B.Name) and
    (not BlockActionsQuery.EOF)
  do
  begin
    if not B.Actions.TryGetValue(BlockActionsQuery.FieldByName('id').AsString, BA) then
    begin
      BA := TBlockAction.Create;
      BA.Name := BlockActionsQuery.FieldByName('id').AsString;
      B.Actions.Add(BA.Name, BA);
    end;

    BA.Caption := BlockActionsQuery.FieldByName('caption').AsString;
    BA.OrderNum := BlockActionsQuery.FieldByName('order_num').AsInteger;
    BA.LinksToName := BlockActionsQuery.FieldByName('links_to').AsString;
    if BlockActionsQuery.FieldByName('image_index').IsNull then
      BA.ImageIndex := -1
    else
      BA.ImageIndex := BlockActionsQuery.FieldByName('image_index').AsInteger;
    BA.ActionStyle := GetActionStyleByName(BlockActionsQuery.FieldByName('action_style').AsString);
    BA.RefreshMode := GetRefreshModeByName(BlockActionsQuery.FieldByName('refresh_mode').AsString);

    BA.ParamBinds.Clear;
    ActionBindsQuery.Locate('block;action', VarArrayOf([B.Name, BA.Name]), []);
    while
      (ActionBindsQuery.FieldByName('block').AsString = B.Name) and
      (ActionBindsQuery.FieldByName('action').AsString = BA.Name) and
      (not ActionBindsQuery.EOF)
    do
    begin
      BA.ParamBinds.Add(
        ActionBindsQuery.FieldByName('param').AsString,
        ActionBindsQuery.FieldByName('destination_param').AsString
      );
      ActionBindsQuery.Next;
    end;
    FLoadedBlockActions.Add(BA);
    BlockActionsQuery.Next;
  end;

  for ActionName in B.Actions.Keys do
    if FLoadedBlockActions.IndexOf(B.Actions[ActionName]) = -1 then
      B.Actions.Remove(ActionName);

  // ссылки из блока, для редактирования
  FLoadedParamRefs.Clear;
  LastLinkGroup := 0;
  BlockRefParamsQuery.Locate('block', B.Name, []);
  while
    (BlockRefParamsQuery.FieldByName('block').AsString = B.Name) and
    (not BlockRefParamsQuery.EOF)
  do
  begin
    if BlockRefParamsQuery.FieldByName('ref').AsInteger <> LastLinkGroup then
    begin
      if not B.BlockRefs.TryGetValue(BlockRefParamsQuery.FieldByName('ref').AsInteger, BlockRef) then
      begin
        BlockRef := TBlockRef.Create;
        BlockRef.ID := BlockRefParamsQuery.FieldByName('ref').AsInteger;
        B.BlockRefs.Add(BlockRef.ID, BlockRef);
      end;
      BlockRef.RefsTo := BlockRefParamsQuery.FieldByName('refs_to').AsString;
      BlockRef.Params.Clear;
      BlockRef.MainParam := '';
      LastLinkGroup := BlockRef.ID;
      FLoadedParamRefs.Add(BlockRef);

      // для формы бинды у ссылок свои
      if not (B is TFormDescription) then
        FillRefBinds(BlockRef);
    end;
    BlockRef.Params.Add(BlockRefParamsQuery.FieldByName('param').AsString);
    if BlockRefParamsQuery.FieldByName('is_main_param').AsInteger <> 0 then
      if BlockRef.MainParam <> '' then
        raise Exception.Create('У ссылки %d более одного главного параметра')
      else
        BlockRef.MainParam := BlockRefParamsQuery.FieldByName('param').AsString;
    BlockRefParamsQuery.Next;
  end;

  for RefID in B.BlockRefs.Keys do
    if FLoadedParamRefs.IndexOf(B.BlockRefs[RefID]) = -1 then
      B.BlockRefs.Remove(RefId);
end;

procedure TDescriptionsLoaderDM.LoadProcedure(AID: string; ADescription: TProcedureDescription);
var
  ProcedureName: string;
begin
  ProcedureName := BlocksQuery.FieldByName('id').AsString;
  if not ProceduresQuery.Locate('id', ProcedureName, []) then
    raise Exception.CreateFmt('Описание процедуры не найдено: %s', [ProcedureName]);
  FillBlockDescription(ADescription);
  ADescription.ProcedureName := ProceduresQuery.FieldByName('procedure_name').AsString;
end;

procedure TDescriptionsLoaderDM.LoadForm(AID: string; ADescription: TFormDescription);
var
  FormName: string;
  F: TFormDescription;
  C: TBlockDescription;
  P: TParamDescription;
  ChildId, PanelId: integer;
  Panel: TPanelDescription;
  BlockRef: TBlockRef;
begin
  F := ADescription;
  FormName := BlocksQuery.FieldByName('id').AsString;
  if not FormsQuery.Locate('id', FormName, []) then
    raise Exception.CreateFmt('Описание формы не найдено: %s', [FormName]);
  FillBlockDescription(F);
  F.MainProcedureName := FormsQuery.FieldByName('main_procedure').AsString;
  F.HeaderVisible := (FormsQuery.FieldByName('header_visible').AsInteger <> 0);

  FLoadedChildBlocks.Clear;
  FormChildsQuery.Locate('form', AID, []);
  while
    (FormChildsQuery.FieldByName('form').AsString = AID) and
    (not FormChildsQuery.EOF)
  do
  begin
    ChildId := FormChildsQuery.FieldByName('id').AsInteger;
    BlocksQuery.Locate('id', FormChildsQuery.FieldByName('block').AsString, []);
    if not F.Blocks.TryGetValue(ChildId, C) then
    begin
      C := CreateBlock;
      F.Blocks.Add(ChildId, C);
    end;

    LoadBlock(FormChildsQuery.FieldByName('block').AsString, C);
    FormChildsQuery.Locate('id', ChildId, []);
    C.ChildId := ChildId;
    C.PanelID := FormChildsQuery.FieldByName('panel').AsInteger;
    C.IndexOnPanel := FormChildsQuery.FieldByName('order_num').AsInteger;
    C.DisplayLabel := FormChildsQuery.FieldByName('caption').AsString;
    C.ParamsDrawDirection := GetDrawDirectionByName(
      FormChildsQuery.FieldByName('param_align').AsString
    );
    C.ParamGroupsDrawDirection := GetDrawDirectionByName(
      FormChildsQuery.FieldByName('param_group_align').AsString
    );
    C.Visible := FormChildsQuery.FieldByName('visible').AsInteger <> 0;

    FormChildParamsQuery.Locate('form_child', ChildId, []);
    while
      (FormChildParamsQuery.FieldByName('form_child').AsInteger = ChildId) and
      (not FormChildParamsQuery.EOF)
    do
    begin
      P := C.ParamByName(FormChildParamsQuery.FieldByName('param').AsString);
      P.Visible := (FormChildParamsQuery.FieldByName('visible').AsInteger <> 0);
      P.Required := (FormChildParamsQuery.FieldByName('required').AsInteger <> 0);
      P.ReadOnly := (FormChildParamsQuery.FieldByName('read_only').AsInteger <> 0);
      P.AutoRefresh := (FormChildParamsQuery.FieldByName('auto_refresh').AsInteger <> 0);
      P.SourceBlockId := FormChildParamsQuery.FieldByName('source_child').AsInteger;
      P.SourceParamName := FormChildParamsQuery.FieldByName('source_param').AsString;
      FormChildParamsQuery.Next;
    end;

    for BlockRef in C.BlockRefs.Values do
      FillChildRefBinds(ChildId, BlockRef);

    FLoadedChildBlocks.Add(C);
    FormChildsQuery.Next;
  end;

  for ChildId in F.Blocks.Keys do
    if FLoadedChildBlocks.IndexOf(F.Blocks[ChildId]) = -1 then
      F.Blocks.Remove(ChildId);

  FLoadedPanels.Clear;
  FormPanelsQuery.Locate('form', AID, []);
  while
    (FormPanelsQuery.FieldByName('form').AsString = AID) and
    (not FormPanelsQuery.EOF)
  do
  begin
    if not F.Panels.TryGetValue(FormPanelsQuery.FieldByName('id').AsInteger, Panel) then
    begin
      Panel := TPanelDescription.Create;
      Panel.Id := FormPanelsQuery.FieldByName('id').AsInteger;
      F.Panels.Add(Panel.Id, Panel);
    end;

    Panel.ParentId := FormPanelsQuery.FieldByName('parent').AsInteger;
    Panel.IndexOnParent := FormPanelsQuery.FieldByName('index_on_parent').AsInteger;
    Panel.DisplayLabel := FormPanelsQuery.FieldByName('caption').AsString;
    Panel.ItemsDrawStyle := GetPanelDrawDirectionByName(FormPanelsQuery.FieldByName('align').AsString);

    FLoadedPanels.Add(Panel);
    FormPanelsQuery.Next;
  end;

  for PanelId in F.Panels.Keys do
    if FLoadedPanels.IndexOf(F.Panels[PanelId]) = -1 then
      F.Panels.Remove(PanelId)
    else
    begin
      F.Panels[PanelId].ChildPanels.Clear;
      F.Panels[PanelId].Blocks.Clear;
    end;

  for Panel in F.Panels.Values do
    if Panel.ParentId = 0 then
      F.RootPanel := Panel
    else
      F.Panels[Panel.ParentId].ChildPanels.Add(Panel);

  for C in F.Blocks.Values do
  begin
    Panel := F.Panels[C.PanelId];
    if not Assigned(Panel) then
      raise Exception.CreateFmt('Панель %d не найдена', [C.PanelId]);
    Panel.Blocks.Add(C);
  end;

  BlocksQuery.Locate('id', AID, []);
end;

procedure TDescriptionsLoaderDM.LoadBlock(AID: string; ADescription: TBlockDescription);
var
  BlockName: string;
begin
  BlockName := BlocksQuery.FieldByName('id').AsString;
  if ADescription is TFormDescription then
    LoadForm(BlockName, ADescription as TFormDescription)
  else if ADescription is TProcedureDescription then
    LoadProcedure(BlockName, ADescription as TProcedureDescription)
  else
    raise Exception.CreateFmt(
      'Неизвестный тип блока: %s',
      [ADescription.ClassType]
    );
end;


procedure TDescriptionsLoaderDM.InternalExecute(AConnection: TADConnection);

procedure PrepareQueries;
var
  i: integer;

  procedure OpenProcedureQuery(AProcedureName: string; AQuery: TADQuery);
  begin
    // для оракла - в спец. пакете
    FTempProcedureDescription.ProcedureName := AProcedureName;
    CustomMainDM.DBDependend.FillQuery(FTempProcedureDescription, AQuery);
    AQuery.Open;
  end;

begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TADQuery then
      (Components[i] as TADQuery).Connection := AConnection;

  OpenProcedureQuery('ui$cr_block', BlocksQuery);
  OpenProcedureQuery('ui$cr_procedure', ProceduresQuery);
  OpenProcedureQuery('ui$cr_block_param', BlockParamsQuery);
  OpenProcedureQuery('ui$cr_block_action', BlockActionsQuery);
  OpenProcedureQuery('ui$cr_action_bind', ActionBindsQuery);
  OpenProcedureQuery('ui$cr_form', FormsQuery);
  OpenProcedureQuery('ui$cr_form_child', FormChildsQuery);
  OpenProcedureQuery('ui$cr_form_child_param', FormChildParamsQuery);
  OpenProcedureQuery('ui$cr_form_panel', FormPanelsQuery);
  OpenProcedureQuery('ui$cr_block_ref_param', BlockRefParamsQuery);
  OpenProcedureQuery('ui$cr_block_ref_bind', BlockRefBindsQuery);
  OpenProcedureQuery('ui$cr_form_child_ref_bind', ChildRefBindsQuery);

end;

procedure LoadBlocks;
var
  BlockName: string;
  B: TBlockDescription;
begin
  FLoadedBlocks.Clear;
  BlocksQuery.First;
  while not BlocksQuery.EOF do
  begin
    BlockName := BlocksQuery.FieldByName('id').AsString;
    if not BlocksManager.Blocks.TryGetValue(BlockName, B) then
    begin
      B := CreateBlock;
      BlocksManager.Blocks.Add(BlockName, B);
    end;
    LoadBlock(BlockName, B);
    FLoadedBlocks.Add(B);
    BlocksQuery.Next;
  end;

  // чистим BlocksManager от удаленных блоков, не освобождая их,
  // поскольку они могут использоваться во фреймах
  // TODO: подумать, считать ли это утечкой памяти!
  for BlockName in BlocksManager.Blocks.Keys do
  begin
    B := BlocksManager.Blocks[BlockName];
    if FLoadedBlocks.IndexOf(B) = -1 then
      BlocksManager.Blocks.Remove(BlockName);
  end;
end;

begin
  PrepareQueries;
  LoadBlocks;
end;

procedure TDescriptionsLoaderDM.DataModuleCreate(Sender: TObject);
var
  PD: TParamDescription;
begin
  FTempProcedureDescription := TProcedureDescription.Create;

  FLoadedParams := TObjectList<TParamDescription>.Create;
  FLoadedParams.OwnsObjects := false;

  FLoadedBlocks := TObjectList<TBlockDescription>.Create;
  FLoadedBlocks.OwnsObjects := false;

  FLoadedChildBlocks := TObjectList<TBlockDescription>.Create;
  FLoadedChildBlocks.OwnsObjects := false;

  FLoadedBlockActions := TObjectList<TBlockAction>.Create;
  FLoadedBlockActions.OwnsObjects := false;

  FLoadedPanels := TObjectList<TPanelDescription>.Create;
  FLoadedPanels.OwnsObjects := false;

  FLoadedParamRefs := TObjectList<TBlockRef>.Create;
  FLoadedParamRefs.OwnsObjects := false;

  FLoadedBlockRefBinds := TObjectList<TBlockRefBind>.Create;
  FLoadedBlockRefBinds.OwnsObjects := false;

  PD := TParamDescription.Create;
  PD.Name := 'ID';
  PD.IndexInKeyFields := 1;
  PD.ParamDirection := pdField;
  PD.DataType := ftInteger;
  PD.OrderNum := 1;
  FTempProcedureDescription.Params.Add('ID', PD);
end;

procedure TDescriptionsLoaderDM.DataModuleDestroy(Sender: TObject);
begin
  FTempProcedureDescription.Free;
  FLoadedParams.Free;
  FLoadedBlocks.Free;
  FLoadedChildBlocks.Free;
  FLoadedBlockActions.Free;
  FLoadedBlockRefBinds.Free;
end;

class procedure TDescriptionsLoaderDM.Execute(AConnection: TADConnection);
var
  D: TDescriptionsLoaderDM;
begin
  D := TDescriptionsLoaderDM.Create(nil);
  try
    D.InternalExecute(AConnection);
  finally
    D.Free;
  end;
end;

procedure TDescriptionsLoaderDM.FillRefBinds(BlockRef: TBlockRef);
var
  RBDestinationParam: string;
  RB: TBlockRefBind;
begin
  FLoadedBlockRefBinds.Clear;
  BlockRefBindsQuery.Locate('ref', BlockRef.ID);
  while (BlockRefBindsQuery.FieldByName('ref').AsInteger = BlockRef.ID) and (not BlockRefBindsQuery.EOF) do
  begin
    if not BlockRef.Binds.TryGetValue(BlockRefBindsQuery.FieldByName('destination_param').AsString, RB) then
    begin
      RB := TBlockRefBind.Create(0, BlockRefBindsQuery.FieldByName('source_param').AsString, BlockRefBindsQuery.FieldByName('destination_param').AsString);
      BlockRef.Binds.Add(RB.DestinationParam, RB);
    end
    else
    begin
      RB.SourceParam := BlockRefBindsQuery.FieldByName('source_param').AsString;
      RB.SourceBlockId := 0;
    end;
    FLoadedBlockRefBinds.Add(RB);
    BlockRefBindsQuery.Next;
  end;
  for RBDestinationParam in BlockRef.Binds.Keys do
    if FLoadedBlockRefBinds.IndexOf(BlockRef.Binds[RBDestinationParam]) = -1 then
      BlockRef.Binds.Remove(RBDestinationParam);
end;

procedure TDescriptionsLoaderDM.FillChildRefBinds(ChildId: integer; BlockRef: TBlockRef);
var
  RBDestinationParam: string;
  RB: TBlockRefBind;
begin
  FLoadedBlockRefBinds.Clear;
  ChildRefBindsQuery.Locate('form_child;ref', VarArrayOf([ChildId, BlockRef.ID]), []);
  while
    (ChildRefBindsQuery.FieldByName('ref').AsInteger = BlockRef.ID) and
    (ChildRefBindsQuery.FieldByName('form_child').AsInteger = ChildId) and
    (not ChildRefBindsQuery.EOF)
  do
  begin
    if not BlockRef.Binds.TryGetValue(ChildRefBindsQuery.FieldByName('destination_param').AsString, RB) then
    begin
      RB := TBlockRefBind.Create(
        ChildRefBindsQuery.FieldByName('source_child').AsInteger,
        ChildRefBindsQuery.FieldByName('source_param').AsString,
        ChildRefBindsQuery.FieldByName('destination_param').AsString
      );
      BlockRef.Binds.Add(RB.DestinationParam, RB);
    end
    else
    begin
      RB.SourceParam := ChildRefBindsQuery.FieldByName('source_param').AsString;
      RB.SourceBlockId := ChildRefBindsQuery.FieldByName('source_child').AsInteger;
    end;
    FLoadedBlockRefBinds.Add(RB);
    ChildRefBindsQuery.Next;
  end;
  for RBDestinationParam in BlockRef.Binds.Keys do
    if FLoadedBlockRefBinds.IndexOf(BlockRef.Binds[RBDestinationParam]) = -1 then
      BlockRef.Binds.Remove(RBDestinationParam);
end;

end.
