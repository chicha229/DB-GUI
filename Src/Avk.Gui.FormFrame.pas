unit Avk.Gui.FormFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Avk.Gui.BlockFrame, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxSkinsCore, uFormErrors, Vcl.ExtCtrls, cxLabel,
  System.Generics.Collections,
  Avk.Gui.Descriptions, dxBar, cxClasses, uAdCompClient, dxSkinsDefaultPainters,
  dxSkinsdxBarPainter, Avk.Gui.Connection;

type
  TFormFrame = class (TBlockFrame)
  published
    FramesScrollBox: TScrollBox;
  private
    { Private declarations }
    FFrames: TObjectDictionary<integer, TBlockFrame>;
    FParentControls: TObjectList<TWinControl>;
    FRefParamValues: TParamValues;
    FSavepointName: string;

    function GetFormDescription: TFormDescription;
    procedure BuildPanel(APanel: TPanelDescription; AParent: TWinControl);
    procedure BindFrameParamsFromSource(Frame: TBlockFrame; var AllParamsBinded: Boolean);
  protected
    procedure SetTransaction(const Value: ITransaction); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Open: boolean; override;
    function Save: boolean; override;
    function Modified: boolean; override;
    function ConfirmCancel: boolean; override;
    procedure DropChanges; override;

    procedure OnChangeParamValues(Sender: TBlockFrame; AChangeId: Int64); override;

    procedure Build(AParent: TWinControl); override;
    property FormDescription: TFormDescription read GetFormDescription;
  end;

implementation

uses
  System.Math, System.Generics.Defaults,
  cxPC,
  Avk.Gui.ProcedureFrame, Avk.Gui.CustomMainForm, Avk.Gui.CustomMainDM;

{$R *.dfm}

type
  TIndexOnParentComparer<T: class> = class (TComparer<T>, IComparer<T>)
  public
    function Compare(const Left, Right: T): Integer; override;
  end;

var
  FIndexOnParentComparer: TIndexOnParentComparer<TObject>;

function IndexOnParentComparer: TIndexOnParentComparer<TObject>;
begin
  if not Assigned(FIndexOnParentComparer) then
    FIndexOnParentComparer := TIndexOnParentComparer<TObject>.Create;
  Result := FIndexOnParentComparer;
end;

{ TFormFrame }

procedure TFormFrame.BuildPanel(APanel: TPanelDescription; AParent: TWinControl);
var
  B: TObject;
  L: TObjectList<TObject>;
  PC: TcxPageControl;
  PD: TPanelDescription;
  Block: TBlockDescription;
  F: TBlockFrame;
  P: TPanel;
  CP, C: TWinControl;
  LastVisibleCP: TWinControl;
  T: TcxTabSheet;
  X, Y: integer;
  S: TSplitter;
begin
  S := nil;
  if APanel.ItemsDrawStyle = pddTabs then
  begin
    PC := TcxPageControl.Create(Self);
    PC.Parent := AParent;
    PC.Align := alClient;
    FParentControls.Add(PC);
  end
  else
    PC := nil;

  L := TObjectList<TObject>.Create;
  try
    L.OwnsObjects := false;
    for B in APanel.Blocks do
      L.Add(B);
    for B in APanel.ChildPanels do
      L.Add(B);
    L.Sort(IndexOnParentComparer);

    X := 0;
    Y := 0;
    case APanel.ItemsDrawStyle of
      pddLeft: X := 1;
      pddRight: X := AParent.Width - 1;
      pddTop: Y := 1;
      pddBottom: Y := AParent.Height - 1;
    end;

    LastVisibleCP := nil;
    for B in L do
    begin
      PD := nil;
      Block := nil;
      if B is TPanelDescription then
        PD := TPanelDescription(B)
      else if B is TBlockDescription then
        Block := TBlockDescription(B)
      else
        raise Exception.CreateFmt('Unknown item type: %s', [B.ClassName]);

      // создаем контрол (CP) дл€ расположени€ панели или блока
      if APanel.ItemsDrawStyle in [pddLeft, pddRight, pddTop, pddBottom] then
      begin
        T := nil;
        if Assigned(PD) then
        begin
          P := TPanel.Create(Self);
          P.Parent := AParent;
          P.Caption := '';
          P.BevelOuter := bvNone;
          P.Constraints.MinWidth := 200;
          P.Constraints.MinHeight := 200;
          CP := P;
          FParentControls.Add(P);
        end
        else
          CP := AParent;
      end
      else
      begin
        T := TcxTabSheet.Create(Self);
        T.PageControl := PC;
        if Assigned(PD) then
          T.Caption := PD.DisplayLabel
        else
          T.Caption := Block.DisplayLabel;
        CP := T;
      end;

      // билдим панель или блок, результат - на контроле C
      C := nil;
      if Assigned(PD) then
      begin
        BuildPanel(PD, CP);
        C := CP;
      end
      else if Assigned(Block) then
      begin
        F := CustomMainForm.CreateBlockFrame(Block, Transaction, false, Self);
        F.Build(CP);
        FFrames.Add(Block.ChildId, F);
        F.OwnerFrame := Self;
        C := F;
      end;

      C.Width := C.Constraints.MinWidth + 8;
      C.Height := C.Constraints.MinHeight + 8;

      if (Assigned(Block) and Block.Visible) or Assigned(PD) then
      begin
        S := TSplitter.Create(Self);
        S.Parent := AParent;
        case APanel.ItemsDrawStyle of
          pddLeft: begin C.Align := alLeft; C.Left := X; Inc(X, C.Width + 3); end;
          pddRight: begin C.Align := alRight; C.Left := X; Dec(X, C.Width + 3); end;
          pddTop: begin C.Align := alTop; C.Top := Y; Inc(Y, C.Height + 3); end;
          pddBottom: begin C.Align := alBottom; C.Top := Y; Dec(Y, C.Height + 3); end;
        end;
        S.Align := C.Align;
        case APanel.ItemsDrawStyle of
          pddLeft: S.Left := C.Left + C.Width + 3;
          pddRight: S.Left := C.Left - 3;
          pddTop: S.Top := C.Top + C.Height + 3;
          pddBottom: S.Top := C.Top - 3;
        end;
        LastVisibleCP := C;
      end;
      if Assigned(Block) and (not Block.Visible) then
        if APanel.ItemsDrawStyle = pddTabs then
          T.Visible := false
        else
          C.Hide;
    end;
    if Assigned(LastVisibleCP) then
    begin
      LastVisibleCP.Align := alClient;
      if Assigned(S) then
        S.Hide;
    end;
  finally
    L.Free;
  end;
end;

procedure TFormFrame.Build(AParent: TWinControl);
begin
  inherited;
  FFrames.Clear;
  FParentControls.Clear;
  BuildPanel(FormDescription.RootPanel, FramesScrollBox);

  if not FormDescription.HeaderVisible then
    HideHeader;
end;

procedure TFormFrame.BindFrameParamsFromSource(Frame: TBlockFrame; var AllParamsBinded: Boolean);
var
  Param: TParamDescription;
  ParamFound: boolean;
  SourceFrame: TBlockFrame;
begin
  AllParamsBinded := true;
  for Param in Frame.BlockDescription.Params.Values do
  begin
    ParamFound := false;
    if Param.SourceParamName = '' then
      ParamFound := true
    else
    begin
      if Param.SourceBlockId = 0 then
        SourceFrame := Self
      else
        SourceFrame := FFrames[Param.SourceBlockId];
      if SourceFrame.IsOpened then
      begin
        if SourceFrame.ParamValues.ContainsKey(Param.SourceParamName) then
          Frame.ParamValues.AddOrSetValue(
            Param.Name,
            SourceFrame.ParamValues[Param.SourceParamName]
          )
        else
          Frame.ParamValues.AddOrSetValue(
            Param.Name,
            Null
          );
        ParamFound := true;
      end;
    end;

    if not ParamFound then
      AllParamsBinded := false;
  end;

  Frame.ParamValuesToEditors;
end;

function TFormFrame.GetFormDescription: TFormDescription;
begin
  Result := BlockDescription as TFormDescription;
end;


function TFormFrame.Modified: boolean;
var
  F: TBlockFrame;
begin
  for F in FFrames.Values do
    if F.Modified then
    begin
      Result := true;
      Exit;
    end;
  Result := false;
end;

procedure TFormFrame.OnChangeParamValues(
  Sender: TBlockFrame; AChangeId: Int64
);

procedure RefreshRefQueries;
var
  ParamName: string;
  PV: TParamValues;
  RefId: integer;
  Ref: TBlockRef;
  RefInfo: TRefInfo;
  RefBind: TBlockRefBind;
  RefFrame: TBlockFrame;
  FrameChangeId: Int64;
begin
  PV := Sender.ChangedParams[AChangeId];
  for ParamName in PV.Keys do
    // измененное значение. какой из рефов зависит от этого
    for RefFrame in FFrames.Values do
    begin
      FrameChangeId := RefFrame.BeginParamChanging;
      try
        for RefId in RefFrame.BlockDescription.BlockRefs.Keys do
        begin
          RefInfo := nil;
          FRefParamValues.Clear;
          Ref := RefFrame.BlockDescription.BlockRefs[RefId];
          for RefBind in Ref.Binds.Values do
            if
              (RefBind.SourceBlockId = Sender.BlockDescription.ChildId) and
              (RefBind.SourceParam = ParamName)
            then
            begin
              RefInfo := CustomMainDM.GetRefInfo(Ref.RefsTo);
              FRefParamValues.Add(RefBind.DestinationParam, Sender.ParamValues[ParamName]);
            end;
          if Assigned(RefInfo) then
            RefInfo.RefreshData(FRefParamValues);
        end;
      finally
        RefFrame.EndParamChanging(FrameChangeId);
      end;
    end;
end;

procedure RefreshChildParams;
var
  ChangedParamName: string;
  ChangedParamValues: TParamValues;
  F: TBlockFrame;
  ParamName: string;
  FrameChangeId: Int64;
begin
  ChangedParamValues := Sender.ChangedParams[AChangeId];
  for ChangedParamName in ChangedParamValues.Keys do
    for F in FFrames.Values do
    begin
      FrameChangeId := F.BeginParamChanging;
      try
        for ParamName in F.BlockDescription.Params.Keys do
          if
//            (F.BlockDescription.Params[ParamName].AutoRefresh) and
            (F.BlockDescription.Params[ParamName].SourceParamName = ChangedParamName) and
            (F.BlockDescription.Params[ParamName].SourceBlockId = Sender.BlockDescription.ChildId)
          then
            F.ParamValues.AddOrSetValue(ParamName, Sender.ParamValues[ChangedParamName]);
        F.ParamValuesToEditors;
      finally
        F.EndParamChanging(FrameChangeId);
      end;
    end;
end;

procedure RefreshSelfParams;
var
  ChangedParamName: string;
  ChangedParamValues: TParamValues;
  ParamName: string;
  FormChangeId: Int64;
begin
  ChangedParamValues := Sender.ChangedParams[AChangeId];

  FormChangeId := BeginParamChanging;
  try
    for ChangedParamName in ChangedParamValues.Keys do
      for ParamName in FormDescription.Params.Keys do
        if
          (Sender.BlockDescription.Params[ChangedParamName].SourceBlockId = 0) and
          (Sender.BlockDescription.Params[ChangedParamName].SourceParamName = ParamName)
        then
          ParamValues.AddOrSetValue(ParamName, Sender.ParamValues[ChangedParamName]);
  finally
    EndParamChanging(FormChangeId);
  end;
end;

begin
  inherited;

  // свои параметры не мен€ютс€ после открыти€ фрейма
  if Sender <> Self then
  begin
    RefreshRefQueries;
    RefreshChildParams;
    RefreshSelfParams;
  end;
end;

function TFormFrame.Open: boolean;

  procedure OpenChildFrames;
  var
    OpenedCount, LoopCounter: integer;
    Frame: TBlockFrame;
    AllParamsBinded: boolean;
  begin
    for Frame in FFrames.Values do
    begin
      Frame.IsOpened := false;
      Frame.IsOpening := true;
    end;

    LoopCounter := 0;
    repeat
      OpenedCount := 0;
      for Frame in FFrames.Values do
        if not Frame.IsOpened then
        begin
          BindFrameParamsFromSource(Frame, AllParamsBinded);
          if AllParamsBinded then
          begin
            Frame.Open;
            Frame.IsOpened := true;
            Inc(OpenedCount);
          end;
        end;
      Inc(LoopCounter);
    until (OpenedCount = 0) or (LoopCounter > FFrames.Count + 1);
    if LoopCounter > FFrames.Count + 1 then
      raise Exception.Create('÷икл зависимостей при открытии формы');

    for Frame in FFrames.Values do
      Frame.IsOpening := false;
  end;

  procedure OpenRefs;
  var
    F: TBlockFrame;
    R: TBlockRef;
    B: TBlockRefBind;
    RefInfo: TRefInfo;
    SourceFrame: TBlockFrame;
  begin
    for F in FFrames.Values do
      for R in F.BlockDescription.BlockRefs.Values do
      begin
        RefInfo := CustomMainDM.GetRefInfo(R.RefsTo);
        if not Assigned(RefInfo) then
          Continue;
        FRefParamValues.Clear;
        for B in R.Binds.Values do
        begin
          if B.SourceBlockId = 0 then
            SourceFrame := Self
          else
            SourceFrame := FFrames[B.SourceBlockId];
          FRefParamValues.Add(
            B.DestinationParam,
            SourceFrame.ParamValues[B.SourceParam]
          );
        end;
        RefInfo.RefreshData(FRefParamValues);
      end;
  end;

begin
  Self.IsOpened := true;
  Self.IsOpening := true;

  OpenChildFrames;
  OpenRefs;

  Self.IsOpening := false;
  Result := true;

  FSavepointName := 'S$' + IntToStr(GetSavepointId);
  Transaction.MakeSavepoint(FSavepointName);
end;

function TFormFrame.Save: boolean;
var
  F: TBlockFrame;

  procedure GetBindedFrameParamValues(F: TBlockFrame);
  var
    P: TParamDescription;
  begin
    for P in F.BlockDescription.Params.Values do
      if
        (P.SourceParamName <> '') and
        (P.SourceBlockId = 0) and
        (P.ParamDirection in [pdOut, pdInOut])
      then
        ParamValues.AddOrSetValue(P.SourceParamName, F.ParamValues[P.Name]);
  end;

begin
//  PostEditorsValues;
  // пока без каскадных сохранений
  for F in FFrames.Values do
  begin
    if not F.BlockDescription.Visible then
      Continue;
//    F.PostEditorsValues;
    F.Save;
    GetBindedFrameParamValues(F);
    F.IsOpened := true;
  end;
  if IsTransactionStart then
    Transaction.Commit;
  Result := true;
end;

procedure TFormFrame.SetTransaction(const Value: ITransaction);
var
  F: TBlockFrame;
begin
  inherited SetTransaction(Value);
  for F in FFrames.Values do
    if F is TProcedureFrame then
      (F as TProcedureFrame).Transaction := Value
    else if F is TFormFrame then
      (F as TFormFrame).Transaction := Value
end;

function TFormFrame.ConfirmCancel: boolean;
var
  F: TBlockFrame;
begin
  Result := false;
  for F in FFrames.Values do
    if (not F.BlockDescription.IsDataSet) and F.ConfirmCancel then
    begin
      Result := true;
      Exit;
    end;
end;

constructor TFormFrame.Create(AOwner: TComponent);
begin
  inherited;
  FFrames := TObjectDictionary<integer, TBlockFrame>.Create([]);
  FParentControls := TObjectList<TWinControl>.Create;
  FParentControls.OwnsObjects := false;
  FRefParamValues := TParamValues.Create;
end;

destructor TFormFrame.Destroy;
begin
  FFrames.Free;
  FParentControls.Free;
  FRefParamValues.Free;
  inherited;
end;

procedure TFormFrame.DropChanges;
begin
  if IsTransactionStart then
    Transaction.Rollback
  else
    Transaction.RollbackToSavepoint(FSavepointName);
end;

{ TIndexOnParentComparer<T> }

function ExtractIndexOnParent(AObject: TObject): integer;
begin
  if AObject is TPanelDescription then
    Result := (AObject as TPanelDescription).IndexOnParent
  else if AObject is TBlockDescription then
    Result := (AObject as TBlockDescription).IndexOnPanel
  else
    raise Exception.CreateFmt('Unknown item type: %s', [TObject(AObject).ClassName]);
end;

function TIndexOnParentComparer<T>.Compare(const Left, Right: T): Integer;
begin
  Result := Sign(ExtractIndexOnParent(Left) - ExtractIndexOnParent(Right));
end;

initialization
  ;

finalization
  FIndexOnParentComparer.Free;

end.
