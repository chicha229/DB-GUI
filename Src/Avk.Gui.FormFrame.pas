unit Avk.Gui.FormFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Avk.Gui.BlockFrame, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxSkinsCore, uFormErrors, Vcl.ExtCtrls, cxLabel,
  System.Generics.Collections,
  Avk.Gui.Descriptions, dxBar, cxClasses, uAdCompClient, dxSkinsDefaultPainters,
  dxSkinsdxBarPainter;

type
  TFormFrame = class (TBlockFrame)
  published
    FramesScrollBox: TScrollBox;
  private
    { Private declarations }
    FFrames: TObjectDictionary<integer, TBlockFrame>;
    FParentControls: TObjectList<TWinControl>;
    FConnection: TADConnection;

    function GetFormDescription: TFormDescription;
    procedure BuildPanel(APanel: TPanelDescription; AParent: TWinControl);
    procedure BindFrameParamsFromSource(Frame: TBlockFrame; var AllParamsBinded: Boolean);
    procedure SetConnection(const Value: TADConnection);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Open: boolean; override;
    function Save: boolean; override;

    procedure EditorsToParamValues; override;
    procedure OnChangeParamValues(Sender: TBlockFrame; AChangeId: Int64); override;

    procedure Build(AParent: TWinControl); override;
    property FormDescription: TFormDescription read GetFormDescription;
    property Connection: TADConnection read FConnection write SetConnection;
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
    L.Sort(TIndexOnParentComparer<TObject>.Create);

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

      // создаем контрол (CP) для расположения панели или блока
      if APanel.ItemsDrawStyle in [pddLeft, pddRight, pddTop, pddBottom] then
      begin
        T := nil;
        if Assigned(PD) then
        begin
          P := TPanel.Create(Self);
          P.Parent := AParent;
          P.Caption := '';
          P.BevelOuter := bvNone;
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
        F := CustomMainForm.CreateBlockFrame(Block, Self);
        F.Build(CP);
        FFrames.Add(Block.ChildId, F);
        F.OwnerFrame := Self;
        C := F;
      end;

      C.Width := Max(C.Constraints.MinWidth, 200);
      C.Height := Max(C.Constraints.MinHeight, 200);
{
      C.Width := C.Constraints.MinWidth;
      C.Height := C.Constraints.MinHeight;
}
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
  Param, FormParam: TParamDescription;
  ParamFound: boolean;
begin
  AllParamsBinded := true;
  for Param in Frame.BlockDescription.Params.Values do
  begin
    ParamFound := false;
    if (Param.SourceBlockId = 0) or (FFrames[Param.SourceBlockId].IsOpened) then
    begin
      if
        (Param.SourceBlockId <> 0) and
        (FFrames[Param.SourceBlockId].ParamValues.ContainsKey(Param.SourceParamName))
      then
        Frame.ParamValues.AddOrSetValue(
          Param.Name,
          FFrames[Param.SourceBlockId].ParamValues[Param.SourceParamName]
        );
      ParamFound := true;
    end;
    // все in и inout параметры формы, у которых SourceBlock = переданному
    for FormParam in FormDescription.Params.Values do
      if
        (FormParam.SourceBlockId = Frame.BlockDescription.ChildID) and
        (FormParam.ParamDirection in [pdIn, pdInOut]) and
        (FormParam.SourceParamName = Param.Name)
      then
      begin
        if ParamValues.ContainsKey(FormParam.Name) then
          Frame.ParamValues.AddOrSetValue(Param.Name, ParamValues[FormParam.Name]);
        ParamFound := true;
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


procedure TFormFrame.OnChangeParamValues(
  Sender: TBlockFrame; AChangeId: Int64
);

procedure RefreshRefQueries;
var
  ParamName: string;
  PV: TParamValues;
  RefId: integer;
  Ref: TBlockRef;
  RefBind: TBlockRefBind;
  RefQuery: TADQuery;
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
          RefQuery := nil;
          Ref := RefFrame.BlockDescription.BlockRefs[RefId];
          for RefBind in Ref.Binds.Values do
            if
              (RefBind.SourceBlockId = Sender.BlockDescription.ChildId) and
              (RefBind.SourceParam = ParamName)
            then
            begin
              RefQuery := (CustomMainDM.GetRefDataSource(Ref.RefsTo).DataSet as TADQuery);
              RefQuery.ParamByName(ParamName).Value := Sender.ParamValues[ParamName];
            end;
          if Assigned(RefQuery) then
          begin
            RefQuery.Close;
            RefQuery.Open;
          end;
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
  SelfParam: TParamDescription;
begin
  ChangedParamValues := Sender.ChangedParams[AChangeId];

  FormChangeId := BeginParamChanging;
  try
    for ChangedParamName in ChangedParamValues.Keys do
      for ParamName in FormDescription.Params.Keys do
      begin
        SelfParam := FormDescription.Params[ParamName];
        if
         (SelfParam.SourceParamName = ChangedParamName) and
         (SelfParam.SourceBlockId = Sender.BlockDescription.ChildId)
        then
          ParamValues.AddOrSetValue(SelfParam.Name, Sender.ParamValues[ChangedParamName]);
      end;
  finally
    EndParamChanging(FormChangeId);
  end;
end;

begin
  inherited;

  // свои параметры не меняются после открытия фрейма
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
    FrameOpened: boolean;
    AllParamsBinded: boolean;
  begin
    for Frame in FFrames.Values do
      Frame.IsOpened := false;

    LoopCounter := 0;
    repeat
      OpenedCount := 0;
      for Frame in FFrames.Values do
        if not Frame.IsOpened then
        begin
          BindFrameParamsFromSource(Frame, AllParamsBinded);
          if AllParamsBinded then
          begin
            try
              FrameOpened := Frame.Open;
            except
              FrameOpened := false;
            end;
            if FrameOpened then
            begin
              Frame.IsOpened := true;
              Inc(OpenedCount);
            end;
          end;
        end;
      Inc(LoopCounter);
    until (OpenedCount = 0) or (LoopCounter > FFrames.Count + 1);
    if LoopCounter > FFrames.Count + 1 then
      raise Exception.Create('Цикл зависимостей при открытии формы');
  end;

  procedure SetReqQueriesParamValues;
  var
    PV: TParamValues;
    ParamName: string;
    RefId: integer;
    FrameChangeId: integer;
    RefBind: TBlockRefBind;
    RefQuery: TADQuery;
    RefFrame: TBlockFrame;
    Ref: TBlockRef;
  begin
    PV := ParamValues;
    for ParamName in PV.Keys do
      // измененное значение. какой из рефов зависит от этого
      for RefFrame in FFrames.Values do
      begin
        FrameChangeId := RefFrame.BeginParamChanging;
        try
          for RefId in RefFrame.BlockDescription.BlockRefs.Keys do
          begin
            RefQuery := nil;
            Ref := RefFrame.BlockDescription.BlockRefs[RefId];
            for RefBind in Ref.Binds.Values do
              if
                (RefBind.SourceBlockId = 0) and
                (RefBind.SourceParam = ParamName)
              then
              begin
                RefQuery := (CustomMainDM.GetRefDataSource(Ref.RefsTo).DataSet as TADQuery);
                RefQuery.ParamByName(ParamName).Value := PV[ParamName];
              end;
            if Assigned(RefQuery) then
            begin
              RefQuery.Close;
              RefQuery.Open;
            end;
          end;
        finally
          RefFrame.EndParamChanging(FrameChangeId);
        end;
      end;
  end;

begin
  Self.IsOpened := false;

  OpenChildFrames;
  SetReqQueriesParamValues;

  Self.IsOpened := true;
  Result := true;
end;

function TFormFrame.Save: boolean;
var
  F: TBlockFrame;
begin
//  PostEditorsValues;
  // пока без каскадных сохранений
  for F in FFrames.Values do
  begin
    if not F.BlockDescription.Visible then
      Continue;
//    F.PostEditorsValues;
    F.Save;
    F.IsOpened := true;
  end;
  Result := true;
end;

procedure TFormFrame.SetConnection(const Value: TADConnection);
var
  F: TBlockFrame;
begin
  FConnection := Value;
  for F in FFrames.Values do
    if F is TProcedureFrame then
      (F as TProcedureFrame).Connection := Value
    else if F is TFormFrame then
      (F as TFormFrame).Connection := Value
end;

constructor TFormFrame.Create(AOwner: TComponent);
begin
  inherited;
  FFrames := TObjectDictionary<integer, TBlockFrame>.Create([]);
  FParentControls := TObjectList<TWinControl>.Create;
  FParentControls.OwnsObjects := false;
end;

destructor TFormFrame.Destroy;
begin
  FFrames.Free;
  FParentControls.Free;
  inherited;
end;

procedure TFormFrame.EditorsToParamValues;
var
  Param: TParamDescription;
begin
  inherited;
  for Param in FormDescription.Params.Values do
    if
      (Param.ParamDirection in [pdOut, pdInOut, pdField]) and
      (Param.SourceBlockId <> 0) and
      (FFrames[Param.SourceBlockId].ParamValues.ContainsKey(Param.SourceParamName))
    then
      ParamValues.AddOrSetValue(
        Param.Name,
        FFrames[Param.SourceBlockId].ParamValues[Param.SourceParamName]
      );
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

end.
