unit Avk.Gui.CustomMainDM;

interface

uses
  System.SysUtils, System.Classes, uADGUIxIntf, uADStanIntf,
  uADStanOption, uADStanError, uADPhysIntf, uADStanDef, uADStanPool,
  uADStanAsync, uADPhysManager, Data.DB, uADCompClient, uADPhysOracle,
  uADCompGUIx, uADGUIxFormsWait,
  Avk.Gui.Connection, Vcl.ImgList, Vcl.Controls, System.Generics.Collections,
  Avk.Gui.Descriptions, cxStyles, cxClasses, cxContainer, cxEdit;

type
  TCustomMainDataModule = class;
  TCustomMainDataModuleClass = class of TCustomMainDataModule;

  TRefInfo = class
  private
    FRefsTo: TProcedureDescription;
    FDataSet: TADMemTable;
    FDataSource: TDataSource;
    FCallParamValues: TParamValues;
  public
    constructor Create(
      ARefsTo: TProcedureDescription;
      ADataSet: TADMemTable;
      ADataSource: TDataSource
    );
    destructor Destroy; override;

    procedure RefreshData(AParamValues: TParamValues);

    property DataSource: TDataSource read FDataSource;
    property DataSet: TADMemTable read FDataSet;
  end;

  TCustomMainDataModule = class (TDataModule)
    ADGUIxWaitCursor1: TADGUIxWaitCursor;
    Images: TImageList;
    cxStyleRepository: TcxStyleRepository;
    GridHeaderStyle: TcxStyle;
    ReadOnlyEditStyleController: TcxEditStyleController;
  private
    FConnection: IConnection;
    FMainTransaction: ITransaction;
    FCommonRefs: TObjectDictionary<string, TRefInfo>;
    FLogDetails: TStrings;

    function GetConnection: IConnection;
    function GetMainTransaction: ITransaction;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }

    function GetConnectionMode: string; virtual; abstract;
    property Connection: IConnection read GetConnection;
    property MainTransaction: ITransaction read GetMainTransaction;
    function GetRefInfo(ARefBlockName: string): TRefInfo;
    procedure OnRefreshProcedure(AProcedureName: string);
  end;

function GetSavepointId: integer;

var
  CustomMainDM: TCustomMainDataModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses
  Variants,
  uADStanParam,
  AVK.Core.Utils;

{$R *.dfm}

var
  GSavepointId: integer;

function GetSavepointId: integer;
begin
  Inc(GSavepointId);
  Result := GSavepointId;
end;

constructor TCustomMainDataModule.Create(AOwner: TComponent);
begin
  inherited;
  FCommonRefs := TObjectDictionary<string, TRefInfo>.Create([doOwnsValues]);
  FLogDetails := TStringList.Create;
end;

destructor TCustomMainDataModule.Destroy;
begin
  FCommonRefs.Free;
  FLogDetails.Free;
  inherited;
end;

function TCustomMainDataModule.GetConnection: IConnection;
var
  ConnectionString: string;
begin
  if not Assigned(FConnection) then
  begin
    FConnection := TConnectionFactory.CreateConnection(GetConnectionMode);
    ConnectionString := LoadSettingsValue('common', 'ConnectionString');
    FConnection.SetConnectionString(ConnectionString);
  end;
  Result := FConnection;
end;

function TCustomMainDataModule.GetRefInfo(
  ARefBlockName: string): TRefInfo;
var
  D: TADMemTable;
  DS: TDataSource;
  F: TFormDescription;
  B: TBlockdescription;
  P: TProcedureDescription;
begin
  if not FCommonRefs.ContainsKey(ARefBlockName) then
  begin
    B := BlocksManager.Blocks[ARefBlockName];
    if B is TProcedureDescription then
      P := B as TProcedureDescription
    else
    begin
      F := B as TFormDescription;
      P := BlocksManager.Blocks[F.MainProcedureName] as TProcedureDescription;
    end;
    D := TADMemTable.Create(nil);
    MainTransaction.QueryData(P, nil, D);
    DS := TDataSource.Create(Self);
    DS.DataSet := D;
    Result := TRefInfo.Create(P, D, DS);
    FCommonRefs.Add(ARefBlockName, Result);
  end
  else
    Result := FCommonRefs[ARefBlockName];
end;

function TCustomMainDataModule.GetMainTransaction: ITransaction;
begin
  if not Assigned(FMainTransaction) then
    FMainTransaction := Connection.StartTransaction;
  Result := FMainTransaction;
end;

procedure TCustomMainDataModule.OnRefreshProcedure(AProcedureName: string);
var
  DS: TADMemTable;
begin
  if not FCommonRefs.ContainsKey(AProcedureName) then
    Exit;
  DS := FCommonRefs[AProcedureName].DataSet as TADMemTable;
  if DS.Active then
  begin
    DS.Close;
    MainTransaction.QueryData(
      BlocksManager.Blocks[AProcedureName] as TProcedureDescription,
      nil,
      DS
    );
  end;
end;

{ TRefInfo }

constructor TRefInfo.Create(
  ARefsTo: TProcedureDescription;
  ADataSet: TADMemTable;
  ADataSource: TDataSource
);
begin
  FRefsTo := ARefsTo;
  FDataSet := ADataSet;
  FDataSource := ADataSource;
  FCallParamValues := TParamValues.Create;
end;

destructor TRefInfo.Destroy;
begin
  FDataSet.Free;
  FDataSource.Free;
  FCallParamValues.Free;
  inherited;
end;

procedure TRefInfo.RefreshData(AParamValues: TParamValues);
var
  ParamsChanged: boolean;
  PD: string;
begin
  if not DataSet.Active then
  begin
    AParamValues.Clear;
    AddParamValues(AParamValues, FCallParamValues);
    CustomMainDM.MainTransaction.QueryData(
      FRefsTo,
      AParamValues,
      FDataSet
    );
  end
  else
  begin
    ParamsChanged := false;
    for PD in AParamValues.Keys do
      if
        (FRefsTo.Params[PD].ParamDirection in [pdIn, pdInOut]) and
        (not VarIsNull(AParamValues[PD]))
      then
      begin
        if
          (not FCallParamValues.ContainsKey(PD)) or
          (FCallParamValues[PD] <> AParamValues[PD])
        then
        begin
          FCallParamValues.AddOrSetValue(PD, AParamValues[PD]);
          ParamsChanged := true;
        end;
      end;
    if ParamsChanged then
    begin
      CustomMainDM.MainTransaction.QueryData(
        FRefsTo,
        AParamValues,
        FDataSet
      );
      AParamValues.Clear;
      AddParamValues(AParamValues, FCallParamValues);
    end;
  end;
end;

initialization
  GSavepointId := 0;

end.

