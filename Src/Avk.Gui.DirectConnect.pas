unit Avk.Gui.DirectConnect;

interface

uses
  DB,
  uADCompClient,
  Avk.Gui.Connection, Avk.Gui.Descriptions;

type
  TDirectTransaction = class (TInterfacedObject, ITransaction)
  private
    FTransaction: TADTransaction;
    FCmd: TADCommand;
  protected
    function GetTransaction: TADTransaction;
    procedure LogTransactionAction(AAction: string);
    procedure ExecSql(ASql: string);
  public
    constructor Create(AConnection: TADConnection); virtual;
    destructor Destroy; override;

    procedure Commit; virtual;
    procedure Rollback; virtual;

    procedure CommitRetaining; virtual;
    procedure RollbackRetaining; virtual;

    procedure MakeSavepoint(const AName: string); virtual;
    procedure RollbackToSavepoint(const AName: string); virtual;

    procedure QueryData(
      const AProcedure: TProcedureDescription;
      const AParamValues: TParamValues;
      const AData: TADMemTable;
      const ACacheData: boolean = false
    ); virtual;
    procedure ExecuteProcedure(
      const AProcedure: TProcedureDescription; const AParamValues: TParamValues
    ); virtual;
    procedure SetCacheChanged(const Value: TCacheChanged);
  end;

  TDirectTransactionClass = class of TDirectTransaction;

  TDirectConnection = class (TInterfacedObject, IConnection)
  private
    FConnectionString: string;
    FConnection: TADConnection;
  protected
    function GetConnectionString: string;
    function GetConnection: TADConnection;

    procedure BeforeConnect; virtual;
    procedure AfterConnect; virtual;

    function GetDirectTransactionClass: TDirectTransactionClass; virtual; abstract;
  public
    class function NewInstance: TObject; override;
    destructor Destroy; override;

    procedure SetConnectionString(const AConnectionString: string);

    procedure Connect;
    procedure Disconnect;

    function StartTransaction: ITransaction;
  end;

procedure FillQueryParams(
  AQuery: TADQuery;
  AProcedure: TProcedureDescription;
  AParamValues: TParamValues
);

procedure FillDataSetFields(
  ADataSet: TDataSet;
  AProcedureDescription: TProcedureDescription
);

implementation

uses
  System.Classes, SysUtils, Variants,
  CodeSiteLogging,
  uADStanParam;

var
  GLogDetails: TStringList;

procedure FillQueryParams(
  AQuery: TADQuery;
  AProcedure: TProcedureDescription;
  AParamValues: TParamValues
);
var
  P: TADParam;
  PD: TParamDescription;
begin
  AQuery.Params.ClearValues;
  for PD in AProcedure.Params.Values do
    if PD.ParamDirection <> pdField then
    begin
      P := AQuery.ParamByName(PD.Name);
      if PD.DataType = ftBoolean then
        P.DataType := ftSmallint
      else
        P.DataType := PD.DataType;
      case PD.ParamDirection of
        pdIn: P.ParamType := ptInput;
        pdOut: P.ParamType := ptOutput;
        pdInOut: P.ParamType := ptInputOutput;
        pdCursor: P.ParamType := ptResult;
      end;
      if Assigned(AParamValues) and AParamValues.ContainsKey(PD.Name) then
        AQuery.ParamByName(PD.Name).Value := AParamValues[PD.Name];
    end;
end;

procedure FillDataSetFields(
  ADataSet: TDataSet;
  AProcedureDescription: TProcedureDescription
);
var
  P: TParamDescription;
  F: TField;
begin
  for P in AProcedureDescription.Params.Values do
    if P.ParamDirection = pdField then
    begin
      F := ADataset.FindField(P.Name);
      if Assigned(F) then
      begin
        F.DisplayLabel := P.DisplayLabel;
        F.Visible := P.Visible;
      end;
    end;
end;

{ TDirectConnection }

procedure TDirectConnection.AfterConnect;
begin
  ;
end;

procedure TDirectConnection.BeforeConnect;
begin
  ;
end;

procedure TDirectConnection.Connect;
begin
  Assert(not GetConnection.Connected);
  BeforeConnect;
  FConnection.Open(FConnectionString);
  AfterConnect;
end;

destructor TDirectConnection.Destroy;
begin
  FConnection.Free;
  inherited;
end;

procedure TDirectConnection.Disconnect;
begin
  Assert(GetConnection.Connected);
  FConnection.Close;
end;

function TDirectConnection.GetConnection: TADConnection;
begin
  Result := FConnection;
end;

function TDirectConnection.GetConnectionString: string;
begin
  Result := FConnectionString;
end;

class function TDirectConnection.NewInstance: TObject;
var
  C: TADConnection;
begin
  Result := inherited NewInstance;
  C := TADConnection.Create(nil);
  C.TxOptions.AutoCommit := false;
  C.TxOptions.AutoStart := false;
  C.TxOptions.AutoStop := false;

  (Result as TDirectConnection).FConnection := C;
end;

procedure TDirectConnection.SetConnectionString(const AConnectionString: string);
begin
  Assert(not GetConnection.Connected);
  FConnectionString := AConnectionString;
end;

function TDirectConnection.StartTransaction: ITransaction;
begin
  Assert(GetConnection.Connected);
  Result := GetDirectTransactionClass.Create(GetConnection);
end;

{ TDirectTransaction }

procedure TDirectTransaction.Commit;
begin
  LogTransactionAction('commit');
end;

procedure TDirectTransaction.CommitRetaining;
begin
  LogTransactionAction('commit retaining');
end;

constructor TDirectTransaction.Create(AConnection: TADConnection);
begin
  FTransaction := TADTransaction.Create(nil);
  FTransaction.Options.AutoStop := false;
  FTransaction.Options.AutoStart := false;
  FTransaction.Options.AutoCommit := false;
  FTransaction.Connection := AConnection;

  FCmd := TADCommand.Create(nil);
  FCmd.Connection := AConnection;
  FCmd.Transaction := FTransaction;

  FTransaction.StartTransaction;
  LogTransactionAction('transaction started');
end;

destructor TDirectTransaction.Destroy;
begin
  FTransaction.Free;
  FCmd.Free;
  inherited;
end;

procedure LogProcedureCall(
  Msg: string;
  const AProcedure: TProcedureDescription;
  const AParamValues: TParamValues
);
var
  ParamValue: variant;
  ParamName: string;
begin
  GLogDetails.Clear;
  GLogDetails.Add(AProcedure.ProcedureName);
  GLogDetails.Add('');
  if Assigned(AParamValues) then
    for ParamName in AParamValues.Keys do
    begin
      try
        if VarIsNull(AParamValues[ParamName]) then
          ParamValue := ''
        else
          VarCast(ParamValue, AParamValues[ParamName], varString);
      except
        ParamValue := '';
      end;
      GLogDetails.Add(Format('%s = %s', [ParamName, ParamValue]));
    end;
  CodeSite.Send(Msg, GLogDetails);
end;

procedure TDirectTransaction.ExecSql(ASql: string);
begin
  FCmd.CommandText.Text := ASql;
  FCmd.Execute();
end;

procedure TDirectTransaction.ExecuteProcedure(
  const AProcedure: TProcedureDescription; const AParamValues: TParamValues);
begin
  LogProcedureCall('exec ' + AProcedure.Name, AProcedure, AParamValues);
end;

function TDirectTransaction.GetTransaction: TADTransaction;
begin
  Result := FTransaction;
end;

procedure TDirectTransaction.LogTransactionAction(AAction: string);
begin
  CodeSite.Send(AAction, Integer(GetTransaction));
end;

procedure TDirectTransaction.MakeSavepoint(const AName: string);
begin
  LogTransactionAction('savepoint ' + AName);
end;

procedure TDirectTransaction.QueryData(
  const AProcedure: TProcedureDescription;
  const AParamValues: TParamValues;
  const AData: TADMemTable;
  const ACacheData: boolean
);
begin
  LogProcedureCall('open ' + AProcedure.Name, AProcedure, AParamValues);
end;

procedure TDirectTransaction.Rollback;
begin
  LogTransactionAction('rollback');
end;

procedure TDirectTransaction.RollbackRetaining;
begin
  CodeSite.Send('rollback retaining');
end;

procedure TDirectTransaction.RollbackToSavepoint(const AName: string);
begin
  LogTransactionAction('rollback to savepoint ' + AName);
end;

procedure TDirectTransaction.SetCacheChanged(const Value: TCacheChanged);
begin
  ;
end;

initialization
  GLogDetails := TStringList.Create;

finalization
  GLogDetails.Free;

end.
