unit Avk.Gui.FirebirdDirectConnect;

interface

uses
  uADCompClient,
  Avk.Gui.Connection, Avk.Gui.DirectConnect, Avk.Gui.Descriptions;

type
  TFirebirdDirectTransaction = class (TDirectTransaction)
  private
    FQuery: TADQuery;
    procedure PrepareQuery(
      const AProcedure: TProcedureDescription;
      AParamValues: TParamValues
    );
  public
    constructor Create(AConnection: TADConnection); override;
    destructor Destroy; override;

    procedure MakeSavepoint(const AName: string); override;
    procedure RollbackToSavepoint(const AName: string); override;

    procedure Commit; override;
    procedure Rollback; override;

    procedure CommitRetaining; override;
    procedure RollbackRetaining; override;

    procedure QueryData(
      const AProcedure: TProcedureDescription;
      const AParamValues: TParamValues;
      const AData: TADMemTable;
      const ACacheData: boolean = false
    ); override;
    procedure ExecuteProcedure(
      const AProcedure: TProcedureDescription; const AParamValues: TParamValues
    ); override;
  end;

  TFirebirdDirectConnection = class (TDirectConnection)
  protected
    procedure BeforeConnect; override;
    function GetDirectTransactionClass: TDirectTransactionClass; override;
  end;

const
  FirebirdConnectMode: string = 'firebird_connect_mode';

implementation

uses
  SysUtils,
  Avk.Core.Utils;

{ TFirebirdDirectConnection }

procedure TFirebirdDirectConnection.BeforeConnect;
begin
  inherited;
  GetConnection.DriverName := 'IB';
end;

function TFirebirdDirectConnection.GetDirectTransactionClass: TDirectTransactionClass;
begin
  Result := TFirebirdDirectTransaction;
end;

{ TFirebirdDirectTransaction }

procedure TFirebirdDirectTransaction.Commit;
begin
  inherited;
  if GetTransaction.Active then
    GetTransaction.Commit;
end;

procedure TFirebirdDirectTransaction.CommitRetaining;
begin
  inherited;
  if GetTransaction.Active then
    GetTransaction.CommitRetaining;
end;

constructor TFirebirdDirectTransaction.Create;
begin
  inherited;
  FQuery := TADQuery.Create(nil);
  FQuery.Transaction := GetTransaction;
  FQuery.Connection := GetTransaction.Connection;
end;

destructor TFirebirdDirectTransaction.Destroy;
begin
  FQuery.Free;
  inherited;
end;

procedure TFirebirdDirectTransaction.ExecuteProcedure(
  const AProcedure: TProcedureDescription; const AParamValues: TParamValues);
var
  P: TParamDescription;
  OutParamExists: boolean;
begin
  inherited;
  Assert(not AProcedure.IsDataSet);
  PrepareQuery(AProcedure, AParamValues);

  OutParamExists := false;
  for P in AProcedure.Params.Values do
    if P.ParamDirection = pdOut then
    begin
      OutParamExists := true;
      Break;
    end;

  if OutParamExists then
    FQuery.Open
  else
    FQuery.Execute;

  FillParamsFromQuery(FQuery, AProcedure, AParamValues);

  for P in AProcedure.Params.Values do
    if P.ParamDirection = pdOut then
      AParamValues.AddOrSetValue(P.Name, FQuery[P.Name]);
end;

procedure TFirebirdDirectTransaction.MakeSavepoint(const AName: string);
begin
  inherited;
  ExecSQL('savepoint ' + AName);
end;

procedure TFirebirdDirectTransaction.PrepareQuery(
  const AProcedure: TProcedureDescription; AParamValues: TParamValues);
var
  QueryText: string;
  QueryParams: string;
  PD: TParamDescription;
begin
  if AProcedure.IsDataSet then
    QueryText := 'select * from ' + AProcedure.ProcedureName
  else
    QueryText := 'execute procedure ' + AProcedure.ProcedureName;

  QueryParams := '';
  for PD in AProcedure.CallSortedParams do
  begin
    if PD.ParamDirection = pdIn then
    begin
      QueryParams := DelimitedConcat(
        QueryParams,
        Format(':%s', [PD.Name]),
        ','
      );
    end;
    if PD.ParamDirection = pdCursor then
      raise Exception.Create(
        '� �������� � Firebird �� ������ ���� ��������� ���� ������'
      );
  end;
  if QueryParams <> '' then
    QueryText := QueryText + '(' + QueryParams + ')';

  FQuery.SQL.Text := QueryText;
  FillQueryParams(FQuery, AProcedure, AParamValues);
end;

procedure TFirebirdDirectTransaction.QueryData(
  const AProcedure: TProcedureDescription;
  const AParamValues: TParamValues;
  const AData: TADMemTable;
  const ACacheData: boolean
);
begin
  inherited;
  Assert(AProcedure.IsDataSet);
  PrepareQuery(AProcedure, AParamValues);
  FQuery.Open;
  try
    AData.CloneCursor(FQuery);
    FillDataSetFields(AData, AProcedure);
  finally
    FQuery.Close;
  end;
end;

procedure TFirebirdDirectTransaction.Rollback;
begin
  inherited;
  if GetTransaction.Active then
    GetTransaction.Rollback;
end;

procedure TFirebirdDirectTransaction.RollbackRetaining;
begin
  inherited;
  if GetTransaction.Active then
    GetTransaction.RollbackRetaining
end;

procedure TFirebirdDirectTransaction.RollbackToSavepoint(const AName: string);
begin
  inherited;
  ExecSQL('rollback to ' + AName);
end;

initialization
  TConnectionFactory.RegisterConnectionImplementation(
    FirebirdConnectMode, TFirebirdDirectConnection
  );

end.
