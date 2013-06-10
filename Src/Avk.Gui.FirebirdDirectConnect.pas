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
      AParams: TParamValues
    );
  public
    constructor Create(AConnection: TADConnection); override;
    destructor Destroy; override;

    procedure MakeSavepoint(const AName: string); override;
    procedure RollbackToSavepoint(const AName: string); override;

    procedure QueryData(
      const AProcedure: TProcedureDescription;
      AParams: TParamValues;
      AData: TADMemTable
    ); override;
    procedure ExecuteProcedure(
      const AProcedure: TProcedureDescription; AParams: TParamValues
    ); override;
  end;

  TFirebirdDirectConnection = class (TDirectConnection)
  protected
    procedure AfterConnect; override;
    function GetDirectTransactionClass: TDirectTransactionClass; override;
  end;

const
  FirebirdConnectMode: string = 'firebird_connect_mode';

implementation

uses
  SysUtils,
  Avk.Core.Utils;

{ TFirebirdDirectConnection }

procedure TFirebirdDirectConnection.AfterConnect;
begin
  inherited;
  GetConnection.DriverName := 'IB';
end;

function TFirebirdDirectConnection.GetDirectTransactionClass: TDirectTransactionClass;
begin
  Result := TFirebirdDirectTransaction;
end;

{ TFirebirdDirectTransaction }

constructor TFirebirdDirectTransaction.Create;
begin
  inherited;
  FQuery := TADQuery.Create(nil);
  FQuery.Transaction := GetTransaction;
end;

destructor TFirebirdDirectTransaction.Destroy;
begin
  FQuery.Free;
  inherited;
end;

procedure TFirebirdDirectTransaction.ExecuteProcedure(
  const AProcedure: TProcedureDescription; AParams: TParamValues);
begin
  Assert(not AProcedure.IsDataSet);
  PrepareQuery(AProcedure, AParams);
  FQuery.Execute();
end;

procedure TFirebirdDirectTransaction.MakeSavepoint(const AName: string);
begin
  inherited;
  GetTransaction.Connection.ExecSQL('savepoint ' + AName);
end;

procedure TFirebirdDirectTransaction.PrepareQuery(
  const AProcedure: TProcedureDescription; AParams: TParamValues);
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
  for PD in AProcedure.SortedParams do
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
        'У процедур в Firebird не должно быть параметра типа курсор'
      );
  end;
  if QueryParams = '' then
    QueryText := QueryText + QueryParams
  else
    QueryText := QueryText + '(' + QueryParams + ')';

  FQuery.SQL.Text := QueryText;
  FillQueryParams(FQuery, AProcedure);
end;

procedure TFirebirdDirectTransaction.QueryData(
  const AProcedure: TProcedureDescription; AParams: TParamValues;
  AData: TADMemTable);
begin
  inherited;
  Assert(AProcedure.IsDataSet);
  PrepareQuery(AProcedure, AParams);
  FQuery.Open;
  try
    AData.CloneCursor(FQuery, true);
  finally
    FQuery.Close;
  end;
end;

procedure TFirebirdDirectTransaction.RollbackToSavepoint(const AName: string);
begin
  inherited;
  GetTransaction.Connection.ExecSQL('rollback to ' + AName);
end;

initialization
  TConnectionFactory.RegisterConnectionImplementation(
    FirebirdConnectMode, TFirebirdDirectConnection
  );

end.
