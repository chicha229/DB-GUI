unit Avk.Gui.DirectConnect;

interface

uses
  uADCompClient,
  Avk.Gui.Connection, Avk.Gui.Descriptions;

type
  TDirectTransaction = class (TInterfacedObject, ITransaction)
  private
    FTransaction: TADTransaction;
  protected
    function GetTransaction: TADTransaction;
  public
    constructor Create(AConnection: TADConnection); virtual;
    destructor Destroy; override;

    procedure Commit;
    procedure Rollback;

    procedure CommitRetaining;
    procedure RollbackRetaining;

    procedure MakeSavepoint(const AName: string); virtual; abstract;
    procedure RollbackToSavepoint(const AName: string); virtual; abstract;

    procedure QueryData(
      const AProcedure: TProcedureDescription;
      AParams: TParamValues;
      AData: TADMemTable
    ); virtual; abstract;
    procedure ExecuteProcedure(
      const AProcedure: TProcedureDescription; AParams: TParamValues
    ); virtual; abstract;
  end;

  TDirectTransactionClass = class of TDirectTransaction;

  TDirectConnection = class (TInterfacedObject, IConnection)
  private
    FConnectionString: string;
    FConnection: TADConnection;
  protected
    function GetConnectionString: string;
    function GetConnection: TADConnection;

    procedure AfterConnect; virtual;
    function GetDirectTransactionClass: TDirectTransactionClass; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetConnectionString(const AConnectionString: string);

    procedure Connect;
    procedure Disconnect;

    function StartTransaction: ITransaction;
  end;

procedure FillQueryParams(AQuery: TADQuery; AProcedure: TProcedureDescription);

implementation

uses
  DB,
  uADStanParam;

procedure FillQueryParams(AQuery: TADQuery; AProcedure: TProcedureDescription);
var
  P: TADParam;
  PD: TParamDescription;
begin
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
    end;
end;

{ TDirectConnection }

procedure TDirectConnection.AfterConnect;
begin
  ;
end;

procedure TDirectConnection.Connect;
begin
  FConnection.Open(FConnectionString);
  AfterConnect;
end;

constructor TDirectConnection.Create;
begin
  FConnection := TADConnection.Create(nil);
end;

destructor TDirectConnection.Destroy;
begin
  FConnection.Free;
  inherited;
end;

procedure TDirectConnection.Disconnect;
begin
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

procedure TDirectConnection.SetConnectionString(const AConnectionString: string);
begin
  FConnectionString := AConnectionString;
end;

function TDirectConnection.StartTransaction: ITransaction;
begin
  Result := GetDirectTransactionClass.Create(GetConnection);
end;

{ TDirectTransaction }

procedure TDirectTransaction.Commit;
begin
  FTransaction.Commit;
end;

procedure TDirectTransaction.CommitRetaining;
begin
  FTransaction.CommitRetaining;
end;

constructor TDirectTransaction.Create(AConnection: TADConnection);
begin
  FTransaction := TADTransaction.Create(nil);
  FTransaction.Connection := AConnection;
end;

destructor TDirectTransaction.Destroy;
begin
  FTransaction.Free;
  inherited;
end;

function TDirectTransaction.GetTransaction: TADTransaction;
begin
  Result := FTransaction;
end;

procedure TDirectTransaction.Rollback;
begin
  FTransaction.Rollback;
end;

procedure TDirectTransaction.RollbackRetaining;
begin
  FTransaction.RollbackRetaining;
end;

end.
