unit Avk.Gui.Connection;

interface

uses
  Generics.Collections,
  uADCompClient,
  Avk.Gui.Descriptions;

{
вопросы:
  формат строки соединения - описать в зависимости от типа соединения
}

type
  ITransaction = interface (IUnknown)
    ['{B6830EA5-6802-4983-A570-57930EEB234A}']

    // эти две завершают транзакцию
    procedure Commit;
    procedure Rollback;

    procedure CommitRetaining;
    procedure RollbackRetaining;

    procedure MakeSavepoint(const AName: string);
    procedure RollbackToSavepoint(const AName: string);

    procedure QueryData(
      const AProcedure: TProcedureDescription;
      const AParamValues: TParamValues;
      const AData: TADMemTable
    );
    procedure ExecuteProcedure(
      const AProcedure: TProcedureDescription; const AParamValues: TParamValues
    );
  end;

  IConnection = interface (IUnknown)
    ['{40F7EC9B-8C74-4675-AAEB-5315A4C28EAA}']

    procedure SetConnectionString(const AConnectionString: string);

    procedure Connect;
    procedure Disconnect;

    function StartTransaction: ITransaction;
  end;

  TConnectionFactory = class (TObject)
  private
    FImplementations: TDictionary<string, TClass>;
  public
    constructor Create;
    destructor Destroy; override;

    class procedure RegisterConnectionImplementation(
      const AConnectMode: string; AImplementationClass: TClass
    );
    class function CreateConnection(const AConnectMode: string): IConnection;
  end;

implementation

uses
  SysUtils;

var
  FConnectionFactory: TConnectionFactory;

function ConnectionFactory: TConnectionFactory;
begin
  if not Assigned(FConnectionFactory) then
    FConnectionFactory := TConnectionFactory.Create;
  Result := FConnectionFactory;
end;

{ TConnectionFactory }

constructor TConnectionFactory.Create;
begin
  if Assigned(FConnectionFactory) then
    raise Exception.Create('Cannot create second instance of TConnectionFactory');
  FImplementations := TDictionary<string, TClass>.Create;
end;

class function TConnectionFactory.CreateConnection(
  const AConnectMode: string): IConnection;
begin
  Supports(
    ConnectionFactory.FImplementations.Items[AConnectMode].Create,
    IConnection,
    Result
  );
end;

destructor TConnectionFactory.Destroy;
begin
  FImplementations.Free;
  inherited;
end;

class procedure TConnectionFactory.RegisterConnectionImplementation(
  const AConnectMode: string; AImplementationClass: TClass);
begin
  if not Supports(AImplementationClass, IConnection) then
    raise Exception.CreateFmt('class %s not implements IConnection!', [AImplementationClass.ClassName]);

  ConnectionFactory.FImplementations.Add(AConnectMode, AImplementationClass);
end;

end.
