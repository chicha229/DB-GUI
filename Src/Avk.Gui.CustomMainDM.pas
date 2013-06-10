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

  TCustomMainDataModule = class (TDataModule)
    ADGUIxWaitCursor1: TADGUIxWaitCursor;
    MainConnection: TADConnection;
    Images: TImageList;
    cxStyleRepository: TcxStyleRepository;
    GridHeaderStyle: TcxStyle;
    ReadOnlyEditStyleController: TcxEditStyleController;
  private
    FConnection: IConnection;
    FMainTransaction: ITransaction;
    FCommonRefs: TObjectDictionary<string, TDataSource>;
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
    function GetRefDataSource(ARefBlockName: string): TDataSource;
    procedure FillQueryFields(ADataset: TDataSet; ABlock: TBlockDescription);
    procedure OnRefreshProcedure(AProcedureName: string);
  end;

var
  CustomMainDM: TCustomMainDataModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses
  CodeSiteLogging, uADStanParam;

{$R *.dfm}

constructor TCustomMainDataModule.Create(AOwner: TComponent);
begin
  inherited;
  FCommonRefs := TObjectDictionary<string, TDataSource>.Create([]);
  FLogDetails := TStringList.Create;
end;

destructor TCustomMainDataModule.Destroy;
begin
  FCommonRefs.Free;
  FLogDetails.Free;
  inherited;
end;

function TCustomMainDataModule.GetConnection: IConnection;
begin
  if not Assigned(FConnection) then
    FConnection := TConnectionFactory.CreateConnection(GetConnectionMode);
  Result := FConnection;
end;

procedure TCustomMainDataModule.LogRefQueryOpen(Sender: TDataSet);
begin
  LogQueryOpen(Sender as TADQuery, 'ref query open');
end;

function TCustomMainDataModule.GetRefDataSource(
  ARefBlockName: string): TDataSource;
var
  D: TADMemTable;
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
    Result := TDataSource.Create(Self);
    Result.DataSet := D;
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
  DS: TDataSet;
begin
  DS := FCommonRefs[AProcedureName].DataSet;
  if FCommonRefs.ContainsKey(AProcedureName) and DS.Active then
  begin
    DS.Close;
    MainTransaction.QueryData(BlocksManager.Blocks[AProcedureName], nil, DS);
  end;
end;

{
procedure TCustomMainDataModule.LogQueryOpen(Q: TADQuery; Msg: string);
var
  i: Integer;
  ParamValue: string;
begin
  FLogDetails.Clear;
  FLogDetails.Add(Q.Sql.Text);
  for i := 0 to Q.Params.Count - 1 do
  begin
    try
      ParamValue := Q.Params[i].AsString;
    except
      ParamValue := '';
    end;
    FLogDetails.Add(Format('%s = %s', [Q.Params[i].Name, ParamValue]));
  end;
  CodeSite.Send(Msg, FLogDetails);
end;
}

procedure TCustomMainDataModule.FillQueryFields(ADataset: TDataSet; ABlock: TBlockDescription);
var
  P: TParamDescription;
  F: TField;
begin
  for P in ABlock.Params.Values do
    if P.ParamDirection = pdField then
    begin
      F := ADataset.FindField(P.Name);
      if Assigned(F) then
      begin
        F.DisplayLabel := P.DisplayLabel;
        F.Visible := P.Visible;
      end;
    end;
//  TADMemTable(ADataset).SetDefaultFields(false);
end;


end.

