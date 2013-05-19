unit Avk.Gui.CustomMainDM;

interface

uses
  System.SysUtils, System.Classes, uADGUIxIntf, uADGUIxConsoleWait, uADStanIntf,
  uADStanOption, uADStanError, uADPhysIntf, uADStanDef, uADStanPool,
  uADStanAsync, uADPhysManager, Data.DB, uADCompClient, uADPhysOracle,
  uADCompGUIx, uADGUIxFormsWait,
  Avk.Gui.DbDependend, Vcl.ImgList, Vcl.Controls, System.Generics.Collections,
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
    FDBDependend: TDBDependend;
    FCommonRefs: TObjectDictionary<string, TDataSource>;
    FLogDetails: TStrings;

    function GetDBDependend: TDBDependend;
    procedure LogRefQueryOpen(Sender: TDataSet);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
    procedure LogQueryOpen(Q: TADQuery; Msg: string);

    function GetDBType: TSupportedDB; virtual; abstract;
    property DBDependend: TDBDependend read GetDBDependend;
    function GetRefDataSource(ARefBlockName: string): TDataSource;
    procedure FillQueryFields(AQuery: TADQuery; ABlock: TBlockDescription);
    procedure OnRefreshProcedure(AProcedureName: string);
  end;

var
  CustomMainDM: TCustomMainDataModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses
  CodeSiteLogging, uADStanParam;

{$R *.dfm}

type
  TADQueryCrack = class (TADQuery)
  end;

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

function TCustomMainDataModule.GetDBDependend: TDBDependend;
begin
  if not Assigned(FDBDependend) then
    FDBDependend := TDBDependendFactory.CreateDependend(GetDBType);
  Result := FDBDependend;
end;

procedure TCustomMainDataModule.LogRefQueryOpen(Sender: TDataSet);
begin
  LogQueryOpen(Sender as TADQuery, 'ref query open');
end;

function TCustomMainDataModule.GetRefDataSource(
  ARefBlockName: string): TDataSource;
var
  Q: TADQuery;
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
    Q := TADQuery.Create(Self);
    Q.Connection := MainConnection;
    Q.AfterOpen := LogRefQueryOpen;
    DBDependend.FillQuery(P, Q);
    Result := TDataSource.Create(Self);
    Result.DataSet := Q;
    FCommonRefs.Add(ARefBlockName, Result);
  end
  else
    Result := FCommonRefs[ARefBlockName];
end;

procedure TCustomMainDataModule.OnRefreshProcedure(AProcedureName: string);
begin
  if FCommonRefs.ContainsKey(AProcedureName) then
    FCommonRefs[AProcedureName].DataSet.Refresh;
end;

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

procedure TCustomMainDataModule.FillQueryFields(AQuery: TADQuery; ABlock: TBlockDescription);
var
  P: TParamDescription;
  F: TField;
begin
  for P in ABlock.Params.Values do
    if P.ParamDirection = pdField then
    begin
      F := AQuery.FindField(P.Name);
      if Assigned(F) then
      begin
        F.DisplayLabel := P.DisplayLabel;
        F.Visible := P.Visible;
      end;
    end;
  TADQueryCrack(AQuery).SetDefaultFields(false);
end;


end.

