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
    function GetDBDependend: TDBDependend;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
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

{$R *.dfm}

type
  TADQueryCrack = class (TADQuery)
  end;

constructor TCustomMainDataModule.Create(AOwner: TComponent);
begin
  inherited;
  FCommonRefs := TObjectDictionary<string, TDataSource>.Create([]);
end;

destructor TCustomMainDataModule.Destroy;
begin
  FCommonRefs.Free;
  inherited;
end;

function TCustomMainDataModule.GetDBDependend: TDBDependend;
begin
  if not Assigned(FDBDependend) then
    FDBDependend := TDBDependendFactory.CreateDependend(GetDBType);
  Result := FDBDependend;
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
    DBDependend.FillQuery(P, Q);
    Result := TDataSource.Create(Self);
    Result.DataSet := Q;
  end
  else
    Result := FCommonRefs[ARefBlockName];
end;

procedure TCustomMainDataModule.OnRefreshProcedure(AProcedureName: string);
begin
  if FCommonRefs.ContainsKey(AProcedureName) then
    FCommonRefs[AProcedureName].DataSet.Refresh;
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

