unit Avk.Gui.DbDependend;

interface

uses
  Avk.Gui.Descriptions, uADCompClient;

type
  TSupportedDB = (sdbFirebird, sdbOracle);

  TDBDependend = class (TObject)
  public
    procedure FillQuery(
      ProcedureDescription: TProcedureDescription;
      AQuery: TADQuery
    ); virtual; abstract;
  end;

  TDBDependendFactory = class (TObject)
  public
    class function CreateDependend(ADB: TSupportedDB): TDBDependend;
  end;


implementation

uses
  SysUtils, Data.DB, System.Generics.Collections,
  uADStanParam,
  AVK.Core.Utils;

type
  TFBDBDepended = class (TDBDependend)
    procedure FillQuery(
      ProcedureDescription: TProcedureDescription;
      AQuery: TADQuery
    ); override;
  end;

  TOracleDBDepended = class (TDBDependend)
    procedure FillQuery(
      ProcedureDescription: TProcedureDescription;
      AQuery: TADQuery
    ); override;
  end;

{ TDBDependendFactory }

class function TDBDependendFactory.CreateDependend(
  ADB: TSupportedDB): TDBDependend;
begin
  case ADB of
    sdbFirebird: Result := TFBDBDepended.Create;
    sdbOracle: Result := TOracleDBDepended.Create;
  else
    raise Exception.Create('DB dependend for this server is not registered');
  end;
end;

{ TFBDBDepended }

procedure TFBDBDepended.FillQuery(ProcedureDescription: TProcedureDescription;
  AQuery: TADQuery);
var
  QueryText: string;
  QueryParams: string;
  PD: TParamDescription;
  P: TADParam;
begin
  if ProcedureDescription.IsDataSet then
    QueryText := 'select * from ' + ProcedureDescription.ProcedureName
  else
    QueryText := 'execute procedure ' + ProcedureDescription.ProcedureName;

  QueryParams := '';

  for PD in ProcedureDescription.SortedParams do
    if PD.ParamDirection = pdIn then
    begin
      QueryParams := DelimitedConcat(
        QueryParams,
        Format(':%s', [PD.Name]),
        ','
      );
    end;
  if QueryParams = '' then
    QueryText := QueryText + QueryParams
  else
    QueryText := QueryText + '(' + QueryParams + ')';

  AQuery.SQL.Text := QueryText;
  for PD in ProcedureDescription.Params.Values do
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

{ TOracleDBDepended }

procedure TOracleDBDepended.FillQuery(
  ProcedureDescription: TProcedureDescription; AQuery: TADQuery);
var
  QueryText: string;
  QueryParams: string;
  PD: TParamDescription;
  P: TADParam;
begin
  QueryText := 'begin ' + ProcedureDescription.ProcedureName + '(';
  QueryParams := '';
  for PD in ProcedureDescription.Params.Values do
    if PD.ParamDirection <> pdField then
      QueryParams := DelimitedConcat(
        QueryParams,
        Format('%s => :%s', [PD.Name, PD.Name]),
        ','
      );
  QueryText := QueryText + QueryParams + '); end;';
  AQuery.SQL.Text := QueryText;
  for PD in ProcedureDescription.Params.Values do
    if PD.ParamDirection <> pdField then
    begin
      P := AQuery.ParamByName(PD.Name);
      P.DataType := PD.DataType;
      case PD.ParamDirection of
        pdIn: P.ParamType := ptInput;
        pdOut: P.ParamType := ptOutput;
        pdInOut: P.ParamType := ptInputOutput;
        pdCursor: P.ParamType := ptResult;
      end;
    end;
end;

end.
