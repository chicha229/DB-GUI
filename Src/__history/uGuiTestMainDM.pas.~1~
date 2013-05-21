unit uGuiTestMainDM;

interface

uses
  System.SysUtils, System.Classes, Avk.Gui.CustomMainDM, uADGUIxIntf,
  uADGUIxConsoleWait, uADStanIntf, uADStanOption, uADStanError, uADPhysIntf,
  uADStanDef, uADStanPool, uADStanAsync, uADPhysManager, uADPhysOracle, Data.DB,
  uADCompClient, uADCompGUIx, uADGUIxFormsWait, uADPhysIB, Vcl.ImgList,
  Vcl.Controls, Avk.Gui.DbDependend, cxStyles, cxClasses, cxContainer, cxEdit;

type
  TTestsMainDataModule = class (TCustomMainDataModule)
    ADPhysOracleDriverLink1: TADPhysOracleDriverLink;
    ADPhysIBDriverLink1: TADPhysIBDriverLink;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function GetDBType: TSupportedDB; override;
  end;

var
  TestsMainDataModule: TTestsMainDataModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TTestsMainDataModule.DataModuleCreate(Sender: TObject);
begin
  inherited;
  TestsMainDataModule := Self;
end;

function TTestsMainDataModule.GetDBType: TSupportedDB;
begin
  Result := sdbFirebird;
end;

end.
