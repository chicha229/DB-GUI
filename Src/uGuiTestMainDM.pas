unit uGuiTestMainDM;

interface

uses
  System.SysUtils, System.Classes, Avk.Gui.CustomMainDM, uADGUIxIntf,
  uADStanIntf, uADStanOption, uADStanError, uADPhysIntf,
  uADStanDef, uADStanPool, uADStanAsync, uADPhysManager, uADPhysOracle, Data.DB,
  uADCompClient, uADCompGUIx, uADGUIxFormsWait, uADPhysIB, Vcl.ImgList,
  Vcl.Controls, cxStyles, cxClasses, cxContainer, cxEdit;

type
  TTestsMainDataModule = class (TCustomMainDataModule)
    ADPhysOracleDriverLink1: TADPhysOracleDriverLink;
    ADPhysIBDriverLink1: TADPhysIBDriverLink;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function GetConnectionMode: string; override;
  end;

var
  TestsMainDataModule: TTestsMainDataModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  Avk.Gui.FireBirdDirectConnect;

procedure TTestsMainDataModule.DataModuleCreate(Sender: TObject);
begin
  inherited;
  TestsMainDataModule := Self;
end;

function TTestsMainDataModule.GetConnectionMode: string;
begin
  Result := FireBirdConnectMode;
end;

end.
