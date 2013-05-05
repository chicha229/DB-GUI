unit Avk.Gui.FormsMenuFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Avk.Gui.ProcedureFrame, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  cxCustomData, cxStyles, cxTL, cxTLdxBarBuiltInMenu, cxFilter, cxData,
  cxDataStorage, Data.DB, cxDBData, uADStanIntf, uADStanOption, uADStanParam,
  uADStanError, uADDatSManager, uADPhysIntf, uADDAptIntf, uADStanAsync,
  uADDAptManager, uADCompDataSet, uADCompClient, dxBar, cxClasses, uFormErrors,
  cxGridLevel, cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridBandedTableView, cxGridDBBandedTableView, cxGrid, cxInplaceContainer,
  cxTLData, cxDBTL, Vcl.ExtCtrls, cxLabel, cxMaskEdit, dxSkinsCore,
  cxCheckBox, cxTextEdit, cxButtonEdit, cxGroupBox;

type
  TFormsMenuFrame = class (TProcedureFrame)
    procedure TreeListDblClick(Sender: TObject);
    procedure OpenBarButtonClick(Sender: TObject);
  private
    procedure OpenBlock;
    { Private declarations }
  public
    { Public declarations }
    procedure Build(AParent: TWinControl); override;
    procedure AfterRefresh; override;
  end;

implementation

{$R *.dfm}

uses
  Avk.Gui.CustomMainDM, Avk.Gui.CustomMainForm,
  Avk.Gui.Descriptions, Avk.Gui.DescriptionsLoader;

{ TFormsMenuFrame }

procedure TFormsMenuFrame.AfterRefresh;
begin
  inherited;
  TDescriptionsLoaderDM.Execute(CustomMainDM.MainConnection);
end;

procedure TFormsMenuFrame.Build(AParent: TWinControl);
begin
  inherited Build(AParent);
  TopLabel.Hide;
end;

procedure TFormsMenuFrame.OpenBlock;
var
  BlockName: string;
begin
  BlockName := Query.FieldByName('block').AsString;
  if BlockName = '' then
    Exit;
  CustomMainForm.ShowBlock(BlocksManager.Blocks[BlockName], nil);
end;

procedure TFormsMenuFrame.OpenBarButtonClick(Sender: TObject);
begin
  inherited;
  OpenBlock;
end;

procedure TFormsMenuFrame.TreeListDblClick(Sender: TObject);
begin
  inherited;
  OpenBlock;
end;

initialization
  RegisterClass(TFormsMenuFrame);

end.
