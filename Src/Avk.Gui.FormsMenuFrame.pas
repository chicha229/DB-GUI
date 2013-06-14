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
  cxCheckBox, cxTextEdit, cxButtonEdit, cxGroupBox, dxSkinsDefaultPainters,
  dxSkinscxPCPainter, dxSkinsdxBarPainter;

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
  Avk.Gui.CustomMainDM, Avk.Gui.CustomMainForm, Avk.Gui.Connection,
  Avk.Gui.Descriptions, Avk.Gui.DescriptionsLoader;

{ TFormsMenuFrame }

procedure TFormsMenuFrame.AfterRefresh;
begin
  inherited;
  TDescriptionsLoaderDM.Execute;
end;

procedure TFormsMenuFrame.Build(AParent: TWinControl);
begin
  ParamValues.AddOrSetValue('I_PROJECT', CustomMainForm.ProjectName);
  inherited Build(AParent);
  TopLabel.Hide;
end;

procedure TFormsMenuFrame.OpenBlock;
var
  T: ITransaction;
  BlockName: string;
begin
  BlockName := MemTable.FieldByName('block').AsString;
  if BlockName = '' then
    Exit;
  if BlocksManager.Blocks[BlockName].IsModal then
    T := CustomMainDM.Connection.StartTransaction
  else
    T := CustomMainDM.MainTransaction;

  CustomMainForm.ShowBlock(BlocksManager.Blocks[BlockName], T, true, nil);
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
