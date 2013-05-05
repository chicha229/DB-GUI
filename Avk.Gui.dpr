program Avk.Gui;

uses
  Vcl.Forms,
  Avk.Gui.Descriptions in 'Src\Avk.Gui.Descriptions.pas',
  Avk.Gui.BaseFrame in 'Src\Avk.Gui.BaseFrame.pas' {BaseFrame: TFrame},
  Avk.Gui.BlockFrame in 'Src\Avk.Gui.BlockFrame.pas' {BlockFrame: TFrame},
  uUtils in '..\_Core\uUtils.pas',
  Avk.Gui.CustomMainDM in 'Src\Avk.Gui.CustomMainDM.pas' {CustomMainDataModule: TDataModule},
  uGuiTestMainDM in 'Src\uGuiTestMainDM.pas' {TestsMainDataModule: TDataModule},
  Avk.Gui.ProcedureFrame in 'Src\Avk.Gui.ProcedureFrame.pas' {ProcedureFrame: TFrame},
  Avk.Gui.DbDependend in 'Src\Avk.Gui.DbDependend.pas',
  Avk.Gui.FormFrame in 'Src\Avk.Gui.FormFrame.pas' {FormFrame: TFrame},
  Avk.Gui.CustomMainForm in 'Src\Avk.Gui.CustomMainForm.pas' {CustomMainForm},
  TabOrder in '..\_Core\TabOrder.pas',
  Avk.Gui.BaseForm in 'Src\Avk.Gui.BaseForm.pas' {BaseForm},
  Avk.Gui.CustomEditorForm in 'Src\Avk.Gui.CustomEditorForm.pas' {CustomEditorForm},
  Avk.Gui.ModalFrameForm in 'Src\Avk.Gui.ModalFrameForm.pas' {FrameModalForm},
  uDevExpressEditorsPost in '..\_Core\DE\uDevExpressEditorsPost.pas',
  uLookupFilter in '..\_Core\DE\uLookupFilter.pas',
  Avk.Core.Exceptions in 'Src\Avk.Core.Exceptions.pas',
  Avk.Gui.GlobalSettings in 'Src\Avk.Gui.GlobalSettings.pas',
  Avk.Gui.DescriptionsLoader in 'Src\Avk.Gui.DescriptionsLoader.pas' {DescriptionsLoaderDM: TDataModule},
  uGuiTestsMain in 'Src\uGuiTestsMain.pas' {GuiTestMainForm},
  Avk.Gui.FormsMenuFrame in 'Src\Avk.Gui.FormsMenuFrame.pas' {FormsMenuFrame: TFrame},
  Avk.Core.Helpers in 'Src\Avk.Core.Helpers.pas',
  Avk.Gui.SearchFieldsSelect in 'Src\Avk.Gui.SearchFieldsSelect.pas' {SelectSearchFieldsForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TGuiTestMainForm, GuiTestMainForm);
  Application.Run;
end.
