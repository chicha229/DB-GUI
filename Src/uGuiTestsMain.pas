unit uGuiTestsMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Avk.Gui.CustomMainForm, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxPCdxBarPopupMenu, cxPC,
  Avk.Gui.CustomMainDM, cxSplitter, dxSkinsCore, dxSkinsDefaultPainters,
  dxSkinscxPCPainter;

type
  TGuiTestMainForm = class (TCustomMainForm)
  private
    { Private declarations }
  public
    { Public declarations }
    class function GetMainDMClass: TCustomMainDataModuleClass; override;
  end;

var
  GuiTestMainForm: TGuiTestMainForm;

implementation

{$R *.dfm}

uses
  uGuiTestMainDM;

{ TGuiTestMainForm }

class function TGuiTestMainForm.GetMainDMClass: TCustomMainDataModuleClass;
begin
  Result := TTestsMainDataModule;
end;

end.
