unit Avk.Gui.CustomMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Avk.Gui.Descriptions, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxPCdxBarPopupMenu, cxPC,
  Avk.Gui.BlockFrame, Avk.Gui.CustomMainDM, cxSplitter, Avk.Gui.FormFrame,
  dxSkinsCore, dxSkinsDefaultPainters, dxSkinscxPCPainter,
  Avk.Gui.Connection;

type
  TCustomMainForm = class (TForm)
    FormsPageControl: TcxPageControl;
    TreeSplitter: TcxSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FGuiObjectsFrame: TBlockFrame;
  public
    { Public declarations }
    property GuiObjectsFrame: TBlockFrame read FGuiObjectsFrame;
    class function GetMainDMClass: TCustomMainDataModuleClass; virtual; abstract;

    function ProjectName: string; virtual;

    function CreateBlockFrame(
      ABlock: TBlockDescription;
      ATransaction: ITransaction;
      AIsTransactionStart: boolean;
      AOwner: TComponent
    ): TBlockFrame;
    function ShowBlock(
      ABlockDescription: TBlockDescription;
      ATransaction: ITransaction;
      AIsTransactionStart: boolean;
      AParamValues: TParamValues
    ): boolean;
  end;

var
  CustomMainForm: TCustomMainForm;

implementation

{$R *.dfm}

uses
  Avk.Gui.ProcedureFrame,
  Avk.Gui.ModalFrameForm,
  Avk.Gui.DescriptionsLoader;

{ TCustomMainForm }

procedure TCustomMainForm.FormCreate(Sender: TObject);
begin
  CustomMainForm := Self;
  CustomMainDM := Self.GetMainDMClass.Create(Application);
  CustomMainDM.Connection.Connect;
  TDescriptionsLoaderDM.Execute;

  FGuiObjectsFrame := CreateBlockFrame(
    BlocksManager.Blocks['UI$MENU_CR'],
    CustomMainDM.MainTransaction,
    false,
    Self
  );
  FGuiObjectsFrame.Build(Self);
  FGuiObjectsFrame.Open;
  FGuiObjectsFrame.Align := alLeft;
  FGuiObjectsFrame.Width := 200;
  TreeSplitter.Left := FGuiObjectsFrame.Width + 1;
  TreeSplitter.Control := FGuiObjectsFrame;
  FormsPageControl.Align := alClient;
end;

procedure TCustomMainForm.FormShow(Sender: TObject);
begin
  WindowState := wsMaximized;
end;

function TCustomMainForm.ProjectName: string;
begin
  Result := '';
end;

function TCustomMainForm.CreateBlockFrame(
  ABlock: TBlockDescription;
  ATransaction: ITransaction;
  AIsTransactionStart: boolean;
  AOwner: TComponent
): TBlockFrame;
var
  C: TClass;
begin
  if ABlock.CustomClassName <> '' then
  begin
    C := FindClass(ABlock.CustomClassName);
    if not Assigned(C) then
      raise Exception.CreateFmt('Cannot find custom class %s', [ABlock.CustomClassName]);
    Result := TBlockFrameClass(C).Create(AOwner)
  end
  else if ABlock is TProcedureDescription then
    Result := TProcedureFrame.Create(AOwner)
  else if ABlock is TFormDescription then
    Result := TFormFrame.Create(AOwner)
{
  else if ABlock is TBlockDescription then
    Result := TBlockFrame.Create(AOwner)
}
  else
    raise Exception.CreateFmt(
      'Cannot find frame to create from block %s',
      [ABlock.Name]
    );

  if Result is TProcedureFrame then
  begin
    (Result as TProcedureFrame).Transaction := ATransaction;
    (Result as TProcedureFrame).IsTransactionStart := AIsTransactionStart;
  end
  else if Result is TFormFrame then
  begin
    (Result as TFormFrame).Transaction := ATransaction;
    (Result as TFormFrame).IsTransactionStart := AIsTransactionStart;
  end;

  Result.Name := '';
  Result.BlockDescription := ABlock;
end;

function TCustomMainForm.ShowBlock(
  ABlockDescription: TBlockDescription;
  ATransaction: ITransaction;
  AIsTransactionStart: boolean;
  AParamValues: TParamValues
): boolean;
var
  T: TcxTabSheet;
  Frame: TBlockFrame;
  Form: TFrameModalForm;
  FrameParent: TWinControl;
begin
  Form := nil;
  Result := false;
  if ABlockDescription.GetIsModal then
  begin
    Form := TFrameModalForm.Create(Application);
    FrameParent := Form;
  end
  else
  begin
    T := TcxTabSheet.Create(FormsPageControl);
    T.PageControl := FormsPageControl;
    T.Caption := ABlockDescription.DisplayLabel;
    FrameParent := T;
    FormsPageControl.ActivePageIndex := T.TabIndex;
  end;
  Frame := CreateBlockFrame(
    ABlockDescription,
    ATransaction,
    AIsTransactionStart,
    FrameParent
  );
  if Assigned(AParamValues) then
    Frame.AssignParamValues(AParamValues);
  Frame.Build(FrameParent);
  Frame.Open;
  if Assigned(Form) then
  begin
    Form.Frame := Frame;
    Result := Form.ShowModal = mrOk;
    Form.Frame.PostEditorsValues;
    if Assigned(AParamValues) then
      Form.Frame.SaveParamValues(AParamValues);
  end;
end;

end.
