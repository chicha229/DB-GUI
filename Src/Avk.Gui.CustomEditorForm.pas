unit Avk.GUI.CustomEditorForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Avk.Gui.BaseForm, cxLookAndFeelPainters, StdCtrls, cxButtons, ExtCtrls,
  Menus, cxControls, cxContainer, cxEdit, cxGroupBox, uFormErrors,
  cxGraphics, cxLookAndFeels, dxSkinsCore, dxSkinsDefaultPainters;

type
  TCustomEditorForm = class (TBaseForm)
    BottomPanel: TPanel;
    ClientPanel: TPanel;
    ScrollBox: TScrollBox;
    CancelButton: TcxButton;
    OkButton: TcxButton;
    cxBottomLineGroupBox: TcxGroupBox;
    FormErrors: TFormErrors;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormErrorsRaiseError(Sender: TFormErrors; Errors: string);
  private
    FExitConfirmed: boolean;
    procedure ModalClose;
  public
    procedure PostEditors;

    function  HandleError(Err: Exception): boolean; virtual;
    procedure SaveChanges; virtual;
    procedure DropChanges; virtual;
    function  ConfirmCancel: boolean; virtual;
    function  Modified: boolean; virtual;
    procedure ValidateInput; virtual;
  end;

implementation

uses
  AVK.Core.Utils, AVK.DX.PostEditors, AVK.DX.LookupFilter, Avk.Core.Exceptions,
  Avk.Gui.GlobalSettings;

{$R *.dfm}

{ TCustomEditorForm }

procedure TCustomEditorForm.DropChanges;
begin
  ;
end;

function TCustomEditorForm.ConfirmCancel: boolean;
begin
  Result := true;
end;

function TCustomEditorForm.HandleError(Err: Exception): boolean;
begin
  Result := false;
end;

procedure TCustomEditorForm.ModalClose;
begin
  if fsModal in FormState then
  begin
    if ModalResult = mrNone then
      ModalResult := mrCancel;
  end
  else
    Close;
end;

function TCustomEditorForm.Modified: boolean;
begin
  // Сами-то они без потери фокуса не сохраняются
  PostEditors;
  Result := false;
end;

procedure TCustomEditorForm.PostEditors;
begin
  PostDevExpressEditorsValues(ScrollBox);
end;

procedure TCustomEditorForm.SaveChanges;
begin
  ;
end;

procedure TCustomEditorForm.OkButtonClick(Sender: TObject);
begin
  if not Modified then
  begin
    ModalClose;
    Exit;
  end;
  try
    ValidateInput;
    FormErrors.ShowErrors;
    SaveChanges;
    FExitConfirmed := true;
    ModalClose;
  except on Err: Exception do
    begin
      ModalResult := mrNone;
      if not HandleError(Err) then
        raise;
    end;
  end;
end;

procedure TCustomEditorForm.CancelButtonClick(Sender: TObject);
begin
  if Modified then
  begin
    if ConfirmCancel then
    begin
      if Confirm('Вы действительно хотите выйти без сохранения?') then
      begin
        DropChanges;
        FExitConfirmed := true;
        ModalClose;
      end
      else
        ModalResult := mrNone;
    end
    else
    begin
      DropChanges;
      FExitConfirmed := true;
      ModalClose;
    end;
  end
  else
    ModalClose;
end;

procedure TCustomEditorForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  inherited;
  if (not FExitConfirmed) and Modified and ConfirmCancel then
    case ConfirmYNC('Данные изменены. Сохранить изменения?') of
      mrYes:
        try
          SaveChanges;
          ModalResult := mrOk;
        except on Err: Exception do
          begin
            CanClose := false;
            if not HandleError(Err) then
              raise;
          end;
        end;
      mrNo:
        DropChanges;
      mrCancel:
        CanClose := false;
    end;
end;

procedure TCustomEditorForm.FormCreate(Sender: TObject);
begin
  inherited;
  if FullSearch then
    SetFilterToLookups(ScrollBox);
end;

procedure TCustomEditorForm.ValidateInput;
begin
  ;
end;

procedure TCustomEditorForm.FormErrorsRaiseError(Sender: TFormErrors;
  Errors: string);
begin
  inherited;
  raise EUserError.Create(Errors);
end;

end.
