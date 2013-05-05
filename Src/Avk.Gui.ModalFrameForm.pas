unit Avk.Gui.ModalFrameForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Avk.GUI.CustomEditorForm, cxGraphics,
  cxLookAndFeels, cxLookAndFeelPainters, Vcl.Menus, cxControls, cxContainer,
  cxEdit, uFormErrors, cxGroupBox, Vcl.StdCtrls, cxButtons, Vcl.ExtCtrls,
  Avk.Gui.FormFrame, dxSkinsCore;

type
  TFrameModalForm = class (TCustomEditorForm)
  private
    FFrame: TFormFrame;
    procedure SetFrame(const Value: TFormFrame);
    { Private declarations }
  public
    { Public declarations }
    function  FormSectionName: string; override;

    procedure SaveChanges; override;
    procedure DropChanges; override;
    function  ConfirmCancel: boolean; override;
    function  Modified: boolean; override;
    procedure ValidateInput; override;

    property Frame: TFormFrame read FFrame write SetFrame;
  end;

implementation

{$R *.dfm}

{ TFrameModalForm }

function TFrameModalForm.ConfirmCancel: boolean;
begin
  Result := true;
end;

procedure TFrameModalForm.DropChanges;
begin
  inherited;

end;

function TFrameModalForm.FormSectionName: string;
begin
  if not Assigned(FFrame) then
    Result := inherited FormSectionName
  else
    Result := FFrame.FormDescription.Name + 'ModalForm';
end;

function TFrameModalForm.Modified: boolean;
begin
  // TODO: развернуть, вычислить!
  FFrame.EditorsToParamValues;
  Result := true;
end;

procedure TFrameModalForm.SaveChanges;
begin
  inherited;
  FFrame.Save;
end;

procedure TFrameModalForm.SetFrame(const Value: TFormFrame);
begin
  FFrame := Value;
  Caption := FFrame.FormDescription.DisplayLabel;
  LoadFormSettings;
end;

procedure TFrameModalForm.ValidateInput;
begin
  inherited;
  FFrame.ValidateInput;
end;

end.
