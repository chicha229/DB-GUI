unit Avk.Gui.SearchFieldsSelect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Avk.Gui.CustomEditorForm, Menus, cxLookAndFeelPainters, cxControls,
  cxContainer, cxEdit, cxGroupBox, StdCtrls, cxButtons, ExtCtrls, cxStyles,
  cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage, cxCheckBox,
  cxGridCustomTableView, cxGridTableView, cxGridCustomView, cxClasses,
  cxGridLevel, cxGrid, cxLookAndFeels, uFormErrors, Generics.Collections;

type
  TFieldDescription = class (TObject)
  public
    FieldName: string;
    DisplayLabel: string;
    SearchEnabled: boolean;
  end;

  TFieldsDescription = TObjectDictionary<string, TFieldDescription>;

  TSelectSearchFieldsForm = class (TCustomEditorForm)
    FieldsGridLevel1: TcxGridLevel;
    FieldsGrid: TcxGrid;
    FieldsGridTableView: TcxGridTableView;
    GridFieldColumn: TcxGridColumn;
    GridSearchColumn: TcxGridColumn;
    GridFieldNameColumn: TcxGridColumn;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FFields: TFieldsDescription;
    FDC: TcxGridDataController;

    procedure SaveField(ARowIndex: Integer; ARowInfo: TcxRowInfo);
  public
    { Public declarations }
    class function Execute(AFields: TFieldsDescription): boolean;

    procedure SaveChanges; override;
    function  ConfirmCancel: boolean; override;
    function  Modified: boolean; override;
  end;

implementation

{$R *.dfm}

{ TSelectSearchFieldsForm }

class function TSelectSearchFieldsForm.Execute(AFields: TFieldsDescription): boolean;
var
  F: TSelectSearchFieldsForm;
begin
  F := TSelectSearchFieldsForm.Create(Application);
  try
    F.FFields := AFields;
    Result := F.ShowModal = mrOk;
  finally
    F.Free;
  end;
end;

procedure TSelectSearchFieldsForm.FormShow(Sender: TObject);
var
  RecordIndex: integer;
  F: TFieldDescription;
begin
  inherited;
  for F in FFields.Values do
  begin
    RecordIndex := FDC.AppendRecord;
    FDC.Values[RecordIndex, GridFieldNameColumn.Index] := F.FieldName;
    FDC.Values[RecordIndex, GridFieldColumn.Index] := F.DisplayLabel;
    FDC.Values[RecordIndex, GridSearchColumn.Index] := F.SearchEnabled;
  end;
end;

procedure TSelectSearchFieldsForm.FormCreate(Sender: TObject);
begin
  inherited;
  FDC := FieldsGridTableView.DataController;
  FreeOnClose := false;
end;

function TSelectSearchFieldsForm.ConfirmCancel: boolean;
begin
  Result := False;
end;

function TSelectSearchFieldsForm.Modified: boolean;
begin
  Result := True;
end;

procedure TSelectSearchFieldsForm.SaveField(ARowIndex: Integer; ARowInfo: TcxRowInfo);
var
  FieldName: string;
begin
  FieldName :=
    FDC.Values[ARowInfo.RecordIndex, GridFieldNameColumn.Index];
  FFields[FieldName].SearchEnabled :=
    FDC.Values[ARowInfo.RecordIndex, GridSearchColumn.Index];
end;

procedure TSelectSearchFieldsForm.SaveChanges;
begin
  inherited;
  FDC.ForEachRow(false, SaveField);
end;

end.
 