unit Avk.DX.LookupFilter;

interface

uses
  Controls,
  cxDBLookupComboBox, cxDropDownEdit, cxDBData, cxCustomData, cxFilter;

// Установка фильтра для полей lookup-а, с разбивкой по словам и полям
procedure ApplySearchFilter(
  Controller: TcxDBDataController;
  Fields: string;
  Text: string
);

procedure SetFilterToLookups(ParentControl: TWinControl);

implementation

uses
  Classes,
  cxLookupEdit,
  AVK.Core.Utils;

type
  TStringsHolder = class (TObject)
  public
    FieldList: TStrings;
    TextWords: TStrings;
    constructor Create;
    destructor Destroy; override;

    procedure OnChange(Sender: TObject);
    procedure OnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

var
  SH: TStringsHolder;

procedure SetFilterToLookups(ParentControl: TWinControl);
var
  i: integer;
  C: TcxCustomLookupEdit;
  P: TcxLookupComboBoxProperties;
begin
  for i := 0 to ParentControl.ControlCount - 1 do
    if ParentControl.Controls[i] is TcxCustomLookupEdit then
    begin
      C := ParentControl.Controls[i] as TcxCustomLookupEdit;
      if not Assigned(C.Properties.OnChange) then
      begin
        C.OnKeyUp := SH.OnKeyUp;
        P := C.Properties as TcxLookupComboBoxProperties;
        P.OnChange := SH.OnChange;
        P.IncrementalFiltering := false;
        P.DropDownListStyle := lsEditList;
      end;
    end;
end;

{ TStringsHolder }

constructor TStringsHolder.Create;
begin
  inherited;
  FieldList := TStringList.Create;
  TextWords := TStringList.Create;
end;

destructor TStringsHolder.Destroy;
begin
  FieldList.Free;
  TextWords.Free;
  inherited;
end;

procedure ApplySearchFilter(
  Controller: TcxDBDataController;
  Fields: string;
  Text: string
);
var
  i, j: integer;
  ItemLink: TObject;
  Filter: TcxDataFilterCriteria;
  FL: TcxFilterCriteriaItemList;
begin
  Filter := Controller.Filter;
  Filter.BeginUpdate;
  try
    Filter.Active := false;
    Filter.Clear;
    Filter.Options := Filter.Options + [fcoCaseInsensitive];
    Filter.Root.BoolOperatorKind := fboOr;
    SplitDelimitedString(SH.FieldList, Fields, ';');
    SplitDelimitedString(SH.TextWords, Text, ' ');
    for i := 0 to SH.FieldList.Count - 1 do
      if SH.FieldList[i] <> '' then
      begin
        FL := Filter.Root.AddItemList(fboAnd);
        ItemLink := Controller.GetItemByFieldName(SH.FieldList[i]);
        if Assigned(ItemLink) then
          for j := 0 to SH.TextWords.Count - 1 do
            if SH.TextWords[j] <> '' then
              FL.AddItem(ItemLink, foLike, '%' + SH.TextWords[j] + '%', SH.TextWords[j]);
      end;
    Filter.Active := true;
  finally
    Filter.EndUpdate;
  end;
end;

procedure TStringsHolder.OnChange(Sender: TObject);
var
  S: TcxCustomLookupEdit;
begin
  S := Sender as TcxCustomLookupEdit;
  ApplySearchFilter(
    (S.Properties as TcxLookupComboBoxProperties).DataController,
    (S.Properties as TcxLookupComboBoxProperties).ListFieldNames,
    Copy(S.Text, 1, S.SelStart)
  );
end;

procedure TStringsHolder.OnKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  (Sender as TcxCustomLookupEdit).Properties.OnChange(Sender);
end;

initialization
  SH := TStringsHolder.Create;

finalization
  SH.Free;

end.
