unit Avk.DX.PostEditors;

interface

uses
  Controls, cxEdit;

type
  TcxCustomEditCrack = class (TcxCustomEdit)
  public
    property DataBinding;
  end;

procedure PostDevExpressEditorsValues(AParentControl: TWinControl);

implementation

procedure PostDevExpressEditorsValues(AParentControl: TWinControl);
var
  i: integer;
begin
  for i := 0 to AParentControl.ControlCount - 1 do
    if AParentControl.Controls[i] is TcxCustomEdit then
      with TcxCustomEditCrack(AParentControl.Controls[i]) do
        PostEditValue;
//        DoHideEdit(false);
end;

end.
