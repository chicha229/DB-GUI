CREATE OR ALTER TRIGGER UI$FORM_PANEL_BI FOR UI$FORM_PANEL
ACTIVE BEFORE INSERT POSITION 0
as
begin
  if (new.id is null) then
    new.id = gen_id(gen_ui$form_panel_id,1);
end
^