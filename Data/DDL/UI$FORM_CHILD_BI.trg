CREATE OR ALTER TRIGGER UI$FORM_CHILD_BI FOR UI$FORM_CHILD
ACTIVE BEFORE INSERT POSITION 0
as
begin
  if (new.id is null) then
    new.id = gen_id(gen_ui$form_child_id,1);
end
^