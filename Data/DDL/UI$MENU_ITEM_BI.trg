CREATE OR ALTER TRIGGER UI$MENU_ITEM_BI FOR UI$MENU_ITEM
ACTIVE BEFORE INSERT POSITION 0
as
begin
  if (new.id is null) then
    new.id = gen_id(gen_ui$menu_item_id,1);
end
^