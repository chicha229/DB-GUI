CREATE OR ALTER PROCEDURE UI$ADD_TO_MENU (
    I_MENU_ITEM_PARENT INTEGER,
    I_MENU_ITEM_ORDER_NUM SMALLINT,
    I_BLOCK D_IDENT)
AS
begin
  insert into ui$menu_item (name, item_type, parent, "BLOCK", order_num)
    select b.name, 'block', :i_menu_item_parent, :i_block, :i_menu_item_order_num
    from ui$block b
    where b.id = :i_block;
end