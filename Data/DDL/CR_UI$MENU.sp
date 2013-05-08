CREATE OR ALTER PROCEDURE CR_UI$MENU
RETURNS (
    ID INTEGER,
    NAME D_NAME,
    ITEM_TYPE D_IDENT,
    PARENT INTEGER,
    "BLOCK" D_IDENT,
    ORDER_NUM SMALLINT)
AS
begin
  for
    select mi.id, mi.name, mi.item_type, mi.parent, mi.block, mi.order_num
    from ui$menu_item mi
    order by mi.order_num
    into :id, :name, :item_type, :parent, :"BLOCK", :order_num
  do
    suspend;
end

COMMENT ON PROCEDURE CR_UI$MENU IS 
'Меню приложения';


COMMENT ON PARAMETER CR_UI$MENU.ID IS 
'Код';
COMMENT ON PARAMETER CR_UI$MENU.ITEM_TYPE IS 
'Тип пункта меню';
COMMENT ON PARAMETER CR_UI$MENU.NAME IS 
'Название';
COMMENT ON PARAMETER CR_UI$MENU.ORDER_NUM IS 
'Порядковый номер в папке';
COMMENT ON PARAMETER CR_UI$MENU.PARENT IS 
'Папка';