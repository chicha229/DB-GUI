CREATE OR ALTER PROCEDURE UI$CR_BLOCK_ACTION
RETURNS (
    ID D_IDENT,
    "BLOCK" D_IDENT,
    CAPTION D_NAME,
    LINKS_TO D_IDENT,
    ACTION_STYLE D_IDENT,
    IMAGE_INDEX INTEGER,
    REFRESH_MODE D_IDENT,
    ORDER_NUM SMALLINT)
AS
begin
  for
    select
      a.id, a."BLOCK", a.caption, a.links_to, a.action_style, a.image_index,
      a.refresh_mode, a.order_num
    from ui$block_action a
    order by a."BLOCK", a.order_num
    into
      :id, :"BLOCK", :caption, :links_to, :action_style, :image_index,
      :refresh_mode, :order_num
  do
    suspend;
end