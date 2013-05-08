CREATE OR ALTER PROCEDURE UI$CR_FORM_CHILD
RETURNS (
    ID INTEGER,
    FORM D_IDENT,
    "BLOCK" D_IDENT,
    CAPTION D_NAME,
    PANEL INTEGER,
    ORDER_NUM SMALLINT,
    PARAM_GROUP_ALIGN D_IDENT,
    PARAM_ALIGN D_IDENT,
    VISIBLE D_BOOLEAN)
AS
begin
  for
    select
      c.id, p.form, c.block, coalesce(c.caption, b.name), c.panel, c.order_num,
      c.param_group_align, c.param_align, c.visible
    from ui$form_child c
    join ui$form_panel p on p.id = c.panel
    join ui$block b on b.id = c.block
    order by p.form, c.panel, c.order_num
    into
      :id, :form, :block, :caption, :panel, :order_num,
      :param_group_align, :param_align, :visible
  do
    suspend;
end