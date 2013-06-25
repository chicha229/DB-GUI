CREATE OR ALTER PROCEDURE UI$CR_BLOCK_PARAM
RETURNS (
    "BLOCK" D_IDENT,
    PARAM D_IDENT,
    PARAM_DIRECTION D_IDENT,
    DATA_TYPE D_IDENT,
    GROUP_NAME D_NAME,
    ENABLER_PARAM D_IDENT,
    CAPTION D_NAME,
    VISIBLE D_BOOLEAN,
    REQUIRED D_BOOLEAN,
    READ_ONLY D_BOOLEAN,
    INDEX_IN_KEY SMALLINT,
    INDEX_IN_PARENT SMALLINT,
    INDEX_IN_NAME SMALLINT,
    ORDER_NUM SMALLINT,
    CALL_ORDER_NUM SMALLINT,
    DEFAULT_VALUE VARCHAR(1000))
AS
begin
  for
    select
      p."BLOCK", p.param, p.param_direction, p.data_type,
      p.group_name, p.caption, p.enabler_param,
      p.visible, p.required, p.read_only,
      p.index_in_key, p.index_in_parent, p.index_in_name,
      p.order_num, p.call_order_num, p.default_value
    from ui$block_param p
    order by p."BLOCK", p.order_num
    into
      :"BLOCK", :param, :param_direction, :data_type,
      :group_name, :caption, :enabler_param,
      :visible, :required, :read_only,
      :index_in_key, :index_in_parent, :index_in_name,
      :order_num, :call_order_num, :default_value
  do
    suspend;
end