CREATE OR ALTER PROCEDURE UI$GET_TABLE_FIELDS_STR (
    I_TABLE_NAME D_IDENT,
    I_FIELD_PREFIX VARCHAR(10),
    I_ADD_DATA_TYPE D_BOOLEAN,
    I_KEY_ONLY D_BOOLEAN = 0)
RETURNS (
    O_FIELDS VARCHAR(1000))
AS
  declare variable l_field varchar(1000);
  declare variable l_index_on_key smallint;
begin
  o_fields = '';
  for
    select
      case
        when :i_field_prefix is not null then :i_field_prefix || f.field_name
        else f.field_name
      end ||
      case
        when :i_add_data_type = 1 then ' ' || f.field_type
        else ''
      end,
      f.index_on_key
    from ui$get_table_fields(:i_table_name) f
    order by f.order_num
    into :l_field, :l_index_on_key
  do
    if (i_key_only = 0 or l_index_on_key is not null) then
      o_fields = o_fields || l_field ||  ',';
  if (o_fields <> '') then
    o_fields = substring(o_fields from 1 for char_length(o_fields) - 1);
end