CREATE OR ALTER PROCEDURE UI$GET_TABLE_FIELDS (
    I_TABLE_NAME D_IDENT)
RETURNS (
    FIELD_NAME D_IDENT,
    FIELD_TYPE D_NAME,
    FIELD_DESCRIPTION VARCHAR(1000),
    FIELD_TYPE_FB INTEGER,
    FIELD_TYPE_NAME D_IDENT,
    INDEX_ON_KEY INTEGER,
    ORDER_NUM SMALLINT)
AS
begin
  for
    select
      trim(f.rdb$field_name),
      trim(t.type_name),
      trim(f.rdb$description),
      trim(ff.rdb$field_type),
      dt.id,
      pf.rdb$field_position + 1,
      f.rdb$field_position
    from rdb$relation_fields f
    join rdb$fields ff on ff.rdb$field_name = f.rdb$field_source
    join rdb$relation_constraints pc on pc.rdb$relation_name = f.rdb$relation_name and pc.rdb$constraint_type = 'PRIMARY KEY'
    left join rdb$index_segments pf on pf.rdb$index_name = pc.rdb$index_name and pf.rdb$field_name = f.rdb$field_name
    left join ui$get_field_type_name(f.rdb$field_source) t on 1=1
    left join ui$field_type_fb ft on ft.fb_type_id = ff.rdb$field_type
    left join ui$data_type dt on dt.id = ft.data_type
    where
      f.rdb$relation_name = :i_table_name and
      1=1
    order by f.rdb$field_position
    into
      :field_name, :field_type, :field_description,
      :field_type_fb, :field_type_name, :index_on_key, :order_num
  do
    suspend;
end