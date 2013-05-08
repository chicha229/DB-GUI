CREATE OR ALTER PROCEDURE UI$GET_TABLE_LINKS (
    I_TABLE_NAME D_IDENT)
RETURNS (
    FK_NAME D_IDENT,
    LINKS_TO_TABLE D_IDENT,
    LINKS_TO_PROCEDURE D_IDENT,
    FIELD_NAME D_IDENT,
    LINKS_TO_FIELD D_IDENT,
    FIELD_POSITION SMALLINT,
    LINK_INDEX SMALLINT)
AS
  declare variable l_last_fk_name d_ident;
begin
  link_index = 0;
  l_last_fk_name = '';
  for
    select
      trim(c.rdb$constraint_name),
      trim(cc.rdb$relation_name),
      p.id,
      trim(f.rdb$field_name),
      trim(lf.rdb$field_name),
      f.rdb$field_position + 1
    from rdb$relation_constraints c
    join rdb$ref_constraints rc on rc.rdb$constraint_name = c.rdb$constraint_name
    join rdb$relation_constraints cc on cc.rdb$constraint_name = rc.rdb$const_name_uq
    left join ui$procedure p on p.is_grid_for_table = cc.rdb$relation_name
    join rdb$index_segments f on f.rdb$index_name = c.rdb$index_name
    join rdb$index_segments lf on lf.rdb$index_name = rc.rdb$const_name_uq and lf.rdb$field_position = f.rdb$field_position
    where
      c.rdb$constraint_type = 'FOREIGN KEY' and
      c.rdb$relation_name = :i_table_name and
      1=1
    order by c.rdb$constraint_name, lf.rdb$field_position
    into :fk_name, :links_to_table, :links_to_procedure, :field_name, :links_to_field, :field_position
  do
  begin
    if (fk_name <> l_last_fk_name) then
    begin
      link_index = link_index + 1;
      l_last_fk_name = fk_name;
    end
    suspend;
  end
end