CREATE OR ALTER PROCEDURE UI$GET_FIELD_TYPE_NAME (
    I_FIELD_NAME VARCHAR(30))
RETURNS (
    TYPE_NAME D_NAME)
AS
  declare variable l_type integer;
  declare variable l_sub_type integer;
  declare variable l_length integer;
  declare variable l_scale integer;
  declare variable l_prec integer;
begin
  select
    f.rdb$field_type,
    f.rdb$field_sub_type,
    -- utf!
    f.rdb$field_length / 4,
    f.rdb$field_scale,
    f.rdb$field_precision
    from rdb$fields f
    where f.rdb$field_name = :i_field_name
    into :l_type, :l_sub_type, :l_length, :l_scale, :l_prec;
  if (l_type = 261) then
    type_name = 'BLOB SUB_TYPE ' || :l_sub_type;
  else if (l_type = 45) then
    type_name = 'BLOB_ID';
  else if (l_type = 17) then
    type_name = 'BOOLEAN';
  else if (l_type = 14) then
    type_name = 'CHAR(' || :l_length || ')';
  else if (l_type = 11) then
    type_name = 'D_FLOAT';
  else if (l_type = 27) then
    type_name = 'DOUBLE';
  else if (l_type = 10) then
    type_name = 'FLOAT';
  else if (l_type = 16) then
    type_name = 'INT64';
  else if (l_type = 8) then
    type_name = 'INTEGER';
  else if (l_type = 9) then
    type_name = 'QUAD';
  else if (l_type = 7) then
    type_name = 'SMALLINT';
  else if (l_type = 12) then
    type_name = 'DATE';
  else if (l_type = 13) then
    type_name = 'TIME';
  else if (l_type = 35) then
    type_name = 'TIMESTAMP';
  else if (l_type = 37) then
    type_name = 'VARCHAR(' || :l_length || ')';
  suspend;
end