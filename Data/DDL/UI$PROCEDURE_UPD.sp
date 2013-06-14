CREATE OR ALTER PROCEDURE UI$PROCEDURE_UPD (
    I_ID VARCHAR(30),
    I_NAME VARCHAR(100),
    I_CUSTOM_CLASS VARCHAR(100),
    I_PROCEDURE_NAME VARCHAR(30),
    I_IS_GRID_FOR_TABLE VARCHAR(30),
    I_IS_MODAL SMALLINT,
    I_FORCE_SAVE SMALLINT)
AS
begin
  update ui$procedure t
  set
   t.procedure_name=:i_procedure_name,
   t.is_grid_for_table=:i_is_grid_for_table,
   t.force_save = :i_force_save
  where t.id=:i_id and 1=1;

  update ui$block b
    set
      b.name = :i_name,
      b.custom_class = :i_custom_class,
      b.is_modal = :i_is_modal
    where
      b.id = :i_id;
end