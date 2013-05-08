CREATE OR ALTER PROCEDURE UI$FORM_DEL (
    I_ID VARCHAR(30))
AS
BEGIN
/*
  delete from ui$block_param p
    where p."BLOCK" = :i_id;
 */
  delete from ui$block b
    where b.id = :i_id;
END