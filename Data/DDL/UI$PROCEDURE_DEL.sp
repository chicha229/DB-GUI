CREATE OR ALTER PROCEDURE UI$PROCEDURE_DEL (
    I_ID VARCHAR(30))
AS
BEGIN
  delete from ui$block b
  where b.id= :i_id;
END