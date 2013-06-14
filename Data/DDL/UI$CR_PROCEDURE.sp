CREATE OR ALTER PROCEDURE UI$CR_PROCEDURE
RETURNS (
    ID D_IDENT,
    PROCEDURE_NAME D_IDENT,
    PROCEDURE_OWNER D_IDENT,
    FORCE_SAVE D_BOOLEAN)
AS
BEGIN
  PROCEDURE_OWNER = NULL;
  FOR
    SELECT P.ID, P.PROCEDURE_NAME, P.FORCE_SAVE
    FROM UI$PROCEDURE P
    INTO :ID, :PROCEDURE_NAME, :FORCE_SAVE
  DO
    SUSPEND;
END