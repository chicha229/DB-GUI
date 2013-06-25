CREATE OR ALTER PROCEDURE UI$CR_PROCEDURE
RETURNS (
    ID D_IDENT,
    PROCEDURE_NAME D_IDENT,
    PACKAGE_NAME D_IDENT,
    PROCEDURE_OWNER D_IDENT,
    FORCE_SAVE D_BOOLEAN,
    GRID_STYLE D_IDENT)
AS
BEGIN
  PROCEDURE_OWNER = NULL;
  PACKAGE_NAME = NULL;
  FOR
    SELECT P.ID, P.PROCEDURE_NAME, P.FORCE_SAVE, P.GRID_STYLE
    FROM UI$PROCEDURE P
    INTO :ID, :PROCEDURE_NAME, :FORCE_SAVE, :GRID_STYLE
  DO
    SUSPEND;
END