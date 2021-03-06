CREATE OR ALTER PROCEDURE UI$PROCEDURE_CR (
    I_ID VARCHAR(30),
    I_EXCEPT_SYSTEM D_BOOLEAN)
RETURNS (
    ID VARCHAR(30),
    NAME VARCHAR(100),
    CUSTOM_CLASS VARCHAR(100),
    PROCEDURE_NAME VARCHAR(30),
    IS_GRID_FOR_TABLE VARCHAR(30),
    IS_MODAL SMALLINT)
AS
BEGIN
  FOR
    SELECT T.ID,T.PROCEDURE_NAME,T.IS_GRID_FOR_TABLE,B.NAME,B.CUSTOM_CLASS,B.IS_MODAL
    FROM UI$PROCEDURE T
    JOIN UI$BLOCK B ON B.ID = T.ID
    WHERE
      (T.ID = :I_ID OR :I_ID IS NULL) AND
      (COALESCE(:I_EXCEPT_SYSTEM, 0) = 0 OR T.ID NOT LIKE 'UI$%') AND
      1=1
    ORDER BY T.ID
    INTO :ID,:PROCEDURE_NAME,:IS_GRID_FOR_TABLE,:NAME,:CUSTOM_CLASS,:IS_MODAL
  DO SUSPEND;
END