CREATE OR ALTER PROCEDURE UI$FORM_PANEL_DEL (
    I_ID INTEGER,
    I_FORM VARCHAR(30),
    I_CAPTION VARCHAR(100),
    I_PARENT INTEGER,
    I_ALIGN VARCHAR(30),
    I_INDEX_ON_PARENT INTEGER)
AS
 BEGIN
  DELETE FROM UI$FORM_PANEL T
  WHERE T.ID=:I_ID AND 1=1;
END