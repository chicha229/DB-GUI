CREATE OR ALTER PROCEDURE UI$FORM_CHILD_PARAM_DEL (
    I_FORM_CHILD INTEGER,
    I_PARAM VARCHAR(30))
AS
 BEGIN
  DELETE FROM UI$FORM_CHILD_PARAM T
  WHERE T.FORM_CHILD=:I_FORM_CHILD AND T.PARAM=:I_PARAM AND 1=1;
END