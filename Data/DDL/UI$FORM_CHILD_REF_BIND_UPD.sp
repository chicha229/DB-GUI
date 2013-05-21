CREATE OR ALTER PROCEDURE UI$FORM_CHILD_REF_BIND_UPD (
    I_FORM_CHILD INTEGER,
    I_FORM VARCHAR(30),
    I_BLOCK VARCHAR(30),
    I_REF INTEGER,
    I_DESTINATION_PARAM VARCHAR(30),
    I_SOURCE_CHILD INTEGER,
    I_SOURCE_BLOCK VARCHAR(30),
    I_SOURCE_PARAM VARCHAR(30))
AS
 BEGIN
  UPDATE UI$FORM_CHILD_REF_BIND T
  SET T.FORM=:I_FORM,T.BLOCK=:I_BLOCK,T.SOURCE_CHILD=:I_SOURCE_CHILD,T.SOURCE_BLOCK=:I_SOURCE_BLOCK,T.SOURCE_PARAM=:I_SOURCE_PARAM
  WHERE T.FORM_CHILD=:I_FORM_CHILD AND T.REF=:I_REF AND T.DESTINATION_PARAM=:I_DESTINATION_PARAM AND 1=1;
END