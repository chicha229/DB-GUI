CREATE OR ALTER PROCEDURE UI$BLOCK_REF_BIND_DEL (
    I_REF INTEGER,
    I_BLOCK VARCHAR(30),
    I_DESTINATION_PARAM VARCHAR(30),
    I_SOURCE_PARAM VARCHAR(30))
AS
 BEGIN
  DELETE FROM UI$BLOCK_REF_BIND T
  WHERE T.REF=:I_REF AND T.BLOCK=:I_BLOCK AND T.DESTINATION_PARAM=:I_DESTINATION_PARAM AND 1=1;
END