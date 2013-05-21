CREATE OR ALTER PROCEDURE UI$BLOCK_REF_PARAM_CR_D (
    I_REF INTEGER,
    I_BLOCK VARCHAR(30),
    I_PARAM VARCHAR(30))
RETURNS (
    REF INTEGER,
    "BLOCK" VARCHAR(30),
    PARAM VARCHAR(30),
    ORDER_NUM SMALLINT,
    IS_MAIN_PARAM SMALLINT)
AS
BEGIN
  FOR
    SELECT :I_REF, :I_BLOCK, P.PARAM, RP.ORDER_NUM, RP.IS_MAIN_PARAM
    FROM UI$BLOCK_REF R
    JOIN UI$BLOCK_PARAM P ON P."BLOCK" = :I_BLOCK AND P.PARAM = :I_PARAM
    LEFT JOIN UI$BLOCK_REF_PARAM RP ON RP."BLOCK" = :I_BLOCK AND RP.REF = :I_REF AND RP.PARAM = P.PARAM
    WHERE
      R.ID = :I_REF AND
      R."BLOCK" = :I_BLOCK AND
      1=1
    INTO :REF,:BLOCK,:PARAM,:ORDER_NUM,:IS_MAIN_PARAM
  DO SUSPEND;
END