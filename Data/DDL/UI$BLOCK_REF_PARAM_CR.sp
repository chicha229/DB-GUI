CREATE OR ALTER PROCEDURE UI$BLOCK_REF_PARAM_CR (
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
/*
    SELECT T.REF,T.BLOCK,T.PARAM,T.ORDER_NUM,T.IS_MAIN_PARAM
    FROM UI$BLOCK_REF_PARAM T     
    LEFT JOIN UI$BLOCK_REF L1 ON L1.ID=T.REF AND L1.BLOCK=T.BLOCK
    LEFT JOIN UI$BLOCK_PARAM L2 ON L2.BLOCK=T.BLOCK AND L2.PARAM=T.PARAM
    WHERE
      (T.REF = :I_REF OR :I_REF IS NULL) AND
      (T.BLOCK = :I_BLOCK OR :I_BLOCK IS NULL) AND
      (T.PARAM = :I_PARAM OR :I_PARAM IS NULL) AND
      1=1
*/
    select :i_ref, :i_block, p.param, rp.order_num, rp.is_main_param
    from UI$BLOCK_REF r
    join ui$block_param p on p."BLOCK" = :i_block
    left join ui$block_ref_param rp on rp."BLOCK" = :i_block and rp.ref = :i_ref and rp.param = p.param
    where
      r.id = :i_ref and
      r."BLOCK" = :i_block and
      1=1
    INTO :REF,:BLOCK,:PARAM,:ORDER_NUM,:IS_MAIN_PARAM
  DO SUSPEND;
END