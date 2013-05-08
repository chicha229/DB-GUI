CREATE OR ALTER PROCEDURE UI$BLOCK_REF_BIND_CR (
    I_REF INTEGER,
    I_BLOCK VARCHAR(30),
    I_DESTINATION_PARAM VARCHAR(30))
RETURNS (
    REF INTEGER,
    "BLOCK" VARCHAR(30),
    DESTINATION_PARAM VARCHAR(30),
    SOURCE_PARAM VARCHAR(30))
AS
BEGIN
  FOR
/*
    SELECT T.REF,T.BLOCK,T.DESTINATION_PARAM,T.SOURCE_PARAM
    FROM UI$BLOCK_REF_BIND T     
    LEFT JOIN UI$BLOCK_REF L1 ON L1.ID=T.REF AND L1.BLOCK=T.BLOCK
    LEFT JOIN UI$BLOCK_PARAM L2 ON L2.BLOCK=T.BLOCK AND L2.PARAM=T.SOURCE_PARAM
    WHERE
      (T.REF = :I_REF OR :I_REF IS NULL) AND
      (T.BLOCK = :I_BLOCK OR :I_BLOCK IS NULL) AND
      (T.DESTINATION_PARAM = :I_DESTINATION_PARAM OR :I_DESTINATION_PARAM IS NULL) AND
      1=1
*/
    select r.id, r.block, rp.param, b.source_param
    from ui$block_ref r
    join ui$block_param rp on rp."BLOCK" = r.refs_to and rp.param_direction in ('in', 'in_out')
    left join ui$block_ref_bind b on b.ref = :i_ref and b.destination_param = rp.param
    where
      r.id = :i_ref and
      1=1

    INTO :REF,:BLOCK,:DESTINATION_PARAM,:SOURCE_PARAM
  DO SUSPEND;
END