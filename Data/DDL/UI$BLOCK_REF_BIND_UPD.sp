CREATE OR ALTER PROCEDURE UI$BLOCK_REF_BIND_UPD (
    I_REF INTEGER,
    I_BLOCK VARCHAR(30),
    I_DESTINATION_PARAM VARCHAR(30),
    I_SOURCE_PARAM VARCHAR(30))
AS
BEGIN
  merge into UI$BLOCK_REF_BIND d
    using (select null v from rdb$database)
    on (d.ref = :i_ref and d."BLOCK" = :i_block and d.destination_param = :i_destination_param)
    when matched then update set
      d.source_param = :i_source_param
    when not matched then insert (ref,block,destination_param,source_param)
  values  (:i_ref,:i_block,:i_destination_param,:i_source_param);

/*
  UPDATE UI$BLOCK_REF_BIND T
  SET T.SOURCE_PARAM=:I_SOURCE_PARAM
  WHERE T.REF=:I_REF AND T.BLOCK=:I_BLOCK AND T.DESTINATION_PARAM=:I_DESTINATION_PARAM AND 1=1;
*/
END