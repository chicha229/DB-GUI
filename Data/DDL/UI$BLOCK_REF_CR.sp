CREATE OR ALTER PROCEDURE UI$BLOCK_REF_CR (
    I_ID INTEGER,
    I_BLOCK VARCHAR(30))
RETURNS (
    ID INTEGER,
    "BLOCK" VARCHAR(30),
    REFS_TO VARCHAR(30),
    L_BLOCK_NAME VARCHAR(100),
    L_REFS_TO_NAME VARCHAR(100))
AS
  declare variable l_refs_to_key d_ident;
  declare variable l_ref_with_keys varchar(100);
BEGIN
  FOR
    SELECT T.ID,T.BLOCK,T.REFS_TO,L1.NAME AS L_BLOCK_NAME,L2.NAME AS L_REFS_TO_NAME
    FROM UI$BLOCK_REF T     
    LEFT JOIN UI$BLOCK L1 ON L1.ID=T.BLOCK
    LEFT JOIN UI$BLOCK L2 ON L2.ID=T.REFS_TO
    WHERE
      (T.ID = :I_ID OR :I_ID IS NULL) AND
      (T.BLOCK = :I_BLOCK OR :I_BLOCK IS NULL) AND
      1=1
    INTO :ID,:BLOCK,:REFS_TO,:L_BLOCK_NAME,:L_REFS_TO_NAME
  DO
  begin
    l_ref_with_keys = refs_to || '(';
    for
      select p.param || ','
      from ui$block_param p
      where
        p."BLOCK" = :refs_to and
        p.index_in_key is not null and
        1=1
      order by p.index_in_key
      into :l_refs_to_key
    do
      l_ref_with_keys = l_ref_with_keys || l_refs_to_key;
    l_ref_with_keys = l_ref_with_keys || ')';
    refs_to = l_ref_with_keys;
    SUSPEND;
  end
END