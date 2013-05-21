CREATE OR ALTER PROCEDURE UI$BLOCK_PARAM_UPD (
    I_BLOCK VARCHAR(30),
    I_PARAM VARCHAR(30),
    I_PARAM_DIRECTION VARCHAR(30),
    I_DATA_TYPE VARCHAR(30),
    I_ORDER_NUM SMALLINT,
    I_CALL_ORDER_NUM SMALLINT,
    I_GROUP_NAME VARCHAR(100),
    I_CAPTION VARCHAR(100),
    I_ENABLER_PARAM VARCHAR(30),
    I_SOURCE_CHILD INTEGER,
    I_SOURCE_PARAM VARCHAR(30),
    I_INDEX_IN_KEY SMALLINT,
    I_INDEX_IN_PARENT SMALLINT,
    I_INDEX_IN_NAME SMALLINT,
    I_VISIBLE SMALLINT,
    I_REQUIRED SMALLINT,
    I_READ_ONLY SMALLINT)
AS
BEGIN
  UPDATE UI$BLOCK_PARAM T
  SET
    T.PARAM_DIRECTION=:I_PARAM_DIRECTION,
    T.DATA_TYPE=:I_DATA_TYPE,
    T.ORDER_NUM=:I_ORDER_NUM,
    T.CALL_ORDER_NUM=:I_CALL_ORDER_NUM,
    T.GROUP_NAME=:I_GROUP_NAME,
    T.CAPTION=:I_CAPTION,
    T.ENABLER_PARAM=:I_ENABLER_PARAM,
    T.SOURCE_CHILD=case when :I_SOURCE_CHILD = 0 then null else :I_SOURCE_CHILD end,
    T.SOURCE_PARAM=:I_SOURCE_PARAM,
    T.INDEX_IN_KEY=:I_INDEX_IN_KEY,
    T.INDEX_IN_PARENT=:I_INDEX_IN_PARENT,
    T.INDEX_IN_NAME=:I_INDEX_IN_NAME,
    T.VISIBLE=:I_VISIBLE,
    T.REQUIRED=:I_REQUIRED,
    T.READ_ONLY=:I_READ_ONLY
  WHERE T.BLOCK=:I_BLOCK AND T.PARAM=:I_PARAM AND 1=1;
END