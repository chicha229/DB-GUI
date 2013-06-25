CREATE OR ALTER PROCEDURE UI$BLOCK_ACTION_INS (
    I_ID VARCHAR(30),
    I_BLOCK VARCHAR(30),
    I_CAPTION VARCHAR(100),
    I_LINKS_TO VARCHAR(30),
    I_ACTION_STYLE VARCHAR(30),
    I_IMAGE_INDEX INTEGER,
    I_ORDER_NUM SMALLINT,
    I_REFRESH_MODE VARCHAR(30),
    I_SHORTCUT VARCHAR(100))
AS
 BEGIN
  INSERT INTO UI$BLOCK_ACTION (ID,BLOCK,CAPTION,LINKS_TO,ACTION_STYLE,IMAGE_INDEX,ORDER_NUM,REFRESH_MODE, SHORTCUT)
  VALUES  (:I_ID,:I_BLOCK,:I_CAPTION,:I_LINKS_TO,:I_ACTION_STYLE,:I_IMAGE_INDEX,:I_ORDER_NUM,:I_REFRESH_MODE, :I_SHORTCUT);
END