CREATE OR ALTER PROCEDURE UI$FORM_PANEL_CR (
    I_ID INTEGER,
    I_FORM D_IDENT)
RETURNS (
    ID INTEGER,
    FORM VARCHAR(30),
    CAPTION VARCHAR(100),
    PARENT INTEGER,
    ALIGN VARCHAR(30),
    INDEX_ON_PARENT INTEGER,
    L_ALIGN_NAME VARCHAR(100))
AS
BEGIN
  FOR
    SELECT T.ID,T.FORM,T.CAPTION,T.PARENT,T.ALIGN,T.INDEX_ON_PARENT,L_3.NAME AS L_ALIGN_NAME
    FROM UI$FORM_PANEL T
    LEFT JOIN UI$FORM L_1 ON L_1.ID = T.FORM
    LEFT JOIN UI$FORM_PANEL L_2 ON L_2.ID = T.PARENT
    LEFT JOIN UI$PANEL_ALIGN L_3 ON L_3.ID = T.ALIGN
    WHERE
      (T.ID = :I_ID OR :I_ID IS NULL) AND
      (:I_FORM IS NULL OR T.FORM = :I_FORM) AND
      1=1
    INTO :ID,:FORM,:CAPTION,:PARENT,:ALIGN,:INDEX_ON_PARENT,:L_ALIGN_NAME
  DO SUSPEND;
END