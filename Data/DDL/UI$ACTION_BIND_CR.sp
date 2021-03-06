CREATE OR ALTER PROCEDURE UI$ACTION_BIND_CR (
    I_ACTION VARCHAR(30),
    I_PARAM VARCHAR(30),
    I_BLOCK VARCHAR(30))
RETURNS (
    "BLOCK" VARCHAR(30),
    "ACTION" VARCHAR(30),
    PARAM VARCHAR(30),
    DESTINATION_PARAM VARCHAR(30))
AS
BEGIN
  FOR
    SELECT A.BLOCK,A.ID,T.PARAM,P.PARAM
    FROM UI$BLOCK_ACTION A
    JOIN UI$BLOCK_PARAM P ON P."BLOCK" = A.LINKS_TO
    LEFT JOIN UI$ACTION_BIND T ON T.DESTINATION_PARAM = P.PARAM AND T."BLOCK" = A.BLOCK AND T."ACTION" = A.ID
    WHERE
      (A.ID = :I_ACTION OR :I_ACTION IS NULL) AND
      (P.PARAM = :I_PARAM OR :I_PARAM IS NULL) AND
      (A.BLOCK = :I_BLOCK OR :I_BLOCK IS NULL) AND
      1=1
    INTO :BLOCK,:ACTION,:PARAM,:DESTINATION_PARAM
  DO SUSPEND;
END