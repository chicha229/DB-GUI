CREATE OR ALTER PROCEDURE UI$CR_PROCEDURE
RETURNS (
    ID D_IDENT,
    PROCEDURE_NAME D_IDENT)
AS
begin
  for
    select p.id, p.procedure_name
    from ui$procedure p
    into :id, :procedure_name
  do
    suspend;
end