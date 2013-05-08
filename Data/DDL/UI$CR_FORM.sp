CREATE OR ALTER PROCEDURE UI$CR_FORM
RETURNS (
    ID D_IDENT,
    MAIN_PROCEDURE D_IDENT,
    HEADER_VISIBLE D_BOOLEAN)
AS
begin
  for
    select f.id, f.main_procedure, f.header_visible
    from ui$form f
    order by f.id
    into :id, :main_procedure, :header_visible
  do
    suspend;
end