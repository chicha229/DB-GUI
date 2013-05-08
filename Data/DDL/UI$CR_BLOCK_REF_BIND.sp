CREATE OR ALTER PROCEDURE UI$CR_BLOCK_REF_BIND
RETURNS (
    REF INTEGER,
    DESTINATION_PARAM D_IDENT,
    SOURCE_PARAM D_IDENT)
AS
begin
  for
    select b.ref, b.destination_param, b.source_param
    from UI$BLOCK_REF_BIND B
    order by b.ref, b.destination_param
    into :ref, :destination_param, :source_param
  do
    suspend;
end