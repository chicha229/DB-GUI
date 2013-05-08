CREATE OR ALTER PROCEDURE UI$CR_ACTION_BIND
RETURNS (
    "BLOCK" D_IDENT,
    "ACTION" D_IDENT,
    PARAM D_IDENT,
    DESTINATION_PARAM D_IDENT)
AS
begin
  for
    select b."BLOCK", b."ACTION", b.param, b.destination_param
    from ui$action_bind b
    order by b."BLOCK", b."ACTION", b.param
    into :"BLOCK", :"ACTION", :param, :destination_param
  do
    suspend;
end