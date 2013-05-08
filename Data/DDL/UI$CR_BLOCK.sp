CREATE OR ALTER PROCEDURE UI$CR_BLOCK
RETURNS (
    ID D_IDENT,
    BLOCK_TYPE D_IDENT,
    NAME D_NAME,
    CUSTOM_CLASS D_NAME,
    IS_MODAL D_BOOLEAN)
AS
begin
  for
    select b.id, b.block_type, b.name, b.custom_class, b.is_modal
    from ui$block b
    into :id, :block_type, :name, :custom_class, :is_modal
  do
    suspend;
end