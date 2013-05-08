CREATE OR ALTER TRIGGER UI$BLOCK_REF_BI FOR UI$BLOCK_REF
ACTIVE BEFORE INSERT POSITION 0
as
begin
  if (new.id is null) then
    new.id = gen_id(gen_ui$block_ref_id,1);
end
^