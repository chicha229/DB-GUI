CREATE OR ALTER PROCEDURE UI$CR_FORM_CHILD_REF_BIND
RETURNS (
    FORM_CHILD INTEGER,
    REF INTEGER,
    DESTINATION_PARAM D_IDENT,
    SOURCE_CHILD INTEGER,
    SOURCE_PARAM D_IDENT)
AS
begin
  for
    select
      b.form_child, b.ref, b.destination_param, b.source_child, b.source_param
    from ui$form_child_ref_bind b
    order by b.form_child, b.ref
    into :form_child, :ref, :destination_param, :source_child, :source_param
  do
    suspend;
end