CREATE OR ALTER PROCEDURE UI$CR_FORM_CHILD_PARAM
RETURNS (
    FORM_CHILD INTEGER,
    PARAM D_IDENT,
    VISIBLE D_BOOLEAN,
    READ_ONLY D_BOOLEAN,
    REQUIRED D_BOOLEAN,
    SOURCE_CHILD INTEGER,
    SOURCE_PARAM D_IDENT,
    AUTO_REFRESH D_BOOLEAN)
AS
begin
  for
    select
      cp.form_child, cp.param,
      coalesce(cp.visible, p.visible),
      coalesce(cp.read_only, p.read_only),
      coalesce(cp.required, p.required),
      cp.source_child, cp.source_param, cp.auto_refresh
    from ui$form_child_param cp
    join ui$form_child c on c.id = cp.form_child
    join ui$block_param p on p."BLOCK" = c."BLOCK" and p.param = cp.param
    order by cp.form_child, cp.param
    into
      :form_child, :param, :visible, :read_only, :required,
      :source_child, :source_param, :auto_refresh
  do
    suspend;
end