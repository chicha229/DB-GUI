CREATE OR ALTER PROCEDURE UI$CR_FORM_PANEL
RETURNS (
    ID INTEGER,
    FORM D_IDENT,
    CAPTION D_NAME,
    PARENT INTEGER,
    ALIGN D_IDENT,
    INDEX_ON_PARENT SMALLINT)
AS
begin
  for
    select p.id, p.form, p.caption, p.parent, p.align, p.index_on_parent
    from ui$form_panel p
    order by p.form, p.id
    into :id, :form, :caption, :parent, :align, :index_on_parent
  do
    suspend;
end