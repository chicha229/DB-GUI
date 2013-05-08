CREATE OR ALTER PROCEDURE UI$GENERATE_FORM_BY_PROCEDURE (
    I_PROCEDURE_NAME D_IDENT)
AS
declare variable L_PROCEDURE_ID D_IDENT;
declare variable L_FORM_NAME D_IDENT;
declare variable L_PROCEDURE_DESCRIPTION D_NAME;
declare variable L_FORM_PANEL integer;
declare variable L_FORM_CHILD integer;
declare variable L_FORM_EXISTS D_BOOLEAN;
begin
  l_form_name = i_procedure_name || '_FR';

  select count(*)
    from ui$form f
    where f.id = :l_form_name
    into :l_form_exists;

  select max(p.id)
    from ui$procedure p
    where p.procedure_name = :i_procedure_name
    into :l_procedure_id;

  if (l_form_exists = 0) then
  begin
    -- форма
    insert into ui$block (id, block_type, name)
      select :l_form_name, 'form', b.name
      from ui$procedure p
      join ui$block b on b.id = p.id
      where p.procedure_name = :i_procedure_name
      returning name into :l_procedure_description;
    insert into ui$form (id, main_procedure)
      values (:l_form_name, :l_procedure_id);
    insert into ui$form_panel (form, caption, parent, align, index_on_parent)
      values (:l_form_name, :l_procedure_description, null, 'top', 1)
      returning id into :l_form_panel;
    insert into ui$form_child (form, panel, block, order_num, param_group_align, param_align, visible)
      values (:l_form_name, :l_form_panel, :l_procedure_id, 1, 'horizontal', 'horizontal', 1)
      returning id into :l_form_child;
  end
  else
  begin
    select case when count(*) = 1 then min(c.id) end
      from ui$form_panel p
      join ui$form_child c on c.panel = p.id and c."BLOCK" = :l_procedure_id
      where
        p.form = :l_form_name and
        1=1
      into :l_form_child;
  end

  if (l_form_child is not null) then
    delete from ui$block_param bp
      where bp."BLOCK" = :l_form_name;

  insert into ui$block_param (
      block, param, param_direction, data_type, caption, visible,
      source_form, source_block, source_child, source_param,
      index_in_key, index_in_name, order_num, call_order_num)
    select
      :l_form_name, p.param, p.param_direction, p.data_type, p.caption, 0,
      :l_form_name, :l_procedure_id, :l_form_child, p.param,
      p.index_in_key, p.index_in_name, p.order_num, p.call_order_num
    from ui$block_param p
    where p."BLOCK" = :l_procedure_id;
end