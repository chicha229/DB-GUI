CREATE OR ALTER PROCEDURE UI$GENERATE_EDITOR_FORM (
    I_TABLE_NAME D_IDENT,
    I_ACTION D_IDENT)
AS
  declare variable L_ACTION_CODE_NAME D_IDENT;
  declare variable L_FORM_NAME D_IDENT;
  declare variable L_FORM_DESCRIPTION D_NAME;
  declare variable L_DETAIL_CURSOR_NAME D_IDENT;
  declare variable L_EDIT_PROCEDURE D_IDENT;
  declare variable L_EDIT_PROCEDURE_BLOCK_NAME D_IDENT;
  declare variable L_CURSOR_CHILD integer;
  declare variable L_EDIT_CHILD integer;
  declare variable L_FORM_PANEL integer;
  declare variable L_LAST_REF integer;
  declare variable L_REF_ID integer;
  declare variable L_REF_LINKS_TO D_IDENT;
  declare variable L_REF_INDEX smallint;
  declare variable L_REF_FIELD D_IDENT;
  declare variable L_REF_FIELD_POS smallint;
  declare variable l_force_save d_boolean;
begin
  select r.rdb$description || ' - ' || a.name, a.name_in_code, a.force_save
    from ui$default_action a
    cross join rdb$relations r
    where
      a.id = :i_action and
      r.rdb$relation_name = :i_table_name and
      1=1
    into :l_form_description, :l_action_code_name, :l_force_save;

  l_edit_procedure = i_table_name || '_' || l_action_code_name;
  l_form_name = l_edit_procedure || '_FR';
  l_detail_cursor_name = i_table_name || '_CR_D';
  l_edit_procedure_block_name = i_table_name || '_' || l_action_code_name || '_P';

  -- форма редактирования
  insert into ui$block (id, block_type, name, is_modal)
    values (:l_form_name, 'form', :l_form_description, 0);
  insert into ui$form (id)
    values (:l_form_name);

  -- панель
  insert into ui$form_panel (form, caption, parent, align, index_on_parent)
    values (:l_form_name, :l_form_description, null, 'left', 1)
    returning id into :l_form_panel;

  -- процедура редактирования
  insert into ui$block (id, block_type, name, is_modal)
    values (:l_edit_procedure_block_name, 'procedure', :l_form_description, 1);
  insert into ui$procedure (id, procedure_name, force_save)
    values (:l_edit_procedure_block_name, :l_edit_procedure, :l_force_save);
  insert into ui$block_param (
      block, param, param_direction, data_type, order_num, caption,
      visible, required, group_name, enabler_param, read_only, call_order_num
    )
    select
      :l_edit_procedure_block_name, 'I_' || f.field_name, 'in',
      f.field_type_name, p.order_num, coalesce(p.caption, f.field_name),
      p.visible, p.required, p.group_name, p.enabler_param, p.required, p.call_order_num
    from ui$block_param p
    join ui$get_table_fields(:i_table_name) f on f.field_name = p.param
    where
      p."BLOCK" = :i_table_name || '_CR' and
      1=1;
  insert into ui$block_param (
      block, param, param_direction, data_type, order_num, caption,
      visible, required, group_name, enabler_param, read_only, call_order_num
    )
    select
      :l_edit_procedure_block_name, 'O_' || f.field_name, 'out',
      f.field_type_name, p.order_num, coalesce(p.caption, f.field_name),
      p.visible, p.required, p.group_name, p.enabler_param, p.required, p.call_order_num
    from ui$block_param p
    join ui$get_table_fields(:i_table_name) f on f.field_name = p.param
    where
      p."BLOCK" = :i_table_name || '_CR' and
      p.index_in_key is not null and
      :i_action = 'insert' and
      1=1;

  -- блоки - курсор и процедура редактирования
  insert into ui$form_child (form, panel, block, order_num, param_group_align, param_align, visible)
    values (:l_form_name, :l_form_panel, :l_detail_cursor_name, 1, 'horizontal', 'horizontal', 0)
    returning id into :l_cursor_child;
  insert into ui$form_child (form, panel, block, order_num, param_group_align, param_align, visible)
    values (:l_form_name, :l_form_panel, :l_edit_procedure_block_name, 2, 'vertical', 'vertical', 1)
    returning id into :l_edit_child;

  -- входные параметры процедуры - первичный ключ
  insert into ui$block_param (block, param, param_direction, data_type, order_num, caption, call_order_num, visible)
    select
      :l_form_name, 'I_' || f.field_name, 'in', f.field_type_name,
      f.index_on_key,
      coalesce(f.field_description, f.field_name),
      f.index_on_key, 0
    from ui$get_table_fields(:i_table_name) f
    where
      f.index_on_key is not null and
      :i_action <> 'insert' and
      1=1;

  -- выходные параметры вставки - первичный ключ
  insert into ui$block_param (block, param, param_direction, data_type, order_num, caption, call_order_num, visible)
    select
      :l_form_name, 'O_' || f.field_name, 'out', f.field_type_name,
      f.index_on_key,
      coalesce(f.field_description, f.field_name),
      f.index_on_key, 0
    from ui$get_table_fields(:i_table_name) f
    where
      f.index_on_key is not null and
      :i_action = 'insert' and
      1=1;

  -- бинд параметров курсора от параметров формы
  insert into ui$form_child_param (
      form_child, form, block,
      param, read_only,
      source_block, source_child, source_param
    )
    select
      :l_cursor_child, :l_form_name, :i_table_name || '_CR',
      'I_' || f.field_name, case when :i_action = 'delete' then 1 end,
      :l_form_name, null, f.field_name
    from ui$get_table_fields(:i_table_name) f;

  -- бинд значений курсора к процедуре редактирования
  insert into ui$form_child_param (
      form_child, form, block,
      param, read_only,
      source_block, source_child, source_param
    )
    select
      :l_edit_child, :l_form_name, :l_edit_procedure_block_name,
      'I_' || f.field_name, case when :i_action = 'delete' then 1 end,
      :l_detail_cursor_name, :l_cursor_child, f.field_name
    from ui$get_table_fields(:i_table_name) f;

  -- бинд выходных значений ключа вставки к форме
  insert into ui$form_child_param (
      form_child, form, block,
      param, read_only,
      source_block, source_child, source_param
    )
    select
      :l_edit_child, :l_form_name, :l_edit_procedure_block_name,
      'O_' || f.field_name, null,
      null, null, 'O_' || f.field_name
    from ui$get_table_fields(:i_table_name) f
    where
      f.index_on_key is not null and
      :i_action = 'insert' and
      1=1;

  -- выбор значений из справочников
  l_last_ref = 0;
  for
    select
      l.links_to_procedure, l.link_index,
      l.field_name, l.field_position
    from ui$get_table_links(:i_table_name) l
    into :l_ref_links_to, :l_ref_index, :l_ref_field, :l_ref_field_pos
  do
  begin
    if (l_last_ref <> l_ref_index) then
      insert into ui$block_ref (block, refs_to)
        values (:l_edit_procedure_block_name, :l_ref_links_to)
        returning id into :l_ref_id;
    insert into ui$block_ref_param (ref, block, param, order_num, is_main_param)
      values (:l_ref_id, :l_edit_procedure_block_name, 'I_' || :l_ref_field, :l_ref_field_pos, case when :l_ref_field_pos = 1 then 1 else 0 end);
  end
end