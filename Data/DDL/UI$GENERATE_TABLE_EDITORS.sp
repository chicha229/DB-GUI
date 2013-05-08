CREATE OR ALTER PROCEDURE UI$GENERATE_TABLE_EDITORS (
    I_TABLE_NAME D_IDENT,
    I_INCLUDE_TERM D_BOOLEAN = 1)
RETURNS (
    O_SCRIPT VARCHAR(8000))
AS
declare variable L_GRID_CURSOR_NAME D_IDENT;
declare variable L_DETAIL_CURSOR_NAME D_IDENT;
declare variable L_TABLE_DESCRIPTION D_NAME;
declare variable L_WHERE_D varchar(1000);
declare variable L_WHERE_D_ITEM varchar(100);
declare variable L_FIELD_NAME D_IDENT;
declare variable L_FIELD_CAPTION D_NAME;
declare variable L_FIELD_TYPE_D D_IDENT;
declare variable L_FIELD_INDEX integer;
declare variable L_ACTION D_IDENT;
declare variable L_ACTION_ID D_IDENT;
declare variable L_FIELDS varchar(1000);
declare variable L_FIELD_DATAS varchar(1000);
declare variable L_FIELD_PARAMS varchar(1000);
declare variable L_IN_PARAMS varchar(1000);
declare variable L_FIELD_UPDATE varchar(1000);
declare variable L_FIELD_UPDATES varchar(1000);
declare variable L_CRLF varchar(10);
begin
  l_crlf = ascii_char(13) || ascii_char(10);
  l_grid_cursor_name = i_table_name || '_CR';
  l_detail_cursor_name = l_grid_cursor_name || '_D';

  select r.rdb$description
    from rdb$relations r
    where r.rdb$relation_name = :i_table_name
    into :l_table_description;

  -- процедура детали
  insert into ui$block (id, block_type, name, is_modal)
    values (:l_detail_cursor_name, 'procedure', :l_table_description || ' - Детали', 0);
  insert into ui$procedure (id, procedure_name)
    values (:l_detail_cursor_name, :l_detail_cursor_name);
  insert into ui$block_param (block, param, param_direction, data_type, caption, index_in_key, index_in_name)
    select :l_detail_cursor_name, f.field_name, 'field', dt.id, coalesce(f.field_description, f.field_name), f.index_on_key, case when f.field_name = 'NAME' then 1 end
    from ui$get_table_fields(:i_table_name) f
    left join ui$field_type_fb ff on ff.fb_type_id = f.field_type_fb
    left join ui$data_type dt on dt.id = ff.data_type;

  -- заполняем where
  l_field_index = 1;
  l_where_d = 'WHERE' || l_crlf;
  for
    select
      '      T.' || f.field_name || ' = :I_' || f.field_name || ' AND',
      f.field_name, coalesce(f.field_description, f.field_name), f.field_type_name
    from ui$get_table_fields(:i_table_name) f
    where
      f.index_on_key is not null and
      1=1
    order by f.order_num
    into :l_where_d_item, :l_field_name, :l_field_caption, :l_field_type_d
  do
  begin
    l_where_d = l_where_d || l_where_d_item || l_crlf;
    insert into ui$block_param (block, param, param_direction, data_type, order_num, caption, call_order_num)
      values (:l_detail_cursor_name, 'I_' || :l_field_name, 'in', :l_field_type_d, :l_field_index, :l_field_caption, :l_field_index);
    l_field_index = l_field_index + 1;
  end
  l_where_d = l_where_d || '      1=1';

  if (i_include_term = 1) then
    o_script = 'SET TERM ^ ;' || l_crlf;
  else
    o_script = '';

  execute procedure ui$get_table_fields_str(i_table_name, null, 1) returning_values (l_field_datas);
  execute procedure ui$get_table_fields_str(i_table_name, 'T.', 0) returning_values (l_fields);
  execute procedure ui$get_table_fields_str(i_table_name, ':', 0) returning_values (l_field_params);
  execute procedure ui$get_table_fields_str(i_table_name, 'I_', 1, 1) returning_values (l_in_params);

  -- курсор для показа одной записи, для редактирования
  o_script = o_script || 'CREATE OR ALTER PROCEDURE ' || l_detail_cursor_name || l_crlf;
  o_script = o_script || '(' || l_in_params || ') RETURNS (' || l_crlf;
  o_script = o_script || '  ' || l_field_datas || l_crlf;
  o_script = o_script || ') AS BEGIN' || l_crlf;
  o_script = o_script || '  FOR' || l_crlf;
  o_script = o_script || '    SELECT ' || l_fields || l_crlf;
  o_script = o_script || '    FROM ' || i_table_name || ' T ' || l_crlf;
  o_script = o_script || '    ' || l_where_d || l_crlf;
  o_script = o_script || '    INTO ' || l_field_params || l_crlf;
  o_script = o_script || '  DO SUSPEND;' || l_crlf;
  o_script = o_script || 'END^' || l_crlf;

  -- процедура добавления записи
  execute procedure ui$get_table_fields_str(i_table_name, 'I_', 1) returning_values (l_field_datas);
  execute procedure ui$get_table_fields_str(i_table_name, null, 0) returning_values (l_fields);
  execute procedure ui$get_table_fields_str(i_table_name, ':I_', 0) returning_values (l_field_params);

  o_script = o_script || 'CREATE OR ALTER PROCEDURE ' || i_table_name || '_INS' || l_crlf;
  o_script = o_script || '(' || l_field_datas || ')' || l_crlf;
  o_script = o_script || 'AS BEGIN' || l_crlf;
  o_script = o_script || '  INSERT INTO ' || i_table_name || ' (' || l_fields || ')' || l_crlf;
  o_script = o_script || '  VALUES  (' || l_field_params || ');' || l_crlf;
  o_script = o_script || 'END^' || l_crlf;

  -- изменения записи
  l_field_updates = '';
  for
    select 'T.' || f.field_name || '=:I_' || f.field_name f
    from ui$get_table_fields(:i_table_name) f
    where
      f.index_on_key is null and
      1=1
    into :l_field_update
  do
    l_field_updates = l_field_updates || l_field_update || ',';
  l_field_updates = substring(l_field_updates from 1 for char_length(l_field_updates) - 1);
  o_script = o_script || 'CREATE OR ALTER PROCEDURE ' || i_table_name || '_UPD' || l_crlf;
  o_script = o_script || '(' || l_field_datas || ')' || l_crlf;
  o_script = o_script || 'AS BEGIN' || l_crlf;
  o_script = o_script || '  UPDATE ' || i_table_name || ' T' || l_crlf;
  o_script = o_script || '  SET ' || l_field_updates || l_crlf;
  l_field_updates = '';
  for
    select 'T.' || f.field_name || '=:I_' || f.field_name f
    from ui$get_table_fields(:i_table_name) f
    where
      f.index_on_key is not null and
      1=1
    order by f.order_num
    into :l_field_update
  do
    l_field_updates = l_field_updates || l_field_update || ' AND ';
  l_field_updates = l_field_updates || '1=1';
  o_script = o_script || '  WHERE ' || l_field_updates || ';' || l_crlf;
  o_script = o_script || 'END^' || l_crlf;

  -- удаления записи
  o_script = o_script || 'CREATE OR ALTER PROCEDURE ' || i_table_name || '_DEL' || l_crlf;
  o_script = o_script || '(' || l_field_datas || ')' || l_crlf;
  o_script = o_script || 'AS BEGIN' || l_crlf;
  o_script = o_script || '  DELETE FROM ' || i_table_name || ' T' || l_crlf;
  o_script = o_script || '  WHERE ' || l_field_updates || ';' || l_crlf;
  o_script = o_script || 'END^' || l_crlf;

  if (i_include_term = 1) then
    o_script = o_script || 'SET TERM ; ^' || l_crlf;

  -- действия по умолчанию
  for
    select a.id
    from ui$default_action a
    into :l_action
  do
  begin
    execute procedure ui$generate_editor_form (
      i_table_name, l_action
    );

    -- действия в процедуре просмотра
    insert into ui$block_action (
        id, block, caption,
        links_to, action_style,
        image_index, order_num, refresh_mode
      )
      select
        a.id, :l_grid_cursor_name, a.name,
        :i_table_name || '_' || a.name_in_code || '_FR',
        'button',
        a.image_index, a.order_num, a.refresh_mode
      from ui$default_action a
      where a.id = :l_action
      returning id into :l_action_id;

    -- бинды к действию
    insert into ui$action_bind (block, action, param, destination_param)
      select :l_grid_cursor_name, :l_action_id, f.field_name, 'I_' || f.field_name
      from ui$get_table_fields(:i_table_name) f
      where
        f.index_on_key is not null and
        :l_action <> 'insert' and
        1=1;
  end

end