CREATE OR ALTER PROCEDURE UI$GENERATE_FORM_BY_TABLE (
    I_TABLE_NAME D_IDENT,
    I_GENERATE_FORM D_BOOLEAN = 0,
    I_GENERATE_EDITORS D_BOOLEAN = 1,
    I_MENU_ITEM_PARENT INTEGER = null,
    I_MENU_ITEM_ORDER_NUM SMALLINT = null)
RETURNS (
    O_SCRIPT VARCHAR(8000))
AS
declare variable L_FIELD_DATAS varchar(1000);
declare variable L_JOIN_FIELD_DATAS varchar(1000);
declare variable L_FIELDS varchar(1000);
declare variable L_FIELD_PARAMS varchar(1000);
declare variable L_JOIN_FIELD_PARAMS varchar(1000);
declare variable L_IN_PARAMS varchar(1000);
declare variable L_WHERE_ITEM varchar(100);
declare variable L_WHERE varchar(1000);
declare variable L_JOINS varchar(1000);
declare variable L_JOIN_FIELDS varchar(1000);
declare variable L_CRLF varchar(2);
declare variable L_JOIN_INDEX integer;
declare variable L_GRID_CURSOR_NAME varchar(1000);
declare variable L_CONSTRAINT_NAME D_IDENT;
declare variable L_LAST_CONSTRAINT_NAME D_IDENT;
declare variable L_LINK_TABLE D_IDENT;
declare variable L_LINK_FIELD D_IDENT;
declare variable L_FIELD_NAME D_IDENT;
declare variable L_FIELD_TYPE D_NAME;
declare variable L_FIELD_TYPE_D D_IDENT;
declare variable L_FIELD_CAPTION D_NAME;
declare variable L_LINK_FIELD_NAME D_NAME;
declare variable L_CONDITIONS varchar(1000);
declare variable L_LINK_INDEX smallint;
declare variable L_FIELD_INDEX smallint;
declare variable L_EDIT_SCRIPT varchar(4000);
declare variable L_TABLE_DESCRIPTION varchar(1000);
begin
  l_crlf = ascii_char(13) || ascii_char(10);
  l_grid_cursor_name = i_table_name || '_CR';
  o_script = 'SET TERM ^ ;' || l_crlf;

  l_join_fields = '';
  l_join_field_datas = '';
  l_join_field_params = '';

  -- выбираем поля строками полностью
  execute procedure ui$get_table_fields_str(i_table_name, null, 1) returning_values (l_field_datas);
  execute procedure ui$get_table_fields_str(i_table_name, 'T.', 0) returning_values (l_fields);
  execute procedure ui$get_table_fields_str(i_table_name, ':', 0) returning_values (l_field_params);
  execute procedure ui$get_table_fields_str(i_table_name, 'I_', 1, 1) returning_values (l_in_params);

  select r.rdb$description
    from rdb$relations r
    where r.rdb$relation_name = :i_table_name
    into :l_table_description;

  -- процедура грида
  insert into ui$block (id, block_type, name, is_modal)
    values (:l_grid_cursor_name, 'procedure', :l_table_description, 0);
  insert into ui$procedure (id, procedure_name, is_grid_for_table)
    values (:l_grid_cursor_name, :l_grid_cursor_name, :i_table_name);
  insert into ui$block_param (
      block, param, param_direction, data_type,
      caption,
      index_in_key,
      index_in_name,
      call_order_num
    )
    select
      :l_grid_cursor_name, f.field_name,
      'field', dt.id, coalesce(f.field_description, f.field_name),
      f.index_on_key,
      case when f.field_name = 'NAME' then 1 end,
      f.order_num
    from ui$get_table_fields(:i_table_name) f
    left join ui$field_type_fb ff on ff.fb_type_id = f.field_type_fb
    left join ui$data_type dt on dt.id = ff.data_type;

  -- заполняем where
  l_field_index = 1;
  l_where = 'WHERE' || l_crlf;
  for
    select
      '      (T.' || f.field_name || ' = :I_' || f.field_name || ' OR :I_' || f.field_name || ' IS NULL) AND',
      f.field_name, coalesce(f.field_description, f.field_name), f.field_type_name
    from ui$get_table_fields(:i_table_name) f
    where
      f.index_on_key is not null and
      1=1
    order by f.order_num
    into :l_where_item, :l_field_name, :l_field_caption, :l_field_type_d
  do
  begin
    l_where = l_where || l_where_item || l_crlf;
    insert into ui$block_param (block, param, param_direction, data_type, order_num, caption, call_order_num)
      values (:l_grid_cursor_name, 'I_' || :l_field_name, 'in', :l_field_type_d, :l_field_index, :l_field_caption, :l_field_index);
    l_field_index = l_field_index + 1;
  end
  l_where = l_where || '      1=1';

  -- заполняем join-ы к процедурам
  l_joins = '';
  l_conditions = '';
  l_last_constraint_name = '';
  for
    select tl.fk_name, tl.links_to_table, tl.field_name, tl.links_to_field, tl.link_index
    from ui$get_table_links(:i_table_name) tl
    order by tl.link_index, tl.field_position
    into :l_constraint_name, :l_link_table, :l_field_name, :l_link_field, :l_join_index
  do
  begin
    if (l_last_constraint_name <> l_constraint_name) then
    begin
      if (l_joins <> '') then
        l_joins = l_joins || ' L' || (l_join_index - 1) || ' ON ' || substring(l_conditions from 1 for char_length(l_conditions) - 5);
      l_joins = l_joins || l_crlf || '    LEFT JOIN ' || l_link_table;
      l_last_constraint_name = l_constraint_name;
      l_conditions = '';
    end
    l_conditions = l_conditions ||
      'L' || l_join_index || '.' || l_link_field ||
      '=T.' || l_field_name || ' AND ';
  end
  if (l_last_constraint_name <> '') then
    l_joins = l_joins || ' L' || l_join_index || ' ON ' || substring(l_conditions from 1 for char_length(l_conditions) - 5);

  -- заполняем поля расшифровки
  l_join_fields = '';
  l_last_constraint_name = '';
  for
    select
      tl.fk_name, tl.field_name, lp.param, lp.index_in_name,
      f.field_type, f.field_type_name, p.caption, tl.link_index
    from ui$get_table_links(:i_table_name) tl
    join ui$procedure b on b.is_grid_for_table = :i_table_name
    join ui$block_param p on p."BLOCK" = b.id and p.param = tl.field_name
    join ui$block_param lp on
      lp."BLOCK" = tl.links_to_procedure and
      lp.param_direction = 'field' and
      lp.index_in_name is not null and
      1=1
    join ui$get_table_fields(tl.links_to_table) f on f.field_name = lp.param
    where
      tl.links_to_procedure is not null and
      1=1
    order by tl.link_index, tl.field_position, lp.index_in_name
    into
      :l_constraint_name, :l_field_name, :l_link_field,
      :l_field_index, :l_field_type, :l_field_type_d, :l_field_caption, :l_link_index
  do
  begin
    l_link_field_name = 'L_' || l_link_index || '_' || l_field_name || '_' || l_link_field;
    l_join_fields = l_join_fields ||
      ',L' || l_link_index || '.' || l_link_field || ' AS ' || l_link_field_name;
    l_join_field_datas = l_join_field_datas || ',' || l_link_field_name || ' ' || l_field_type;
    l_join_field_params = l_join_field_params || ',:' || l_link_field_name;
    insert into ui$block_param (block, param, param_direction, data_type, caption)
      values (:l_grid_cursor_name, :l_link_field_name, 'field', :l_field_type_d, :l_field_caption || case when :l_field_index > 1 then :l_field_index else '' end);
  end

  -- форма
  if (i_generate_form = 1) then
    execute procedure ui$generate_form_by_procedure(l_grid_cursor_name);

  -- меню
  if (i_menu_item_parent is not null) then
    execute procedure ui$add_to_menu(
      i_menu_item_parent,
      i_menu_item_order_num,
      case
        when :i_generate_form = 1 then :l_grid_cursor_name || '_FR'
        else :l_grid_cursor_name
      end
    );

  -- курсор для показа таблицы
  o_script = o_script || 'CREATE OR ALTER PROCEDURE ' || l_grid_cursor_name || l_crlf;

  o_script = o_script || '(' || l_in_params || ') RETURNS (' || l_crlf;
  o_script = o_script || '  ' || l_field_datas || l_join_field_datas || l_crlf;
  o_script = o_script || ') AS BEGIN' || l_crlf;
  o_script = o_script || '  FOR' || l_crlf;
  o_script = o_script || '    SELECT ' || l_fields || l_join_fields || l_crlf;
  o_script = o_script || '    FROM ' || i_table_name || ' T ';
  o_script = o_script || '    ' || l_joins || l_crlf;
  o_script = o_script || '    ' || l_where || l_crlf;
  o_script = o_script || '    INTO ' || l_field_params || l_join_field_params || l_crlf;
  o_script = o_script || '  DO SUSPEND;' || l_crlf;
  o_script = o_script || 'END^' || l_crlf;

  -- процедуры и формы редактирования
  if (i_generate_editors = 1) then
  begin
    execute procedure ui$generate_table_editors(i_table_name, 0)
      returning_values (l_edit_script);
    o_script = o_script || l_edit_script || l_crlf;
  end

  o_script = o_script || 'SET TERM ; ^' || l_crlf;

end

COMMENT ON PROCEDURE UI$GENERATE_FORM_BY_TABLE IS 
'Создать процедуры и описание интерфейса таблицы';


COMMENT ON PARAMETER UI$GENERATE_FORM_BY_TABLE.I_GENERATE_EDITORS IS 
'Создавать формы редактирования';
COMMENT ON PARAMETER UI$GENERATE_FORM_BY_TABLE.I_GENERATE_FORM IS 
'Создавать форму (к процедуре) просмотра';
COMMENT ON PARAMETER UI$GENERATE_FORM_BY_TABLE.I_MENU_ITEM_PARENT IS 
'В какую папку меню помещать';
COMMENT ON PARAMETER UI$GENERATE_FORM_BY_TABLE.I_TABLE_NAME IS 
'Таблица';