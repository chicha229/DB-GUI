CREATE OR ALTER PROCEDURE UI$GENERATE_BLOCK_BY_PROCEDURE (
    I_PROCEDURE_NAME D_IDENT,
    I_GENERATE_FORM D_BOOLEAN = 0,
    I_MENU_ITEM_PARENT INTEGER = null,
    I_MENU_ITEM_ORDER_NUM SMALLINT = null)
AS
declare variable L_PROCEDURE_DESCRIPTION varchar(1000);
declare variable L_BLOCK_EXISTS D_BOOLEAN;
declare variable L_BLOCK_ID D_IDENT;
begin
  select coalesce(p.rdb$description, p.rdb$procedure_name)
    from rdb$procedures p
    where p.rdb$procedure_name = :i_procedure_name
    into :l_procedure_description;

  select count(*), max(p.id)
    from ui$procedure p
    where p.procedure_name = :i_procedure_name
    into l_block_exists, l_block_id;

  if (l_block_exists = 0) then
  begin
    l_block_id = i_procedure_name || '_P';
    -- процедура
    insert into ui$block (id, block_type, name)
      values (:l_block_id, 'procedure', :l_procedure_description);
    insert into ui$procedure (id, procedure_name)
      values (:l_block_id, :i_procedure_name);
  end
  merge into ui$block_param bp
    using (
      select
        trim(pp.rdb$parameter_name) name,
        trim(case when pp.rdb$parameter_type = 0 then 'in' else 'field' end) direction,
        dt.id data_type,
        trim(coalesce(pp.rdb$description, pp.rdb$parameter_name)) description,
        case when trim(pp.rdb$parameter_name) = 'NAME' then 1 end index_in_name,
        (pp.rdb$parameter_number + 1) * 10 order_num,
        pp.rdb$parameter_number + 1 call_order_num
      from rdb$procedure_parameters pp
      left join rdb$fields f on f.rdb$field_name = pp.rdb$field_source
      left join ui$field_type_fb ft on ft.fb_type_id = f.rdb$field_type
      left join ui$data_type dt on dt.id = ft.data_type
      where
        pp.rdb$procedure_name = :i_procedure_name and
        1=1
    ) p
    on (bp."BLOCK" = :l_block_id and bp.param = p.name)
    when not matched then
      insert ("BLOCK", param, param_direction, data_type, caption, index_in_name, order_num, call_order_num)
      values (:l_block_id, p.name, p.direction, p.data_type, p.description, p.index_in_name, p.order_num, p.call_order_num)
    when matched then update set
      bp.call_order_num = p.call_order_num,
      bp.data_type =
        case
          when bp.data_type = 'boolean' and p.data_type = 'integer' then bp.data_type
          else p.data_type
        end
  ;
  delete from ui$block_param bp
    where
      bp."BLOCK" = :l_block_id and
      bp.param not in (
        select trim(pp.rdb$parameter_name)
        from rdb$procedure_parameters pp
        where
          pp.rdb$procedure_name = :i_procedure_name and
          1=1
      );

  if (i_generate_form = 1) then
    execute procedure ui$generate_form_by_procedure(i_procedure_name);

  if (i_menu_item_parent is not null) then
    execute procedure ui$add_to_menu(
      i_menu_item_parent,
      i_menu_item_order_num,
      case
        when :i_generate_form = 1 then :i_procedure_name || '_FR'
        else :l_block_id
      end
    );
end