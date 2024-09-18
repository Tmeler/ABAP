*&---------------------------------------------------------------------*
*& Report ZTC_ALV_OO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztc_alv_oo.
INCLUDE <icon>.
TYPE-POOLS: slis.
TABLES: ztc_curso.
TYPES:
BEGIN OF ly_curso_aluno.
INCLUDE TYPE ztc_curso_aluno.
TYPES: id TYPE icon-id,
color TYPE char4,
END OF ly_curso_aluno,
BEGIN OF ly_curso.
INCLUDE TYPE ztc_curso.
TYPES: celltab TYPE lvc_t_styl,
END OF ly_curso.
CLASS lcl_event_grid DEFINITION.
PUBLIC SECTION.
METHODS:
data_changed
FOR EVENT data_changed
OF cl_gui_alv_grid IMPORTING er_data_changed
e_onf4
e_onf4_before
e_onf4_after
e_ucomm,
hotspot_click
FOR EVENT hotspot_click
OF cl_gui_alv_grid IMPORTING e_row_id
e_column_id
es_row_no.
ENDCLASS.
DATA: lt_ztc_curso TYPE TABLE OF ztc_curso,
lt_ztc_curso_negrito TYPE TABLE OF ly_curso,lt_ztc_curso_aluno TYPE TABLE OF ly_curso_aluno,
lt_ztc_professor TYPE TABLE OF ztc_professor,
lo_grid_100a TYPE REF TO cl_gui_alv_grid,
lo_grid_100b TYPE REF TO cl_gui_alv_grid,
lo_grid_200 TYPE REF TO cl_gui_alv_grid,
lo_container_100a TYPE REF TO cl_gui_custom_container,
lo_container_100b TYPE REF TO cl_gui_custom_container,
lo_container_200 TYPE REF TO cl_gui_custom_container,
lo_event_grid TYPE REF TO lcl_event_grid,
lv_okcode_100 TYPE sy-ucomm,
lv_okcode_200 TYPE sy-ucomm,
lt_fieldcata TYPE lvc_t_fcat,
lt_fieldcatb TYPE lvc_t_fcat,
lt_fieldcat200 TYPE lvc_t_fcat,
lt_tool_bar TYPE ui_functions,
lt_sort TYPE TABLE OF lvc_s_sort,
ls_layout TYPE lvc_s_layo,
ls_layout200 TYPE lvc_s_layo,
ls_variant TYPE disvariant,
lv_salvou_item TYPE char1.
CLASS lcl_event_grid IMPLEMENTATION.
METHOD data_changed.
LOOP AT er_data_changed->mt_good_cells[] ASSIGNING
FIELD-SYMBOL(<fs_good_cells>).
READ TABLE lt_ztc_curso_aluno[] ASSIGNING
FIELD-SYMBOL(<fs_ztc_curso_aluno>) INDEX <fs_good_cells>-row_id.
CHECK sy-subrc EQ 0.
CASE <fs_good_cells>-fieldname.
WHEN 'VALOR'.
<fs_ztc_curso_aluno>-valor = <fs_good_cells>-value.
ENDCASE.
ENDLOOP.
ENDMETHOD.
METHOD hotspot_click.READ TABLE lt_ztc_curso_negrito[] ASSIGNING
FIELD-SYMBOL(<fs_curso_negrito>) INDEX e_row_id.
CHECK sy-subrc EQ 0.
SELECT *
INTO TABLE lt_ztc_professor[]
FROM ztc_professor
WHERE curso EQ <fs_curso_negrito>-nome_curso.
IF sy-subrc EQ 0.
CALL SCREEN 200 STARTING AT 45 5 ENDING AT 148 20.
ELSE.
MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'W'.
ENDIF.
ENDMETHOD.
ENDCLASS.
SELECTION-SCREEN BEGIN OF BLOCK b11 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS: so_curso FOR ztc_curso-nome_curso NO INTERVALS.
SELECTION-SCREEN SKIP.
PARAMETERS: p_basic TYPE char1 RADIOBUTTON GROUP gr1,
p_cmpl TYPE char1 RADIOBUTTON GROUP gr1 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b11.
START-OF-SELECTION.
PERFORM f_obtem_dados.
FORM f_obtem_dados.
PERFORM f_select_curso.
PERFORM f_select_aluno.
IF p_basic EQ 'X'.
PERFORM f_visualizar_dados_alv_basico.
ELSE.
PERFORM f_visualizar_dados_alv_compl.
ENDIF.
ENDFORM.FORM f_select_curso.
DATA: ls_ztc_curso_negrito LIKE LINE OF lt_ztc_curso_negrito[],
ls_celltab LIKE LINE OF ls_ztc_curso_negrito-celltab[].
FREE lt_ztc_curso_negrito[].
SELECT *
INTO TABLE lt_ztc_curso[]
FROM ztc_curso
WHERE nome_curso IN so_curso.
LOOP AT lt_ztc_curso[] ASSIGNING FIELD-SYMBOL(<fs_curso>).
FREE: ls_ztc_curso_negrito-celltab[].
MOVE-CORRESPONDING <fs_curso> TO ls_ztc_curso_negrito.
IF ls_ztc_curso_negrito-ativo EQ 'X'.
ls_celltab-fieldname = 'DATA_INICIO'.
ls_celltab-style = '00000121'.
INSERT ls_celltab INTO TABLE ls_ztc_curso_negrito-celltab[].
ls_celltab-fieldname = 'DATA_FINAL'.
ls_celltab-style = '00000121'.
INSERT ls_celltab INTO TABLE ls_ztc_curso_negrito-celltab[].
ENDIF.
APPEND ls_ztc_curso_negrito TO lt_ztc_curso_negrito[].
ENDLOOP.
ENDFORM.
FORM f_select_aluno.
FREE lt_ztc_curso_aluno[].
SELECT *
INTO TABLE lt_ztc_curso_aluno[]
FROM ztc_curso_aluno
WHERE nome_curso IN so_curso.
"Para adicionar os iconesLOOP AT lt_ztc_curso_aluno[] ASSIGNING FIELD-SYMBOL(<fs_curso_aluno>).
IF <fs_curso_aluno>-inscr_confirmada EQ 'X' AND
<fs_curso_aluno>-pgto_confirmado EQ 'X'.
<fs_curso_aluno>-id = icon_green_light.
<fs_curso_aluno>-color = 'C500'.
ELSEIF <fs_curso_aluno>-inscr_confirmada EQ 'X' AND
<fs_curso_aluno>-pgto_confirmado IS INITIAL.
<fs_curso_aluno>-id = icon_yellow_light.
<fs_curso_aluno>-color = 'C300'.
ELSE.
<fs_curso_aluno>-id = icon_red_light.
<fs_curso_aluno>-color = 'C600'.
ENDIF.
ENDLOOP.
ENDFORM.
FORM f_visualizar_dados_alv_compl.
IF lt_ztc_curso IS NOT INITIAL OR lt_ztc_curso_aluno IS NOT INITIAL.
IF lv_salvou_item EQ 'X'.
lo_grid_100a->refresh_table_display( ).
lo_grid_100b->refresh_table_display( ).
ELSE.
CALL SCREEN 100.
ENDIF.
ELSE.
MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'W'.
ENDIF.
ENDFORM.
FORM f_visualizar_dados_alv_basico.
DATA: lt_fieldcata_basico TYPE slis_t_fieldcat_alv,
ls_layout_basico TYPE slis_layout_alv.
CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
EXPORTING
i_structure_name = 'ztc_curso'
CHANGING
ct_fieldcat = lt_fieldcata_basico[].ls_layout_basico-colwidth_optimize = 'X'.
ls_layout_basico-zebra = 'X'.
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
EXPORTING
is_layout = ls_layout_basico
it_fieldcat = lt_fieldcata_basico[]
TABLES
t_outtab = lt_ztc_curso[] "Tabela interna de saída. (Retorno
dos dados)
EXCEPTIONS
program_error = 1
OTHERS = 2.
ENDFORM.
FORM f_salvar_alteracoes.
MODIFY ztc_curso_aluno FROM TABLE lt_ztc_curso_aluno[].
IF sy-subrc EQ 0.
COMMIT WORK.
lv_salvou_item = 'X'.
PERFORM f_select_aluno.
MESSAGE TEXT-004 TYPE 'S' DISPLAY LIKE 'S'.
ELSE.
ROLLBACK WORK.
MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module USER_COMMAND_0100 INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
CASE lv_okcode_100.
WHEN 'BACK'.
LEAVE TO SCREEN 0.
CLEAR: lv_salvou_item.
WHEN 'EXIT'.
LEAVE PROGRAM.
CLEAR: lv_salvou_item.WHEN 'SAVE' OR 'SALVAR'.
PERFORM f_salvar_alteracoes.
ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
SET PF-STATUS 'STATUS100'.
SET TITLEBAR 'TITLE100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module M_SHOW_GRID_100 OUTPUT
*&---------------------------------------------------------------------*
MODULE m_show_grid_100 OUTPUT.
FREE: lt_fieldcata[],
lt_fieldcatb[].
* ls_layout-cwidth_opt = 'X'. "Largura da coluna de acordo com o texto.
ls_layout-zebra = 'X'.
ls_layout-info_fname = 'COLOR'.
ls_layout-stylefname = 'CELLTAB'.
* ls_layout-ctab_fname = 'COLOR_CELL'. "Alguns ambientes precisam desse
atributo para colorir a coluna
ls_variant-report = sy-repid. "Usado para salvar variantes no
programa.
PERFORM f_remove_alv_button.
PERFORM f_build_grid_a.
PERFORM f_build_grid_b.
ENDMODULE.
FORM f_build_grid_a.
PERFORM f_build_fieldcat USING: "Construcao catalago de campo
'NOME_CURSO' 'NOME_CURSO' 'ztc_curso' 'Curso' '' ''
'C300' '' 'X' '' 10 CHANGING lt_fieldcata[], "Elemento de dado - campo -
nome da tabela - texto da coluna
'DATA_INICIO' 'DATA_INICIO' 'ztc_curso' 'Dt. Início' '' '' ''
'' '' '' 10 CHANGING lt_fieldcata[],'DATA_FINAL' 'DATA_FINAL' 'ztc_curso' 'Dt. Fim' '' '' ''
'' '' '' 10 CHANGING lt_fieldcata[],
'ATIVO' 'ATIVO' 'ztc_curso' 'Ativo' 'X' '' ''
'' '' '' 06 CHANGING lt_fieldcata[].
IF lo_grid_100a IS INITIAL.
"Instancia do objeto ALV
lo_container_100a = NEW cl_gui_custom_container( container_name =
'CONTAINERA' ).
lo_grid_100a = NEW cl_gui_alv_grid( i_parent = lo_container_100a
).
lo_event_grid = NEW lcl_event_grid( ).
lo_grid_100a->set_ready_for_input( 1 ). "Para selecionar multiplas
linhas
"Chama o ALV na tela a primeira vez.
lo_grid_100a->set_table_for_first_display(
EXPORTING
it_toolbar_excluding = lt_tool_bar[]
is_variant = ls_variant
is_layout = ls_layout
i_save = 'A'
CHANGING
it_fieldcatalog = lt_fieldcata[]
it_outtab = lt_ztc_curso_negrito[]
).
lo_grid_100a->set_gridtitle( 'Lista de cursos' ).
SET HANDLER lo_event_grid->hotspot_click FOR lo_grid_100a.
ELSE.
lo_grid_100a->set_frontend_fieldcatalog( "Reconstruindo o fieldcat
EXPORTING
it_fieldcatalog = lt_fieldcata[]
).
lo_grid_100a->refresh_table_display( ). "Atualiza a tela, caso haja
alteracao de dados na tebala interna.
ENDIF.
ENDFORM.
FORM f_build_grid_b.
PERFORM f_build_sort USING:
'NOME_CURSO' 'X' '' '' 'X' ' ','VALOR' 'X' '' '' '' ' '.
PERFORM f_build_fieldcat USING: "Construcao catalago de campo
'ID' 'ID' 'ICON'
'Status' '' 'X' '' '' '' '' 06 CHANGING lt_fieldcatb[],
"Elemento de dado - campo - nome da tabela - texto da coluna
'NOME_ALUNO' 'NOME_ALUNO' 'ztc_curso_aluno' 'Aluno'
'' '' '' '' '' '' 15 CHANGING lt_fieldcatb[],
'NOME_CURSO' 'NOME_CURSO' 'ztc_curso_aluno' 'Curso'
'' '' '' '' '' '' 10 CHANGING lt_fieldcatb[],
'DATA_NASCIMENTO' 'DATA_NASCIMENTO' 'ztc_curso_aluno' 'Dt.
Nascimento' '' '' '' '' '' '' 15 CHANGING lt_fieldcatb[],
'INSCR_CONFIRMADA' 'INSCR_CONFIRMADA' 'ztc_curso_aluno' 'Insc.
Confirmada' 'X' '' '' '' '' '' 15 CHANGING lt_fieldcatb[],
'PGTO_CONFIRMADO' 'PGTO_CONFIRMADO' 'ztc_curso_aluno' 'Pgto.
Confirmado' 'X' '' '' '' '' '' 15 CHANGING lt_fieldcatb[],
'VALOR' 'VALOR' 'ztc_curso_aluno' 'Valor'
'' '' '' 'X' '' 'X' 08 CHANGING lt_fieldcatb[].
IF lo_grid_100b IS INITIAL.
"Instancia do objeto ALV
lo_container_100b = NEW cl_gui_custom_container( container_name =
'CONTAINERB' ).
lo_grid_100b = NEW cl_gui_alv_grid( i_parent = lo_container_100b
).
lo_event_grid = NEW lcl_event_grid( ).
lo_grid_100b->set_ready_for_input( 1 ). "Para selecionar multiplas
linhas
"Registrar edicao de evento
lo_grid_100b->register_edit_event(
i_event_id = cl_gui_alv_grid=>mc_evt_modified "parametro de
modificacao de grid
).
"Chama o ALV na tela a primeira vez.
lo_grid_100b->set_table_for_first_display(
EXPORTING
it_toolbar_excluding = lt_tool_bar[]
is_variant = ls_variant
is_layout = ls_layout
i_save = 'A'
CHANGINGit_fieldcatalog = lt_fieldcatb[]
it_sort = lt_sort[]
it_outtab = lt_ztc_curso_aluno[]
).
lo_grid_100b->set_gridtitle( 'Lista de alunos' ).
SET HANDLER lo_event_grid->data_changed FOR lo_grid_100b.
ELSE.
lo_grid_100b->set_frontend_fieldcatalog( "Reconstruindo o fieldcat
EXPORTING
it_fieldcatalog = lt_fieldcatb[]
).
lo_grid_100b->refresh_table_display( ). "Atualiza a tela, caso haja
alteracao de dados na tebala interna.
ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module M_SHOW_GRID_200 OUTPUT
*&---------------------------------------------------------------------*
MODULE m_show_grid_200 OUTPUT.
FREE: lt_fieldcat200[].
ls_layout200-zebra = 'X'.
ls_layout200-cwidth_opt = 'X'.
PERFORM f_build_fieldcat USING: "Construcao catalago de campo
'NOME_PROF' 'NOME_PROF' 'ZTC_PROFESSOR' 'Professor' '' '' ''
'' '' '' 00 CHANGING lt_fieldcat200[].
IF lo_grid_200 IS INITIAL.
"Instancia do objeto ALV
lo_container_200 = NEW cl_gui_custom_container( container_name =
'CONTAINER200' ).
lo_grid_200 = NEW cl_gui_alv_grid( i_parent = lo_container_200 ).
lo_grid_200->set_ready_for_input( 1 ). "Para selecionar multiplas
linhas
"Chama o ALV na tela a primeira vez.
lo_grid_200->set_table_for_first_display(
EXPORTING
it_toolbar_excluding = lt_tool_bar[]is_variant = ls_variant
is_layout = ls_layout
i_save = 'A'
CHANGING
it_fieldcatalog = lt_fieldcat200[]
it_outtab = lt_ztc_professor[]
).
lo_grid_200->set_gridtitle( 'Lista de Professores' ).
ELSE.
lo_grid_200->refresh_table_display( ). "Atualiza a tela, caso haja
alteracao de dados na tebala interna.
ENDIF.
ENDMODULE.
FORM f_remove_alv_button.
* APPEND cl_gui_alv_grid=>mc_fc_excl_all TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_evt_delayed_change_select TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_evt_delayed_move_curr_cell TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_evt_enter TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_evt_modified TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_auf TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_average TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_back_classic TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_call_abc TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_call_chain TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_call_crbatch TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_call_crweb TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_call_lineitems TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_call_master_data TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_call_more TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_call_report TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_call_xint TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_check TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_col_invisible TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_col_optimize TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_count TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_current_variant TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_data_save TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_delete_filter TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_deselect_all TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_detail TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_extend TO lt_tool_bar[].APPEND cl_gui_alv_grid=>mc_fc_f4 TO lt_tool_bar[].
* APPEND cl_gui_alv_grid=>mc_fc_filter TO lt_tool_bar[].
* APPEND cl_gui_alv_grid=>mc_fc_find TO lt_tool_bar[].
* APPEND cl_gui_alv_grid=>mc_fc_find_more TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_fix_columns TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_graph TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_help TO lt_tool_bar[].
* APPEND cl_gui_alv_grid=>mc_fc_html TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_info TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_load_variant TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_loc_move_row TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_loc_paste TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_maintain_variant TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_maximum TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_minimum TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_pc_file TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_print TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_print_back TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_print_prev TO lt_tool_bar[].
APPEND cl_gui_alv_grid=>mc_fc_refresh TO lt_tool_bar[].
* APPEND cl_gui_alv_grid=>mc_fc_save_variant TO lt_tool_bar[].
* APPEND cl_gui_alv_grid=>mc_fc_subtot TO lt_tool_bar[].
* APPEND cl_gui_alv_grid=>mc_fc_sum TO lt_tool_bar[].
* APPEND cl_gui_alv_grid=>mc_fc_views TO lt_tool_bar[].
* APPEND cl_gui_alv_grid=>mc_fc_sort TO lt_tool_bar[].
* APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO lt_tool_bar[].
* APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO lt_tool_bar[].
* APPEND cl_gui_alv_grid=>mc_fc_expcrdata TO lt_tool_bar[].
* APPEND cl_gui_alv_grid=>mc_fc_expcrdesig TO lt_tool_bar[].
* APPEND cl_gui_alv_grid=>mc_fc_expcrtempl TO lt_tool_bar[].
* APPEND cl_gui_alv_grid=>mc_fc_call_xml_export TO lt_tool_bar[].
* APPEND cl_gui_alv_grid=>mc_fc_reprep TO lt_tool_bar[].
* APPEND cl_gui_alv_grid=>mc_fc_expmdb TO lt_tool_bar[].
* APPEND cl_gui_alv_grid=>mc_mb_export TO lt_tool_bar[].
"Botão exportar* APPEND cl_gui_alv_grid=>mc_fc_pc_file TO lt_tool_bar[].
"Subbotão - File local
* APPEND cl_gui_alv_grid=>mc_fc_call_xxl TO lt_tool_bar[].
"Subbotão - Planilha eletrônica
ENDFORM.
FORM f_build_fieldcat USING VALUE(p_fieldname) TYPE c
VALUE(p_field) TYPE c
VALUE(p_table) TYPE c
VALUE(p_coltext) TYPE c
VALUE(p_checkbox) TYPE c
VALUE(p_icon) TYPE c
VALUE(p_emphasize) TYPE c
VALUE(p_edit) TYPE c
VALUE(p_hotspot) TYPE c
VALUE(p_do_sum) TYPE c
VALUE(p_outputlen) TYPE i
CHANGING t_fieldcat TYPE lvc_t_fcat.
DATA: ls_fieldcat LIKE LINE OF t_fieldcat[].
ls_fieldcat-fieldname = p_fieldname.
ls_fieldcat-ref_field = p_field.
ls_fieldcat-ref_table = p_table.
ls_fieldcat-coltext = p_coltext.
ls_fieldcat-checkbox = p_checkbox.
ls_fieldcat-icon = p_icon.
ls_fieldcat-emphasize = p_emphasize.
ls_fieldcat-edit = p_edit.
ls_fieldcat-hotspot = p_hotspot.
ls_fieldcat-do_sum = p_do_sum.
ls_fieldcat-outputlen = p_outputlen.
APPEND ls_fieldcat TO t_fieldcat[].
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_BUILD_SORT
*&---------------------------------------------------------------------*
FORM f_build_sort USING p_name
p_up
p_down
p_group
p_subtot
p_expa.DATA: ls_sort LIKE LINE OF lt_sort[].
ls_sort-fieldname = p_name. "Campo de seleção do agrupamento.
ls_sort-up = p_up. "Pelo maior valor.
ls_sort-down = p_down. "Pelo menor valor.
ls_sort-group = p_group. "Agrupar.
ls_sort-subtot = p_subtot. "Subtotal.
ls_sort-expa = p_expa. "Expandido.
APPEND ls_sort TO lt_sort.
ENDFORM. " F_BUILD_SORT
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
SET PF-STATUS 'STATUS200'.
SET TITLEBAR 'TITLE200'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module USER_COMMAND_0200 INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
CASE lv_okcode_200.
WHEN 'CANCEL'.
LEAVE TO SCREEN 0.
ENDCASE.
ENDMODULE.
TELAS:
