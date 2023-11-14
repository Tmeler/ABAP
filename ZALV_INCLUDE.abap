REPORT ZALV_INCLUDE.

*-----------------------------------------------------------------------
* Tipos standard para ALV
* O conjunto de tipos VRM e SLIS são utilizados por funções ALV
*-----------------------------------------------------------------------
TYPE-POOLS: kkblo,
            vrm,                             "Necessário para uso de ALV
            slis.                            "Tipos globais para ALV

*-----------------------------------------------------------------------
* Estruturas (SLIS)
*-----------------------------------------------------------------------
DATA: t_linecolor    TYPE slis_specialcol_alv OCCURS 0 WITH HEADER LINE,
      t_sort         TYPE slis_sortinfo_alv   OCCURS 0 WITH HEADER LINE,
      t_listheader   TYPE slis_t_listheader            WITH HEADER LINE,
      t_fieldcat     TYPE slis_t_fieldcat_alv          WITH HEADER LINE,
      t_events       TYPE slis_t_event,
      t_layout       TYPE slis_layout_alv,
      t_print        TYPE slis_print_alv,
      event_exit_row TYPE slis_event_exit,
      event_exit_tbl TYPE slis_t_event_exit,
      v_f_name       TYPE slis_fieldname.

DATA: gs_lineinfo    TYPE kkblo_lineinfo,
      v_listheader   LIKE t_listheader-info,
      v_variante     LIKE disvariant,
      v_colpos       TYPE i,
      v_f_color      TYPE i,
      v_f_int        TYPE i.

DATA: v_i_save       TYPE c VALUE 'A'.
* Opções - 'X' - salvar somente global
*          'U' - salvar somente layout do usuário
*          'A' - permite salvar o layout global e do usuário
*          ' ' - não permite salvar layout


*&---------------------------------------------------------------------*
*&      Form  RECUPERA_LAYOUTS_SALVOS
*&---------------------------------------------------------------------*
*       Abre um search help com os layouts já gravados.
*----------------------------------------------------------------------*
FORM recupera_layouts_salvos USING p_repid
                          CHANGING p_varia.
  v_variante-report = p_repid.
  v_variante-username = sy-uname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = v_variante
      i_save     = v_i_save
    IMPORTING
      es_variant = v_variante
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    p_varia = v_variante-variant.
  ENDIF.

ENDFORM.                    " RECUPERA_LAYOUTS_SALVOS
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV
*&---------------------------------------------------------------------*
*       Busca layout de exibição default para o relatório.
*----------------------------------------------------------------------*
FORM init_alv USING p_repid
           CHANGING p_varia.
  CLEAR v_variante.
  v_variante-report   = p_repid.
  v_variante-username = sy-uname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = v_i_save
    CHANGING
      cs_variant = v_variante
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 0.
    p_varia = v_variante-variant.
  ENDIF.

ENDFORM.                    " INIT_ALV
*&---------------------------------------------------------------------*
*&      Form  F_CONFIGURA_ALV
*&---------------------------------------------------------------------*
FORM f_configura_alv USING p_varia
                           p_tipo_events.
  t_layout-expand_all           = 'X'.       "Abrir subitens
  t_layout-colwidth_optimize    = 'X'.       "Largura melhor possível da coluna
  t_layout-no_min_linesize      = 'X'.
  t_layout-zebra                = 'X'.
  t_layout-detail_initial_lines = 'X'.

  v_variante-variant         = p_varia.
  t_print-no_print_listinfos = 'X'.

  IF p_tipo_events <> ' '.
    PERFORM f_events USING t_events
                           p_tipo_events.
  ENDIF.

ENDFORM.                               " F_CONFIGURA_ALV
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_ESTRUTURA
*&---------------------------------------------------------------------*
FORM f_monta_estrutura USING value(p_0561)
                             value(p_0562)
                             value(p_0563)
                             value(p_0564)
                             value(p_0565)
                             value(p_0566)
                             value(p_0567)
                             value(p_0568)
                             value(p_0569).
  ADD 1 TO v_colpos.

  CLEAR t_fieldcat.
  t_fieldcat-col_pos       = v_colpos.
  t_fieldcat-fieldname     = p_0561.
  t_fieldcat-tabname       = p_0562.
  t_fieldcat-ref_tabname   = p_0563.
  t_fieldcat-key           = p_0564.
  t_fieldcat-outputlen     = p_0565.
  t_fieldcat-seltext_m     = p_0566.
  t_fieldcat-do_sum        = p_0567.
  t_fieldcat-no_zero       = p_0568.
  t_fieldcat-edit          = p_0569.

  IF NOT p_0566 IS INITIAL.
    t_fieldcat-ddictxt     = 'M'.
  ENDIF.

  APPEND t_fieldcat.

ENDFORM.                               " F_MONTA_ESTRUTURA
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_SORT
*&---------------------------------------------------------------------*
FORM f_monta_sort USING value(p_0561)
                        value(p_0562)
                        value(p_0563)
                        value(p_0564)
                        value(p_0565).
  ADD 1 TO v_colpos.

  CLEAR t_sort.
  t_sort-spos      = v_colpos.
  t_sort-fieldname = p_0561.
  t_sort-tabname   = p_0562.
  t_sort-up        = p_0563.
  t_sort-down      = p_0564.
  t_sort-subtot    = p_0565.

  APPEND t_sort.

ENDFORM.                               " F_MONTA_SORT
*&---------------------------------------------------------------------*
*&      Form  F_CELL_COLOR
*&---------------------------------------------------------------------*
FORM f_cell_color TABLES color_tab TYPE slis_t_specialcol_alv
                   USING fname     TYPE slis_fieldname
                         color     TYPE i
                         int       TYPE i.
* 0 - Cinza
* 1 - Azul
* 2 - Lay-out padrão
* 3 - Amarelo
* 4 - Azul Claro
* 5 - Verde claro
* 6 - Vermelho
* 7 - Laranjado
* 8 - Normal (não coloca cor)

  DATA: s_color    TYPE slis_specialcol_alv,
        cell_color TYPE slis_color.

  READ TABLE color_tab INTO s_color WITH KEY fieldname = fname.

  cell_color-col    = color.
  cell_color-int    = int.
  s_color-fieldname = fname.
  s_color-color     = cell_color.

  IF sy-subrc = 0.
    MODIFY color_tab FROM s_color INDEX sy-tabix.
  ELSE.
    APPEND s_color TO color_tab.
  ENDIF.

ENDFORM.                    " F_CELL_COLOR
*&---------------------------------------------------------------------*
*&      Form  F_EVENTS_EXIT
*&---------------------------------------------------------------------*
FORM f_events_exit.
* Preenchendo tabela com teclas exit
  REFRESH event_exit_tbl.

  event_exit_row-after = 'X'.
  CLEAR event_exit_row-before.
  event_exit_row-ucomm = '&F03'.
  APPEND event_exit_row TO event_exit_tbl.

  event_exit_row-after = 'X'.
  CLEAR event_exit_row-before.
  event_exit_row-ucomm = '&F15'.
  APPEND event_exit_row TO event_exit_tbl.

  event_exit_row-after = 'X'.
  CLEAR event_exit_row-before.
  event_exit_row-ucomm = '&F12'.
  APPEND event_exit_row TO event_exit_tbl.

ENDFORM.                    " F_EVENTS_EXIT
*&---------------------------------------------------------------------*
*&      Form  F_EVENTS
*&---------------------------------------------------------------------*
FORM f_events USING t_events TYPE slis_t_event
                    p_tipo.
  DATA e_events TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    IMPORTING
      et_events       = t_events
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF p_tipo >= 1.
    READ TABLE t_events INTO e_events
                    WITH KEY name = slis_ev_top_of_page.
    e_events-form = 'ALV_TOP_OF_PAGE'.
    MODIFY t_events FROM e_events INDEX sy-tabix.
  ENDIF.

  IF p_tipo >= 2.
    READ TABLE t_events INTO e_events
                    WITH KEY name = slis_ev_user_command.
    e_events-form = 'ALV_USER_COMMAND'.
    MODIFY t_events FROM e_events INDEX sy-tabix.
  ENDIF.

  IF p_tipo >= 3.
    READ TABLE t_events INTO e_events
                    WITH KEY name = slis_ev_before_line_output.
    e_events-form = 'ALV_BEFORE_LINE_OUTPUT'.
    MODIFY t_events FROM e_events INDEX sy-tabix.
  ENDIF.

  IF p_tipo >= 4.
    READ TABLE t_events INTO e_events
                    WITH KEY name = slis_ev_after_line_output.
    e_events-form = 'ALV_AFTER_LINE_OUTPUT'.
    MODIFY t_events FROM e_events INDEX sy-tabix.
  ENDIF.

ENDFORM.                    " F_EVENTS
