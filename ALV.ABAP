*&---------------------------------------------------------------------*
*& Report  Z_ALV_DEBUG
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_alv_debug.

*-----------------------------------------------------------------------
* Tabelas transparentes
*-----------------------------------------------------------------------
TABLES:
 vbrk. "Documentos de faturamento
*-----------------------------------------------------------------------
* Tipos standard
*
* O conjunto de tipos VRM e SLIS são utilizados por funções ALV. Defina
* sempre no início. O mais importante mesmo é o SLIS.
*-----------------------------------------------------------------------
TYPE-POOLS:
 vrm, "Necessário para uso de ALV
 slis. "Tipos globais para ALV
*-----------------------------------------------------------------------
* Tipos do usuário
*-----------------------------------------------------------------------
TYPES:
 BEGIN OF y_vbrk,
 vbeln LIKE vbrk-vbeln, "Número documento
 kdgrp LIKE vbrk-kdgrp, "Grupo de clientes
 netwr LIKE vbrk-netwr, "Valor líquido
 mwsbk LIKE vbrk-mwsbk, "Montante do imposto
 fkart LIKE vbrk-fkart, "Tipo documento faturamento
 vtweg LIKE vbrk-vtweg, "Canal de distribuição
 kunag LIKE vbrk-kunrg, "Emissor da ordem
 xblnr LIKE vbrk-xblnr, "Nota fiscal
 mark TYPE c, "Marcar alterações
 END OF y_vbrk.
*-----------------------------------------------------------------------
* Tabelas internas ALV
*
* As estruturas aqui utilizadas (SLIS) estão explicadas com as opções
* mais importantes no final da apostila
*-----------------------------------------------------------------------
DATA:
 t_linecolor TYPE slis_specialcol_alv OCCURS 0 WITH HEADER LINE,
 t_listheader TYPE slis_t_listheader,
 t_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
 t_sort TYPE slis_sortinfo_alv OCCURS 0 WITH HEADER LINE.
DATA:
 v_listheader TYPE slis_listheader, "Cabeçalho
 v_layout TYPE slis_layout_alv, "layout para saída
 v_print TYPE slis_print_alv, "Ctrl de impressão
 v_variante LIKE disvariant. "Variante de exibição

*-----------------------------------------------------------------------
* Tabelas internas
*-----------------------------------------------------------------------
DATA:
 t_vbrk TYPE y_vbrk OCCURS 0 WITH HEADER LINE.
* A próxima tabela é necessário porque não é possível um select em
* tabelas que possuem campos como outras tabelas.
* No caso foi necessário incluir a SLIS_T_SPECIALCOL_ALV
DATA: BEGIN OF t_alv OCCURS 0.
        INCLUDE STRUCTURE t_vbrk.
DATA:
 color TYPE slis_t_specialcol_alv. "Definir a cor
DATA: END OF t_alv.
*-----------------------------------------------------------------------
* Variáveis de uso geral
*-----------------------------------------------------------------------
DATA:
 v_tabix LIKE sy-tabix,
 v_repid LIKE sy-repid,
 v_flag.
*-----------------------------------------------------------------------
* Tela de seleção
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK one.
SELECT-OPTIONS:
 s_vbeln FOR vbrk-vbeln. "Documento de faturamento
SELECTION-SCREEN SKIP.
PARAMETERS:
 p_varia LIKE disvariant-variant. "Variante de exibição
*-----------------------------------------------------------------------
* O usuário terá a opção de iniciar a apresentação do relatório com
* algum layout salvo anteriormente.
* Essa escolha será armazenada em P_VARIA. Utilizamos uma função que
* retorna todos os layout possíveis.
*-----------------------------------------------------------------------
SELECTION-SCREEN END OF BLOCK one.
*-----------------------------------------------------------------------
* Eventos
*-----------------------------------------------------------------------
INITIALIZATION.
  PERFORM zf_init_alv.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.
  PERFORM zf_recupera_layouts_salvos.
*-----------------------------------------------------------------------
* Principal
*-----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM:
  zf_selecao_dados, "Seleciona a VBRK
  zf_altera_cores, "Alterando as cores dos campos
  zf_monta_tabela_alv, "Preenche o catálogo
  zf_sort_subtotal, "Ordenação dos campos e subtotais
  zf_executa_funcao_alv. "Gera o relatório

END-OF-SELECTION.
*-----------------------------------------------------------------------
* Rotinas
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
* Form zf_init_alv
*-----------------------------------------------------------------------
* Busca layout de exibição default para o relatório. Se houver
* algum formato padrão para o relatório, essa função busca e já
* apresenta o relatório nesse formato.
* Um layout fica como default quando marcamos "Config.Prelim." Um
* flag que pode ser marcado na opção "Gravar layout" na barra de
* ferramentas do ALV
*-----------------------------------------------------------------------
FORM zf_init_alv.
  v_repid = sy-repid.
  CLEAR v_variante.
  v_variante-report = v_repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = 'A'
    CHANGING
      cs_variant = v_variante
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 0.
    p_varia = v_variante-variant.
  ENDIF.
ENDFORM.
*-----------------------------------------------------------------------
* Form zf_recupera_layouts_salvos
*-----------------------------------------------------------------------
* Abre um search help com os layouts já gravados. Se o usuário
* escolher algum aqui, o programa vai iniciar a apresentação do
* relatório com esse layout, e não o que é default, retornado na
* função REUSE_ALV_VARIANT_DEFAULT_GET em ZF_INIT_ALV (Acima)
*-----------------------------------------------------------------------
FORM zf_recupera_layouts_salvos.
  v_variante-report = v_repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = v_variante
      i_save     = 'A'
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
ENDFORM.
*-----------------------------------------------------------------------
* Form zf_selecao_dados
*-----------------------------------------------------------------------
* Seleção dos dados
*-----------------------------------------------------------------------
FORM zf_selecao_dados.

  SELECT vbeln kdgrp netwr mwsbk
  fkart vtweg kunag xblnr
  FROM vbrk
  INTO TABLE t_vbrk
  WHERE vbeln IN s_vbeln.
  LOOP AT t_vbrk.
    MOVE-CORRESPONDING t_vbrk TO t_alv.
    APPEND t_alv.
  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
* Form zf_altera_cores
*----------------------------------------------------------------------*
* Permite informar a cor que vc deseja para a coluna, inclusive
* pelo valor da variável
*----------------------------------------------------------------------*
FORM zf_altera_cores.
* Veja algumas cores
* Azul = 1
* Verde = 5
* Normal = 2
* Vermelha = 6
* Mas vai depender de como estão as cores do seu SAP-Gui
  LOOP AT t_alv.
    REFRESH t_linecolor.
    REFRESH t_alv-color.
    t_linecolor-fieldname = 'VBELN'.
    t_linecolor-color-col = '6'.
    t_linecolor-color-inv = '1'. "Inverso, 1 liga e 0 desliga
    APPEND t_linecolor.
* Temos a opção INV, que é invertido, mesmo esquema do INT
* 1 liga e 0 desliga
    IF t_vbrk-netwr > 10000.
      t_linecolor-fieldname = 'NETWR'.
      t_linecolor-color-col = '1'.
      t_linecolor-color-int = '1'. "Negrito (1-ligado, 0-Desligado)
    ELSE.
      t_linecolor-fieldname = 'NETWR'.
      t_linecolor-color-col = '6'.
      t_linecolor-color-int = '1'. "Negrigo (1-ligado, 0-Desligado)
    ENDIF.
    APPEND t_linecolor.
    t_alv-color[] = t_linecolor[].
    MODIFY t_alv.
  ENDLOOP.
ENDFORM.

*-----------------------------------------------------------------------
* Form zf_monta_tabela_alv
*-----------------------------------------------------------------------
* Monta tabela para apresentação do relatório. Aqui montamos um
* catálogo com as informações dos campos.
* Veja que não estamos preenchendo todas as opções do catálogo,
* não é necessário. No anexo você poderá encontrar os principais
*-----------------------------------------------------------------------
FORM zf_monta_tabela_alv.
  CLEAR t_fieldcat.
  t_fieldcat-fieldname = 'MARK'.
  t_fieldcat-tabname = 'T_ALV'.
  t_fieldcat-reptext_ddic = 'S'.
  t_fieldcat-inttype = 'C'.
  t_fieldcat-outputlen = 1.
  t_fieldcat-checkbox = 'X'.
  APPEND t_fieldcat.
  CLEAR t_fieldcat.
  t_fieldcat-fieldname = 'VBELN'.
  t_fieldcat-tabname = 'T_ALV'.
  t_fieldcat-reptext_ddic = 'Doc. Fatura'.
  t_fieldcat-inttype = 'C'.
  t_fieldcat-outputlen = 10.
  APPEND t_fieldcat.
  CLEAR t_fieldcat.
  t_fieldcat-fieldname = 'KDGRP'.
  t_fieldcat-tabname = 'T_ALV'.
  t_fieldcat-reptext_ddic = 'Grupo de Clientes'.
  t_fieldcat-inttype = 'C'.
  t_fieldcat-outputlen = 2.
  APPEND t_fieldcat.
* Para o campo NETWR, o relatório já vai mostrar linha de total
  CLEAR t_fieldcat.
  t_fieldcat-fieldname = 'NETWR'.
  t_fieldcat-tabname = 'T_ALV'.
  t_fieldcat-reptext_ddic = 'Valor líquido'.
  t_fieldcat-inttype = 'P'.
  t_fieldcat-outputlen = 15.
  t_fieldcat-do_sum = 'X'.
  APPEND t_fieldcat.
  CLEAR t_fieldcat.
  t_fieldcat-fieldname = 'MWSBK'.
  t_fieldcat-tabname = 'T_ALV'.
  t_fieldcat-reptext_ddic = 'Montante do Imposto'.
  t_fieldcat-inttype = 'P'.
  t_fieldcat-outputlen = 15.
  APPEND t_fieldcat.
* Os campos abaixo não irão aparecer no relatório, apenas quando
* o usuário modificar o layout e inserir esses campos nas colunas
* a serem apresentadas

  CLEAR t_fieldcat.
  t_fieldcat-fieldname = 'FKART'.
  t_fieldcat-tabname = 'T_ALV'.
  t_fieldcat-reptext_ddic = 'Tipo do documento'.
  t_fieldcat-inttype = 'C'.
  t_fieldcat-outputlen = 4.
  t_fieldcat-no_out = 'X'.
  APPEND t_fieldcat.
  CLEAR t_fieldcat.
  t_fieldcat-fieldname = 'VTWEG'.
  t_fieldcat-tabname = 'T_ALV'.
  t_fieldcat-reptext_ddic = 'Canal de Distribuição'.
  t_fieldcat-inttype = 'C'.
  t_fieldcat-outputlen = 2.
  t_fieldcat-no_out = 'X'.
  APPEND t_fieldcat.
  CLEAR t_fieldcat.
  t_fieldcat-fieldname = 'KUNAG'.
  t_fieldcat-tabname = 'T_ALV'.
  t_fieldcat-reptext_ddic = 'Emissor da Ordem'.
  t_fieldcat-inttype = 'C'.
  t_fieldcat-outputlen = 10.
  t_fieldcat-no_out = 'X'.
  APPEND t_fieldcat.
* Para o campo XBLNR, não vamos preencher nada. Nem disponível
* na modificação do layout ele vai estar.
* Não é necessário atribuir todos os campos, não ocorre
* nenhum erro.
ENDFORM.
*-----------------------------------------------------------------------
* Form zf_sort_subtotal
*-----------------------------------------------------------------------
* Classificação e item de subtotalização
*-----------------------------------------------------------------------
FORM zf_sort_subtotal.
  CLEAR t_sort[].
  t_sort-spos = 1.
  t_sort-fieldname = 'KDGRP'.
  t_sort-tabname = 'T_ALV'.
  t_sort-up = 'X'.
  t_sort-subtot = 'X'.
  APPEND t_sort.
* Com isso o relatório vai sair classificado em ordem crescente de Grupo
* de cliente e ainda irá aparecer um subtotal por esse campo.
ENDFORM.
*-----------------------------------------------------------------------
* Form zf_executa_funcao_alv
*-----------------------------------------------------------------------
* Apresenta relatório
*-----------------------------------------------------------------------
FORM zf_executa_funcao_alv.
* Preenchendo algumas opções de impressão (Não é obrigatório)
  v_layout-expand_all = 'X'. "Abrir subitens
  v_layout-colwidth_optimize = 'X'. "Largura melhor possível da coluna
  v_layout-edit = 'X'. "Permitir a edição

* Indicando para função qual o layout que deve ser apresentado
* primeiro
  v_variante-variant = p_varia.
  v_print-no_print_listinfos = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = v_repid
      i_background_id         = 'ALV_BACKGROUND'
      i_callback_top_of_page  = 'ZF_TOP_OF_PAGE'
*     i_callback_pf_status_set = 'ZF_STATUS'
      i_callback_user_command = 'ZF_USER_COMMAND'
      it_fieldcat             = t_fieldcat[]
      is_layout               = v_layout
      it_sort                 = t_sort[]
      i_default               = 'X'
      i_save                  = 'A'
      is_variant              = v_variante
      is_print                = v_print
    TABLES
      t_outtab                = t_alv
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
* As funções que geram relatórios ALV possuem vários parâmetros de
* I_CALLBACK. Os que mais são utilizados, são os que estão
* na chamada acima. Para ver os demais use a transação SE37. Esses
* parâmetros são preenchidos com nomes de FORMS do programa
* i_callback_program = Qual programa que executou a função
* i_callback_top_of_page = Rotina de cabeçalho
* i_callback_pf_status_set = Qual barra de tarefas a função vai usar
* i_callback_user_command = Tratamento dos botões alterados ou criados
ENDFORM.
*-----------------------------------------------------------------------
* Form zf_user_command
*-----------------------------------------------------------------------
* Tratamento das opções do usuário. Por exemplo um Drill-down ou
* algum botão que você inseriu ou alterou. O importante é conhecer
* os parâmetros que o form recebe
*-----------------------------------------------------------------------
FORM zf_user_command USING ucomm LIKE sy-ucomm
 selfield TYPE slis_selfield.
* UCOMM: é o sy-ucomm (Ok-code)
* SELFIELD: é uma estrutura com dados que nós permite identifcar
* o que foi selecionado. Essa estrutura também está
* explicada no anexo ao final da apostila
* Salva a posição do relatório (Linha escolhida)
  selfield-row_stable = 'X'.
* Uma das questões foi como alterar o conteúdo de uma tabela
* transparente com as alterações feitas no relatório ALV
* Segue um exemplo de como pode ser feito:
* Em nossa barra de ferramentas criamos o botão com código
* ZATU
  IF ucomm = 'ZATU'.
* Vamos ler a tabela T_VBRK onde mark = X. A idéia é que

* o usuário mark com X os registros alterados
    LOOP AT t_vbrk WHERE mark = 'X'.
      v_tabix = sy-tabix.
* Atualiza a tabela transparente
      " update ztabela ....
* Então voltamos a T_VBRK sem marcação alguma
      CLEAR t_vbrk-mark.
      MODIFY t_vbrk INDEX v_tabix.
* Veja que esse tipo de esquema pode ser feito para
* excluir registros também
    ENDLOOP.
  ENDIF.
* Para testar o código do botão
  IF ucomm = 'ZLOG'.
    " perform ...
    " call transaction...
  ENDIF.
* Para um drill down a partir de um registro
  IF NOT selfield-tabindex = 0.
    READ TABLE t_vbrk INDEX selfield-tabindex.
    "perform ...
    "call transaction ...
  ELSE.
* Clicou em linha inválida, linha de total, cabeçalho, etc
  ENDIF.
ENDFORM.
*-----------------------------------------------------------------------
* Form zf_top_of_page
*-----------------------------------------------------------------------
* Cabeçalho do relatório
*-----------------------------------------------------------------------
FORM zf_top_of_page.
* Uma dica, em relatórios ALV com utilização de drill down, na volta ao
* relatório principal ele vai executar novamente o cabeçalho. Isso faz
* com que ele fique duplicado. Utilize um flag que após apresentar o
* cabeçalho uma vez, fique marcado com um 'X', por exemplo.
* V_FLAG na primeira vez é branco.
  CHECK v_flag IS INITIAL.
* Monta as linhas de cabeçalho
  CLEAR t_listheader[].
  CLEAR v_listheader.
  v_listheader-typ = 'H'.
* TYP = H, faz com que a fonte fique maior
  v_listheader-info = 'Apenas um Exemplo em ALV'.
  APPEND v_listheader TO t_listheader.
* Definição do Projeto
  CLEAR v_listheader.
  v_listheader-typ = 'A'.

* TYP = S, outro tipo de fonte
  v_listheader-info = 'Segunda linha do cabeçalho'.
  APPEND v_listheader TO t_listheader.
* O campo INFO, pode ter no máximo 60 caracteres
* Apresenta o cabeçalho.
* Veja que já é uma outra função, essa apresenta a figura, já
* a função REUSE_ALV_GRID_DISPLAY (Principal) tem o parâmetro
* que você indica a imagem que será apresentada em background,
* como na WEB.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      i_logo             = 'ENJOYSAP_LOGO'
      it_list_commentary = t_listheader.
* Para não apresentar mais o cabeçalho no refresh
  v_flag = 'X'.
ENDFORM.
*----------------------------------------------------------------------
* Form zf_status
*----------------------------------------------------------------------
* Status com botão de log (Item a mais na barra ALV)
*----------------------------------------------------------------------
FORM zf_status USING rt_extab TYPE slis_t_extab.
* Aqui estamos informando a função que ela deverá utilizar a barra de
* ferramentas ZALV_BOTOES.
  "set pf-status 'ZALV_BOTOES'.
* Também é possível excluir funções
  "if sy-uname = ...
  "EXCLUDING ...
  "endif.
ENDFORM.
