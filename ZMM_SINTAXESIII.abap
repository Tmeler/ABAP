REPORT zmm_sintaxesiii.

MESSAGE 'Erro' TYPE 'E'.

*------------------------------------------------------------------------*
*SINTAXES UTILIZADAS NO PROGRAMA: NO STANDARD PAGE HEADING, AT SELECTION-SCREEN ON
*TOP-OF-PAGE, END-OF-PAGE, MESSAGE-ID, MESSAGE, SY-SYBRC, CONCATENATE
*------------------------------------------------------------------------*

* -> LINE-SIZE n_colunas
* -> LINE-COUNT n_linhas
* -> NO STANDARD PAGE HEADING ( não usar cabeçalho padrão )
* -> MESSAGE-ID z01 ( onde z01 é a classe de mensagem )

TABLES : marc, t001w.


DATA : BEGIN OF t_marc OCCURS 0,
         matnr LIKE marc-matnr,
         werks LIKE marc-werks,
         name1 LIKE t001w-name1,
         maktx LIKE makt-maktx,
       END OF t_marc.
DATA : BEGIN OF t_001 OCCURS 0,
         werks LIKE t001w-werks,
       END OF t_001.
REFRESH : t_marc.
CLEAR : t_marc.
* PARA CRIAR UM BOX NA JANELA SELECTION-SCREEN BEGIN OF BLOCK nome_do_bloco WITH FRAME title nome_do_elemento_de_texto
* -> DEPOIS COLOCAR OS SELECT-OPTIONS ( variáveis )
* FECHAR COM : SELECTION-SCREEN END OF BLOCK nome_do_bloco
* clicar duas vezes sobre o nome_do_bloco para colocar o nome que ira aparecer na tela
SELECTION-SCREEN BEGIN OF BLOCK b_janela WITH FRAME TITLE text-001.
SELECT-OPTIONS s_materi FOR t_marc-matnr.
SELECT-OPTIONS s_centro FOR t_marc-werks.
SELECTION-SCREEN END OF BLOCK b_janela.

* COMANDO PARA INICIALIZAÇÃO DE VARIAVEL
INITIALIZATION.
* PARA CRIAR ROTINAS : PERFORM nome_da_rotina. NÃO USAR ROTINAS PARA CRIAR TABELAS E abrir tabelas estas devem estar no inicio do programa fazer sempre dentro dos eventos
  PERFORM f_selecionar.
* ANALISA O RESULTADO DA VARIAVEL s_materi ANTES DE INICIAR LISTA DE MATERIAL
AT SELECTION-SCREEN ON s_materi.
  PERFORM f_buscar_material.

TOP-OF-PAGE.

  PERFORM f_cabeca.

END-OF-PAGE.
* EVENTO DE EXTRAÇÃO DE DADOS - ANTES DA SELEÇÃO
START-OF-SELECTION.
  PERFORM f_buscar_dados.
* EVENTO PARA EXIBIR OS DADOS
END-OF-SELECTION.
  PERFORM f_mostrar_dados.
*&---------------------------------------------------------------------*
*& Form F_SELECIONAR
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM f_selecionar .
* seleciona o CAMPO_que_iremos tratar INTO TABLE tabela_interna FROM tabela_de_onde_vira os dados
  SELECT werks INTO TABLE t_001 FROM t001w.
* LIMPA A TABELA DE VARIAVEL
  REFRESH s_centro.
* LOOP DA TABELA INTERNA
  LOOP AT t_001.
* LIMPA A VARIAVEL
    CLEAR s_centro.
* iguala SIGN a I e OPTION a EQ
    s_centro-sign = 'I'.
    s_centro-option = 'EQ'.
* COLOCAR O VALOR do campo
    s_centro-low = t_001-werks.
* atualiza a tabela.
    APPEND s_centro.
  ENDLOOP.
ENDFORM. " F_SELECIONAR
*&---------------------------------------------------------------------*
*& Form F_BUSCAR_DADOS
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM f_buscar_dados .
* USO DE INNER JOIN
* SELECT campos_das_tabelas que contem os dados
* INTO TABLE nome_da_tabela que ira guardar os dados
* FROM tabela principal para fazer o join
* INNER JOIN tabela secundária para fazer o join com o from
* ON condição de ligação do JOIN
* WHERE condições
  SELECT marc~matnr marc~werks t001w~name1 makt~maktx
  INTO TABLE t_marc
  FROM marc
  INNER JOIN t001w ON marc~werks = t001w~werks
  INNER JOIN makt ON marc~matnr = makt~matnr
  AND makt~spras = sy-langu
  WHERE t001w~werks IN s_centro
  AND marc~matnr IN s_materi.
* sy-subrc <> 0 não achou nada
* MESSAGE tipo_de_mensagem (E/I/W/S)+numero_seq_da_mensagem
  IF sy-subrc <> 0.
    MESSAGE 'Não Localizado' TYPE 'E'.
  ENDIF.

ENDFORM. " F_BUSCAR_DADOS
*&---------------------------------------------------------------------*
*& Form F_MOSTRAR_DADOS
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM f_mostrar_dados .
  LOOP AT t_marc.
    WRITE 5 t_marc-matnr.
    WRITE 18 t_marc-maktx.
    WRITE 60 t_marc-werks.
    WRITE 80 t_marc-name1.
    SKIP 1.
  ENDLOOP.
ENDFORM. " F_MOSTRAR_DADOS
*&---------------------------------------------------------------------*
*& Form F_CABECA
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM f_cabeca .
  DATA novo(60) TYPE c.
  WRITE 5 sy-datum.
  WRITE 50 'CHEMYUNION QUIMICA LTDA'.
  WRITE 120 sy-pagno.
* WRITE AT /40 'RELATÓRIO DE MATERIAIS POR CENTRO DE CUSTO : '.
  CONCATENATE text-002 ' : ' s_centro+3(4) INTO novo.
  WRITE AT /40 novo.
  WRITE AT /5 'Material'.
  WRITE 18 'Descrição'.
  WRITE 60 'Centro'.
  WRITE 80 'Descrição'.
  ULINE.
ENDFORM. " F_CABECA
*&---------------------------------------------------------------------*
*& Form F_BUSCAR_MATERIAL
*&---------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM f_buscar_material .
* VERIFICA SE EXISTE O MATERIAL
  SELECT * FROM marc UP TO 1 ROWS WHERE matnr IN s_materi.
  ENDSELECT.
* ANALISA O RESULTADO DO SELECT
  IF sy-subrc <> 0.
    MESSAGE 'Não Localizado' TYPE 'W'.
  ENDIF.
ENDFORM. " F_BUSCAR_MATERIAL
