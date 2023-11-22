REPORT zmm_sintaxesiv.

*------------------------------------------------------------------------*
*SINTAXES UTILIZADAS NO PROGRAMA: APPEND, AT END OF, SY-DATUM, SY-PAGNO, AT LINE-SELECTION, IF NOT IS INITIAL
*SET TILTEBAR, SY-ULINE, SY-VLINE, HIDE, SUM
*
*------------------------------------------------------------------------*
MESSAGE 'VERIFIQUE A TELA DE SELEÇÃO' TYPE 'I'.
TABLES : marc, mard, t001l, t001w.
* para fazer a quebra do relatório por centro, este deve ser o primeiro
* da tabela interna.
* no SELECT ... INNER JOIN os campos devem sempre seguir a seqüência da
* tabela interna.
DATA : BEGIN OF t_marc OCCURS 0,
         werks LIKE marc-werks,
         matnr LIKE marc-matnr,
         name1 LIKE t001w-name1,
         maktx LIKE makt-maktx,
         lgort LIKE mard-lgort,
         lfgja LIKE mard-lfgja,
         lfmon LIKE mard-lfmon,
         labst LIKE mard-labst,
         speme LIKE mard-speme,
       END OF t_marc.
DATA : BEGIN OF t_001 OCCURS 0,
         werks LIKE t001w-werks,
       END OF t_001.
REFRESH : t_marc.
CLEAR : t_marc.
SELECTION-SCREEN BEGIN OF BLOCK b_janela WITH FRAME TITLE text-001.
SELECT-OPTIONS s_materi FOR t_marc-matnr.
SELECT-OPTIONS s_centro FOR t_marc-werks.
SELECT-OPTIONS s_deposi FOR t_marc-lgort.
SELECT-OPTIONS s_ano FOR t_marc-lfgja.
SELECT-OPTIONS s_mes FOR t_marc-lfmon.
SELECTION-SCREEN END OF BLOCK b_janela.

INITIALIZATION.
  PERFORM f_selecionar.

AT SELECTION-SCREEN ON s_materi.

AT SELECTION-SCREEN ON s_deposi.
  PERFORM f_buscar_material.

TOP-OF-PAGE.
  PERFORM f_cabeca.

END-OF-PAGE.

START-OF-SELECTION.
  PERFORM f_buscar_dados.

END-OF-SELECTION.
  PERFORM f_mostrar_dados.
* o comando AT LINE-SELECTION é um evento que serve para
* quando clicarmos em uma linha de relatório abrir uma outra
* janela e mostrarmos outros dados
AT LINE-SELECTION.
* IF NOT var IS INITIAL -> verifica se o header line esta vazio
* se não for vazio imprime a tela de detalhes
  IF NOT t_marc IS INITIAL.
* SET TITLEBAR 'var' mostra o titulo da nova tela
    SET TITLEBAR 'DET'.
* SY-ULINE -> imprime linha com comprimento fixo
    WRITE sy-uline(83).
* SY-VLINE -> serve para fazer fechamento de bordas
    WRITE / sy-vline.
    WRITE :'Material : '.
    WRITE : t_marc-matnr,' - ', t_marc-maktx,' '.
    WRITE 83 sy-vline.
    WRITE / sy-vline.
    WRITE :'Centro : '.
    WRITE : t_marc-werks, ' - ', t_marc-name1,' '.
    WRITE 83 sy-vline.
    WRITE /:sy-vline.
    WRITE :'Deposito : '.
    WRITE : t_marc-lgort.
    WRITE 83 sy-vline.
    WRITE /:sy-vline.
    WRITE :'Disponível : '.
    WRITE : t_marc-labst.
    WRITE 83 sy-vline.
    WRITE /:sy-vline.
    WRITE : 'Bloqueado : '.
    WRITE : t_marc-speme.
    WRITE 83 sy-vline.
    WRITE / sy-uline(83).
  ENDIF.
* depois de listar a nova informação devemos limpar o header line da
* tabela
  CLEAR t_marc.
*&---------------------------------------------------------------------*
*& Form F_SELECIONAR
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM f_selecionar .
  SELECT werks INTO TABLE t_001 FROM t001w.
  REFRESH s_centro.
  LOOP AT t_001.
    CLEAR s_centro.
    s_centro-sign = 'I'.
    s_centro-option = 'EQ'.
    s_centro-low = t_001-werks.
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
  SELECT marc~werks marc~matnr t001w~name1 makt~maktx mard~lgort
  mard~lfgja mard~lfmon mard~labst mard~speme
  INTO TABLE t_marc
  FROM marc
  INNER JOIN t001w ON marc~werks = t001w~werks
  INNER JOIN makt ON marc~matnr = makt~matnr
  AND makt~spras = sy-langu
  INNER JOIN mard ON marc~matnr = mard~matnr
  WHERE t001w~werks IN s_centro
  AND marc~matnr IN s_materi
  AND mard~lfgja IN s_ano
  AND mard~lfmon IN s_mes
  AND mard~lgort IN s_deposi.
  IF sy-subrc <> 0.
    MESSAGE 'VERIFIQUE A TELA DE SELEÇÃO' TYPE 'I'.
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
  SORT t_marc BY matnr werks lgort.
* comando LOOP AT tabela serve para criar um loop até o final da tabela
* para podermos imprimir seu conteúdo.
  LOOP AT t_marc.
    WRITE /5 t_marc-matnr.
    WRITE 18 t_marc-maktx.
    WRITE 60 t_marc-werks.
    WRITE 80 t_marc-name1.
    WRITE 120 t_marc-lgort.
    WRITE 140 t_marc-lfgja.
    WRITE 150 t_marc-lfmon.
    WRITE 160 t_marc-labst.
* HIDE com os campos que queiramos mostrar na nova janela. Deve ser
* colocado depois de imprimir linha
    HIDE :t_marc-matnr,t_marc-maktx,t_marc-werks, t_marc-name1,
    t_marc-lgort,t_marc-labst, t_marc-speme.
* o comando AT END OF campo, faz com que quando houver uma quebra de
* centro ( no caso werks ) o programa entre e execute as instruções
* contidas dentro dele.
    AT END OF werks.
* o comando SUM totaliza os campos numéricos
      SUM.
      WRITE : /5 'TOTAL DO MATERIAL : ',t_marc-maktx.
      WRITE 160 t_marc-labst.
    ENDAT.
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
  WRITE AT /40 text-002.
  WRITE AT /5 'Material'.
  WRITE 18 'Descrição'.
  WRITE 60 'Centro'.
  WRITE 80 'Descrição'.
  WRITE 120 'DEPOSTIO'.
  WRITE 140 'ANO '.
  WRITE 150 'MES '.
  WRITE 180 'QTD '.
  ULINE.
ENDFORM. " F_CABECA
*&---------------------------------------------------------------------*
*& Form F_BUSCAR_MATERIAL
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM f_buscar_material .
  SELECT * FROM marc UP TO 1 ROWS WHERE matnr IN s_materi.
  ENDSELECT.
  IF sy-subrc <> 0.
    MESSAGE 'VERIFIQUE A TELA DE SELEÇÃO' TYPE 'I'.
  ENDIF.
  SELECT * FROM t001l UP TO 1 ROWS WHERE lgort IN s_deposi.
  ENDSELECT.
  IF sy-subrc <> 0.
    MESSAGE 'VERIFIQUE A TELA DE SELEÇÃO' TYPE 'I'.
  ENDIF.
ENDFORM. " F_BUSCAR_MATERIAL
