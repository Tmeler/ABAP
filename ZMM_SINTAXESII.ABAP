REPORT zmm_sintaxesii.

*------------------------------------------------------------------------*
*SINTAXES UTILIZADAS NO PROGRAMA: SELECTION-SCREEN, SELECTION-OPTIONS, SELECT INTO TABLE, SELECT INTO TABLE INNER JOIN,
*INITIALIZATION, START-OF-SELECTION, END-OF-SELECTION
*------------------------------------------------------------------------*

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

* PARA CRIAR UM BOX NA JANELA
* SELECTION-SCREEN BEGIN OF BLOCK nome_do_bloco WITH FRAME TITLE
* nome_do_elemento_de_texto
* -> DEPOIS COLOCAR OS SELECT-OPTIONS ( variáveis )
* FECHAR COM : SELECTION-SCREEN END OF BLOCK nome_do_bloco
* clicar duas vezes sobre o nome_do_bloco para colocar o nome que ira aparecer na tela

SELECTION-SCREEN BEGIN OF BLOCK b_janela WITH FRAME TITLE text-001.
SELECT-OPTIONS s_materi FOR t_marc-matnr.
SELECT-OPTIONS s_centro FOR t_marc-werks.
SELECTION-SCREEN END OF BLOCK b_janela.

* COMANDO PARA INICIALIZAÇÃO DE VARIAVEL
INITIALIZATION.

* seleciona o CAMPO_que_iremos tratar INTO TABLE
* tabela_interna FROM tabela_de_onde_vira os dados

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

* EVENTO DE EXTRAÇÃO DE DADOS - ANTES DA SELEÇÃO
START-OF-SELECTION.

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

*WRITE 5 'MATERIAL'.
*WRITE 18 'Descrição'.
*WRITE 60 'Centro'.
*WRITE 80 'Descrição'.
*ULINE.
* EVENTO PARA EXIBIR OS DADOS
END-OF-SELECTION.
  LOOP AT t_marc.
    WRITE 5 t_marc-matnr.
    WRITE 18 t_marc-maktx.
    WRITE 60 t_marc-werks.
    WRITE 80 t_marc-name1.
    SKIP 1.
  ENDLOOP.
