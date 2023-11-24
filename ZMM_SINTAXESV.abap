REPORT zmm_sintaxesv.

*------------------------------------------------------------------------*
*SINTAXES UTILIZADAS NO PROGRAMA: DATA (INCLUDE STRUCTURE), CONSTANTS, PARAMETERS RLGRAP-FILENAME
*WS_UPLOAD, SY-MANDT,SPLIT, UNPACK
*INSERT, MOVE-CORRESPONDING, COMMIT WORK
*------------------------------------------------------------------------*

* TABELA QUE IRA CONTER OS DADOS IMPORTADOS
TABLES : ztab1_01.
* TABELA INTERNA PARA MANIPULAR OS DADOS
DATA : BEGIN OF t_tabela OCCURS 0,
         texto01(100) TYPE c,
       END OF t_tabela.
* COMO CRIAR UMA TABELA INTERNA COPIANDO A ESTRUTURA DA TABELA INTERNA.
DATA : BEGIN OF t_tempo OCCURS 0.
        INCLUDE STRUCTURE ztab1_01.
DATA : END OF t_tempo.
* CRIA UMA CONSTANTE caractere com o valor ';'
CONSTANTS c_tipo TYPE c VALUE ';'.
SELECTION-SCREEN BEGIN OF BLOCK b_janela WITH FRAME TITLE text-001.
* nome do parametro para importar o arquivo -> RLGRAP-FILENAME
PARAMETERS p_arq LIKE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK b_janela.

START-OF-SELECTION.
  PERFORM f_carregar_dados.
  PERFORM f_dados_p_tab_interna.
  PERFORM f_atualizar_dados.
*&---------------------------------------------------------------------*
*& Form F_CARREGAR_DADOS
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM f_carregar_dados .
* limpar a tabela de entrada e o read line da tabela
  REFRESH t_tabela.
  CLEAR t_tabela.
* FUNÇÃO PARA IMPORTAR ARQUIVO
* chamar a função usar o botão MODELO
  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
*     CODEPAGE                = ' '
      filename                = p_arq " COLOCAR O NOME DA VARIAVEL
      " IRA CONTER O NOME DO ARQUIVO
      filetype                = 'ASC' " TIPO DE ARQUIVO
*     HEADLEN                 = ' '
*     LINE_EXIT               = ' '
*     TRUNCLEN                = ' '
*     USER_FORM               = ' '
*     USER_PROG               = ' '
*     DAT_D_FORMAT            = ' '
* IMPORTING
*     FILELENGTH              =
    TABLES
      data_tab                = t_tabela " NOME DA TABELA INTERNA QUE
      " IRA RECEBER OS DADOS
* CODIGOS DE ERROS DE RETONO
    EXCEPTIONS
      conversion_error        = 1
      file_open_error         = 2
      file_read_error         = 3
      invalid_type            = 4
      no_batch                = 5
      unknown_error           = 6
      invalid_table_width     = 7
      gui_refuse_filetransfer = 8
      customer_error          = 9
      no_authority            = 10
      OTHERS                  = 11.
* tratamento dos erros.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM. " F_CARREGAR_DADOS
*&---------------------------------------------------------------------*
*& Form F_DADOS_P_TAB_INTERNA
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM f_dados_p_tab_interna .
* LIMPAR A TABELA INTERNA
  REFRESH t_tempo.
  CLEAR t_tempo.
  LOOP AT t_tabela.
* limpar o header line da tabela
    CLEAR t_tempo.
* sy-mandt -> volta o mandante do sistema.
    t_tempo-mandt = sy-mandt.
* SPLIT tabela_interna AT delimitador_do_arquivo INTO
* campos que irão receber os dados na seqüência do
* arquivo
    SPLIT t_tabela-texto01 AT c_tipo INTO: t_tempo-codigo t_tempo-nome.
* APPEND nome da tabela que ira guardar os dados importados
* gravar na tabela
* coloca zeros na frente do numero
    UNPACK t_tempo-codigo TO t_tempo-codigo.
    APPEND t_tempo.
  ENDLOOP.
ENDFORM. " F_DADOS_P_TAB_INTERNA
*&---------------------------------------------------------------------*
*& Form F_ATUALIZAR_DADOS
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM f_atualizar_dados .
  LOOP AT t_tempo.
* move o conteúdo de uma tabela para outra tabela
* apenas os campos com o mesmo nome ( variáveis )
    MOVE-CORRESPONDING t_tempo TO ztab1_01.
* INSERE O CONTEUDO DA VARIAVEL PARA A TABELA ( GRAVAR )
    INSERT ztab1_01.
* UPDATE tabela interna
* DELETE tabela interna
  ENDLOOP.
* EFETIVA AS ALTERAÇÕES NO BANCO
  COMMIT WORK.
ENDFORM. " F_ATUALIZAR_DADOS
