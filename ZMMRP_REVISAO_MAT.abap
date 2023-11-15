REPORT ZMMRP_REVISAO_MAT NO STANDARD PAGE HEADING LINE-SIZE 099
                                                  LINE-COUNT 065.

* ******************************************************************** *
* Revisar determinados campos no cadastro de materiais    *
* ******************************************************************** *

** Tabelas
TABLES : MAKT,
         MARC,
         MVKE,
         MARA.

** Tabelas Internas

DATA : BEGIN OF I_TAB OCCURS 0,
          MATNR    LIKE MARA-MATNR,
          MAKTX    LIKE MAKT-MAKTX,
          MTART    LIKE MARA-MTART,
          VERSG    LIKE MVKE-VERSG,
          VTWEG    LIKE MVKE-VTWEG,
          WERKS    LIKE MARC-WERKS.
DATA : END OF I_TAB.

DATA : BEGIN OF I_TAB1 OCCURS 0,
         BASEG    LIKE MVKE-VERSG,
         BASEC    LIKE MVKE-VTWEG.
         INCLUDE STRUCTURE I_TAB.
DATA : END OF I_TAB1.

** Variáveis

DATA : V_VERSG     LIKE MVKE-VERSG,
       V_VTWEG     LIKE MVKE-VTWEG,
       V_GRUPO     LIKE MVKE-VERSG,
       V_CANAL     LIKE MVKE-VTWEG,
       V_MATANTG   LIKE MARA-MATNR.

* ******************************************************************** *
* PARAMETROS DE SELECAO                                                *
* ******************************************************************** *

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS P_MATNR FOR MAKT-MATNR. " Material
SELECT-OPTIONS P_WERKS FOR MARC-WERKS. " Centro
SELECT-OPTIONS P_VTWEG FOR MVKE-VTWEG. " Canal

SELECTION-SCREEN END OF BLOCK B1.

* ******************************************************************** *

* ******************************************************************** *
*                        PROGRAMA PRINCIPAL                            *
* ******************************************************************** *

START-OF-SELECTION.

  PERFORM SELECIONA_DADOS.
  PERFORM VERIFICA_DADOS.
  PERFORM IMPRIMIR_DADOS.

* ******************************************************************** *
TOP-OF-PAGE.

 FORMAT COLOR COL_HEADING INTENSIFIED ON.
 WRITE : /(08) 'CAN BASE',
          (08) 'GRP BASE',
          (18) 'MATERIAL',
          (35) 'DESCRICAO',
          (05) 'GRUPO',
          (05) 'CANAL',
          (04) 'TIPO',
          (06) 'CENTRO'.
 FORMAT RESET.
END-OF-PAGE.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM SELECIONA_DADOS.

  SELECT MARA~MATNR MARA~MTART MVKE~MATNR MVKE~VERSG MVKE~VTWEG
         MARC~FEVOR MARC~MATNR MARC~WERKS MAKT~MAKTX MAKT~MATNR
  INTO CORRESPONDING FIELDS OF TABLE I_TAB
  FROM ( MARA
         INNER JOIN MVKE
         ON MVKE~MATNR = MARA~MATNR
         INNER JOIN MARC
         ON MARC~MATNR = MARA~MATNR
         INNER JOIN MAKT
         ON MAKT~MATNR = MARA~MATNR )
         WHERE MVKE~VTWEG IN P_VTWEG
           AND MARC~WERKS IN P_WERKS
           AND MAKT~MATNR IN P_MATNR
         ORDER BY MAKT~MATNR MVKE~VTWEG ASCENDING.



  ENDFORM.                             " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_DADOS
*&---------------------------------------------------------------------*
FORM VERIFICA_DADOS.

 LOOP AT I_TAB.

   V_VERSG = I_TAB-VERSG.      " Grupo
   V_VTWEG   = I_TAB-VTWEG.      " Canal

   AT NEW MATNR.
     V_GRUPO = V_VERSG.
     V_CANAL = V_VTWEG.
   ENDAT.

   IF I_TAB-MATNR = V_MATANTG.
      IF I_TAB-VERSG <> V_GRUPO.
         MOVE-CORRESPONDING I_TAB TO I_TAB1.
         I_TAB1-BASEG = V_GRUPO.
         I_TAB1-BASEC = V_CANAL.
         APPEND I_TAB1.
      ENDIF.
   ENDIF.

   V_MATANTG = I_TAB-MATNR.

ENDLOOP.

ENDFORM.                    " VERIFICA_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
FORM IMPRIMIR_DADOS.

 LOOP AT I_TAB1.

    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    WRITE : /(08) I_TAB1-BASEC CENTERED,
             (08) I_TAB1-BASEG CENTERED,
             (18) I_TAB1-MATNR,
             (35) I_TAB1-MAKTX,
             (05) I_TAB1-VERSG CENTERED,
             (05) I_TAB1-VTWEG CENTERED,
             (04) I_TAB1-MTART,
             (06) I_TAB1-WERKS.

 ENDLOOP.

ENDFORM.                    " IMPRIMIR_DADOS
