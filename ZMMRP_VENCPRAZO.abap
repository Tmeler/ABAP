
************************************************************************
* *  Acompanhamento do cumprimento de prazos de entrega nos processos de compras
************************************************************************

REPORT ZMMRP_VENCPRAZO NO STANDARD PAGE HEADING LINE-SIZE  177
                                                LINE-COUNT 65.
** Tabelas
TABLES: MSEG, "Segmento de documento - material
        MKPF, "Cabeçalho do documento do material
        EKKO, "Cabeçalho do documento de compra
        EKPO, "Item do documento de compras
        EKET, "Divisões do programa de remessas
        LFA1, "Mestre de fornecedores (parte geral)
        T024, "Grupos de compra
        EKBE, "Histórico para o documento de compra
        MBEW, "Avaliação do material
        KONV, "Condições (dados de operação)
        KONP, "Condições (item)
        A003, "Código de imposto
        J_1BTXIP1. "J_1BTXIP1

* Tabelas internas
*     tabela de materiais que foram selecionados
DATA: BEGIN OF T_ITENS OCCURS 0,
         MBLNR      LIKE MSEG-MBLNR,
         EBELN      LIKE EKPO-EBELN,
         EBELP      LIKE EKPO-EBELP,
         EKORG      LIKE EKKO-EKORG,
         EKGRP      LIKE EKKO-EKGRP,
         BEDAT      LIKE EKKO-BEDAT,
         EINDT      LIKE EKPO-AGDAT,
         PENDENTE   LIKE EKET-MENGE,
         MEINS      LIKE EKPO-MEINS,
         BUDAT      LIKE MKPF-BUDAT,
         XBLNR      LIKE MKPF-XBLNR,
         KNUMV      LIKE EKKO-KNUMV,
         VENCIDO(3) TYPE N,
         CONT       TYPE I,
         MATNR      LIKE MSEG-MATNR,
         WERKS      LIKE MSEG-WERKS,
         TXZ01      LIKE EKPO-TXZ01,
         NETPR      LIKE EKPO-NETPR,
         BRTWR      LIKE EKPO-BRTWR,
         BRUTO      TYPE P DECIMALS 4,
         LIFNR      LIKE MSEG-LIFNR,
         MWSKZ      LIKE EKPO-MWSKZ,
         J_1BNBM    LIKE EKPO-J_1BNBM,
         PEINH      LIKE EKPO-PEINH,
         MENGE      LIKE MSEG-MENGE,
         ERFMG      LIKE MSEG-ERFMG,
         ERFME      LIKE MSEG-ERFME,
         PRUNI(12)  TYPE P DECIMALS 4.
DATA: END OF T_ITENS.

DATA: BEGIN OF T_ITENS_EKGRP OCCURS 0,
         EKGRP      LIKE EKKO-EKGRP,
         VENCIDO(3) TYPE C,
         CONT       TYPE I,
         PERC(3)    TYPE P DECIMALS 2.
DATA: END OF T_ITENS_EKGRP.

DATA: BEGIN OF T_TOT_ITENS OCCURS 0,
         EKGRP      LIKE EKKO-EKGRP,
         CONT       TYPE I.
DATA: END OF T_TOT_ITENS.

DATA: V_TITLE(100) TYPE C,
      V_DATUM(10)  TYPE C,
      V_EKGRP_ANT  LIKE T_ITENS_EKGRP-EKGRP,
      V_BRUTO(12)  TYPE P.

*----------------------------------------------------------------------*
* Parametros de Seleção
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS P_EKORG FOR EKKO-EKORG.   "Org.Compras
  SELECT-OPTIONS P_EKGRP FOR EKKO-EKGRP.   "Grupo de Compras
  SELECT-OPTIONS P_PERI  FOR MKPF-BUDAT. "obligatory."Per
  SELECT-OPTIONS P_LIFNR FOR LFA1-LIFNR MATCHCODE OBJECT KRED.  "Fornec.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-012.
  SELECTION-SCREEN BEGIN OF LINE.
   PARAMETERS: P_DETAL AS CHECKBOX.
   SELECTION-SCREEN COMMENT 03(27) TEXT-005. "Detalhar pedidos
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

INITIALIZATION.

* ******************************************************************** *
AT USER-COMMAND.
* ******************************************************************** *
  CASE SY-UCOMM.
    WHEN 'ME23'.
      PERFORM CALL_ME23.

    WHEN 'ME9F'.
      PERFORM CALL_ME9F.

  ENDCASE.


* ******************************************************************** *
*                         PROGRAMA PRINCIPAL                           *
* ******************************************************************** *
START-OF-SELECTION.

  PERFORM SELECT_MAINDATA.
  PERFORM PRINT.

END-OF-SELECTION.

* -------------------------------------------------------------------- *
*       Form  SELECT_MAINDATA
* -------------------------------------------------------------------- *
FORM SELECT_MAINDATA.

  STATICS: C_ISENT_IPI(1) TYPE C,
           N_DIVPRECO(13) TYPE P DECIMALS 5,
           N_ICM          LIKE ZITEMCOMPR-ICM,
           N_IPI          LIKE ZITEMCOMPR-IPI,
           V_KNUMH        LIKE A003-KNUMH,
           V_KBETR        LIKE KONP-KBETR,
           V_APLICACAO    LIKE ZITEMCOMPR-APLICACAO.

 STATICS: N_TOTAL(11) TYPE P DECIMALS 4,
          N_DEN(11)   TYPE P DECIMALS 2,
          N_NETPR(11) TYPE P DECIMALS 2,
          N_NUM(11)   TYPE P DECIMALS 2.


  STATICS: V_LFBNR LIKE MSEG-LFBNR,
           V_VENCIDO(3) TYPE P.

   SELECT MSEG~MBLNR MSEG~MATNR MSEG~WERKS MSEG~LIFNR
          MSEG~MENGE MSEG~ERFMG MSEG~ERFME MSEG~EBELN
          MSEG~EBELP EKKO~EKORG EKKO~EKGRP EKKO~BEDAT
          EKKO~KNUMV MKPF~BUDAT MKPF~XBLNR
      INTO (T_ITENS-MBLNR, T_ITENS-MATNR, T_ITENS-WERKS, T_ITENS-LIFNR,
            T_ITENS-MENGE, T_ITENS-ERFMG, T_ITENS-ERFME, T_ITENS-EBELN,
            T_ITENS-EBELP, T_ITENS-EKORG, T_ITENS-EKGRP, T_ITENS-BEDAT, T_ITENS-KNUMV,
            T_ITENS-BUDAT, T_ITENS-XBLNR)
      FROM ( MSEG INNER JOIN MKPF
                        ON (    MKPF~MBLNR = MSEG~MBLNR
                            AND MKPF~MJAHR = MSEG~MJAHR ) )
      INNER JOIN EKKO
            ON ( EKKO~EBELN = MSEG~EBELN )
      WHERE   MKPF~BUDAT IN P_PERI
        AND ( MSEG~LIFNR IN P_LIFNR AND MSEG~LIFNR IS NOT NULL )
        AND ( MSEG~BWART  =  '101' OR MSEG~BWART  =  '103' )
        AND EKKO~EKORG  IN P_EKORG
        AND EKKO~EKGRP  IN P_EKGRP
        AND ( EKKO~BSART  = 'NB' OR EKKO~BSART = 'ZNB' ).

      IF T_ITENS-MENGE IS INITIAL.
         T_ITENS-MENGE = T_ITENS-ERFMG.
         T_ITENS-MEINS = T_ITENS-ERFME.
      ENDIF.

      SELECT SMBLN INTO V_LFBNR UP TO 1 ROWS
         FROM MSEG
         WHERE EBELN = T_ITENS-EBELN
           AND EBELP = T_ITENS-EBELP
           AND ( BWART = '102' OR BWART = '104' )
           AND SMBLN = T_ITENS-MBLNR.
      ENDSELECT.
      IF SY-SUBRC <> 0.
         APPEND T_ITENS.
      ENDIF.
   ENDSELECT.


   LOOP AT T_ITENS.
      SELECT TXZ01 MENGE MEINS NETPR BRTWR PEINH AGDAT MWSKZ J_1BNBM
        INTO (T_ITENS-TXZ01, T_ITENS-PENDENTE, T_ITENS-MEINS, T_ITENS-NETPR,
              T_ITENS-BRTWR,
              T_ITENS-PEINH, T_ITENS-EINDT, T_ITENS-MWSKZ, T_ITENS-J_1BNBM)
        FROM EKPO
        WHERE EBELN = T_ITENS-EBELN
          AND EBELP = T_ITENS-EBELP
          AND NOT AGDAT IS NULL.
      ENDSELECT.

      IF T_ITENS-EINDT IS INITIAL.
         SELECT EINDT MENGE INTO (T_ITENS-EINDT, T_ITENS-PENDENTE) UP TO 1 ROWS
            FROM EKET
            WHERE EBELN = T_ITENS-EBELN
              AND EBELP = T_ITENS-EBELP
              AND MENGE > 0.
         ENDSELECT.
      ENDIF.

      V_VENCIDO = T_ITENS-BUDAT - T_ITENS-EINDT.
      IF V_VENCIDO <= 0.
         T_ITENS-VENCIDO = 0.
      ELSE.
         T_ITENS-VENCIDO = V_VENCIDO.
      ENDIF.

      T_ITENS-CONT = 1.

*     INÍCIO: CÁLCULO PARA PREÇO UNITÁRIO + IPI (NÃO UTILIZADO NO MOMENTO)

      SELECT * UP TO 1 ROWS
            FROM MBEW
            WHERE MATNR = T_ITENS-MATNR
              AND BWKEY = T_ITENS-WERKS.
      ENDSELECT.

      IF SY-SUBRC = 0.
         CASE MBEW-MTUSE.
           WHEN 0.
              V_APLICACAO = 'REMESSA'.
            WHEN 1.
              V_APLICACAO = 'INDUSTRIALIZAÇÃO'.
            WHEN 2.
              V_APLICACAO = 'CONSUMO'.
            WHEN 3.
              V_APLICACAO = 'IMOBILIZADO'.
         ENDCASE.
      ENDIF.

      SELECT *
              FROM KONV
              WHERE KNUMV = T_ITENS-KNUMV
                AND KPOSN = T_ITENS-EBELP
                AND KSCHL = 'ZICM'.
      ENDSELECT.

      IF SY-SUBRC = 0.
        N_ICM = KONV-KBETR / 10.
        N_ICM = ( N_ICM * -1 ) / 100.
      ENDIF.

      SELECT *
              FROM KONV
              WHERE KNUMV = T_ITENS-KNUMV
                AND KPOSN = T_ITENS-EBELP
                AND KSCHL = 'ZIPI'.
      ENDSELECT.

      IF SY-SUBRC = 0.
        IF SY-SUBRC = 0.
          N_IPI = ( KONV-KBETR / 1000 ).
        ENDIF.
      ENDIF.

      C_ISENT_IPI = 'N'.

      IF N_IPI = 0.
         IF V_APLICACAO = 'INDUSTRIALIZAÇÃO'.

            SELECT SINGLE KNUMH INTO V_KNUMH
                FROM A003
                WHERE KAPPL = 'TX'
                  AND KSCHL = 'IPI1'
                  AND ALAND = 'BR'
                  AND MWSKZ = T_ITENS-MWSKZ.

            IF SY-SUBRC = 0.
               SELECT SINGLE KBETR INTO V_KBETR
                      FROM KONP
                     WHERE KNUMH = V_KNUMH.

               IF SY-SUBRC = 0.
                  IF V_KBETR IS INITIAL.
                    C_ISENT_IPI = 'S'.
                  ENDIF.
               ENDIF.
            ELSE.
               C_ISENT_IPI = 'S'.
            ENDIF.
         ENDIF.

         IF V_APLICACAO = 'CONSUMO'.
            SELECT SINGLE KNUMH INTO V_KNUMH
                   FROM A003
                   WHERE KAPPL = 'TX'
                     AND KSCHL = 'IPI2'
                     AND ALAND = 'BR'
                     AND MWSKZ = T_ITENS-MWSKZ.

            IF SY-SUBRC = 0.
               SELECT SINGLE KBETR INTO V_KBETR
                       FROM KONP
                      WHERE KNUMH = V_KNUMH.

               IF SY-SUBRC = 0.
                  IF V_KBETR IS INITIAL.
                    C_ISENT_IPI = 'S'.
                  ENDIF.
               ENDIF.
            ELSE.
               C_ISENT_IPI = 'S'.
            ENDIF.
         ENDIF.

         IF C_ISENT_IPI = 'N'.
            SELECT SINGLE RATE INTO J_1BTXIP1-RATE
                   FROM J_1BTXIP1
                   WHERE NBMCODE = T_ITENS-J_1BNBM.

            IF SY-SUBRC = 0.
               N_IPI = ( J_1BTXIP1-RATE / 100 ).    "konv-kbetr / 10.
            ENDIF.
         ENDIF.
      ENDIF.

      N_DIVPRECO = ( T_ITENS-NETPR / T_ITENS-PEINH ) * 1000.

      N_NUM = ( (    ( N_DIVPRECO / 1000 ) + ( ( N_DIVPRECO / 1000 ) * N_IPI )    ) ).
      N_DEN =  ( 1 - N_ICM  - ( N_IPI * N_ICM  ) ).

      N_TOTAL = ( (    ( N_DIVPRECO / 1000 )
                    + ( ( N_DIVPRECO / 1000 ) * N_IPI )    ) )
                * ( 1 / ( 1 - N_ICM  - ( N_IPI * N_ICM  ) )   ) .
      T_ITENS-PRUNI  = ( N_TOTAL ).

*     FIM: CÁLCULO PARA PREÇO UNITÁRIO + IPI

      T_ITENS-BRUTO  = T_ITENS-BRTWR / T_ITENS-PENDENTE.
      T_ITENS-BRUTO  = T_ITENS-BRUTO * T_ITENS-MENGE.
      MODIFY T_ITENS.
   ENDLOOP.

   SORT T_ITENS BY EKGRP.
   LOOP AT T_ITENS.
     IF T_ITENS-VENCIDO = 0.
        T_ITENS-VENCIDO = 0.
     ELSEIF T_ITENS-VENCIDO  <= 1.
        T_ITENS-VENCIDO = 1.
     ELSEIF T_ITENS-VENCIDO  <= 4.
        T_ITENS-VENCIDO = 2.
     ELSEIF T_ITENS-VENCIDO  <= 9.
        T_ITENS-VENCIDO = 5.
     ELSEIF T_ITENS-VENCIDO  <= 14.
        T_ITENS-VENCIDO = 10.
     ELSEIF T_ITENS-VENCIDO  <= 29.
        T_ITENS-VENCIDO = 15.
     ELSEIF T_ITENS-VENCIDO  >= 30.
        T_ITENS-VENCIDO = 30.
     ENDIF.

     MOVE-CORRESPONDING T_ITENS TO T_ITENS_EKGRP.
     COLLECT T_ITENS_EKGRP.
     MOVE-CORRESPONDING T_ITENS TO T_TOT_ITENS.
     COLLECT T_TOT_ITENS.
   ENDLOOP.
   SORT T_TOT_ITENS   BY EKGRP.
   SORT T_ITENS_EKGRP BY EKGRP VENCIDO.

ENDFORM.                               " SELECT_MAINDATA

* -------------------------------------------------------------------- *
*       Form  PRINT
* -------------------------------------------------------------------- *
FORM PRINT.

  STATICS: WF_FLAG      TYPE I,
           V_LINES      LIKE SY-DBCNT,
           V_EKNAM      LIKE T024-EKNAM,
           V_TXZ01(60)  TYPE C,
           V_SORTL(12)  TYPE C,
           V_DATA(08)   TYPE C,
           V_DOCDAT     LIKE J_1BNFDOC-DOCDAT,
           V_BELNR      LIKE EKBE-BELNR,
           V_TRANSP(3)  TYPE P,
           N_COL        TYPE I VALUE 110.

* Imprime as linhas de detalhe
  DESCRIBE TABLE T_ITENS_EKGRP LINES V_LINES.
  IF V_LINES <= 0.
     FORMAT COLOR COL_NORMAL INTENSIFIED ON.
     WRITE: (196) TEXT-003.
  ENDIF.

  V_EKGRP_ANT = ''.
  LOOP AT T_TOT_ITENS.
    WF_FLAG = 1 - WF_FLAG.
    IF WF_FLAG = 0.
       FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    ELSE.
       FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ENDIF.

    SELECT EKNAM INTO V_EKNAM
       FROM T024
       WHERE EKGRP = T_TOT_ITENS-EKGRP.
    ENDSELECT.

    WRITE: /02 T_TOT_ITENS-EKGRP,
            06 V_EKNAM.

    LOOP AT T_ITENS_EKGRP WHERE EKGRP = T_TOT_ITENS-EKGRP.
       IF T_ITENS_EKGRP-VENCIDO = '000'.
          WRITE 24 T_ITENS_EKGRP-CONT.
       ELSEIF T_ITENS_EKGRP-VENCIDO = '001'.
          WRITE 35 T_ITENS_EKGRP-CONT.
       ELSEIF T_ITENS_EKGRP-VENCIDO = '002'.
          WRITE 46 T_ITENS_EKGRP-CONT.
       ELSEIF T_ITENS_EKGRP-VENCIDO = '005'.
          WRITE 57 T_ITENS_EKGRP-CONT.
       ELSEIF T_ITENS_EKGRP-VENCIDO = '010'.
          WRITE 68 T_ITENS_EKGRP-CONT.
       ELSEIF T_ITENS_EKGRP-VENCIDO = '015'.
          WRITE 79 T_ITENS_EKGRP-CONT.
       ELSEIF T_ITENS_EKGRP-VENCIDO = '030'.
          WRITE 90 T_ITENS_EKGRP-CONT.
       ENDIF.
       T_ITENS_EKGRP-PERC = ( T_ITENS_EKGRP-CONT / T_TOT_ITENS-CONT )
                            * 100.
       MODIFY T_ITENS_EKGRP.
       V_EKGRP_ANT = T_ITENS_EKGRP-EKGRP.
    ENDLOOP.
    WRITE: 101 T_TOT_ITENS-CONT,
           177 ' '.
    WRITE: /01 ' '.

    LOOP AT T_ITENS_EKGRP WHERE EKGRP = T_TOT_ITENS-EKGRP.
       IF T_ITENS_EKGRP-VENCIDO = '000'.
          WRITE: 26 T_ITENS_EKGRP-PERC, 33 '%'.
       ELSEIF T_ITENS_EKGRP-VENCIDO = '001'.
          WRITE: 37 T_ITENS_EKGRP-PERC, 44 '%'.
       ELSEIF T_ITENS_EKGRP-VENCIDO = '002'.
          WRITE: 48 T_ITENS_EKGRP-PERC, 55 '%'.
       ELSEIF T_ITENS_EKGRP-VENCIDO = '005'.
          WRITE: 59 T_ITENS_EKGRP-PERC, 66 '%'.
       ELSEIF T_ITENS_EKGRP-VENCIDO = '010'.
          WRITE: 70 T_ITENS_EKGRP-PERC, 77 '%'.
       ELSEIF T_ITENS_EKGRP-VENCIDO = '015'.
          WRITE: 81 T_ITENS_EKGRP-PERC, 88 '%'.
       ELSEIF T_ITENS_EKGRP-VENCIDO = '030'.
          WRITE: 92 T_ITENS_EKGRP-PERC, 99 '%'.
       ENDIF.
    ENDLOOP.
    WRITE: 106 '100 % ',
           177 ' '.
    SORT T_ITENS BY VENCIDO EBELN EBELP.
    WRITE: / SY-ULINE(177).
    IF P_DETAL = 'X'.
       FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

       WRITE: /01 'D+ ' COLOR COL_NORMAL INTENSIFIED ON,
               05 'Pedido     Item'.

       WRITE:  21 ' Fornecedor  ' COLOR COL_NORMAL INTENSIFIED ON,
               35 'Material'.

       WRITE:  95 ' Qtde Receb ' COLOR COL_NORMAL INTENSIFIED ON.

       FORMAT COLOR COL_NORMAL INTENSIFIED ON.
       WRITE: 107 'UM'          COLOR COL_NORMAL INTENSIFIED ON,
              112 'Preço Bruto ' COLOR COL_NORMAL INTENSIFIED ON.

       FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
       WRITE: 125 'Dt.Prazo '   COLOR COL_NORMAL INTENSIFIED OFF,
              134 'Dt.Entreg'   COLOR COL_NORMAL INTENSIFIED OFF,
              144 'Dt.Pedido '  COLOR COL_NORMAL INTENSIFIED OFF.

       FORMAT COLOR COL_NORMAL INTENSIFIED ON.
       WRITE: 155 'Nt.Fisc'     COLOR COL_NORMAL INTENSIFIED ON,
              163 'Dt.NFisc'    COLOR COL_NORMAL INTENSIFIED ON,
              172 'Transp'      COLOR COL_NORMAL INTENSIFIED ON,
              177 ' '.
       FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

       LOOP AT T_ITENS WHERE EKGRP = T_TOT_ITENS-EKGRP.
          V_TXZ01 = T_ITENS-TXZ01.
          WRITE T_ITENS-MATNR TO T_ITENS-MATNR NO-ZERO.
          IF NOT T_ITENS-MATNR IS INITIAL.
            CONCATENATE T_ITENS-MATNR '-' T_ITENS-TXZ01 INTO V_TXZ01.
                                            "separated by space.
          ENDIF.
          CLEAR V_SORTL.
          SELECT SORTL INTO V_SORTL
             FROM LFA1
             WHERE LIFNR = T_ITENS-LIFNR.
          ENDSELECT.
          CONCATENATE '(' V_SORTL ')' INTO V_SORTL.
          CLEAR V_BELNR.
          SELECT MAX( BELNR ) INTO V_BELNR
            FROM EKBE
            WHERE EBELN = T_ITENS-EBELN
              AND EBELP = T_ITENS-EBELP
              AND BEWTP = 'Q'
              AND LFBNR = T_ITENS-MBLNR
              AND SHKZG = 'S'.
*         endselect.

          CLEAR V_DOCDAT.
          IF NOT V_BELNR IS INITIAL.
             SELECT DOCDAT INTO V_DOCDAT UP TO 1 ROWS
                    FROM J_1BNFDOC
                    WHERE BELNR EQ V_BELNR
                      AND PSTDAT >= T_ITENS-BUDAT        
                      AND CANCEL <> 'X'.
             ENDSELECT.
          ENDIF.
          IF T_ITENS-VENCIDO >= 6.
             WRITE: /01 T_ITENS-VENCIDO
                                  COLOR COL_NEGATIVE INTENSIFIED OFF.
          ELSE.
             WRITE: /01 T_ITENS-VENCIDO.
          ENDIF.


          WRITE: 05 T_ITENS-EBELN,
                 15 '-',
                 16 T_ITENS-EBELP,
                 22 V_SORTL,
                 35 V_TXZ01.
          WRITE: 90 T_ITENS-MENGE RIGHT-JUSTIFIED.
          WRITE: 107 T_ITENS-MEINS.

          WRITE AT N_COL(14) T_ITENS-BRUTO RIGHT-JUSTIFIED.

          CONCATENATE T_ITENS-EINDT+6(02) '.'
                      T_ITENS-EINDT+4(02) '.'
                      T_ITENS-EINDT+2(02) INTO V_DATA.
          WRITE: 125 V_DATA.
          CONCATENATE T_ITENS-BUDAT+6(02) '.'
                      T_ITENS-BUDAT+4(02) '.'
                      T_ITENS-BUDAT+2(02) INTO V_DATA.
          WRITE: 135 V_DATA.
          CONCATENATE T_ITENS-BEDAT+6(02) '.'
                      T_ITENS-BEDAT+4(02) '.'
                      T_ITENS-BEDAT+2(02) INTO V_DATA.
          WRITE: 145 V_DATA.


          WRITE: 154 T_ITENS-XBLNR(08) RIGHT-JUSTIFIED.
          IF NOT V_DOCDAT IS INITIAL.
             CONCATENATE V_DOCDAT+6(02) '.'
                         V_DOCDAT+4(02) '.'
                         V_DOCDAT+2(02) INTO V_DATA.


             WRITE: 163 V_DATA.
             V_TRANSP = T_ITENS-BUDAT - V_DOCDAT.
             IF V_TRANSP > 3.
                WRITE: 172 V_TRANSP COLOR COL_NEGATIVE INTENSIFIED OFF.
             ELSE.
                WRITE: 172 V_TRANSP.
             ENDIF.
          ENDIF.
          WRITE: 177 ' '.
          HIDE T_ITENS-EBELN.
       ENDLOOP.
       WRITE: / SY-ULINE(177).
    ENDIF.
  ENDLOOP.

ENDFORM.                               " PRINT

*-------------------------------------------------------------------- *
*       Form  CALL_ME23
*-------------------------------------------------------------------- *
FORM CALL_ME23.
  IF NOT T_ITENS-EBELN IS INITIAL.
    SET PARAMETER ID 'BES'  FIELD T_ITENS-EBELN.
    CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
  ENDIF.
ENDFORM.                               " CALL_ME23

*-------------------------------------------------------------------- *
*       Form  CALL_ME23
*-------------------------------------------------------------------- *
FORM CALL_ME9F.
  IF NOT T_ITENS-EBELN IS INITIAL.
    SET PARAMETER ID 'BES'  FIELD T_ITENS-EBELN.
    CALL TRANSACTION 'ME9F'.
  ENDIF.
ENDFORM.                               " CALL_ME23


* -------------------------------------------------------------------- *
*  TOP-OF-PAGE.
* -------------------------------------------------------------------- *

 TOP-OF-PAGE.

 DATA: V_PERILOW(10)  TYPE C,
       V_PERIHIGH(10) TYPE C,
       V_PERIODO(40)  TYPE C.

  SET PF-STATUS 'ME23'.

  WRITE: /(196)  SY-PAGNO RIGHT-JUSTIFIED.
  FORMAT COLOR COL_KEY INTENSIFIED ON.
  WRITE: / SY-ULINE(177).
  WRITE SY-DATUM TO V_DATUM.
  WRITE P_PERI-LOW TO V_PERILOW.
  WRITE P_PERI-HIGH TO V_PERIHIGH.
  IF P_PERI-HIGH IS INITIAL.
     V_PERIODO = V_PERILOW.
  ELSE.
     CONCATENATE 'Período ' V_PERILOW 'até' V_PERIHIGH
                 INTO V_PERIODO SEPARATED BY SPACE.

  ENDIF.
  CONCATENATE TEXT-002 '- ' V_PERIODO '- Emitido em' V_DATUM
              INTO V_TITLE SEPARATED BY SPACE.

  WRITE: /(196)  V_TITLE CENTERED.
* Imprime os títulos
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE: /(22) 'Comprador'       CENTERED
                              COLOR COL_HEADING INTENSIFIED ON,
         (154)  'Resumo dos itens recebidos x prazo de entrega' CENTERED
                              COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: /(22) '(Grp. Compras)'       CENTERED
                              COLOR COL_HEADING INTENSIFIED ON.

  WRITE:  (11)  'no prazo(D)'
                              COLOR COL_HEADING INTENSIFIED OFF,
          (09)  '   = D+01'   CENTERED
                              COLOR COL_HEADING INTENSIFIED ON,
          (11)  '  >= D+02'   CENTERED
                              COLOR COL_HEADING INTENSIFIED OFF,
          (09)  '  >= D+05'   CENTERED
                              COLOR COL_HEADING INTENSIFIED ON,
          (11)  '  >= D+10'   CENTERED
                              COLOR COL_HEADING INTENSIFIED OFF,
          (09)  '  >= D+15'   CENTERED
                              COLOR COL_HEADING INTENSIFIED ON,
          (11)  '  >= D+30'   CENTERED
                              COLOR COL_HEADING INTENSIFIED OFF,
          (10)  '    Total'   CENTERED
                              COLOR COL_HEADING INTENSIFIED ON,
          (65) ' '.

  WRITE: / SY-ULINE(177).
