REPORT ZMMALV_LISTA_MATERIAL.

INCLUDE ZALV_INCLUDE.

* Tabelas
TABLES: MAKT,
        MARA,
        MARD,
        MBEW.

* Tabelas internas
DATA: BEGIN OF I_TAB OCCURS 0,
        MATNR        LIKE MARA-MATNR,
        MTART        LIKE MARA-MTART,
        LVORM        LIKE MARA-LVORM,
        MAKTX        LIKE MAKT-MAKTX,
        BISMT        LIKE MARA-BISMT,
        BRGEW        LIKE MARA-BRGEW,
        GEWEI_BR     LIKE MARA-GEWEI,
        NTGEW        LIKE MARA-NTGEW,
        GEWEI_NT     LIKE MARA-GEWEI,
        PRECO        LIKE MBEW-VERPR,
        MEINS        LIKE MARA-MEINS,
        ERSDA        LIKE MARA-ERSDA,
        ZDATAE       LIKE MARA-ZDATAE,
      END OF I_TAB.

DATA: BEGIN OF I_MARD OCCURS 0,
        MATNR        LIKE MARD-MATNR,
        WERKS        LIKE MARD-WERKS,
        LGORT        LIKE MARD-LGORT,
        LGPBE        LIKE MARD-LGPBE,
      END OF I_MARD.

DATA: BEGIN OF I_MAT OCCURS 0,
        MATNR        LIKE MARD-MATNR,
        CONTADOR     TYPE I,
      END OF I_MAT.

* Variáveis
DATA: V_SEQ(02)      TYPE N,
      V_CONTA(01)    TYPE C,
      V_COLOR(04)    TYPE C,
      V_NAME(40)     TYPE C.

FIELD-SYMBOLS: <F>,
               <GT_OUTTAB> TYPE TABLE,
               <LT_OUTTAB>.

************************************************************************
* Parametros de Seleção
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BISMT FOR MARA-BISMT,
                S_MATNR FOR MARA-MATNR,
                S_MTART FOR MARA-MTART,
                S_MAKTX FOR MAKT-MAKTX.
SELECTION-SCREEN END OF BLOCK B1.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_POSI AS CHECKBOX USER-COMMAND UCOM.
SELECT-OPTIONS: S_WERKS FOR MARD-WERKS MODIF ID GP1,
                S_LGORT FOR MARD-LGORT MODIF ID GP1.
PARAMETERS: P_DIF AS CHECKBOX MODIF ID GP1.
SELECTION-SCREEN END OF BLOCK B2.
SELECTION-SCREEN BEGIN OF BLOCK B9 WITH FRAME TITLE TEXT-009.
PARAMETERS: P_VARIA LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK B9.

************************************************************************
INITIALIZATION.
************************************************************************
  PERFORM INIT_ALV USING SY-REPID
                CHANGING P_VARIA.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARIA.
  PERFORM RECUPERA_LAYOUTS_SALVOS USING SY-REPID
                               CHANGING P_VARIA.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'GP1'.

    IF P_POSI = 'X'.
      SCREEN-INPUT = '1'.
    ELSE.
      SCREEN-INPUT = '0'.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

************************************************************************
START-OF-SELECTION.
************************************************************************
  PERFORM SELECIONA_DADOS.
  PERFORM MONTA_TABELA_DINAMICA.
  PERFORM MONTA_DADOS.

************************************************************************
END-OF-SELECTION.
************************************************************************
  PERFORM ALV.


*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM SELECIONA_DADOS.
  CLEAR I_TAB.   REFRESH I_TAB.
  CLEAR I_MAT.   REFRESH I_MAT.
  CLEAR I_MARD.  REFRESH I_MARD.

  SELECT A~MATNR A~LVORM A~MTART A~BISMT A~MEINS A~BRGEW A~NTGEW A~GEWEI
         B~MAKTX A~ERSDA A~ZDATAE
    INTO (I_TAB-MATNR, I_TAB-LVORM, I_TAB-MTART, I_TAB-BISMT, I_TAB-MEINS,
          I_TAB-BRGEW, I_TAB-NTGEW, I_TAB-GEWEI_BR, I_TAB-MAKTX, I_TAB-ERSDA, I_TAB-ZDATAE)
    FROM MARA AS A INNER JOIN MAKT AS B ON B~MATNR = A~MATNR
                                       AND B~SPRAS = SY-LANGU
    WHERE A~BISMT IN S_BISMT
      AND A~MATNR IN S_MATNR
      AND A~MTART IN S_MTART
      AND B~MAKTX IN S_MAKTX.

    I_TAB-GEWEI_NT = I_TAB-GEWEI_BR.

    CLEAR MBEW.
    SELECT SINGLE VPRSV VERPR STPRS
      INTO CORRESPONDING FIELDS OF MBEW
      FROM MBEW
      WHERE MATNR EQ I_TAB-MATNR.

    CASE MBEW-VPRSV.
      WHEN 'S'.
        I_TAB-PRECO = MBEW-STPRS.
      WHEN 'V'.
        I_TAB-PRECO = MBEW-STPRS.
    ENDCASE.

    APPEND I_TAB.
  ENDSELECT.

  CHECK P_POSI = 'X' AND NOT I_TAB[] IS INITIAL.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE I_MARD
    FROM MARD
    FOR ALL ENTRIES IN I_TAB
    WHERE MATNR EQ I_TAB-MATNR
      AND WERKS IN S_WERKS
      AND LGORT IN S_LGORT.

  IF P_DIF = 'X'.
    DELETE I_MARD WHERE LGPBE = SPACE.
  ENDIF.

  LOOP AT I_MARD.
    CLEAR I_MAT.
    I_MAT-MATNR    = I_MARD-MATNR.
    I_MAT-CONTADOR = 1.
    COLLECT I_MAT.
  ENDLOOP.

ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  MONTA_TABELA_DINAMICA
*&---------------------------------------------------------------------*
FORM MONTA_TABELA_DINAMICA.
  CLEAR T_FCAT.  REFRESH T_FCAT.

  CLEAR S_FCAT.
  S_FCAT-FIELDNAME = 'MATNR'.
  S_FCAT-REF_TABLE = 'MARA'.
  S_FCAT-REF_FIELD = 'MATNR'.
  S_FCAT-KEY       = 'X'.
  APPEND S_FCAT TO T_FCAT.

  CLEAR S_FCAT.
  S_FCAT-FIELDNAME = 'MTART'.
  S_FCAT-REF_TABLE = 'MARA'.
  S_FCAT-REF_FIELD = 'MTART'.
  APPEND S_FCAT TO T_FCAT.

  CLEAR S_FCAT.
  S_FCAT-FIELDNAME = 'LVORM'.
  S_FCAT-REF_TABLE = 'MARA'.
  S_FCAT-REF_FIELD = 'LVORM'.
  APPEND S_FCAT TO T_FCAT.

  CLEAR S_FCAT.
  S_FCAT-FIELDNAME = 'MAKTX'.
  S_FCAT-REF_TABLE = 'MAKT'.
  S_FCAT-REF_FIELD = 'MAKTX'.
  S_FCAT-COLTEXT   = 'Descrição'.
  APPEND S_FCAT TO T_FCAT.

  CLEAR S_FCAT.
  S_FCAT-FIELDNAME = 'BISMT'.
  S_FCAT-REF_TABLE = 'MARA'.
  S_FCAT-REF_FIELD = 'BISMT'.
  APPEND S_FCAT TO T_FCAT.

  CLEAR S_FCAT.
  S_FCAT-FIELDNAME = 'NTGEW'.
  S_FCAT-REF_TABLE = 'MARA'.
  S_FCAT-REF_FIELD = 'NTGEW'.
  S_FCAT-COLTEXT   = 'Peso Líquido'.
  APPEND S_FCAT TO T_FCAT.

  CLEAR S_FCAT.
  S_FCAT-FIELDNAME = 'GEWEI_NT'.
  S_FCAT-REF_TABLE = 'MARA'.
  S_FCAT-REF_FIELD = 'GEWEI'.
  S_FCAT-COLTEXT   = 'UP'.
  APPEND S_FCAT TO T_FCAT.

  CLEAR S_FCAT.
  S_FCAT-FIELDNAME = 'BRGEW'.
  S_FCAT-REF_TABLE = 'MARA'.
  S_FCAT-REF_FIELD = 'BRGEW'.
  S_FCAT-COLTEXT   = 'Peso Bruto'.
  APPEND S_FCAT TO T_FCAT.

  CLEAR S_FCAT.
  S_FCAT-FIELDNAME = 'GEWEI_BR'.
  S_FCAT-REF_TABLE = 'MARA'.
  S_FCAT-REF_FIELD = 'GEWEI'.
  S_FCAT-COLTEXT   = 'UP'.
  APPEND S_FCAT TO T_FCAT.

  CLEAR S_FCAT.
  S_FCAT-FIELDNAME = 'PRECO'.
  S_FCAT-REF_TABLE = 'MBEW'.
  S_FCAT-REF_FIELD = 'VERPR'.
  S_FCAT-COLTEXT   = 'Preço'.
  APPEND S_FCAT TO T_FCAT.

  CLEAR S_FCAT.
  S_FCAT-FIELDNAME = 'MEINS'.
  S_FCAT-REF_TABLE = 'MARA'.
  S_FCAT-REF_FIELD = 'MEINS'.
  APPEND S_FCAT TO T_FCAT.

  CLEAR S_FCAT.
  S_FCAT-FIELDNAME = 'ERSDA'.
  S_FCAT-REF_TABLE = 'MARA'.
  S_FCAT-REF_FIELD = 'ERSDA'.
  APPEND S_FCAT TO T_FCAT.

  CLEAR S_FCAT.
  S_FCAT-FIELDNAME = 'ZDATAE'.
  S_FCAT-REF_TABLE = 'MARA'.
  S_FCAT-REF_FIELD = 'ZDATAE'.
  APPEND S_FCAT TO T_FCAT.

  IF P_POSI = 'X'.
    SORT I_MAT BY CONTADOR DESCENDING.

    READ TABLE I_MAT INDEX 1.

    CLEAR V_CONTA.

    DO I_MAT-CONTADOR TIMES.
      V_SEQ = SY-INDEX.

      ADD 1 TO V_CONTA.
      IF V_CONTA > 7.
        V_CONTA = 1.
      ENDIF.

      CONCATENATE 'C' V_CONTA '00' INTO V_COLOR.

      CLEAR S_FCAT.
      CONCATENATE 'WERKS_' V_SEQ INTO S_FCAT-FIELDNAME.
      S_FCAT-REF_TABLE = 'MARD'.
      S_FCAT-REF_FIELD = 'WERKS'.
      S_FCAT-EMPHASIZE = V_COLOR.
      APPEND S_FCAT TO T_FCAT.

      CLEAR S_FCAT.
      CONCATENATE 'LGORT_' V_SEQ INTO S_FCAT-FIELDNAME.
      S_FCAT-REF_TABLE = 'MARD'.
      S_FCAT-REF_FIELD = 'LGORT'.
      S_FCAT-EMPHASIZE = V_COLOR.
      APPEND S_FCAT TO T_FCAT.

      CLEAR S_FCAT.
      CONCATENATE 'LGPBE_' V_SEQ INTO S_FCAT-FIELDNAME.
      S_FCAT-REF_TABLE = 'MARD'.
      S_FCAT-REF_FIELD = 'LGPBE'.
      S_FCAT-EMPHASIZE = V_COLOR.
      APPEND S_FCAT TO T_FCAT.
    ENDDO.
  ENDIF.

  DATA: TABLE_REF TYPE REF TO DATA,
        LINE_REF  TYPE REF TO DATA.

  CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
    EXPORTING
      IT_FIELDCATALOG = T_FCAT
    IMPORTING
      EP_TABLE        = TABLE_REF.

  ASSIGN TABLE_REF->* TO <GT_OUTTAB>.

  CREATE DATA LINE_REF LIKE LINE OF <GT_OUTTAB>.
  ASSIGN LINE_REF->* TO <LT_OUTTAB>.

ENDFORM.                    " MONTA_TABELA_DINAMICA
*&---------------------------------------------------------------------*
*&      Form  MONTA_DADOS
*&---------------------------------------------------------------------*
FORM MONTA_DADOS.
  LOOP AT I_TAB.
    CLEAR <LT_OUTTAB>.
    MOVE-CORRESPONDING I_TAB TO <LT_OUTTAB>.

    IF P_POSI = 'X'.
      V_SEQ = '01'.

      LOOP AT I_MARD WHERE MATNR = I_TAB-MATNR.
        CONCATENATE '<LT_OUTTAB>-WERKS_' V_SEQ INTO V_NAME.
        ASSIGN (V_NAME) TO <F>.
        <F> = I_MARD-WERKS.

        CONCATENATE '<LT_OUTTAB>-LGORT_' V_SEQ INTO V_NAME.
        ASSIGN (V_NAME) TO <F>.
        <F> = I_MARD-LGORT.

        CONCATENATE '<LT_OUTTAB>-LGPBE_' V_SEQ INTO V_NAME.
        ASSIGN (V_NAME) TO <F>.
        <F> = I_MARD-LGPBE.

        ADD 1 TO V_SEQ.
      ENDLOOP.
    ENDIF.

    APPEND <LT_OUTTAB> TO <GT_OUTTAB>.
  ENDLOOP.

ENDFORM.                    " MONTA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ALV
*&---------------------------------------------------------------------*
FORM ALV.
  V_ALV_TABDIM = 'X'.

  PERFORM ALV_LVC TABLES <GT_OUTTAB>
                   USING ' '.

ENDFORM.                    " ALV
*&---------------------------------------------------------------------*
*&      Form  EXECUTA_FUNCAO_ALV
*&---------------------------------------------------------------------*
FORM EXECUTA_FUNCAO_ALV.
  V_EV_USER_COM = ' '.

  PERFORM F_CONFIGURA_ALV USING P_VARIA.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM = SY-REPID
      I_BACKGROUND_ID    = V_ALV_LOGO
      IS_LAYOUT_LVC      = S_LAYOUT
      IT_FIELDCAT_LVC    = T_FCAT[]
      I_SAVE             = V_I_SAVE
      IS_VARIANT         = V_VARIANTE
      IT_EVENTS          = T_EVENTS
      IT_EVENT_EXIT      = T_EVENT_EXIT
      IS_PRINT_LVC       = S_PRINT
    TABLES
      T_OUTTAB           = <GT_OUTTAB>
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.

ENDFORM.                    " EXECUTA_FUNCAO_ALV
*-----------------------------------------------------------------------
*       Form  ALV_TOP_OF_PAGE
*-----------------------------------------------------------------------
FORM ALV_TOP_OF_PAGE.
  V_ALV_VER_QTDEREG = 'X'.

  PERFORM F_ALV_TOP_OF_PAGE.

ENDFORM.                    " ALV_TOP_OF_PAGE
