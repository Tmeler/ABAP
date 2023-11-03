*&---------------------------------------------------------------------*
*& Report  Z_ALIMENTA_TABELA_2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_alimenta_tabela_2.

DATA: wa_zztabela1 TYPE zztabela1.

DATA: t_zztabela1 TYPE TABLE OF zztabela1.

TABLES: zztabela1.

PARAMETERS: p_matnr TYPE zztabela1-matnr.

*IF p_matnr IS NOT INITIAL.
*
*  SELECT SINGLE *
*    FROM zztabela1
*    INTO wa_zztabela1
*    WHERE matnr EQ p_matnr.
*
*  CLEAR: wa_zztabela1-lgort.
*  MODIFY zztabela1 FROM wa_zztabela1.
*
*ENDIF.

  SELECT *
    FROM zztabela1
    INTO TABLE t_zztabela1.

  LOOP AT t_zztabela1 INTO wa_zztabela1.

    CLEAR: wa_zztabela1-lgort.
    MODIFY t_zztabela1 FROM wa_zztabela1 INDEX sy-tabix.

  ENDLOOP.

  MODIFY zztabela1 FROM TABLE t_zztabela1.
