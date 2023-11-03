*&---------------------------------------------------------------------*
*& Report  Z_ALIMENTA_TABELA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_alimenta_tabela.

**DATA: v_matnr TYPE mara-matnr.

DATA: wa_zztabela1 TYPE zztabela1.
DATA: v_i          TYPE i.

DATA: t_zztabela1 TYPE TABLE OF zztabela1.

PARAMETERS: p_matnr TYPE mara-matnr. "Variável

DO 10 TIMES.
  ADD 1 TO v_i.
  p_matnr = p_matnr + v_i.
  wa_zztabela1-matnr = p_matnr.
  wa_zztabela1-werks = '100'.
  APPEND wa_zztabela1 TO t_zztabela1.
ENDDO.

*LOOP AT t_zztabela1 INTo wa_ZZTABELA1.
*
*  wa_ZZTABELA1-lgort = '200'.
*
*  MODIFY zztabela1 FROM wa_zztabela1.
*ENDLOOP.

LOOP AT t_zztabela1 INTo wa_ZZTABELA1.
  wa_ZZTABELA1-lgort = '200'.
  MODIFY t_zztabela1 FROM wa_ZZTABELA1 INDEX sy-tabix.
ENDLOOP.

  MODIFY zztabela1 FROM TABLE t_zztabela1.

COMMIT WORK.

BREAK-POINT.


*Selection texts
*----------------------------------------------------------
* P_MATNR         Material
