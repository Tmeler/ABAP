*&---------------------------------------------------------------------*
*& Report  Z_BREAK_CONDICAO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT Z_BREAK_CONDICAO.

Data: WA_MARA TYPE MARA,
      T_MARA TYPE TABLE OF MARA.

DATA: v_i TYPE i.

do 10000 TIMES.
  ADD 1 to v_i.
  wa_mara-matnr = v_i.
  CONDENSE wa_mara-matnr NO-GAPS.
  APPEND wa_mara to t_mara.
ENDDO.

BREAK-POINT.
