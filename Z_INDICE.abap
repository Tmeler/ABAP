*&---------------------------------------------------------------------*
*& Report  Z_INDICE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT Z_INDICE.

TABLES: mara.

SELECT-OPTIONS: s_mtart FOR mara-mtart.

DATA: t_mara TYPE TABLE OF mara.

SELECT *
  FROM mara
  INTO TABLE t_mara
  WHERE mtart In s_mtart
  %_HINTS ORACLE 'INDEX("MARA" "MARA~T")'.
