*&---------------------------------------------------------------------*
*& Report  Z_MANDANTE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_mandante.

DATA: wa_usr02 TYPE usr02.

SELECT SINGLE *
  FROM usr02 CLIENT SPECIFIED "Utilização obrigatória desse comando
  INTO wa_usr02
  WHERE mandt = '000' "Informar o mandante desejado
    AND bname = 'GPUSER0331'.

IF sy-subrc EQ 0.
  BREAK-POINT.
