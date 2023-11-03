*&---------------------------------------------------------------------*
*& Report  Z_OBJETO_BLOQUEIO_2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_aula_10_objeto_bloqueio_2.

DATA: wa_mara TYPE mara.

PARAMETERS: p_matnr TYPE mara-matnr.

SELECT SINGLE matnr
  FROM mara
  INTO wa_mara-matnr
  WHERE matnr EQ p_matnr.

IF sy-subrc EQ 0.

  CALL FUNCTION 'ENQUEUE_EZAULA10_2'
    EXPORTING
      mode_mara      = 'E'
      mandt          = sy-mandt
      matnr          = wa_mara-matnr
      x_matnr        = ' '
      _scope         = '2'
      _wait          = ' '
      _collect       = ' '
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc NE 0.
    MESSAGE e398(00) WITH text-001 sy-uname text-002 wa_mara-matnr.
  ENDIF.

ENDIF.

PERFORM f_processa_dados.

CALL FUNCTION 'DEQUEUE_EZAULA10_2'
 EXPORTING
   MODE_MARA       = 'E'
   MANDT           = SY-MANDT
   MATNR           = wa_mara-matnr
   X_MATNR         = ' '
   _SCOPE          = '3'
   _SYNCHRON       = ' '
   _COLLECT        = ' '.

*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_processa_dados .

  WAIT UP TO 30 SECONDS.

ENDFORM.                    " F_PROCESSA_DADOS

*Text elements
*----------------------------------------------------------
* 001 O usuário:
* 002 está processando o material


*Messages
*----------------------------------------------------------
*
* Message class: 00
*398   & & & &
