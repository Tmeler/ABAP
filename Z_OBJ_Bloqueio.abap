*&---------------------------------------------------------------------*
*& Report  Z_OBJETO_BLOQUEIO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_aula_10_objeto_bloqueio.

DATA: wa_mara TYPE mara.

PARAMETERS: p_matnr TYPE mara-matnr.

SELECT SINGLE matnr
  FROM mara
  INTO wa_mara-matnr
  WHERE matnr EQ p_matnr.

*--------------------------------------------------------------------*
* Bloqueia a entrada informada na tela de seleção
*--------------------------------------------------------------------*
CALL FUNCTION 'ENQUEUE_EZAULA10'
  EXPORTING
    mode_mara      = 'E'
    mandt          = sy-mandt
    matnr          = wa_mara-matnr "Informação que está sendo bloqueada
    x_matnr        = ' '
    _scope         = '2'
    _wait          = ' '
    _collect       = ' '
  EXCEPTIONS
    foreign_lock   = 1
    system_failure = 2
    OTHERS         = 3.

IF sy-subrc <> 0.
  MESSAGE 'Material já bloqueado para utilização' TYPE 'S' DISPLAY LIKE 'E'.
ENDIF.

CALL FUNCTION 'DEQUEUE_EZAULA10'
  EXPORTING
    mode_mara = 'E'
    mandt     = sy-mandt
    matnr     = wa_mara-matnr
    x_matnr   = ' '
    _scope    = '3'
    _synchron = ' '
    _collect  = ' '.


*Messages
*----------------------------------------------------------
*
* Message class: Hard coded
*   Material já bloqueado para utilização
