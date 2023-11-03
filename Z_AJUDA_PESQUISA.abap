*&---------------------------------------------------------------------*
*& Report  Z_AJUDA_PESQUISA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT Z_AJUDA_PESQUISA.

PARAMETERS: p_matnr TYPE char18 MATCHCODE OBJECT Z_AJUDAPESQUISA,
            p_maktx TYPE makt-maktx.

CALL FUNCTION 'ENQUEUE_EZOBJBLOQUEIO'
 EXPORTING
   MODE_ZVIEW           = 'E'
   MATNR                = p_matnr
   MAKTX                = p_maktx
   X_MATNR              = ' '
   X_MAKTX              = ' '
   _SCOPE               = '2'
   _WAIT                = ' '
   _COLLECT             = ' '
 EXCEPTIONS
   FOREIGN_LOCK         = 1
   SYSTEM_FAILURE       = 2
   OTHERS               = 3
          .
IF sy-subrc <> 0.
* Implement suitable error handling here
  MESSAGE 'Objeto Bloqueado por Usuário APASSARELLI' TYPE 'E'.
ENDIF.


CALL FUNCTION 'DEQUEUE_EZOBJBLOQUEIO'
 EXPORTING
   MODE_ZVIEW       = 'E'
   MATNR            = p_matnr
   MAKTX            = p_maktx
   X_MATNR          = ' '
   X_MAKTX          = ' '
   _SCOPE           = '3'
   _SYNCHRON        = ' '
   _COLLECT         = ' '
          .


*Messages
*----------------------------------------------------------
*
* Message class: Hard coded
