FUNCTION z_rfc_paralela.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_GJAHR) TYPE  GJAHR OPTIONAL
*"  TABLES
*"      T_BSEG STRUCTURE  BSEG
*"----------------------------------------------------------------------

*       Global data declarations

  SELECT *
    FROM bseg
    INTO TABLE t_bseg
    WHERE gjahr EQ i_gjahr.


ENDFUNCTION.


*Messages
*----------------------------------------------------------
*
* Message class: Hard coded
*   STOP
