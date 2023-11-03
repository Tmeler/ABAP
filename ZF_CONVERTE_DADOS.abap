FUNCTION ZF_CONVERTE_DADOS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_I_RG) TYPE  CHAR9
*"     REFERENCE(P_I_CPF) TYPE  CHAR11
*"  EXPORTING
*"     REFERENCE(P_E_RG) TYPE  CHAR12
*"     REFERENCE(P_E_CPF) TYPE  CHAR14
*"----------------------------------------------------------------------

*       Global data declarations

  WRITE p_i_rg  to p_e_rg  USING EDIT MASK '__.___.___-_'.
  WRITE p_i_cpf to p_e_cpf USING EDIT MASK '___.___.___-__'.


ENDFUNCTION.
