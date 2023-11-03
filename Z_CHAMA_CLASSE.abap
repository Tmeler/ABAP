*&---------------------------------------------------------------------*
*& Report  Z_CHAMA_CLASSE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_chama_classe.

DATA: o_convert TYPE REF TO zcl_converte_dados.

DATA: v_cpf_out TYPE char14,
      v_rg_out  TYPE char12.

      PARAMETERS: p_cpf TYPE char11,
      p_rg  TYPE char9.

      CREATE OBJECT o_convert.

      CALL METHOD o_convert->convert_cpf
      EXPORTING
        cpf_in  = p_cpf
      IMPORTING
        cpf_out = v_cpf_out.

  CALL METHOD o_convert->convert_rg
  EXPORTING
      rg_in  = p_rg
  IMPORTING
      rg_out = v_rg_out.

  WRITE: v_cpf_out,
         v_rg_out.
