*&---------------------------------------------------------------------*
*& Report  Z_DADOS_TABELA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_dados_tabela.

**TYPES: BEGIN OF ty_tabela,
**      cpf(14)      TYPE c,
**      nome(50)     TYPE c,
**      endereco(50) TYPE c,
**       END OF ty_tabela.

PARAMETERS: p_matnr TYPE ZTABELA36-matnr.

*SELECT-OPTIONS: s_matnr FOR ZTABELA36-matnr.

*DATA: wa_tabela TYPE ZTABELA33.
*
*PARAMETERS: p_cpf(14)  TYPE c,
*            p_nome(50) TYPE c,
*            p_end(50)  TYPE c.
*
*wa_tabela-cpf      = p_cpf.
*wa_tabela-nome     = p_nome.
*wa_tabela-endereco = p_end.
*
*MODIFY ZTABELA33 FROM wa_tabela.
