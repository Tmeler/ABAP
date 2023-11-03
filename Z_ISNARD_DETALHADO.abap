*&---------------------------------------------------------------------*
*& Report  Z_ISNARD_DETALHADO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
* Início do programa
*--------------------------------------------------------------------*
REPORT Z_ISNARD_DETALHADO.
*--------------------------------------------------------------------*
* Definição da estrutura que será usada no programa de forma geral
*--------------------------------------------------------------------*
TYPES: BEGIN OF ty_bsid,
         bukrs TYPE bsid-bukrs,
         kunnr TYPE bsid-kunnr,
         zuonr TYPE bsid-zuonr,
         gjahr TYPE bsid-gjahr,
         budat TYPE bsid-budat,
         dmbtr TYPE bsid-dmbtr,
       END OF ty_bsid.
*--------------------------------------------------------------------*
* Estrutura usada para a sumarização dos dados obtidos
*--------------------------------------------------------------------*
TYPES: BEGIN OF ty_bsid_s,
         budat(10) TYPE c,
         dmbtr     TYPE bsid-dmbtr,
       END OF ty_bsid_s.
*--------------------------------------------------------------------*
* Declaração de tabelas, work áreas e variáveis usadas no programa
*--------------------------------------------------------------------*
data: t_bsid      TYPE TABLE OF ty_bsid,
      t_bsid_s    TYPE TABLE OF ty_bsid_s,
      t_sum       TYPE TABLE OF ty_bsid_s WITH HEADER LINE,
*      t_final     TYPE TABLE OF ty_bsid_s,
      wa_bsid_s   TYPE ty_bsid_s,
      wa_sum      TYPE ty_bsid_s,
      v_date(10)  TYPE c.

*--------------------------------------------------------------------*
* Tela de seleção para informar os dados através do usuário
*--------------------------------------------------------------------*
PARAMETERS: p_bukrs TYPE bukrs DEFAULT 1000,
            p_kunnr TYPE kunnr DEFAULT 1032..

*--------------------------------------------------------------------*
* Seleção dos 5 campos ukrs kunnr zuonr gjahr budat dmbtr
* que serão usados no programa pelo campo de seleção da tela
*--------------------------------------------------------------------*
SELECT bukrs kunnr zuonr gjahr budat dmbtr
  FROM bsid
  INTO TABLE t_bsid
  WHERE bukrs = p_bukrs
        and kunnr = p_kunnr.

*--------------------------------------------------------------------*
* Após obter os dados na primeira tabela, mover a segunda tabela de dados
* que serão sumarizados
*--------------------------------------------------------------------*
MOVE-CORRESPONDING t_bsid to t_bsid_s.

*--------------------------------------------------------------------*
* Como os campos presentes na tabela T_BSID_S são apenas os campos necessários
* para a sumarização, o comando COLLECT fará automaticamente a soma dos valores
* que pertencem ao mesmo grupo, neste caso o grupo como informado no exercício
* será a data e inserindo na terceira tabela criada T_SUM
*--------------------------------------------------------------------*
LOOP AT t_bsid_s into wa_bsid_s.
  COLLECT wa_bsid_s INTO t_sum.
ENDLOOP.

*--------------------------------------------------------------------*
* O comando SORT organiza os campos por data
*--------------------------------------------------------------------*
SORT t_sum by budat.

*--------------------------------------------------------------------*
* Leitura e exibição dos dados
*--------------------------------------------------------------------*
LOOP AT t_sum INTO wa_sum.
*--------------------------------------------------------------------*
* Formatando o formato da data para remover o . e inserir /
*--------------------------------------------------------------------*
  CONCATENATE wa_sum-budat+6(2) wa_sum-budat+4(2) wa_sum-budat+0(4) INTO v_date SEPARATED BY '/'.
*--------------------------------------------------------------------*
* Inserindo a data convertida no campo da tabela de saída
*--------------------------------------------------------------------*
  wa_sum-budat = v_date.
*--------------------------------------------------------------------*
* Exibindo os dados do programa
*--------------------------------------------------------------------*
  WRITE wa_sum-budat.
  WRITE wa_sum-dmbtr.
  NEW-LINE.
ENDLOOP.

*--------------------------------------------------------------------*
* Fim do programa
*--------------------------------------------------------------------*
