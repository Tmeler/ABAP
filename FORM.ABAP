FUNCTION zfm_gecex_cria_produto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_MATNR) TYPE  MATNR
*"  EXPORTING
*"     REFERENCE(P_RETURN) TYPE  STRING
*"----------------------------------------------------------------------



  DATA: lr_proxy      TYPE REF TO zws_gecexco_wsr_produto_soap,
        input         TYPE zws_gecexexp777soap_in,
        output        TYPE zws_gecexexp777soap_out,
        exp_produto   TYPE zws_gecexarray_of_str_exp_pr10.
       
        

  DATA: wa_mara        TYPE mara,
        vl_matnr       TYPE char18,
        vl_steuc       TYPE marc-steuc,
        vl_maktx       TYPE makt-maktx,
        text_steuc     TYPE char25,
        vl_qtd_cx      TYPE EKPO-MENGE,
        vl_qtd_cx_str  TYPE string,
        vl_qtd_ms1     TYPE EKPO-MENGE,
        vl_qtd_ms1_str TYPE string,
        vl_lvorm       TYPE mvke-lvorm,
        vl_ausme       TYPE marc-ausme.  "MELER - tr 220364- Trazer info de acondicionamento emb. padrão 02/09/24

  DATA: convert_data TYPE string,
        convert_hora TYPE string.

  DATA: str_exp_produto TYPE zws_gecexstr_exp_produto.




  " Buscar material
  SELECT SINGLE *
    INTO wa_mara
    FROM mara
    WHERE matnr EQ p_matnr AND mtart EQ 'FERT'.


  " Buscar NCM
  SELECT SINGLE steuc
    INTO vl_steuc
    FROM marc
    WHERE matnr EQ p_matnr.

  " Buscar Descrição!
  SELECT SINGLE maktx
    INTO vl_maktx
    FROM makt
    WHERE matnr EQ p_matnr.

  " Buscar medidas
  SELECT SINGLE  lvorm
    INTO (vl_lvorm)
    FROM mvke
*    WHERE matnr EQ p_matnr AND vtweg EQ '60'. "MELER - tr 220364- 
    WHERE matnr EQ p_matnr
    AND vtweg IN ('60', '50').
	
	" Buscar Unidade de saída! "MELER - tr 220364- 
  SELECT SINGLE ausme
    INTO vl_ausme
    FROM marc
    WHERE matnr EQ p_matnr.


  IF wa_mara IS NOT INITIAL AND  vl_steuc IS NOT INITIAL AND
    vl_maktx IS NOT INITIAL.



    CONCATENATE sy-datum+6(2) '/' sy-datum+4(2) '/' sy-datum(4) INTO convert_data.
    CONCATENATE sy-uzeit(2) ':' sy-uzeit+2(2) ':' sy-uzeit+4(2) INTO convert_hora.


    input-log_informacoes-sistema_origem    = 'SAP'.
    input-log_informacoes-sistema_destino   = 'GECEX'.
    input-log_informacoes-versao_integracao = '1.0'.
    input-log_informacoes-data_mensagem     = convert_data.
    input-log_informacoes-hora_mensagem     = convert_hora.
    input-log_informacoes-usuario_mensagem  = 'SAP'.



    "********************************************************
    " Processar Dados!
    "********************************************************

    " Retirar pontos do NCM
    text_steuc = vl_steuc.
    REPLACE ALL OCCURRENCES OF '.' IN text_steuc WITH ''.

    str_exp_produto-cod_empresa   = '99'.
    str_exp_produto-cod_filial    = '1'.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        INPUT  = wa_mara-matnr
      IMPORTING
        OUTPUT = vl_matnr.

    str_exp_produto-cod_produto   = vl_matnr.
    str_exp_produto-cod_unid_med  = wa_mara-meins.
    str_exp_produto-pes_unit     = wa_mara-ntgew.
    str_exp_produto-cod_nbm      = text_steuc.
    str_exp_produto-cod_ncm      = text_steuc.
    str_exp_produto-cod_naladi   = text_steuc.
    IF wa_mara-LVORM EQ 'X'.
      str_exp_produto-cod_situacao = 'C'.
    ELSEIF vl_lvorm EQ 'X'.
      str_exp_produto-cod_situacao = 'I'.
    ELSE.
      str_exp_produto-cod_situacao = 'A'.
    ENDIF.
    str_exp_produto-cod_produto_pad  = vl_matnr.
    str_exp_produto-cod_produto_ext   = vl_matnr.


    DATA: str_exp_produto_acond           TYPE zws_gecexstr_exp_produto_a_tab.
    DATA: zws_gecexstr_exp_produto_acond  TYPE zws_gecexstr_exp_produto_acond.
    DATA: exp_produto_acond               TYPE zws_gecexarray_of_str_exp_pro4.

    IF vl_ausme EQ 'KI'.    
    zws_gecexstr_exp_produto_acond-cod_acondic = '35'.
    zws_gecexstr_exp_produto_acond-cod_embalagem = '002'.
    zws_gecexstr_exp_produto_acond-cod_tipo_embal = '1'.
    CLEAR: vl_qtd_cx, vl_qtd_ms1, vl_qtd_cx_str, vl_qtd_ms1_str.
    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        I_MATNR              = wa_mara-matnr
        I_IN_ME              = 'MS1'
        I_OUT_ME             = wa_mara-meins
        I_MENGE              = 1
      IMPORTING
        E_MENGE              = vl_qtd_ms1
      EXCEPTIONS
        ERROR_IN_APPLICATION = 1
        ERROR                = 2
        OTHERS               = 3.
    vl_qtd_ms1_str = vl_qtd_ms1.
    CONDENSE vl_qtd_ms1_str.
    REPLACE ALL OCCURRENCES OF '.' IN vl_qtd_ms1_str WITH ','.
    zws_gecexstr_exp_produto_acond-qtd_padrao = vl_qtd_ms1_str.
    zws_gecexstr_exp_produto_acond-pes_embalagem  = '0,22700'.
    APPEND zws_gecexstr_exp_produto_acond TO str_exp_produto_acond.

    CLEAR: zws_gecexstr_exp_produto_acond.
    zws_gecexstr_exp_produto_acond-cod_acondic = '4'.
    zws_gecexstr_exp_produto_acond-cod_embalagem = '002'.
    zws_gecexstr_exp_produto_acond-cod_tipo_embal = '1'.
    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        I_MATNR              = wa_mara-matnr
        I_IN_ME              = 'KI'
        I_OUT_ME             = wa_mara-meins
        I_MENGE              = 1
      IMPORTING
        E_MENGE              = vl_qtd_cx
      EXCEPTIONS
        ERROR_IN_APPLICATION = 1
        ERROR                = 2
        OTHERS               = 3.
    vl_qtd_cx_str = vl_qtd_cx.
    CONDENSE vl_qtd_cx_str.
    REPLACE ALL OCCURRENCES OF '.' IN vl_qtd_cx_str WITH ','.
    zws_gecexstr_exp_produto_acond-qtd_padrao = vl_qtd_cx_str.
    
   ELSE. vl_ausme EQ 'PAK'. 
    zws_gecexstr_exp_produto_acond-cod_acondic = '35'.
    zws_gecexstr_exp_produto_acond-cod_embalagem = '002'.
    zws_gecexstr_exp_produto_acond-cod_tipo_embal = '1'.
    CLEAR: vl_qtd_cx, vl_qtd_ms1, vl_qtd_cx_str, vl_qtd_ms1_str.
    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        I_MATNR              = wa_mara-matnr
        I_IN_ME              = 'MS1'
        I_OUT_ME             = wa_mara-meins
        I_MENGE              = 1
      IMPORTING
        E_MENGE              = vl_qtd_ms1
      EXCEPTIONS
        ERROR_IN_APPLICATION = 1
        ERROR                = 2
        OTHERS               = 3.
    vl_qtd_ms1_str = vl_qtd_ms1.
    CONDENSE vl_qtd_ms1_str.
    REPLACE ALL OCCURRENCES OF '.' IN vl_qtd_ms1_str WITH ','.
    zws_gecexstr_exp_produto_acond-qtd_padrao = vl_qtd_ms1_str.
    zws_gecexstr_exp_produto_acond-pes_embalagem  = '0,22700'.
    APPEND zws_gecexstr_exp_produto_acond TO str_exp_produto_acond.

    CLEAR: zws_gecexstr_exp_produto_acond.
    zws_gecexstr_exp_produto_acond-cod_acondic = '4'.
    zws_gecexstr_exp_produto_acond-cod_embalagem = '002'.
    zws_gecexstr_exp_produto_acond-cod_tipo_embal = '1'.
    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        I_MATNR              = wa_mara-matnr
        I_IN_ME              = 'PAK'
        I_OUT_ME             = wa_mara-meins
        I_MENGE              = 1
      IMPORTING
        E_MENGE              = vl_qtd_cx
      EXCEPTIONS
        ERROR_IN_APPLICATION = 1
        ERROR                = 2
        OTHERS               = 3.
    vl_qtd_cx_str = vl_qtd_cx.
    CONDENSE vl_qtd_cx_str.
    REPLACE ALL OCCURRENCES OF '.' IN vl_qtd_cx_str WITH ','.
    zws_gecexstr_exp_produto_acond-qtd_padrao = vl_qtd_cx_str.
	ENDIF.
	
    APPEND zws_gecexstr_exp_produto_acond TO str_exp_produto_acond.
    

    exp_produto_acond-str_exp_produto_acond = str_exp_produto_acond.


    "******************************************************************
    "Descrição do material
    "********************************************************************
    DATA: zws_gecexstr_exp_produto_descr TYPE zws_gecexstr_exp_produto_descr.
    DATA: str_exp_produto_descri TYPE zws_gecexstr_exp_produto_d_tab.
    DATA: exp_produto_descri TYPE zws_gecexarray_of_str_exp_pro9.

    zws_gecexstr_exp_produto_descr-cod_idioma  = '5'.
    zws_gecexstr_exp_produto_descr-des_produto = vl_maktx.
    zws_gecexstr_exp_produto_descr-utilizacao = 'S'.


    APPEND zws_gecexstr_exp_produto_descr TO str_exp_produto_descri.

    exp_produto_descri-str_exp_produto_descri = str_exp_produto_descri.



    str_exp_produto-exp_produto_acond = exp_produto_acond.
    str_exp_produto-exp_produto_descri = exp_produto_descri.

    APPEND str_exp_produto TO exp_produto-str_exp_produto.
    input-exp_produto = exp_produto.



    "*******************************************************************
    " Enviar os dados!
    "*******************************************************************

    TRY.
        CREATE OBJECT lr_proxy
          EXPORTING
            logical_port_name = 'ZLP_GECEX_PRODUTO'.


        CALL METHOD lr_proxy->exp777
          EXPORTING
            input  = input
          IMPORTING
            output = output.

      CATCH cx_ai_system_fault.
      CATCH cx_ai_application_fault .
    ENDTRY.

    "WRITE: output-exp777result-descricao_retorno.




    p_return = output-exp777result-descricao_retorno.


  ELSE.

    p_return = 'Houve algum erro'.

  ENDIF.


ENDFUNCTION.
