REPORT z_fetch_data_from_cpi.

PARAMETERS: p_url TYPE string.

DATA: lt_response TYPE TABLE OF string,
      lv_response TYPE string,
      lv_http_code TYPE string.

" Define a estrutura da tabela Z
TYPES: BEGIN OF ty_data,
         field1 TYPE string,
         field2 TYPE string,
         " Adicione outros campos conforme necessário
       END OF ty_data.

DATA: lt_data TYPE TABLE OF ty_data,
      lt_errors TYPE TABLE OF string.

" Execute HTTP GET request
CALL METHOD cl_http_client=>create_by_url
  EXPORTING
    url                = p_url
  IMPORTING
    client             = DATA(lo_http_client).

CALL METHOD lo_http_client->send
  IMPORTING
    response_code      = lv_http_code
    response_body      = lv_response.

IF lv_http_code = '200'.
  " Parse the response
  SPLIT lv_response AT cl_abap_char_utilities=>newline INTO TABLE lt_response.

  LOOP AT lt_response INTO lv_response.
    " Assuma-se que os dados estão separados por vírgula
    DATA: lt_fields TYPE TABLE OF string,
          lv_field TYPE string.

    SPLIT lv_response AT ',' INTO TABLE lt_fields.

    READ TABLE lt_fields INDEX 1 INTO lv_field.
    IF sy-subrc = 0.
      DATA(ls_data) = VALUE ty_data(
        field1 = lv_field
        " Preencha outros campos
      ).
      APPEND ls_data TO lt_data.
    ENDIF.
  ENDLOOP.

  " Gravar dados na tabela Z
  LOOP AT lt_data INTO DATA(ls_data).
    INSERT ztable INTO TABLE lt_data.
    IF sy-subrc <> 0.
      APPEND 'Erro ao inserir dados na tabela Z' TO lt_errors.
    ENDIF.
  ENDLOOP.

  IF lines( lt_errors ) > 0.
    LOOP AT lt_errors INTO DATA(lv_error).
      WRITE: / lv_error.
    ENDLOOP.
  ELSE.
    WRITE: / 'Dados importados com sucesso!'.
  ENDIF.
ELSE.
  WRITE: / 'Erro ao obter dados. Código HTTP:', lv_http_code.
ENDIF.
