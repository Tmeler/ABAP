*&---------------------------------------------------------------------*
*& Report  Z_CHAMA_BADI
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_chama_badi.

DATA: handle TYPE REF TO z_badi_calc_valor  ,

sum TYPE p,

vat TYPE p,

percent TYPE p.

sum = 50.

GET BADI handle.

CALL BADI handle->get_valor
  EXPORTING
    im_ammount      = sum
  IMPORTING
    ex_ammount_vat  = vat
    ex_percent_vat = percent.

WRITE: 'percentage:', percent, 'VAT:', vat.

BREAK-POINT.
