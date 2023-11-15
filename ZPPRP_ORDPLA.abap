*------------------------------------------------------------------------*
*Relatório para controle de prazos de entrega
*------------------------------------------------------------------------*

REPORT ZPPRP_ORDPLA NO STANDARD PAGE HEADING.

TABLES: EKKO,
        EKPO,
        EKET.

DATA: BEGIN OF T_PEDIDOS OCCURS 0,
       EBELN LIKE EKKO-EBELN,   " Numero do pedido
       EBELP LIKE EKPO-EBELP,  " Numero do item
       MATNR LIKE EKPO-MATNR,  " Nº material
       TXZ01 LIKE EKPO-TXZ01,  " Descrição do item
      END OF T_PEDIDOS.

DATA: BEGIN OF T_PEDITEM OCCURS 0,
       EBELN LIKE EKKO-EBELN,   " Numero do pedido
       EBELP LIKE EKPO-EBELP,  " Numero do item
       MATNR LIKE EKPO-MATNR,  " Nº material
       TXZ01 LIKE EKPO-TXZ01,  " Descrição do item
       MENGE LIKE EKET-MENGE,  " Qtde do item
       WEMNG LIKE EKET-WEMNG,  " Qtde já fornecida
       EINDT LIKE EKET-EINDT, " Data de entrega
      END OF T_PEDITEM.

*----------------------------------------------------------------------*
* Parametros de Seleção
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS P_LIFNR FOR EKKO-LIFNR.
  SELECT-OPTIONS P_EBELN FOR EKKO-EBELN.
  SELECT-OPTIONS P_BEDAT FOR EKKO-BEDAT.
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.

* ******************************************************************** *
*  start-of-selection.                                                 *
* ******************************************************************** *
START-OF-SELECTION.

  PERFORM SELECT_MAINDATA.
  PERFORM GET_ALLDATA.
  PERFORM PRINT_DATA.

END-OF-SELECTION.

* -------------------------------------------------------------------- *
*       Form  SELECT_MAINDATA
* -------------------------------------------------------------------- *
FORM SELECT_MAINDATA.

SELECT EKKO~EBELN EKPO~EBELP EKPO~MATNR EKPO~TXZ01
  FROM ( EKKO INNER JOIN EKPO ON EKKO~EBELN = EKPO~EBELN )
    INTO (T_PEDIDOS-EBELN, T_PEDIDOS-EBELP,
          T_PEDIDOS-MATNR, T_PEDIDOS-TXZ01)
    WHERE ( EKKO~BSART = 'NB  ' OR
            EKKO~BSART = 'ZCON' )
      AND EKKO~LIFNR IN P_LIFNR
      AND EKKO~BEDAT IN P_BEDAT
      AND EKPO~EBELN IN P_EBELN
      AND EKPO~LOEKZ <> 'X'.
  APPEND T_PEDIDOS.
ENDSELECT.

ENDFORM.                               " SELECT_MAINDATA

* -------------------------------------------------------------------- *
*       Form  GET_ALLDATA
* -------------------------------------------------------------------- *
FORM GET_ALLDATA.


 DATA V_FIRST(1) TYPE C.

SORT T_PEDIDOS BY EBELN.

LOOP AT T_PEDIDOS.
  V_FIRST =  'S'.
  MOVE-CORRESPONDING T_PEDIDOS TO T_PEDITEM.

  SELECT MENGE WEMNG EINDT
   FROM EKET
   INTO (T_PEDITEM-MENGE, T_PEDITEM-WEMNG, T_PEDITEM-EINDT)
   WHERE ( EBELN = T_PEDIDOS-EBELN
    AND EBELP = T_PEDIDOS-EBELP ).
       APPEND T_PEDITEM.
  ENDSELECT.
ENDLOOP.

ENDFORM.                               " GET_ALLDATA

* -------------------------------------------------------------------- *
*       Form  PRINT_DATA
* -------------------------------------------------------------------- *
FORM PRINT_DATA.

SORT T_PEDITEM BY EBELN EBELP.
LOOP AT T_PEDITEM.
 WRITE / T_PEDITEM-EBELN.
 WRITE T_PEDITEM-EBELP.
 WRITE T_PEDITEM-MATNR.
 WRITE T_PEDITEM-TXZ01.
 WRITE T_PEDITEM-MENGE.
 WRITE T_PEDITEM-EINDT.
ENDLOOP.

ENDFORM.                               " PRINT_DATA

****Cabeçalho do relatório****
TOP-OF-PAGE.
 WRITE / 'Nome do emissor : '.
 WRITE SY-UNAME.
 WRITE / 'Data de emissão : '.
 WRITE SY-DATUM.
 WRITE / 'Hora da emissão : '.
 WRITE SY-UZEIT.
 SKIP.
 WRITE /(10) 'Pedido'.
 WRITE (5)   'Item'.
 WRITE (18) 'Material'.
 WRITE (52) 'Descrição'.
 WRITE (05) 'Qtde'.
 WRITE 'Entrega'.
ULINE.
