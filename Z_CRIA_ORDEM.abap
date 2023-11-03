report Z_CRIA_ORDEM
       no standard page heading line-size 255.

include bdcrecx1.

parameters: dataset(132) lower case.
***    DO NOT CHANGE - the generated data section - DO NOT CHANGE    ***
*
*   If it is nessesary to change the data section use the rules:
*   1.) Each definition of a field exists of two lines
*   2.) The first line shows exactly the comment
*       '* data element: ' followed with the data element
*       which describes the field.
*       If you don't have a data element use the
*       comment without a data element name
*   3.) The second line shows the fieldname of the
*       structure, the fieldname must consist of
*       a fieldname and optional the character '_' and
*       three numbers and the field length in brackets
*   4.) Each field must be type C.
*
*** Generated data section with specific formatting - DO NOT CHANGE  ***
data: begin of record,
* data element: AUART
        AUART_001(004),
* data element: VBELN_VA
        VBELN_002(030),
* data element: KUNAG
        KUNNR_003(010),
* data element: KUNWE
        KUNNR_004(010),
* data element: KETDAT
        KETDAT_005(010),
* data element: KPRGBZ
        KPRGBZ_006(001),
* data element: PRSDT
        PRSDT_007(010),
* data element: DZTERM
        ZTERM_008(004),
* data element: INCO1
        INCO1_009(003),
* data element: INCO2
        INCO2_010(028),
      end of record.

*** End generated data section ***

start-of-selection.

perform open_dataset using dataset.
perform open_group.

do.

read dataset dataset into record.
if sy-subrc <> 0. exit. endif.

perform bdc_dynpro      using 'SAPMV45A' '0101'.
perform bdc_field       using 'BDC_CURSOR'
                              'VBAK-AUART'.
perform bdc_field       using 'BDC_OKCODE'
                              '=COPY'.
perform bdc_field       using 'VBAK-AUART'
                              record-AUART_001.
perform bdc_dynpro      using 'SAPLV45C' '0100'.
perform bdc_field       using 'BDC_OKCODE'
                              '=RAUF'.
perform bdc_field       using 'BDC_CURSOR'
                              'LV45C-VBELN'.
perform bdc_dynpro      using 'SAPLV45C' '0100'.
perform bdc_field       using 'BDC_OKCODE'
                              '=UEBR'.
perform bdc_field       using 'BDC_CURSOR'
                              'LV45C-VBELN'.
perform bdc_field       using 'LV45C-VBELN'
                              record-VBELN_002.
perform bdc_dynpro      using 'SAPMV45A' '4001'.
perform bdc_field       using 'BDC_OKCODE'
                              '=SICH'.
perform bdc_field       using 'KUAGV-KUNNR'
                              record-KUNNR_003.
perform bdc_field       using 'KUWEV-KUNNR'
                              record-KUNNR_004.
perform bdc_field       using 'RV45A-KETDAT'
                              record-KETDAT_005.
perform bdc_field       using 'RV45A-KPRGBZ'
                              record-KPRGBZ_006.
perform bdc_field       using 'VBKD-PRSDT'
                              record-PRSDT_007.
perform bdc_field       using 'VBKD-ZTERM'
                              record-ZTERM_008.
perform bdc_field       using 'VBKD-INCO1'
                              record-INCO1_009.
perform bdc_field       using 'VBKD-INCO2'
                              record-INCO2_010.
perform bdc_field       using 'BDC_CURSOR'
                              'RV45A-MABNR(02)'.
perform bdc_transaction using 'VA01'.

enddo.

perform close_group.
perform close_dataset using dataset.

*Text elements
*----------------------------------------------------------
* E00 Erro ao abrir conj.dados, cód.retorno:
* I01 Nome pasta
* I02 Abrir pasta
* I03 Inserir transação
* I04 Fechar pasta
* I05 Cód.ret.=
* I06 Pasta de erro criada
* S01 Nome pasta
* S02 Usuário
* S03 Manter pasta
* S04 Data de bloqueio
* S05 Modo de processamento
* S06 Modo atualização
* S07 Gerar pasta
* S08 Call Transaction
* S09 Pasta erro
* S10 Código nodata
* S11 Log breve


*Messages
*----------------------------------------------------------
*
* Message class: MESSTAB-MSGID
*MES
*
* Message class: MS
*613   Entrar um nome de pasta e um nome de usuário
