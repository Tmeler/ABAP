report Z_BATCH_CRIA_USER
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
* data element: XUBNAME
        BNAME_001(012),
* data element:
        GV_COPY_UNAME_SRC_002(012),
* data element:
        GV_COPY_UNAME_DST_003(012),
* data element:
        DEFAULTS_004(001),
* data element:
        PARAMETERS_005(001),
* data element:
        REFUSER_006(001),
* data element:
        ROLES_007(001),
* data element:
        PROFILES_008(001),
* data element:
        USERGROUPS_009(001),
* data element:
        PERS_010(001),
* data element:
        UCLASS_011(001),
* data element:
        EASY_ACCESS_012(001),
* data element: XUUSTYP
        USTYP_013(001),
* data element: XUNCODE
        PASSWORD_014(040),
* data element: XUNCOD2
        PASSWORD2_015(040),
* data element: XUCLASS
        CLASS_016(012),
      end of record.

*** End generated data section ***

start-of-selection.

perform open_dataset using dataset.
perform open_group.

do.

read dataset dataset into record.
if sy-subrc <> 0. exit. endif.

perform bdc_dynpro      using 'SAPLSUID_MAINTENANCE' '1050'.
perform bdc_field       using 'BDC_CURSOR'
                              'SUID_ST_BNAME-BNAME'.
perform bdc_field       using 'BDC_OKCODE'
                              '=COPY'.
perform bdc_field       using 'SUID_ST_BNAME-BNAME'
                              record-BNAME_001.
perform bdc_dynpro      using 'SAPLSUID_MAINTENANCE' '1200'.
perform bdc_field       using 'BDC_CURSOR'
                              'GV_COPY_UNAME_DST'.
perform bdc_field       using 'BDC_OKCODE'
                              '=COPY'.
perform bdc_field       using 'GV_COPY_UNAME_SRC'
                              record-GV_COPY_UNAME_SRC_002.
perform bdc_field       using 'GV_COPY_UNAME_DST'
                              record-GV_COPY_UNAME_DST_003.
perform bdc_field       using 'GS_COPY_OPTIONS-DEFAULTS'
                              record-DEFAULTS_004.
perform bdc_field       using 'GS_COPY_OPTIONS-PARAMETERS'
                              record-PARAMETERS_005.
perform bdc_field       using 'GS_COPY_OPTIONS-REFUSER'
                              record-REFUSER_006.
perform bdc_field       using 'GS_COPY_OPTIONS-ROLES'
                              record-ROLES_007.
perform bdc_field       using 'GS_COPY_OPTIONS-PROFILES'
                              record-PROFILES_008.
perform bdc_field       using 'GS_COPY_OPTIONS-USERGROUPS'
                              record-USERGROUPS_009.
perform bdc_field       using 'GS_COPY_OPTIONS-PERS'
                              record-PERS_010.
perform bdc_field       using 'GS_COPY_OPTIONS-UCLASS'
                              record-UCLASS_011.
perform bdc_field       using 'GS_COPY_OPTIONS-EASY_ACCESS'
                              record-EASY_ACCESS_012.
perform bdc_dynpro      using 'SAPLSUID_MAINTENANCE' '1100'.
perform bdc_field       using 'BDC_OKCODE'
                              '=UPD'.
perform bdc_field       using 'BDC_CURSOR'
                              'SUID_ST_NODE_PASSWORD_EXT-PASSWORD2'.
perform bdc_field       using 'SUID_ST_NODE_LOGONDATA-USTYP'
                              record-USTYP_013.
perform bdc_field       using 'SUID_ST_NODE_PASSWORD_EXT-PASSWORD'
                              record-PASSWORD_014.
perform bdc_field       using 'SUID_ST_NODE_PASSWORD_EXT-PASSWORD2'
                              record-PASSWORD2_015.
perform bdc_field       using 'SUID_ST_NODE_LOGONDATA-CLASS'
                              record-CLASS_016.
perform bdc_transaction using 'SU01'.

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
