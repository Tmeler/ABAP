REPORT z_debug_tela_dinamica.

************************************************************************
* External Tables                                                      *
************************************************************************
TABLES: kna1.
************************************************************************
* Internal Tables                                                      *
************************************************************************
DATA: i_filetable TYPE filetable.
************************************************************************
* Screen Parameters                                                    *
************************************************************************
* Begin - Block 1
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: r_local RADIOBUTTON GROUP rad1 USER-COMMAND flg, "Local
            r_server RADIOBUTTON GROUP rad1 DEFAULT 'X'.     "Server
* Begin - Block 2
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETER: p_local  LIKE rlgrap-filename DEFAULT 'C:\' OBLIGATORY.
PARAMETER: p_server LIKE rlgrap-filename DEFAULT '/default' OBLIGATORY.
* End - Block 2
SELECTION-SCREEN END OF BLOCK b2.
* Begin - Block 3
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
SELECT-OPTIONS: s_kunnr FOR kna1-kunnr, "Customer Number 1
                s_land1 FOR kna1-land1. "Country Key
* End - Block 3
SELECTION-SCREEN END OF BLOCK b3.
* End - Block 1
SELECTION-SCREEN END OF BLOCK b1.
************************************************************************
* At Selection Screen OUTPUT                                           *
************************************************************************
AT SELECTION-SCREEN OUTPUT.
* Check what radiobutton is selected
* Local
  IF r_local = 'X'.
    LOOP AT SCREEN.
      IF "screen-name = 'P_LOCAL'. " OR
         screen-name = '%_P_LOCAL_%_APP_%-TEXT'.
        screen-invisible = '0'.
        screen-input = '1'.
        MODIFY SCREEN.
      ELSEIF screen-name = 'P_SERVER'. " OR
             screen-name = '%_P_SERVER_%_APP_%-TEXT'.
        screen-invisible = '1'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
* Server
  ELSE.
    LOOP AT SCREEN.
      IF screen-name = 'P_SERVER' OR
         screen-name = '%_P_SERVER_%_APP_%-TEXT'.
        screen-invisible = '0'.
        screen-input = '0'.
        MODIFY SCREEN.
      ELSEIF screen-name = 'P_LOCAL' OR
             screen-name = '%_P_LOCAL_%_APP_%-TEXT' OR
             screen-name = '%_S_KUNNR_%_APP_%-TEXT' OR
             screen-name = '%_S_KUNNR_%_APP_%-OPTI_PUSH' OR
             screen-name = 'S_KUNNR-LOW' OR
             screen-name = '%_S_KUNNR_%_APP_%-TO_TEXT' OR
             screen-name = 'S_KUNNR-HIGH' OR
             screen-name = '%_S_KUNNR_%_APP_%-VALU_PUSH'.             .
        screen-invisible = '1'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
************************************************************************
* At Selection Screen ON VALUE REQUEST                                 *
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_local.
  PERFORM filename_local CHANGING p_local.
************************************************************************
START-OF-SELECTION.
************************************************************************
************************************************************************
* Forms                                                                *
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  filename_local
*&---------------------------------------------------------------------*
*&
FORM filename_local CHANGING p_file TYPE rlgrap-filename.
  DATA: vl_rc           TYPE i,
        vl_action       TYPE i.
* Open the File Open Dialog
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      file_filter             = ''
      initial_directory       = 'C:\'
    CHANGING
      file_table              = i_filetable
      rc                      = vl_rc
      user_action             = vl_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0 OR vl_rc < 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF vl_action EQ cl_gui_frontend_services=>action_ok.
      CLEAR p_file.
      READ TABLE i_filetable INDEX 1 INTO p_file.
    ENDIF.
  ENDIF.
ENDFORM.                               " filename_local
