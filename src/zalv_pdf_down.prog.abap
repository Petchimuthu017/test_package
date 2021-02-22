*&---------------------------------------------------------------------*
*& Report ZALV_PDF_DOWN
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZALV_PDF_DOWN. "TO SAP-SPOOL
                       "    SPOOL PARAMETERS print_parameters
                        "   ARCHIVE PARAMETERS archi_parameters
                         "  WITHOUT SPOOL DYNPRO.
tables :kna1.

TYPES : BEGIN OF t_final,
      KUNNR TYPE KUNNR,
      LAND1 TYPE LAND1_GP,
      END OF t_final.
 DATA :   lt_final  TYPE STANDARD TABLE OF t_final,
          lfl_final TYPE t_final.
"Data Declaration:
DATA: lv_val         TYPE c,
      lv_pripar      TYPE pri_params,
      lv_arcpar      TYPE arc_params,
      lv_spoolid     LIKE tsp01-rqident,
      lv_no_of_bytes TYPE i                 ##NEEDED,
      lv_pdf_spoolid LIKE tsp01-rqident     ##NEEDED,
      lv_jobname     LIKE tbtcjob-jobname   ##NEEDED,
      lv_jobcount    LIKE tbtcjob-jobcount  ##NEEDED,

      lv_amount      TYPE dmbtr,
      lv_total       TYPE string,
      lit_pdf        TYPE TABLE OF tline,
      lv_file        TYPE string.


      CALL FUNCTION 'GET_PRINT_PARAMETERS'
       EXPORTING
*         ARCHIVE_ID                     = C_CHAR_UNKNOWN
*         ARCHIVE_INFO                   = C_CHAR_UNKNOWN
*         ARCHIVE_MODE                   = C_CHAR_UNKNOWN
*         ARCHIVE_TEXT                   = C_CHAR_UNKNOWN
*         AR_OBJECT                      = C_CHAR_UNKNOWN
*         ARCHIVE_REPORT                 = C_CHAR_UNKNOWN
*         AUTHORITY                      = C_CHAR_UNKNOWN
*         COPIES                         = C_NUM3_UNKNOWN
*         COVER_PAGE                     = C_CHAR_UNKNOWN
*         DATA_SET                       = C_CHAR_UNKNOWN
*         DEPARTMENT                     = C_CHAR_UNKNOWN
*         DESTINATION                    = C_CHAR_UNKNOWN
*         EXPIRATION                     = C_NUM1_UNKNOWN
*         IMMEDIATELY                    = C_CHAR_UNKNOWN
         IN_ARCHIVE_PARAMETERS          = 'lv_arcpar'
         IN_PARAMETERS                  = 'lv_pripar'
         LAYOUT                         = 'X_65_132'
         LINE_COUNT                     = 36
         LINE_SIZE                      = 260
*         LIST_NAME                      = C_CHAR_UNKNOWN
*         LIST_TEXT                      = C_CHAR_UNKNOWN
*         MODE                           = ' '
*         NEW_LIST_ID                    = C_CHAR_UNKNOWN
*         PROTECT_LIST                   = C_CHAR_UNKNOWN
*         NO_DIALOG                      = X
*         RECEIVER                       = C_CHAR_UNKNOWN
*         RELEASE                        = C_CHAR_UNKNOWN
*         REPORT                         = C_CHAR_UNKNOWN
*         SAP_COVER_PAGE                 = C_CHAR_UNKNOWN
*         HOST_COVER_PAGE                = C_CHAR_UNKNOWN
*         PRIORITY                       = C_NUM1_UNKNOWN
*         SAP_OBJECT                     = C_CHAR_UNKNOWN
*         TYPE                           = C_CHAR_UNKNOWN
*         USER                           = SY-UNAME
*         USE_OLD_LAYOUT                 = ' '
*         UC_DISPLAY_MODE                = C_CHAR_UNKNOWN
*         DRAFT                          = C_CHAR_UNKNOWN
*         ABAP_LIST                      = ' '
*         USE_ARCHIVENAME_DEF            = ' '
*         DEFAULT_SPOOL_SIZE             = C_CHAR_UNKNOWN
*         WITH_STRUCTURE                 = C_CHAR_UNKNOWN
*         SUPPRESS_SHADING               = C_CHAR_UNKNOWN
*         PO_FAX_STORE                   = ' '
*         NO_FRAMES                      = C_CHAR_UNKNOWN
       IMPORTING
         OUT_ARCHIVE_PARAMETERS         = lv_arcpar
         OUT_PARAMETERS                 = lv_pripar
         VALID                          = lv_val

*         VALID_FOR_SPOOL_CREATION       =
       EXCEPTIONS
         ARCHIVE_INFO_NOT_FOUND         = 1
         INVALID_PRINT_PARAMS           = 2
         INVALID_ARCHIVE_PARAMS         = 3
         OTHERS                         = 4
                .
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.


*FUNCTION 'GET_PRINT_PARAMETERS'
*EXPORTING
*in_archive_parameters  = lv_arcpar
*in_parameters          = lv_pripar
*layout                 = 'X_65_132'
*line_count             = 35
*line_size              = 260
*no_dialog              = 'X'
*IMPORTING
*out_archive_parameters = lv_arcpar
*out_parameters         = lv_pripar
*valid                  = lv_val.
IF lv_val  NE space.
lv_pripar-prrel = space.
lv_pripar-primm = space.
NEW-PAGE PRINT ON  NEW-SECTION PARAMETERS
lv_pripar ARCHIVE PARAMETERS lv_arcpar NO DIALOG.
ENDIF.

"Write Internal Table
LOOP AT lt_final  INTO lfl_final.
    WRITE:/ '|',lfl_final-kunnr,'|', lfl_final-LAND1,'|'.
    CLEAR lfl_final.
    WRITE: sy-uline.
  ENDLOOP.

NEW-PAGE PRINT OFF.
CALL FUNCTION 'ABAP4_COMMIT_WORK'.

  lv_spoolid = sy-spono.
  CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
    EXPORTING
      src_spoolid   = lv_spoolid
      no_dialog     = ' '
    IMPORTING
      pdf_bytecount = lv_no_of_bytes
      pdf_spoolid   = lv_pdf_spoolid
      btc_jobname   = lv_jobname
      btc_jobcount  = lv_jobcount
    TABLES
      pdf           = lit_pdf.               "OTF Data

CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_file      "File Path
      filetype                = 'BIN'
    TABLES
      data_tab                = lit_pdf
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.
