REPORT ZBUTTON.
data: carr_id type sbook-carrid,
      cparam type ssfctrlop,
      outop type ssfcompop,
      fm_name type rs38l_fnam.

DATA: tab_otf_data TYPE ssfcrescl,
      pdf_tab LIKE tline OCCURS 0 WITH HEADER LINE,
      tab_otf_final TYPE itcoo OCCURS 0 WITH HEADER LINE,
      file_size TYPE i,
      bin_filesize TYPE i,
      FILE_NAME type string,
      File_path type string,
      FULL_PATH type string.

parameter:      p_custid type scustom-id default 1.
select-options: s_carrid for carr_id     default 'LH' to 'LH'.
parameter:      p_form   type tdsfname   default 'ZAMIT_SMART_FORM'.

data: customer    type scustom,
      bookings    type ty_bookings,
      connections type ty_connections.

start-of-selection.

***************** suppressing the dialog box for print preview****************************
outop-tddest = 'LP01'.
cparam-no_dialog = 'X'.
cparam-preview = SPACE.
cparam-getotf = 'X'.

  select single * from scustom into customer where id = p_custid.
  check sy-subrc = 0.


  select * from sbook   into table bookings
           where customid = p_custid
           and   carrid in s_carrid
           order by primary key.


  select * from spfli into table connections
           for all entries in bookings
           where carrid = bookings-carrid
           and   connid = bookings-connid
           order by primary key.



  call function 'SSF_FUNCTION_MODULE_NAME'
       exporting  formname           = p_form
*                 variant            = ' '
*                 direct_call        = ' '
       importing  fm_name            = fm_name
       exceptions no_form            = 1
                  no_function_module = 2
                  others             = 3.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    exit.
  endif.



* calling the generated function module
  call function fm_name
       exporting
*                 archive_index        =
*                 archive_parameters   =
                 control_parameters   = cparam
*                 mail_appl_obj        =
*                 mail_recipient       =
*                 mail_sender          =
                 output_options       =  outop
                 user_settings        = SPACE
                 bookings             = bookings
                  customer             = customer
                  connections          = connections
      importing
*                 document_output_info =
                 job_output_info      = tab_otf_data
*                 job_output_options   =
       exceptions formatting_error     = 1
                  internal_error       = 2
                  send_error           = 3
                  user_canceled        = 4
                  others               = 5.

  if sy-subrc <> 0.
*   error handling
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.


  tab_otf_final[] = tab_otf_data-otfdata[].



  CALL FUNCTION 'CONVERT_OTF'
 EXPORTING
   format                      = 'PDF'
   max_linewidth               = 132
*   ARCHIVE_INDEX               = ' '
*   COPYNUMBER                  = 0
*   ASCII_BIDI_VIS2LOG          = ' '
 IMPORTING
   bin_filesize                = bin_filesize
*   BIN_FILE                    =
  TABLES
    otf                         = tab_otf_final
    lines                       = pdf_tab
 EXCEPTIONS
   err_max_linewidth           = 1
   err_format                  = 2
   err_conv_not_possible       = 3
   err_bad_otf                 = 4
   OTHERS                      = 5
          .
IF sy-subrc <> 0.
 MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.




CALL METHOD cl_gui_frontend_services=>file_save_dialog
*  EXPORTING
*    WINDOW_TITLE         =
*    DEFAULT_EXTENSION    =
*    DEFAULT_FILE_NAME    =
*    FILE_FILTER          =
*    INITIAL_DIRECTORY    =
*    WITH_ENCODING        =
*    PROMPT_ON_OVERWRITE  = 'X'
  CHANGING
    filename             = FILE_NAME
    path                 = FILE_PATH
    fullpath             = FULL_PATH
*    USER_ACTION          =
*    FILE_ENCODING        =
*  EXCEPTIONS
*    CNTL_ERROR           = 1
*    ERROR_NO_GUI         = 2
*    NOT_SUPPORTED_BY_GUI = 3
*    others               = 4
        .
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.




*************downloading the converted PDF data to your local PC********

CALL FUNCTION 'GUI_DOWNLOAD'
  EXPORTING
   bin_filesize                    = bin_filesize
   filename                        = FULL_PATH
   filetype                        = 'BIN'
*   APPEND                          = ' '
*   WRITE_FIELD_SEPARATOR           = ' '
*   HEADER                          = '00'
*   TRUNC_TRAILING_BLANKS           = ' '
*   WRITE_LF                        = 'X'
*   COL_SELECT                      = ' '
*   COL_SELECT_MASK                 = ' '
*   DAT_MODE                        = ' '
*   CONFIRM_OVERWRITE               = ' '
*   NO_AUTH_CHECK                   = ' '
*   CODEPAGE                        = ' '
*   IGNORE_CERR                     = ABAP_TRUE
*   REPLACEMENT                     = '#'
*   WRITE_BOM                       = ' '
*   TRUNC_TRAILING_BLANKS_EOL       = 'X'
 IMPORTING
   filelength                      = file_size
  TABLES
    data_tab                        = pdf_tab
*   FIELDNAMES                      =
 EXCEPTIONS
   file_write_error                = 1
   no_batch                        = 2
   gui_refuse_filetransfer         = 3
   invalid_type                    = 4
   no_authority                    = 5
   unknown_error                   = 6
   header_not_allowed              = 7
   separator_not_allowed           = 8
   filesize_not_allowed            = 9
   header_too_long                 = 10
   dp_error_create                 = 11
   dp_error_send                   = 12
   dp_error_write                  = 13
   unknown_dp_error                = 14
   access_denied                   = 15
   dp_out_of_memory                = 16
   disk_full                       = 17
   dp_timeout                      = 18
   file_not_found                  = 19
   dataprovider_exception          = 20
   control_flush_error             = 21
   OTHERS                          = 22
          .
IF sy-subrc <> 0.

ENDIF.       .
