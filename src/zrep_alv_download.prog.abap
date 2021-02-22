*---------------------------------------------------------------------*
* Syngenta Technologies Ltd.
*---------------------------------------------------------------------*
* Program Name       : ZREP_ALV_DOWNLOAD
* Title              : extract data from kna1 and adr6 table.
* Functional Analyst : -
* Developer          : Akash
* Date               : 06/03/2020
* Description        :
*
***********************************************************************
* Modification Log
*---------------------------------------------------------------------*
* Date      Developer ID Transport #  Description
*---------------------------------------------------------------------*
* 06.03.2020 s1010576     E2BK925985   Initial Development

REPORT ZREP_ALV_DOWNLOAD NO STANDARD PAGE HEADING
                                        MESSAGE-ID 00
                                        LINE-SIZE 132
                                        LINE-COUNT 65.

*----------------------------------------------------------------------*
*                            Data and Type-pools
*----------------------------------------------------------------------*
TABLES : kna1, adr6, tsp01.
*
DATA : lv_file TYPE string.

TYPES:BEGIN OF ty_kna1,
        kunnr TYPE kunnr,      " customers no
        name1 TYPE name1_gp,   " customers name
        adrnr TYPE adrnr,      " house number
        stras TYPE stras_gp,   "  street
        ort01 TYPE ort01_gp,   " City
        pstlz TYPE pstlz,      " Postal Code
        telf1 TYPE telf1,      " telephone number
      END   OF ty_kna1.

DATA : gt_kna1 TYPE STANDARD TABLE OF ty_kna1 WITH HEADER LINE,
       gs_kna1 TYPE ty_kna1.
*
TYPES: BEGIN OF ty_adr6,
        smtp_addr TYPE ad_smtpadr,          " E-Mail Address
        addrnumber type adr6-addrnumber,    " address no
      END OF ty_adr6.

DATA : gt_adr6 TYPE STANDARD TABLE OF ty_adr6 WITH HEADER LINE,
       gs_adr6 TYPE ty_adr6.
*
TYPES:BEGIN OF ty_final,
        kunnr TYPE kunnr,      " customers no
        name1 TYPE name1_gp,   " customers name
        adrnr TYPE adrnr,      " house number
        stras TYPE stras_gp,   " , street
        ort01 TYPE ort01_gp,   " City
        pstlz TYPE pstlz,      " Postal Code
        telf1 TYPE telf1,      " telephone number
        smtp_addr TYPE ad_smtpadr,  " E-Mail Address
      END   OF ty_final.

DATA : gt_final TYPE STANDARD TABLE OF ty_final, " WITH HEADER LINE,
       gs_final TYPE ty_final.
*
*"----------------- End changes s1010356---------------------------------

DATA: pdf LIKE tline OCCURS 0.
DATA:
      g_spool   TYPE tsp01-rqident,
      g_program TYPE sy-repid VALUE sy-repid.

TYPE-POOLS:slis.
 DATA: it_fcat TYPE slis_t_fieldcat_alv,
       wa_fcat LIKE LINE OF it_fcat,
       wa_lay  TYPE slis_layout_alv.

DATA:
      w_print TYPE slis_print_alv,
      w_print_ctrl TYPE alv_s_pctl.


DATA: t_fieldcat   TYPE slis_t_fieldcat_alv,
      x_fieldcat  TYPE slis_fieldcat_alv.

* Data declartion for ALV
DATA: go_table            TYPE REF TO cl_salv_table,
      gv_o_salv           TYPE REF TO cl_salv_table,
      go_columns_table    TYPE REF TO cl_salv_columns_table,
      go_column           TYPE REF TO cl_salv_column,       "#EC NEEDED
      go_functions        TYPE REF TO cl_salv_functions_list,
      go_display_settings TYPE REF TO cl_salv_display_settings,
      go_layout           TYPE REF TO cl_salv_layout,
      gs_layout_key       TYPE        salv_s_layout_key,
      go_events           TYPE REF TO cl_salv_events_table, "#EC NEEDED
      go_content          TYPE REF TO cl_salv_form_element, "#EC NEEDED
      go_grid             TYPE REF TO cl_salv_form_layout_grid, "#EC NEEDED
      gv_repid            TYPE sy-repid,
      gv_title            TYPE sy-title,
      gv_salv_msg         TYPE REF TO cx_salv_msg,          "#EC NEEDED
      gv_salv_data_error  TYPE REF TO cx_salv_data_error,   "#EC NEEDED
      gv_msg              TYPE string.



*DATA : BEGIN OF it_heading OCCURS 0,
*         text(15),
*
*       END OF it_heading.
*
*it_heading-text = 'Customer_Number'.
*APPEND it_heading.
*it_heading-text = 'Customer_Name'.
*APPEND it_heading.
*it_heading-text = 'House_Number'.
*APPEND it_heading.
*it_heading-text = 'Street'.
*APPEND it_heading.
*it_heading-text = 'City'.
*APPEND it_heading.
*it_heading-text = 'postal_code'.
*APPEND it_heading.
*it_heading-text = 'telephone_number'.
*APPEND it_heading.
*it_heading-text = 'email_address'.
*APPEND it_heading.

*----------------------------------------------------------------------*
*                            Selection-Screen
*----------------------------------------------------------------------*

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS : p_coun TYPE kna1-land1 .
SELECTION-SCREEN : END OF BLOCK b1.

SELECTION-SCREEN : BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS : p_file TYPE rlgrap-filename.

SELECTION-SCREEN : END OF BLOCK b2.

SELECTION-SCREEN : BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
PARAMETERS : p_alv RADIOBUTTON GROUP rg1 DEFAULT 'X',
             p_pdf RADIOBUTTON GROUP rg1.

SELECTION-SCREEN : END OF BLOCK b3.

*----------------------------------------------------------------------*
**** AT SELECTION-SCREEN ON VALUE-REQUEST
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      field_name = 'P_FILE'
    IMPORTING
      file_name  = p_file.

*-------------------------------------------------------------------------*
*                            INITIALIZATION
*-------------------------------------------------------------------------*
initialization.
*-------------------------------------------------------------------------*
*                          START-OF-SELECTION
*-------------------------------------------------------------------------*
start-of-selection.
" using for all entries. "
SELECT kunnr name1 adrnr stras ort01 pstlz telf1 FROM kna1
                                                 INTO TABLE gt_kna1
                                                 UP TO 20 ROWS
                                                 WHERE LAND1 eq p_coun.
SORT gt_kna1 BY adrnr.
IF gt_kna1[] IS NOT INITIAL.

  SELECT  smtp_addr addrnumber
                              FROM adr6
                              INTO TABLE gt_adr6
                              FOR ALL ENTRIES IN gt_kna1
                              WHERE ADDRNUMBER = gt_kna1-adrnr.
ENDIF.

LOOP AT gt_kna1 INTO gs_kna1.

READ TABLE gt_adr6 INTO gs_adr6 WITH KEY ADDRNUMBER = gs_kna1-adrnr.

                    gs_final-kunnr = gs_kna1-kunnr.
                    gs_final-name1 = gs_kna1-name1.
                    gs_final-adrnr = gs_kna1-adrnr.
                    gs_final-stras = gs_kna1-stras.
                    gs_final-ort01 = gs_kna1-ort01.
                    gs_final-pstlz = gs_kna1-pstlz.
                    gs_final-telf1 = gs_kna1-telf1.
                    gs_final-smtp_addr = gs_adr6-smtp_addr.

      APPEND gs_final TO gt_final.
      CLEAR : gs_kna1, gs_adr6.
    ENDLOOP.



"dowload functionality"
IF p_alv = 'X'.
  IF gt_final[] IS NOT INITIAL.
      TRY.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table   = go_table
            CHANGING
              t_table        = gt_final  ).
        CATCH cx_salv_msg .
      ENDTRY.
*      CREATE OBJECT go_event_handler.
      go_columns_table = go_table->get_columns(  ).
*get_color_column
      TRY.
          CALL METHOD go_columns_table->set_cell_type_column
            EXPORTING
              value = 'CELL_TYPE'.
        CATCH cx_salv_data_error .
      ENDTRY.

 GV_TITLE = ' ZSOAUTH TABLE REPORT'.
*Display settings
      GO_DISPLAY_SETTINGS = GO_TABLE->GET_DISPLAY_SETTINGS( ).
      GO_DISPLAY_SETTINGS->SET_STRIPED_PATTERN( ABAP_TRUE ).
      GO_DISPLAY_SETTINGS->SET_LIST_HEADER( GV_TITLE ).

*Show all Tools on ALV output
      go_functions = go_table->get_functions( ).
      go_functions->set_all( ).
      go_functions->set_view_excel( abap_true ).
*
*Update some Functions restricted by default
      go_layout = go_table->get_layout( ).
      gs_layout_key-report = gv_repid.
      go_layout->set_key( gs_layout_key ).
      go_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

*Display the data
      go_table->display( ).
    ELSE.
      MESSAGE s000(zb) WITH 'No Data to Display' DISPLAY LIKE 'I'.
    ENDIF.

*--Build field catalog
  x_fieldcat-fieldname  = 'kunnr'.    " Fieldname in the data table
  x_fieldcat-seltext_m  = 'Customer_Number'." Column description
  APPEND x_fieldcat TO t_fieldcat.
  CLEAR x_fieldcat.

  x_fieldcat-fieldname  = 'name1'.
  x_fieldcat-seltext_m  = 'customer_name'.
  APPEND x_fieldcat TO t_fieldcat.
  CLEAR x_fieldcat.

  x_fieldcat-fieldname  = 'adrnr'.
  x_fieldcat-seltext_m  = 'House_Number'.
  APPEND x_fieldcat TO t_fieldcat.
  CLEAR x_fieldcat.

  x_fieldcat-fieldname  = 'stras'.
  x_fieldcat-seltext_m  = 'Street'.
  APPEND x_fieldcat TO t_fieldcat.
  CLEAR x_fieldcat.

  x_fieldcat-fieldname  = 'ort01'.
  x_fieldcat-seltext_m  = 'City'.
  APPEND x_fieldcat TO t_fieldcat.
  CLEAR x_fieldcat.

  x_fieldcat-fieldname  = 'pstlz'.
  x_fieldcat-seltext_m  = 'postal_code'.
  APPEND x_fieldcat TO t_fieldcat.
  CLEAR x_fieldcat.

  x_fieldcat-fieldname  = 'telf1'.
  x_fieldcat-seltext_m  = 'telephone_number'.
  APPEND x_fieldcat TO t_fieldcat.
  CLEAR x_fieldcat.

  x_fieldcat-fieldname  = 'smtp_addr'.
  x_fieldcat-seltext_m  = 'email_address'.
  APPEND x_fieldcat TO t_fieldcat.
  CLEAR x_fieldcat.

ELSEIF p_pdf = 'X'."---------------------------------Convert ALV Output Into A PDF file.----------------------

IF SY-SUBRC EQ 0.
     wa_fcat-fieldname = 'kunnr'.
     wa_fcat-tabname   = 'gt_final'.
     wa_fcat-seltext_m = 'Customer_Number'.
     APPEND wa_fcat TO it_fcat.

      wa_fcat-fieldname = 'name1'.
     wa_fcat-tabname   = 'gt_final'.
     wa_fcat-seltext_m = 'customer_Name'.
     APPEND wa_fcat TO it_fcat.

     wa_fcat-fieldname = 'adrnr'.
     wa_fcat-tabname   = 'gt_final'.
     wa_fcat-seltext_m = 'House_Number'.
     APPEND wa_fcat TO it_fcat.

     wa_fcat-fieldname = 'stras'.
     wa_fcat-tabname   = 'gt_final'.
     wa_fcat-seltext_m = 'Street'.
     APPEND wa_fcat TO it_fcat.

     wa_fcat-fieldname = 'ort01'.
     wa_fcat-tabname   = 'gt_final'.
     wa_fcat-seltext_m = 'City'.
     APPEND wa_fcat TO it_fcat.

     wa_fcat-fieldname = 'pstlz'.
     wa_fcat-tabname   = 'gt_final'.
     wa_fcat-seltext_m = 'Postal_Code'.
     APPEND wa_fcat TO it_fcat.

     wa_fcat-fieldname = 'telf1'.
     wa_fcat-tabname   = 'gt_final'.
     wa_fcat-seltext_m = 'Telephone_Number'.
     APPEND wa_fcat TO it_fcat.

     wa_fcat-fieldname = 'smtp_addr'.
     wa_fcat-tabname   = 'gt_final'.
     wa_fcat-seltext_m = 'Email_Id'.
     APPEND wa_fcat TO it_fcat.


     wa_lay-colwidth_optimize = 'X'.
     wa_lay-zebra = 'X'.

    w_print-print = 'X'.

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
EXPORTING
is_layout     = wa_lay
it_fieldcat   = it_fcat
i_callback_program = g_program
i_structure_name   = 'ty_final'
is_print           = w_print
TABLES
t_outtab           = gt_final.

ENDIF.

lv_file = p_file.

IF sy-subrc <> 0.
MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ELSE.

g_spool = sy-spono.

CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
  EXPORTING
    src_spoolid = g_spool
  TABLES
    pdf         = pdf.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ELSE.
CALL FUNCTION 'GUI_DOWNLOAD'
  EXPORTING
      filename = lv_file
      filetype = 'BIN'
  TABLES
      data_tab = pdf.
ENDIF.

ENDIF.



ENDIF.

*"-------------------------------------------------------------------------*
*                           END-OF-SELECTION.
*"-------------------------------------------------------------------------*

end-of-selection.
