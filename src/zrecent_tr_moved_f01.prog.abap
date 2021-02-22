*&---------------------------------------------------------------------*
*&  Include           ZRECENT_TR_MOVED_F01
*&---------------------------------------------------------------------*
*&      Form  DEFAULT_OPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM default_option .

  DATA: lv_date TYPE sy-datum.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = sy-datum
      days      = 30
      months    = 0
      signum    = '-'
      years     = 0
    IMPORTING
      calc_date = lv_date.

  s_date-sign = 'I'.
  s_date-option = 'EQ'.
  s_date-low = lv_date.
  s_date-high = sy-datum.
  APPEND s_date.

ENDFORM.                    " DEFAULT_OPTION
*&---------------------------------------------------------------------*
*&      Form  FIND_TR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  TYPES: BEGIN OF pt_e070.
          INCLUDE TYPE e070.
  TYPES: as4text TYPE as4text.
  TYPES:  END OF pt_e070.

  DATA: lt_e070             TYPE STANDARD TABLE OF pt_e070,
        lw_e070             TYPE pt_e070,
        lt_e071             TYPE STANDARD TABLE OF e071,
        lw_e071             TYPE e071,
        ls_cofile_header    TYPE tstrfcofih,
        lv_project          TYPE trkorr_p,
        lt_cofile_lines_tmp TYPE tr_cofilines,
        lw_cofile_lines_tmp TYPE tstrfcofil.

  CONSTANTS:lc_dir_type TYPE c VALUE 'T' . "Transport

*Begin of insertion T451990
  TYPES: BEGIN OF ty_e070,
          trkorr  TYPE e070-trkorr,
          as4user TYPE e070-as4user,
         END OF ty_e070,
         BEGIN OF ty_e07t,
          trkorr TYPE e07t-trkorr,
          as4text TYPE e07t-as4text,
         END OF ty_e07t.

  DATA:lt_tab_data   TYPE TABLE OF tab512,
       lt_sel        TYPE TABLE OF rfc_db_opt,
       ls_sel_line   LIKE LINE OF lt_sel,
       lt_fields_tab TYPE TABLE OF rfc_db_fld,
       lt_e070_tr    TYPE TABLE OF ty_e070,
       ls_e070_tr    TYPE ty_e070,
       lt_e07t       TYPE TABLE OF ty_e07t,
       ls_e07t       TYPE ty_e07t,
       lv_error(20)  TYPE c,
       lv_low        TYPE sy-datum,
       lv_high       TYPE sy-datum,
       lv_msg        TYPE string,
       lv_tr         TYPE e070-trkorr,
       lv_rest       TYPE string,
       lv_type       TYPE string,
       lv_user       TYPE string,
       lv_date       TYPE string.


  FIELD-SYMBOLS: <lfs_date> LIKE s_date,
                 <lfs_tab_data> LIKE LINE OF lt_tab_data.

  CONSTANTS: lc_k TYPE c VALUE 'K'.

  READ TABLE s_date ASSIGNING <lfs_date>.
  IF sy-subrc EQ 0.
    lv_low = <lfs_date>-low.
    lv_high = <lfs_date>-high.
  ENDIF.
  UNASSIGN <lfs_date>.

  CONCATENATE 'TRFUNCTION = ''' lc_k '''AND' INTO ls_sel_line.
  APPEND ls_sel_line TO lt_sel.
  CLEAR ls_sel_line.
  CONCATENATE 'AS4DATE >= ''' lv_low ''' AND' INTO ls_sel_line.
  APPEND ls_sel_line TO lt_sel.
  CLEAR ls_sel_line.
  CONCATENATE 'AS4DATE <= ''' lv_high '''' INTO ls_sel_line.
  APPEND ls_sel_line TO lt_sel.
  CLEAR ls_sel_line.

  CALL FUNCTION 'RFC_READ_TABLE' DESTINATION 'P1BCLNT010'
    EXPORTING
      query_table          = 'E070'
      delimiter            = space
      no_data              = space
    TABLES
      options              = lt_sel
      fields               = lt_fields_tab
      data                 = lt_tab_data
    EXCEPTIONS
      table_not_available  = 1
      table_without_data   = 2
      option_not_valid     = 3
      field_not_valid      = 4
      not_authorized       = 5
      data_buffer_exceeded = 6
      OTHERS               = 7.

  IF sy-subrc NE 0.
    CLEAR lv_error.
    CASE sy-subrc.
      WHEN 1.
        lv_error = 'TABLE_NOT_AVAILABLE'.
      WHEN 2.
        lv_error = 'TABLE_WITHOUT_DATA'.
      WHEN 3.
        lv_error = 'OPTION_NOT_VALID'.
      WHEN 4.
        lv_error = 'FIELD_NOT_VALID'.
      WHEN 5.
        lv_error = 'NOT_AUTHORIZED'.
      WHEN 6.
        lv_error = 'DATA_BUFFER_EXCEEDED'.
      WHEN OTHERS.
        lv_error = 'OTHERS'.
    ENDCASE.
    CONCATENATE 'RFC_READ_TABLE of E070.'
          'Error:' lv_error INTO lv_msg
          SEPARATED BY space.
    MESSAGE lv_msg TYPE 'E'.
  ENDIF.

  REFRESH lt_e070_tr.
  LOOP AT lt_tab_data ASSIGNING <lfs_tab_data>.
    SPLIT <lfs_tab_data> AT space INTO lv_tr lv_rest.
    CONDENSE lv_rest.
    SPLIT lv_rest AT SPACE INTO lv_type lv_user.
    CONDENSE lv_user.
    lv_user = lv_user+4(7).
    ls_e070_tr-trkorr = lv_tr.
    ls_e070_tr-as4user = lv_user.
    APPEND ls_e070_tr TO lt_e070_tr.
    CLEAR: lv_tr,lv_rest,lv_user,lv_type,ls_e070_tr.
  ENDLOOP.
  UNASSIGN <lfs_tab_data>.
   IF lt_e070_tr[] IS NOT INITIAL.
*End of insertion T451990
******Extracting e070 data************
*  SELECT * FROM e070 INNER JOIN e07t ON e07t~trkorr = e070~trkorr
*    INTO CORRESPONDING FIELDS OF TABLE lt_e070
*                WHERE trfunction = 'K'
*                 AND trstatus = 'R'
*                 AND as4date IN  s_date.
* IF sy-subrc = 0.
*Begin of insertion T451990
     SELECT trkorr as4text FROM e07t
       INTO TABLE lt_e07t
       FOR ALL ENTRIES IN lt_e070_tr
       WHERE trkorr EQ lt_e070_tr-trkorr.
       IF sy-subrc EQ 0.
         SKIP 0.
       ENDIF.
*End of insertion T451990
    SELECT * FROM e071
      INTO TABLE lt_e071
      FOR ALL ENTRIES IN lt_e070_tr
      WHERE trkorr = lt_e070_tr-trkorr
      AND obj_name LIKE 'Z%'.
    IF sy-subrc = 0.
      LOOP AT lt_e071 INTO lw_e071.
        AT NEW trkorr.
          CLEAR gw_final.
          gw_final-transport = lw_e071-trkorr.
          READ TABLE lt_e070_tr INTO ls_e070_tr WITH KEY trkorr = lw_e071-trkorr.
          IF sy-subrc = 0.
            CALL FUNCTION 'STRF_READ_COFILE'
              EXPORTING
                iv_dirtype     = lc_dir_type
                iv_trkorr      = ls_e070_tr-trkorr
              IMPORTING
                ev_cofi_header = ls_cofile_header
                ev_project     = lv_project
              TABLES
                tt_cofi_lines  = lt_cofile_lines_tmp
              EXCEPTIONS
                no_info_found  = 2.
            IF sy-subrc = 0.
              SORT lt_cofile_lines_tmp BY tarsystem.
              CLEAR gv_prod_flag."INS T451990
              LOOP AT lt_cofile_lines_tmp INTO lw_cofile_lines_tmp WHERE tarsystem CP 'P*'."CHG T451990
**T451990
                IF lw_cofile_lines_tmp-function EQ 'I'.
*                  gv_prod_flag = 'X'.
*                ENDIF.
**T451990
*                IF gw_final-moved_time < lw_cofile_lines_tmp-trtime.
                  gw_final-moved_on = lw_cofile_lines_tmp-trdate.
                  gw_final-moved_time = lw_cofile_lines_tmp-trtime.
                ENDIF.
              ENDLOOP.
            ENDIF.
            gw_final-user = ls_e070_tr-as4user.
            READ TABLE lt_e07t INTO ls_e07t WITH KEY trkorr = lw_e071-trkorr."T451990
            IF sy-subrc EQ 0."T451990
              gw_final-description = lw_e070-as4text.
            ENDIF."T451990
          ENDIF.
        ENDAT.
        gw_final-obj_name = lw_e071-obj_name.
        gw_final-obj_type = lw_e071-object.
*        IF gv_prod_flag EQ 'X'. "INS T451990
          APPEND gw_final TO gt_final.
*        ENDIF."INS T451990
      ENDLOOP.
      MOVE gt_final TO gt_transports.
      SORT gt_transports BY moved_on DESCENDING moved_time DESCENDING transport.
      DELETE ADJACENT DUPLICATES FROM gt_transports COMPARING moved_on moved_time transport.
    ENDIF.
  ENDIF.




ENDFORM.                    " FIND_TR

*&---------------------------------------------------------------------*
*&      Form  ALV_SETUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_setup .

  ws_layout-colwidth_optimize = 'X'.

  fieldcatalog-fieldname   = 'TRANSPORT'.
  fieldcatalog-seltext_m   = 'TR'.
  fieldcatalog-outputlen   = 20.
  fieldcatalog-col_pos     = 1.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.
  fieldcatalog-fieldname   = 'OBJ_NAME'.
  fieldcatalog-seltext_m   = 'Object'.
  fieldcatalog-col_pos     = 2.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.
  fieldcatalog-fieldname   = 'OBJ_TYPE'.
  fieldcatalog-seltext_m   = 'Object Type'.
  fieldcatalog-col_pos     = 3.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.
  fieldcatalog-fieldname   = 'MOVED_ON'.
  fieldcatalog-seltext_m   = 'Date'.
  fieldcatalog-col_pos     = 4.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.
  fieldcatalog-fieldname   = 'MOVED_TIME'.
  fieldcatalog-seltext_m   = 'Time'.
  fieldcatalog-col_pos     = 5.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.
  fieldcatalog-fieldname   = 'USER'.
  fieldcatalog-seltext_m   = 'User'.
  fieldcatalog-col_pos     = 6.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.
  fieldcatalog-fieldname   = 'CD'.
  fieldcatalog-seltext_m   = 'CD'.
  fieldcatalog-col_pos     = 7.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.




ENDFORM.                    " ALV_SETUP
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .

  DATA: gd_repid     LIKE sy-repid.
  gd_repid = sy-repid.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = gd_repid
      is_layout          = ws_layout
      it_fieldcat        = fieldcatalog[]
      i_save             = 'X'
    TABLES
      t_outtab           = gt_final
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.                    "display_alv


*----------------------------------------------------------------------*
*       CLASS lcl_source_scan DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_tr_moved DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      on_link_click
            FOR EVENT link_click OF cl_salv_events_hierseq
        IMPORTING
            sender
            level
            row
            column.

ENDCLASS.                    "lcl_source_scan DEFINITION

*----------------------------------------------------------------------*
*       CLASS gcl_tr_moved IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_tr_moved IMPLEMENTATION.
  METHOD on_link_click.

    DATA:
      ls_alv_header     LIKE LINE OF gt_transports,
      ls_alv_item       LIKE LINE OF gt_final,
      ls_key            TYPE e071k,
      ls_request_header TYPE trwbo_request_header.

    CASE level.
      WHEN '1'.
        READ TABLE gt_transports INDEX row INTO gw_transports.
        CHECK sy-subrc IS INITIAL.

*        display_version_management( ls_alv_header ).

      WHEN '2'.
        READ TABLE gt_final INDEX row INTO gw_final.
        CHECK sy-subrc IS INITIAL.

        IF r1 NE 'X'.
          ls_key-trkorr = gw_final-transport.
          ls_key-objname = gw_final-obj_name.
          ls_key-tabkey = gw_final-tabkey.

          CALL FUNCTION 'TRKLE_POPUP_TO_EDIT_TABKEY'
            EXPORTING
              iv_edit_mode      = ''
              is_key            = ls_key
              is_request_header = ls_request_header
*            IMPORTING
*             ev_new_tabkey     = lv_new_tabkey
*             ev_new_tabkey_str = lv_new_tabkey_str
*             ev_new_key_lens   = lv_key_lens
*             ev_user_navigation = lv_user_navigation
            EXCEPTIONS
              user_canceled     = 1
              OTHERS            = 1.
*   Leave DO-loop, if user cancelled. Take over changes into glob.tabs
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
        ELSE.

          IF gw_final-obj_type = 'REPS' .

            CALL FUNCTION 'EDITOR_PROGRAM'
              EXPORTING
                appid   = 'PG'
                display = 'X'
                program = gw_final-obj_name
                line    = '1'
                topline = '1'
              EXCEPTIONS
                OTHERS  = 1.

          ENDIF.
*         Call screen painter
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "on_link_click

ENDCLASS.                    "gcl_tr_moved IMPLEMENTATION
*

*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display .

  DATA:
    go_alv   TYPE REF TO gcl_tr_moved.
  CREATE OBJECT go_alv.

  DATA:
       lo_alv        TYPE REF TO cl_salv_hierseq_table.

  "ALV INstance

  DATA:
    lt_alv_bind TYPE salv_t_hierseq_binding,
    ls_alv_bind LIKE LINE OF lt_alv_bind.

  DATA:
    lo_layout    TYPE REF TO cl_salv_layout,
    lo_events    TYPE REF TO cl_salv_events_hierseq,
    lo_functions TYPE REF TO cl_salv_functions_list,
    lo_level     TYPE REF TO cl_salv_hierseq_level,
    lo_column    TYPE REF TO cl_salv_column_hierseq,
    lo_columns   TYPE REF TO cl_salv_columns_hierseq,
    lt_columns   TYPE salv_t_column_ref,
    ls_columns   LIKE LINE OF lt_columns,
    lo_settings  TYPE REF TO cl_salv_display_settings,
    lv_title     TYPE lvc_title,
    lv_hits      TYPE lvc_title,
    ls_color     TYPE lvc_s_colo,
    ls_layout    TYPE salv_s_layout_key,
    lt_functions TYPE salv_t_ui_func.

  CONSTANTS: gc_x TYPE c VALUE 'X' .

  ls_alv_bind-master = ls_alv_bind-slave = 'TRANSPORT'.
  APPEND ls_alv_bind TO lt_alv_bind.

  TRY.
      CALL METHOD cl_salv_hierseq_table=>factory
        EXPORTING
          t_binding_level1_level2 = lt_alv_bind
        IMPORTING
          r_hierseq               = lo_alv
        CHANGING
          t_table_level1          = gt_transports
          t_table_level2          = gt_final.

    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found.
  ENDTRY.

*   Layout
  ls_layout-report = sy-repid.
  ls_layout-handle = 'SCAN'.

  lo_layout = lo_alv->get_layout( ).
  lo_layout->set_key( ls_layout ).
  lo_layout->set_save_restriction( ).

*   Function keys/buttons
  lo_functions = lo_alv->get_functions( ).
  lo_functions->set_all( gc_x ).

*   exclude the following functions (column paging buttons)
  lt_functions = lo_functions->get_functions( ).

  TRY.
*    lo_functions->remove_function( '&CRB' ).
*    lo_functions->remove_function( '&CRL' ).
*    lo_functions->remove_function( '&CRR' ).
*    lo_functions->remove_function( '&CRE' ).
    CATCH cx_salv_wrong_call
          cx_salv_not_found.
  ENDTRY.
*   Display settings
  lo_settings = lo_alv->get_display_settings( ).

**   Title
*    lv_hits = gv_hit_count.
*    SHIFT lv_hits LEFT DELETING LEADING space.
*
*    CONCATENATE lv_hits
*                'Treffer'(001)
*                INTO lv_hits SEPARATED BY space.

*   Event handling
  lo_events = lo_alv->get_event( ).
  SET HANDLER go_alv->on_link_click FOR lo_events.

*   Field catalog
  TRY.
*       Field catalog/columns - header table
      lo_columns  = lo_alv->get_columns( '1' ).
      lt_columns = lo_columns->get( ).

      TRY.
          lo_columns->set_expand_column( 'EXPAND' ).

          lo_level = lo_alv->get_level( '1' ).
          lo_level->set_items_expanded( gc_x ).

        CATCH cx_salv_data_error.
      ENDTRY.

      LOOP AT lt_columns INTO ls_columns.
        CASE ls_columns-columnname.
          WHEN 'EXPAND'.
            ls_columns-r_column->set_technical( ).

*          WHEN 'DYNNR'.
*            IF gv_dynp_found IS INITIAL.
*              ls_columns-r_column->set_technical( ).
*            ELSE.
*              ls_columns-r_column->set_output_length( '15' ).
*            ENDIF.
*
*          WHEN 'VERSNO'.
*            IF gv_vers_found IS INITIAL.
*              ls_columns-r_column->set_technical( ).
*            ELSE.
*              ls_columns-r_column->set_leading_zero( gc_x ).
*              ls_columns-r_column->set_output_length( '15' ).
*              TRY.
*                  lo_column ?= ls_columns-r_column.
*                  lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*                CATCH cx_sy_move_cast_error.
*              ENDTRY.
*            ENDIF.
        ENDCASE.
      ENDLOOP.

*       Field catalog/columns - item table
      lo_columns = lo_alv->get_columns( '2' ).

      TRY.
          lo_columns->set_color_column( 'CELL_COLOR' ).
        CATCH cx_salv_data_error.
      ENDTRY.

      lt_columns = lo_columns->get( ).

      LOOP AT lt_columns INTO ls_columns.
        CASE ls_columns-columnname.
          WHEN 'TRANSPORT'.
            ls_columns-r_column->set_technical( ).

          WHEN 'MOVED_ON'.
            ls_columns-r_column->set_technical( ).

          WHEN 'MOVED_TIME'.
            ls_columns-r_column->set_technical( ).

          WHEN 'USER'.
            ls_columns-r_column->set_technical( ).

          WHEN 'DESCRIPTION'.
            ls_columns-r_column->set_technical( ).
          WHEN 'TROBJ_NAME'.
            IF r1 = 'X'.
              ls_columns-r_column->set_technical( ).
            ENDIF.

          WHEN 'OBJ_NAME'.
            TRY.
                lo_column ?= ls_columns-r_column.
                lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
              CATCH cx_sy_move_cast_error.
            ENDTRY.

*          WHEN 'OBJ_TYPE'.
*            ls_columns-r_column->set_technical( ).
*
*          WHEN 'moved_on'.
*            ls_columns-r_column->set_technical( ).
*
*          WHEN 'HIT'.
*            ls_columns-r_column->set_technical( ).
*
*          WHEN 'LINE_NO'.
*            ls_color-col = '4'.
*            TRY.
*                lo_column ?= ls_columns-r_column.
*                lo_column->set_color( ls_color ).
*                lo_column->set_leading_zero( gc_x ).
*              CATCH cx_sy_move_cast_error.
*            ENDTRY.
*
*          WHEN 'TEXT'.
*            TRY.
*                lo_column ?= ls_columns-r_column.
*                lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*              CATCH cx_sy_move_cast_error.
*            ENDTRY.

        ENDCASE.
      ENDLOOP.
    CATCH cx_salv_not_found.
  ENDTRY.

  lo_alv->display( ).


ENDFORM.                    " DISPLAY
*&---------------------------------------------------------------------*
*&      Form  GET_ENTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_entry .

  TYPES: BEGIN OF pt_e070.
          INCLUDE TYPE e070.
  TYPES: as4text TYPE as4text.
  TYPES:  END OF pt_e070.

  DATA: lt_e070             TYPE STANDARD TABLE OF pt_e070,
        lw_e070             TYPE pt_e070,
        lt_e071             TYPE STANDARD TABLE OF e071k,
        lw_e071             TYPE e071k,
        ls_cofile_header    TYPE tstrfcofih,
        lv_project          TYPE trkorr_p,
        lt_cofile_lines_tmp TYPE tr_cofilines,
        lw_cofile_lines_tmp TYPE tstrfcofil..

  CONSTANTS:lc_dir_type TYPE c VALUE 'T' . "Transport
*Begin of insertion T451990
  TYPES: BEGIN OF ty_e070,
          trkorr TYPE e070-trkorr,
          as4user TYPE e070-as4user,
         END OF ty_e070,
         BEGIN OF ty_e07t,
          trkorr TYPE e07t-trkorr,
          as4text TYPE e07t-as4text,
         END OF ty_e07t.

  DATA:lt_tab_data   TYPE TABLE OF tab512,
       lt_sel        TYPE TABLE OF rfc_db_opt,
       ls_sel_line   LIKE LINE OF lt_sel,
       lt_fields_tab TYPE TABLE OF rfc_db_fld,
       lt_e070_tr    TYPE TABLE OF ty_e070,
       ls_e070_tr    TYPE ty_e070,
       lt_e07t       TYPE TABLE OF ty_e07t,
       ls_e07t       TYPE ty_e07t,
       lv_error(20)  TYPE c,
       lv_low        TYPE sy-datum,
       lv_high       TYPE sy-datum,
       lv_msg        TYPE string,
       lv_tr         TYPE e070-trkorr,
       lv_rest       TYPE string,
       lv_type       TYPE string,
       lv_user       TYPE string,
       lv_date       TYPE string.


  FIELD-SYMBOLS: <lfs_date> LIKE s_date,
                 <lfs_tab_data> LIKE LINE OF lt_tab_data.

  CONSTANTS: lc_w TYPE c VALUE 'W'.

  READ TABLE s_date ASSIGNING <lfs_date>.
  IF sy-subrc EQ 0.
    lv_low = <lfs_date>-low.
    lv_high = <lfs_date>-high.
  ENDIF.
  UNASSIGN <lfs_date>.

  CONCATENATE 'TRFUNCTION = ''' lc_w '''AND' INTO ls_sel_line.
  APPEND ls_sel_line TO lt_sel.
  CLEAR ls_sel_line.
  CONCATENATE 'AS4DATE >= ''' lv_low ''' AND' INTO ls_sel_line.
  APPEND ls_sel_line TO lt_sel.
  CLEAR ls_sel_line.
  CONCATENATE 'AS4DATE <= ''' lv_high '''' INTO ls_sel_line.
  APPEND ls_sel_line TO lt_sel.
  CLEAR ls_sel_line.

  CALL FUNCTION 'RFC_READ_TABLE' DESTINATION 'P1BCLNT010'
    EXPORTING
      query_table          = 'E070'
      delimiter            = space
      no_data              = space
    TABLES
      options              = lt_sel
      fields               = lt_fields_tab
      data                 = lt_tab_data
    EXCEPTIONS
      table_not_available  = 1
      table_without_data   = 2
      option_not_valid     = 3
      field_not_valid      = 4
      not_authorized       = 5
      data_buffer_exceeded = 6
      OTHERS               = 7.

  IF sy-subrc NE 0.
    CLEAR lv_error.
    CASE sy-subrc.
      WHEN 1.
        lv_error = 'TABLE_NOT_AVAILABLE'.
      WHEN 2.
        lv_error = 'TABLE_WITHOUT_DATA'.
      WHEN 3.
        lv_error = 'OPTION_NOT_VALID'.
      WHEN 4.
        lv_error = 'FIELD_NOT_VALID'.
      WHEN 5.
        lv_error = 'NOT_AUTHORIZED'.
      WHEN 6.
        lv_error = 'DATA_BUFFER_EXCEEDED'.
      WHEN OTHERS.
        lv_error = 'OTHERS'.
    ENDCASE.
    CONCATENATE 'RFC_READ_TABLE of E070.'
          'Error:' lv_error INTO lv_msg
          SEPARATED BY space.
    MESSAGE lv_msg TYPE 'E'.
  ENDIF.

  REFRESH lt_e070_tr.
  LOOP AT lt_tab_data ASSIGNING <lfs_tab_data>.
    SPLIT <lfs_tab_data> AT space INTO lv_tr lv_rest.
    CONDENSE lv_rest.
    SPLIT lv_rest AT SPACE INTO lv_type lv_user.
    CONDENSE lv_user.
    lv_user = lv_user+4(7).
    ls_e070_tr-trkorr = lv_tr.
    ls_e070_tr-as4user = lv_user.
    APPEND ls_e070_tr TO lt_e070_tr.
    CLEAR: lv_tr,lv_rest,lv_user,lv_type,ls_e070_tr.
  ENDLOOP.
  UNASSIGN <lfs_tab_data>.
  IF lt_e070_tr[] IS NOT INITIAL.
*End of insertion T451990
******Extracting e070 data************
*  SELECT * FROM e070 INNER JOIN e07t ON e07t~trkorr = e070~trkorr
*    INTO CORRESPONDING FIELDS OF TABLE lt_e070
*                WHERE trfunction = 'W'
*                 AND trstatus = 'R'
*                 AND as4date IN  s_date.

*  IF sy-subrc = 0.
*Begin of insertion T451990
     SELECT trkorr as4text FROM e07t
       INTO TABLE lt_e07t
       FOR ALL ENTRIES IN lt_e070_tr
       WHERE trkorr EQ lt_e070_tr-trkorr.
       IF sy-subrc EQ 0.
         SKIP 0.
       ENDIF.
*End of insertion T451990
    SELECT * FROM e071k
      INTO TABLE lt_e071
      FOR ALL ENTRIES IN lt_e070_tr
      WHERE trkorr = lt_e070_tr-trkorr
      AND objname LIKE 'Z%'.
    IF sy-subrc = 0.
      LOOP AT lt_e071 INTO lw_e071.
        AT NEW trkorr.
          CLEAR gw_final.
          gw_final-transport = lw_e071-trkorr.
          READ TABLE lt_e070_tr INTO ls_e070_tr WITH KEY trkorr = lw_e071-trkorr.
          IF sy-subrc = 0.
            CALL FUNCTION 'STRF_READ_COFILE'
              EXPORTING
                iv_dirtype     = lc_dir_type
                iv_trkorr      = lw_e070-trkorr
              IMPORTING
                ev_cofi_header = ls_cofile_header
                ev_project     = lv_project
              TABLES
                tt_cofi_lines  = lt_cofile_lines_tmp
              EXCEPTIONS
                no_info_found  = 2.
            IF sy-subrc = 0.
              SORT lt_cofile_lines_tmp BY tarsystem.
              CLEAR gv_prod_flag."INS T451990
              LOOP AT lt_cofile_lines_tmp INTO lw_cofile_lines_tmp WHERE tarsystem CP 'P*'."CHG T451990
**                T451990
                IF lw_cofile_lines_tmp-function EQ 'I'.
*                  gv_prod_flag = 'X'.
*                ENDIF.
**                T451990
*                IF gw_final-moved_time < lw_cofile_lines_tmp-trtime.
                  gw_final-moved_on = lw_cofile_lines_tmp-trdate.
                  gw_final-moved_time = lw_cofile_lines_tmp-trtime.
                ENDIF.
              ENDLOOP.
            ENDIF.
            gw_final-user = ls_e070_tr-as4user.
            READ TABLE lt_e07t INTO ls_e07t WITH KEY trkorr = lw_e071-trkorr."T451990
            IF sy-subrc EQ 0."T451990
              gw_final-description = lw_e070-as4text.
            ENDIF."T451990
          ENDIF.
        ENDAT.
        gw_final-obj_name = lw_e071-objname.
        gw_final-tabkey = lw_e071-tabkey.
*        IF gv_prod_flag EQ 'X'."INS T451990
          APPEND gw_final TO gt_final.
*        ENDIF."INS T451990
      ENDLOOP.

      MOVE gt_final TO gt_transports.
      SORT gt_transports BY moved_on DESCENDING moved_time DESCENDING transport.
      DELETE ADJACENT DUPLICATES FROM gt_transports COMPARING moved_on moved_time transport.

    ENDIF.
  ENDIF.

ENDFORM.                    " GET_ENTRY
