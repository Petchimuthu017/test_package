*&---------------------------------------------------------------------*
*&  Include           ZRECENT_TR_MOVED_TOP
*&---------------------------------------------------------------------*

*****Tables declaration********
TABLES:e070,tadir,vrsd.

TYPES:BEGIN OF ty_final,
  transport  TYPE trkorr,
  moved_on TYPE datum,
  moved_time TYPE as4time,
  user TYPE as4text,
  description TYPE char50,
  obj_name TYPE trobj_name,
  obj_type TYPE trobjtype,
  tabkey TYPE trobj_name,
  END OF ty_final,
  BEGIN OF ty_transports,
    transport TYPE trkorr,
    moved_on TYPE datum,
    moved_time TYPE as4time,
    description TYPE char50,
    user TYPE as4text,
    expand  TYPE xfeld,
    END OF ty_transports.

DATA:gt_final TYPE STANDARD TABLE OF ty_final,
     gw_final TYPE ty_final,
     gt_transports TYPE STANDARD TABLE OF ty_transports,
     gw_transports TYPE ty_transports,
     gt_fieldcat TYPE slis_t_fieldcat_alv,
     gw_fieldcat TYPE slis_fieldcat_alv,
     fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE.

DATA: gv_prod_flag TYPE c. "INS FFDK965128 T451990

DATA: ws_layout TYPE slis_layout_alv.

DATA: alv_fieldcat TYPE slis_t_fieldcat_alv,
           alv_layout   TYPE slis_layout_alv,
           gt_xevents TYPE slis_t_event,
           gt_print TYPE slis_print_alv,
           gd_repid     LIKE sy-repid.

DATA:      l_wa TYPE slis_fieldcat_alv,
           l_wa1 TYPE slis_fieldcat_alv,
           l_wa2 TYPE slis_fieldcat_alv.

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-s01.

SELECT-OPTIONS: s_date FOR e070-as4date NO-EXTENSION OBLIGATORY.


PARAMETERS:
r1 RADIOBUTTON GROUP rad1 DEFAULT 'X'USER-COMMAND flg,
r2 RADIOBUTTON GROUP rad1.

SELECTION-SCREEN END OF BLOCK bl1.
