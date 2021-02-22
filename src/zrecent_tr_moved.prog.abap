*&---------------------------------------------------------------------*
*& Report  ZRECENT_TR_MOVED
*&
*&---------------------------------------------------------------------*
*& The objective of this development is to find all the TR moved to
*& production for this system for a range of dates.
*&---------------------------------------------------------------------*

REPORT zrecent_tr_moved.

INCLUDE zrecent_tr_moved_top.
INCLUDE zrecent_tr_moved_f01.

INITIALIZATION.

  PERFORM default_option.

START-OF-SELECTION.



  IF r1 = 'X'.
    PERFORM get_data.
  ELSE.
    PERFORM get_entry.
  ENDIF.

END-OF-SELECTION.

    PERFORM display.
