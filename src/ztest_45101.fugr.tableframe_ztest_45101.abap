*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTEST_45101
*   generation date: 28.10.2006 at 17:34:12 by user U255290
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTEST_45101        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
