*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 28.10.2006 at 17:34:12 by user U255290
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZFI_COGS_DETAILS................................*
DATA:  BEGIN OF STATUS_ZFI_COGS_DETAILS              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_COGS_DETAILS              .
CONTROLS: TCTRL_ZFI_COGS_DETAILS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFI_COGS_DETAILS              .
TABLES: ZFI_COGS_DETAILS               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
