*%26---------------------------------------------------------------------*
*%26 Report ZTR_TEST2
*%26---------------------------------------------------------------------*
*%26
*%26---------------------------------------------------------------------*
REPORT ZTR_TEST2.

*** DEMo Test
** Bug Fix Branch code
WRITE: /'version2'.
WRITE : /'Version3'.  " INS TEST1
WRITE:/'Version4'.
WRITE:/'Version6'.
WRITE: /'Version5'. " NEW INS

INCLUDE ZTR_TEST2_TOP.
