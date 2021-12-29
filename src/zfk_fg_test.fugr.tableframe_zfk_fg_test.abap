*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFK_FG_TEST
*   generation date: 02.11.2021 at 11:15:59
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFK_FG_TEST        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
