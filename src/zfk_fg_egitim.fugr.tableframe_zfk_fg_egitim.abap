*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFK_FG_EGITIM
*   generation date: 08.12.2021 at 17:24:47
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFK_FG_EGITIM      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
