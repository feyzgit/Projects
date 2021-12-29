*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFK_T_ISKONTO...................................*
DATA:  BEGIN OF STATUS_ZFK_T_ISKONTO                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFK_T_ISKONTO                 .
CONTROLS: TCTRL_ZFK_T_ISKONTO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFK_T_ISKONTO                 .
TABLES: ZFK_T_ISKONTO                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
