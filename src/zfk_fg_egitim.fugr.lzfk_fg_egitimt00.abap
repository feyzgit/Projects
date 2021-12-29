*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFK_T_SIRKET....................................*
DATA:  BEGIN OF STATUS_ZFK_T_SIRKET                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFK_T_SIRKET                  .
CONTROLS: TCTRL_ZFK_T_SIRKET
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZFK_T_TAKVIM....................................*
DATA:  BEGIN OF STATUS_ZFK_T_TAKVIM                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFK_T_TAKVIM                  .
CONTROLS: TCTRL_ZFK_T_TAKVIM
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: *ZFK_T_SIRKET                  .
TABLES: *ZFK_T_TAKVIM                  .
TABLES: ZFK_T_SIRKET                   .
TABLES: ZFK_T_TAKVIM                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
