*&---------------------------------------------------------------------*
*& Report ZCALISMA3
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_calisma3.
TYPES BEGIN OF ty_test.
TYPES bukrs TYPE t001-bukrs.
TYPES waers TYPE t001-waers.
INCLUDE TYPE spflı.
TYPES END OF ty_test.
TYPES tt_test TYPE TABLE OF ty_test.


DATA:
  lv_spflı  TYPE TABLE OF spflı,
  gs_spflı  TYPE spflı,
  gr_carrid TYPE RANGE OF spflı-carrid,
  gs_carrid LIKE LINE OF gr_carrid.

DATA: wa_test  TYPE  ty_test,
      it_test  LIKE TABLE OF wa_test,
      it_test2 TYPE tt_test,
      it_test3 TYPE TABLE OF ty_test.

RANGES: gr_connid FOR spflı-connid.



SELECTION-SCREEN BEGIN OF BLOCK b1.
SELECT-OPTIONS: airline FOR gs_spfli-carrid,
                flight_n FOR gs_spfli-connid,
                dep FOR gs_spfli-cityfrom,
                country FOR gs_spfli-countryto.
PARAMETERS: p_ch  AS CHECKBOX,
            p_ch1 AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b1.




START-OF-SELECTION.

  PERFORM start_of_selection.

END-OF-SELECTION.

  PERFORM end_of_selection.

*&---------------------------------------------------------------------*
*& Form START_OF_SELECTION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM start_of_selection .





  IF p_ch = 'X'.
    gr_connid-sign = 'I'.
    gr_connid-option = 'BT'.
    gr_connid-low = 0 .
    gr_connid-high = 1000.
    APPEND gr_connid .
  ELSEIF p_ch1 = 'X'.
    gs_carrid-sign = 'I'.
    gs_carrid-option = 'BT'.
    gs_carrid-high = 'DZ'.
    gs_carrid-low = 'AA'.
    APPEND gs_carrid TO gr_carrid.

  ENDIF.

*  APPEND gr_connid .
*  APPEND gs_carrid TO gr_carrid.


  SELECT * FROM spflı INTO TABLE lv_spfli
    WHERE carrid IN airline
    AND carrid IN gr_carrid
    AND connid IN flight_n
    AND connid IN gr_connid
    AND cityfrom IN dep
  AND countryto IN country.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form END_OF_SELECTION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM end_of_selection .

*  PERFORM ekrana_yazdır_write.
  PERFORM fonksiyon_yazdır.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EKRANA_YAZDIR_WRITE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ekrana_yazdir_write .

  WRITE : 'Airline',
      'Flight no.',
      'Country key',
      'Departure city',
      'Departure airport',
      'Country key',
      'Arrival city',
      'Target',
      'Flight time',
      'Departure time',
      'Arrival time',
      'Distance',
      'Distance in',
      'Charter flt',
      'Day(s)'.


  LOOP AT lv_spfli INTO gs_spfli.
    WRITE: / ,gs_spflı-carrid,
    13 gs_spflı-connid,
    25 gs_spflı-countryfr,
    32 gs_spflı-cityfrom,
    50 gs_spflı-airpfrom,
    68 gs_spflı-countryto,
    80 gs_spflı-cityto,
    95 gs_spflı-airpto,
    103 gs_spflı-fltime,
    115 gs_spflı-deptime,
    125 gs_spflı-arrtime,
    140 gs_spflı-distance,
    150 gs_spflı-distid,
    160 gs_spflı-fltype,
    170 gs_spflı-period.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FONKSIYON_YAZDIR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fonksiyon_yazdir.

*  CALL FUNCTION 'ZFK_FM_YAZDIR'
*  TABLES t_spfli = lv_spfli.

*CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
** EXPORTING
*   I_INTERFACE_CHECK              = ' '
*   I_BYPASSING_BUFFER             =
*   I_BUFFER_ACTIVE                = ' '
*   I_CALLBACK_PROGRAM             = ' '
*   I_CALLBACK_PF_STATUS_SET       = ' '
*   I_CALLBACK_USER_COMMAND        = ' '
*   I_STRUCTURE_NAME               = 'SPFLI'
*   IS_LAYOUT                      =
*   IT_FIELDCAT                    =
*   IT_EXCLUDING                   =
*   IT_SPECIAL_GROUPS              =
*   IT_SORT                        =
*   IT_FILTER                      =
*   IS_SEL_HIDE                    =
*   I_DEFAULT                      = 'X'
*   I_SAVE                         = ' '
*   IS_VARIANT                     =
*   IT_EVENTS                      =
*   IT_EVENT_EXIT                  =
*   IS_PRINT                       =
*   IS_REPREP_ID                   =
*   I_SCREEN_START_COLUMN          = 0
*   I_SCREEN_START_LINE            = 0
*   I_SCREEN_END_COLUMN            = 0
*   I_SCREEN_END_LINE              = 0
*   IR_SALV_LIST_ADAPTER           =
*   IT_EXCEPT_QINFO                =
*   I_SUPPRESS_EMPTY_DATA          = ABAP_FALSE
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
*  TABLES
*    t_outtab                       = lv_spfli
* EXCEPTIONS
*   PROGRAM_ERROR                  = 1
*   OTHERS                         = 2
*          .
*IF sy-subrc <> 0.
* Implement suitable error handling here
*ENDIF.

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
 EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
*   I_CALLBACK_PROGRAM                = ' '
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
   I_STRUCTURE_NAME                  = 'SPFLI'
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
*   IS_LAYOUT                         =
*   IT_FIELDCAT                       =
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           =
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
*   I_SAVE                            = ' '
*   IS_VARIANT                        =
*   IT_EVENTS                         =
*   IT_EVENT_EXIT                     =
*   IS_PRINT                          =
*   IS_REPREP_ID                      =
*   I_SCREEN_START_COLUMN             = 0
*   I_SCREEN_START_LINE               = 0
*   I_SCREEN_END_COLUMN               = 0
*   I_SCREEN_END_LINE                 = 0
*   I_HTML_HEIGHT_TOP                 = 0
*   I_HTML_HEIGHT_END                 = 0
*   IT_ALV_GRAPHICS                   =
*   IT_HYPERLINK                      =
*   IT_ADD_FIELDCAT                   =
*   IT_EXCEPT_QINFO                   =
*   IR_SALV_FULLSCREEN_ADAPTER        =
*   O_PREVIOUS_SRAL_HANDLER           =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
  TABLES
    t_outtab                          = lv_spfli
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
          .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.




ENDFORM.
