*&---------------------------------------------------------------------*
*&  Include           ZCO_DOSYA_KAPAMA_KONTROL_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization .

  CLEAR : d_ok,
          gt_fieldcat[],
          gt_report[],
          gt_events[],
          gt_excluding[],
          gs_layout.

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
FORM at_selection_screen_output .

ENDFORM.                    " AT_SELECTION_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM at_selection_screen .

ENDFORM.                    " AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  MAIN_SEL
*&---------------------------------------------------------------------*
FORM main_sel .

  PERFORM get_data.

  PERFORM process_data.

  CHECK gt_report[] IS NOT INITIAL.

  gv_count = lines( gt_report[] ).
  d_ok = 'X'.

ENDFORM.                    " MAIN_SEL
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM get_data.

  TYPES: BEGIN OF ty_vbrp,
    bukrs TYPE vbrk-bukrs,
    fkdat TYPE vbrk-fkdat,
    vbeln TYPE vbrp-vbeln,
    posnr TYPE vbrp-posnr,
    aubel TYPE vbrp-aubel,
    aupos TYPE vbrp-aupos,
    matnr TYPE vbrp-matnr,
    werks TYPE vbrp-werks,
    fkimg TYPE vbrp-fkimg,
    deger TYPE char1,
  END OF ty_vbrp.

  TYPES: BEGIN OF ty_afpo,
    aufnr TYPE afpo-aufnr,
    matnr TYPE afpo-matnr,
    pwerk TYPE afpo-pwerk,
    wemng TYPE afpo-wemng,
    idat2 TYPE aufk-idat2,
    kdauf TYPE aufk-kdauf,
    kdpos TYPE aufk-kdpos,
    deger TYPE char1,
  END OF ty_afpo.

  TYPES: BEGIN OF ty_ckmlpp,
    aubel TYPE ckmlhd-vbeln,
    lbkum TYPE ckmlpp-lbkum,
  END OF ty_ckmlpp.


  DATA: lt_vbrp TYPE STANDARD TABLE OF ty_vbrp WITH HEADER LINE,
        lt_afpo TYPE STANDARD TABLE OF ty_afpo WITH HEADER LINE,
        lt_ckmlpp TYPE STANDARD TABLE OF ty_ckmlpp WITH HEADER LINE.

  CLEAR : gt_report[], gt_report.


  SELECT a~fkdat b~vbeln b~aubel b~aupos b~matnr b~werks
    SUM( b~fkimg ) AS fkimg
      FROM vbrk AS a
        INNER JOIN vbrp AS b ON a~vbeln = b~vbeln
          INTO CORRESPONDING FIELDS OF TABLE lt_vbrp
            WHERE a~fkdat IN s_fkdat
              AND a~bukrs EQ p_bukrs
*              AND b~aubel EQ '1200002329'
              GROUP BY  a~fkdat b~vbeln b~aubel b~aupos b~matnr b~werks.

  IF NOT lt_vbrp[] IS INITIAL.
    SELECT DISTINCT b~vbeln AS aubel
      FROM ckmlpp AS a
        INNER JOIN ckmlhd AS b ON b~kalnr = a~kalnr
          INTO CORRESPONDING FIELDS OF TABLE lt_ckmlpp
            FOR ALL ENTRIES IN lt_vbrp
              WHERE b~vbeln EQ lt_vbrp-aubel
                AND a~lbkum <> 0.
    SORT lt_ckmlpp BY aubel.
  ENDIF.

  IF NOT lt_vbrp[] IS  INITIAL.
    SELECT a~aufnr a~matnr a~pwerk a~wemng b~idat2 b~kdauf b~kdpos
           b~idat2 c~sobkz
      FROM afpo AS a
       INNER JOIN aufk AS b ON a~aufnr = b~aufnr
        INNER JOIN ckmlhd AS c ON c~matnr EQ a~matnr
          INTO CORRESPONDING FIELDS OF TABLE lt_afpo
            FOR ALL ENTRIES IN lt_vbrp
              WHERE a~matnr IN s_matnr
                AND a~pwerk EQ p_werks
                AND a~kdauf EQ lt_vbrp-aubel
                AND c~abrechdat IN s_fkdat.
  ENDIF.

  LOOP AT lt_vbrp REFERENCE INTO DATA(r_vbrp).
    APPEND INITIAL LINE TO gt_report REFERENCE INTO DATA(r_report).
    r_report->fkdat = r_vbrp->fkdat.
    r_report->aubel = r_vbrp->aubel.
    r_report->aupos = r_vbrp->aupos.
    r_report->fkimg = r_vbrp->fkimg.
    r_report->matnr = r_vbrp->matnr.

    READ TABLE lt_ckmlpp WITH KEY aubel = r_vbrp->aubel
                         TRANSPORTING NO FIELDS BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      r_report->deger = 'X'.
    ENDIF.

    LOOP AT lt_afpo REFERENCE INTO DATA(r_afpo)
                    WHERE kdauf = r_vbrp->aubel
                      AND kdpos = r_vbrp->aupos.
      r_report->kdpos = r_afpo->kdpos.
      r_report->aufnr = r_afpo->aufnr.
      r_report->wemng = r_afpo->wemng.
      r_report->idat2 = r_afpo->idat2.
    ENDLOOP.

    LOOP AT lt_afpo REFERENCE INTO r_afpo
                    WHERE kdauf = r_vbrp->aubel
                      AND kdpos <> r_vbrp->aupos.
      APPEND INITIAL LINE TO gt_report REFERENCE INTO DATA(r_report2).
      r_report2->kdpos = r_afpo->kdpos.
      r_report2->aufnr = r_afpo->aufnr.
      r_report2->wemng = r_afpo->wemng.
      r_report2->idat2 = r_afpo->idat2.
      r_report2->matnr = r_afpo->matnr.
      r_report2->deger = r_report->deger.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
FORM process_data .

ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  REPORT
*&---------------------------------------------------------------------*
FORM report  TABLES  p_table    TYPE STANDARD TABLE
             USING   p_tabname  TYPE tabname.

  gv_count = lines( p_table[] ).
  PERFORM list_layout_specification.
  PERFORM create_field_catalog      TABLES p_table .
  PERFORM build_header.
  PERFORM build_eventtab.
  PERFORM call_alv_display          TABLES p_table .

ENDFORM.                    " REPORT
*&---------------------------------------------------------------------*
*&      Form  LIST_LAYOUT_SPECIFICATION
*&---------------------------------------------------------------------*
FORM list_layout_specification.

  CLEAR gs_layout.

  gs_layout-box_fname   = 'SELKZ'.
  gs_layout-cwidth_opt  = 'X'.
  gs_layout-zebra       = 'X'.
*  gs_layout-stylefname  = 'STYLE'.

ENDFORM.                    " LIST_LAYOUT_SPECIFICATION
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATALOG
*&---------------------------------------------------------------------*
FORM create_field_catalog  TABLES   p_table    TYPE STANDARD TABLE .

  DATA :   lv_dtext     TYPE scrtext_l.

  DATA:
    lo_columns      TYPE REF TO cl_salv_columns_table,
    lo_aggregations TYPE REF TO cl_salv_aggregations,
    lo_salv_table   TYPE REF TO cl_salv_table,
    lr_table        TYPE REF TO data.
  FIELD-SYMBOLS:
    <table>         TYPE STANDARD TABLE.


* create unprotected table from import data
  CREATE DATA lr_table LIKE p_table[].
  ASSIGN lr_table->* TO <table>.


*...New ALV Instance ...............................................
  TRY.
      cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false
        IMPORTING
          r_salv_table = lo_salv_table
        CHANGING
          t_table      = <table> ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

  lo_columns      = lo_salv_table->get_columns( ).
  lo_aggregations = lo_salv_table->get_aggregations( ).
  gt_fieldcat[]   = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                          r_columns             = lo_columns
                          r_aggregations        = lo_aggregations ).


  LOOP AT gt_fieldcat INTO gs_fieldcat.
    CLEAR lv_dtext.

    CASE gs_fieldcat-fieldname.
      WHEN 'SELKZ'.
        gs_fieldcat-tech = 'X'.
      WHEN 'ICON'.
        gs_fieldcat-icon = 'X'.
        lv_dtext = 'Statu'.
      WHEN 'STYLE'.
        gs_fieldcat-tech = 'X'.
      WHEN 'INFO'.
        gs_fieldcat-tech = 'X'.
    ENDCASE.

    IF lv_dtext IS NOT INITIAL.
      MOVE lv_dtext TO : gs_fieldcat-scrtext_l,
                         gs_fieldcat-scrtext_m,
                         gs_fieldcat-scrtext_s,
                         gs_fieldcat-coltext.
    ENDIF.

    MODIFY gt_fieldcat FROM gs_fieldcat.
  ENDLOOP.


ENDFORM.                    " CREATE_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_DISPLAY
*&---------------------------------------------------------------------*
FORM call_alv_display  TABLES   p_table TYPE STANDARD TABLE.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_bypassing_buffer       = 'X'
      i_callback_program       = sy-cprog
*     i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_top_of_page   = formname_top_of_page
      is_layout_lvc            = gs_layout
      it_fieldcat_lvc          = gt_fieldcat[]
      it_excluding             = gt_excluding[]
      i_default                = 'X'
      i_save                   = 'X'
*     IS_VARIANT               =
      it_events                = gt_events[]
    TABLES
      t_outtab                 = p_table[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.


  IF sy-subrc <> 0.

    CHECK sy-msgty IS NOT INITIAL .
    MESSAGE ID sy-msgid
            TYPE sy-msgty
            NUMBER sy-msgno
            WITH sy-msgv1
                 sy-msgv2
                 sy-msgv3
                 sy-msgv4.

  ENDIF.


ENDFORM.                    " CALL_ALV_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*      -->v_ucomm  LIKE sy-ucomm
*      -->v_selfld TYPE slis_selfield
*----------------------------------------------------------------------*
FORM user_command USING v_ucomm  LIKE sy-ucomm
                        v_selfld TYPE slis_selfield.

  PERFORM check_data_changed.
  v_selfld-refresh = 'X'.

  CLEAR : gs_report.
  READ TABLE gt_report INTO gs_report INDEX v_selfld-tabindex.
  CASE v_ucomm.
    WHEN '&LOG'.
      PERFORM display_log.

  ENDCASE .

ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM check_data_changed .

  DATA : lo_grid  TYPE REF TO cl_gui_alv_grid,
         lv_valid .

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lo_grid.

  CALL METHOD lo_grid->check_changed_data
    IMPORTING
      e_valid = lv_valid.

ENDFORM.                    " CHECK_DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page .

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_heading.

ENDFORM.                    " TOP_OF_PAGE

*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
FORM set_pf_status USING it_excluding .

  SET PF-STATUS 'STANDARD' EXCLUDING it_excluding.

ENDFORM.                    " SET_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENTTAB
*&---------------------------------------------------------------------*
FORM build_eventtab .
  DATA: ls_event TYPE slis_alv_event,
        ls_exclu TYPE LINE OF slis_t_extab.

  CLEAR: gt_events[], gt_excluding[].

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = gt_events[].

  READ TABLE gt_events WITH KEY name = slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE formname_top_of_page TO ls_event-form.
    APPEND ls_event TO gt_events.
  ENDIF.

* Menu Functions to be excluded
  ls_exclu-fcode = '&CRB'.  APPEND ls_exclu TO gt_excluding.
  ls_exclu-fcode = '&CRL'.  APPEND ls_exclu TO gt_excluding.
  ls_exclu-fcode = '&CRR'.  APPEND ls_exclu TO gt_excluding.
  ls_exclu-fcode = '&CRE'.  APPEND ls_exclu TO gt_excluding.
  ls_exclu-fcode = '&AQW'.  APPEND ls_exclu TO gt_excluding.
  ls_exclu-fcode = '%SL'.   APPEND ls_exclu TO gt_excluding.
  ls_exclu-fcode = '&ABC'.  APPEND ls_exclu TO gt_excluding.
ENDFORM.                    " BUILD_EVENTTAB
*&---------------------------------------------------------------------*
*&      Form  BUILD_HEADER
*&---------------------------------------------------------------------*
FORM build_header .

  DATA: hline    TYPE slis_listheader,
        text(60) TYPE c,
        w_inx    LIKE sy-tabix.

  CLEAR gt_heading[].

  CLEAR: hline, text.
  hline-typ  = 'H'.
  hline-info = sy-title.
  APPEND hline TO gt_heading.

  CLEAR text.
  hline-typ  = 'S'.
  WRITE: 'Kullanıcı: '(hd1) TO text,
         sy-uname TO text+11,
         'Tarih: '(hd2) TO text+25,
         sy-datum TO text+32,
         'Saat: '(hd3) TO text+45,
         sy-uzeit TO text+51.
  hline-info = text.
  APPEND hline TO gt_heading.
  hline-typ = 'S'.
  DATA lv_count(10) TYPE c.
  lv_count = gv_count.
  CONCATENATE 'Kayıt Sayısı:' lv_count
              INTO text SEPARATED BY space.

  hline-info = text.
  APPEND hline TO gt_heading.

ENDFORM.                    " BUILD_HEADER
*&---------------------------------------------------------------------*
*&      Form  SHOW_DIALOG_REPORT
*&---------------------------------------------------------------------*
FORM show_dialog_report  TABLES p_table TYPE STANDARD TABLE .


*fieldcatalog for alv dialog
  PERFORM create_field_catalog TABLES p_table.
*Layout for ALV dialog
  PERFORM list_layout_specification.
**ALV dialog output
  PERFORM dialog_show_alv TABLES p_table.


ENDFORM.                    " SHOW_DIALOG_REPORT
*&---------------------------------------------------------------------*
*&      Form  DALOG_SHOW_ALV
*&---------------------------------------------------------------------*
FORM dialog_show_alv  TABLES   p_table TYPE STANDARD TABLE.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_bypassing_buffer    = 'X'
      i_callback_program    = sy-repid
      i_grid_title          = text-101
      is_layout_lvc         = gs_layout
      it_fieldcat_lvc       = gt_fieldcat[]
      i_screen_start_column = 1
      i_screen_start_line   = 1
      i_screen_end_column   = 100
      i_screen_end_line     = 10
    TABLES
      t_outtab              = p_table
    EXCEPTIONS
      program_error         = 1
      error_message         = 3
      OTHERS                = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DiALOG_SHOW_ALV

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LOG
*&---------------------------------------------------------------------*
FORM display_log .

  CHECK gt_messtab[] IS NOT INITIAL.

  CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
    EXPORTING
      it_message = gt_messtab[].


ENDFORM.                    " DISPLAY_LOG


*&---------------------------------------------------------------------*
*&      Form  GET_DTEXT
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*      -->P_DOMNAME  text
*      -->P_DOMVALUE  text
*      <--P_DDTEXT  text
*----------------------------------------------------------------------*
FORM get_dtext  USING    p_domname
                         p_domvalue
                CHANGING p_ddtext.

  CLEAR p_ddtext.

  SELECT SINGLE ddtext
           INTO p_ddtext
           FROM dd07t
          WHERE domname     = p_domname
            AND ddlanguage  = sy-langu
            AND as4local    = 'A'
            AND domvalue_l  = p_domvalue.

ENDFORM.                    " GET_DTEXT
*&---------------------------------------------------------------------*
*&      Form  ADD_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MSGTY   text
*      -->P_MSGID   text
*      -->P_MSGNO   text
*      -->P_MSGV1   text
*      -->P_MSGV2   text
*      -->P_MSGV3   text
*      -->P_MSGV4   text
*----------------------------------------------------------------------*
FORM add_message  USING p_msgty
                        p_msgid
                        p_msgno
                        p_msgv1
                        p_msgv2
                        p_msgv3
                        p_msgv4
                        p_index.
  DATA: ls_bapiret2 TYPE bapiret2.

  CLEAR ls_bapiret2.
  ls_bapiret2-type         = p_msgty.
  ls_bapiret2-id           = p_msgid.
  ls_bapiret2-number       = p_msgno.
  ls_bapiret2-message_v1   = p_msgv1.
  ls_bapiret2-message_v2   = p_msgv2.
  ls_bapiret2-message_v3   = p_msgv3.
  ls_bapiret2-message_v4   = p_msgv4.
  ls_bapiret2-row          = p_index.
  APPEND ls_bapiret2 TO gt_messtab.

ENDFORM.                    " ADD_MESSAGE