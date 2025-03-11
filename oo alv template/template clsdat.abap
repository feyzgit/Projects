*&---------------------------------------------------------------------*
*& Include          ZFKARAKAS_CLSDAT
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  Class           lcl_application           Definition
*&---------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.

    CLASS-DATA: app TYPE REF TO lcl_application.

    CLASS-METHODS:
      instance_app
        RETURNING
          VALUE(ro_app) TYPE REF TO lcl_application.

    DATA:
      mo_grid        TYPE REF TO cl_gui_alv_grid,
      mo_dyndoc_id   TYPE REF TO cl_dd_document,
      mo_splitter    TYPE REF TO cl_gui_splitter_container,
      mo_parent_grid TYPE REF TO cl_gui_container,
      mo_parent_top  TYPE REF TO cl_gui_container,
      mo_html_cntrl  TYPE REF TO cl_gui_html_viewer,
      mt_fieldcat    TYPE lvc_t_fcat.

    TYPES:
      ty_bookings TYPE STANDARD TABLE OF sbook.

    DATA:
      mt_outdat TYPE ty_bookings.

    TYPES: BEGIN OF ty_param,
             param_id TYPE memoryid,
             value    TYPE char100,
           END  OF ty_param.

    METHODS:
      initialization,
      start_of_selection,
      retrieve_dat
        IMPORTING
          iv_airline TYPE s_carr_id
        EXCEPTIONS
          contains_error,
      alv_session
        EXCEPTIONS
          contains_error,
      display_alvdat
        EXCEPTIONS
          contains_error,
      create_fieldcat
        IMPORTING
          !im_strname TYPE tabname
        EXCEPTIONS
          contains_error,
      update_fieldcat
        EXCEPTIONS
          contains_error,
      set_layout_dat
        RETURNING
          VALUE(rv_layoutdat) TYPE lvc_s_layo,
      set_exclude_dat
        RETURNING
          VALUE(rv_excludedat) TYPE ui_functions,
      attach_handlers
        IMPORTING
          VALUE(im_grid) TYPE REF TO cl_gui_alv_grid
        EXCEPTIONS
          contains_error,
      refresh_alv
        EXCEPTIONS
          contains_error,
      get_long_date
        IMPORTING
          !im_dat         TYPE datum
        RETURNING
          VALUE(r_dattxt) TYPE text40,
      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
          e_column_id
          es_row_no,
      handle_toolbar_set FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_interactive,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm,
      handle_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING
          e_dyndoc_id,
      event_top_of_page
        CHANGING
          dg_dyndoc_id TYPE REF TO cl_dd_document,
      display_top_of_page
        IMPORTING
          dg_dyndoc_id TYPE REF TO cl_dd_document.


  PRIVATE SECTION.
    CONSTANTS:
      mc_strname TYPE tabname VALUE 'SBOOK'.

ENDCLASS.                    "lcl_application DEFINITION
*&---------------------------------------------------------------------*
*&  Class           lcl_application           Implementation
*&---------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD instance_app.
    FREE: ro_app.
    IF lcl_application=>app IS NOT BOUND.
      CREATE OBJECT lcl_application=>app.
    ENDIF.
    ro_app = app.
  ENDMETHOD.                    "instance_app
  METHOD initialization.
  ENDMETHOD.                    "initialization
  METHOD start_of_selection.

    app->retrieve_dat(
      EXPORTING
        iv_airline     = p_carrid
      EXCEPTIONS
        contains_error = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4. RETURN.
    ENDIF.

    CALL SCREEN 100.

  ENDMETHOD.                    "start_of_selection
  METHOD retrieve_dat.
    FREE: mt_outdat.
    SELECT *
      FROM sbook
      INTO TABLE @mt_outdat
      WHERE carrid EQ @iv_airline.
    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.
  ENDMETHOD.
  METHOD alv_session.

    IF NOT lines( app->mt_outdat ) IS INITIAL.
      app->display_alvdat(
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "alv_session
  METHOD display_alvdat.

    IF mo_grid IS NOT BOUND.
*-&Create TOP-Document
      CREATE OBJECT mo_dyndoc_id
        EXPORTING
          style = 'ALV_GRID'.

      CREATE OBJECT mo_splitter
        EXPORTING
          parent  = cl_gui_custom_container=>screen0
          rows    = 2
          columns = 1.

      CALL METHOD mo_splitter->get_container
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = mo_parent_top.

      CALL METHOD mo_splitter->get_container
        EXPORTING
          row       = 2
          column    = 1
        RECEIVING
          container = mo_parent_grid.

      CALL METHOD mo_splitter->set_row_height
        EXPORTING
          id     = 1
          height = 12.

      CREATE OBJECT mo_grid
        EXPORTING
          i_parent          = mo_parent_grid
          i_lifetime        = cl_gui_alv_grid=>lifetime_dynpro
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      create_fieldcat(
        EXPORTING
          im_strname     = mc_strname
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      update_fieldcat(
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      attach_handlers(
        EXPORTING
          im_grid        = mo_grid
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      mo_grid->set_table_for_first_display(
        EXPORTING
          i_buffer_active               = space
          i_bypassing_buffer            = abap_true
          is_layout                     = set_layout_dat( )
          it_toolbar_excluding          = CONV #( set_exclude_dat( ) )
        CHANGING
          it_fieldcatalog               = mt_fieldcat
          it_outtab                     = app->mt_outdat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      CALL METHOD mo_dyndoc_id->initialize_document
        EXPORTING
          background_color = cl_dd_area=>col_textarea.

      CALL METHOD mo_grid->list_processing_events
        EXPORTING
          i_event_name = 'TOP_OF_PAGE'
          i_dyndoc_id  = mo_dyndoc_id.
    ELSE.
      refresh_alv(
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "display
  METHOD create_fieldcat.

    FREE: mt_fieldcat.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = im_strname
      CHANGING
        ct_fieldcat            = mt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.

  ENDMETHOD.                    "create_fieldcat
  METHOD update_fieldcat.

    DATA: _text(40) TYPE c.
    LOOP AT mt_fieldcat REFERENCE INTO DATA(r_fieldcat).
      CLEAR: _text.
      CASE r_fieldcat->fieldname .
        WHEN 'XXX'.
      ENDCASE.
      IF _text <> space.
        MOVE _text TO: r_fieldcat->scrtext_l, r_fieldcat->scrtext_m, r_fieldcat->scrtext_s, r_fieldcat->reptext.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "update_fieldcat
  METHOD set_layout_dat.

    rv_layoutdat = VALUE lvc_s_layo( col_opt = abap_true cwidth_opt = abap_true zebra = abap_true ).

  ENDMETHOD.                    "set_layout_dat
  METHOD set_exclude_dat.

    rv_excludedat = VALUE #( ( cl_gui_alv_grid=>mc_fc_loc_copy_row ) ( cl_gui_alv_grid=>mc_fc_loc_delete_row ) ( cl_gui_alv_grid=>mc_fc_loc_append_row ) ( cl_gui_alv_grid=>mc_fc_loc_insert_row ) ( cl_gui_alv_grid=>mc_fc_loc_move_row )
                             ( cl_gui_alv_grid=>mc_fc_loc_copy ) ( cl_gui_alv_grid=>mc_fc_loc_cut ) ( cl_gui_alv_grid=>mc_fc_loc_paste ) ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row ) ( cl_gui_alv_grid=>mc_fc_loc_undo )
                             ( cl_gui_alv_grid=>mc_fc_graph ) ( cl_gui_alv_grid=>mc_fc_info ) ( cl_gui_alv_grid=>mc_fc_refresh ) ( cl_gui_alv_grid=>mc_fc_print ) ( cl_gui_alv_grid=>mc_fc_detail ) ) .

  ENDMETHOD.                    "set_exclude_dat
  METHOD attach_handlers.

    CALL METHOD im_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    SET HANDLER me->handle_hotspot_click FOR im_grid.
    SET HANDLER me->handle_toolbar_set   FOR im_grid.
    SET HANDLER me->handle_user_command  FOR im_grid.
    SET HANDLER me->handle_top_of_page   FOR im_grid.
  ENDMETHOD.                    "attach_handlers
  METHOD refresh_alv.

    mo_grid->refresh_table_display(
      EXPORTING
        is_stable = VALUE #( row = abap_true col = abap_true )
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.

  ENDMETHOD.                    "refresh_alv
  METHOD get_long_date.

    DATA: t_month_names TYPE TABLE OF t247,
          r_month_names TYPE REF TO t247,
          l_daytxt      TYPE hrvsched-daytxt,
          l_year(4)     TYPE c,
          l_month(2)    TYPE c,
          l_day(2)      TYPE c.

    FREE: t_month_names.
    CALL FUNCTION 'MONTH_NAMES_GET'
      EXPORTING
        language    = sy-langu
      TABLES
        month_names = t_month_names.

    l_year = im_dat+0(4).
    l_month = im_dat+4(2).
    l_day = im_dat+6(2).

    CLEAR: l_daytxt.
    CALL FUNCTION 'RH_GET_DATE_DAYNAME'
      EXPORTING
        langu               = sy-langu
        date                = im_dat
      IMPORTING
        daytxt              = l_daytxt
      EXCEPTIONS
        no_langu            = 1
        no_date             = 2
        no_daytxt_for_langu = 3
        invalid_date        = 4
        OTHERS              = 5.

    READ TABLE t_month_names REFERENCE INTO r_month_names INDEX l_month.
    IF sy-subrc IS INITIAL.
      CONCATENATE l_day r_month_names->ltx l_year l_daytxt INTO r_dattxt SEPARATED BY space.
    ENDIF.

  ENDMETHOD.              "get_long_date
  METHOD handle_hotspot_click.

    CASE e_column_id.
      WHEN 'XXX'.
    ENDCASE.

  ENDMETHOD.              "handle_hotspot_click
  METHOD handle_toolbar_set.

    DELETE e_object->mt_toolbar WHERE function = '&LOCAL&COPY_ROW'
                                   OR function = '&LOCAL&APPEND'
                                   OR function = '&LOCAL&INSERT_ROW'
                                   OR function = '&LOCAL&DELETE_ROW'
                                   OR function = '&LOCAL&CUT'
                                   OR function = '&LOCAL&COPY'
                                   OR function = '&LOCAL&PASTE' .
*-&Add ALV buttons;
*    e_object->mt_toolbar = VALUE #( BASE e_object->mt_toolbar
*                                  ( butn_type = 0
*                                    function  = '&XXX'
*                                    icon      = '@M3@'
*                                    text      = 'XXX'
*                                    quickinfo = 'XXX' ) ).

  ENDMETHOD.                    "handle_toolbar
  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'XXX'.
    ENDCASE.

  ENDMETHOD.                    "handle_user_command
  METHOD handle_top_of_page.
    event_top_of_page(
      CHANGING
        dg_dyndoc_id = e_dyndoc_id ).
  ENDMETHOD.                    "handle_top_of_page
  METHOD event_top_of_page.

    DATA : dl_text(255) TYPE c,
           v_name_last  TYPE adrp-name_last,
           v_name_first TYPE adrp-name_first.

    CONSTANTS: c_date_from TYPE adrp-date_from VALUE '00010101'.

*-&Free class and create top of page document;
    FREE dg_dyndoc_id.
    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

*--------------------------------------------------------------------*
*-&HEADER ->
*--------------------------------------------------------------------*
    CLEAR: dl_text.
    CONCATENATE 'Başlık:' TEXT-h01 INTO dl_text.
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_style    = cl_dd_area=>heading
        sap_fontsize = cl_dd_area=>standard
        sap_color    = cl_dd_area=>list_heading_int.

    CLEAR: dl_text.
    dl_text = TEXT-h02.
    CALL METHOD dg_dyndoc_id->new_line.
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_style    = cl_dd_area=>heading
        sap_fontsize = cl_dd_area=>large
        sap_emphasis = cl_dd_area=>strong.

    CLEAR: dl_text.
    dl_text = 'Kullanıcı Adı :'.
    CALL METHOD dg_dyndoc_id->new_line.
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_emphasis = cl_dd_area=>strong
        sap_color    = cl_dd_area=>list_heading_int.

    CLEAR: dl_text, v_name_last, v_name_first.
    SELECT SINGLE name_last name_first
      FROM usr21 JOIN adrp ON adrp~persnumber = usr21~persnumber AND
                              adrp~date_from  = c_date_from AND
                              adrp~nation     = space
        INTO (v_name_last, v_name_first)
        WHERE usr21~bname = sy-uname.
    CONCATENATE v_name_first v_name_last INTO dl_text SEPARATED BY space.

    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_emphasis = cl_dd_area=>heading
        sap_color    = cl_dd_area=>list_negative_inv.

    CLEAR: dl_text.
    dl_text = 'Tarih :'.
    CALL METHOD dg_dyndoc_id->new_line.
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_emphasis = cl_dd_area=>strong
        sap_color    = cl_dd_area=>list_heading_int.

    CLEAR dl_text.
    dl_text = get_long_date( im_dat = sy-datum ).
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_emphasis = cl_dd_area=>heading
        sap_color    = cl_dd_area=>list_negative_inv.

    display_top_of_page(
      EXPORTING
        dg_dyndoc_id = dg_dyndoc_id ).

  ENDMETHOD.              "event_top_of_page
  METHOD display_top_of_page.

    IF app->mo_html_cntrl IS INITIAL.
      CREATE OBJECT app->mo_html_cntrl
        EXPORTING
          parent = app->mo_parent_top.
    ENDIF.
    CALL METHOD dg_dyndoc_id->merge_document.
    dg_dyndoc_id->html_control = app->mo_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = abap_true
        parent             = app->mo_parent_top
      EXCEPTIONS
        html_display_error = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.              "display_top_of_page
ENDCLASS.                    "lcl_application IMPLEMENTATION