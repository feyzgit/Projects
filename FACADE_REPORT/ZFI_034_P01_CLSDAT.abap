*&---------------------------------------------------------------------*
*&  Interface           LIF_VIEW
*&---------------------------------------------------------------------*
INTERFACE lif_view.
  DATA:
    mo_grid     TYPE REF TO cl_gui_alv_grid,
    mt_fieldcat TYPE lvc_t_fcat.

  METHODS:
    create_fcat
      CHANGING
        ct_fcat TYPE lvc_t_fcat
      RAISING
        zcx_tjk,
    display
      IMPORTING
        im_previous TYPE REF TO lcl_abstract_reporting
        im_alvdat   TYPE REF TO data
      EXCEPTIONS
        contains_error.
ENDINTERFACE.

CLASS lcl_selection_screen_control DEFINITION FINAL.
  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF mc_msg,
        id      TYPE symsgid    VALUE 'ZFI_034',
        success TYPE bapi_mtype VALUE 'S',
        error   TYPE bapi_mtype VALUE 'E',
        warning TYPE bapi_mtype VALUE 'W',
        info    TYPE bapi_mtype VALUE 'I',
        abort   TYPE bapi_mtype VALUE 'A',
      END OF mc_msg.

    CONSTANTS:
      BEGIN OF mc_cons,
        abap_true  TYPE char1 VALUE 'X',
        abap_false TYPE char1 VALUE '',
      END OF mc_cons.

    TYPES:
      BEGIN OF mty_dynfld,
        fieldname TYPE dynfnam,
      END OF mty_dynfld,
      mtt_dynfld TYPE HASHED TABLE OF mty_dynfld WITH UNIQUE KEY fieldname.

    CLASS-METHODS:
      class_constructor,
      load_of_program
        RAISING
          zcx_tjk,
      initialization
        RAISING
          zcx_tjk,
      at_selection_screen_output
        RAISING
          zcx_tjk,
      at_selection_screen
        RAISING
          zcx_tjk,
      start_of_selection
        RAISING
          zcx_tjk,
      get_last_month_days
        IMPORTING
          im_kdate        TYPE datum
        RETURNING
          VALUE(rv_kdate) TYPE datum
        RAISING
          zcx_tjk.

  PRIVATE SECTION.

ENDCLASS.
CLASS lcl_selection_screen_control IMPLEMENTATION.
  METHOD class_constructor.

  ENDMETHOD.
  METHOD load_of_program.

  ENDMETHOD.
  METHOD initialization.

    blok_1 = TEXT-b01.
    blok_2 = TEXT-b02.

    FREE: password.
    password = 'Şifre'.

    FREE: p_uname, p_passw.
    p_uname = 'VF_Reports'.
    p_passw = '1834F95D'.

    FREE: p_bdate, p_edate.
    p_bdate = sy-datum.
    p_edate = sy-datum.

    FREE: mt_switch.
    mt_switch = VALUE #( ( repex = '01' repet = 'Bayi Satış Raporu' cname = 'LCL_REPEX01_APPLICATION' sname = 'ZFI_034_S01' ) ).

  ENDMETHOD.
  METHOD at_selection_screen_output.

    hidden = COND #( WHEN p_hsval EQ abap_true THEN '@06@ Göster' ELSE '@07@ Gizle' ).

    LOOP AT SCREEN.
      IF screen-name CS 'BLOK'.
        screen-intensified = 1.
      ENDIF.
      IF screen-name EQ 'P_BDATE' OR screen-name EQ 'P_EDATE' .
        screen-required = 2.
      ENDIF.

      IF screen-name EQ 'P_UNAME' OR screen-name EQ 'P_PASSW' .
        screen-required = 2.
      ENDIF.

      IF screen-name EQ 'P_PASSW'.
        screen-invisible = COND #( WHEN p_hsval EQ abap_true THEN '1' ELSE '0').
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.

  ENDMETHOD.
  METHOD at_selection_screen.

    CASE sy-ucomm.
      WHEN 'ONLI'.
    ENDCASE.

  ENDMETHOD.
  METHOD start_of_selection.

    CALL SCREEN mc_screen.

  ENDMETHOD.
  METHOD get_last_month_days.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = im_kdate
      IMPORTING
        last_day_of_month = rv_kdate
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_tjk
        MESSAGE ID sy-msgid
          TYPE sy-msgty
            NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
CLASS lcl_helper DEFINITION.

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF mc_msg,
        id      TYPE symsgid    VALUE 'ZFI_034',
        success TYPE bapi_mtype VALUE 'S',
        error   TYPE bapi_mtype VALUE 'E',
        warning TYPE bapi_mtype VALUE 'W',
        info    TYPE bapi_mtype VALUE 'I',
        abort   TYPE bapi_mtype VALUE 'A',
      END OF mc_msg.

    METHODS:
      display_messege
        IMPORTING
          it_bapiret2 TYPE bapiret2_tab.

ENDCLASS.
CLASS lcl_helper IMPLEMENTATION.
  METHOD display_messege.

    cl_rmsl_message=>display( it_message = it_bapiret2 ).

  ENDMETHOD.

ENDCLASS.
CLASS lcl_view_alv DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES: lif_view.
    DATA:
      mo_dyndoc_id   TYPE REF TO cl_dd_document,
      mo_splitter    TYPE REF TO cl_gui_splitter_container,
      mo_parent_grid TYPE REF TO cl_gui_container,
      mo_parent_top  TYPE REF TO cl_gui_container,
      mo_html_cntrl  TYPE REF TO cl_gui_html_viewer.

    ALIASES:
      mo_grid     FOR lif_view~mo_grid,
      mt_fieldcat FOR lif_view~mt_fieldcat,
      create_fcat FOR lif_view~create_fcat.

    METHODS:
      constructor
        IMPORTING
          iv_strname TYPE tabname.
  PRIVATE SECTION.

    DATA: mv_strname TYPE tabname.

    METHODS:
      get_long_date
        IMPORTING
          !im_dat         TYPE datum
        RETURNING
          VALUE(r_dattxt) TYPE text40,
      handle_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING
          e_dyndoc_id,
      event_top_of_page
        CHANGING
          dg_dyndoc_id TYPE REF TO cl_dd_document,
      display_top_of_page
        IMPORTING
          dg_dyndoc_id TYPE REF TO cl_dd_document,
      set_layout_dat
        RETURNING
          VALUE(rv_layoutdat) TYPE lvc_s_layo,
      set_exclude_dat
        RETURNING
          VALUE(rv_excludedat) TYPE ui_functions,
      attach_handlers
        IMPORTING
          im_event       TYPE REF TO lcl_abstract_reporting
          VALUE(im_grid) TYPE REF TO cl_gui_alv_grid
        EXCEPTIONS
          contains_error,
      refresh_alv
        EXCEPTIONS
          contains_error.

ENDCLASS.

CLASS lcl_abstract_reporting DEFINITION ABSTRACT.

  PUBLIC SECTION.

    DATA: view      TYPE REF TO lif_view,
          mo_helper TYPE REF TO lcl_helper.

    CLASS-METHODS:
      create
        IMPORTING
          im_clsname        TYPE seoclsname
          im_view           TYPE REF TO lif_view
        RETURNING
          VALUE(r_instance) TYPE REF TO lcl_abstract_reporting
        RAISING
          zcx_tjk.

    METHODS:
      constructor
        IMPORTING
          i_view TYPE REF TO lif_view,
      start,
      _handle_hotspot_click ABSTRACT FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
          e_row_id
          e_column_id
          es_row_no
          sender,
      _handle_toolbar_set ABSTRACT FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_interactive,
      _handle_user_command ABSTRACT FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm.

  PROTECTED SECTION.
    METHODS:
      _retrieve_dat ABSTRACT
        RAISING
          zcx_tjk,
      _processing ABSTRACT
        RAISING
          zcx_tjk,
      _update_fieldcat ABSTRACT
        CHANGING
          ct_fcat TYPE lvc_t_fcat
        RAISING
          zcx_tjk,
      _display
        RAISING
          zcx_tjk.

    DATA: table TYPE REF TO data.

ENDCLASS.
CLASS lcl_abstract_reporting IMPLEMENTATION.

  METHOD constructor.
    me->view = i_view.

    IF mo_helper IS NOT BOUND.
      CREATE OBJECT mo_helper.
    ENDIF.

  ENDMETHOD.

  METHOD create.

    DATA:
      mo_application TYPE REF TO object,
      ptab           TYPE abap_parmbind_tab,
      ptab_line      TYPE abap_parmbind.

    FREE: ptab.
    ptab_line-name = 'I_VIEW'.
    ptab_line-kind = cl_abap_objectdescr=>exporting.
    ptab_line-value = REF #( im_view ).
    INSERT ptab_line INTO TABLE ptab.

    TRY.
        CREATE OBJECT mo_application TYPE (im_clsname) PARAMETER-TABLE ptab.
        r_instance = CAST #( mo_application ).
      CATCH cx_sy_create_object_error.
        RAISE EXCEPTION TYPE zcx_tjk
          MESSAGE e006 WITH VALUE #( mt_switch[ KEY primary_key COMPONENTS repex = p_repex ]-repet OPTIONAL ) im_clsname.
    ENDTRY.

  ENDMETHOD.

  METHOD _display.

    view->display(
      EXPORTING
        im_previous = me
        im_alvdat   = table ).

  ENDMETHOD.
  METHOD start.

    TRY.
        _retrieve_dat( ).
        _processing( ).
        _update_fieldcat( CHANGING ct_fcat = view->mt_fieldcat ).
        _display( ).

      CATCH zcx_tjk INTO mr_message.
        MESSAGE mr_message TYPE mr_message->msgty.
        LEAVE TO SCREEN 0.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
CLASS lcl_view_alv IMPLEMENTATION.

  METHOD constructor.
    mv_strname = iv_strname.

    TRY.
        create_fcat(
          CHANGING
            ct_fcat = mt_fieldcat ).
      CATCH zcx_tjk INTO mr_message.
        MESSAGE mr_message TYPE mr_message->msgty.
    ENDTRY.
  ENDMETHOD.
  METHOD create_fcat.

    FREE: ct_fcat.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = mv_strname
      CHANGING
        ct_fieldcat            = ct_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_tjk
        MESSAGE e005.
    ENDIF.

  ENDMETHOD.                    "create_fcat
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

  ENDMETHOD.                    "get_long_date
  METHOD handle_top_of_page.

    event_top_of_page(
      CHANGING
        dg_dyndoc_id = e_dyndoc_id ).

  ENDMETHOD.                    "handle_top_of_page
  METHOD event_top_of_page.

    DATA:
      dl_text(255) TYPE c,
      v_name_last  TYPE adrp-name_last,
      v_name_first TYPE adrp-name_first.

    CONSTANTS:
      c_date_from TYPE adrp-date_from VALUE '00010101'.

*-&Free class and create top of page document;
    FREE dg_dyndoc_id.
    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

*--------------------------------------------------------------------*
*-&HEADER ->
*--------------------------------------------------------------------*
    CLEAR: dl_text.
    dl_text = |Başlık: { VALUE #( mt_switch[ KEY primary_key COMPONENTS repex = p_repex ]-repet OPTIONAL ) }|.
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


  ENDMETHOD.                    "event_top_of_page
  METHOD display_top_of_page.

    IF mo_html_cntrl IS INITIAL.
      CREATE OBJECT mo_html_cntrl
        EXPORTING
          parent = mo_parent_top.
    ENDIF.
    CALL METHOD dg_dyndoc_id->merge_document.
    dg_dyndoc_id->html_control = mo_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = abap_true
        parent             = mo_parent_top
      EXCEPTIONS
        html_display_error = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "display_top_of_page
  METHOD set_layout_dat.

    rv_layoutdat = VALUE lvc_s_layo( col_opt = abap_true cwidth_opt = abap_true zebra = abap_true sel_mode = 'D' ).

  ENDMETHOD.                    "set_layout_dat
  METHOD set_exclude_dat.

    rv_excludedat = VALUE #( ( cl_gui_alv_grid=>mc_fc_loc_copy_row ) ( cl_gui_alv_grid=>mc_fc_loc_delete_row ) ( cl_gui_alv_grid=>mc_fc_loc_append_row ) ( cl_gui_alv_grid=>mc_fc_loc_insert_row ) ( cl_gui_alv_grid=>mc_fc_loc_move_row )
                             ( cl_gui_alv_grid=>mc_fc_loc_copy ) ( cl_gui_alv_grid=>mc_fc_loc_cut ) ( cl_gui_alv_grid=>mc_fc_loc_paste ) ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row ) ( cl_gui_alv_grid=>mc_fc_loc_undo )
                             ( cl_gui_alv_grid=>mc_fc_graph ) ( cl_gui_alv_grid=>mc_fc_info ) ( cl_gui_alv_grid=>mc_fc_refresh ) ( cl_gui_alv_grid=>mc_fc_print ) ( cl_gui_alv_grid=>mc_fc_detail ) ) .

  ENDMETHOD.                    "set_exclude_dat
  METHOD attach_handlers.

    SET HANDLER handle_top_of_page              FOR mo_grid.
    SET HANDLER im_event->_handle_hotspot_click FOR mo_grid.
    SET HANDLER im_event->_handle_toolbar_set   FOR mo_grid.
    SET HANDLER im_event->_handle_user_command  FOR mo_grid.

    CALL METHOD im_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

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
  METHOD lif_view~display.

    FIELD-SYMBOLS: <ft_alvdat> TYPE STANDARD TABLE.

    IF mo_grid IS NOT BOUND.
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

      attach_handlers(
        EXPORTING
          im_event       = im_previous
          im_grid        = mo_grid
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      ASSIGN im_alvdat->* TO <ft_alvdat>.
      mo_grid->set_table_for_first_display(
        EXPORTING
          i_buffer_active               = space
          i_bypassing_buffer            = abap_true
          is_layout                     = set_layout_dat( )
          it_toolbar_excluding          = CONV #( set_exclude_dat( ) )
        CHANGING
          it_fieldcatalog               = mt_fieldcat
          it_outtab                     = <ft_alvdat>
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
  ENDMETHOD.

ENDCLASS.