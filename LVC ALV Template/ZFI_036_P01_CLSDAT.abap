*&---------------------------------------------------------------------*
*& Include          ZFI_036_P01_CLSDAT
*&---------------------------------------------------------------------*
TABLES: sscrfields, zfi_036_s01, bseg.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
  PARAMETERS:
    p_path TYPE rlgrap-filename OBLIGATORY DEFAULT 'C:\' MODIF ID md1.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN: FUNCTION KEY 2.


DATA: mt_outdat    TYPE STANDARD TABLE OF zfi_036_s01,
      it_excluding TYPE slis_t_extab.

CONSTANTS: BEGIN OF _msg,
             id      TYPE symsgid VALUE 'ZFI_036',
             success TYPE bapi_mtype VALUE 'S',
             error   TYPE bapi_mtype VALUE 'E',
             warning TYPE bapi_mtype VALUE 'W',
             info    TYPE bapi_mtype VALUE 'I',
             abort   TYPE bapi_mtype VALUE 'A',
           END OF _msg.

CONSTANTS: BEGIN OF mc_cons,
             strname TYPE tabname VALUE 'ZFI_036_S01',
             excel   TYPE w3objid VALUE 'ZFI_036_EXCEL_TEMP',
           END OF mc_cons.

*----------------------------------------------------------------------*
*       CLASS application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS application DEFINITION .

  PUBLIC SECTION .

    TYPES:
      BEGIN OF ty_exceldat,
        bukrs TYPE c LENGTH 100,
        ruhid TYPE c LENGTH 100,
        appid TYPE c LENGTH 100,
        xref3 TYPE c LENGTH 100,
        menge TYPE c LENGTH 100,
        meins TYPE c LENGTH 100,
      END OF ty_exceldat,
      mtt_exceldat TYPE STANDARD TABLE OF ty_exceldat.


    CLASS-DATA: app TYPE REF TO application.
    CLASS-METHODS:
      initialization,
      at_selection_screen,
      at_selection_screen_output,
      at_selection_screen_request
        IMPORTING
          !iv_fieldname TYPE clike,
      app_instance
        RETURNING
          VALUE(mo_app) TYPE REF TO application.

    METHODS:
      f4_finanscal_perio
        CHANGING
          ch_spmon TYPE s031-spmon,
      mount_last_day
        IMPORTING
          !iv_date       TYPE datum
        RETURNING
          VALUE(rv_date) TYPE datum,
      retrieve_data
        EXPORTING
          !iv_path TYPE rlgrap-filename
        EXCEPTIONS
          contains_error,
      conv_price_excel_to_sap
        CHANGING
          cv_price TYPE char100,
      conv_meins_excel_to_sap
        CHANGING
          cv_meins TYPE char100,
      save_dat
        RETURNING
          VALUE(rt_retdat) TYPE bapiret2_tab,
      popup_confirm
        IMPORTING
          !im_batch         TYPE abap_bool DEFAULT abap_false
          !im_titlebar      TYPE char20
          !im_text_question TYPE char100
        EXPORTING
          ev_answer         TYPE char1,
      show_message
        IMPORTING
          it_msgdat TYPE bapiret2_tab,
      show_message_tool
        IMPORTING
          !iv_msgdat TYPE bapiret2_tab,
      job_scheduler,
      show.

  PROTECTED SECTION .

    DATA: fieldcat TYPE lvc_t_fcat .

  PRIVATE SECTION .
    METHODS:
      describe_by_data,
      alv.

ENDCLASS.                    "application DEFINITION

*----------------------------------------------------------------------*
*       CLASS application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS application IMPLEMENTATION .
  METHOD initialization.

    sscrfields-functxt_02 = VALUE smp_dyntxt( text = TEXT-t03 icon_id = icon_xls icon_text = TEXT-t03 ).

  ENDMETHOD.                    "initialization
  METHOD at_selection_screen.

    CASE sy-ucomm.
      WHEN 'FC02'.
        DATA(t_retdat) = NEW zfi_helper_cl02( )->download_templ( im_objname = mc_cons-excel ).
        IF line_exists( t_retdat[ type = 'E' ] ).
          app->show_message(
            EXPORTING
              it_msgdat = t_retdat ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "at_selection_screen
  METHOD at_selection_screen_output.



  ENDMETHOD.                    "at_selection_screen_output
  METHOD at_selection_screen_request.

    DATA: lt_filetable TYPE filetable,
          lr_filetable TYPE REF TO file_table,
          lv_rc        TYPE i.

    REFRESH lt_filetable.
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      CHANGING
        file_table              = lt_filetable[]
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.

    READ TABLE lt_filetable REFERENCE INTO lr_filetable INDEX 1.
    IF sy-subrc IS INITIAL.
      MOVE lr_filetable->filename TO p_path.
    ENDIF.

  ENDMETHOD.                    "at_selection_screen_request
  METHOD app_instance.

    FREE: mo_app, app.
    IF application=>app IS NOT BOUND.
      CREATE OBJECT application=>app.
    ENDIF.
    mo_app = app.

  ENDMETHOD.                    "app_instance
  METHOD f4_finanscal_perio.

    DATA: l_returncode1 TYPE sy-subrc,
          l_monat1      TYPE isellist-month,
          l_hlp_repid1  TYPE sy-repid.

    l_monat1 = sy-datum+0(6).
    CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
      EXPORTING
        actual_month               = l_monat1
        factory_calendar           = ' '
        holiday_calendar           = ' '
        language                   = sy-langu
        start_column               = 8
        start_row                  = 5
      IMPORTING
        selected_month             = l_monat1
        return_code                = l_returncode1
      EXCEPTIONS
        factory_calendar_not_found = 1
        holiday_calendar_not_found = 2
        month_not_found            = 3
        OTHERS                     = 4.
    IF sy-subrc = 0 AND l_returncode1 = 0.
      ch_spmon = l_monat1.
    ENDIF.

  ENDMETHOD.                    "f4_finanscal_perio
  METHOD mount_last_day.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = iv_date
      IMPORTING
        last_day_of_month = rv_date
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.

  ENDMETHOD.                    "mount_last_day
  METHOD retrieve_data.
    DATA: lt_excel_file TYPE TABLE OF alsmex_tabline,
          ls_excel_file TYPE alsmex_tabline,
          ls_exceldat   TYPE ty_exceldat,
          ev_exceldat   TYPE mtt_exceldat.

    DATA: counter     TYPE i,
          tot_counter TYPE i,
          percentage  TYPE i,
          l_index     TYPE i.

    DATA : lv_item_no TYPE bapiacar09-itemno_acc.
    DATA : lv_message TYPE bapi_msg.
    DATA : lv_koart TYPE koart.
    DATA : lv_error TYPE char1.
    DATA : lv_row TYPE numc04.
    DATA : ls_x001  TYPE x001,
           lv_where TYPE char50 VALUE '*',
           ls_t001  TYPE t001.

    FIELD-SYMBOLS : <fs>    TYPE any,
                    <fs_cc> TYPE any.

    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = p_path
        i_begin_col             = 1
        i_begin_row             = 2
        i_end_col               = 256
        i_end_row               = 65536
      TABLES
        intern                  = lt_excel_file
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          DISPLAY LIKE 'E'.
    ENDIF.

    IF lt_excel_file[] IS INITIAL.
      MESSAGE s001(00) WITH 'Dosyada veri yok!' RAISING contains_error.
    ELSE.
      SORT lt_excel_file BY row col.
      LOOP AT lt_excel_file INTO ls_excel_file.
        MOVE ls_excel_file-col TO l_index.
        ASSIGN COMPONENT l_index OF STRUCTURE ls_exceldat TO <fs>.
        MOVE ls_excel_file-value TO <fs>.
        AT END OF row.
          APPEND ls_exceldat TO ev_exceldat.
          CLEAR ls_exceldat.
        ENDAT.
      ENDLOOP.
    ENDIF.

    LOOP AT ev_exceldat ASSIGNING FIELD-SYMBOL(<exceldat>).
      conv_price_excel_to_sap( CHANGING cv_price = <exceldat>-menge ).
      conv_meins_excel_to_sap( CHANGING cv_meins = <exceldat>-meins ).

      APPEND INITIAL LINE TO mt_outdat ASSIGNING FIELD-SYMBOL(<outdat>).
      <outdat> = CORRESPONDING #( <exceldat> ).

    ENDLOOP.

    describe_by_data( ).

  ENDMETHOD .                    "retrieve_data
  METHOD conv_price_excel_to_sap.

    TRANSLATE cv_price USING '. '.
    TRANSLATE cv_price USING ',.'.
    CONDENSE cv_price NO-GAPS.

  ENDMETHOD.                    "conv_price_excel_to_sap
  METHOD conv_meins_excel_to_sap.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = cv_meins
        language       = sy-langu
      IMPORTING
        output         = cv_meins
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
    ENDIF.
  ENDMETHOD.                    "conv_meins_excel_to_sap
  METHOD save_dat.

    IF NOT line_exists( mt_outdat[ selkz = abap_true ] ).
      MESSAGE TEXT-m01 TYPE _msg-success DISPLAY LIKE _msg-error. RETURN.
    ENDIF.

    me->popup_confirm(
      EXPORTING
        im_titlebar      = CONV #( TEXT-t01 )
        im_text_question = CONV #( TEXT-t02 )
      IMPORTING
        ev_answer        = DATA(l_answer) ).
    CHECK l_answer EQ '1'.

    MODIFY zfi_036_t01 FROM TABLE @( VALUE #(
      FOR <outdat> IN mt_outdat
        WHERE ( selkz = abap_true )
        ( bukrs = <outdat>-bukrs
          ruhid = <outdat>-ruhid
          appid = <outdat>-appid
          xref3 = <outdat>-xref3
          menge = <outdat>-menge
          meins = <outdat>-meins ) ) ).
    COMMIT WORK AND WAIT.

    IF sy-subrc <> 0.
      MESSAGE TEXT-m02 TYPE _msg-success DISPLAY LIKE _msg-error. RETURN.
    ELSE.
      MESSAGE TEXT-m03 TYPE _msg-success. RETURN.
    ENDIF.

  ENDMETHOD .                    "save_dat
  METHOD popup_confirm.

    IF im_batch EQ abap_false.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = im_titlebar
          text_question         = im_text_question
          text_button_1         = 'Evet'
          text_button_2         = 'Hayır'
          default_button        = '2'
          display_cancel_button = abap_false
        IMPORTING
          answer                = ev_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
    ELSE.
      ev_answer = '1'.
    ENDIF.

  ENDMETHOD.                    "popup_confirm
  METHOD show_message.

    cl_rmsl_message=>display(
      EXPORTING
        it_message = it_msgdat ).

  ENDMETHOD.
  METHOD show_message_tool.

    CHECK iv_msgdat[] IS NOT INITIAL.

    DATA: lt_message_tab TYPE esp1_message_tab_type,
          lr_message_tab TYPE REF TO esp1_message_wa_type,
          lr_messtab     TYPE REF TO bapiret2.


    LOOP AT iv_msgdat REFERENCE INTO lr_messtab.
      APPEND INITIAL LINE TO lt_message_tab
         REFERENCE INTO lr_message_tab.
      lr_message_tab->msgty = lr_messtab->type       .
      lr_message_tab->msgid = lr_messtab->id         .
      lr_message_tab->msgno = lr_messtab->number     .
      lr_message_tab->msgv1 = lr_messtab->message_v1 .
      lr_message_tab->msgv2 = lr_messtab->message_v2 .
      lr_message_tab->msgv3 = lr_messtab->message_v3 .
      lr_message_tab->msgv4 = lr_messtab->message_v4 .
    ENDLOOP.

    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = lt_message_tab[].

  ENDMETHOD.                    "show_message_tool
  METHOD job_scheduler.

    me->show_message_tool(
      EXPORTING
        iv_msgdat = me->save_dat( ) ).

  ENDMETHOD.                    "job_scheduler
  METHOD show.

    IF lines( mt_outdat ) EQ 0 .
      MESSAGE TEXT-m01 TYPE _msg-success DISPLAY LIKE _msg-error. EXIT.
    ELSE.
      me->alv( ) .
    ENDIF.

  ENDMETHOD.                    "show
  METHOD describe_by_data .

    REFRESH: me->fieldcat.

    DATA: i_fcat     TYPE lvc_t_fcat,
          d_text(40) TYPE c.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = mc_cons-strname
      CHANGING
        ct_fieldcat            = i_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    MODIFY i_fcat FROM VALUE #( key = space ) TRANSPORTING key WHERE key NE space.

    me->fieldcat[] = i_fcat[].

    LOOP AT me->fieldcat REFERENCE INTO DATA(lr_fieldcat).
      CLEAR d_text.

      CASE lr_fieldcat->fieldname .
        WHEN 'SELKZ'.
          lr_fieldcat->no_out = abap_true.
      ENDCASE.
      IF d_text NE space.
        MOVE d_text TO: lr_fieldcat->scrtext_l,
                        lr_fieldcat->scrtext_m,
                        lr_fieldcat->scrtext_s,
                        lr_fieldcat->reptext.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "describe_by_data
  METHOD alv.

    DATA: ls_layout TYPE lvc_s_layo.

    CLEAR: ls_layout.
    ls_layout = VALUE #( col_opt = abap_true
                         cwidth_opt = abap_true
                         zebra = abap_true
                         box_fname = 'SELKZ' ).

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'SET_PF_STATUS'
        i_callback_user_command  = 'USER_COMMAND'
        i_callback_top_of_page   = 'TOP_OF_PAGE'
        is_layout_lvc            = ls_layout
        it_fieldcat_lvc          = me->fieldcat
        it_excluding             = it_excluding[]
        i_default                = abap_true
        i_save                   = abap_true
      TABLES
        t_outtab                 = mt_outdat
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0 .
      MESSAGE TEXT-m03 TYPE _msg-success DISPLAY LIKE _msg-error. EXIT.
    ENDIF.
  ENDMETHOD.                    "alv
ENDCLASS.                    "application IMPLEMENTATION