*&---------------------------------------------------------------------*
*& Include          ZCO_001_P01_CLSDAT
*&---------------------------------------------------------------------*
TYPE-POOLS: slis, icon.
TABLES: sscrfields, anla.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-h01.
  PARAMETERS: p_fname TYPE rlgrap-filename OBLIGATORY DEFAULT 'C:\' MODIF ID md1.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN: FUNCTION KEY 2.

CLASS: lcl_mvc_controller DEFINITION DEFERRED,
       lcl_mvc_model DEFINITION DEFERRED,
       lcl_mvc_view DEFINITION DEFERRED.

DATA: _controller TYPE REF TO lcl_mvc_controller,
      _model      TYPE REF TO lcl_mvc_model,
      _view       TYPE REF TO lcl_mvc_view.
*&---------------------------------------------------------------------*
*&  Class           LCL_MVC_MODEL           Definition
*&---------------------------------------------------------------------*
CLASS lcl_mvc_model DEFINITION.
  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF mc_cons,
        p_fname TYPE char10 VALUE 'P_FNAME',
        excel   TYPE w3objid VALUE 'ZCO_001_EXCEL_TEMP',
      END OF mc_cons.

    TYPES:
      BEGIN OF ty_exceldat,
        ldgrp TYPE c LENGTH 100,
        bukrs TYPE c LENGTH 100,
        kostl TYPE c LENGTH 100,
        kstar TYPE c LENGTH 100,
        gjahr TYPE c LENGTH 100,
      END OF ty_exceldat,
      mtt_exceldat TYPE STANDARD TABLE OF ty_exceldat.
    TYPES:
      ty_outdat TYPE STANDARD TABLE OF zco_001_s01.

    DATA:
      mt_outdat TYPE ty_outdat.

    METHODS:
      initialization_dat
        EXCEPTIONS
          contains_error,
      excel_upload
        IMPORTING
          iv_fname    TYPE rlgrap-filename
        EXPORTING
          ev_exceldat TYPE mtt_exceldat
        EXCEPTIONS
          ex_errordat,
      retrieve_data
        IMPORTING
          iv_fname TYPE rlgrap-filename
        EXCEPTIONS
          contains_error,
      popup_confirm
        IMPORTING
          im_titlebar      TYPE clike
          im_question      TYPE clike
        RETURNING
          VALUE(rv_answer) TYPE char1.

ENDCLASS.
*&---------------------------------------------------------------------*
*&  Class           LCL_MVC_VIEW           Definition
*&---------------------------------------------------------------------*
CLASS lcl_mvc_view DEFINITION.

  PUBLIC SECTION.
    CONSTANTS:
      mc_strname TYPE tabname VALUE 'ZCO_001_S01'.

    TYPES: BEGIN OF ty_param,
             param_id TYPE memoryid,
             value    TYPE char100,
           END  OF ty_param,
           tt_params TYPE TABLE OF ty_param WITH DEFAULT KEY.

    DATA:
      mt_fcat  TYPE lvc_t_fcat,
      mt_exdat TYPE slis_t_extab.

    METHODS:
      display_applog
        IMPORTING
          !iv_msgdat TYPE bapiret2_tab
        EXCEPTIONS
          contains_error,
      call_transaction
        IMPORTING
          im_parameter TYPE tt_params
          im_tcode     TYPE sy-tcode,
      prepare_fcatdat
        IMPORTING
          !im_strname TYPE tabname
        EXCEPTIONS
          contains_error,
      display_alvdat
        EXCEPTIONS
          contains_error.

ENDCLASS.                    "lcl_mvc_view DEFINITION
*&---------------------------------------------------------------------*
*&  Class           LCL_MVC_CONTROLLER           Definition
*&---------------------------------------------------------------------*
CLASS lcl_mvc_controller DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:
      mc_model TYPE seoclsname VALUE 'LCL_MVC_MODEL',
      mc_view  TYPE seoclsname VALUE 'LCL_MVC_VIEW'.

    DATA:
      mo_model TYPE REF TO lcl_mvc_model,
      mo_view  TYPE REF TO lcl_mvc_view.

    METHODS:
      constructor,
      instantiate_app
        IMPORTING
          iv_model             TYPE seoclsname
          iv_view              TYPE seoclsname
        RETURNING
          VALUE(ro_controller) TYPE REF TO lcl_mvc_controller,
      at_selection_screen_request
        IMPORTING
          im_fieldname TYPE char10,
      at_selection_screen,
      at_selection_screen_output,
      get_file_path
        CHANGING
          ch_fname TYPE rlgrap-filename,
      start_of_selection,
      show_message
        IMPORTING
          it_msgdat TYPE bapiret2_tab,
      alv_session
        EXCEPTIONS
          contains_error,
      popup_confirm
        IMPORTING
          im_titlebar      TYPE clike
          im_question      TYPE clike
        RETURNING
          VALUE(rv_answer) TYPE char1,
      call_rundat
        RETURNING
          VALUE(rt_msgdat) TYPE bapiret2_tab
        EXCEPTIONS
          contains_error,
      get_long_date
        IMPORTING
          !im_dat         TYPE datum
        RETURNING
          VALUE(r_dattxt) TYPE text40.

ENDCLASS.
*&---------------------------------------------------------------------*
*&  Class           LCL_MVC_VIEW           Implementation
*&---------------------------------------------------------------------*
CLASS lcl_mvc_model IMPLEMENTATION.
  METHOD initialization_dat.

    sscrfields-functxt_02 = VALUE smp_dyntxt( text = TEXT-t01 icon_id = icon_save_as_template icon_text = TEXT-t01 ).

  ENDMETHOD.                    "initialization_dat
  METHOD excel_upload.

    DATA : lv_start_col TYPE i VALUE '1',
           lv_start_row TYPE i VALUE '2',
           lv_end_col   TYPE i VALUE '256',
           lv_end_row   TYPE i VALUE '65536'.

    DATA: lt_excel_file TYPE TABLE OF alsmex_tabline,
          ls_excel_file TYPE alsmex_tabline,
          ls_exceldat   TYPE ty_exceldat.

    DATA: counter     TYPE i,
          tot_counter TYPE i,
          percentage  TYPE i,
          l_index     TYPE i.
    FIELD-SYMBOLS : <fs> TYPE any.

    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = p_fname
        i_begin_col             = lv_start_col
        i_begin_row             = lv_start_row
        i_end_col               = lv_end_col
        i_end_row               = lv_end_row
      TABLES
        intern                  = lt_excel_file
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE s001(00) WITH 'Dosya okuma hatası!' RAISING ex_errordat.
    ENDIF.

    DESCRIBE TABLE lt_excel_file LINES tot_counter.
    CLEAR: counter,percentage.
    percentage = counter / tot_counter * 100.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = percentage
        text       = 'Excel içeri alınıyor '.

    IF lt_excel_file[] IS INITIAL.
      MESSAGE s001(00) WITH 'Dosyada veri yok!' RAISING ex_errordat.
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

  ENDMETHOD.                    "excel_upload
  METHOD retrieve_data.

    excel_upload(
      EXPORTING
        iv_fname    = iv_fname
      IMPORTING
        ev_exceldat = DATA(t_exceldat)
      EXCEPTIONS
        ex_errordat = 1
        OTHERS      = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.

    SELECT ldgrp, bukrs, kostl, kstar, gjahr
      FROM zco_000_t01
      INTO TABLE @DATA(t_t01).

    LOOP AT t_exceldat ASSIGNING FIELD-SYMBOL(<exceldat>).
      APPEND INITIAL LINE TO mt_outdat ASSIGNING FIELD-SYMBOL(<outdat>).
      <outdat> = CORRESPONDING #( <exceldat> ).
      <outdat>-ldgrp = COND #( WHEN <outdat>-ldgrp IS INITIAL THEN 'ORTK' ELSE <outdat>-ldgrp ).
      <outdat>-kostl = |{ <outdat>-kostl ALPHA = IN }|.
      <outdat>-kstar = |{ <outdat>-kstar ALPHA = IN }|.
      READ TABLE t_t01 ASSIGNING FIELD-SYMBOL(<t01>) WITH KEY ldgrp = <outdat>-ldgrp bukrs = <outdat>-bukrs kostl = <outdat>-kostl kstar = <outdat>-kstar gjahr = <outdat>-gjahr.
      IF sy-subrc IS INITIAL.
        <outdat>-light = icon_green_light.
      ELSE.
        <outdat>-light = icon_yellow_light.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "retrieve_data
  METHOD popup_confirm.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = im_titlebar
        text_question         = im_question
        text_button_1         = 'Evet'
        text_button_2         = 'Hayır'
        default_button        = '2'
        display_cancel_button = abap_true
      IMPORTING
        answer                = rv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

  ENDMETHOD.                    "popup_confirm
ENDCLASS.                    "lcl_mvc_model IMPLEMENTATION
*&---------------------------------------------------------------------*
*&  Class           LCL_MVC_VIEW           Implementation
*&---------------------------------------------------------------------*
CLASS lcl_mvc_view IMPLEMENTATION.
  METHOD call_transaction.

    LOOP AT im_parameter INTO DATA(wa_param).
      SET PARAMETER ID wa_param-param_id FIELD wa_param-value.
    ENDLOOP.
    IF sy-subrc IS INITIAL AND
       im_tcode IS NOT INITIAL.
      CALL TRANSACTION im_tcode AND SKIP FIRST SCREEN.
    ENDIF.

  ENDMETHOD.                    "call_transaction
  METHOD prepare_fcatdat.

    DATA: _text(40) TYPE c.

    FREE: me->mt_fcat.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = im_strname
      CHANGING
        ct_fieldcat            = me->mt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE s001(00) WITH 'Fiedcatalog oluşturma sırasında hatalar oluştu!' RAISING contains_error.
    ENDIF.

    LOOP AT me->mt_fcat REFERENCE INTO DATA(r_fcat).
      CLEAR: _text.
      CASE r_fcat->fieldname .
        WHEN 'LIGHT'.
          _text = TEXT-f01.
          r_fcat->just = 'C'.
        WHEN 'SELKZ'.
          r_fcat->tech = abap_true.
      ENDCASE.
      IF _text <> space.
        MOVE _text TO: r_fcat->scrtext_l, r_fcat->scrtext_m, r_fcat->scrtext_s, r_fcat->reptext.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD display_alvdat.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'SET_PF_STATUS'
        i_callback_user_command  = 'USER_COMMAND'
        i_callback_top_of_page   = 'TOP_OF_PAGE'
        is_layout_lvc            = VALUE lvc_s_layo( sel_mode = 'D' box_fname = 'SELKZ' col_opt = abap_true cwidth_opt = abap_true zebra = abap_true )
        it_fieldcat_lvc          = me->mt_fcat
        it_excluding             = me->mt_exdat
        i_default                = abap_true
        i_save                   = abap_true
      TABLES
        t_outtab                 = _model->mt_outdat
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0 .
      MESSAGE s001(00) WITH 'ALV gösterimi sırasında hatalar oluştu!' RAISING contains_error.
    ENDIF.

  ENDMETHOD.
  METHOD display_applog.

    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING
        it_message = iv_msgdat.

  ENDMETHOD.
ENDCLASS.                    "lcl_mvc_view IMPLEMENTATION
*&---------------------------------------------------------------------*
*&  Class           LCL_MVC_CONTROLLER           Implementation
*&---------------------------------------------------------------------*
CLASS lcl_mvc_controller IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.                    "constructor
  METHOD instantiate_app.

    DATA: lo_object TYPE REF TO object.

    ro_controller = NEW lcl_mvc_controller( ).

    FREE: lo_object.
    CREATE OBJECT lo_object TYPE (iv_model).
    IF lo_object IS BOUND.
      ro_controller->mo_model ?= lo_object.
    ENDIF.

    FREE: lo_object.
    CREATE OBJECT lo_object TYPE (iv_view).
    IF lo_object IS BOUND.
      ro_controller->mo_view ?= lo_object.
    ENDIF.

    IF ro_controller->mo_model IS BOUND AND ro_controller->mo_view IS BOUND.
      _model ?= ro_controller->mo_model.
      _view  ?= ro_controller->mo_view.
    ENDIF.

  ENDMETHOD.                    "instantiate_app
  METHOD at_selection_screen_request.
    CASE im_fieldname.
      WHEN 'P_FNAME'.
        get_file_path(
          CHANGING
            ch_fname = p_fname ).
    ENDCASE.
  ENDMETHOD.
  METHOD at_selection_screen.
    CASE sy-ucomm.
      WHEN 'FC02'.
        DATA(t_retdat) = NEW zfi_helper_cl02( )->download_templ( im_objname = _model->mc_cons-excel ).
        IF line_exists( t_retdat[ type = 'E' ] ).
          _controller->show_message(
            EXPORTING
              it_msgdat = t_retdat ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.
  METHOD at_selection_screen_output.

  ENDMETHOD.
  METHOD get_file_path.

    DATA : lv_subrc LIKE sy-subrc,
           lt_path  TYPE filetable,
           lr_path  TYPE REF TO file_table.

    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title      = 'Select Source Excel File'
        default_extension = 'XLSX'
        initial_directory = 'C:\'
        multiselection    = abap_false
      CHANGING
        file_table        = lt_path
        rc                = lv_subrc.

    READ TABLE lt_path REFERENCE INTO lr_path INDEX 1.
    IF sy-subrc IS INITIAL.
      MOVE lr_path->filename TO ch_fname.
    ENDIF.

  ENDMETHOD.                    "get_file_path
  METHOD start_of_selection.

    mo_model->retrieve_data(
      EXPORTING
        iv_fname       = p_fname
      EXCEPTIONS
        contains_error = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4. RETURN.
    ENDIF.

    CALL SCREEN 100.

  ENDMETHOD.                    "start_of_selection
  METHOD show_message.

    cl_rmsl_message=>display(
      EXPORTING
        it_message = it_msgdat ).

  ENDMETHOD.
  METHOD popup_confirm.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = im_titlebar
        text_question         = im_question
        text_button_1         = 'Evet'
        text_button_2         = 'Hayır'
        default_button        = '2'
        display_cancel_button = abap_true
      IMPORTING
        answer                = rv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

  ENDMETHOD.                    "popup_confirm
  METHOD alv_session.

    IF NOT lines( _controller->mo_model->mt_outdat ) IS INITIAL.
      _controller->mo_view->prepare_fcatdat(
        EXPORTING
          im_strname     = _controller->mo_view->mc_strname
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      _controller->mo_view->display_alvdat(
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "alv_session
  METHOD call_rundat.

    DATA: _basedat TYPE zco_001_s01,
          t_msgdat TYPE bapiret2_tab.

    CHECK '1' = popup_confirm(
      EXPORTING
        im_titlebar = TEXT-t02
        im_question = TEXT-t03 ).

    LOOP AT mo_model->mt_outdat ASSIGNING FIELD-SYMBOL(<outdat>) WHERE selkz EQ abap_true AND ( light NE icon_green_light AND light NE icon_alert ).
      FREE: _basedat.
      _basedat = CORRESPONDING #( <outdat> ).

      INSERT zco_000_t01 FROM TABLE @(
        VALUE #( ( ldgrp = _basedat-ldgrp
                   bukrs = _basedat-bukrs
                   kostl = _basedat-kostl
                   kstar = _basedat-kstar
                   gjahr = _basedat-gjahr ) ) ).
      IF line_exists( t_msgdat[ type = 'E' ] ).
        <outdat>-light = icon_red_light.
        APPEND LINES OF t_msgdat TO rt_msgdat.
      ELSE.
        <outdat>-light = icon_green_light.
      ENDIF.
    ENDLOOP.
    IF NOT sy-subrc IS INITIAL.
      MESSAGE e001(00) WITH 'Aktarım için uygun satır seçimi yapınız!' RAISING contains_error.
    ENDIF.

  ENDMETHOD.                    "call_rundat
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

ENDCLASS.                    "lcl_mvc_controller IMPLEMENTATION