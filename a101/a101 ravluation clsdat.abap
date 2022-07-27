*&---------------------------------------------------------------------*
*& Include          ZFI_P_ASSET_REVALUATION_CLSDAT
*&---------------------------------------------------------------------*
TYPE-POOLS: slis, icon.
TABLES: sscrfields, anla.

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-s00.
  PARAMETERS:
    p_rb1 RADIOBUTTON GROUP grp1 DEFAULT 'X' USER-COMMAND cmd1,
    p_rb2 RADIOBUTTON GROUP grp1.
SELECTION-SCREEN END OF BLOCK b0.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
  PARAMETERS:
    p_fname TYPE rlgrap-filename OBLIGATORY DEFAULT 'C:\' MODIF ID md1,
    p_bukrs TYPE t001-bukrs OBLIGATORY DEFAULT '1001' MODIF ID md2.
  SELECT-OPTIONS:
    s_anln1 FOR anla-anln1 MODIF ID md2,
    s_anln2 FOR anla-anln2 MODIF ID md2.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS:
    p_xlog AS CHECKBOX DEFAULT abap_true MODIF ID md2.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS:
   p_xjob AS CHECKBOX MODIF ID md2.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN: FUNCTION KEY 2.
SELECTION-SCREEN: FUNCTION KEY 3.

CLASS: lcl_controller DEFINITION DEFERRED,
       lcl_model DEFINITION DEFERRED,
       lcl_view DEFINITION DEFERRED.

DATA: _controller TYPE REF TO lcl_controller,
      _model      TYPE REF TO lcl_model,
      _view       TYPE REF TO lcl_view.

*----------------------------------------------------------------------*
* CLASS lcl_model DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_model DEFINITION.


  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF mc_cons,
        p_fname TYPE char10 VALUE 'P_FNAME',
        excel   TYPE w3objid VALUE 'ZFI_ASSET_REVALUATION_EXCEL_TEMP',
      END OF mc_cons.

    CONSTANTS:
      BEGIN OF mc_logger,
        _object    TYPE balobj_d VALUE 'ZASSET_REV',
        _subobject TYPE balsubobj VALUE '',
      END OF mc_logger.

    CONSTANTS:
      BEGIN OF mc_msg,
        id      TYPE symsgid    VALUE '00',
        success TYPE bapi_mtype VALUE 'S',
        error   TYPE bapi_mtype VALUE 'E',
        warning TYPE bapi_mtype VALUE 'W',
        info    TYPE bapi_mtype VALUE 'I',
        abort   TYPE bapi_mtype VALUE 'A',
      END OF mc_msg.


    TYPES:
      BEGIN OF ty_exceldat,
        username      TYPE char50,
        doc_date      TYPE char50,
        pstng_date    TYPE char50,
        fis_period    TYPE char50,
        trans_date    TYPE char50,
        comp_code     TYPE char50,
        assetmaino    TYPE char50,
        assetsubno    TYPE char50,
        assettrtyp    TYPE char50,
        depr_area     TYPE char50,
        ledger_group  TYPE char50,
        acc_principle TYPE char50,
        amount        TYPE char50,
        valuedate     TYPE char50,
        ord_rev_cu    TYPE char50,
        currency      TYPE char50,
        item_text     TYPE char50,
      END OF ty_exceldat,
      mtt_exceldat TYPE STANDARD TABLE OF ty_exceldat.

    TYPES:
      mtt_anln1_rng TYPE RANGE OF anla-anln1,
      mtt_anln2_rng TYPE RANGE OF anla-anln2.

    DATA:
      mt_outdat TYPE STANDARD TABLE OF zfi_s_asset_revaluation_alvdat.

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
          iv_xjob TYPE xfeld
        EXCEPTIONS
          contains_error,
      run_excel_dat
        IMPORTING
          iv_fname TYPE rlgrap-filename
        EXCEPTIONS
          contains_error,
      run_process_dat
        IMPORTING
          iv_bukrs TYPE bukrs
          iv_anln1 TYPE mtt_anln1_rng
          iv_anln2 TYPE mtt_anln2_rng
        EXCEPTIONS
          contains_error,
      conv_price_excel_to_sap
        CHANGING
          cv_price TYPE char50,
      conv_date_excel_to_sap
        CHANGING
          cv_date TYPE char50,
      popup_confirm
        IMPORTING
          im_titlebar      TYPE clike
          im_question      TYPE clike
        RETURNING
          VALUE(rv_answer) TYPE char1,
      run_bapidat
        IMPORTING
          iv_xjob          TYPE xfeld OPTIONAL
        RETURNING
          VALUE(rt_msgdat) TYPE bapiret2_tab
        EXCEPTIONS
          ex_rassing.

ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_model IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_model IMPLEMENTATION .

  METHOD initialization_dat.

    sscrfields-functxt_02 = VALUE smp_dyntxt( text = TEXT-t01 icon_id = icon_save_as_template icon_text = TEXT-t01 ).
    sscrfields-functxt_03 = VALUE smp_dyntxt( text = TEXT-t06 icon_id = icon_office_document icon_text = TEXT-t06 ).

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

    IF iv_xjob EQ abap_false.
      CASE abap_true.
        WHEN p_rb1.
          run_excel_dat(
            EXPORTING
              iv_fname       = p_fname
            EXCEPTIONS
              contains_error = 1
              OTHERS         = 2 ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
          ENDIF.
        WHEN p_rb2.
          run_process_dat(
            EXPORTING
              iv_bukrs       = p_bukrs
              iv_anln1       = s_anln1[]
              iv_anln2       = s_anln2[]
            EXCEPTIONS
              contains_error = 1
              OTHERS         = 2 ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ELSE.
      IF sy-batch EQ abap_false.
        MESSAGE s001(00) WITH 'Lütfen programı artlanda çalıştırınız!' RAISING contains_error.
      ENDIF.

      run_process_dat(
        EXPORTING
          iv_bukrs       = p_bukrs
          iv_anln1       = s_anln1[]
          iv_anln2       = s_anln2[]
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      run_bapidat(
        EXPORTING
          iv_xjob = iv_xjob
        EXCEPTIONS
          ex_rassing = 1
          OTHERS     = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "retrieve_data
  METHOD run_excel_dat.

    DATA: t_logdat TYPE STANDARD TABLE OF zfi_t_asset_reva.

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

    SELECT COUNT( * )
      FROM zfi_t_asset_reva
        INTO @DATA(_count). "#EC CI_NOWHERE
    IF _count GT 0.
      CHECK popup_confirm(
        EXPORTING
          im_titlebar = TEXT-t04
          im_question = TEXT-t05 ) EQ '1'.
    ENDIF.

    FREE: t_logdat.
    LOOP AT t_exceldat ASSIGNING FIELD-SYMBOL(<exceldat>).
      conv_price_excel_to_sap( CHANGING cv_price = <exceldat>-amount ).
      conv_price_excel_to_sap( CHANGING cv_price = <exceldat>-ord_rev_cu ).

      conv_date_excel_to_sap( CHANGING cv_date = <exceldat>-doc_date ).
      conv_date_excel_to_sap( CHANGING cv_date = <exceldat>-pstng_date ).
      conv_date_excel_to_sap( CHANGING cv_date = <exceldat>-trans_date ).
      conv_date_excel_to_sap( CHANGING cv_date = <exceldat>-valuedate ).

      APPEND INITIAL LINE TO t_logdat ASSIGNING FIELD-SYMBOL(<logdat>).
      <logdat> = CORRESPONDING #( <exceldat> ).
    ENDLOOP.

    DELETE FROM zfi_t_asset_reva. "#EC CI_NOWHERE
    MODIFY zfi_t_asset_reva FROM TABLE t_logdat.
    IF sy-subrc IS INITIAL.
      COMMIT WORK AND WAIT.
      MESSAGE i001(00) WITH 'Kayıtlar başarıyla aktarıldı.'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE s001(00) WITH 'Kayıt aktarımı sırasında hatalar oluştu!' RAISING contains_error.
    ENDIF.

  ENDMETHOD.                    "run_excel_dat
  METHOD run_process_dat.

    SELECT
      '@09@' AS light,
      comp_code,
      assetmaino,
      assetsubno,
      username,
      doc_date,
      pstng_date,
      fis_period,
      trans_date,
      assettrtyp,
      depr_area,
      ledger_group,
      acc_principle,
      amount,
      valuedate,
      ord_rev_cu,
      currency,
      item_text,
      obj_key,
      '@WA@' AS msgshow
      FROM zfi_t_asset_reva
      INTO CORRESPONDING FIELDS OF TABLE @mt_outdat
      WHERE comp_code EQ @iv_bukrs
        AND assetmaino IN @iv_anln1
        AND assetsubno IN @iv_anln2
        AND mtype IN ( 'E', '' ).
    IF NOT sy-subrc IS INITIAL.
      MESSAGE s001(00) WITH 'Seçim kriterlerine uygun veri bulunamadı!' RAISING contains_error.
    ENDIF.

  ENDMETHOD.                    "run_process_dat
  METHOD conv_price_excel_to_sap.

    TRANSLATE cv_price USING '. '.
    TRANSLATE cv_price USING ',.'.
    CONDENSE cv_price NO-GAPS.

  ENDMETHOD.                    "conv_price_excel_to_sap
  METHOD conv_date_excel_to_sap.

    CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
      EXPORTING
        input  = cv_date
      IMPORTING
        output = cv_date.

  ENDMETHOD.                    "conv_date_excel_to_sap
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
  METHOD run_bapidat.

    DATA: _generalpostingdata TYPE bapifapo_gen_info,
          _revaluationdata    TYPE bapifapo_revaluation,
          _furtherpostingdata TYPE bapifapo_add_info,
          _documentreference  TYPE bapifapo_doc_ref,
          t_revalareavalues   TYPE STANDARD TABLE OF bapifapo_areavalues_reval,
          t_return_all        TYPE STANDARD TABLE OF bapiret2.

    IF iv_xjob EQ abap_false.
      CHECK popup_confirm(
        EXPORTING
          im_titlebar = CONV #( TEXT-t02 )
          im_question = CONV #( TEXT-t03 ) ) EQ '1'.
    ELSE.
      MODIFY mt_outdat FROM VALUE #( selkz = abap_true )
        TRANSPORTING selkz
          WHERE light EQ icon_yellow_light OR light EQ icon_red_light.
    ENDIF.


    LOOP AT mt_outdat ASSIGNING FIELD-SYMBOL(<outdat>)
          WHERE selkz EQ abap_true
            AND light NE icon_green_light.

      IF p_xlog EQ abap_true.
        DATA(_logger) = ybc_logger_toolkit=>get_instance(
          EXPORTING
            iv_log_object    = mc_logger-_object
            iv_log_subobject = mc_logger-_subobject
            iv_extnumber     = CONV #( |{ <outdat>-comp_code }_{ <outdat>-assetmaino }_{ <outdat>-assetsubno }| ) ).
      ENDIF.

      FREE: <outdat>-msgdat, _generalpostingdata.
      _generalpostingdata = VALUE #(
        username = <outdat>-username
        doc_date = <outdat>-doc_date
        pstng_date = <outdat>-pstng_date
        fis_period = <outdat>-fis_period
        trans_date = <outdat>-trans_date
        comp_code = <outdat>-comp_code
        assetmaino = <outdat>-assetmaino
        assetsubno = <outdat>-assetsubno
        assettrtyp = <outdat>-assettrtyp
        depr_area = <outdat>-depr_area
        ledger_group = <outdat>-ledger_group
        acc_principle = <outdat>-acc_principle ).

      FREE: _revaluationdata.
      _revaluationdata = VALUE #(
        amount = <outdat>-amount
        valuedate = <outdat>-valuedate
        ord_rev_cu = <outdat>-ord_rev_cu
        currency = <outdat>-currency ).

      FREE: _furtherpostingdata.
      _furtherpostingdata = VALUE #(
        item_text = <outdat>-item_text ).

      FREE: t_revalareavalues.
      APPEND VALUE #(
        depr_area = <outdat>-depr_area
        amount = <outdat>-amount
        ord_rev_cu = ( <outdat>-ord_rev_cu * -1 )
        currency = <outdat>-currency ) TO t_revalareavalues.

      APPEND VALUE #(
        depr_area = '62'
        currency = 'USD' ) TO t_revalareavalues.

      APPEND VALUE #(
        depr_area = '63'
        currency = 'EUR' ) TO t_revalareavalues.

      CALL FUNCTION 'BAPI_ASSET_REVALUATION_POST'
        EXPORTING
          generalpostingdata = _generalpostingdata
          revaluationdata    = _revaluationdata
          furtherpostingdata = _furtherpostingdata
        IMPORTING
          documentreference  = _documentreference
        TABLES
          revalareavalues    = t_revalareavalues
          return_all         = t_return_all.

      APPEND LINES OF t_return_all TO rt_msgdat.

      IF NOT line_exists( t_return_all[ type = mc_msg-error ] ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
        <outdat>-light = icon_green_light.
        <outdat>-obj_key = _documentreference-obj_key.

        UPDATE zfi_t_asset_reva
          SET obj_key = <outdat>-obj_key mtype = 'S'
            WHERE comp_code EQ <outdat>-comp_code
              AND assetmaino EQ <outdat>-assetmaino
              AND assetsubno EQ <outdat>-assetsubno.

        IF p_xlog EQ abap_true.
          _logger->add_success( iv_msgid = '00' iv_msgno = '001' iv_msgv1 = 'Başarıyla işlendi.' ).
          _logger->add_bulk_msg(
            EXPORTING
              iv_msgdat = t_return_all ).
        ENDIF.

        INSERT LINES OF t_return_all INTO TABLE <outdat>-msgdat.
      ELSE.
        ROLLBACK WORK.
        <outdat>-light = icon_red_light.

        UPDATE zfi_t_asset_reva
          SET mtype = 'E'
            WHERE comp_code EQ <outdat>-comp_code
              AND assetmaino EQ <outdat>-assetmaino
              AND assetsubno EQ <outdat>-assetsubno.

        IF p_xlog EQ abap_true.
          _logger->add_error( iv_msgid = '00' iv_msgno = '001' iv_msgv1 = 'Hata oluştu!' ).
          _logger->add_bulk_msg(
            EXPORTING
              iv_msgdat = t_return_all ).
        ENDIF.

        INSERT LINES OF t_return_all INTO TABLE <outdat>-msgdat.
      ENDIF.
    ENDLOOP.
    IF NOT sy-subrc IS INITIAL.
      MESSAGE s001(00) WITH 'Kayıt için uygun satır seçimi yapınız!' DISPLAY LIKE 'E'.
    ELSE.
      IF p_xlog EQ abap_true.
        _logger->post( iv_refresh = abap_true iv_commit = abap_true ).
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "run_bapidat

ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_view DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_view DEFINITION FINAL.

  PUBLIC SECTION.
    CONSTANTS:
      mc_strname TYPE tabname VALUE 'ZFI_S_ASSET_REVALUATION_ALVDAT'.

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

ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_view IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_view IMPLEMENTATION .

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
        WHEN 'MSGSHOW'.
          _text = TEXT-f02.
          r_fcat->hotspot = abap_true.
          r_fcat->just = 'C'.
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

ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_controller DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_controller DEFINITION FINAL.

  PUBLIC SECTION.

    CONSTANTS:
      mc_model TYPE seoclsname VALUE 'LCL_MODEL',
      mc_view  TYPE seoclsname VALUE 'LCL_VIEW'.

    DATA:
      mo_model TYPE REF TO lcl_model,
      mo_view  TYPE REF TO lcl_view.

    METHODS:
      instantiate_app
        IMPORTING
          iv_model             TYPE seoclsname
          iv_view              TYPE seoclsname
        RETURNING
          VALUE(ro_controller) TYPE REF TO lcl_controller,
      at_selection_screen_request
        IMPORTING
          im_fieldname TYPE char10,
      at_selection_screen,
      at_selection_screen_output,
      get_file_path
        CHANGING
          ch_fname TYPE rlgrap-filename,
      popup_confirm
        IMPORTING
          im_titlebar      TYPE clike
          im_question      TYPE clike
        RETURNING
          VALUE(rv_answer) TYPE char1,
      show_message
        IMPORTING
          it_msgdat TYPE bapiret2_tab,
      alv_session
        EXCEPTIONS
          contains_error.

ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_controller IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_controller IMPLEMENTATION.

  METHOD instantiate_app.

    DATA: lo_object TYPE REF TO object.

    ro_controller = NEW lcl_controller( ).

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

  ENDMETHOD.
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
        DATA(t_retdat) = NEW zcl_bc_file_templ_toolkit( )->download_templ( im_objname = _model->mc_cons-excel ).
        IF line_exists( t_retdat[ type = 'E' ] ).
          _controller->show_message(
            EXPORTING
              it_msgdat = t_retdat ).
        ENDIF.
      WHEN 'FC03'.
        SUBMIT zfi_p_asset_revalog VIA SELECTION-SCREEN.
    ENDCASE.
  ENDMETHOD.
  METHOD at_selection_screen_output.

    LOOP AT SCREEN.
      CASE abap_true.
        WHEN p_rb1.
          IF screen-group1 EQ 'MD2'.
            screen-active = 0.
          ENDIF.
        WHEN p_rb2.
          IF screen-group1 EQ 'MD1'.
            screen-active = 0.
          ENDIF.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.

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
  METHOD show_message.

    cl_rmsl_message=>display(
      EXPORTING
        it_message = it_msgdat ).

  ENDMETHOD.
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

  ENDMETHOD.

ENDCLASS.