*&---------------------------------------------------------------------*
*& Include          ZFI_023_P01_CLSDAT
*&---------------------------------------------------------------------*
TABLES: sscrfields.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
  PARAMETERS:
    p_bukrs LIKE t001-bukrs      OBLIGATORY MEMORY ID buk DEFAULT '1000',
    p_fname LIKE rlgrap-filename OBLIGATORY MEMORY ID zfi_028_fname DEFAULT 'C:\Users\FINPRO\OneDrive\Masaüstü\zfi_023_tem01.xlsx'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN FUNCTION KEY 1.

CLASS: lcl_controller DEFINITION DEFERRED,
       lcl_model DEFINITION DEFERRED,
       lcl_view DEFINITION DEFERRED.

TYPES BEGIN OF ty_list.
INCLUDE TYPE zfi_023_s01.
TYPES END OF ty_list.

TYPES:BEGIN OF ty_exceldat,
        anlkl TYPE char50,
        bukrs TYPE char50,
        ranl1 TYPE char50,
        ranl2 TYPE char50,
        txt50 TYPE char50,
        sernr TYPE char50,
        belnr TYPE char50,
        bldat TYPE char50,
        budat TYPE char50,
        bzdat TYPE char50,
        sgtxt TYPE char50,
        blart TYPE char50,
        anbtr TYPE char50,
        waers TYPE char50,
        menge TYPE char50,
        meins TYPE char50,
        prozs TYPE char50,
      END OF ty_exceldat.

TYPES:
 tt_exceldat TYPE STANDARD TABLE OF ty_exceldat WITH DEFAULT KEY .

DATA: mt_exceldat TYPE tt_exceldat,
      ev_exceldat TYPE tt_exceldat.

DATA: _controller TYPE REF TO lcl_controller,
      _model      TYPE REF TO lcl_model,
      _view       TYPE REF TO lcl_view.

*----------------------------------------------------------------------*
* CLASS lcx_exception DEFINITION
*----------------------------------------------------------------------*
CLASS lcx_exception DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    INTERFACES if_t100_message .


    METHODS: constructor
      IMPORTING
        iv_id     TYPE symsgid
        iv_number TYPE symsgno
        iv_msg_v1 TYPE symsgv OPTIONAL
        iv_msg_v2 TYPE symsgv OPTIONAL
        iv_msg_v3 TYPE symsgv OPTIONAL
        iv_msg_v4 TYPE symsgv OPTIONAL.

ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcx_exception IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcx_exception IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    if_t100_message~t100key-msgid = iv_id.
    if_t100_message~t100key-msgno = iv_number.
    if_t100_message~t100key-attr1 = iv_msg_v1.
    if_t100_message~t100key-attr2 = iv_msg_v2.
    if_t100_message~t100key-attr3 = iv_msg_v3.
    if_t100_message~t100key-attr4 = iv_msg_v4.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_model DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_model DEFINITION.

  PUBLIC SECTION.

    TYPES:
      ty_outdat TYPE STANDARD TABLE OF zfi_023_s01.

    DATA:
      mt_outdat TYPE ty_outdat.

    METHODS:
      retrieve_data
        IMPORTING
          iv_bukrs TYPE bukrs
          iv_fname TYPE rlgrap-filename
        EXCEPTIONS
          contains_error,
      excel_upload
        IMPORTING
          im_fname    TYPE rlgrap-filename
        EXPORTING
          ev_exceldat TYPE tt_exceldat
        RAISING
          lcx_exception,
      excel_to_sap_date
        CHANGING
          cv_value TYPE clike
        EXCEPTIONS
          cx_error,
      excel_to_sap_amount
        CHANGING
          cv_value TYPE clike
        EXCEPTIONS
          cx_error.

ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_model IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_model IMPLEMENTATION .

  METHOD retrieve_data.

    TRY.
        excel_upload(
          EXPORTING
            im_fname       = p_fname
          IMPORTING
            ev_exceldat    = mt_exceldat ).

        mt_outdat = CORRESPONDING #( mt_exceldat ).

      CATCH lcx_exception INTO DATA(lo_exc).
        MESSAGE ID lo_exc->if_t100_message~t100key-msgid
        TYPE 'I'
        NUMBER lo_exc->if_t100_message~t100key-msgno
        WITH lo_exc->if_t100_message~t100key-attr1.
    ENDTRY.



  ENDMETHOD.                    "retrieve_data

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
        filename                = im_fname
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
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING
          iv_id     = '00'
          iv_number = '001'
          iv_msg_v1 = 'Dosya okuma hatası!'.
    ENDIF.

    DESCRIBE TABLE lt_excel_file LINES tot_counter.
    CLEAR: counter,percentage.
    percentage = counter / tot_counter * 100.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = percentage
        text       = 'Excel içeri alınıyor '.

    IF lt_excel_file[] IS INITIAL.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING
          iv_id     = '00'
          iv_number = '001'
          iv_msg_v1 = 'Dosyada veri yok!'.
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
    DATA: i_error.

    LOOP AT ev_exceldat ASSIGNING FIELD-SYMBOL(<exceldat>).

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input          = <exceldat>-meins
          language       = sy-langu
        IMPORTING
          output         = <exceldat>-meins
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      excel_to_sap_date(
        CHANGING
          cv_value = <exceldat>-bldat
        EXCEPTIONS
          cx_error = 1
          OTHERS   = 2 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            iv_id     = '00'
            iv_number = '001'
            iv_msg_v1 = 'Belge tarihi alanı hatalı!!'.
      ENDIF.

      excel_to_sap_date(
        CHANGING
          cv_value = <exceldat>-budat
        EXCEPTIONS
          cx_error = 1
          OTHERS   = 2 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            iv_id     = '00'
            iv_number = '001'
            iv_msg_v1 = 'Kayıt tarihi alanı hatalı!!'.
      ENDIF.

      excel_to_sap_date(
        CHANGING
          cv_value = <exceldat>-bzdat
        EXCEPTIONS
          cx_error = 1
          OTHERS   = 2 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            iv_id     = '00'
            iv_number = '001'
            iv_msg_v1 = 'Referans belge tarihi alanı hatalı!!'.
      ENDIF.

      excel_to_sap_amount(
        CHANGING
          cv_value = <exceldat>-anbtr
        EXCEPTIONS
          cx_error = 1
          OTHERS   = 2 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            iv_id     = '00'
            iv_number = '001'
            iv_msg_v1 = 'Tutar alanı hatalı!!'.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD excel_to_sap_date.

    DATA: _error TYPE char1,
          _idate TYPE datum.

    CALL FUNCTION 'CONVERT_DATE_TO_INTERN_FORMAT'
      EXPORTING
        datum = cv_value
        dtype = 'DATS'
      IMPORTING
        error = _error
        idate = _idate.
    IF _error IS INITIAL.
      cv_value = _idate.
    ELSE.
      RAISE cx_error.
    ENDIF.

  ENDMETHOD.

  METHOD excel_to_sap_amount.

    DATA: _wrbtr TYPE bseg-wrbtr.

    TRY.
        TRANSLATE cv_value USING '. '.
        TRANSLATE cv_value USING ',.'.
        CONDENSE cv_value NO-GAPS.

        MOVE cv_value TO _wrbtr.

      CATCH cx_root.
        RAISE cx_error.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_view DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_view DEFINITION FINAL.

  PUBLIC SECTION.
    CONSTANTS:
      mc_strname TYPE tabname VALUE 'ZFI_023_S01'.

    DATA:
      mt_fcat  TYPE lvc_t_fcat,
      mt_exdat TYPE slis_t_extab.

    METHODS:
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
        is_layout_lvc            = VALUE lvc_s_layo( col_opt = abap_true cwidth_opt = abap_true zebra = abap_true box_fname = 'SELKZ' )
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

ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_controller DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_controller DEFINITION FINAL.

  PUBLIC SECTION.

    CONSTANTS:
      mc_model TYPE seoclsname VALUE 'LCL_MODEL',
      mc_view  TYPE seoclsname VALUE 'LCL_VIEW'.

    CONSTANTS:
      BEGIN OF mc_msg,
        id      TYPE symsgid    VALUE '00',
        success TYPE bapi_mtype VALUE 'S',
        error   TYPE bapi_mtype VALUE 'E',
        warning TYPE bapi_mtype VALUE 'W',
        info    TYPE bapi_mtype VALUE 'I',
        abort   TYPE bapi_mtype VALUE 'A',
      END OF mc_msg.

    DATA:
      mo_model TYPE REF TO lcl_model,
      mo_view  TYPE REF TO lcl_view.

    DATA:
      gt_alv        TYPE TABLE OF ty_list,
      gt_file       TYPE STANDARD TABLE OF file_table,
      gt_excel_data TYPE TABLE OF ty_list,
      d_ok,
      gv_count      TYPE i,
      d_repname     LIKE sy-repid,
      gv_rc         TYPE i.

    METHODS:
      instantiate_app
        IMPORTING
          iv_model             TYPE seoclsname
          iv_view              TYPE seoclsname
        RETURNING
          VALUE(ro_controller) TYPE REF TO lcl_controller,
      initialization,
      at_selection_screen_output,
      at_selection_value,
      at_selection_screen,
      download_template,
      asset_create
        IMPORTING
          VALUE(im_key)                   TYPE bapi1022_key
          VALUE(im_reference)             TYPE bapi1022_reference
          VALUE(im_general_data)          TYPE bapi1022_feglg001
          VALUE(im_general_data_x)        TYPE bapi1022_feglg001x
          VALUE(im_time_dependent_data)   TYPE bapi1022_feglg003
          VALUE(im_time_dependent_data_x) TYPE bapi1022_feglg003x
        EXPORTING
          im_comcode                      TYPE bapi1022_1-comp_code
          im_asset                        TYPE bapi1022_1-assetmaino
          im_subnumber                    TYPE bapi1022_1-assetsubno
          im_assetcreated                 TYPE bapi1022_reference
        RETURNING
          VALUE(rt_return)                TYPE bapiret2,
      save
        RETURNING
          VALUE(rt_retdat) TYPE bapiret2_t
        EXCEPTIONS
          contains_error,
      display_message
        IMPORTING
          it_msgdat TYPE bapiret2_tab
        EXCEPTIONS
          contains_error,
      display_popup_message
        IMPORTING
          !im_batch         TYPE abap_bool DEFAULT abap_false
          !im_titlebar      TYPE char20
          !im_text_question TYPE char100
        EXPORTING
          ev_answer         TYPE char1,
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

  METHOD initialization.

    DATA: functxt TYPE smp_dyntxt.
    CLEAR: functxt.
    functxt-text = 'Şablonu İndir'.
    functxt-icon_id = '@J2@'.
    functxt-icon_text = 'Şablonu İndir'.
    sscrfields-functxt_01 = functxt.

  ENDMETHOD.

  METHOD at_selection_screen_output.


  ENDMETHOD.

  METHOD at_selection_value.

    DATA: lv_error(50),
          lv_subrc(4),
          ls_file  TYPE file_table .

    CLEAR:
      gt_file, gt_file[].

    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title            = 'Select a file'
        default_extension       = 'XLSX'
        default_filename        = 'C:\file.xlsx'
        initial_directory       = 'C:\'
      CHANGING
        file_table              = gt_file
        rc                      = gv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.
    IF sy-subrc <> 0.
      lv_subrc = sy-subrc.
      CONCATENATE 'Dosya seçim hatası. Hata Kodu : ' lv_subrc INTO lv_error.
      MESSAGE  lv_error  TYPE 'S'.
      EXIT.
    ENDIF.
    READ TABLE gt_file INTO ls_file INDEX 1.
    MOVE ls_file-filename TO p_fname.

  ENDMETHOD.

  METHOD at_selection_screen.
    CASE sy-ucomm.
      WHEN 'FC01'.
        _controller->download_template( ).
        RETURN.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
  METHOD asset_create.
    CLEAR rt_return.
    CALL FUNCTION 'BAPI_FIXEDASSET_CREATE1'
      EXPORTING
        key                = im_key
        reference          = im_reference
        generaldata        = im_general_data
        generaldatax       = im_general_data_x
        timedependentdata  = im_time_dependent_data
        timedependentdatax = im_time_dependent_data_x
      IMPORTING
        companycode        = im_comcode
        asset              = im_asset
        subnumber          = im_subnumber
        assetcreated       = im_assetcreated
        return             = rt_return.

    IF rt_return-type CA 'EAX'.
      EXIT.
    ENDIF.

    IF sy-subrc IS INITIAL AND im_assetcreated IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ENDMETHOD.

  METHOD download_template.

    DATA(t_retdat) = NEW zfi_helper_cl02( )->download_templ(
      EXPORTING
        im_objname = 'ZFI_023_TEMP01' ).
    IF line_exists( t_retdat[ type = 'E' ] ).
      MESSAGE TEXT-m03 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.
  METHOD save.

    TYPES:
      BEGIN OF ty_bapidat,
        key             TYPE bapi1022_key,
        ls_reference    TYPE bapi1022_reference,
        ls_gendata      TYPE bapi1022_feglg001,
        ls_gendata_x    TYPE bapi1022_feglg001x,
        ls_timedep      TYPE bapi1022_feglg003,
        ls_timedepx     TYPE bapi1022_feglg003x,
        ls_companycode  TYPE bapi1022_1-comp_code,
        ls_asset        TYPE bapi1022_1-assetmaino,
        ls_subnumber    TYPE bapi1022_1-assetsubno,
        ls_assetcreated TYPE bapi1022_reference,
        ls_return       TYPE bapiret2,
        ls_depareas     TYPE bapi1022_dep_areas,
        ls_depareasx    TYPE bapi1022_dep_areasx,
        ls_extensionin  TYPE bapiparex,
        lt_extensionin  TYPE STANDARD TABLE OF bapiparex WITH DEFAULT KEY,
      END OF ty_bapidat.

    DATA: _bapidat TYPE ty_bapidat.

    LOOP AT _controller->mo_model->mt_outdat ASSIGNING FIELD-SYMBOL(<outdat>) WHERE selkz EQ abap_true.

               _bapidat-key = VALUE #( asset = <outdat>-ranl1
                                       companycode = <outdat>-bukrs
                                       subnumber = <outdat>-ranl2 ).

      _bapidat-ls_reference = VALUE #( asset = <outdat>-ranl1
                                       companycode = <outdat>-bukrs
                                       subnumber = <outdat>-ranl2 ).

      _bapidat-ls_gendata   = VALUE #( assetclass = <outdat>-anlkl
                                       descript = <outdat>-txt50
                                       serial_no = <outdat>-sernr
                                       quantity = 1
                                       base_uom = <outdat>-meins ).


      _bapidat-ls_gendata_x = VALUE #( assetclass = 'X'
                                       descript   = 'X'
                                       serial_no  = 'X'
                                       quantity   = 'X'
                                       base_uom   = 'X' ).

      _bapidat-ls_return = me->asset_create(
                             EXPORTING
                               im_key                   = _bapidat-key
                               im_reference             = _bapidat-ls_reference
                               im_general_data          = _bapidat-ls_gendata
                               im_general_data_x        = _bapidat-ls_gendata_x
                               im_time_dependent_data   = _bapidat-ls_timedep
                               im_time_dependent_data_x = _bapidat-ls_timedepx
                             IMPORTING
                               im_comcode               = _bapidat-ls_companycode
                               im_asset                 = _bapidat-ls_asset
                               im_subnumber             = _bapidat-ls_subnumber
                               im_assetcreated          = _bapidat-ls_assetcreated ).

      IF NOT _bapidat-ls_return-type = mc_msg-error.
        <outdat>-anln1 = _bapidat-ls_assetcreated-asset.
        <outdat>-anln2 = _bapidat-ls_assetcreated-subnumber.
      ENDIF.

      APPEND _bapidat-ls_return TO rt_retdat.

    ENDLOOP.
  ENDMETHOD.

  METHOD display_message.

    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING
        it_message = it_msgdat.

  ENDMETHOD.

  METHOD display_popup_message.

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


  ENDMETHOD.

  METHOD alv_session.

    IF lines( _controller->mo_model->mt_outdat ) IS INITIAL.
      MESSAGE s001(00) WITH 'Seçim kriterlerine uygun veri bulunamadı!' RAISING contains_error.
    ENDIF.

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

  ENDMETHOD.

ENDCLASS.