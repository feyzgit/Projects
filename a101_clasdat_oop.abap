*&---------------------------------------------------------------------*
*& Include          YLOOMIS_P01_CLSDAT
*&---------------------------------------------------------------------*
TYPE-POOLS: slis, icon.
TABLES: yloomis_t02.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
  PARAMETERS:
    p_bukrs TYPE yloomis_t02-bukrs OBLIGATORY DEFAULT '1001'.

  SELECT-OPTIONS:
    s_prsid FOR yloomis_t02-prs_id,
    s_pkind FOR yloomis_t02-prs_kind.

  SELECTION-SCREEN SKIP 1.
  SELECT-OPTIONS:
    s_erdat FOR yloomis_t02-erdat NO-EXTENSION OBLIGATORY,
    s_erzet FOR yloomis_t02-erzet NO-EXTENSION OBLIGATORY,
    s_ernam FOR yloomis_t02-ernam NO INTERVALS.

  SELECTION-SCREEN SKIP 1.
  PARAMETERS: p_rb1 RADIOBUTTON GROUP grp1 DEFAULT 'X',
              p_rb2 RADIOBUTTON GROUP grp1.

SELECTION-SCREEN END OF BLOCK b1.

CLASS: lcl_controller DEFINITION DEFERRED,
       lcl_model DEFINITION DEFERRED,
       lcl_view DEFINITION DEFERRED.

TYPES: ty_prsid TYPE RANGE OF yloomis_t02-prs_id,
       ty_pkind TYPE RANGE OF yloomis_t02-prs_kind,
       ty_erdat TYPE RANGE OF yloomis_t02-erdat,
       ty_erzet TYPE RANGE OF yloomis_t02-erzet,
       ty_ernam TYPE RANGE OF yloomis_t02-ernam.


DATA: _controller TYPE REF TO lcl_controller,
      _model      TYPE REF TO lcl_model,
      _view       TYPE REF TO lcl_view.

*----------------------------------------------------------------------*
* CLASS lcl_model DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_model DEFINITION.

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF mc_logger,
        _object    TYPE balobj_d VALUE 'YLOOMIS',
        _subobject TYPE balsubobj VALUE '',
      END OF mc_logger .

    CONSTANTS:
      BEGIN OF mc_msg,
        id      TYPE symsgid    VALUE 'YLOOMIS',
        success TYPE bapi_mtype VALUE 'S',
        error   TYPE bapi_mtype VALUE 'E',
        warning TYPE bapi_mtype VALUE 'W',
        info    TYPE bapi_mtype VALUE 'I',
        abort   TYPE bapi_mtype VALUE 'A',
      END OF mc_msg.

    TYPES: BEGIN OF ty_rev,
             bukrs TYPE bkpf-bukrs,
             belnr TYPE bkpf-belnr,
             gjahr TYPE bkpf-gjahr,
             stblg TYPE bkpf-stblg,
             stjah TYPE bkpf-stjah,
           END OF ty_rev.

    DATA:
      mt_outdat TYPE STANDARD TABLE OF yloomis_s04,
      mt_revdat TYPE SORTED TABLE OF ty_rev WITH NON-UNIQUE KEY bukrs, belnr, gjahr.

    METHODS:
      initialization_dat
        EXCEPTIONS
          contains_error,
      retrieve_data
        IMPORTING
          !iv_bukrs TYPE bukrs
          !iv_prsid TYPE ty_prsid
          !iv_pkind TYPE ty_pkind
          !iv_erdat TYPE ty_erdat
          !iv_erzet TYPE ty_erzet
          !iv_ernam TYPE ty_ernam
          !iv_mtype TYPE bapi_mtype OPTIONAL
        EXCEPTIONS
          contains_error,
      application_logdat
        IMPORTING
          !iv_object       TYPE balobj_d
          !iv_subobject    TYPE balsubobj
          !iv_extnumber    TYPE balnrext
        RETURNING
          VALUE(rt_msgdat) TYPE bapiret2_tab
        EXCEPTIONS
          contains_error.

ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_model IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_model IMPLEMENTATION .

  METHOD initialization_dat.

    FREE: s_erdat[], s_erzet[].
    s_erdat[]  = VALUE #( sign = 'I' option = 'EQ' ( low = sy-datum ) ).
    s_erzet[]  = VALUE #( sign = 'I' option = 'EQ' ( high = '235959' ) ).

  ENDMETHOD.                    "initialization_dat
  METHOD retrieve_data.

    SELECT
         CASE WHEN @iv_mtype EQ @mc_msg-success THEN @icon_green_light ELSE @icon_red_light END AS light,
         yloomis_t02~bukrs, yloomis_t02~prs_id, yloomis_t02~prs_kind, yloomis_t02~guid,
         yloomis_t02~awkey, yloomis_t02~erdat, yloomis_t02~erzet, yloomis_t02~ernam,
         @icon_message_faulty_orphan AS msgshw,
         substring( yloomis_t02~awkey,1,10 ) AS belnr,
         CAST( substring( yloomis_t02~awkey,15,4 ) AS NUMC( 4 ) ) AS gjahr
           FROM yloomis_t02
               WHERE yloomis_t02~bukrs EQ @iv_bukrs
                 AND yloomis_t02~erdat IN @iv_erdat
                 AND yloomis_t02~prs_id IN @s_prsid
                 AND yloomis_t02~erzet IN @iv_erzet
                 AND yloomis_t02~ernam IN @iv_ernam
                 AND yloomis_t02~mtype EQ @iv_mtype
             INTO TABLE @DATA(t_basedat).

    IF NOT t_basedat IS INITIAL.
      SELECT bukrs, belnr, gjahr, stblg, stjah
        FROM bkpf
        FOR ALL ENTRIES IN @t_basedat
        WHERE bukrs EQ @iv_bukrs
          AND belnr EQ @t_basedat-belnr
          AND gjahr EQ @t_basedat-gjahr
          AND stblg NE @space
          INTO TABLE @mt_revdat.
    ENDIF.

    LOOP AT t_basedat ASSIGNING FIELD-SYMBOL(<basedat>).

      APPEND INITIAL LINE TO mt_outdat ASSIGNING FIELD-SYMBOL(<outdat>).
      <outdat> = CORRESPONDING #( <basedat> ).

      READ TABLE mt_revdat ASSIGNING FIELD-SYMBOL(<revdat>) WITH KEY bukrs = <basedat>-bukrs
                                                                     belnr = <basedat>-belnr
                                                                     gjahr = <basedat>-gjahr.
      IF sy-subrc IS INITIAL.
        <outdat>-awkey_rev = |{ <revdat>-stblg }{ <revdat>-bukrs }{ <revdat>-stjah }|.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "retrieve_data
  METHOD application_logdat.

    DATA: t_msgdat TYPE STANDARD TABLE OF balm.

    FREE: t_msgdat.
    CALL FUNCTION 'APPL_LOG_READ_DB'
      EXPORTING
        object          = iv_object
        subobject       = iv_subobject
        external_number = iv_extnumber
      TABLES
        messages        = t_msgdat.

    LOOP AT t_msgdat REFERENCE INTO DATA(_msgdat).
      rt_msgdat = VALUE #( BASE rt_msgdat ( type = _msgdat->msgty id =  _msgdat->msgid number = _msgdat->msgno message_v1 = _msgdat->msgv1 message_v2 = _msgdat->msgv2 message_v3 = _msgdat->msgv3 message_v4 = _msgdat->msgv4 ) ).
    ENDLOOP.
    IF sy-subrc <> 0.
      MESSAGE e008(yloomis) RAISING contains_error.
    ENDIF.

  ENDMETHOD.                    "application_logdat

ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_view DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_view DEFINITION FINAL.

  PUBLIC SECTION.
    CONSTANTS:
      mc_strname TYPE tabname VALUE 'YLOOMIS_S04'.

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
        WHEN 'AWKEY'.
          r_fcat->hotspot = abap_true.
          r_fcat->no_out = p_rb2.
        WHEN 'MSGSHW'.
          _text = TEXT-f02.
          r_fcat->no_out = p_rb1.
          r_fcat->just = 'C'.
          r_fcat->hotspot = abap_true.
        WHEN 'AWKEY_REV'.
          _text = TEXT-f03.
          r_fcat->hotspot = abap_true.
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
        is_layout_lvc            = VALUE lvc_s_layo( col_opt = abap_true cwidth_opt = abap_true zebra = abap_true )
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