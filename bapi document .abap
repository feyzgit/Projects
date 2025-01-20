post_document
        IMPORTING
          VALUE(im_header)     TYPE bapiache09
          VALUE(im_curramount) TYPE bapiaccr09_tab
          VALUE(im_accountgl)  TYPE bapiacgl09_tab
          VALUE(im_accountrec) TYPE bapiacar09_tab
          VALUE(im_accountpay) TYPE bapiacap09_tab
          VALUE(im_criteria)   TYPE bapiackec9_tab OPTIONAL
        EXPORTING
          ev_obj_key           TYPE bapiache09-obj_key
        RETURNING
          VALUE(rt_return)     TYPE bapiret2_t ,



METHOD post_document.
    CLEAR: rt_return.
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader    = im_header
      IMPORTING
        obj_key           = ev_obj_key
      TABLES
        accountgl         = im_accountgl
        accountreceivable = im_accountrec
        currencyamount    = im_curramount
        accountpayable    = im_accountpay
        criteria          = im_criteria
        return            = rt_return.

    LOOP AT rt_return REFERENCE INTO DATA(lr_return) WHERE type CA 'EAX'.
      EXIT.
    ENDLOOP.
    IF sy-subrc IS NOT INITIAL AND ev_obj_key IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ENDMETHOD.                    "post_document
  

METHOD run_document.

    TYPES:
      BEGIN OF ty_bapidat,
        header         TYPE bapiache09,
        accountgl_tab  TYPE STANDARD TABLE OF bapiacgl09 WITH DEFAULT KEY,
        accountrec_tab TYPE STANDARD TABLE OF bapiacar09 WITH DEFAULT KEY,
        accountpay_tab TYPE STANDARD TABLE OF bapiacap09 WITH DEFAULT KEY,
        curramount_tab TYPE STANDARD TABLE OF bapiaccr09 WITH DEFAULT KEY,
        return_tab     TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY,
        obj_key        TYPE bapiache09-obj_key,
        itemno         TYPE posnr_acc,
      END OF ty_bapidat.

    DATA: _bapidat TYPE ty_bapidat,
          _logdat  TYPE STANDARD TABLE OF zfi_016_t03 WITH DEFAULT KEY,
          _awkey_a TYPE zfi_016_t03-awkey_a,
          _awkey_b TYPE zfi_016_t03-awkey_a,
          t_logdat TYPE STANDARD TABLE OF zfi_016_t03.

    _bapidat-header = VALUE #( obj_type   = |BKPFF|
                               obj_key    = |RFBU|
                               username   = sy-uname
                               header_txt = |Gider Yansıtma Program|
                               comp_code  = p_bukrs
                               doc_date   = p_budat
                               pstng_date = p_budat
                               ref_doc_no = |Gider Yansıtma|
                               fisc_year  = p_budat(4)
                               fis_period = p_budat+4(2)
                               doc_type   = |DR| ).

    LOOP AT _controller->mo_model->mt_outdat REFERENCE INTO DATA(r_outdat) WHERE awkey_a = space.
      IF sy-tabix EQ 1.

        ADD 1 TO _bapidat-itemno.
        _bapidat-accountrec_tab = VALUE #( BASE _bapidat-accountrec_tab (
                                                itemno_acc = _bapidat-itemno
                                                customer = |{ r_outdat->kunnr ALPHA = IN }|
                                                item_text = |Gider Yansıtma Programı| ) ).


        _bapidat-curramount_tab = VALUE #( BASE _bapidat-curramount_tab (
                                                itemno_acc = _bapidat-itemno
                                                curr_type = '00'
                                                currency = 'TRY'
                                                amt_doccur = r_outdat->total_d ) ).
      ENDIF.

      ADD 1 TO _bapidat-itemno.
      _bapidat-accountgl_tab = VALUE #( BASE _bapidat-accountgl_tab (
                                        itemno_acc = _bapidat-itemno
                                        gl_account = r_outdat->hkont_a
                                        tax_code   = r_outdat->mwskz_a
                                        costcenter = r_outdat->kostl_a
                                        item_text = |Gider Yansıtma Programı| ) ).

      _bapidat-curramount_tab = VALUE #( BASE _bapidat-curramount_tab (
                                              itemno_acc = _bapidat-itemno
                                              curr_type = '00'
                                              currency = 'TRY'
                                              amt_doccur = r_outdat->amount * -1 ) ).
    ENDLOOP.
    IF sy-subrc IS INITIAL.
*-&Create Document->
      _bapidat-return_tab = me->post_document(
        EXPORTING
          im_header     = _bapidat-header
          im_curramount = _bapidat-curramount_tab
          im_accountgl  = _bapidat-accountgl_tab
          im_accountpay = _bapidat-accountpay_tab
          im_accountrec = _bapidat-accountrec_tab
        IMPORTING
          ev_obj_key    = _bapidat-obj_key ).

      IF NOT line_exists( _bapidat-return_tab[ type = mc_msg-error ] ).
        _awkey_a = _bapidat-obj_key.
      ENDIF.

      LOOP AT mo_model->mt_outdat ASSIGNING FIELD-SYMBOL(<outdat>).
        FREE: <outdat>-msgdat_a.
        APPEND LINES OF _bapidat-return_tab TO <outdat>-msgdat_a.
      ENDLOOP.
      INSERT LINES OF _bapidat-return_tab INTO TABLE rt_retdat.

      LOOP AT mo_model->mt_outdat ASSIGNING <outdat>.
        <outdat>-statu_a = <outdat>-statu_b = icon_message_faulty_uptodate.
        <outdat>-awkey_a = COND #( WHEN <outdat>-awkey_a IS INITIAL THEN _awkey_a ).
        <outdat>-icon = COND #( WHEN <outdat>-awkey_a IS NOT INITIAL AND <outdat>-awkey_b IS NOT INITIAL THEN icon_green_light ELSE icon_yellow_light ).
      ENDLOOP.

      IF _awkey_a IS NOT INITIAL.
        SELECT SINGLE *
          FROM zfi_016_t03
            INTO @DATA(_t03dat)
            WHERE poper = @p_poper
              AND gjahr = @p_budat(4)
              AND bukrs_a = @p_bukrs
              AND bukrs_b = @p_rbukrs.
        IF sy-subrc IS NOT INITIAL.
          APPEND VALUE #( poper = p_poper
                          gjahr = p_budat(4)
                          bukrs_a = p_bukrs
                          bukrs_b = p_rbukrs
                          budat = p_budat
                          dmbtr_a = VALUE #( mo_model->mt_outdat[ 1 ]-total OPTIONAL )
                          dmbtr_b = VALUE #( mo_model->mt_outdat[ 1 ]-total_d OPTIONAL )
                          awkey_a = _awkey_a
                          awkey_b = _awkey_b
                          erdat = sy-datum
                          erzet = sy-uzeit
                          ernam = sy-uname ) TO t_logdat.
          MODIFY zfi_016_t03 FROM TABLE t_logdat.
          COMMIT WORK.
        ELSE.
          IF _awkey_a IS NOT INITIAL.
            UPDATE zfi_016_t03 SET awkey_a = _awkey_a erdat = sy-datum erzet = sy-uzeit ernam = sy-uname
              WHERE poper = p_poper
                AND gjahr = p_budat(4)
                AND bukrs_a = p_bukrs
                AND bukrs_b = p_rbukrs.
            COMMIT WORK.
          ENDIF.
          IF _awkey_b IS NOT INITIAL.
            UPDATE zfi_016_t03 SET awkey_b = _awkey_b erdat = sy-datum erzet = sy-uzeit ernam = sy-uname
              WHERE poper = p_poper
                AND gjahr = p_budat(4)
                AND bukrs_a = p_bukrs
                AND bukrs_b = p_rbukrs.
            COMMIT WORK.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.


check_document
        IMPORTING
          im_header         TYPE bapiache09
          im_curramount     TYPE bapiaccr09_tab
          im_accountgl      TYPE bapiacgl09_tab
          im_accountpayable TYPE bapiacap09_tab
          im_criteria       TYPE bapiackec9_tab
        RETURNING
          VALUE(rt_return)  TYPE bapiret2_t,


METHOD check_document.
    CLEAR: rt_return.
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
      EXPORTING
        documentheader = im_header
      TABLES
        accountgl      = im_accountgl
        accountpayable = im_accountpayable
        currencyamount = im_curramount
        criteria       = im_criteria
        return         = rt_return.

  ENDMETHOD.                    "check_document


post_document_rev
        IMPORTING
          VALUE(im_reversal) TYPE bapiacrev
          VALUE(im_busact)   TYPE bapiache09-bus_act
        EXPORTING
          ev_objkey          TYPE bapiacrev-obj_key
          et_return          TYPE bapiret2_tab,

METHOD post_document_rev.

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_REV_POST'
      EXPORTING
        reversal = im_reversal
        bus_act  = im_busact
      IMPORTING
        obj_key  = ev_objkey
      TABLES
        return   = et_return.

    LOOP AT et_return REFERENCE INTO DATA(r_return) WHERE type CA 'EAX'.
      EXIT.
    ENDLOOP.
    IF sy-subrc IS NOT INITIAL AND ev_objkey IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ENDMETHOD.                "post_document_rev


  METHOD reversal_document.

    TYPES:
      BEGIN OF ty_bapidat,
        ls_reversal TYPE bapiacrev,
        ls_busact   TYPE bapiache09-bus_act,
        ls_objkey   TYPE bapiacrev-obj_key,
        retdat      TYPE bapiret2_tab,
      END OF ty_bapidat.

    DATA: _bapidat TYPE ty_bapidat,
          _logsys  TYPE tbdls-logsys.


    LOOP AT mo_model->mt_outdat INTO DATA(outdat)
                                GROUP BY ( awkey_a = outdat-awkey_a  awkey_b = outdat-awkey_b )
                                ASSIGNING FIELD-SYMBOL(<router>).
      IF mo_model->t_kontrol IS INITIAL.
        CLEAR: _logsys.
        CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
          IMPORTING
            own_logical_system             = _logsys
          EXCEPTIONS
            own_logical_system_not_defined = 1
            OTHERS                         = 2.

        IF NOT <router>-awkey_a IS INITIAL.
          CLEAR: _bapidat.
          _bapidat-ls_reversal-obj_type = 'BKPFF'.
          _bapidat-ls_reversal-obj_sys = _logsys.
          _bapidat-ls_reversal-obj_key = <router>-awkey_a.
          _bapidat-ls_reversal-obj_key_r = <router>-awkey_a.
          _bapidat-ls_reversal-comp_code = <router>-awkey_a+10(4).
          _bapidat-ls_reversal-reason_rev = '01'.
          _bapidat-ls_busact = 'RFBU'.

          _controller->post_document_rev(
            EXPORTING
              im_reversal = _bapidat-ls_reversal
              im_busact   = _bapidat-ls_busact
            IMPORTING
              ev_objkey   = _bapidat-ls_objkey
              et_return   = _bapidat-retdat ).
          IF NOT line_exists( _bapidat-retdat[ type = 'E' ] ).
            MODIFY mo_model->mt_outdat FROM VALUE #( awkey_a = space ) TRANSPORTING awkey_a WHERE awkey_a = <router>-awkey_a.
            UPDATE zfi_016_t03
              SET awkey_a = @space
                WHERE poper = @p_poper
                  AND gjahr = @p_budat(4)
                  AND bukrs_a = @p_bukrs
                  AND bukrs_b = @p_rbukrs.
            COMMIT WORK.
          ENDIF.
        ENDIF.

        IF NOT <router>-awkey_b IS INITIAL.
          CLEAR: _bapidat.
          _bapidat-ls_reversal-obj_type = 'BKPFF'.
          _bapidat-ls_reversal-obj_sys = _logsys.
          _bapidat-ls_reversal-obj_key = <router>-awkey_b.
          _bapidat-ls_reversal-obj_key_r = <router>-awkey_b.
          _bapidat-ls_reversal-comp_code = <router>-awkey_b+10(4).
          _bapidat-ls_reversal-reason_rev = '01'.
          _bapidat-ls_busact = 'RFBU'.

          _controller->post_document_rev(
            EXPORTING
              im_reversal = _bapidat-ls_reversal
              im_busact   = _bapidat-ls_busact
            IMPORTING
              ev_objkey   = _bapidat-ls_objkey
              et_return   = _bapidat-retdat ).
          IF NOT line_exists( _bapidat-retdat[ type = 'E' ] ).
            MODIFY mo_model->mt_outdat FROM VALUE #( awkey_b = space ) TRANSPORTING awkey_b WHERE awkey_b = <router>-awkey_b.
            UPDATE zfi_016_t03
              SET awkey_b = @space
                WHERE poper = @p_poper
                  AND gjahr = @p_budat(4)
                  AND bukrs_a = @p_bukrs
                  AND bukrs_b = @p_rbukrs.
            COMMIT WORK.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE s001(00) WITH 'Sonraki dönemde kayıt bulunmaktadır!' DISPLAY LIKE 'E'.
      ENDIF.
    ENDLOOP.
    MODIFY mo_model->mt_outdat FROM VALUE #( icon = icon_yellow_light ) TRANSPORTING icon WHERE awkey_a IS INITIAL AND awkey_b IS INITIAL.

  ENDMETHOD.                    "reversal_document  