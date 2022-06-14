*&---------------------------------------------------------------------*
*& Include          YLOOMIS_P01_FRMDAT
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command USING v_ucomm  LIKE sy-ucomm
                        v_selfld TYPE slis_selfield.

  CASE v_ucomm.
    WHEN '&BACK' OR '&CANC'.
      LEAVE TO SCREEN 0.
    WHEN '&EXIT'.
      LEAVE PROGRAM.
    WHEN '&IC1'.
      READ TABLE _controller->mo_model->mt_outdat REFERENCE INTO DATA(_outdat) INDEX v_selfld-tabindex.
      IF sy-subrc IS INITIAL.
        CASE v_selfld-fieldname.
          WHEN 'MSGSHW'.
            _controller->mo_model->application_logdat(
              EXPORTING
                iv_object      = _controller->mo_model->mc_logger-_object
                iv_subobject   = _controller->mo_model->mc_logger-_subobject
                iv_extnumber   = |{ _outdat->prs_id }_{ _outdat->prs_kind }_{ _outdat->guid }|
              RECEIVING
                rt_msgdat      = DATA(t_msgdat)
              EXCEPTIONS
                contains_error = 1
                OTHERS         = 2 ).
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

            _controller->mo_view->display_applog(
              EXPORTING
                iv_msgdat      = t_msgdat
              EXCEPTIONS
                contains_error = 1
                OTHERS         = 2 ).
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
          WHEN 'AWKEY'.
            IF NOT _outdat->awkey IS INITIAL.
              _controller->mo_view->call_transaction(
                EXPORTING
                  im_tcode     = CONV #('FB03')
                  im_parameter = CONV #( VALUE #( ( param_id = 'BLN' value = _outdat->awkey(10) )
                                                  ( param_id = 'BUK' value = _outdat->awkey+10(4) )
                                                  ( param_id = 'GJR' value = _outdat->awkey+14(4) ) ) ) ).
            ENDIF.
        ENDCASE.
      ENDIF.
  ENDCASE .

  v_selfld-refresh = abap_true.
  v_selfld-row_stable = abap_true.

ENDFORM .                    "user_command
*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
FORM set_pf_status USING p_status.
  SET PF-STATUS 'STANDARD' EXCLUDING _view->mt_exdat.
ENDFORM .                    "set_pf_status
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.

  DATA: d_heading    TYPE slis_t_listheader.

  SELECT SINGLE name_text FROM v_usr_name INTO @DATA(name_text) WHERE bname = @sy-uname.

  d_heading = VALUE #( ( typ = 'H' info = sy-title )
                       ( typ = 'S' info = |Kulanıcı Adı : { name_text }| )
                       ( typ = 'S' info = |Tarih / Saat : { sy-datum DATE = USER } / { sy-uzeit TIME = USER }| )
                       ( typ = 'S' info = |Kayıt Sayısı : { lines( _model->mt_outdat ) }| ) ).

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = d_heading.

ENDFORM .                    "top_of_page