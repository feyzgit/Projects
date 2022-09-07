*&---------------------------------------------------------------------*
*& Include          ZFI_023_P01_FRMDAT
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command USING v_ucomm  LIKE sy-ucomm
                        v_selfld TYPE slis_selfield.

  DATA:answer TYPE char1.

  CASE v_ucomm.
    WHEN '&BACK' OR '&CANC'.
      LEAVE TO SCREEN 0.
    WHEN '&EXIT'.
      LEAVE PROGRAM.
    WHEN '&IC1'.

    WHEN '&SAVE'.
      _controller->display_popup_message(
        EXPORTING
*          im_batch         = abap_false
          im_titlebar      = TEXT-m01
          im_text_question = TEXT-m02
        IMPORTING
          ev_answer        = answer ).
      IF answer EQ 1.
        _controller->save(
          RECEIVING
            rt_retdat      = DATA(retdat)
          EXCEPTIONS
            contains_error = 1
            OTHERS         = 2 ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          _controller->display_message(
            EXPORTING
              it_msgdat      = retdat
            EXCEPTIONS
              contains_error = 1
              OTHERS         = 2 ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
      ELSE.
        EXIT.
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