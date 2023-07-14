  METHOD set_selected_rows.

    MODIFY mo_model->mt_outdat FROM VALUE #( selkz = abap_false ) TRANSPORTING selkz WHERE selkz EQ abap_true.
    IF iv_xjob EQ abap_true.
      MODIFY mo_model->mt_outdat FROM VALUE #( selkz = abap_true ) TRANSPORTING selkz WHERE light EQ icon_yellow_light.
    ELSE.
      mo_view->mo_grid->get_selected_rows(
        IMPORTING
          et_row_no = DATA(t_seldat) ).
      LOOP AT t_seldat ASSIGNING FIELD-SYMBOL(<seldat>).
        READ TABLE mo_model->mt_outdat ASSIGNING FIELD-SYMBOL(<outdat>) INDEX <seldat>-row_id.
        IF sy-subrc IS INITIAL.
          <outdat>-selkz = abap_true.
        ENDIF.
      ENDLOOP.
      IF NOT sy-subrc IS INITIAL.
        MESSAGE s001(00) WITH 'Lütfen satır seçimi yapınız!' RAISING cx_error.
      ENDIF.
    ENDIF.

  ENDMETHOD.              "set_selected_rows

  set_selected_rows
        IMPORTING
          iv_xjob TYPE abap_bool
        EXCEPTIONS
          cx_error,

    _controller->set_selected_rows(
      EXPORTING
        iv_xjob  = p_xjob
      EXCEPTIONS
        cx_error = 1
        OTHERS   = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING cx_error.
    ENDIF.          