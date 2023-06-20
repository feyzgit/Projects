      handle_on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING
          e_fieldname
          es_row_no
          er_event_data,


METHOD handle_on_f4.

    DATA: l_shlp    TYPE shlp_descr_t,
          l_wa      TYPE ddshiface,
          l_ret_val TYPE TABLE OF ddshretval.

    CASE e_fieldname.
      WHEN 'AKIND'.
        READ TABLE _controller->mo_model->mt_outdat ASSIGNING FIELD-SYMBOL(<outdat>) INDEX es_row_no-row_id.
        IF sy-subrc IS INITIAL.

          CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
            EXPORTING
              shlpname = 'ZFI_020_SH11'
            IMPORTING
              shlp     = l_shlp.

          LOOP AT l_shlp-interface INTO l_wa.
            IF l_wa-shlpfield EQ 'AKIND'.
              l_wa-valtabname = 'ZFI_020_S06'.
              l_wa-valfield = 'AKIND'.
            ENDIF.
            MODIFY l_shlp-interface FROM l_wa.
          ENDLOOP.

          CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
            EXPORTING
              shlp          = l_shlp
            TABLES
              return_values = l_ret_val.

          <outdat>-akind = VALUE #( l_ret_val[ 1 ]-fieldval OPTIONAL ).
          IF <outdat>-akind IS NOT INITIAL.
            SELECT akind_desc, umskz
              FROM ZFI_020_T17
              INTO TABLE @DATA(t_akind)
              WHERE akind EQ @<outdat>-akind.
            IF sy-subrc IS INITIAL.
              <outdat>-akind_desc = t_akind[ 1 ]-akind_desc.
              <outdat>-umskz = t_akind[ 1 ]-umskz.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    CALL METHOD _controller->mo_view->mo_grid->refresh_table_display
      EXPORTING
        is_stable      = VALUE #( col = abap_true row = abap_true )
        i_soft_refresh = 'X'
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2.
    er_event_data->m_event_handled = 'X'.


  ENDMETHOD.                    "handle_on_f4


    SET HANDLER mo_controller->handle_on_f4         FOR im_grid.


after set_table_for_first_display

CALL METHOD mo_grid->register_f4_for_fields
        EXPORTING
          it_f4 = VALUE #( ( fieldname = 'AKIND' register = abap_true chngeafter = abap_true getbefore = abap_true ) ).
