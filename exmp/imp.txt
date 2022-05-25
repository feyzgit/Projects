*&---------------------------------------------------------------------*
*& Include          ZFI_048_TEDARIKCI_ONAY_IMP
*&---------------------------------------------------------------------*
CLASS lcl_main IMPLEMENTATION.

  METHOD start.

    CREATE OBJECT lr_main.

    lr_main->get_data( ).
    lr_main->display( ).

  ENDMETHOD.
  METHOD display.

    CALL SCREEN 0100.

  ENDMETHOD.
  METHOD free_data.

    IF gt_container IS NOT INITIAL.

      CALL METHOD gt_container->free
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3.

      CLEAR gt_container.
      FREE gt_container.

    ENDIF.

    IF grid IS NOT INITIAL.

      CALL METHOD grid->free
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3.

      CLEAR grid.

    ENDIF.

  ENDMETHOD.
  METHOD alv.

    DATA: ls_layout  TYPE  lvc_s_layo,
          ls_variant TYPE  disvariant.
    DATA: lw_toolbar TYPE stb_button.

    ls_layout-zebra      = 'X'.
    ls_layout-cwidth_opt = 'X'.
    ls_layout-sel_mode   = 'A'.
    ls_layout-stylefname = 'CELLTAB'.
    ls_variant-report = sy-repid.

    IF gt_container IS INITIAL.

      CREATE OBJECT gt_container
        EXPORTING
          container_name              = 'CONT1'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.

      CREATE OBJECT grid
        EXPORTING
          i_parent = gt_container.

      CREATE OBJECT event_receiver .
      SET HANDLER event_receiver->handle_double_click FOR grid.
      SET HANDLER event_receiver->handle_hotspot_click FOR grid.
      SET HANDLER event_receiver->handle_user_command FOR grid.
      SET HANDLER event_receiver->handle_toolbar_set FOR grid.
      SET HANDLER event_receiver->check_change_data FOR grid.

      CALL METHOD grid->set_table_for_first_display
        EXPORTING
          is_variant      = ls_variant
*         i_buffer_active = ' '
          is_layout       = ls_layout
*         i_save          = 'U'
*         i_bypassing_buffer = 'X'
        CHANGING
          it_fieldcatalog = gt_fc[]
          it_outtab       = alvtable[].


    ELSE.

      CALL METHOD grid->refresh_table_display
        EXPORTING
          i_soft_refresh = ''.

    ENDIF.


    IF it_row_no[] IS NOT INITIAL.

      CALL METHOD grid->set_selected_rows
        EXPORTING
          it_row_no = it_row_no.

    ENDIF.

    CALL METHOD grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*    CALL METHOD grid->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*    CALL METHOD cl_gui_control=>set_focus
*      EXPORTING
*        control = grid.

  ENDMETHOD.
  METHOD fcat.

    REFRESH gt_fc.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = strname
      CHANGING
        ct_fieldcat            = gt_fc
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    LOOP AT gt_fc INTO gs_fc.
      CASE gs_fc-fieldname.
        WHEN 'UNAME'.
          gs_fc-edit = 'X'.
        WHEN 'DELETE'.
          gs_fc-tech = abap_true.
      ENDCASE.
      MODIFY gt_fc FROM gs_fc.
    ENDLOOP.


  ENDMETHOD.
  METHOD get_data.

    SELECT lfb1~bukrs, lfa1~lifnr, lfa1~name1, lfa1~name2, zlog~uname
      FROM lfa1 AS lfa1
      INNER JOIN lfb1 AS lfb1 ON lfb1~lifnr = lfa1~lifnr
      LEFT JOIN zfi_48_t_tedarik AS zlog ON zlog~lifnr = lfb1~lifnr
                                        AND zlog~bukrs = lfb1~bukrs
      INTO CORRESPONDING FIELDS OF TABLE @gt_report
      WHERE lfb1~bukrs EQ @p_bukrs
        AND lfb1~loevm EQ @space
        AND lfb1~sperr EQ @space.


  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_double_click.
  ENDMETHOD.
  METHOD handle_user_command.
    DATA: lt_report TYPE STANDARD TABLE OF zfi_s_tedarik_onay,
          lt_dbdat  TYPE STANDARD TABLE OF zfi_48_t_tedarik,
          ls_dbdat  TYPE zfi_48_t_tedarik,
          _sytabix  TYPE sy-tabix.
    CASE e_ucomm.
      WHEN '&IC1' OR '&ETA'.
      WHEN 'SAVEDAT'.
        FREE: i_selected_rows.
        CALL METHOD grid->get_selected_rows
          IMPORTING
            et_index_rows = i_selected_rows.

        IF i_selected_rows IS INITIAL.
          MESSAGE 'Bir satır seçiniz' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

        FREE: lt_dbdat.
        LOOP AT i_selected_rows INTO DATA(ls_row).
          READ TABLE gt_report INTO DATA(ls_report) INDEX ls_row-index.
          IF sy-subrc IS INITIAL AND ls_report-uname IS NOT INITIAL.
            CLEAR: ls_dbdat.
            MOVE-CORRESPONDING ls_report TO ls_dbdat.
            APPEND ls_dbdat TO lt_dbdat.
          ENDIF.
        ENDLOOP.
        IF NOT lt_dbdat[] IS INITIAL.
          MODIFY zfi_48_t_tedarik FROM TABLE lt_dbdat.
          IF sy-subrc IS INITIAL.
            LOOP AT i_selected_rows INTO ls_row.
              READ TABLE gt_report INTO ls_report INDEX ls_row-index.
              IF sy-subrc IS INITIAL AND ls_report-uname IS NOT INITIAL.
                FREE: ls_report-celltab.
                MODIFY gt_report FROM ls_report INDEX ls_row-index.
              ENDIF.
            ENDLOOP.

            MESSAGE 'Başarıyla kaydedildi' TYPE 'S'.
            COMMIT WORK AND WAIT.

          ELSE.
            MESSAGE 'Kaydetme sırasında hatalar oluştu!!!' TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
        ENDIF.
      WHEN 'DELDAT'.
        FREE: i_selected_rows.
        CALL METHOD grid->get_selected_rows
          IMPORTING
            et_index_rows = i_selected_rows.

        IF i_selected_rows IS INITIAL.
          MESSAGE 'Bir alan seçiniz' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
        FREE: lt_dbdat, lt_report.
        LOOP AT i_selected_rows INTO ls_row.
          READ TABLE gt_report INTO ls_report INDEX ls_row-index.
          IF sy-subrc IS INITIAL AND ls_report-uname IS NOT INITIAL.
            CLEAR: ls_dbdat.
            MOVE-CORRESPONDING ls_report TO ls_dbdat.
            APPEND ls_dbdat TO lt_dbdat.

            ls_report-delete = abap_true.
            MODIFY gt_report FROM ls_report INDEX ls_row-index TRANSPORTING delete.
          ENDIF.
        ENDLOOP.
        IF NOT lt_dbdat[] IS INITIAL.
          DELETE zfi_48_t_tedarik FROM TABLE lt_dbdat.
          IF sy-subrc IS INITIAL.
            DELETE gt_report WHERE delete = abap_true.
            MESSAGE 'Başarıyla silindi.' TYPE 'S'.
            COMMIT WORK AND WAIT.
          ELSE.
            MODIFY gt_report FROM VALUE #( delete = abap_false ) TRANSPORTING delete WHERE delete = abap_true.
            MESSAGE 'Kaydetme sırasında hatalar oluştu!!!' TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
        ENDIF.

      WHEN 'ADDDAT'.
        CLEAR ls_report.
        ls_report-bukrs = p_bukrs.
        ls_celltab-fieldname = 'LIFNR'.
        ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
        INSERT ls_celltab INTO TABLE ls_report-celltab.
        INSERT ls_report INTO gt_report INDEX 1.

    ENDCASE.
    DATA: ls_stable TYPE lvc_s_stbl.
    ls_stable = 'XX'.
    CALL METHOD grid->refresh_table_display
      EXPORTING
        is_stable = ls_stable
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.

  ENDMETHOD.
  METHOD handle_hotspot_click.
  ENDMETHOD.
  METHOD check_change_data.
    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<fs_cells>).
      CASE <fs_cells>-fieldname.
        WHEN 'LIFNR'.
          READ TABLE gt_report ASSIGNING FIELD-SYMBOL(<fs_report>) INDEX <fs_cells>-row_id.
          IF sy-subrc IS INITIAL.
            SELECT SINGLE name1, name2
              FROM lfa1
              INTO ( @<fs_report>-name1,
                     @<fs_report>-name2 )
              WHERE lifnr = @<fs_cells>-value.
          ENDIF.
          DATA: ls_stable TYPE lvc_s_stbl.
          ls_stable = 'XX'.
          grid->refresh_table_display(
            EXPORTING
              is_stable      = ls_stable
*              i_soft_refresh =                  " Without Sort, Filter, etc.
            EXCEPTIONS
              finished       = 1                " Display was Ended (by Export)
              OTHERS         = 2
          ).
          IF sy-subrc <> 0.
*           MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
  METHOD handle_toolbar_set.
    DATA: ls_toolbar  TYPE stb_button.

    DEFINE add_button.

      CLEAR ls_toolbar.

      ls_toolbar-function = &1.
      ls_toolbar-icon     = &2.
      ls_toolbar-text     = &3.

      APPEND ls_toolbar TO e_object->mt_toolbar.

    END-OF-DEFINITION.

    add_button: 'ADDDAT' icon_create 'Ekle',
                'DELDAT' icon_delete 'Sil',
                'SAVEDAT' icon_save_as_template 'Kaydet'.

    DELETE e_object->mt_toolbar WHERE function EQ '&CHECK'
                                  OR function EQ '&REFRESH'
                                  OR function EQ '&LOCAL&CUT'
                                  OR function EQ '&LOCAL&COPY'
                                  OR function EQ '&LOCAL&PASTE'
                                  OR function EQ '&LOCAL&UNDO'
                                  OR function EQ '&LOCAL&APPEND'
                                  OR function EQ '&LOCAL&INSERT_ROW'
                                  OR function EQ '&LOCAL&DELETE_ROW'
                                  OR function EQ '&LOCAL&COPY_ROW'.
  ENDMETHOD.
  METHOD handle_toolbar_set_popup.
    IF sy-ucomm EQ ''.

      REFRESH e_object->mt_toolbar.

    ELSE.

*      DELETE e_object->mt_toolbar WHERE function NE '&LOCAL&APPEND'
*                                    AND function NE '&LOCAL&INSERT_ROW'
*                                    AND function NE '&LOCAL&DELETE_ROW'
*                                    AND function NE '&LOCAL&COPY_ROW'.

    ENDIF.
  ENDMETHOD.
ENDCLASS.