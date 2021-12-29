*&---------------------------------------------------------------------*
*& Include          ZFK_ODEV6_TOP_CLASS
*&---------------------------------------------------------------------*
CLASS lcl_batch DEFINITION.
  PUBLIC SECTION.
    METHODS:
      get_data,
      merge,
      display.


ENDCLASS.

CLASS lcl_batch IMPLEMENTATION.
  METHOD get_data.
    SELECT bukrs
           belnr
           gjahr
           monat
           xblnr
           bktxt
      FROM bkpf
      INTO TABLE gt_batch
      WHERE bukrs = p_bukrs
        AND gjahr = p_gjahr
        AND monat = p_monat.


  ENDMETHOD.

  METHOD merge.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZFK_S_ODEV6'
      CHANGING
        ct_fieldcat      = gt_fieldcat.



    LOOP AT gt_fieldcat INTO gs_fieldcat.
      CASE gs_fieldcat-fieldname.
        WHEN 'BKTXT' .
          gs_fieldcat-edit = abap_true.
        WHEN OTHERS.
      ENDCASE.
      MODIFY gt_fieldcat FROM gs_fieldcat.

    ENDLOOP.


  ENDMETHOD.


  METHOD display.
    IF go_alv IS INITIAL.


      me->merge( ).

      CREATE OBJECT go_cust
      EXPORTING
        container_name = 'CUST'.

      CREATE OBJECT go_alv
        EXPORTING
          i_parent = go_cust.

      gs_layout-cwidth_opt = abap_true.

      CALL METHOD go_alv->set_table_for_first_display
        EXPORTING
          is_layout       = gs_layout
        CHANGING
          it_fieldcatalog = gt_fieldcat
          it_outtab       = gt_batch.

      CALL METHOD go_alv->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.


      CALL METHOD go_alv->refresh_table_display
        EXPORTING
          is_stable = stable.

    ENDIF.



  ENDMETHOD.



ENDCLASS.
