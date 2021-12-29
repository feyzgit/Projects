*&---------------------------------------------------------------------*
*& Include          ZFK_OO_ODEV1_TOP_CLASS
*&---------------------------------------------------------------------*

CLASS lcl_bkpf DEFINITION.
  PUBLIC SECTION.

    METHODS : get_data,
      merge,
      display,
      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no,data_changed
                  FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed
                  e_onf4
                  e_ucomm.
ENDCLASS.

CLASS lcl_bkpf IMPLEMENTATION.
  METHOD get_data.
    SELECT b~bukrs,
           b~belnr,
           b~gjahr,
           b~monat,
           bs~lifnr,
           bs~wrbtr,
           i~iskonto,
           b~bldat,
           b~xblnr
      INTO CORRESPONDING FIELDS OF TABLE @gt_bkpf FROM bkpf AS b
      INNER JOIN bseg AS bs ON b~bukrs = bs~bukrs AND
                               b~belnr = bs~belnr AND
                               b~gjahr = bs~gjahr
      INNER JOIN zfk_t_iskonto AS i ON bs~lifnr = i~satıcı
      WHERE b~bukrs = @p_bukrs AND
      b~gjahr = @p_gjahr AND
      b~monat IN @s_monat AND
      bs~lifnr IN @s_lifnr .

    LOOP AT gt_bkpf INTO gs_bkpf.
      gs_bkpf-tutar = gs_bkpf-wrbtr - ( gs_bkpf-wrbtr * gs_bkpf-iskonto ) / 100.
      MODIFY gt_bkpf FROM gs_bkpf.
    ENDLOOP.

  ENDMETHOD.

  METHOD merge.
    CLEAR : gs_fieldcat.
    gs_fieldcat-fieldname = 'SELKZ'.
    gs_fieldcat-no_out = abap_true.
    APPEND gs_fieldcat TO gt_fieldcat.

    CLEAR : gs_fieldcat.
    gs_fieldcat-fieldname = 'BLDAT'.
    gs_fieldcat-no_out = abap_true.
    APPEND gs_fieldcat TO gt_fieldcat.

    CLEAR : gs_fieldcat.
    gs_fieldcat-fieldname = 'XBLNR'.
    gs_fieldcat-no_out = abap_true.
    APPEND gs_fieldcat TO gt_fieldcat.

    CLEAR : gs_fieldcat.
    gs_fieldcat-fieldname = 'BELNR'.
    gs_fieldcat-hotspot = abap_true.
    APPEND gs_fieldcat TO gt_fieldcat.

    CLEAR: gs_fieldcat.
    gs_fieldcat-fieldname = 'ISKONTO'.
    gs_fieldcat-edit = abap_true.
    APPEND gs_fieldcat TO gt_fieldcat.


    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZFK_S_BKPF1'
      CHANGING
        ct_fieldcat      = gt_fieldcat.
  ENDMETHOD.

  METHOD display.
    DATA:
    ls_stable TYPE lvc_s_stbl VALUE 'XX'.

    IF go_alv_bkpf IS INITIAL.
      gs_layout-box_fname = 'SElKZ'.
      gs_layout-sel_mode = 'D'.
      gs_layout-cwidth_opt = abap_true.

      me->merge( ).
      CREATE OBJECT go_alv_cust
        EXPORTING
          container_name = 'CC_ALV'.

      CREATE OBJECT go_alv_bkpf
        EXPORTING
          i_parent = go_alv_cust.

      CALL METHOD go_alv_bkpf->set_table_for_first_display
        EXPORTING
          is_layout       = gs_layout
        CHANGING
          it_fieldcatalog = gt_fieldcat[]
          it_outtab       = gt_bkpf.

    ELSE.

      go_alv_bkpf->refresh_table_display(
      EXPORTING
        is_stable     = ls_stable
        i_soft_refresh = abap_true
        EXCEPTIONS
          finished = 1
          OTHERS = 2
    ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_hotspot_click .
    IF e_column_id-fieldname = 'BELNR'.
      READ TABLE gt_bkpf INTO gs_bkpf INDEX e_row_id-index.
      IF sy-subrc EQ 0.
        SET PARAMETER ID 'BLN' FIELD gs_bkpf-belnr.
        SET PARAMETER ID 'BUK' FIELD gs_bkpf-bukrs.
        SET PARAMETER ID 'GJR' FIELD gs_bkpf-gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD data_changed.
    DATA: ls_good    TYPE lvc_s_modi,
          ls_iskonto TYPE zfk_t_iskonto.
    LOOP AT er_data_changed->mt_good_cells INTO ls_good.
      CASE ls_good-fieldname.
        WHEN 'ISKONTO'.

          CLEAR ls_iskonto.

          READ TABLE gt_bkpf INTO gs_bkpf INDEX ls_good-row_id.
          gs_bkpf-iskonto = ls_good-value.
          gs_bkpf-tutar = gs_bkpf-wrbtr - ( gs_bkpf-wrbtr * gs_bkpf-iskonto ) / 100.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'ISKONTO'
              i_value     = gs_bkpf-iskonto.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'TUTAR'
              i_value     = gs_bkpf-tutar.





      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
*&---------------------------------------------------------------------*
*& Class bseg
*&---------------------------------------------------------------------*
CLASS lcl_bseg DEFINITION.
  PUBLIC SECTION.
    METHODS: get_data,
      merge,
      display.
ENDCLASS.
*&---------------------------------------------------------------------*
*& Class (Implementation) bseg
*&---------------------------------------------------------------------*
CLASS lcl_bseg IMPLEMENTATION.
  METHOD get_data.
    SELECT g~bukrs,
           g~belnr,
           g~gjahr,
           b~buzei,
           b~sgtxt,
           b~gsber,
           b~shkzg,
           b~wrbtr
          FROM @gt_bkpf AS g
          INNER JOIN bseg AS b ON b~bukrs = g~bukrs AND
                                  b~belnr = g~belnr AND
                                  b~gjahr = g~gjahr
           INTO CORRESPONDING FIELDS OF TABLE @gt_bseg.
  ENDMETHOD.

  METHOD merge.
    CLEAR gt_fieldcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZFK_S_BSEG'
      CHANGING
        ct_fieldcat      = gt_fieldcat.
  ENDMETHOD.

  METHOD display.

    DATA:
    ls_stable TYPE lvc_s_stbl VALUE 'XX'.

    IF go_alv_bseg IS INITIAL.

      me->merge( ).

      CREATE OBJECT go_alv_cust2
        EXPORTING
          container_name = 'CC_ALV2'.

      CREATE OBJECT go_alv_bseg
        EXPORTING
          i_parent = go_alv_cust2.


      CALL METHOD go_alv_bseg->set_table_for_first_display
        EXPORTING
          is_layout       = gs_layout
        CHANGING
          it_fieldcatalog = gt_fieldcat[]
          it_outtab       = gt_bseg_alv.

    ELSE.
      go_alv_bseg->refresh_table_display(
        EXPORTING
          is_stable      =  ls_stable                " With Stable Rows/Columns
          i_soft_refresh =  'X'               " Without Sort, Filter, etc.
        EXCEPTIONS
          finished       = 1                " Display was Ended (by Export)
          OTHERS         = 2
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDIF.
  ENDMETHOD.

ENDCLASS.

DATA : go_bkpf TYPE REF TO lcl_bkpf.
DATA : go_bseg TYPE REF TO lcl_bseg.
