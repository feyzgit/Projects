*&---------------------------------------------------------------------*
*& Include          ZFK_OO_ALV_TOP
*&---------------------------------------------------------------------*


DATA: go_alv   TYPE REF TO cl_gui_alv_grid.
*      go_container TYPE REF TO cl_gui_custom_container.

DATA: gt_islem   TYPE TABLE OF zfk_islem,
      gs_islem   TYPE  zfk_islem,
      gt_s_islem TYPE TABLE OF zfk_s_islem_oo,
      gs_s_islem TYPE  zfk_s_islem_oo.

DATA: gt_fieldcat TYPE lvc_t_fcat,
      gs_fieldcat TYPE lvc_s_fcat,
      gs_layout   TYPE lvc_s_layo.

CLASS lcl_event_receiver DEFINITION DEFERRED .
DATA event_receiver TYPE REF TO lcl_event_receiver.
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS: my_data_changed
                FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4
                e_ucomm.

*
*    METHODS: handle_toolbar_set
*                FOR EVENT toolbar OF cl_gui_alv_grid
*      IMPORTING e_object e_interactive.
*
*    METHODS: handle_menu_button
*                FOR EVENT menu_button OF cl_gui_alv_grid
*      IMPORTING e_object e_ucomm.
*
*    METHODS : handle_hotspot_click
*                FOR EVENT hotspot_click OF cl_gui_alv_grid
*      IMPORTING e_row_id
*                e_column_id
*                es_row_no.
*
*    METHODS data_changed_finished        " DATA_CHANGED_FINISHED
*        FOR EVENT data_changed_finished OF cl_gui_alv_grid
*      IMPORTING
*        e_modified
*        et_good_cells.
*
*    METHODS : handle_user_command
*                FOR EVENT user_command OF cl_gui_alv_grid
*      IMPORTING e_ucomm .
*
*  PRIVATE SECTION.
*
ENDCLASS.                    "lcl_event_receiver DEFINITION
**----------------------------------------------------------------------*
**       CLASS lcl_event_receiver IMPLEMENTATION
**----------------------------------------------------------------------*
**
**----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD my_data_changed.
    DATA: ls_good    TYPE lvc_s_modi,
          ls_ogrenci TYPE zfk_ogrenci.
    LOOP AT er_data_changed->mt_good_cells INTO ls_good.
      CASE ls_good-fieldname.
        WHEN 'OGRENCINO'.

          CLEAR ls_ogrenci.
          SELECT SINGLE * FROM zfk_ogrenci INTO ls_ogrenci
            WHERE ogrencino = ls_good-value.

*          READ TABLE gt_s_islem INTO gs_s_islem INDEX ls_good-row_id.
*          IF sy-subrc IS INITIAL.
*            LOOP AT gt_s_islem INTO gs_s_islem WHERE islemno = gs_s_islem-islemno .
*
*
          gs_s_islem-ogrencino = ls_good-value.
          CONCATENATE ls_ogrenci-ogrenciadi ls_ogrenci-ogrencisoyadi INTO gs_s_islem-adisoyadi SEPARATED BY space.
*              MODIFY gt_s_islem FROM gs_s_islem TRANSPORTING adisoyadi ogrencino.
*            ENDLOOP.
*          ENDIF.


          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'ADISOYADI'
              i_value     = gs_s_islem-adisoyadi.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'OGRENCINO'
              i_value     = ls_good-value.
*          CALL METHOD go_alv->refresh_table_display.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.                    "data_changed
*  METHOD handle_toolbar_set.
*
*  ENDMETHOD.                    "handle_toolbar
*  METHOD handle_user_command.
*
*  ENDMETHOD.                    "handle_user_command
*  METHOD handle_hotspot_click.
*
*  ENDMETHOD.                    "handle_hotspot_click
*
*  METHOD data_changed_finished.
*
*  ENDMETHOD.                    "handle_data_changed_finished
*
*  METHOD handle_menu_button.
*
*
*  ENDMETHOD.                    "handle_menu_button

*
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
