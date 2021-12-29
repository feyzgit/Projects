*&---------------------------------------------------------------------*
*& Include          ZFK_OO_ALV_FRM
*& Form F_DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM f_display_alv .
*  CREATE OBJECT go_container
*    exporting
*      container_name = 'CC_ALV'.

  CREATE OBJECT go_alv
    EXPORTING
      i_parent = cl_gui_container=>screen0.


  CALL METHOD go_alv->set_table_for_first_display
    EXPORTING
*     i_structure_name   = 'ZFK_ISLEM'
      i_buffer_active    = ' '
      i_save             = 'U'
      i_bypassing_buffer = 'X'
      is_layout          = gs_layout
    CHANGING
      it_fieldcatalog    = gt_fieldcat[]
      it_outtab          = gt_s_islem.

  CALL METHOD go_alv->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  CREATE OBJECT event_receiver.
  SET HANDLER event_receiver->my_data_changed FOR go_alv.
*    SET HANDLER event_receiver->data_changed_finished FOR grid1.
*    SET HANDLER event_receiver->handle_toolbar_set    FOR grid1.
*    SET HANDLER event_receiver->handle_user_command   FOR grid1.
*    SET HANDLER event_receiver->handle_menu_button    FOR grid1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
FORM f_get_data .
  DATA: lt_ogrenci TYPE TABLE OF zfk_ogrenci,
        ls_ogrenci TYPE  zfk_ogrenci.
  SELECT * FROM zfk_islem INTO CORRESPONDING FIELDS OF TABLE gt_s_islem ORDER BY islemno ASCENDING.
  SELECT * FROM zfk_ogrenci INTO CORRESPONDING FIELDS OF TABLE lt_ogrenci.
  LOOP AT gt_s_islem INTO gs_s_islem.
    READ TABLE lt_ogrenci INTO ls_ogrenci WITH KEY ogrencino = gs_s_islem-ogrencino.
    IF sy-subrc = 0.
      CONCATENATE ls_ogrenci-ogrenciadi ls_ogrenci-ogrencisoyadi INTO gs_s_islem-adisoyadi SEPARATED BY space.
    ENDIF.
    MODIFY gt_s_islem FROM gs_s_islem.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CERATE_FIELDCAT
*&---------------------------------------------------------------------*
FORM create_fieldcat .

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZFK_S_ISLEM_OO'
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT gt_fieldcat INTO gs_fieldcat.
    CASE gs_fieldcat-fieldname.
      WHEN 'OGRENCINO'.
        gs_fieldcat-edit = abap_true.
      WHEN OTHERS.
    ENDCASE.
    MODIFY gt_fieldcat FROM gs_fieldcat.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SAVE
*&---------------------------------------------------------------------*
FORM f_save.
  DATA: ls_stable  TYPE lvc_s_stbl,
        lt_islem   TYPE TABLE OF zfk_islem,
        lt_t_islem TYPE TABLE OF zfk_s_islem_oo,
        lt_s_islem TYPE  zfk_s_islem_oo.
  MODIFY zfk_islem FROM TABLE lt_islem.
  COMMIT WORK.
  ls_stable-row = 'X'.
  ls_stable-col = 'X'.
  CALL METHOD go_alv->refresh_table_display( is_stable = ls_stable ).
ENDFORM.
