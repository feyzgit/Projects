*&---------------------------------------------------------------------*
*& Include          ZFK_OO_ODEV1_PBO
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'GUI_STATUS_1000'.
  go_bkpf->display( ).
  SET HANDLER go_bkpf->handle_hotspot_click FOR go_alv_bkpf.
  SET HANDLER go_bkpf->data_changed FOR go_alv_bkpf.
  CALL METHOD go_alv_bkpf->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'GUI_STATUS_0200'.
  go_bseg->display( ).
ENDMODULE.
