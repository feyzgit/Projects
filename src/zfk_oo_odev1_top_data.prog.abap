*&---------------------------------------------------------------------*
*& Include          ZFK_OO_ODEV1_TOP_DATA
*&---------------------------------------------------------------------*

TABLES: bkpf,bseg.

DATA: go_alv_bkpf TYPE REF TO cl_gui_alv_grid.
DATA: go_alv_bseg TYPE REF TO cl_gui_alv_grid,
      go_alv_cust TYPE REF TO cl_gui_custom_container,
      go_alv_cust2 TYPE REF TO cl_gui_custom_container.

DATA: gt_fieldcat TYPE lvc_t_fcat,
      gs_fieldcat TYPE lvc_s_fcat,
      gs_layout   TYPE lvc_s_layo.

DATA: gt_bkpf TYPE TABLE OF zfk_s_bkpf1,
      gs_bkpf TYPE zfk_s_bkpf1.

DATA: gt_bseg_alv TYPE TABLE OF zfk_s_bseg,
      gs_bseg_alv TYPE zfk_s_bseg.

DATA: gt_bseg TYPE TABLE OF zfk_s_bseg,
      gs_bseg TYPE zfk_s_bseg.

DATA: satirkontrol TYPE i.

DATA: i_selected_rows TYPE lvc_t_row,
      w_selected_rows TYPE lvc_s_row.
