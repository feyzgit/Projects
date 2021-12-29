*&---------------------------------------------------------------------*
*& Include          ZFK_ODEV5_TOP_DATA
*&---------------------------------------------------------------------*
TABLES: t012, t012k.

DATA: go_alv    TYPE REF TO cl_gui_alv_grid,
      go_cont1  TYPE REF TO cl_gui_container,
      go_cont2  TYPE REF TO cl_gui_container,
      go_cust   TYPE REF TO cl_gui_custom_container,
      go_split  TYPE REF TO cl_gui_splitter_container,
      go_html   TYPE REF TO cl_gui_html_viewer.

DATA: gt_fieldcat TYPE lvc_t_fcat,
      gs_fieldcat TYPE lvc_s_fcat,
      gs_layout   TYPE lvc_s_layo.

DATA: gt_banka TYPE TABLE OF zfk_s_odev5,
      gs_banka TYPE zfk_s_odev5.
