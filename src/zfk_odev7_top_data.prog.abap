*&---------------------------------------------------------------------*
*& Include          ZFK_ODEV7_TOP_DATA
*&---------------------------------------------------------------------*
DATA: go_alv    TYPE REF TO cl_gui_alv_grid,
      go_cont1  TYPE REF TO cl_gui_container,
      go_cont2  TYPE REF TO cl_gui_container,
      go_cont3  TYPE REF TO cl_gui_container,
      go_cont4  TYPE REF TO cl_gui_container,
      go_cust   TYPE REF TO cl_gui_custom_container,
      go_split  TYPE REF TO cl_gui_splitter_container,
      go_html   TYPE REF TO cl_gui_html_viewer.

DATA: gt_fieldcat TYPE lvc_t_fcat,
      gs_fieldcat TYPE lvc_s_fcat,
      gs_layout   TYPE lvc_s_layo.
