*&---------------------------------------------------------------------*
*& Include          ZFK_ODEV6_TOP_DATA
*&---------------------------------------------------------------------*
DATA: go_alv  TYPE REF TO cl_gui_alv_grid,
      go_cont TYPE REF TO cl_gui_container,
      go_cust TYPE REF TO cl_gui_custom_container.


DATA: gt_fieldcat TYPE lvc_t_fcat,
      gs_fieldcat TYPE lvc_s_fcat,
      gs_layout   TYPE lvc_s_layo.

DATA: gt_batch TYPE STANDARD TABLE OF zfk_s_odev6,
      gs_batch TYPE zfk_s_odev6.

DATA: i_selected_rows TYPE lvc_t_row,
      w_selected_rows TYPE lvc_s_row,
      stable          TYPE lvc_s_stbl VALUE 'XX'.

  DATA: gt_bdcdata    TYPE TABLE OF bdcdata,
        gs_bdcoptions TYPE ctu_params,
        gs_bdcdata    TYPE bdcdata.

  DEFINE addl_line.
    CLEAR gs_bdcdata.
    gs_bdcdata-program = &1.
    gs_bdcdata-dynpro = &2.
    gs_bdcdata-dynbegin = &3.
    gs_bdcdata-fnam = &4.
    gs_bdcdata-fval = &5.
    APPEND gs_bdcdata TO gt_bdcdata.

  END-OF-DEFINITION.
