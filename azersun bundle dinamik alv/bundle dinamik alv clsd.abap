*&---------------------------------------------------------------------*
*&  Include           ZCO_I_BUNDLE_DGT_CLSD
*&---------------------------------------------------------------------*
CLASS lcl_gui_alv_grid DEFINITION DEFERRED.
*&---------------------------------------------------------------------*
*&       Class lcx_message
*&---------------------------------------------------------------------*
CLASS lcx_message DEFINITION INHERITING FROM cx_dynamic_check.
  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.
    ALIASES msgty FOR if_t100_dyn_msg~msgty.
ENDCLASS.
TYPES: BEGIN OF ty_coldat,
         fname TYPE lvc_fname,
         ftext TYPE lvc_txt,
         hkont TYPE hkont,
       END OF ty_coldat,
       tt_coldat TYPE TABLE OF ty_coldat WITH DEFAULT KEY.
CLASS lcl_main DEFINITION .
  PUBLIC SECTION.

    CLASS-DATA:
      app          TYPE REF TO application,
      mt_fieldcat  TYPE lvc_t_fcat,
      mt_excluding TYPE slis_t_extab,
      mt_msgdat    TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY,
      mt_coldat    TYPE tt_coldat,
      mr_coldat    TYPE REF TO ty_coldat.

    CLASS-METHODS :
      load_of_program,
      initialization,
      at_selection_screen_output,
      mod_scr_0100,
      fill_sscrdata,
      at_selection_screen,
      start_of_selection,
      end_of_selection.

    METHODS :
      save,
      post RAISING lcx_message,
      f_02_bapi RETURNING VALUE(rv_belnr) TYPE belnr_d,
      save_log IMPORTING is_data TYPE zco_s_bundle_dgt_itm,
      fill_header IMPORTING iv_vbeln TYPE vbrp-vbeln iv_posnr TYPE vbrp-posnr,
      fill_accountgl  IMPORTING iv_itemno TYPE posnr_acc
                                iv_hkont  TYPE hkont
                                iv_kostl  TYPE kostl OPTIONAL
                                iv_matnr  TYPE matnr OPTIONAL
                                iv_werks  TYPE werks_d OPTIONAL,
      fill_curramount IMPORTING iv_itemno TYPE posnr_acc iv_dmbtr TYPE dmbtr,
      fill_criteria   IMPORTING iv_itemno TYPE posnr_acc iv_paobjnr TYPE rkeobjnr iv_matnr TYPE matnr,
      fill_ters_kyt,
      create_grid,
      create_grid_dty,
      show_message,
      retrieve_data EXCEPTIONS not_found,
      generate_dynamic_alvdat IMPORTING  VALUE(im_coldat) TYPE tt_coldat
                              EXPORTING  VALUE(ev_refdat) TYPE data
                              EXCEPTIONS not_found,
      modify_field_catalog IMPORTING grid TYPE REF TO lcl_gui_alv_grid,
      exclude RETURNING VALUE(rt_exclude) TYPE ui_functions,
      toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object
                    e_interactive,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id
                    es_row_no,

      last_day_of_months IMPORTING bdate TYPE datum
                         EXPORTING edate TYPE datum.

  PRIVATE SECTION.

    DATA : o_grid     TYPE REF TO lcl_gui_alv_grid,
           o_grid_dty TYPE REF TO lcl_gui_alv_grid,
           t_list     LIKE gt_output,
           s_list     LIKE LINE OF t_list,
           t_list_dty TYPE TABLE OF zco_s_bundle_dgt_itm,
           s_list_dty LIKE LINE OF t_list.

    DATA : gv_message TYPE bapi_msg.


ENDCLASS.

CLASS lcl_gui_alv_grid DEFINITION INHERITING FROM cl_gui_alv_grid.

  PUBLIC SECTION.
    TYPES tt_fcode TYPE STANDARD TABLE OF sy-ucomm WITH EMPTY KEY.
    CONSTANTS : c_save TYPE char01 VALUE 'A'.
    DATA : fieldcat TYPE lvc_t_fcat.

    METHODS:

      set_table_for_first_display REDEFINITION,
      refresh_table_display       REDEFINITION,
      merge_fieldcatalog
        IMPORTING
                  i_buffer_active        TYPE any            OPTIONAL
                  i_structure_name       TYPE dd02l-tabname  OPTIONAL
                  i_client_never_display TYPE slis_char_1    DEFAULT 'X'
                  i_bypassing_buffer     TYPE char01         OPTIONAL
                  i_internal_tabname     TYPE dd02l-tabname  OPTIONAL
                  i_internal_table       TYPE ANY TABLE      OPTIONAL
        RETURNING VALUE(et_fieldcat)     TYPE lvc_t_fcat,

      change_fcat_attribute
        IMPORTING
          ip_field     TYPE csequence
          ip_attribute TYPE csequence
          ip_value     TYPE csequence
        CHANGING
          ip_fcat      TYPE lvc_t_fcat,

      change_column_text
        IMPORTING
          ip_field TYPE csequence
          ip_text  TYPE csequence.

  PRIVATE SECTION.

ENDCLASS.
DATA(go_main) = NEW lcl_main( ).