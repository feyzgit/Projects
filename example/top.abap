*&---------------------------------------------------------------------*
*& Include          ZFI_048_TEDARIKCI_ONAY_TOP
*&---------------------------------------------------------------------*
TABLES : lfa1, lfb1.
TYPE-POOLS: slis.
**class definition defferred
CLASS lcl_event_receiver DEFINITION DEFERRED.

**CONSTANTS


**containers
DATA go_customcont   TYPE REF TO cl_gui_custom_container.
DATA: cont         TYPE scrfname,
      grid         TYPE REF TO cl_gui_alv_grid,
      gt_container TYPE REF TO cl_gui_custom_container.

**Field Catalogs
DATA: gs_fieldcat_cust TYPE lvc_s_fcat,
      gt_fieldcat_cust TYPE lvc_t_fcat,
      gt_fc            TYPE lvc_t_fcat,
      gs_fc            TYPE lvc_s_fcat.

**Class objects
DATA event_receiver     TYPE REF TO   lcl_event_receiver.
DATA: gr_events    TYPE REF TO lcl_event_receiver.

**Tables
DATA: gt_report  TYPE TABLE OF zfi_s_tedarik_onay.
DATA: gt_tedarik TYPE TABLE OF zfi_48_t_tedarik.
DATA: it_row_no    TYPE lvc_t_roid.

DATA: i_selected_rows TYPE lvc_t_row.
DATA: ls_celltab      TYPE lvc_s_styl.
DATA: lt_celltab      TYPE lvc_t_styl WITH HEADER LINE.
**Structures

**Variables
DATA: gv_filename TYPE rlgrap-filename,
      ok_code     LIKE sy-ucomm.


TYPES: BEGIN OF ty_lifnr,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
         name2 TYPE lfa1-name2,
         uname TYPE lfa1-name2,
       END OF ty_lifnr.

DATA:ls_lifnr TYPE ty_lifnr.


SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME.

  PARAMETERS: p_bukrs TYPE bukrs DEFAULT '1048'.
  SELECT-OPTIONS: s_lifnr FOR lfa1-lifnr MODIF ID xyz.

SELECTION-SCREEN END OF BLOCK b01.

*SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME.
*
**PARAMETERS:
*
*SELECTION-SCREEN END OF BLOCK b02.