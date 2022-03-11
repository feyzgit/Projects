*&---------------------------------------------------------------------*
*&  Include           ZCO_DOSYA_KAPAMA_KONTROL_T01
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Tables                                                               *
*----------------------------------------------------------------------*
TABLES: sscrfields .

*----------------------------------------------------------------------*
* Data Definition  *---------------------------------------------------*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Type-pools
*----------------------------------------------------------------------*
TYPE-POOLS: slis.

*----------------------------------------------------------------------*
* Includes
*----------------------------------------------------------------------*
INCLUDE <icon>.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_alv,
    selkz   TYPE selkz,
    icon    TYPE icon_d,
    info    TYPE lvc_emphsz,
    style   TYPE lvc_t_styl,
  END OF ty_alv,

  BEGIN OF ty_data,
    fkdat TYPE vbrk-fkdat,
    aubel TYPE vbrp-aubel,
    aupos TYPE vbrp-aupos,
    fkimg TYPE vbrp-fkimg,
    kdpos TYPE afpo-kdpos,
    aufnr TYPE vbrp-aufnr,
    wemng TYPE afpo-wemng,
    idat2 TYPE aufk-idat2,
    matnr TYPE vbrp-matnr,
    deger TYPE char1,
  END OF ty_data.

TYPES BEGIN OF ty_list.
        INCLUDE TYPE ty_alv.
        INCLUDE TYPE ty_data.
TYPES END OF ty_list.

*----------------------------------------------------------------------*
* Field-Symbols
*----------------------------------------------------------------------*

FIELD-SYMBOLS : <fs1> TYPE any.

*----------------------------------------------------------------------*
* Variables
*----------------------------------------------------------------------*
DATA: d_ok,
      gv_count  TYPE i,
      d_repname LIKE sy-repid.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS: formname_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE'.

*----------------------------------------------------------------------*
* Structures
*----------------------------------------------------------------------*
DATA: gs_report   TYPE ty_list.

DATA: gs_styletab TYPE lvc_s_styl,
      gs_layout   TYPE lvc_s_layo,
      gs_fieldcat TYPE lvc_s_fcat,
      gs_variant  TYPE ltvariant.

*----------------------------------------------------------------------*
* Internal Tables
*----------------------------------------------------------------------*

DATA : gt_report    TYPE TABLE OF ty_list WITH HEADER LINE .

DATA : gt_messtab   TYPE TABLE OF bapiret2.

DATA : gt_styletab  TYPE lvc_t_styl,
       gt_fieldcat  TYPE lvc_t_fcat,
       gt_heading   TYPE slis_t_listheader,
       gt_events    TYPE TABLE OF slis_alv_event,
       gt_excluding TYPE slis_t_extab.

*----------------------------------------------------------------------*
* Ranges
*----------------------------------------------------------------------*