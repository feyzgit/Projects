*&---------------------------------------------------------------------*
*& Include          ZFI_034_P01_TOPDAT
*&---------------------------------------------------------------------*
TABLES: sscrfields.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN COMMENT /1(40) blok_1.
SELECTION-SCREEN BEGIN OF BLOCK b1.
  PARAMETERS:
    p_repex TYPE zfi_034_e028 AS LISTBOX VISIBLE LENGTH 30 USER-COMMAND rpx OBLIGATORY DEFAULT '01'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN COMMENT /1(40) blok_2.
SELECTION-SCREEN BEGIN OF BLOCK b2.
  PARAMETERS:
    p_uname TYPE zfi_034_e030 MODIF ID psw.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(33) password MODIF ID psw FOR FIELD p_passw.
    PARAMETERS:
      p_passw TYPE zfi_034_e031 MODIF ID psw.

    SELECTION-SCREEN COMMENT 56(1) space01 MODIF ID psw.
    PARAMETERS:
      p_hsval AS CHECKBOX DEFAULT abap_true USER-COMMAND cbx MODIF ID psw.
    SELECTION-SCREEN COMMENT 61(12) hidden FOR FIELD p_hsval MODIF ID psw.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN SKIP 1.


  PARAMETERS:
    p_bdate TYPE zfi_034_e001,
    p_edate TYPE zfi_034_e002.
SELECTION-SCREEN END OF BLOCK b2.

CLASS lcl_abstract_reporting DEFINITION DEFERRED.

TYPES:
  BEGIN OF mty_switch,
    repex TYPE zfi_034_e028,
    repet TYPE text80,
    cname TYPE seoclsname,
    sname TYPE typename,
  END OF mty_switch.

DATA:
  mc_screen  TYPE sy-dynnr VALUE '9000',
  mr_message TYPE REF TO zcx_tjk,
  mt_switch  TYPE HASHED TABLE OF mty_switch WITH UNIQUE KEY primary_key COMPONENTS repex.