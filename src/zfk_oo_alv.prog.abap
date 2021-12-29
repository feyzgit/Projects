*&---------------------------------------------------------------------*
*& Report ZFK_OO_ALV
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_oo_alv.
TABLES: zfk_islem.

INCLUDE zfk_oo_alv_top.
INCLUDE zfk_oo_alv_frm.
INCLUDE zfk_oo_alv_pbo.
INCLUDE zfk_oo_alv_pai.



START-OF-SELECTION.

  PERFORM f_get_data.

  CALL SCREEN 0100.
