*&---------------------------------------------------------------------*
*& Report ZFK_ODEV6
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_odev6.

INCLUDE zfk_odev6_top_data.
INCLUDE zfk_odev6_top_screen.
INCLUDE zfk_odev6_top_class.
INCLUDE zfk_odev6_pai.
INCLUDE zfk_odev6_pbo.
INCLUDE zfk_odev6_forms.

START-OF-SELECTION.

  DATA: go_report TYPE REF TO lcl_batch.
  CREATE OBJECT go_report.
  go_report->get_data( ).

  CALL SCREEN 0100.
