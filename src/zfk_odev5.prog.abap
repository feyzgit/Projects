*&---------------------------------------------------------------------*
*& Report ZFK_ODEV5
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_odev5.


INCLUDE zfk_odev5_top_data.
INCLUDE zfk_odev5_top_screen.
INCLUDE zfk_odev5_top_class.
INCLUDE zfk_odev5_pai.
INCLUDE zfk_odev5_pbo.

START-OF-SELECTION.

  DATA: go_report TYPE REF TO lcl_t001.
  CREATE OBJECT go_report.
  go_report->get_data( ).



  CALL SCREEN 0100.
