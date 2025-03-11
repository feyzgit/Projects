*&---------------------------------------------------------------------*
*& Report ZFKARAKAS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
PROGRAM zfkarakas.

INCLUDE zfkarakas_topdat.
INCLUDE zfkarakas_clsdat.
INCLUDE zfkarakas_scrdat.

LOAD-OF-PROGRAM.
  mo_application = lcl_application=>instance_app( ).

INITIALIZATION.
  mo_application->initialization( ).

START-OF-SELECTION.
  mo_application->start_of_selection( ).