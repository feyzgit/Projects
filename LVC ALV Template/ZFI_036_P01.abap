*&---------------------------------------------------------------------*
*& Report ZFI_036_P01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
PROGRAM ZFI_036_P01.

INCLUDE ZFI_036_P01_clsdat.
INCLUDE ZFI_036_P01_frmdat.

LOAD-OF-PROGRAM.
  DATA(app) = application=>app_instance( ).

INITIALIZATION.
  application=>initialization( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  application=>at_selection_screen_request(
    EXPORTING
      iv_fieldname = 'P_PATH' ).

AT SELECTION-SCREEN.
  application=>at_selection_screen( ).

AT SELECTION-SCREEN OUTPUT.
  application=>at_selection_screen_output( ).

START-OF-SELECTION.
  app->retrieve_data(
    IMPORTING
      iv_path = p_path ).

END-OF-SELECTION.
  app->show( ).