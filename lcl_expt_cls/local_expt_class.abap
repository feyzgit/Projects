*&---------------------------------------------------------------------*
*& Report ZFI_023_P01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_023_p01.

INCLUDE zfi_023_p01_clsdat.
INCLUDE zfi_023_p01_frmdat.

LOAD-OF-PROGRAM.
  _controller = NEW lcl_controller( )->instantiate_app(
    EXPORTING
      iv_model = lcl_controller=>mc_model
      iv_view  = lcl_controller=>mc_view ).

INITIALIZATION.
  _controller->initialization( ).

AT SELECTION-SCREEN OUTPUT.
  _controller->at_selection_screen_output( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  _controller->at_selection_value( ).

AT SELECTION-SCREEN.
  _controller->at_selection_screen( ).

START-OF-SELECTION.
  _controller->mo_model->retrieve_data(
    EXPORTING
      iv_bukrs = p_bukrs
      iv_fname = p_fname ).

END-OF-SELECTION.
  _controller->alv_session(
    EXCEPTIONS
      contains_error = 1
      OTHERS         = 2 ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.