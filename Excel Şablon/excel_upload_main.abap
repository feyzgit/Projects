*&---------------------------------------------------------------------*
*& Program ZCO_001_P01
*&---------------------------------------------------------------------*
*& @author Feyzanur KARAKAŞ <feyzanur.karakas@finpro.com.tr>
*&---------------------------------------------------------------------*
PROGRAM zco_001_p01.

INCLUDE zco_001_p01_clsdat.
INCLUDE zco_001_p01_scrdat.

LOAD-OF-PROGRAM.
  _controller = NEW lcl_mvc_controller( )->instantiate_app(
    EXPORTING
      iv_model = lcl_mvc_controller=>mc_model
      iv_view  = lcl_mvc_controller=>mc_view ).

INITIALIZATION.
  _controller->mo_model->initialization_dat( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  _controller->at_selection_screen_request(
    EXPORTING
      im_fieldname = _controller->mo_model->mc_cons-p_fname ).

AT SELECTION-SCREEN.
  _controller->at_selection_screen( ).

AT SELECTION-SCREEN OUTPUT.
  _controller->at_selection_screen_output( ).

START-OF-SELECTION.
  _controller->mo_model->retrieve_data(
    EXPORTING
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