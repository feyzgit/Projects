*&---------------------------------------------------------------------*
*& Program YLOOMIS_P01
*&---------------------------------------------------------------------*
*& @author Abdullah ATAN <abdullah.atan@dataliva.com.tr>
*&---------------------------------------------------------------------*
PROGRAM yloomis_p01.

INCLUDE yloomis_p01_clsdat.
INCLUDE yloomis_p01_frmdat.

LOAD-OF-PROGRAM.
  _controller = NEW lcl_controller( )->instantiate_app(
    EXPORTING
      iv_model = lcl_controller=>mc_model
      iv_view  = lcl_controller=>mc_view ).

INITIALIZATION.
  _controller->mo_model->initialization_dat( ).

START-OF-SELECTION.
  _controller->mo_model->retrieve_data(
    EXPORTING
      iv_bukrs = p_bukrs
      iv_prsid = s_prsid[]
      iv_pkind = s_pkind[]
      iv_erdat = s_erdat[]
      iv_erzet = s_erzet[]
      iv_ernam = s_ernam[]
      iv_mtype = COND #( WHEN p_rb1 EQ abap_true THEN _controller->mo_model->mc_msg-success
                                                 ELSE _controller->mo_model->mc_msg-error ) ).

END-OF-SELECTION.
  _controller->alv_session(
    EXCEPTIONS
      contains_error = 1
      OTHERS         = 2 ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.