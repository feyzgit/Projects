*&---------------------------------------------------------------------*
*& Include          ZCO_010_I01_SCREEN
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include          ZCO_R_001_SCREEN
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF100'.
  SET TITLEBAR 'T100' WITH TEXT-tit.
ENDMODULE.

MODULE alv OUTPUT.
  IF go_report->lr_grid IS INITIAL.
    go_report->crate_grid( ).
  ELSE.
    go_report->refresh_grid( iv_grid = go_report->lr_grid iv_stable =  'X' iv_soft = 'X' ).
  ENDIF.
ENDMODULE.

MODULE user_command_0100 INPUT.
  gv_ucomm = sy-ucomm.
  go_report->handle_user_command(
    EXPORTING
      sender  = go_report->lr_grid
      e_ucomm = gv_ucomm
  ).

  CLEAR: gv_ucomm.
ENDMODULE.