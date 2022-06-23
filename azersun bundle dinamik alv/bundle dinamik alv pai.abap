*&---------------------------------------------------------------------*
*&  Include           ZCO_I_BUNDLE_DGT_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
module user_command_0100 input.

  clear: ok_code.
  ok_code = sy-ucomm.
  case ok_code .
    when 'BACK' or 'CANC'.
      leave to screen 0.
    when 'EXIT'.
      leave program.
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100_exit input.
  set screen 0.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0200_exit input.
  clear: ok_code.
  ok_code = sy-ucomm.
  case ok_code .
    when 'BACK' or 'CANC'.
      leave to screen 0.
    when 'EXIT'.
      leave program.
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0200 input.
  clear: ok_code.
  ok_code = sy-ucomm.
  case ok_code .
    when 'BACK' or 'CANC'.
      leave to screen 0.
    when 'EXIT'.
      leave program.
  endcase.
endmodule.