*&---------------------------------------------------------------------*
*&  Include           ZCO_I_BUNDLE_DGT_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  TYPES: BEGIN OF ty_fcode,
           ucomm TYPE sy-ucomm,
         END OF ty_fcode.
  DATA: lt_excl TYPE TABLE OF ty_fcode WITH HEADER LINE.


  SET PF-STATUS 'STATUS_001' EXCLUDING lt_excl.

ENDMODULE.

MODULE pbo_0100 OUTPUT.
  go_main->create_grid( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0200 OUTPUT.
  go_main->create_grid_dty( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STANDARD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE standard OUTPUT.
*
*  SET PF-STATUS 'STANDARD'.
*
*ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.
  TYPES: BEGIN OF ty_fcode_dty,
           ucomm TYPE sy-ucomm,
         END OF ty_fcode_dty.
  DATA: lt_excl_dty TYPE TABLE OF ty_fcode_dty WITH HEADER LINE.


  SET PF-STATUS 'STATUS_002' EXCLUDING lt_excl_dty.
ENDMODULE.