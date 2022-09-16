*&---------------------------------------------------------------------*
*& Report ZCO_010_P01
*&---------------------------------------------------------------------*
*& @author Feyzanur KARAKAŞ <feyzanur.karakas@finpro.com.tr>
*&---------------------------------------------------------------------*
REPORT zco_010_p01.

INCLUDE ZCO_010_I01_top.
INCLUDE ZCO_010_I01_cls.
INCLUDE ZCO_010_I01_screen.

INITIALIZATION.
  lcl_report=>init( ).

START-OF-SELECTION.
  go_report =  NEW lcl_report( ).
  go_report->start( ).