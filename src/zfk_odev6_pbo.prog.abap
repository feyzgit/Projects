*&---------------------------------------------------------------------*
*& Include          ZFK_ODEV6_PBO
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'GUI_STATUS_0100'.

  DATA: lo_alv TYPE REF TO lcl_batch.
  CREATE OBJECT lo_alv.
  lo_alv->display( ).


ENDMODULE.
