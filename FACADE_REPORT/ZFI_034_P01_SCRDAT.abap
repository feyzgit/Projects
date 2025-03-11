*&---------------------------------------------------------------------*
*& Include          ZFI_025_P01_SCRDAT
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'STATUS_ALV'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_9000  OUTPUT
*&----------------------------------------

FLOW LOGIC
PROCESS BEFORE OUTPUT.
  MODULE pbo_9000.
  MODULE status_9000.

PROCESS AFTER INPUT.
  MODULE user_command_9000.-----------------------------*
MODULE pbo_9000 OUTPUT.

  TRY.
      lcl_abstract_reporting=>create(
        EXPORTING
          im_clsname = VALUE #( mt_switch[ KEY primary_key COMPONENTS repex = p_repex ]-cname OPTIONAL )
          im_view    = NEW lcl_view_alv( iv_strname = VALUE #( mt_switch[ KEY primary_key COMPONENTS repex = p_repex ]-sname OPTIONAL ) ) )->start( ).
    CATCH zcx_tjk INTO mr_message.
      MESSAGE mr_message.
  ENDTRY.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.

FLOW LOGIC
PROCESS BEFORE OUTPUT.
  MODULE pbo_9000.
  MODULE status_9000.

PROCESS AFTER INPUT.
  MODULE user_command_9000.