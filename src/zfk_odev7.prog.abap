*&---------------------------------------------------------------------*
*& Report ZFK_ODEV7
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_odev7 .

START-OF-SELECTION.

  DATA(gc_personel) = NEW zfk_cl_odev7( ).
  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'GUI_STATUS_0100'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK'.
      LEAVE TO SCREEN 0.

    WHEN '&BKM'.
      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action    = 'U'
          view_name = 'ZFK_T_SIRKET'
        EXCEPTIONS
          OTHERS    = 1.

    WHEN '&BKM2'.
      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action    = 'U'
          view_name = 'ZFK_T_TAKVIM'
        EXCEPTIONS
          OTHERS    = 1.

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
