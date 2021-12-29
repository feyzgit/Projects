*&---------------------------------------------------------------------*
*& Include          ZFK_ODEV6_PAI
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK'.
      LEAVE TO SCREEN 0.

    WHEN '&BATCH'.

      CALL METHOD go_alv->get_selected_rows
        IMPORTING
          et_index_rows = i_selected_rows.

      IF i_selected_rows IS INITIAL.
        MESSAGE 'Bir alan seçiniz.' TYPE 'E'.
      ENDIF.

      PERFORM change_batch.

    WHEN '&BATCH2'.

      CALL METHOD go_alv->get_selected_rows
        IMPORTING
          et_index_rows = i_selected_rows.

      IF i_selected_rows IS INITIAL.
        MESSAGE 'Bir alan seçiniz.' TYPE 'E'.
      ENDIF.

      PERFORM change_batch2.


*        CALL TRANSACTION 'SM35' AND SKIP FIRST SCREEN.

  ENDCASE.
ENDMODULE.
