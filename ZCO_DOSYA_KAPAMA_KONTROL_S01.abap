*&---------------------------------------------------------------------*
*&  Include           ZCO_DOSYA_KAPAMA_KONTROL_S01
*&---------------------------------------------------------------------*
TABLES: vbrk, vbrp , ckmlhd, ckmlpp.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-s01.

PARAMETERS:
  p_bukrs TYPE bukrs MEMORY ID buk,
  p_werks TYPE vbrp-werks.

SELECT-OPTIONS:
  s_matnr FOR vbrp-matnr,
  s_fkdat FOR vbrk-fkdat.

SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN OUTPUT.
  PERFORM at_selection_screen_output.

AT SELECTION-SCREEN.
  PERFORM at_selection_screen.

START-OF-SELECTION.
  PERFORM main_sel.

END-OF-SELECTION.

  IF d_ok EQ 'X'.
    PERFORM report TABLES gt_report[] USING 'GT_REPORT'.
  ELSE.
    MESSAGE s579(00).
  ENDIF.