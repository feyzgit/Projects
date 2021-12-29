*&---------------------------------------------------------------------*
*& Include          ZFK_ODEV6_TOP_SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

PARAMETERS: p_bukrs TYPE bkpf-bukrs,
*            p_belnr TYPE bkpf-belnr,
            p_gjahr TYPE bkpf-gjahr DEFAULT 2019,
            p_monat TYPE bkpf-monat DEFAULT 9.
*            p_xblnr TYPE bkpf-xblnr,
*            p_bktxt TYPE bkpf-bktxt.

SELECTION-SCREEN END OF BLOCK b1 .
