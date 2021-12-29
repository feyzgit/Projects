*&---------------------------------------------------------------------*
*& Include          ZFK_OO_ODEV1_TOP_SCREEN
*&---------------------------------------------------------------------*


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

PARAMETERS: p_bukrs TYPE bkpf-bukrs,
            p_gjahr TYPE bkpf-gjahr.

SELECT-OPTIONS: s_monat FOR bkpf-monat,
                s_lifnr FOR bseg-lifnr.

SELECTION-SCREEN END OF BLOCK b1 .


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN PUSHBUTTON 5(22) btn1 USER-COMMAND btn1.
SELECTION-SCREEN PUSHBUTTON 30(22) btn2 USER-COMMAND btn2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b2.
