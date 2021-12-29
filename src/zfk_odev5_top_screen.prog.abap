*&---------------------------------------------------------------------*
*& Include          ZFK_ODEV5_TOP_SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_bukrs   TYPE t001-bukrs DEFAULT 'MR01',"OBLIGATORY,
            p_yıl TYPE gjahr DEFAULT 2020,"sy-datum+0(4) OBLIGATORY,
            p_donem   TYPE numc3 DEFAULT 12."sy-datum+4(2) OBLIGATORY.

SELECT-OPTIONS: s_hbkid FOR t012-hbkid,
                s_waers FOR t012k-waers.

SELECTION-SCREEN END OF BLOCK b1 .
