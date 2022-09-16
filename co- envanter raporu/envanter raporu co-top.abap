*&---------------------------------------------------------------------*
*& Include          ZCO_010_I01_TOP
*&---------------------------------------------------------------------*
CLASS lcl_report DEFINITION DEFERRED.

TABLES: ckmlhd, t001k, mara.

DATA: gv_ucomm  TYPE sy-ucomm,
      go_report TYPE REF TO lcl_report.


SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
  PARAMETERS: p_sper   TYPE mldoc-jahrper OBLIGATORY,
              p_eper   TYPE mldoc-jahrper OBLIGATORY,
              p_kalgun TYPE int4 DEFAULT 45 OBLIGATORY.
*            p_bukrs TYPE t001k-bukrs OBLIGATORY DEFAULT '1000'.

  SELECT-OPTIONS: s_bwkey	FOR ckmlhd-bwkey DEFAULT '4032',
                  s_bukrs FOR t001k-bukrs DEFAULT '4032'.
      PARAMETERS: p_curtp TYPE mldoc-curtp DEFAULT 30.
  SELECT-OPTIONS: s_kalnr FOR (mldoc-kalnr),
                  s_matnr FOR ckmlhd-matnr,
                  s_mtart FOR mara-mtart,

                  s_bwtar	FOR ckmlhd-bwtar,
                  s_vbeln	FOR ckmlhd-vbeln,
                  s_posnr	FOR ckmlhd-posnr.
*                s_konts FOR t030-konts.
  SELECTION-SCREEN: SKIP 1.
  PARAMETERS: p_acc   TYPE mldoc-acc_principle NO-DISPLAY.
SELECTION-SCREEN: END OF BLOCK b1.