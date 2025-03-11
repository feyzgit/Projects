*&---------------------------------------------------------------------*
*& Include          ZFKARAKAS_TOPDAT
*&---------------------------------------------------------------------*
TYPE-POOLS: slis, icon.

TABLES: sscrfields, sbook.

SELECTION-SCREEN BEGIN OF BLOCK b1.
  PARAMETERS:
    p_carrid TYPE sbook-carrid.
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_application DEFINITION DEFERRED.

DATA: mo_application TYPE REF TO lcl_application.