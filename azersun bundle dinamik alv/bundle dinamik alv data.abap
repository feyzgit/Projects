*&---------------------------------------------------------------------*
*&  Include           ZCO_I_BUNDLE_DGT_DATA
*&---------------------------------------------------------------------*

TYPE-POOLS: icon.

TABLES: vbrk,vbrp.

TYPES : BEGIN OF ty_output,
          sel(1) TYPE c.
    INCLUDE  TYPE zco_s_bundle_dgt.
TYPES: END OF ty_output.
DATA : gt_output TYPE STANDARD TABLE OF ty_output.

TYPES: BEGIN OF ty_param,
         param_id TYPE memoryid,
         value    TYPE char100,
       END  OF ty_param.
FIELD-SYMBOLS: <outdat> TYPE STANDARD TABLE.
FIELD-SYMBOLS: <lo_outdat> TYPE STANDARD TABLE.

DATA: gt_param TYPE STANDARD TABLE OF ty_param,
      gs_param LIKE LINE OF gt_param.

DATA: lv_itemno         LIKE bapiacgl09-itemno_acc,
      gs_documentheader LIKE bapiache09,
      gt_accountgl      LIKE TABLE OF bapiacgl09 WITH HEADER LINE,
      gs_accountgl      LIKE bapiacgl09,
      gt_currencyamount LIKE TABLE OF bapiaccr09 WITH HEADER LINE,
      gs_currencyamount LIKE bapiaccr09,
      gt_criteria       LIKE TABLE OF bapiackec9,
      gs_criteria       LIKE bapiackec9,
      gt_messtab        TYPE TABLE OF symsg,
      gs_messtab        TYPE symsg.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
PARAMETERS:p_bukrs LIKE vbrk-bukrs OBLIGATORY DEFAULT 'AZ00'.


PARAMETER :  p_poper LIKE ckmlcr-poper  OBLIGATORY  DEFAULT sy-datum+4(2),
             p_bdatj LIKE ckmlcr-bdatj  OBLIGATORY DEFAULT sy-datum(4).

SELECT-OPTIONS:s_kunrg FOR vbrk-kunrg,
               s_matnr FOR vbrp-matnr,
               s_vbeln FOR vbrp-vbeln.

SELECTION-SCREEN END OF BLOCK b01.