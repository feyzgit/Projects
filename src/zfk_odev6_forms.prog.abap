*----------------------------------------------------------------------*
***INCLUDE ZFK_ODEV6_FORMS.
*----------------------------------------------------------------------*
FORM change_batch .

  LOOP AT i_selected_rows INTO w_selected_rows.

    READ TABLE gt_batch INTO gs_batch INDEX w_selected_rows-index.

    IF sy-subrc EQ 0.

      CLEAR gt_bdcdata.

      addl_line:
    'SAPMF05L' '0100' 'X' '' '' ,
    '' '' '' 'BDC_CURSOR' 'RF05L-BELNR',
    '' '' '' 'BDC_OKCODE' '=WEITE',
    '' '' '' 'RF05L-BELNR' gs_batch-belnr,
    '' '' '' 'RF05L-BUKRS' gs_batch-bukrs,
    '' '' '' 'RF05L-GJAHR' gs_batch-gjahr,
    'SAPMF05L' '0700' 'X' '' '' ,
    '' '' '' 'BDC_CURSOR' 'BKPF-BELNR',
    '' '' '' 'BDC_OKCODE' '=CHAE',
    'SAPMF05L' '0700' 'X' '' '' ,
    '' '' '' 'BDC_CURSOR' 'BKPF-BELNR',
    '' '' '' 'BDC_OKCODE' '=VK',
    'SAPMF05L' '1710' 'X' '' '' ,
    '' '' '' 'BDC_CURSOR' 'BKPF-XBLNR',
    '' '' '' 'BDC_OKCODE' '=ENTR',
    '' '' '' 'BKPF-BKTXT' gs_batch-bktxt,
    '' '' '' 'BKPF-XBLNR' gs_batch-xblnr,
    'SAPMF05L' '0700' 'X' '' '' ,
    '' '' '' 'BDC_CURSOR' 'BKPF-BELNR',
    '' '' '' 'BDC_OKCODE' '=AE'.

      gs_bdcoptions-dismode = 'E'.

      CALL TRANSACTION 'FB03' USING gt_bdcdata
                       OPTIONS FROM gs_bdcoptions.
    ENDIF.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHANGE_BATCH2
*&---------------------------------------------------------------------*
FORM change_batch2 .

  LOOP AT i_selected_rows INTO w_selected_rows.

    READ TABLE gt_batch INTO gs_batch INDEX w_selected_rows-index.

    IF sy-subrc EQ 0.

      CLEAR gt_bdcdata.

      addl_line:
    '' '' 'T' 'FB02' 'BS AA X',
    'SAPMF05L' '0100' 'X' '' '' ,
    '' '' '' 'BDC_CURSOR' 'RF05L-BELNR',
    '' '' '' 'BDC_OKCODE' '=WEITE',
    '' '' '' 'RF05L-BELNR' gs_batch-belnr,
    '' '' '' 'RF05L-BUKRS' gs_batch-bukrs,
    '' '' '' 'RF05L-GJAHR' gs_batch-gjahr,
    'SAPMF05L' '0700' 'X' '' '' ,
    '' '' '' 'BDC_CURSOR' 'BKPF-BELNR',
    '' '' '' 'BDC_OKCODE' '=VK',
    'SAPMF05L' '1710' 'X' '' '' ,
    '' '' '' 'BDC_CURSOR' 'RF05L-ACC_PRINCTXT',
    '' '' '' 'BDC_OKCODE' '=ENTR',
    '' '' '' 'BKPF-BKTXT' gs_batch-bktxt,
    '' '' '' 'BKPF-XBLNR' gs_batch-xblnr,
    'SAPMF05L' '0700' 'X' '' '' ,
    '' '' '' 'BDC_CURSOR' 'BKPF-BELNR',
    '' '' '' 'BDC_OKCODE' '=AE'.

      gs_bdcoptions-dismode = 'E'.

      CALL TRANSACTION 'FB02' USING gt_bdcdata
                       OPTIONS FROM gs_bdcoptions.
    ENDIF.

  ENDLOOP.
ENDFORM.
