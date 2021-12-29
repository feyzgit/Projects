*&---------------------------------------------------------------------*
*& Include          ZFK_OO_ODEV1_PAI
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK'.
      LEAVE TO SCREEN 0.
    WHEN '&DETAY'.
      CALL METHOD go_alv_bkpf->get_selected_rows
        IMPORTING
          et_index_rows = i_selected_rows.

      IF i_selected_rows IS INITIAL.
        MESSAGE 'Bir alan seçiniz' TYPE 'E'.
      ENDIF.
      DESCRIBE TABLE i_selected_rows LINES satirkontrol.
      IF satirkontrol > 1.
        MESSAGE 'Sadece 1 alan seçiniz.' TYPE 'E'.
      ENDIF.

      CLEAR:gt_bseg_alv[].

      LOOP AT i_selected_rows INTO w_selected_rows.
        READ TABLE gt_bkpf INTO gs_bkpf INDEX w_selected_rows-index.
        IF sy-subrc EQ 0.
          LOOP AT gt_bseg INTO gs_bseg WHERE bukrs = gs_bkpf-bukrs
                                          AND belnr = gs_bkpf-belnr
                                          AND gjahr = gs_bkpf-gjahr.
            APPEND gs_bseg TO gt_bseg_alv.
          ENDLOOP.

        ENDIF.
      ENDLOOP.
      CALL SCREEN 200 STARTING AT 10 60
      ENDING AT 70 120.

    WHEN '&PRINT'.
      CALL METHOD go_alv_bkpf->get_selected_rows
        IMPORTING
          et_index_rows = i_selected_rows.

      IF i_selected_rows IS INITIAL.
        MESSAGE 'Bir alan seçiniz' TYPE 'E'.
      ENDIF.
      DESCRIBE TABLE i_selected_rows LINES satirkontrol.
      IF satirkontrol > 1.
        MESSAGE 'Sadece 1 alan seçiniz.' TYPE 'E'.
      ENDIF.

      CLEAR:gt_bseg_alv[].

      LOOP AT i_selected_rows INTO w_selected_rows.
        READ TABLE gt_bkpf INTO gs_bkpf INDEX w_selected_rows-index.
        IF sy-subrc EQ 0.
          LOOP AT gt_bseg INTO gs_bseg WHERE bukrs = gs_bkpf-bukrs
                                          AND belnr = gs_bkpf-belnr
                                          AND gjahr = gs_bkpf-gjahr.
            APPEND gs_bseg TO gt_bseg_alv.
          ENDLOOP.

        ENDIF.
      ENDLOOP.

      TYPES : BEGIN OF ty_form.
                INCLUDE TYPE zfk_s_form.
              TYPES END OF ty_form.

      TYPES : BEGIN OF ty_table.
                INCLUDE TYPE zfk_s_table.
              TYPES END OF ty_table.

      DATA: gt_form  TYPE TABLE OF ty_form,
            gs_form  LIKE LINE OF gt_form,
            gt_table TYPE TABLE OF ty_table,
            gs_table TYPE ty_table.

      DATA: lv_fname TYPE rs38l_fnam.

      gs_form-belgenum = gs_bkpf-belnr.
      gs_form-belgetarih = gs_bkpf-bldat.
      gs_form-belgeyil = gs_bkpf-gjahr.
      gs_form-indtoplam = gs_bkpf-wrbtr.
      gs_form-refalan = gs_bkpf-xblnr.
      gs_form-satici = gs_bkpf-lifnr.
      gs_form-toplam = gs_bkpf-wrbtr.

      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname = 'ZFK_SF_ODEV4'
        IMPORTING
          fm_name  = lv_fname.


      CALL FUNCTION lv_fname
        EXPORTING
          iskonto  = gs_bkpf-iskonto
          gt_form  = gs_form
        TABLES
          gt_table = gt_bseg_alv.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK'.
      LEAVE TO SCREEN 0.

  ENDCASE.
ENDMODULE.
