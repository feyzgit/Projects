FUNCTION zfi_209_fm01.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_BUKRS) TYPE  BUKRS
*"     VALUE(IV_ANLN1) TYPE  ANLN1
*"     VALUE(IV_ANLN2) TYPE  ANLN2
*"  EXPORTING
*"     VALUE(EV_TXT50) TYPE  TXA50_ANLT
*"     VALUE(EV_AKTIV) TYPE  AKTIVD
*"     VALUE(EV_ORDTX) TYPE  ORDTX
*"     VALUE(EV_GDLGRP_TXT) TYPE  GDLGRP_TXT
*"     VALUE(ES_RETURN) TYPE  BAPIRET2
*"     VALUE(EV_SERNR) TYPE  AM_SERNR
*"     VALUE(EV_IVDAT) TYPE  IVDAT_ANLA
*"     VALUE(EV_DEAKT) TYPE  DEAKT
*"     VALUE(EV_KOSTL) TYPE  KOSTL
*"     VALUE(EV_WERKS) TYPE  WERKS_D
*"     VALUE(EV_RAUMN) TYPE  RAUMNR
*"     VALUE(EV_PERNR) TYPE  PERNR_D
*"     VALUE(EV_VORNA) TYPE  PAD_VORNA
*"     VALUE(EV_NACHN) TYPE  PAD_NACHN
*"----------------------------------------------------------------------

  DEFINE _datecnt.
    IF &1 EQ space.
      &1 = '00000000'.
    ENDIF.
  END-OF-DEFINITION.

  DATA : ls_return TYPE bapiret2,
         lv_pernr  TYPE pernr_d.

  SELECT SINGLE anla~txt50, anla~aktiv, t087t~ordtx, t087s~gdlgrp_txt,
                anla~sernr, anla~ıvdat, anla~deakt, anlz~kostl,
                anlz~werks, anlz~raumn, anlz~pernr
    FROM anla
    LEFT JOIN t087t ON t087t~ordnr EQ @zfi_000_cl02=>gc_ordnr_4
                   AND t087t~ord4x EQ anla~ord44
                   AND t087t~spras EQ @zfi_000_cl02=>gc_spras_t
    LEFT JOIN t087s ON t087s~gdlgrp EQ anla~gdlgrp
                   AND t087s~spras EQ @zfi_000_cl02=>gc_spras_t
    LEFT JOIN anlz ON  anlz~bukrs EQ anla~bukrs
                   AND anlz~anln1 EQ anla~anln1
                   AND anlz~anln2 EQ anla~anln2
    WHERE anla~bukrs EQ @iv_bukrs
      AND anla~anln1 EQ @iv_anln1
      AND anla~anln2 EQ @iv_anln2
    INTO (@ev_txt50,@ev_aktiv,@ev_ordtx,@ev_gdlgrp_txt,@ev_sernr,
          @ev_ivdat,@ev_deakt,@ev_kostl,@ev_werks,@ev_raumn,
          @lv_pernr).

  ev_pernr = lv_pernr.

*-&Begin Of -aatan/14.03.2022 {
  _datecnt: ev_aktiv, ev_ivdat, ev_deakt.
*-&End Of -aatan/14.03.2022 }

  SELECT vorna, nachn FROM pa0002
           WHERE pernr = @lv_pernr
        INTO TABLE @DATA(lt_pa0002)
        UP TO 1 ROWS .

  READ TABLE lt_pa0002 INTO DATA(ls_pa0002) INDEX 1.
  IF sy-subrc EQ 0.
    ev_vorna = ls_pa0002-vorna.
    ev_nachn = ls_pa0002-nachn.
  ENDIF.


  IF sy-subrc NE 0.
    IF 1 = 2. MESSAGE e023(zfi_000). ENDIF."for where use
    es_return-type = zfi_000_cl02=>gc_type_e.
    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id         = 'ZFI_000'
        number     = '023'
        textformat = zfi_000_cl02=>gc_ascii_asc
      IMPORTING
        return     = es_return.

  ELSE.

    SELECT COUNT(*) FROM zfi_210_t01
      WHERE comp_code   EQ @iv_bukrs
        AND assetmaino  EQ @iv_anln1
        AND assetsubno  EQ @iv_anln2
        AND invent_date EQ @sy-datum.
    IF sy-subrc IS INITIAL.
      CLEAR : ev_txt50,ev_aktiv,ev_ordtx,ev_gdlgrp_txt. *tekrar borkod numarası girildiğinde ilk girilen barkod numarasını siler.

      DATA(lv_barkod) = |{ iv_bukrs }{ iv_anln1 }{ iv_anln2 } |.
      IF 1 = 2. MESSAGE e000(zfi_209) WITH lv_barkod. ENDIF.
      es_return-id ='ZFI_209'.
      es_return-number = 000.
      es_return-type = 'E'.
      es_return-message_v1 = lv_barkod.
    ENDIF.

  ENDIF.

ENDFUNCTION.