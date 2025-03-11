DATA: lr_blart TYPE RANGE OF bkpf-blart.

  SELECT lifnr, kunnr, ziban
    FROM reguh
    FOR ALL ENTRIES IN @t_bseg
    WHERE ( lifnr EQ @t_bseg-lifnr
         OR kunnr EQ @t_bseg-kunnr )
    AND zbukr EQ @t_bseg-bukrs
    AND ziban NE @space
    AND laufd EQ
      ( SELECT MAX( laufd )
          FROM reguh
          WHERE ( lifnr EQ @t_bseg-lifnr
               OR kunnr EQ @t_bseg-kunnr )
              AND zbukr EQ @t_bseg-bukrs
              AND ziban NE @space )
    INTO TABLE @DATA(t_iban).

  SELECT lifnr, iban
    FROM tiban
    INNER JOIN lfbk
     ON lfbk~bankl EQ tiban~bankl
    AND lfbk~bankn EQ tiban~bankn
    FOR ALL ENTRIES IN @t_bseg
    WHERE lfbk~lifnr EQ @t_bseg-lifnr
    INTO TABLE @DATA(t_lifnr_iban).

  SELECT kunnr, iban
    FROM tiban
    INNER JOIN knbk
     ON knbk~bankl EQ tiban~bankl
    AND knbk~bankn EQ tiban~bankn
    FOR ALL ENTRIES IN @t_bseg
    WHERE knbk~kunnr EQ @t_bseg-kunnr
    INTO TABLE @DATA(t_kunnr_iban).

  SELECT valfrom
    FROM setleaf
    INTO TABLE @DATA(lt_blart)
    WHERE setname EQ 'ZFI_BLART'.

  lr_blart = VALUE #( FOR ls_value IN lt_blart ( sign = 'I'
                                                 option = 'EQ'
                                                 low = ls_value-valfrom ) ).
  "end:FI:9003373968-16700 FBCJ Kasa dftr.snrlms.-SSENGUNAY/OELMACI-28.05.2024-kendi çalışmamızı taşımak için yoruma alındı

  CLEAR : lv_sgtxt.
  LOOP AT t_bkpf INTO ls_bkpf_f110  WHERE ( tcode EQ 'F110'
                                         OR tcode EQ 'YFINEKS002' )
                                        AND blart IN lr_blart.
    LOOP AT t_bseg INTO ls_bseg_f110 WHERE bukrs EQ ls_bkpf_f110-bukrs AND
                                           belnr EQ ls_bkpf_f110-belnr AND
                                           gjahr EQ ls_bkpf_f110-gjahr AND
                                           koart EQ 'K'.
      CLEAR : ls_lfa1 .
      SELECT SINGLE name1 name2
        FROM lfa1
        INTO ls_lfa1
        WHERE lifnr EQ ls_bseg_f110-lifnr .

      LOOP AT t_bsegsub INTO ls_bsegsub_f110 WHERE tabix = sy-tabix.
        CONCATENATE ls_lfa1-name1 '-' 'ÖDEME' INTO ls_bsegsub_f110-sgtxt SEPARATED BY space.
        lv_sgtxt = ls_bsegsub_f110-sgtxt.
        MODIFY t_bsegsub FROM ls_bsegsub_f110.
      ENDLOOP. "t_bsegsub INTO s_bsegsub

    ENDLOOP .

    IF sy-subrc IS INITIAL.

      LOOP AT t_bseg INTO ls_bseg_f110 WHERE bukrs EQ ls_bkpf_f110-bukrs AND
                                             belnr EQ ls_bkpf_f110-belnr AND
                                             gjahr EQ ls_bkpf_f110-gjahr AND
                                             koart EQ 'S'  .

        LOOP AT t_bsegsub INTO ls_bsegsub_f110 WHERE tabix = sy-tabix.
          ls_bsegsub_f110-sgtxt = lv_sgtxt.
          MODIFY t_bsegsub FROM ls_bsegsub_f110.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    LOOP AT t_bseg INTO ls_bseg_f110 WHERE bukrs EQ ls_bkpf_f110-bukrs
                                       AND belnr EQ ls_bkpf_f110-belnr
                                       AND gjahr EQ ls_bkpf_f110-gjahr
                                       AND koart EQ 'K'.

      LOOP AT t_bsegsub INTO ls_bsegsub_f110 WHERE tabix = sy-tabix.
        CLEAR: ls_bsegsub_f110-sgtxt.
        READ TABLE t_iban ASSIGNING FIELD-SYMBOL(<lifnr_iban>) WITH KEY lifnr = ls_bseg_f110-lifnr.
        IF sy-subrc IS INITIAL.
          ls_bsegsub_f110-sgtxt = |{ <lifnr_iban>-ziban }-ÖDEME|.
        ENDIF.
        IF ls_bsegsub_f110-sgtxt IS INITIAL.
          ls_bsegsub_f110-sgtxt = |{ VALUE #( t_lifnr_iban[ lifnr = ls_bseg_f110-lifnr ]-iban OPTIONAL ) }-ÖDEME|.
        ENDIF.
        MODIFY t_bsegsub FROM ls_bsegsub_f110.
      ENDLOOP.
    ENDLOOP.

    LOOP AT t_bseg INTO ls_bseg_f110 WHERE bukrs EQ ls_bkpf_f110-bukrs
                                       AND belnr EQ ls_bkpf_f110-belnr
                                       AND gjahr EQ ls_bkpf_f110-gjahr
                                       AND koart EQ 'D'.

      LOOP AT t_bsegsub INTO ls_bsegsub_f110 WHERE tabix = sy-tabix.
        CLEAR: ls_bsegsub_f110-sgtxt.
        READ TABLE t_iban ASSIGNING FIELD-SYMBOL(<kunnr_iban>) WITH KEY kunnr = ls_bseg_f110-kunnr.
        IF sy-subrc IS INITIAL.
          ls_bsegsub_f110-sgtxt = |{ <kunnr_iban>-ziban }-ÖDEME|.
        ENDIF.
        IF ls_bsegsub_f110-sgtxt IS INITIAL.
          ls_bsegsub_f110-sgtxt = |{ VALUE #( t_kunnr_iban[ kunnr = ls_bseg_f110-kunnr ]-iban OPTIONAL ) }-ÖDEME|.
        ENDIF.
        MODIFY t_bsegsub FROM ls_bsegsub_f110.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP .