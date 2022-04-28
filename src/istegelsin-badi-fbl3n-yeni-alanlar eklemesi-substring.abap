  METHOD if_ex_fi_items_ch_data~change_items.

    TYPES: BEGIN OF ty_bwartdat,
             bukrs TYPE bkpf-bukrs,
             belnr TYPE bkpf-belnr,
             gjahr TYPE bkpf-gjahr,
             buzei TYPE bseg-buzei,
             bwart TYPE mseg-bwart,
             btext TYPE t156ht-btext,
           END OF ty_bwartdat,
           tt_bwartdat TYPE HASHED TABLE OF ty_bwartdat WITH UNIQUE KEY bukrs belnr gjahr buzei.
    DATA: t_bwartdat TYPE tt_bwartdat.

    IF sy-tcode EQ 'FBL3N'.

      SELECT DISTINCT bkpf~bukrs, bkpf~belnr, bkpf~gjahr, acdoca~buzei, mseg~bwart, t156ht~btext
        FROM @ct_items AS itab
          INNER JOIN bkpf ON bkpf~bukrs = itab~bukrs AND
                             bkpf~belnr = itab~belnr AND
                             bkpf~gjahr = itab~gjahr
          INNER JOIN mseg ON substring( bkpf~awkey,1,10 ) = mseg~mblnr AND
                             substring( bkpf~awkey,11,4 ) = mseg~mjahr
          INNER JOIN acdoca ON acdoca~rldnr  = '0L'        AND
                               acdoca~rbukrs = itab~bukrs  AND
                               acdoca~belnr  = itab~belnr  AND
                               acdoca~aworg  = itab~gjahr  AND
                               acdoca~buzei  = itab~buzei  AND
                               acdoca~awref  = mseg~mblnr  AND
                               substring( acdoca~awitem,2,4 ) = mseg~zeile
          LEFT JOIN t156ht ON t156ht~spras = @sy-langu AND
                             t156ht~bwart = mseg~bwart
            WHERE bkpf~awtyp = 'MKPF'
            INTO CORRESPONDING FIELDS OF TABLE @t_bwartdat.

      LOOP AT ct_items ASSIGNING FIELD-SYMBOL(<items>).

        READ TABLE t_bwartdat REFERENCE INTO DATA(_bwartdat) WITH TABLE KEY bukrs = <items>-bukrs belnr = <items>-belnr gjahr = <items>-gjahr buzei = <items>-buzei .
        IF sy-subrc IS INITIAL.
          CONCATENATE _bwartdat->bwart '-' _bwartdat->btext INTO DATA(ls_islem) SEPARATED BY space.
          <items>-zz_islem_turu = ls_islem.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.