*&---------------------------------------------------------------------*
*&      Form  GET_KYOV
*&---------------------------------------------------------------------*
FORM get_kyov .

  DATA: BEGIN OF lt_faglflexa OCCURS 0,
          ryear  LIKE faglflexa-ryear,
          docnr  LIKE faglflexa-docnr,
          rbukrs LIKE faglflexa-rbukrs,
          poper  LIKE faglflexa-poper,
          racct  LIKE faglflexa-racct,
          tsl    LIKE faglflexa-tsl,
          hsl    LIKE faglflexa-hsl, "
        END OF lt_faglflexa .

  DATA ls_faglflexa  LIKE lt_faglflexa.
  DATA lt_faglflexa2 LIKE lt_faglflexa OCCURS 0 WITH HEADER LINE.
  DATA lt_faglflexa3 LIKE lt_faglflexa OCCURS 0 WITH HEADER LINE.
  DATA lt_kyovh      LIKE zfi_ebyn_t_kyovh OCCURS 0 WITH HEADER LINE.

  DATA:
    fs_taba TYPE dd07v.
  DATA:
    it_taba TYPE STANDARD TABLE OF dd07v,
    it_tabb TYPE STANDARD TABLE OF dd07v.

  FIELD-SYMBOLS: <ft_data>   TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <fs_data>   LIKE lt_faglflexa.

  FIELD-SYMBOLS: <ft_data2>   TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <fs_data2>   LIKE lt_faglflexa.

  DATA lt_log LIKE zfi_ebyn_t_kyovl OCCURS 0 WITH HEADER LINE.

  CHECK p_kyov EQ 'X'.

  SELECT *
    INTO TABLE lt_kyovh
    FROM zfi_ebyn_t_kyovh.
*   WHERE bukrs EQ p_bukrs.

  CALL FUNCTION 'DD_DOMA_GET'
    EXPORTING
      domain_name   = 'ZFI_EBYN_D_BANKA'
      langu         = sy-langu
      withtext      = 'X'
    TABLES
      dd07v_tab_a   = it_taba
      dd07v_tab_n   = it_tabb
    EXCEPTIONS
      illegal_value = 1
      op_failure    = 2
      OTHERS        = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR:gt_dropdown,gt_dropdown[].
  LOOP AT  it_taba INTO fs_taba.
    gs_dropdown-handle     = '1'.
    gs_dropdown-value      = fs_taba-ddtext.
    gs_dropdown-int_value  = fs_taba-domvalue_l.
    APPEND gs_dropdown TO gt_dropdown.
  ENDLOOP.

*  ASSIGN lt_faglflexa2[] TO <ft_data>.
*
*  SELECT ryear  docnr rbukrs  poper racct tsl hsl
*    FROM faglflexa
*    INTO CORRESPONDING FIELDS OF TABLE lt_faglflexa
*    WHERE ryear  EQ p_gjahr
*    AND   rldnr  EQ '0L'
*    AND   rbukrs EQ p_bukrs
*    AND   racct  LIKE '193%'.
*
*  CHECK lt_faglflexa[] IS NOT INITIAL.
*
*  SELECT ryear  docnr rbukrs  poper racct tsl hsl
*    FROM faglflexa
*    INTO CORRESPONDING FIELDS OF TABLE lt_faglflexa3
*    FOR ALL ENTRIES IN lt_faglflexa
*    WHERE ryear  EQ lt_faglflexa-ryear
*    AND   rldnr  EQ '0L'
*    AND   docnr  EQ lt_faglflexa-docnr
*    AND   rbukrs EQ lt_faglflexa-rbukrs
*    AND   racct  LIKE '642%'.
*
*  CHECK lt_faglflexa3[] IS NOT INITIAL.
*
*  SELECT ryear  docnr rbukrs  poper racct tsl hsl
*    FROM faglflexa
*    INTO CORRESPONDING FIELDS OF TABLE lt_faglflexa2
*    FOR ALL ENTRIES IN lt_faglflexa3
*    WHERE ryear  EQ lt_faglflexa3-ryear
*    AND   rldnr  EQ '0L'
*    AND   docnr  EQ lt_faglflexa3-docnr
*    AND   rbukrs EQ lt_faglflexa3-rbukrs
*    AND   racct  LIKE '102%'
*    AND   drcrk  EQ 'S'.
*
*  CHECK lt_faglflexa2[] IS NOT INITIAL.
*
*  SELECT * FROM zfi_ebyn_t_kyovl
*    INTO TABLE lt_log
*    WHERE bukrs EQ p_bukrs
*    AND   gjahr EQ p_gjahr.
*
*  SORT lt_log BY bukrs gjahr hkont.
*
**  lt_faglflexa2[] = lt_faglflexa[].
**
**  DELETE lt_faglflexa  WHERE racct(3) = '102'.
**  DELETE lt_faglflexa2 WHERE racct(3) = '193'.
*
*  <ft_data>[] = lt_faglflexa2[].
*
*  CLEAR:gt_kyov,gt_kyov[],gt_bynmozet,gt_bynmozet[].
*  LOOP AT <ft_data> ASSIGNING <fs_data> .
*
**    CHECK <fs_data>-racct(3) = '102'.
*
*    CLEAR gt_kyov.
*    gt_kyov-bukrs = <fs_data>-rbukrs.
*    gt_kyov-gjahr = <fs_data>-ryear.
*    gt_kyov-monat = <fs_data>-poper+1(2).
*    gt_kyov-hkont = <fs_data>-racct.
**    READ TABLE lt_faglflexa3 ASSIGNING <fs_data2>
**                             WITH KEY ryear  = <fs_data>-ryear
**                                      rbukrs = <fs_data>-rbukrs
**                                      docnr  = <fs_data>-docnr.
**    gt_kyov-dmbtr = <fs_data2>-hsl * -1. "xaatan
*
*    gt_kyov-dmbtr = <fs_data>-hsl . "acenger 27.12.2016
**    CLEAR <fs_data2>. "acenger 27.12.2016
**    gt_kyov-dmbtr = <fs_data>-tsl.
**    gt_kyov-belnr = <fs_data>-docnr.
*
*    SELECT SINGLE txt50 FROM skat INTO gt_kyov-hkontt
*      WHERE spras EQ sy-langu
*      AND   ktopl EQ 'TGSH'
*      AND   saknr EQ gt_kyov-hkont.
*
*    LOOP AT lt_faglflexa INTO ls_faglflexa
*                         WHERE docnr = <fs_data>-docnr.
**      ADD ls_faglflexa-tsl TO gt_kyov-mwsts.
*      ADD ls_faglflexa-hsl TO gt_kyov-mwsts. "xaatan
*    ENDLOOP.
*
*    CLEAR lt_log.
*    READ TABLE lt_log WITH KEY bukrs = <fs_data>-rbukrs
*                               gjahr = <fs_data>-ryear
*                               hkont = <fs_data>-racct
*                               monat = <fs_data>-poper+1(2)
*                               BINARY SEARCH.
*    IF sy-subrc EQ 0 .
*      gt_kyov-banka = lt_log-banka.
*      gt_kyov-stcd2 = lt_log-stcd2.
*    ELSE.
*      READ TABLE lt_kyovh WITH KEY bukrs = <fs_data>-rbukrs
*                                   hkont = <fs_data>-racct.
*      IF sy-subrc EQ 0.
*        gt_kyov-banka = lt_kyovh-banka.
*        gt_kyov-stcd2 = lt_kyovh-stcd2.
*      ENDIF.
*    ENDIF.
*
*
*    COLLECT gt_kyov.
*
*    CLEAR gt_bynmozet.
*
**    PERFORM get_domain_text USING 'ZFI_EBYN_D_BANKA'
**                                   gt_kyov-banka
**                                   gt_kyov-bankat.
*
*    IF <fs_data>-poper BETWEEN '001' AND '003'.
*      CONCATENATE '01' p_gjahr '03' p_gjahr INTO gt_bynmozet-donem.
**      gt_byn mozet-donem = '01' && p_gjahr && '03' && p_gjahr.
*    ELSEIF <fs_data>-poper BETWEEN '004' AND '006'.
*      CONCATENATE '04' p_gjahr '06' p_gjahr INTO gt_bynmozet-donem.
**      gt_bynmozet-donem = '04' && p_gjahr && '06' && p_gjahr.
*    ELSEIF <fs_data>-poper BETWEEN '007' AND '009'.
*      CONCATENATE '07' p_gjahr '09' p_gjahr INTO gt_bynmozet-donem.
**      gt_bynmozet-donem = '07' && p_gjahr && '09' && p_gjahr.
*    ELSEIF <fs_data>-poper BETWEEN '010' AND '012'.
*      CONCATENATE '10' p_gjahr '12' p_gjahr INTO gt_bynmozet-donem.
**      gt_bynmozet-donem = '10' && p_gjahr && '12' && p_gjahr.
*    ENDIF.
*
*
*    gt_bynmozet-dmbtr = gt_kyov-dmbtr.
*    gt_bynmozet-mwsts = gt_kyov-mwsts.
*    gt_bynmozet-banka = gt_kyov-banka.
*    gt_bynmozet-stcd2 = gt_kyov-stcd2.
*
*    COLLECT gt_bynmozet.
*
*  ENDLOOP.
  DATA:  BEGIN OF lt_bsis OCCURS 0,
           bukrs TYPE bsis-bukrs,
           belnr TYPE bsis-belnr,
           gjahr TYPE bsis-gjahr,
           monat TYPE bsis-monat,
           budat TYPE bsis-budat,
           bldat TYPE bsis-bldat,
           hkont TYPE bsis-hkont,
           shkzg TYPE bsis-shkzg,
           dmbtr TYPE bsis-dmbtr,
           zuonr TYPE bsis-zuonr,
           waers TYPE bsis-waers,
           bankl TYPE zfi_ebyn_t_kyovh-bankl,
           stcd2 TYPE zfi_ebyn_t_kyovh-stcd2,
         END OF lt_bsis,
         BEGIN OF lt_collect OCCURS 0,
           bukrs TYPE bsis-bukrs,
           belnr TYPE bsis-belnr,
           gjahr TYPE bsis-gjahr,
           monat TYPE bsis-monat,
           budat TYPE bsis-budat,
           bldat TYPE bsis-bldat,
           waers TYPE bsis-waers,
*           bankl TYPE zfi_ebyn_t_kyovh-bankl,
*           stcd2 TYPE zfi_ebyn_t_kyovh-stcd2,
         END OF lt_collect,
         lv_domvalue_l TYPE dd07t-domvalue_l,
         lv_eftkod     TYPE char3.
  DATA: lt_kyovc TYPE TABLE OF zfi_ebyn_t_kyovc WITH HEADER LINE.
  FIELD-SYMBOLS: <fs_bsis>    LIKE LINE OF lt_bsis,
                 <fs_bsis2>   LIKE LINE OF lt_bsis,
                 <fs_collect> LIKE LINE OF lt_collect.

  DATA: lr_bsis LIKE REF TO lt_bsis.

  RANGES : lr_hkont2 FOR bsis-hkont.

  CLEAR : lr_hkont2[], lr_hkont2.
  lr_hkont2-sign   = 'I'.
  lr_hkont2-option = 'EQ'.
  lr_hkont2-low    = '1930101001'.
  APPEND lr_hkont2.
  lr_hkont2-option = 'CP'.
  lr_hkont2-low    = '102*'.
  APPEND lr_hkont2.
  lr_hkont2-option = 'CP'.
  lr_hkont2-low    = '118*'.
  APPEND lr_hkont2.
  lr_hkont2-low    = '642*'.
  APPEND lr_hkont2.

  SELECT bsis~bukrs,
         bsis~belnr,
         bsis~gjahr,
         bsis~monat,
         bsis~budat,
         bsis~bldat,
         bsis~hkont,
         bsis~shkzg,
         bsis~dmbtr,
         bsis~zuonr,
         bsis~waers,
         z1~bankl,
         z1~stcd2
    FROM bsis INNER JOIN bkpf ON bsis~bukrs = bkpf~bukrs
                             AND bsis~belnr = bkpf~belnr
                             AND bsis~gjahr = bkpf~gjahr
    LEFT JOIN t012k ON t012k~bukrs EQ bsis~bukrs
                   AND t012k~hkont EQ bsis~hkont
    LEFT JOIN t012 ON t012~bukrs EQ t012k~bukrs
                  AND t012~hbkid EQ t012k~hbkid
    LEFT JOIN zfi_ebyn_t_kyovh AS z1 ON z1~bankl EQ t012~bankl
    INTO TABLE @lt_bsis
   WHERE bsis~bukrs EQ @p_bukrs
     AND bsis~gjahr EQ @p_gjahr
     AND bsis~hkont IN @lr_hkont2
     AND bsis~monat BETWEEN '01' AND '12'
     AND bkpf~stblg EQ @space
     AND bkpf~stgrd EQ @space.

  SELECT bsas~bukrs,
         bsas~belnr,
         bsas~gjahr,
         bsas~monat,
         bsas~budat,
         bsas~bldat,
         bsas~hkont,
         bsas~shkzg,
         bsas~dmbtr,
         bsas~zuonr,
         bsas~waers,
         z1~bankl ,
         z1~stcd2
    FROM bsas INNER JOIN bkpf ON bsas~bukrs = bkpf~bukrs
                             AND bsas~belnr = bkpf~belnr
                             AND bsas~gjahr = bkpf~gjahr
    LEFT JOIN t012k ON t012k~bukrs EQ bsas~bukrs
                   AND t012k~hkont EQ bsas~hkont
    LEFT JOIN t012 ON t012~bukrs EQ t012k~bukrs
                  AND t012~hbkid EQ t012k~hbkid
    LEFT JOIN zfi_ebyn_t_kyovh AS z1 ON z1~bankl EQ t012~bankl
    APPENDING TABLE @lt_bsis
   WHERE bsas~bukrs EQ @p_bukrs
     AND bsas~gjahr EQ @p_gjahr
     AND bsas~hkont IN @lr_hkont2
     AND bsas~monat BETWEEN '01' AND '12'
     AND bkpf~stblg EQ @space
     AND bkpf~stgrd EQ @space.

*-oerdem 20.05.2020
  SELECT * FROM zfi_ebyn_t_kyovc
    INTO CORRESPONDING FIELDS OF TABLE lt_kyovc
    FOR ALL ENTRIES IN lt_bsis
    WHERE bukrs EQ lt_bsis-bukrs
      AND hkont EQ lt_bsis-hkont.

  LOOP AT lt_bsis REFERENCE INTO lr_bsis.
    READ TABLE lt_kyovc WITH KEY hkont = lr_bsis->hkont.
    IF sy-subrc IS INITIAL.
      lr_bsis->bankl = '0' && lt_kyovc-banka.
      lr_bsis->stcd2 = lt_kyovc-stcd2.
    ENDIF.
  ENDLOOP.
*-oerdem 20.05.2020

* Begin of kates - 09.04.2020 14:34:19

  SELECT bsis~bukrs,
         bsis~belnr,
         bsis~gjahr,
         bsis~monat,
         bsis~budat,
         bsis~bldat,
         bsis~hkont,
         bsis~shkzg,
         bsis~dmbtr,
         bsis~zuonr,
         bsis~waers
    FROM bsis INNER JOIN bkpf ON bsis~bukrs = bkpf~bukrs
                             AND bsis~belnr = bkpf~belnr
                             AND bsis~gjahr = bkpf~gjahr
    INTO TABLE @DATA(lt_bsis_642)
   WHERE bsis~bukrs EQ @p_bukrs
     AND bsis~gjahr EQ @p_gjahr
     AND bsis~hkont EQ '6420101001'
     AND bsis~monat BETWEEN '01' AND '12'
     AND bkpf~blart EQ 'S2'
     AND bkpf~stblg EQ @space
     AND bkpf~stgrd EQ @space.
* End of kates - 09.04.2020 14:34:19

  CHECK lt_bsis[] IS NOT INITIAL.

  SELECT * FROM zfi_ebyn_t_kyovl
    INTO TABLE lt_log
    WHERE bukrs EQ p_bukrs
    AND   gjahr EQ p_gjahr.

  SORT lt_log BY bukrs belnr gjahr.

  SORT lt_bsis BY bukrs belnr gjahr hkont.
  LOOP AT lt_bsis ASSIGNING <fs_bsis>.
    CLEAR gt_kyov.

    READ TABLE lt_bsis TRANSPORTING NO FIELDS
      WITH KEY bukrs    = <fs_bsis>-bukrs
               belnr    = <fs_bsis>-belnr
               gjahr    = <fs_bsis>-gjahr
               hkont    = '1930101001'
      BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    READ TABLE lt_bsis TRANSPORTING NO FIELDS
      WITH KEY bukrs    = <fs_bsis>-bukrs
               belnr    = <fs_bsis>-belnr
               gjahr    = <fs_bsis>-gjahr
               hkont(3) = '102'
     BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      READ TABLE lt_bsis TRANSPORTING NO FIELDS
        WITH KEY bukrs    = <fs_bsis>-bukrs
                 belnr    = <fs_bsis>-belnr
                 gjahr    = <fs_bsis>-gjahr
                 hkont(3) = '118'
       BINARY SEARCH.
    ENDIF.
    CHECK sy-subrc EQ 0.

    CLEAR lt_collect .
    MOVE-CORRESPONDING <fs_bsis> TO lt_collect.
    COLLECT lt_collect.
  ENDLOOP.


  LOOP AT lt_collect ASSIGNING <fs_collect>.
    CLEAR gt_kyov.
    MOVE-CORRESPONDING <fs_collect> TO gt_kyov.

    LOOP AT lt_bsis ASSIGNING <fs_bsis>
                        WHERE bukrs EQ <fs_collect>-bukrs
                          AND belnr EQ <fs_collect>-belnr
                          AND gjahr EQ <fs_collect>-gjahr
                          AND hkont CP '193*'.
      IF <fs_bsis>-shkzg EQ 'H'.
        <fs_bsis>-dmbtr = <fs_bsis>-dmbtr * -1.
      ENDIF.

      ADD <fs_bsis>-dmbtr TO gt_kyov-mwsts.
    ENDLOOP.

    LOOP AT lt_bsis ASSIGNING <fs_bsis>
                        WHERE bukrs EQ <fs_collect>-bukrs
                          AND belnr EQ <fs_collect>-belnr
                          AND gjahr EQ <fs_collect>-gjahr
                          AND hkont CP '642*'.
      IF <fs_bsis>-shkzg EQ 'H'.
        <fs_bsis>-dmbtr = <fs_bsis>-dmbtr * -1.
      ENDIF.

      ADD <fs_bsis>-dmbtr TO gt_kyov-dmbtr.
    ENDLOOP.
    IF sy-subrc IS NOT INITIAL.
*      gt_kyov-dmbtr = gt_kyov-mwsts * 100 / 15.

*-&Multi line case->
      DATA: lt_multi_642 LIKE LINE OF lt_bsis_642 OCCURS 0.
      REFRESH: lt_multi_642.
      LOOP AT lt_bsis_642 ASSIGNING FIELD-SYMBOL(<fs_642>)
                          WHERE bldat = <fs_collect>-bldat
                            AND waers = <fs_collect>-waers.
-->begin of-fkarakas - 04.04.2022                            
        LOOP AT lt_bsis INTO DATA(ls_bsis)
                                  WHERE bukrs = <fs_collect>-bukrs AND
                                        belnr = <fs_collect>-belnr AND
                                        gjahr = <fs_collect>-gjahr AND
                                        hkont CP '102*'.
          EXIT.
        ENDLOOP.
        IF sy-subrc IS INITIAL.
          LOOP AT lt_bsis INTO DATA(ls_bsis2)
                                    WHERE bukrs = <fs_642>-bukrs AND
                                          belnr = <fs_642>-belnr AND
                                          gjahr = <fs_642>-gjahr AND
                                          hkont CP '102*'.
            EXIT.
          ENDLOOP.
          IF sy-subrc IS INITIAL.
            IF ls_bsis-hkont = ls_bsis2-hkont .
              APPEND INITIAL LINE TO lt_multi_642
                REFERENCE INTO DATA(lr_multi).
              MOVE-CORRESPONDING <fs_642> TO lr_multi->*.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
<--end of-fkarakas - 04.04.2022   
      SORT lt_multi_642 BY dmbtr ASCENDING.

      TYPES: BEGIN OF ty_multi,
               bukrs TYPE bkpf-bukrs,
               gjahr TYPE bkpf-gjahr,
               belnr TYPE bkpf-belnr,
               budat TYPE bkpf-budat,
               waers TYPE bkpf-waers,
               dmbtr TYPE dmbtr,
             END OF ty_multi.
      DATA: lt_multi_tax TYPE TABLE OF ty_multi,
            ls_tax       TYPE ty_multi.
      REFRESH: lt_multi_tax.
      LOOP AT lt_bsis ASSIGNING FIELD-SYMBOL(<fs_tax>)
                      WHERE budat EQ <fs_collect>-budat
                        AND waers EQ <fs_collect>-waers
                        AND hkont CP '193*'.
-->begin of-fkarakas - 04.04.2022
        LOOP AT lt_bsis INTO DATA(ls_bsis3)
                            WHERE bukrs = <fs_collect>-bukrs AND
                                  belnr = <fs_collect>-belnr AND
                                  gjahr = <fs_collect>-gjahr AND
                                  hkont CP '102*'.
          EXIT.
        ENDLOOP.
        IF sy-subrc IS INITIAL.
          LOOP AT lt_bsis INTO DATA(ls_bsis4)
                               WHERE bukrs = <fs_tax>-bukrs AND
                                     belnr = <fs_tax>-belnr AND
                                     gjahr = <fs_tax>-gjahr AND
                                     hkont CP '102*'.
            EXIT.
          ENDLOOP.
          IF sy-subrc IS INITIAL.
            IF ls_bsis3-hkont = ls_bsis4-hkont .
              CLEAR: ls_tax.
              MOVE-CORRESPONDING <fs_tax> TO ls_tax.
              COLLECT ls_tax INTO lt_multi_tax.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
<--end of-fkarakas - 04.04.2022
      SORT lt_multi_tax BY dmbtr ASCENDING.

      DESCRIBE TABLE lt_multi_642 LINES DATA(lv_size).

      READ TABLE lt_bsis_642 REFERENCE INTO DATA(lr_bsis_642)
      WITH KEY bldat = <fs_collect>-bldat
               waers = <fs_collect>-waers.
      IF sy-subrc EQ 0.
        IF lv_size GT 1.
          READ TABLE lt_multi_tax
            REFERENCE INTO DATA(lr_tax) WITH KEY dmbtr = gt_kyov-mwsts.
          IF sy-subrc IS INITIAL.
            READ TABLE lt_multi_642
              REFERENCE INTO lr_multi INDEX sy-tabix.
            IF sy-subrc IS INITIAL.
              gt_kyov-dmbtr = lr_multi->dmbtr.
            ENDIF.
          ENDIF.
        ELSE.
          gt_kyov-dmbtr = lr_bsis_642->dmbtr.
        ENDIF.
      ENDIF.
    ELSE.
      gt_kyov-dmbtr = abs( gt_kyov-dmbtr ).
    ENDIF.


*    READ TABLE lt_kyovh WITH KEY banka = gt_kyov-banka.
*    IF sy-subrc EQ 0.
*      gt_kyov-stcd2 = lt_kyovh-stcd2.
*    ENDIF.
* Begin of kates - 10.04.2020 15:03:48

    READ TABLE lt_bsis REFERENCE INTO lr_bsis
      WITH KEY bukrs    = <fs_collect>-bukrs
               belnr    = <fs_collect>-belnr
               gjahr    = <fs_collect>-gjahr
               hkont(3) = '102'
     BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      READ TABLE lt_bsis REFERENCE INTO lr_bsis
        WITH KEY bukrs    = <fs_collect>-bukrs
                 belnr    = <fs_collect>-belnr
                 gjahr    = <fs_collect>-gjahr
                 hkont(3) = '118'
       BINARY SEARCH.
    ENDIF.
    IF sy-subrc EQ 0.
      gt_kyov-stcd2 = lr_bsis->stcd2.
      IF gt_kyov-banka IS INITIAL.
        CLEAR: lv_domvalue_l,lv_eftkod.
        lv_eftkod = lr_bsis->bankl+1(3).
        SELECT SINGLE domvalue_l
          FROM dd07t
          INTO lv_domvalue_l
         WHERE domname    EQ 'ZFI_EBYN_D_BANKA'
           AND ddlanguage EQ 'T'
           AND as4local   EQ 'A'
           AND domvalue_l EQ lv_eftkod.
        gt_kyov-banka = lv_domvalue_l.
      ENDIF.
    ENDIF.
* End of kates - 10.04.2020 15:03:48

    CLEAR lt_log.
*    READ TABLE lt_log WITH KEY bukrs = gt_kyov-bukrs
*                               belnr = gt_kyov-belnr
*                               gjahr = gt_kyov-gjahr.
*    IF sy-subrc EQ 0 .
*      gt_kyov-dmbtr = lt_log-dmbtr.
*      gt_kyov-mwsts = lt_log-mwsts.
*    ENDIF.
    APPEND gt_kyov.


    CLEAR gt_bynmozet.
    MOVE-CORRESPONDING gt_kyov TO gt_bynmozet.
    IF gt_kyov-monat BETWEEN '01' AND '03'.
      CONCATENATE '01' p_gjahr '03' p_gjahr INTO gt_bynmozet-donem.
    ELSEIF gt_kyov-monat BETWEEN '04' AND '06'.
      CONCATENATE '04' p_gjahr '06' p_gjahr INTO gt_bynmozet-donem.
    ELSEIF gt_kyov-monat BETWEEN '07' AND '09'.
      CONCATENATE '07' p_gjahr '09' p_gjahr INTO gt_bynmozet-donem.
    ELSEIF gt_kyov-monat BETWEEN '10' AND '12'.
      CONCATENATE '10' p_gjahr '12' p_gjahr INTO gt_bynmozet-donem.
    ENDIF.
    COLLECT gt_bynmozet.

  ENDLOOP.

  LOOP AT lt_log  WHERE belnr IS INITIAL.
    CLEAR gt_kyov.
    MOVE-CORRESPONDING lt_log TO gt_kyov.
    APPEND gt_kyov.

    CLEAR gt_bynmozet.
    MOVE-CORRESPONDING gt_kyov TO gt_bynmozet.
    IF gt_kyov-monat BETWEEN '01' AND '03'.
      CONCATENATE '01' p_gjahr '03' p_gjahr INTO gt_bynmozet-donem.
    ELSEIF gt_kyov-monat BETWEEN '04' AND '06'.
      CONCATENATE '04' p_gjahr '06' p_gjahr INTO gt_bynmozet-donem.
    ELSEIF gt_kyov-monat BETWEEN '07' AND '09'.
      CONCATENATE '07' p_gjahr '09' p_gjahr INTO gt_bynmozet-donem.
    ELSEIF gt_kyov-monat BETWEEN '10' AND '12'.
      CONCATENATE '10' p_gjahr '12' p_gjahr INTO gt_bynmozet-donem.
    ENDIF.
    COLLECT gt_bynmozet.

  ENDLOOP.

*-&Begin of aatan/13.05.2020 |->
*-&Vkn ve banka bilgisi boş olan kayıtların güncellenmesi;
  SELECT bukrs,
         hkont,
         stcd2,
         banka
         FROM zfi_ebyn_t_kyovc
         INTO TABLE @DATA(t_kyovc)
         WHERE bukrs = @p_bukrs. CHECK NOT sy-subrc <> 0.
  SORT t_kyovc BY bukrs hkont.

  LOOP AT gt_kyov ASSIGNING FIELD-SYMBOL(<fs_kyov>)
                  WHERE stcd2 IS INITIAL OR
                        banka IS INITIAL.
    LOOP AT lt_bsis ASSIGNING FIELD-SYMBOL(<fs_doc>)
                    WHERE bukrs EQ <fs_kyov>-bukrs
                      AND gjahr EQ <fs_kyov>-gjahr
                      AND belnr EQ <fs_kyov>-belnr
                      AND hkont CP '102*'.
      EXIT.
    ENDLOOP.
    CHECK NOT sy-subrc <> 0.
    READ TABLE t_kyovc ASSIGNING FIELD-SYMBOL(<fs_kyovc>)
                       WITH KEY bukrs = <fs_doc>-bukrs
                                hkont = <fs_doc>-hkont.
    CHECK NOT sy-subrc <> 0.
    <fs_kyov>-stcd2 = <fs_kyovc>-stcd2.
    <fs_kyov>-banka = <fs_kyovc>-banka.
  ENDLOOP.
*-&End of aatan/13.05.2020 <-|

ENDFORM.   
