*&---------------------------------------------------------------------*
*& Include          ZFK_ODEV5_TOP_CLASS
*&---------------------------------------------------------------------*
CLASS lcl_t001 DEFINITION.
  PUBLIC SECTION.
    METHODS:
      get_data,
      merge,
      display,
      build_html
        RETURNING VALUE(rt_html) TYPE w3_htmltab.

ENDCLASS.

CLASS lcl_t001 IMPLEMENTATION.
  METHOD get_data.

    TYPES: BEGIN OF ty_basedat,
             bukrs TYPE t001-bukrs,
             butxt TYPE t001-butxt,
             hbkid TYPE t012-hbkid,
             hktid TYPE t012k-hktid,
             banks TYPE t012-banks,
             bankl TYPE t012-bankl,
             banka TYPE bnka-banka,
             hkont TYPE t012k-hkont,
             waers TYPE t012k-waers,
             txt50 TYPE skat-txt50,
             text1 TYPE t012t-text1,
             tslvt TYPE faglflext-tslvt,
             hslvt TYPE faglflext-hslvt,

           END OF ty_basedat.

    DATA: t_basedat  TYPE TABLE OF ty_basedat,
          ls_basedat TYPE ty_basedat.


    SELECT t001~bukrs,
           t001~butxt,
           t012~hbkid,
           t012~banks,
           t012~bankl,
           bnka~banka,
           t012k~hktid,
           t012k~hkont,
           skat~txt50,
           t012k~waers,
           t012t~text1
           FROM t001
           INNER JOIN t012 ON t012~bukrs = t001~bukrs
           INNER JOIN bnka ON bnka~banks = t012~banks AND
                              bnka~bankl = t012~bankl
           INNER JOIN  t012k ON t012k~bukrs = t012~bukrs AND
                                t012k~hbkid = t012~hbkid
           LEFT JOIN skat ON skat~spras = 'TR' AND
                             skat~ktopl = t001~ktopl AND
                             skat~saknr = t012k~hkont
           LEFT JOIN t012t ON t012t~bukrs = t012k~bukrs  AND
                              t012t~hbkid = t012k~hbkid AND
                              t012t~hktid = t012k~hktid AND
                              t012t~spras = 'TR'
           INTO CORRESPONDING FIELDS OF TABLE @t_basedat
           WHERE t001~bukrs  EQ @p_bukrs
             AND t012~hbkid  IN @s_hbkid
             AND t012k~waers IN @s_waers.


    LOOP AT t_basedat INTO ls_basedat .

      CLEAR: gs_banka.
*      gs_banka-bukrs = ls_basedat-bukrs.
*      gs_banka-butxt = ls_basedat-butxt.
*      gs_banka-hbkid = ls_basedat-hbkid.
*      gs_banka-hktid = ls_basedat-hktid.
*      gs_banka-banka = ls_basedat-banka.
*      gs_banka-hkont = ls_basedat-hkont.
*      gs_banka-txt50 = ls_basedat-txt50.
*      gs_banka-waers = ls_basedat-waers.
*      gs_banka-text1 = ls_basedat-text1.
      MOVE-CORRESPONDING ls_basedat TO gs_banka.
      APPEND gs_banka TO gt_banka.
    ENDLOOP.

*tutar hesaplama kısmı
    TYPES: BEGIN OF ty_faglflext,
             rbukrs TYPE faglflext-rbukrs,
             hslvt  TYPE faglflext-hslvt,
             hsl01  TYPE faglflext-hsl01,
             hsl02  TYPE faglflext-hsl02,
             hsl03  TYPE faglflext-hsl03,
             hsl04  TYPE faglflext-hsl04,
             hsl05  TYPE faglflext-hsl05,
             hsl06  TYPE faglflext-hsl06,
             hsl07  TYPE faglflext-hsl07,
             hsl08  TYPE faglflext-hsl08,
             hsl09  TYPE faglflext-hsl09,
             hsl10  TYPE faglflext-hsl10,
             hsl11  TYPE faglflext-hsl11,
             hsl12  TYPE faglflext-hsl12,
             tslvt  TYPE faglflext-tslvt,
             tsl01  TYPE faglflext-tsl01,
             tsl02  TYPE faglflext-tsl02,
             tsl03  TYPE faglflext-tsl03,
             tsl04  TYPE faglflext-tsl04,
             tsl05  TYPE faglflext-tsl05,
             tsl06  TYPE faglflext-tsl06,
             tsl07  TYPE faglflext-tsl07,
             tsl08  TYPE faglflext-tsl08,
             tsl09  TYPE faglflext-tsl09,
             tsl10  TYPE faglflext-tsl10,
             tsl11  TYPE faglflext-tsl11,
             tsl12  TYPE faglflext-tsl12,
             hbkid  TYPE t012k-hbkid,
             racct  TYPE faglflext-racct,
           END OF ty_faglflext,

           BEGIN OF ty_banka_tutar,
             hbkid TYPE t012k-hbkid,
             tutar TYPE faglflext-hsl02,
           END OF  ty_banka_tutar.


    DATA: t_faglflext   TYPE STANDARD TABLE OF ty_faglflext,
          s_faglflext   TYPE ty_faglflext,
          t_banka_tutar TYPE STANDARD TABLE OF ty_banka_tutar,
          s_banka_tutar TYPE ty_banka_tutar.

    TYPES: BEGIN OF ty_tabname,
             col_name TYPE char20,
           END OF ty_tabname.

    DATA: t_tabname TYPE STANDARD TABLE OF fieldname,
          s_tabname TYPE fieldname,
          v_sayac   TYPE numc2.

    CLEAR: s_tabname.
    s_tabname = 'HSLVT'.
    APPEND s_tabname TO t_tabname.

    CLEAR: s_tabname.
    s_tabname = 'TSLVT'.
    APPEND s_tabname TO t_tabname.

    CLEAR: s_tabname.
    s_tabname = 'RBUKRS'.
    APPEND s_tabname TO t_tabname.

    CLEAR s_tabname.
    s_tabname = 'HBKID'.
    APPEND s_tabname TO t_tabname.

    CLEAR s_tabname.
    s_tabname = 'RACCT'.
    APPEND s_tabname TO t_tabname.


    CLEAR: v_sayac.
    DO p_donem TIMES.
      ADD 1 TO v_sayac.

      CONCATENATE 'HSL' v_sayac INTO s_tabname.
      APPEND s_tabname TO t_tabname.

      CONCATENATE 'TSL' v_sayac INTO s_tabname.
      APPEND s_tabname TO t_tabname.
    ENDDO.


    SELECT (t_tabname)
      FROM faglflext AS a
     INNER JOIN t012k AS b ON b~hkont EQ a~racct
      INTO CORRESPONDING FIELDS OF TABLE t_faglflext
        WHERE ryear = p_yil
          AND rbukrs = p_bukrs
          AND rldnr = '0L'.


    LOOP AT gt_banka INTO gs_banka.

      LOOP AT t_faglflext INTO s_faglflext WHERE  hbkid = gs_banka-hbkid AND racct = gs_banka-hkont .

        s_banka_tutar-hbkid = s_faglflext-hbkid.

        gs_banka-hslxx = gs_banka-hslxx + s_faglflext-hslvt + s_faglflext-hsl01 + s_faglflext-hsl02 +
                         s_faglflext-hsl03 + s_faglflext-hsl04 + s_faglflext-hsl05 +
                         s_faglflext-hsl06 + s_faglflext-hsl07 + s_faglflext-hsl08 +
                         s_faglflext-hsl09 + s_faglflext-hsl10 + s_faglflext-hsl11 + s_faglflext-hsl12.

        gs_banka-tslxx =  gs_banka-tslxx + s_faglflext-tslvt + s_faglflext-tsl01 + s_faglflext-tsl02 +
                          s_faglflext-tsl03 + s_faglflext-tsl04 + s_faglflext-tsl05 +
                          s_faglflext-tsl06 + s_faglflext-tsl07 + s_faglflext-tsl08 +
                          s_faglflext-tsl09 + s_faglflext-tsl10 + s_faglflext-tsl11 + s_faglflext-tsl12.


      ENDLOOP.

      MODIFY gt_banka FROM gs_banka.



    ENDLOOP.


  ENDMETHOD.

  METHOD merge.

    CLEAR : gs_fieldcat.
    gs_fieldcat-fieldname = 'TSLXX'.
    gs_fieldcat-scrtext_l = 'Toplam (İşlem PB)'.
    gs_fieldcat-scrtext_m = 'Toplam (İşlem PB)'.
    gs_fieldcat-scrtext_s = 'Toplam (İşlem PB)'.
    gs_fieldcat-reptext = 'Toplam (İşlem PB)'.

    APPEND gs_fieldcat TO gt_fieldcat.

    CLEAR : gs_fieldcat.
    gs_fieldcat-fieldname = 'HSLXX'.
    gs_fieldcat-scrtext_m = 'Toplam (Ulusal PB)'.
    gs_fieldcat-scrtext_l = 'Toplam (Ulusal PB)'.
    gs_fieldcat-scrtext_s = 'Toplam (Ulusal PB)'.
    gs_fieldcat-reptext = 'Toplam (Ulusal PB)'.
    APPEND gs_fieldcat TO gt_fieldcat.



    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZFK_S_ODEV5'
      CHANGING
        ct_fieldcat      = gt_fieldcat.

    gs_layout-cwidth_opt = abap_true.

*    LOOP AT gt_fieldcat INTO gs_fieldcat.
*      CLEAR gs_fieldcat.
*      CASE gs_fieldcat-fieldname.
*        WHEN 'TSLXX'.
*          gs_fieldcat-scrtext_l =
*          gs_fieldcat-scrtext_m =
*          gs_fieldcat-scrtext_s =
*          gs_fieldcat-reptext = 'Toplam (İşlem PB)'.
*
*        WHEN 'HSLXX'.
*          gs_fieldcat-scrtext_l =
*          gs_fieldcat-scrtext_m =
*          gs_fieldcat-scrtext_s =
*          gs_fieldcat-reptext = 'Toplam (Ulusal PB)'.
*        WHEN OTHERS.
*
*      ENDCASE.
*      MODIFY gt_fieldcat FROM gs_fieldcat.
*    ENDLOOP.

  ENDMETHOD.


  METHOD display.

    CHECK go_cust IS NOT BOUND.

    CREATE OBJECT go_cust
      EXPORTING
        container_name = 'CC_ODEV'
      EXCEPTIONS
        OTHERS         = 1.

    CHECK sy-subrc IS INITIAL.

    CREATE OBJECT go_split
      EXPORTING
        parent  = go_cust
        rows    = 2
        columns = 1
      EXCEPTIONS
        OTHERS  = 1.

    CHECK sy-subrc IS INITIAL.

    CALL METHOD go_split->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = go_cont1.

    CREATE OBJECT go_alv
      EXPORTING
        i_parent = go_cont1
      EXCEPTIONS
        OTHERS   = 1.

    CHECK sy-subrc EQ 0.
    me->merge( ).
    CALL METHOD go_alv->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
      CHANGING
        it_fieldcatalog = gt_fieldcat[]
        it_outtab       = gt_banka.

    CALL METHOD go_split->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = go_cont2.

    CREATE OBJECT go_html
      EXPORTING
        parent = go_cont2.


    DATA: doc_url(80), lt_html TYPE TABLE OF w3_html.

    lt_html = me->build_html( ).

    go_html->load_data(
    IMPORTING
      assigned_url = doc_url
    CHANGING
      data_table = lt_html ).

    go_html->show_url(
      EXPORTING
        url = doc_url ).



  ENDMETHOD.
  METHOD build_html.
    DEFINE add_html.
      APPEND &1 TO rt_html.
    END-OF-DEFINITION.

    DEFINE add_rapor.
      APPEND &1 TO rt_html.
    END-OF-DEFINITION.

    add_html:
    '<html>',
    '<body>',
    '<h2>Banka Bakiyesi</h2>',

    '<script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>',
    '<div id="piechart" style="width: 900px; height: 500px;"></div>',

    '<script>',
      'google.charts.load(''current'', {''packages'':[''corechart'']});',
      'google.charts.setOnLoadCallback(drawChart);',

      'function drawChart() {',
        'var data = google.visualization.arrayToDataTable([',
        '[''Banka'', ''Tutar''],'.


    TYPES: BEGIN OF ty_chart,
             toplam TYPE i,
             banka  TYPE bnka-banka,
           END OF ty_chart.

    DATA: gs_chart TYPE ty_chart,
          gt_chart TYPE TABLE OF ty_chart.

    LOOP AT gt_banka INTO gs_banka.
      gs_chart-banka = gs_banka-banka.
      gs_chart-toplam =  gs_banka-hslxx.
      COLLECT gs_chart INTO gt_chart.
    ENDLOOP.

    LOOP AT gt_chart INTO gs_chart.
      APPEND | [' { gs_chart-banka } ' , { gs_chart-toplam } ] , | TO rt_html.

    ENDLOOP.

    add_rapor:
          ']);',
          'var options = {',
            'title: ''Bakiye(UPB)''',
          '};',
          'var chart = new google.visualization.PieChart(document.getElementById(''piechart''));',
          'chart.draw(data, options);',
        '}',
    '</script>',
    '</body>',
    '</html> '.
  ENDMETHOD.

ENDCLASS.
