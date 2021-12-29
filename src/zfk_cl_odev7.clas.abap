class ZFK_CL_ODEV7 definition
  public
  final
  create public .

public section.

  class-data GRID_ALV type ref to CL_GUI_ALV_GRID .
  class-data GO_SPLIT type ref to CL_GUI_SPLITTER_CONTAINER .
  class-data GO_CALENDAR type ref to CL_GUI_CALENDAR .
  class-data GT_PLANLAR type ZFK_TT_PLAN .
  class-data GT_TUMPLANLAR type ZFK_TT_PLAN .
  class-data ITAB_ALV type ZFK_TT_PLAN .
  class-data GT_MUSTERILER type ZFK_TT_TAKVIM .
  class-data GT_PERSONEL type ZFK_TT_SIRKET .
  class-data GT_USERID type ZFK_TT_PERSONEL .
  data SELECTED_BEGIN_DATE type CNCA_UTC_DATE .
  class-data SELECTED_END_DATE type DATUM .
  class-data LO_CONT_ALV type ref to CL_GUI_CONTAINER .
  data GRAFIC_HTML type ref to CL_GUI_HTML_VIEWER .

  methods CONSTRUCTOR .
  methods CREATE_CONTAINERS .
  methods LOGO .
  methods CALENDAR .
  methods PIE_CHART .
  methods GET_DATA .
  methods ADD_ALV
    importing
      !CONTAINER type ref to CL_GUI_CONTAINER optional
      !STRUCTURE type DD02L-TABNAME optional .
  methods ALV_BUTTON
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT .
  methods ALV_USERCOMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods DATE_SELECTED
    for event DATE_SELECTED of CL_GUI_CALENDAR
    importing
      !DATE_BEGIN
      !DATE_END
      !SELECTION_TABLE .
  methods MODIFY_ALV .
  methods CALENDAR_COLORED .
  methods USERCHECK .
  PROTECTED SECTION.
private section.

  data GT_TUMPERSONEL type ZFK_TT_SIRKET .
ENDCLASS.



CLASS ZFK_CL_ODEV7 IMPLEMENTATION.


  METHOD add_alv.
    DATA: lt_fieldcat TYPE lvc_t_fcat,
          ls_fieldcat TYPE lvc_s_fcat,
          ls_layout   TYPE lvc_s_layo.
*    DATA: lo_cont_alv TYPE REF TO cl_gui_container.

    IF grid_alv IS INITIAL.

      CALL METHOD go_split->get_container
        EXPORTING
          row       = 2
          column    = 2
        RECEIVING
          container = lo_cont_alv.

      CREATE OBJECT grid_alv
        EXPORTING
          i_parent = lo_cont_alv.                " Parent Container

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = 'ZFK_S_PLAN'
          i_client_never_display = abap_true
        CHANGING
          ct_fieldcat            = lt_fieldcat.

      LOOP AT lt_fieldcat INTO ls_fieldcat.
        CASE ls_fieldcat-fieldname.
          WHEN 'KULLANICIADI'.
            ls_fieldcat-tech = abap_true.
            MODIFY lt_fieldcat FROM ls_fieldcat.

          WHEN 'SELKZ'.
            ls_fieldcat-tech = abap_true.
            MODIFY lt_fieldcat FROM ls_fieldcat.

        ENDCASE.
      ENDLOOP.

      ls_layout-sel_mode = 'A'.
      ls_layout-box_fname = 'SELKZ'.
      ls_layout-edit_mode = abap_true.
      ls_layout-edit = abap_true.
      ls_layout-cwidth_opt = abap_true.
      SET HANDLER alv_button FOR grid_alv.
      SET HANDLER alv_usercommand FOR grid_alv.

      grid_alv->set_table_for_first_display(
        EXPORTING
*          i_buffer_active               =                  " Buffering Active
*          i_bypassing_buffer            =                  " Switch Off Buffer
*          i_consistency_check           =                  " Starting Consistency Check for Interface Error Recognition
*          i_structure_name              =                  " Internal Output Table Structure Name
*          is_variant                    =                  " Layout
*          i_save                        =                  " Save Layout
*          i_default                     = 'X'              " Default Display Variant
          is_layout                     = ls_layout                 " Layout
        CHANGING
          it_outtab                     = itab_alv[]
          it_fieldcatalog               = lt_fieldcat[]
          ).

    ELSE.
      grid_alv->refresh_table_display(
          EXPORTING
*            is_stable      =
            i_soft_refresh = abap_true ).
    ENDIF.






  ENDMETHOD.


  METHOD alv_button.

    DATA: button     TYPE stb_button,
          lr_toolbar TYPE REF TO stb_button.

    DELETE e_object->mt_toolbar WHERE function NE '&LOCAL&INSERT_ROW'.

    READ TABLE e_object->mt_toolbar REFERENCE INTO lr_toolbar WITH KEY function = '&LOCAL&INSERT_ROW'.
    IF sy-subrc IS INITIAL.
      lr_toolbar->text = 'Satır Ekle'.
    ENDIF.

    button-function = 'SIL'.
    button-icon = '@18@'.
    button-text = 'Satır Sil'.
    button-quickinfo = 'Sil'.
    button-butn_type = 0.
    APPEND button TO e_object->mt_toolbar.

    button-function = 'KAYDET'.
    button-icon = '@2L@'.
    button-text = 'Kaydet'.
    button-quickinfo = 'Kaydet'.
    button-butn_type = 0.
    APPEND button TO e_object->mt_toolbar.

    button-function = 'Eposta'.
    button-icon = '@1S@'.
    button-text = 'E-posta gönder'.
    button-quickinfo = 'E-posta gönder'.
    button-butn_type = 0.
    APPEND button TO e_object->mt_toolbar.

  ENDMETHOD.


  METHOD alv_usercommand.
    DATA:lt_row_id TYPE lvc_t_roid,
         lt_row    TYPE lvc_t_row,
         ls_rows   TYPE LINE OF lvc_t_row,
         lv_index  TYPE sy-tabix,
         ls_itab   LIKE LINE OF itab_alv,
         answer    TYPE char1.

    CASE e_ucomm.
      WHEN 'KAYDET'.
        modify_alv( ).
        MESSAGE s004 DISPLAY LIKE 'I'.
        get_data( ).
        calendar_colored( ).
        pie_chart( ).


      WHEN 'SIL'.
        FREE: lt_row, lv_index.
        grid_alv->get_selected_rows(
          IMPORTING
            et_index_rows = lt_row ).
        DESCRIBE TABLE lt_row LINES lv_index .

        IF lv_index IS INITIAL.
          MESSAGE s003 DISPLAY LIKE 'E'. RETURN.
        ENDIF.

        IF sy-subrc EQ 0.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              text_question         = 'İşlemi silmek istediğinize emin misiniz?'
              text_button_1         = 'Evet'
              icon_button_1         = 'ICON_CHECKED'
              text_button_2         = 'Hayır'
              icon_button_2         = 'ICON_CANCEL'
              popup_type            = 'ICON_MESSAGE_ERROR'
              display_cancel_button = ' '
            IMPORTING
              answer                = answer.
          CHECK answer EQ 1.
          LOOP AT lt_row INTO ls_rows.

            READ TABLE itab_alv INTO ls_itab INDEX ls_rows-index.
            IF sy-subrc IS INITIAL.
              ls_itab-selkz = abap_true.
              MODIFY itab_alv FROM ls_itab INDEX ls_rows-index.
            ENDIF.

          ENDLOOP.

          IF sy-subrc IS INITIAL.
            DELETE itab_alv WHERE selkz EQ abap_true.
            DELETE FROM zfk_t_plan WHERE personel = ls_itab-personel
                                        AND tarih  = ls_itab-tarih
                                        AND sirketadi = ls_itab-sirketadi.
            get_data( ).
            calendar_colored( ).
            pie_chart( ).

          ENDIF.

          grid_alv->refresh_table_display(
              EXPORTING
                i_soft_refresh = 'X' ).

        ENDIF.

    ENDCASE.


  ENDMETHOD.


  METHOD calendar.

    DATA: go_cont_calendar TYPE REF TO cl_gui_container,
          events           TYPE cntl_simple_events,
          wa_events        TYPE  cntl_simple_event,
          gv_date          TYPE cnca_utc_date,
          date(20)         TYPE c,
          year(4)          TYPE c,
          year_begin       TYPE i,
          year_end         TYPE i.
    gv_date = sy-datum.
    year = gv_date.
    year_begin = year.
    year_end = year.

    CALL METHOD go_split->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = go_cont_calendar.

    CREATE OBJECT go_calendar
      EXPORTING
        parent          = go_cont_calendar    " Container
        view_style      = '1026'  " Display Combination
        selection_style = '15'  " Selection Style
*       lifetime        = '20200724'    " Lifetime
        year_begin      = year_begin    " Initial Year
        year_end        = year_begin.

DATA: it_events TYPE cntl_simple_events.
    it_events = VALUE #( ( eventid = cl_gui_calendar=>m_id_date_selected
                           appl_event = abap_true ) ).
    me->go_calendar->set_registered_events( events = it_events ).
    SET HANDLER date_selected FOR go_calendar.



  ENDMETHOD.


  METHOD calendar_colored.
    DATA: day_info   TYPE TABLE OF cnca_day_info,
          wa_info    LIKE LINE OF day_info,
          wa_planlar LIKE LINE OF gt_planlar,
          wa_musteri LIKE LINE OF gt_musteriler.

    go_calendar->reset_day_info( ).

    CLEAR day_info.


    LOOP AT gt_planlar INTO wa_planlar.
      wa_info-date = wa_planlar-tarih.
      READ TABLE gt_musteriler INTO wa_musteri WITH KEY projeadi = wa_planlar-proje.
      wa_info-color = wa_musteri-takvim_renk.
      wa_info-text = 'Planlı'.
      APPEND wa_info TO day_info.
    ENDLOOP.

    go_calendar->set_day_info(
      EXPORTING
        day_info   = day_info    " Table Contains Color and Tool Tip Information
    ).




  ENDMETHOD.


  METHOD constructor.

    create_containers( ).

    logo( ).

    selected_begin_date = sy-datum.

    calendar( ).

    get_data( ).

    " pie_chart( ).

    calendar_colored( ).

  ENDMETHOD.


  METHOD create_containers.

    DATA: go_alv      TYPE REF TO cl_gui_alv_grid,
          go_cont_alv TYPE REF TO cl_gui_container,
          go_cust     TYPE REF TO cl_gui_custom_container.



    DATA: gt_fieldcat TYPE lvc_t_fcat,
          gs_fieldcat TYPE lvc_s_fcat,
          gs_layout   TYPE lvc_s_layo.


    CHECK go_cust IS NOT BOUND.

    CREATE OBJECT go_cust
      EXPORTING
        container_name = 'CC_ALV'
      EXCEPTIONS
        OTHERS         = 1.

    CHECK sy-subrc IS INITIAL.

    CREATE OBJECT go_split
      EXPORTING
        parent  = go_cust
        rows    = 2
        columns = 2
      EXCEPTIONS
        OTHERS  = 1.






  ENDMETHOD.


  METHOD date_selected.
    DATA: ls_plan      LIKE LINE OF gt_planlar.


    CLEAR itab_alv.

    LOOP AT gt_planlar INTO ls_plan WHERE tarih BETWEEN date_begin AND date_end and kullaniciadi = sy-uname.
      APPEND ls_plan TO itab_alv.

    ENDLOOP.

    selected_begin_date = date_begin.
    selected_end_date = date_end.
*    me->date_selected_for_grafic( date_begin = date_begin ).
    get_data( ).

    add_alv( ).
*  EXPORTING
*    container = lo_cont_alv     " Abstract Container for GUI Controls
*    structure = 'ZFK_S_PLAN' ).
    pie_chart( ).






  ENDMETHOD.


  METHOD get_data.
    DATA: wa_plan     LIKE LINE OF gt_tumplanlar,
          wa_personel LIKE LINE OF gt_tumpersonel.

    SELECT * FROM zfk_t_sirket INTO CORRESPONDING FIELDS OF TABLE gt_tumpersonel.
    SELECT * FROM zfk_t_plan INTO CORRESPONDING FIELDS OF TABLE gt_tumplanlar WHERE tarih EQ selected_begin_date.
    SELECT * FROM zfk_t_plan INTO CORRESPONDING FIELDS OF TABLE itab_alv WHERE tarih EQ selected_begin_date.
    SELECT * FROM zfk_t_takvim INTO CORRESPONDING FIELDS OF TABLE gt_musteriler.
    SELECT * FROM zfk_t_plan INTO CORRESPONDING FIELDS OF TABLE gt_planlar.

*    LOOP AT gt_tumplanlar INTO wa_plan WHERE kullaniciadi = sy-uname.
*      APPEND wa_plan TO gt_planlar.
*      CLEAR wa_plan.
*    ENDLOOP.

    LOOP AT gt_tumpersonel INTO wa_personel WHERE kullaniciadi = sy-uname.
      APPEND wa_personel TO gt_personel.
      CLEAR wa_personel.
    ENDLOOP.

*    SELECT *
*            FROM @gt_planlar AS p
*           INNER JOIN zfk_t_sirket AS s
*             ON  @sy-uname  = s~kullaniciadi
*             AND  p~personel = s~personeladi
*                INTO CORRESPONDING FIELDS OF TABLE @gt_tumplanlar.






  ENDMETHOD.


  METHOD logo.


    DATA: go_cont_logo TYPE REF TO cl_gui_container.

    TYPES pict_line(256) TYPE c.
    TYPES: BEGIN OF ty_grap_tab,
             line(255) TYPE x,
           END OF ty_grap_tab.
    DATA gt_graphic_table TYPE TABLE OF ty_grap_tab.
    DATA gs_graphic_table TYPE ty_grap_tab.
    DATA: l_graphic_conv TYPE i.
    DATA: l_graphic_offs TYPE i.
    DATA: graphic_size TYPE i.
    DATA: l_graphic_xstr TYPE xstring.
    DATA : picture  TYPE REF TO cl_gui_picture,
           url(255) TYPE c.


    IF picture IS NOT BOUND.

      CALL METHOD go_split->get_container
        EXPORTING
          row       = 1
          column    = 2
        RECEIVING
          container = go_cont_logo.
      CREATE OBJECT picture
        EXPORTING
          parent = go_cont_logo.

      cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp(
        EXPORTING
          p_object       = 'GRAPHICS'    " SAPscript Graphics Management: Application object
          p_name         = 'ZFK_LOGO'   " Name
          p_id           = 'BMAP'   " SAPscript Graphics Management: ID
          p_btype        = 'BCOL'    " SAPscript: Type of graphic
        RECEIVING
          p_bmp          = l_graphic_xstr    " Graphic Data
 ).

      graphic_size = xstrlen( l_graphic_xstr ).
      l_graphic_conv = graphic_size.
      l_graphic_offs = 0.

      WHILE l_graphic_conv > 255.
        gs_graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
        APPEND gs_graphic_table TO gt_graphic_table.
        l_graphic_offs = l_graphic_offs + 255.
        l_graphic_conv = l_graphic_conv - 255.
      ENDWHILE.

      gs_graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
      APPEND gs_graphic_table TO gt_graphic_table.

      CALL FUNCTION 'DP_CREATE_URL'
        EXPORTING
          type     = 'IMAGE'
          subtype  = 'X-UNKNOWN'
          size     = graphic_size
          lifetime = 'T'
        TABLES
          data     = gt_graphic_table
        CHANGING
          url      = url.

      picture->load_picture_from_url(
        EXPORTING
          url    = url    " URL
      ).
      picture->set_display_mode(
        EXPORTING
          display_mode = picture->display_mode_normal_center    " Display Mode
      ).
    ENDIF.


  ENDMETHOD.


  METHOD modify_alv.
    DATA: lt_alv     TYPE TABLE OF zfk_t_plan,
          wa_alv     TYPE  zfk_t_plan,
          ls_alv     LIKE LINE OF itab_alv,
          wa_planlar LIKE LINE OF gt_planlar.


    LOOP AT itab_alv INTO ls_alv.

      wa_alv-kullaniciadi =  ls_alv-kullaniciadi.
      wa_alv-sirketadi = ls_alv-sirketadi.
      wa_alv-personel =  ls_alv-personel.
      wa_alv-proje =  ls_alv-proje.
      wa_alv-tarih =  ls_alv-tarih.
      MODIFY zfk_t_plan FROM wa_alv.

    ENDLOOP.

    DELETE FROM zfk_t_plan WHERE tarih BETWEEN selected_begin_date AND selected_end_date AND kullaniciadi EQ sy-uname.
    MODIFY zfk_t_plan FROM TABLE lt_alv.
    DELETE gt_planlar WHERE tarih BETWEEN selected_begin_date AND selected_end_date AND kullaniciadi EQ sy-uname.
    DELETE gt_tumplanlar WHERE tarih BETWEEN selected_begin_date AND selected_end_date AND kullaniciadi EQ sy-uname.
    CLEAR ls_alv.
    CLEAR wa_alv.
    LOOP AT lt_alv INTO ls_alv.
*         WHERE tarih = selected_date.
      wa_alv-kullaniciadi =  ls_alv-kullaniciadi.
      wa_alv-personel =  ls_alv-personel.
      wa_alv-proje =  ls_alv-proje.
      wa_alv-tarih =  ls_alv-tarih.
      APPEND wa_alv TO gt_planlar.
      APPEND wa_alv TO gt_tumplanlar.
    ENDLOOP.

    calendar_colored( ).

*    grid_alv->refresh_table_display(
*      EXPORTING
*         i_soft_refresh = abap_true  ).


  ENDMETHOD.


  METHOD pie_chart.

    DATA: go_cont_chart TYPE REF TO cl_gui_container,
          lv_url        TYPE char255.

    TYPES : BEGIN OF plan_sirket ,
              sirketadi      TYPE zfk_t_plan-sirketadi,
              personelsayisi TYPE i,
            END OF plan_sirket.

    DATA : gt_plan_sirket TYPE TABLE OF plan_sirket,
           gs_plan_sirket TYPE plan_sirket.

    SELECT p~sirketadi ,COUNT(*)
       FROM @gt_tumplanlar AS p
         GROUP BY p~sirketadi
             INTO TABLE @gt_plan_sirket.

    IF grafic_html IS NOT BOUND.
      CALL METHOD go_split->get_container
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = go_cont_chart.

      CREATE OBJECT grafic_html
        EXPORTING
          parent = go_cont_chart.
    ENDIF.

    DATA(it_html) = VALUE html_table( ( |<html>| )
                                   ( |  <meta http-equiv="content-type" content="text/html" charset="UTF-8">| )
                                   ( |  <head>| )
                                   ( |  <title>Test</title>| )
                                   ( |  <script type="text/javascript" src="https://www.gstatic.com/charts/loader.js">| )
                                   ( |  </script>| )
                                   ( |  </head>| )
                                   ( |  <div id="piechart" style="width: 900px; height: 500px;">| )
                                   ( |  </div>| )
                                   ( |  <script>| )
                                   ( |    google.charts.load('current', \{'packages':['corechart']\});| )
                                   ( |     google.charts.setOnLoadCallback(drawChart);| )
                                    ( |   function drawChart()| )
                                   ( |    \{| )
                                   ( |      var data = google.visualization.arrayToDataTable([| )
                                   ( |      ['Banka', 'Tutar'],| ) ).
    LOOP AT gt_plan_sirket INTO gs_plan_sirket.
      APPEND  |     [' { gs_plan_sirket-sirketadi } ',    { gs_plan_sirket-personelsayisi }], | TO it_html .
    ENDLOOP.
    APPEND  |                 ]);|  TO it_html.
    APPEND  |         var options = \{| TO it_html.
    APPEND  |         title: 'Personel Plan Grafiği' \};| TO it_html.
    APPEND  |         var chart = new google.visualization.PieChart(document.getElementById('piechart'));| TO it_html.
    APPEND  |         chart.draw(data, options); \}  | TO it_html.
    APPEND  |        </script> | TO it_html.
    APPEND  |        </html> | TO it_html.


    CALL METHOD grafic_html->load_data
      IMPORTING
        assigned_url = lv_url
      CHANGING
        data_table   = it_html.

    CALL METHOD grafic_html->show_url
      EXPORTING
        url = lv_url.






  ENDMETHOD.


  METHOD usercheck.
    CALL FUNCTION 'ZFK_F_ODEV7'
      EXPORTING
        userid      = sy-uname
      TABLES
        personeltab = gt_personel.

  ENDMETHOD.
ENDCLASS.
