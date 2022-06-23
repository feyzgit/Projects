*&---------------------------------------------------------------------*
*&  Include           ZCO_I_BUNDLE_DGT_CLSI
*&---------------------------------------------------------------------*
CLASS lcl_main IMPLEMENTATION.
  METHOD load_of_program.

  ENDMETHOD.
*
  METHOD initialization.

  ENDMETHOD.
*
  METHOD at_selection_screen_output.
    LOOP AT SCREEN.
      CHECK screen-name EQ 'P_BUKRS'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDLOOP.

    lcl_main=>mod_scr_0100( ).
    lcl_main=>fill_sscrdata( ).
  ENDMETHOD.

  METHOD at_selection_screen.


  ENDMETHOD.

  METHOD start_of_selection.

    go_main->retrieve_data( EXCEPTIONS not_found = 1 ).
    IF sy-subrc NE 0.
      go_main->gv_message = TEXT-m01.
    ENDIF.

  ENDMETHOD.

  METHOD end_of_selection.
    IF go_main->gv_message IS NOT INITIAL.
      MESSAGE go_main->gv_message TYPE 'S' DISPLAY LIKE 'E' .
    ELSE.
      CALL SCREEN 0100.
    ENDIF.

  ENDMETHOD.

  METHOD mod_scr_0100.

  ENDMETHOD.

  METHOD fill_sscrdata.

  ENDMETHOD.

  METHOD create_grid.
    DATA: ls_layo TYPE lvc_s_layo.
    DATA: t_sort  TYPE lvc_t_sort,
          fs_sort TYPE lvc_s_sort.
    IF o_grid IS NOT BOUND.

      CREATE OBJECT me->o_grid
        EXPORTING
          i_lifetime        = cl_gui_alv_grid=>lifetime_dynpro
          i_parent          = cl_gui_custom_container=>screen0
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      me->o_grid->merge_fieldcatalog(
        i_buffer_active         = space
        i_bypassing_buffer      = 'X'
        i_internal_table        = gt_output ).

      me->modify_field_catalog( me->o_grid ).
      DATA(lt_exclude)  = me->exclude( ).

      ls_layo-cwidth_opt = 'X'.
      ls_layo-zebra      = 'X'.
      ls_layo-sel_mode   = 'A'.
      ls_layo-box_fname  = 'SEL'.
*      ls_layo-ctab_fname  = 'COLOR'.

      SET HANDLER me->hotspot_click FOR me->o_grid.
      SET HANDLER me->toolbar  FOR me->o_grid.
      SET HANDLER me->handle_user_command  FOR me->o_grid.
***
      CALL METHOD me->o_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      me->o_grid->set_table_for_first_display(
        EXPORTING
          i_buffer_active               = space
          i_bypassing_buffer            = 'X'
          is_layout                     = ls_layo
          it_toolbar_excluding          = lt_exclude
        CHANGING
          it_outtab                     = me->t_list
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ELSE.

      me->o_grid->refresh_table_display( ).

    ENDIF.
  ENDMETHOD.
  METHOD create_grid_dty.

    DATA: ls_layo TYPE lvc_s_layo.
    DATA: t_sort  TYPE lvc_t_sort,
          fs_sort TYPE lvc_s_sort.

    IF o_grid_dty IS BOUND.
      FREE    me->o_grid_dty.
    ENDIF.

    CREATE OBJECT me->o_grid_dty
      EXPORTING
        i_lifetime        = cl_gui_alv_grid=>lifetime_dynpro
        i_parent          = cl_gui_custom_container=>screen0
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
*
*    me->o_grid_dty->merge_fieldcatalog(
*      i_buffer_active         = space
*      i_bypassing_buffer      = 'X'
*      i_internal_table        = me->t_list_dty ).

*    me->modify_field_catalog( me->o_grid_dty ).



    DATA(lt_exclude)  = me->exclude( ).

    ls_layo-cwidth_opt = 'X'.
    ls_layo-zebra      = 'X'.
    ls_layo-sel_mode   = 'A'.
    ls_layo-box_fname  = 'SEL'.
*      ls_layo-ctab_fname  = 'COLOR'.

    SET HANDLER me->hotspot_click FOR me->o_grid_dty.
    SET HANDLER me->toolbar  FOR me->o_grid_dty.
    SET HANDLER me->handle_user_command  FOR me->o_grid_dty.
***
    CALL METHOD me->o_grid_dty->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    DATA: lo_columns      TYPE REF TO cl_salv_columns_table,
          lo_aggregations TYPE REF TO cl_salv_aggregations,
          lo_salv_table   TYPE REF TO cl_salv_table,
          l_text          TYPE lvc_txt.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            list_display = abap_false
          IMPORTING
            r_salv_table = lo_salv_table
          CHANGING
            t_table      = <outdat> ).
      CATCH cx_salv_msg.

    ENDTRY.

    FREE: me->mt_fieldcat.
    lo_columns  = lo_salv_table->get_columns( ).
    lo_aggregations = lo_salv_table->get_aggregations( ).
    me->mt_fieldcat[] = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns = lo_columns r_aggregations = lo_aggregations ).

    LOOP AT me->mt_fieldcat REFERENCE INTO DATA(lr_fieldcat).
      CLEAR: l_text.
      CASE lr_fieldcat->fieldname.
        WHEN 'STSIND'.
          lr_fieldcat->no_out = abap_true.
        WHEN 'BRUTSTS_HSP'.
          l_text = 'Brüt Satış Hesabı'.
        WHEN 'STSIND_HSP'.
          l_text = 'Satış İndirimi Hesabı'.
        WHEN 'ISKORN_HSP'.
          l_text = 'İskonto İndirimi Hesabı'.
        WHEN 'KTGIND_HSP'.
          l_text = 'Kategori İndirim Hesabı'.
        WHEN 'BNTLM_HSP'.
          l_text = 'Bantlama Hesabı'.
        WHEN 'BNTHDY_HSP'.
          l_text = 'Bant Hediye Hesabı'.
        WHEN 'SOLOSMM_HSP'.
          l_text = 'Solo SMM Hesabı'.
        WHEN 'ANASKU'.
          lr_fieldcat->checkbox = abap_true.
        WHEN 'BNDL_ITM'.
          lr_fieldcat->checkbox = abap_true.
        WHEN 'HBANT'.
          lr_fieldcat->checkbox = abap_true.
        WHEN OTHERS.
          READ TABLE mt_coldat REFERENCE INTO DATA(r_coldat) WITH KEY fname = lr_fieldcat->fieldname.
          IF sy-subrc IS INITIAL.
            l_text = r_coldat->fname.
          ENDIF.
      ENDCASE.
      IF l_text <> space.
        lr_fieldcat->* = VALUE #( BASE lr_fieldcat->* scrtext_l = l_text scrtext_m = l_text scrtext_s = l_text reptext = l_text ).
      ENDIF.

      IF lr_fieldcat->fieldname CP 'ZD*'.
        lr_fieldcat->col_pos = 23.
      ENDIF.
      IF lr_fieldcat->col_pos > 23.
        lr_fieldcat->col_pos = lr_fieldcat->col_pos + 1.
      ENDIF.
    ENDLOOP.

*    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
*      EXPORTING
*        i_callback_program      = sy-repid
*       i_callback_pf_status_set = 'SET_PF_STATUS'
*        i_callback_user_command = 'USER_COMMAND'
**       i_callback_top_of_page  = 'TOP_OF_PAGE'
*        is_layout_lvc           = VALUE lvc_s_layo( col_opt = abap_true cwidth_opt = abap_true zebra = abap_true )
*        it_fieldcat_lvc         = me->mt_fieldcat
**       it_excluding            = mt_excluding
*        i_default               = abap_true
*        i_save                  = abap_true
*      TABLES
*        t_outtab                = <outdat>
*      EXCEPTIONS
*        program_error           = 1
*        OTHERS                  = 2.

    me->o_grid_dty->set_table_for_first_display(
      EXPORTING
        i_buffer_active               = space
        i_bypassing_buffer            = 'X'
        is_layout                     = VALUE lvc_s_layo( col_opt = abap_true cwidth_opt = abap_true zebra = abap_true )
        it_toolbar_excluding          = lt_exclude
      CHANGING
        it_outtab                     = <outdat>
        it_fieldcatalog               = me->mt_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
*    ELSE.
*
*      me->o_grid_dty->refresh_table_display( ).
*
*    ENDIF.

  ENDMETHOD.

  METHOD modify_field_catalog.

    grid->change_column_text( ip_field = 'BRUTSTS_HSP' ip_text  = 'Brüt Satış Hesabı' ).
    grid->change_column_text( ip_field = 'STSIND_HSP' ip_text  = 'Satış İndirimi Hesabı' ).
    grid->change_column_text( ip_field = 'ISKORN_HSP' ip_text  = 'İskonto İndirimi Hesabı' ).
    grid->change_column_text( ip_field = 'KTGIND_HSP' ip_text  = 'Kategori İndirim Hesabı' ).
    grid->change_column_text( ip_field = 'BNTHDY_HSP' ip_text  = 'Bant Hediye Hesabı' ).
    grid->change_column_text( ip_field = 'BNTLM_HSP' ip_text  = 'Bantlama Hesabı' ).
    grid->change_column_text( ip_field = 'SOLOSMM_HSP' ip_text  = 'Solo SMM Hesabı' ).


    DEFINE _change_fcat_attribute.
      grid->change_fcat_attribute(  EXPORTING
                                       ip_field     = &1
                                       ip_attribute = &2
                                       ip_value     = &3
                                    CHANGING
                                       ip_fcat      = grid->fieldcat ).
    END-OF-DEFINITION.

    _change_fcat_attribute:
        'SEL'                'NO_OUT'        'X',
        'ANASKU'             'CHECKBOX'      'X',
        'BNDL_ITM'           'CHECKBOX'      'X',
        'HBANT'              'CHECKBOX'      'X'.

  ENDMETHOD.
  METHOD hotspot_click.
    DATA : lv_tcode     TYPE sy-tcode,
           lt_parameter LIKE gt_param,
           ls_parameter LIKE LINE OF lt_parameter.

    DEFINE add_params.
      CLEAR: ls_parameter.
       ls_parameter-param_id = &1.
       ls_parameter-value    = &2.
       APPEND ls_parameter TO lt_parameter.
    END-OF-DEFINITION.

    CLEAR: lv_tcode.
    READ TABLE me->t_list INTO DATA(ls_data) INDEX es_row_no-row_id.
    CHECK sy-subrc EQ 0.

    CASE e_column_id.
      WHEN 'XXX'.

    ENDCASE.

  ENDMETHOD.

  METHOD last_day_of_months.
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = bdate
      IMPORTING
        last_day_of_month = edate
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.
  ENDMETHOD.

  METHOD exclude.
    rt_exclude = VALUE #( ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
                         ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
                         ( cl_gui_alv_grid=>mc_fc_loc_append_row )
                         ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
                         ( cl_gui_alv_grid=>mc_fc_loc_move_row )
                         ( cl_gui_alv_grid=>mc_fc_loc_copy )
                         ( cl_gui_alv_grid=>mc_fc_loc_cut )
                         ( cl_gui_alv_grid=>mc_fc_loc_paste )
                         ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
                         ( cl_gui_alv_grid=>mc_fc_loc_undo )
                         ( cl_gui_alv_grid=>mc_fc_graph )
                         ( cl_gui_alv_grid=>mc_fc_info )
                         ( cl_gui_alv_grid=>mc_fc_refresh )
                         ( cl_gui_alv_grid=>mc_fc_print )
                         ( cl_gui_alv_grid=>mc_fc_detail ) ) .
  ENDMETHOD.

  METHOD retrieve_data .

    CONSTANTS lc_brut TYPE kschl   VALUE 'ZP20'.
    CONSTANTS lc_mlyt TYPE kschl   VALUE 'VPRS'.
    CONSTANTS lc_urtm TYPE werks_d VALUE 'AQ01'.

    DATA ls_komg  TYPE komg.
    DATA lt_konp  TYPE konp_t.
    DATA lr_datum TYPE RANGE OF sy-datum.
    DATA lv_datab TYPE datab.
    DATA lv_datbi TYPE datbi.

    DATA:BEGIN OF ls_ciro_oran,
           vbeln   TYPE vbrk-vbeln,
           posnr   TYPE vbrp-posnr,
           bmatnr  TYPE vbrp-matnr,
           brutsts TYPE zco_s_bundle_dgt-brutsts,
           solosmm TYPE zco_s_bundle_dgt-solosmm,
         END OF ls_ciro_oran,

         lt_ciro_oran LIKE TABLE OF ls_ciro_oran.

    DATA:lo_refdat TYPE REF TO data,
         lo_outdat TYPE REF TO data,
         _outdat   TYPE REF TO data,
         l_endda   TYPE endda,
         l_index   TYPE sy-tabix.


    DATA:BEGIN OF ls_ciro_oran_fark,
           vbeln   TYPE vbrk-vbeln,
           posnr   TYPE vbrp-posnr,
           bmatnr  TYPE vbrp-matnr,
           brutsts TYPE zco_s_bundle_dgt_itm-brutsts,
           stsind  TYPE zco_s_bundle_dgt_itm-stsind,
           ktgind  TYPE zco_s_bundle_dgt_itm-ktgind,
           bnthdy  TYPE zco_s_bundle_dgt_itm-bnthdy,
           netsts  TYPE zco_s_bundle_dgt_itm-netsts,
           solosmm TYPE zco_s_bundle_dgt_itm-solosmm,
           bntlm   TYPE zco_s_bundle_dgt_itm-bntlm,
           smmtplm TYPE zco_s_bundle_dgt_itm-smmtplm,
           brutkar TYPE zco_s_bundle_dgt_itm-brutkar,
         END OF ls_ciro_oran_fark,

         lt_ciro_oran_fark LIKE TABLE OF ls_ciro_oran_fark. "oranla yapılan dağıtım farkları

    REFRESH lr_datum.

    lv_datab = p_bdatj && p_poper+1(2) && '01'.
    me->last_day_of_months( EXPORTING bdate = lv_datab IMPORTING edate = lv_datbi ).

    lv_datbi = '99991231'.

    APPEND VALUE #( sign = 'I' option = 'BT' low = lv_datab high = lv_datbi ) TO lr_datum.

    REFRESH: me->t_list.

    SELECT @p_bdatj AS gjahr,
           @p_poper AS poper,
           a~bukrs,
           a~vkorg,
           a~vtweg,
           a~vbeln,
           b~posnr,
           a~fkdat,
           a~kunrg AS kunnr,
           c~name1,
           b~matnr AS bmatnr,
           d~maktx AS bmaktx,
           e~idnrk AS matnr,
           f~maktx AS maktx,
           b~werks,
           e~tip,
           e~ktgdgtmorn,
           b~fkimg AS menge,
           b~vrkme AS meins,
           b~netwr,
           a~waerk AS waers,
           a~knumv,
           e~anasku,
           e~hbant,
           g~meins AS tob
      FROM vbrk AS a
      INNER JOIN vbrp AS b ON a~vbeln = b~vbeln
      LEFT  JOIN kna1 AS c ON c~kunnr = a~kunrg
      LEFT  JOIN makt AS d ON d~matnr = b~matnr AND d~spras EQ @sy-langu
      INNER JOIN zco_t_bundle_mlz AS e ON e~matnr = b~matnr
      LEFT  JOIN makt AS f ON f~matnr = e~idnrk AND f~spras EQ @sy-langu
      LEFT  JOIN mara AS g ON g~matnr = e~idnrk
      INTO TABLE @DATA(lt_data)
      WHERE a~fkdat IN @lr_datum
        AND a~kunrg IN @s_kunrg
        AND a~vbeln IN @s_vbeln
        AND b~matnr IN @s_matnr.

    IF sy-subrc NE 0.
      RAISE not_found.
      EXIT.
    ENDIF.

    SELECT a~matnr,
           a~bwkey,
           b~bdatj,
           b~poper,
           b~stprs AS pvprs,
           b~peinh,
           c~meins
      FROM ckmlhd AS a
      INNER JOIN ckmlcr AS b ON a~kalnr = b~kalnr
      INNER JOIN mara AS c ON c~matnr = a~matnr
      INTO TABLE @DATA(lt_maliyet)
      FOR ALL ENTRIES IN @lt_data
      WHERE a~matnr EQ @lt_data-matnr
        AND a~bwkey EQ @lc_urtm
*        and bdatj eq @p_bdatj
*        and poper eq @p_poper
        AND ( ( bdatj EQ @p_bdatj AND poper LE @p_poper ) OR bdatj LT @p_bdatj )
        AND untper EQ @space
        AND curtp  EQ '10'.

    SORT lt_maliyet BY matnr bwkey bdatj DESCENDING poper DESCENDING .

    DELETE ADJACENT DUPLICATES FROM lt_maliyet COMPARING matnr bwkey.

    SELECT a~matnr,
           a~bwkey,
           a~lfgja,
           a~lfmon,
           a~verpr,
           a~peinh,
           b~meins
      FROM mbewh AS a
      INNER JOIN mara AS b ON a~matnr = b~matnr
      INTO TABLE @DATA(lt_mbewh)
      FOR ALL ENTRIES IN @lt_data
      WHERE a~matnr EQ @lt_data-matnr
        AND a~bwkey EQ @lt_data-werks
         AND ( ( a~lfgja EQ @p_bdatj AND a~lfmon LE @p_poper ) OR a~lfgja LT @p_bdatj ) .

    SORT lt_mbewh BY matnr bwkey lfgja DESCENDING lfmon DESCENDING .

    DELETE ADJACENT DUPLICATES FROM lt_mbewh COMPARING matnr bwkey.

    SELECT a~matnr,
           a~idnrk,
           a~menge
      FROM zco_t_bundle_mlz AS a
      INTO TABLE @DATA(lt_anamlzm).

    SELECT a~knumv,
           a~kposn,
           a~kschl,
           a~kbetr,
           a~kwert,
           a~kpein,
           b~hkont,
*           b~durum,
           b~hspgrp
*           b~mktdgtm
      FROM konv AS a
      INNER JOIN zco_t_bundle_hsp AS b ON a~kschl = b~kschl
      INTO TABLE @DATA(lt_kosul)
      FOR ALL ENTRIES IN @lt_data
      WHERE knumv EQ @lt_data-knumv
        AND kposn EQ @lt_data-posnr.

    SORT lt_kosul BY knumv kposn kschl.

    SELECT hkont,
           hspgrp
      FROM zco_t_bundle_hsp
      INTO TABLE @DATA(lt_hspgrp).

    SORT lt_hspgrp BY hspgrp.

    REFRESH:me->t_list,me->t_list_dty,lt_ciro_oran.

    LOOP AT lt_data INTO DATA(ls_data).

      APPEND INITIAL LINE TO me->t_list REFERENCE INTO DATA(lo_list).

      MOVE-CORRESPONDING ls_data TO lo_list->*.

      IF ls_data-hbant = abap_true.

****  Alt malzeme ise maliyeti hesapla.
        READ TABLE lt_maliyet INTO DATA(ls_maliyet) WITH KEY matnr = ls_data-matnr BINARY SEARCH.

        IF ls_maliyet-peinh IS INITIAL.
          ls_maliyet-peinh = 1.
        ENDIF.

        lo_list->maliyet = lo_list->menge * ( ls_maliyet-pvprs / ls_maliyet-peinh ) .

      ENDIF.

      CHECK ls_data-hbant = space.

      READ TABLE lt_anamlzm INTO DATA(ls_anamlzm)  WITH KEY matnr = lo_list->bmatnr
                                                            idnrk = lo_list->matnr.

**    Ana malzememize ait bilgileri alırız.
      READ TABLE lt_anamlzm INTO DATA(ls_anamlzm_ust) WITH KEY matnr = lo_list->bmatnr
                                                               idnrk = lo_list->bmatnr.

      IF ls_anamlzm_ust-menge IS NOT INITIAL AND ls_anamlzm_ust-menge NE 0.

        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = lo_list->matnr
            i_in_me              = ls_data-tob
            i_out_me             = 'ST'
            i_menge              = ls_anamlzm-menge
          IMPORTING
            e_menge              = ls_anamlzm-menge
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.

        lo_list->menge = lo_list->menge * ( ls_anamlzm-menge / ls_anamlzm_ust-menge ) .
      ENDIF.

      CLEAR ls_anamlzm_ust.


      IF lo_list->bmatnr = lo_list->matnr. "Ana malzeme ise brütü faturadan oku

        READ TABLE lt_kosul INTO DATA(ls_kosul) WITH KEY knumv = ls_data-knumv
                                                         kposn = ls_data-posnr
                                                         kschl = lc_brut
                                                         BINARY SEARCH.

        IF ls_kosul-kpein IS INITIAL.
          ls_kosul-kpein = 1.
        ENDIF.


        lo_list->brutsts = lo_list->menge * ( ls_kosul-kbetr / ls_kosul-kpein ).

        lo_list->brutsts =  lo_list->brutsts.
        lo_list->menge   =  lo_list->menge.

        CLEAR ls_kosul.
        "Ana malzeme ise stış indirimini fatarudaki iskontolardan oku
        LOOP AT lt_kosul INTO ls_kosul WHERE knumv = ls_data-knumv
                                         AND kposn = ls_data-posnr
                                         AND hspgrp = 'SIND'.

          IF ls_kosul-kpein IS INITIAL.
            ls_kosul-kpein = 1.
          ENDIF.

          lo_list->stsind = lo_list->stsind + ls_kosul-kwert.

        ENDLOOP.


        lo_list->stsind = 0 - lo_list->stsind.

        CLEAR ls_kosul.
        "Ana malzeme ise maliyeti faturadan oku
        READ TABLE lt_kosul INTO ls_kosul WITH KEY knumv = ls_data-knumv
                                                   kposn = ls_data-posnr
                                                   kschl = lc_mlyt
                                                   BINARY SEARCH.

        IF ls_kosul-kpein IS INITIAL.
          ls_kosul-kpein = 1.
        ENDIF.


        lo_list->solosmm = ls_kosul-kwert / ls_kosul-kpein.

        lo_list->solosmm = 0 - lo_list->solosmm.

      ELSE. "Alt malzeme ise brütü hesapla

        CLEAR ls_komg.
        ls_komg-vtweg = ls_data-vtweg.
        ls_komg-matnr = ls_data-matnr.
        ls_komg-kunnr = ls_data-kunnr.

        REFRESH lt_konp.
        CALL FUNCTION 'ZCO_F_READ_COND'
          EXPORTING
            i_kschl = 'ZP20'
            i_komg  = ls_komg
            p_datum = ls_data-fkdat
          IMPORTING
            e_konp  = lt_konp.

        READ TABLE lt_konp INTO DATA(ls_konp) INDEX 1.

        IF ls_konp-kpein IS INITIAL.
          ls_konp-kpein = 1.
        ENDIF.

        lo_list->brutsts = -1 * (  lo_list->menge * ls_konp-kbetr / ls_konp-kpein ) .

        lo_list->menge = - lo_list->menge.

        CLEAR ls_konp.

****  Alt malzeme ise maliyeti hesapla.
        READ TABLE lt_maliyet INTO ls_maliyet WITH KEY matnr = ls_data-matnr BINARY SEARCH.
        IF sy-subrc EQ 0 AND ls_maliyet-pvprs NE 0.

          IF ls_maliyet-peinh IS INITIAL.
            ls_maliyet-peinh = 1.
          ENDIF.

          lo_list->maliyet = lo_list->menge * ( ls_maliyet-pvprs / ls_maliyet-peinh ) .
          lo_list->solosmm = lo_list->menge * ( ls_maliyet-pvprs / ls_maliyet-peinh ) .

        ELSE.

          READ TABLE lt_mbewh INTO DATA(ls_mbewh) WITH KEY matnr = ls_data-matnr BINARY SEARCH.
          IF sy-subrc EQ 0.

            IF ls_mbewh-peinh IS INITIAL.
              ls_mbewh-peinh = 1.
            ENDIF.

            lo_list->maliyet = lo_list->menge * ( ls_mbewh-verpr / ls_mbewh-peinh ) .
            lo_list->solosmm = lo_list->menge * ( ls_mbewh-verpr / ls_mbewh-peinh ) .

          ENDIF.

        ENDIF.


        lo_list->solosmm = - lo_list->solosmm.

***Alt Malzemelerde brüt ciro ve SMM dağıtımı için toplam brüt ve smm alınır.
        CLEAR ls_ciro_oran.
        ls_ciro_oran-vbeln   = lo_list->vbeln.
        ls_ciro_oran-posnr   = lo_list->posnr.
        ls_ciro_oran-bmatnr  = lo_list->bmatnr.
        ls_ciro_oran-brutsts = lo_list->brutsts.
        ls_ciro_oran-solosmm = lo_list->solosmm.

        COLLECT ls_ciro_oran INTO lt_ciro_oran.


      ENDIF.


    ENDLOOP.

    SORT lt_ciro_oran BY vbeln posnr.

    FREE: mt_coldat.
    LOOP AT lt_kosul REFERENCE INTO DATA(r_orderdat) WHERE hspgrp = 'SIND'
                                                       AND kwert IS NOT INITIAL.
      CREATE DATA mr_coldat LIKE LINE OF mt_coldat.
      mr_coldat->* = VALUE #( fname = r_orderdat->kschl hkont = r_orderdat->hkont ).
      COLLECT mr_coldat->* INTO mt_coldat.
    ENDLOOP.

    SORT mt_coldat ASCENDING BY fname.


    generate_dynamic_alvdat(
      EXPORTING
        im_coldat      = mt_coldat
      IMPORTING
        ev_refdat      = lo_refdat
      EXCEPTIONS
        not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING not_found.
    ENDIF.
    ASSIGN lo_refdat->* TO <outdat>.
    CREATE DATA lo_outdat LIKE LINE OF <outdat>.
    ASSIGN lo_outdat->* TO FIELD-SYMBOL(<fs_outdat>).

****İtem verisi hesaplanamsı
    REFRESH lt_ciro_oran_fark.
    LOOP AT me->t_list REFERENCE INTO lo_list.

      APPEND INITIAL LINE TO me->t_list_dty REFERENCE INTO DATA(lo_list_dty).

      MOVE-CORRESPONDING lo_list->* TO lo_list_dty->* .

      IF lo_list_dty->bmatnr = lo_list_dty->matnr .
        lo_list_dty->bndl_itm = abap_true.
      ENDIF.


*      CHECK lo_list->hbant EQ space.
      IF lo_list->hbant EQ space.

        READ TABLE me->t_list INTO DATA(ls_list_hdy) WITH KEY vbeln  = lo_list->vbeln
                                                              posnr  = lo_list->posnr
                                                              hbant  = abap_true.

        READ TABLE me->t_list INTO DATA(ls_list) WITH KEY vbeln  = lo_list->vbeln
                                                          posnr  = lo_list->posnr
                                                          bmatnr = lo_list->bmatnr
                                                          matnr  = lo_list->bmatnr.

        IF lo_list_dty->bndl_itm NE abap_true."Alt Malzeme ise

          CLEAR ls_ciro_oran.
          READ TABLE lt_ciro_oran INTO ls_ciro_oran WITH KEY vbeln = lo_list->vbeln
                                                             posnr = lo_list->posnr
                                                             BINARY SEARCH.
          IF ls_ciro_oran-brutsts IS INITIAL.
            ls_ciro_oran-brutsts = 1.
          ENDIF.

          IF ls_ciro_oran-solosmm IS INITIAL.
            ls_ciro_oran-solosmm = 1.
          ENDIF.





          lo_list_dty->stsind = -1 * ls_list-stsind * ( lo_list_dty->brutsts / ls_ciro_oran-brutsts ) .

          lo_list_dty->ktgind = -1 * ( ls_ciro_oran-brutsts + ls_list-brutsts ) * lo_list_dty->ktgdgtmorn / 100.

          lo_list_dty->bntlm = -1 * ( ls_list-solosmm + ls_ciro_oran-solosmm  ) *
                                    ( lo_list_dty->solosmm / ls_ciro_oran-solosmm ).

          lo_list_dty->bnthdy = -1 * ls_list_hdy-maliyet * ( lo_list_dty->brutsts / ls_ciro_oran-brutsts ) .

        ENDIF.

        lo_list_dty->netsts = lo_list_dty->brutsts - lo_list_dty->stsind + lo_list_dty->ktgind.

        lo_list_dty->smmtplm = lo_list_dty->solosmm + lo_list_dty->bntlm.

        lo_list_dty->brutkar = lo_list_dty->netsts + lo_list_dty->smmtplm.

        lo_list_dty->solosmm = lo_list_dty->solosmm - lo_list_dty->bnthdy.

        IF lo_list_dty->stsind NE 0.
          lo_list_dty->iskorn = lo_list_dty->brutsts / lo_list_dty->stsind.
        ENDIF.

        READ TABLE lt_hspgrp INTO DATA(ls_hspgrp) WITH KEY hspgrp = 'BRUTS'.
        lo_list_dty->brutsts_hsp = ls_hspgrp-hkont.

        CLEAR ls_hspgrp.
        READ TABLE lt_hspgrp INTO ls_hspgrp WITH KEY hspgrp = 'SIND'.
        lo_list_dty->stsind_hsp = ls_hspgrp-hkont.

        CLEAR ls_hspgrp.
        READ TABLE lt_hspgrp INTO ls_hspgrp WITH KEY hspgrp = 'BANT'.
        lo_list_dty->bntlm_hsp = ls_hspgrp-hkont.

        CLEAR ls_hspgrp.
        READ TABLE lt_hspgrp INTO ls_hspgrp WITH KEY hspgrp = 'BHDY'.
        lo_list_dty->bnthdy_hsp = ls_hspgrp-hkont.

        CLEAR ls_hspgrp.
        READ TABLE lt_hspgrp INTO ls_hspgrp WITH KEY hspgrp = 'KIND'.
        lo_list_dty->ktgind_hsp = ls_hspgrp-hkont.

        CLEAR ls_hspgrp.
        READ TABLE lt_hspgrp INTO ls_hspgrp WITH KEY hspgrp = 'SSMM'.
        lo_list_dty->solosmm_hsp = ls_hspgrp-hkont.
        CLEAR ls_hspgrp.

        IF lo_list_dty->brutsts IS NOT INITIAL.
          lo_list_dty->iskorn = lo_list_dty->stsind / lo_list_dty->brutsts.
        ENDIF.

****Dağıtımdan gelen farkların hesaplanması.
        CLEAR ls_ciro_oran_fark.
        MOVE-CORRESPONDING lo_list_dty->* TO ls_ciro_oran_fark.
        COLLECT ls_ciro_oran_fark INTO lt_ciro_oran_fark.

      ENDIF.

      CLEAR : ls_kosul,ls_list,ls_list_hdy.
      MOVE-CORRESPONDING lo_list_dty->* TO <fs_outdat>.
      LOOP AT mt_coldat REFERENCE INTO mr_coldat.
        READ TABLE lt_data INTO ls_data WITH KEY vbeln = lo_list_dty->vbeln
                                                 posnr = lo_list_dty->posnr.
        IF sy-subrc EQ 0.
          READ TABLE lt_kosul REFERENCE INTO DATA(kosul) WITH KEY knumv = ls_data-knumv
                                                                  kposn = ls_data-posnr
                                                                  kschl = mr_coldat->fname.
          IF sy-subrc IS INITIAL.
            IF lo_list_dty->bndl_itm EQ abap_true.
              ASSIGN COMPONENT |{ mr_coldat->fname }| OF STRUCTURE <fs_outdat> TO FIELD-SYMBOL(<fs>).
              IF <fs> IS ASSIGNED.
                <fs> = ( <fs> + kosul->kwert ) * -1.
              ENDIF.

            ELSE."Alt Malzeme ise
              ASSIGN COMPONENT |{ mr_coldat->fname }| OF STRUCTURE <fs_outdat> TO <fs>.
              IF <fs> IS ASSIGNED.
                <fs> = kosul->kwert * ( lo_list_dty->brutsts / ls_ciro_oran-brutsts ) .
              ENDIF.
            ENDIF.
            IF lo_list_dty->brutsts IS INITIAL AND lo_list->hbant EQ space AND lo_list->anasku EQ abap_true.
              <fs> = ( <fs> + kosul->kwert ).
            ENDIF.
          ENDIF.
          UNASSIGN <fs>.
        ENDIF.
      ENDLOOP.
      IF <fs_outdat> IS ASSIGNED AND <outdat> IS ASSIGNED.
        APPEND <fs_outdat> TO <outdat>.
        CLEAR <fs_outdat>.
      ENDIF.
    ENDLOOP.

    DATA: _where  TYPE string,
          _vbeln  TYPE vbrk-vbeln,
          _posnr  TYPE vbrp-posnr,
          _bmatnr TYPE mbewh-matnr.

    LOOP AT lt_ciro_oran_fark INTO ls_ciro_oran_fark WHERE smmtplm NE 0 OR brutkar NE 0  OR stsind NE 0 OR ktgind NE 0 .

      _vbeln = ls_ciro_oran_fark-vbeln.
      _posnr = ls_ciro_oran_fark-posnr.
      _bmatnr = ls_ciro_oran_fark-bmatnr.

      _where = |VBELN EQ '{ _vbeln }' AND POSNR EQ '{ _posnr }' AND BMATNR EQ '{ _bmatnr }' AND BNDL_ITM EQ ABAP_FALSE AND ANASKU EQ ABAP_TRUE|.

      LOOP AT <outdat> ASSIGNING <fs_outdat> WHERE (_where).
        ASSIGN COMPONENT | { 'BNTLM' } | OF STRUCTURE <fs_outdat> TO FIELD-SYMBOL(<bntlm>).
        IF <bntlm> IS ASSIGNED.
          IF ls_ciro_oran_fark-smmtplm NE 0.
            <bntlm> = <bntlm> - ls_ciro_oran_fark-smmtplm.
*          <fs> = <fs> - ls_ciro_oran_fark-smmtplm.
          ENDIF.
        ENDIF.

        ASSIGN COMPONENT | { 'SMMTPLM' } | OF STRUCTURE <fs_outdat> TO FIELD-SYMBOL(<smmtplm>).
        IF <smmtplm> IS ASSIGNED.
          IF ls_ciro_oran_fark-smmtplm NE 0.
            <smmtplm> = <smmtplm> - ls_ciro_oran_fark-smmtplm.
*          <fs> = <fs> - ls_ciro_oran_fark-smmtplm.
          ENDIF.
        ENDIF.

        ASSIGN COMPONENT | { 'BRUTKAR' } | OF STRUCTURE <fs_outdat> TO FIELD-SYMBOL(<brutkar>).
        IF <brutkar> IS ASSIGNED.
          IF ls_ciro_oran_fark-smmtplm NE 0.
            <brutkar> = <brutkar> - ls_ciro_oran_fark-brutkar.
*          <fs> = <fs> - ls_ciro_oran_fark-smmtplm.
          ENDIF.
        ENDIF.



      ENDLOOP.
    ENDLOOP.

****Dağıtımdan gelen farkların hesaplanması.
    LOOP AT lt_ciro_oran_fark INTO ls_ciro_oran_fark WHERE smmtplm NE 0 OR brutkar NE 0  OR stsind NE 0 OR ktgind NE 0 .

      READ TABLE me->t_list_dty REFERENCE INTO lo_list_dty WITH KEY vbeln  = ls_ciro_oran_fark-vbeln
                                                                    posnr  = ls_ciro_oran_fark-posnr
                                                                    bmatnr = ls_ciro_oran_fark-bmatnr
                                                                    bndl_itm = abap_false.
      CHECK sy-subrc EQ 0.

      IF ls_ciro_oran_fark-smmtplm NE 0.
        lo_list_dty->bntlm = lo_list_dty->bntlm     - ls_ciro_oran_fark-smmtplm.
        lo_list_dty->smmtplm = lo_list_dty->smmtplm - ls_ciro_oran_fark-smmtplm.
        EXIT.
      ENDIF.

      IF ls_ciro_oran_fark-brutkar NE 0.
        lo_list_dty->brutkar = lo_list_dty->brutkar - ls_ciro_oran_fark-brutkar.
        EXIT.
      ENDIF.

*      IF ls_ciro_oran_fark-stsind NE 0.
*        lo_list_dty->stsind = lo_list_dty->stsind - ls_ciro_oran_fark-stsind.
*      ENDIF.

*      if ls_ciro_oran_fark-ktgind ne 0.
*        lo_list_dty->ktgind = lo_list_dty->ktgind - ls_ciro_oran_fark-ktgind.
*      endif.

    ENDLOOP.

    SORT me->t_list BY vbeln posnr bmatnr .
    SORT me->t_list_dty BY vbeln posnr bmatnr bndl_itm DESCENDING anasku DESCENDING.

  ENDMETHOD.
  METHOD generate_dynamic_alvdat.

    DATA: lo_struct   TYPE REF TO cl_abap_structdescr,
          lo_element  TYPE REF TO cl_abap_elemdescr,
          lo_new_type TYPE REF TO cl_abap_structdescr,
          lo_new_tab  TYPE REF TO cl_abap_tabledescr,
          lt_comp     TYPE cl_abap_structdescr=>component_table,
          lt_tot_comp TYPE cl_abap_structdescr=>component_table.
    TRY.
        lo_struct ?= cl_abap_typedescr=>describe_by_name( 'ZCO_S_BUNDLE_DGT_ITM' ).
        lt_comp  = lo_struct->get_components( ).
        APPEND LINES OF lt_comp TO lt_tot_comp.
        LOOP AT mt_coldat REFERENCE INTO DATA(r_coldat).
          lo_element ?= cl_abap_elemdescr=>describe_by_name( 'ZCO_DE_STSIND' ).
          APPEND INITIAL LINE TO lt_tot_comp REFERENCE INTO DATA(lo_comp).
          lo_comp->name = r_coldat->fname.
          lo_comp->type = cl_abap_elemdescr=>get_p( p_length = lo_element->length
          p_decimals = lo_element->decimals ).
        ENDLOOP.
        lo_new_type = cl_abap_structdescr=>create( lt_tot_comp ).
        lo_new_tab = cl_abap_tabledescr=>create( p_line_type  = lo_new_type p_table_kind = cl_abap_tabledescr=>tablekind_std p_unique = abap_false ).
        CREATE DATA ev_refdat TYPE HANDLE lo_new_tab.
      CATCH cx_root.
        MESSAGE e002(zco_r0003) RAISING not_found.
    ENDTRY.

  ENDMETHOD.
  METHOD toolbar.

    DATA ls_toolbar TYPE stb_button.

    IF sy-dynnr = '0100'.

      ls_toolbar-function = '&DGT'.
      ls_toolbar-icon     = icon_te_costs_assign.
      ls_toolbar-text     = 'Dağıt'.

      APPEND ls_toolbar TO e_object->mt_toolbar.

    ELSEIF sy-dynnr = '0200'.

      ls_toolbar-function = '&KAYDET'.
      ls_toolbar-icon     = icon_system_save.
      ls_toolbar-text     = 'Kaydet'.

      APPEND ls_toolbar TO e_object->mt_toolbar.

    ENDIF.

  ENDMETHOD.
  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN '&DGT' .
        CALL SCREEN 0200.
*        go_main->create_grid_dty( ).
      WHEN '&KAYDET' .
        save( ).
    ENDCASE.

  ENDMETHOD.
  METHOD save.

    TRY.
        me->post( ).
      CATCH lcx_message INTO DATA(_oref).
        MESSAGE _oref->get_longtext( ) TYPE 'I' DISPLAY LIKE _oref->msgty.
    ENDTRY.


  ENDMETHOD.
  METHOD post.

    DATA lv_bklas_hesap TYPE ska1-saknr.
    DATA lv_dmbtr       TYPE dmbtr.
    DATA lt_index       TYPE lvc_t_row.
    DATA ls_index       TYPE lvc_s_row.
    DATA ls_dty         TYPE zco_s_bundle_dgt_itm.

    DATA: _where  TYPE string,
          _vbeln  TYPE vbrk-vbeln,
          _posnr  TYPE vbrp-posnr,
          _bmatnr TYPE mbewh-matnr.

    DATA:BEGIN OF ls_collect,
           vbeln  TYPE zco_s_bundle_dgt_itm-vbeln,
           posnr  TYPE zco_s_bundle_dgt_itm-posnr,
           bmatnr TYPE zco_s_bundle_dgt_itm-bmatnr,
         END OF ls_collect,

         lt_collect LIKE TABLE OF ls_collect.

    REFRESH gt_messtab.

    o_grid_dty->check_changed_data( ).

    o_grid_dty->get_selected_rows( IMPORTING et_index_rows  = lt_index ).


    IF lt_index IS NOT INITIAL.

      LOOP AT lt_index INTO ls_index.

*        READ TABLE me->t_list_dty INTO DATA(ls_dty) INDEX ls_index-index.
        READ TABLE <outdat> ASSIGNING FIELD-SYMBOL(<fs_outdat>) INDEX ls_index-index.
        CHECK sy-subrc EQ 0.

        CLEAR ls_collect.
*        ls_collect-vbeln  = ls_dty-vbeln.
*        ls_collect-posnr  = ls_dty-posnr.
*        ls_collect-bmatnr = ls_dty-bmatnr.
*        COLLECT ls_collect INTO lt_collect.
        MOVE-CORRESPONDING <fs_outdat> TO ls_collect.
        COLLECT ls_collect INTO lt_collect.

      ENDLOOP.

    ELSE.
      RAISE EXCEPTION TYPE lcx_message
       MESSAGE e001(00) WITH 'Lütfen Satır Seçiniz!'.
    ENDIF.

    LOOP AT lt_collect INTO ls_collect.

      CLEAR  :gs_documentheader,gt_accountgl     ,gt_currencyamount,lv_itemno,gt_criteria.
      REFRESH:gt_accountgl     ,gt_currencyamount,gt_criteria.

      fill_header( EXPORTING iv_vbeln = ls_collect-vbeln iv_posnr = ls_collect-posnr ).

      _vbeln = ls_collect-vbeln.
      _bmatnr = ls_collect-bmatnr.

      _where = |VBELN EQ '{ _vbeln }' AND BMATNR EQ '{ _bmatnr }' AND HBANT EQ SPACE|.

      LOOP AT <outdat> ASSIGNING <fs_outdat> WHERE (_where).

*      LOOP AT me->t_list_dty INTO ls_dty WHERE vbeln  = ls_collect-vbeln
*                                           AND bmatnr = ls_collect-bmatnr
*                                           AND hbant  = space.
        CLEAR ls_dty.
        MOVE-CORRESPONDING <fs_outdat> TO ls_dty.

        SELECT SINGLE paobjnr FROM vbrp
          INTO @DATA(lv_paobjnr)
          WHERE vbeln EQ @ls_dty-vbeln
            AND posnr EQ @ls_dty-posnr.

        IF ls_dty-brutsts_hsp IS NOT INITIAL.

          ADD 1 TO lv_itemno.
          lv_dmbtr = ls_dty-brutsts.

          fill_accountgl( EXPORTING iv_itemno = lv_itemno iv_hkont = ls_dty-brutsts_hsp iv_matnr = ls_dty-matnr iv_werks = ls_dty-werks ).
          fill_curramount( EXPORTING iv_itemno = lv_itemno iv_dmbtr = lv_dmbtr ).
          fill_criteria( EXPORTING iv_itemno = lv_itemno iv_paobjnr = lv_paobjnr iv_matnr = ls_dty-matnr ).

        ENDIF.

*        IF ls_dty-stsind_hsp IS NOT INITIAL.
*
*          lv_dmbtr = ls_dty-stsind.
*
*          ADD 1 TO lv_itemno.
*          fill_accountgl( EXPORTING iv_itemno = lv_itemno  iv_hkont  = ls_dty-stsind_hsp iv_matnr = ls_dty-matnr ).
*          fill_curramount( EXPORTING iv_itemno = lv_itemno iv_dmbtr = lv_dmbtr ).
*          fill_criteria( EXPORTING iv_itemno = lv_itemno   iv_paobjnr = lv_paobjnr iv_matnr = ls_dty-matnr ).
*
*        ENDIF.
        LOOP AT mt_coldat INTO DATA(ls_indirim) .

          ASSIGN COMPONENT ls_indirim-fname OF STRUCTURE <fs_outdat> TO FIELD-SYMBOL(<fs>).
          IF <fs> IS ASSIGNED.

            IF <fs> NE 0 AND ls_indirim-hkont IS NOT INITIAL.

              lv_dmbtr = <fs>.

              ADD 1 TO lv_itemno.
              fill_accountgl( EXPORTING iv_itemno = lv_itemno  iv_hkont  = ls_indirim-hkont iv_matnr = ls_dty-matnr ).
              fill_curramount( EXPORTING iv_itemno = lv_itemno iv_dmbtr = lv_dmbtr ).
              fill_criteria( EXPORTING iv_itemno = lv_itemno   iv_paobjnr = lv_paobjnr iv_matnr = ls_dty-matnr ).

            ENDIF.

          ENDIF.

        ENDLOOP.

        IF ls_dty-ktgind_hsp IS NOT INITIAL.

          lv_dmbtr = ls_dty-ktgind.

          ADD 1 TO lv_itemno.
          fill_accountgl( EXPORTING iv_itemno = lv_itemno iv_hkont =  ls_dty-ktgind_hsp iv_matnr = ls_dty-matnr ).
          fill_curramount( EXPORTING iv_itemno = lv_itemno iv_dmbtr = lv_dmbtr ).
          fill_criteria( EXPORTING iv_itemno = lv_itemno iv_paobjnr = lv_paobjnr iv_matnr = ls_dty-matnr ).

        ENDIF.

*        IF ls_dty-bntlm_hsp IS NOT INITIAL.
*
*          lv_dmbtr = ls_dty-bntlm.
*
*          ADD 1 TO lv_itemno.
*          fill_accountgl( EXPORTING iv_itemno = lv_itemno iv_hkont = ls_dty-bntlm_hsp iv_matnr = ls_dty-matnr ).
*          fill_curramount( EXPORTING iv_itemno = lv_itemno iv_dmbtr = lv_dmbtr ).
*          fill_criteria( EXPORTING iv_itemno = lv_itemno iv_paobjnr = lv_paobjnr iv_matnr = ls_dty-matnr ).
*
*        ENDIF.

        IF ls_dty-bnthdy_hsp IS NOT INITIAL.

          lv_dmbtr = ls_dty-bnthdy.

          ADD 1 TO lv_itemno.
          fill_accountgl( EXPORTING iv_itemno = lv_itemno iv_hkont = ls_dty-bnthdy_hsp iv_matnr = ls_dty-matnr ).
          fill_curramount( EXPORTING iv_itemno = lv_itemno iv_dmbtr = lv_dmbtr ).
          fill_criteria( EXPORTING iv_itemno = lv_itemno iv_paobjnr = lv_paobjnr iv_matnr = ls_dty-matnr ).

        ENDIF.

        IF ls_dty-solosmm_hsp IS NOT INITIAL.

*          lv_dmbtr = ls_dty-solosmm.
          lv_dmbtr = - ls_dty-smmtplm.

          ADD 1 TO lv_itemno.
          fill_accountgl( EXPORTING iv_itemno = lv_itemno iv_hkont = ls_dty-solosmm_hsp iv_matnr = ls_dty-matnr ).
          fill_curramount( EXPORTING iv_itemno = lv_itemno iv_dmbtr = lv_dmbtr ).
          fill_criteria( EXPORTING iv_itemno = lv_itemno iv_paobjnr = lv_paobjnr iv_matnr = ls_dty-matnr ).

        ENDIF.

      ENDLOOP.


      DATA(lv_belnr) = f_02_bapi( ).

      LOOP AT me->t_list_dty INTO ls_dty WHERE bmatnr = ls_collect-bmatnr.

        ls_dty-belge_no = lv_belnr.

        MODIFY me->t_list_dty FROM ls_dty INDEX ls_index TRANSPORTING belge_no.

      ENDLOOP.

      save_log( ls_dty ) .

    ENDLOOP.


    o_grid_dty->refresh_table_display( ).

    show_message( ).

  ENDMETHOD.                    "fill_item
  METHOD fill_header.

    DATA lv_datum TYPE datum.

    lv_datum = p_bdatj && p_poper+1(2) && '01'.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = lv_datum
      IMPORTING
        last_day_of_month = lv_datum
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.


    gs_documentheader-username   = sy-uname.
    gs_documentheader-header_txt = 'Bundle Dağıtım'.
    gs_documentheader-ref_doc_no = iv_vbeln && iv_posnr.
    gs_documentheader-comp_code  = p_bukrs.
    gs_documentheader-fisc_year  = lv_datum(4).
    gs_documentheader-doc_date   = lv_datum.
    gs_documentheader-pstng_date = lv_datum.
    gs_documentheader-fis_period = lv_datum+4(2).
    gs_documentheader-doc_type   = 'SA'.

  ENDMETHOD.                    "fill_header
  METHOD fill_accountgl.

    CLEAR gs_accountgl.
    gs_accountgl-itemno_acc  = iv_itemno.
    gs_accountgl-gl_account  = iv_hkont.
    gs_accountgl-costcenter  = iv_kostl.
    gs_accountgl-material    = iv_matnr.
    gs_accountgl-plant       = iv_werks.
    APPEND gs_accountgl TO gt_accountgl.

  ENDMETHOD.                    "fill_accountgl
  METHOD fill_curramount.

    CLEAR gs_currencyamount.
    gs_currencyamount-itemno_acc = iv_itemno.
    gs_currencyamount-curr_type  = '00'.
    gs_currencyamount-currency   = 'TRY'.
    gs_currencyamount-amt_doccur = iv_dmbtr.

    APPEND gs_currencyamount TO gt_currencyamount.

  ENDMETHOD.                    "fill_CURRAMOUNT
  METHOD fill_criteria.

    DATA lt_chars TYPE keak_yt_ipv_field.
    DATA ls_chars TYPE keak_ys_ipv_field.

    CALL FUNCTION 'RKE_GET_CHARS_FOR_PAOBJNR'
      EXPORTING
        i_bukrs        = p_bukrs
        i_paobjnr      = iv_paobjnr
      TABLES
        t_ce4_chars    = lt_chars
      EXCEPTIONS
        no_erkrs_found = 1
        paobjnr_wrong  = 2
        OTHERS         = 3.


*****    bundle olmayan malzemelerde ilgili malzeme gelsin.
    LOOP AT lt_chars INTO ls_chars.

      gs_criteria-itemno_acc = iv_itemno.
      gs_criteria-fieldname  = ls_chars-fieldname.
      gs_criteria-character  = ls_chars-fieldval.

      IF gs_criteria-fieldname = 'ARTNR'.
        gs_criteria-character = iv_matnr.
      ENDIF.

      APPEND gs_criteria TO gt_criteria.

    ENDLOOP.



  ENDMETHOD.                    "fill_criteria
  METHOD fill_ters_kyt.

    CALL FUNCTION 'OIL_GET_NEXT_MONTH'
      EXPORTING
        i_date = gs_documentheader-doc_date
      IMPORTING
        e_date = gs_documentheader-doc_date.

    gs_documentheader-doc_date+6(2) = '01'.

    gs_documentheader-doc_date   = gs_documentheader-doc_date.
    gs_documentheader-pstng_date = gs_documentheader-doc_date.
    gs_documentheader-fisc_year  = gs_documentheader-doc_date(4).
    gs_documentheader-fis_period = gs_documentheader-doc_date+4(2).

    LOOP AT gt_currencyamount INTO gs_currencyamount.

      gs_currencyamount-amt_doccur = 0 - gs_currencyamount-amt_doccur.

      MODIFY gt_currencyamount FROM gs_currencyamount.

    ENDLOOP.

  ENDMETHOD.                    "fill_ters_kyt
  METHOD f_02_bapi.

    DATA: lt_return TYPE TABLE OF bapiret2.
    DATA: ls_return TYPE bapiret2.

    DATA: lv_type TYPE bapiache09-obj_type,
          lv_key  TYPE bapiache09-obj_key,
          lv_sys  TYPE bapiache09-obj_sys.

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader = gs_documentheader
      IMPORTING
        obj_type       = lv_type
        obj_key        = lv_key
        obj_sys        = lv_sys
      TABLES
        accountgl      = gt_accountgl
        currencyamount = gt_currencyamount
        criteria       = gt_criteria
        return         = lt_return.

    LOOP AT lt_return INTO ls_return WHERE type CA 'EAX'.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      rv_belnr = lv_key(10).

    ENDIF.

    LOOP AT lt_return INTO ls_return.

      gs_messtab-msgty = ls_return-type.
      gs_messtab-msgno = ls_return-number.
      gs_messtab-msgid = ls_return-id.
      gs_messtab-msgv1 = ls_return-message_v1.
      gs_messtab-msgv2 = ls_return-message_v2.
      gs_messtab-msgv3 = ls_return-message_v3.
      gs_messtab-msgv4 = ls_return-message_v4.

      APPEND gs_messtab TO gt_messtab.

    ENDLOOP.

  ENDMETHOD.                    "f_02_bapi
  METHOD save_log.

  ENDMETHOD.
  METHOD show_message.

    CHECK gt_messtab[] IS NOT INITIAL.

    CALL FUNCTION 'RHVM_SHOW_MESSAGE'
      EXPORTING
        mess_header = 'Mesajlar'
      TABLES
        tem_message = gt_messtab
      EXCEPTIONS
        canceled    = 1
        OTHERS      = 2.


  ENDMETHOD.                    "show_message

ENDCLASS.
*
CLASS lcl_gui_alv_grid IMPLEMENTATION.

  METHOD set_table_for_first_display.

    DATA: lt_fieldcatalog TYPE lvc_t_fcat,
          ls_variant      TYPE disvariant.

    IF is_variant IS SUPPLIED.
      ls_variant = is_variant.
    ELSE.
      ls_variant-report = sy-repid.
    ENDIF.

    IF it_fieldcatalog IS SUPPLIED.
      lt_fieldcatalog = it_fieldcatalog.
    ELSE.
      lt_fieldcatalog = me->fieldcat.
    ENDIF.

    i_save              = 'A'.
    is_layout-sel_mode  = 'A'.
    is_layout-box_fname = 'CHK'.

    CALL METHOD super->set_table_for_first_display
      EXPORTING
        i_buffer_active               = i_buffer_active
        i_bypassing_buffer            = i_bypassing_buffer
        i_consistency_check           = i_consistency_check "?
        i_structure_name              = i_structure_name
        is_variant                    = ls_variant
        i_save                        = i_save
        i_default                     = i_default
        is_layout                     = is_layout
        is_print                      = is_print "?
        it_special_groups             = it_special_groups "?
        it_toolbar_excluding          = it_toolbar_excluding
        it_hyperlink                  = it_hyperlink "?
        it_alv_graphics               = it_alv_graphics "?
        it_except_qinfo               = it_except_qinfo "?
        ir_salv_adapter               = ir_salv_adapter "?
      CHANGING
        it_outtab                     = it_outtab
        it_fieldcatalog               = lt_fieldcatalog
        it_sort                       = it_sort
        it_filter                     = it_filter
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD refresh_table_display.

    DATA ls_stable TYPE lvc_s_stbl.

    IF is_stable IS SUPPLIED.
      ls_stable = is_stable.
    ELSE.
      ls_stable-row = 'X'.
      ls_stable-col = 'X'.
    ENDIF.

    CALL METHOD super->refresh_table_display
      EXPORTING
        is_stable      = ls_stable
        i_soft_refresh = i_soft_refresh
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD merge_fieldcatalog.

    DATA: lo_salv TYPE REF TO cl_salv_table,
          lo_tab  TYPE REF TO data.
    FIELD-SYMBOLS <table> TYPE ANY TABLE.

    IF i_internal_table IS NOT SUPPLIED.
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_buffer_active        = i_buffer_active
          i_structure_name       = i_structure_name
          i_client_never_display = i_client_never_display "?
          i_bypassing_buffer     = i_bypassing_buffer
          i_internal_tabname     = i_internal_tabname
        CHANGING
          ct_fieldcat            = me->fieldcat
        EXCEPTIONS
          inconsistent_interface = 1
          program_error          = 2
          OTHERS                 = 3.
      IF sy-subrc <> 0.
*     todo: implement suitable error handling here
      ELSE.
        et_fieldcat = me->fieldcat.
      ENDIF.
    ELSE.
      CREATE DATA lo_tab LIKE i_internal_table.
      ASSIGN lo_tab->* TO <table>.
      TRY.
          CALL METHOD cl_salv_table=>factory
            IMPORTING
              r_salv_table = lo_salv
            CHANGING
              t_table      = <table>.
        CATCH cx_salv_msg.
      ENDTRY.

      me->fieldcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
            r_columns      = lo_salv->get_columns( )
            r_aggregations = lo_salv->get_aggregations( ) ).
    ENDIF.
  ENDMETHOD.

  METHOD change_fcat_attribute.

    FIELD-SYMBOLS: <fcat>      LIKE LINE OF me->fieldcat,
                   <attribute> TYPE simple.

    LOOP AT ip_fcat ASSIGNING <fcat> WHERE fieldname = ip_field.
      ASSIGN COMPONENT ip_attribute OF STRUCTURE <fcat> TO <attribute>.
      IF <attribute> IS ASSIGNED.
        <attribute> = ip_value.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD change_column_text.

    FIELD-SYMBOLS <fcat> LIKE LINE OF me->fieldcat.

    LOOP AT me->fieldcat ASSIGNING <fcat> WHERE fieldname = ip_field.
      <fcat>-coltext   = ip_text.
      <fcat>-scrtext_l = ip_text.
      <fcat>-scrtext_m = ip_text.
      <fcat>-scrtext_s = ip_text.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

DATA: fcode   TYPE lcl_gui_alv_grid=>tt_fcode,
      title   TYPE sy-title,
      ok_code TYPE sy-ucomm,
      save_ok TYPE sy-ucomm.