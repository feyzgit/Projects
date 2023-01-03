*&---------------------------------------------------------------------*
*&  Include           YFIN_LP_P01_CLSDAT
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  Class           LCL_MVC_MODEL           Definition
*&---------------------------------------------------------------------*
CLASS lcl_mvc_model DEFINITION.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_rootdat,
        _docdat TYPE STANDARD TABLE OF yfin_lp_s01 WITH DEFAULT KEY,
        _logdat TYPE STANDARD TABLE OF yfin_lp_t03 WITH DEFAULT KEY,
      END OF ty_rootdat.

    TYPES:
      BEGIN OF ty_appdat,
        bukrs    TYPE yfin_lp_t01-bukrs,
        pkind    TYPE yfin_lp_t01-pkind,
        waers    TYPE yfin_lp_t01-waers,
        base_tab TYPE REF TO data,
        line_tab TYPE REF TO data,
        root_dat TYPE ty_rootdat,
      END OF ty_appdat,
      tt_appdat TYPE STANDARD TABLE OF ty_appdat WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_runparams,
        _update_mod TYPE char1,
      END OF ty_runparams.

    DATA:
      mt_alvdat    TYPE REF TO data,
      mt_keydat    TYPE yfin_lp_cl01=>tt_valdat,
      mt_messtab   TYPE bapiret2_tab,
      mt_appdat    TYPE tt_appdat,
      mv_appdat    TYPE ty_appdat,
      mt_basedat   TYPE STANDARD TABLE OF yfin_lp_t01,
      mt_linedat   TYPE STANDARD TABLE OF yfin_lp_t02,
      mt_sourcedat TYPE STANDARD TABLE OF yfin_lp_t06,
      mt_actualdat TYPE yfin_lp_tt01,
      mt_manueldat TYPE yfin_lp_tt02,
      mv_rejectbtn TYPE char1,
      mv_runparams TYPE ty_runparams.

    METHODS:
      constructor,
      retrieve_dat
        IMPORTING
          !im_bukrs TYPE bukrs
          !im_tunit TYPE char1
          !im_keyda TYPE tt_keydat_rng
          !im_ldays TYPE int1
        EXCEPTIONS
          contains_error.

ENDCLASS.                    "lcl_mvc_model DEFINITION
*&---------------------------------------------------------------------*
*&  Class           LCL_MVC_VIEW           Definition
*&---------------------------------------------------------------------*
CLASS lcl_mvc_view DEFINITION.
  PUBLIC SECTION.

    DATA:
      mo_grid        TYPE REF TO cl_gui_alv_grid,
      mo_dyndoc_id   TYPE REF TO cl_dd_document,
      mo_splitter    TYPE REF TO cl_gui_splitter_container,
      mo_parent_grid TYPE REF TO cl_gui_container,
      mo_parent_top  TYPE REF TO cl_gui_container,
      mo_html_cntrl  TYPE REF TO cl_gui_html_viewer,
      mt_fieldcat    TYPE lvc_t_fcat.

    METHODS:
      constructor
        IMPORTING
          io_model      TYPE REF TO lcl_mvc_model
          io_controller TYPE REF TO lcl_mvc_controller,
      display_alvdat
        EXCEPTIONS
          contains_error.

  PRIVATE SECTION.

    DATA:
      mo_model      TYPE REF TO lcl_mvc_model,
      mo_controller TYPE REF TO lcl_mvc_controller.

    METHODS:
      create_fieldcat
        IMPORTING
          !im_strname TYPE tabname
        EXCEPTIONS
          contains_error,
      update_fieldcat
        EXCEPTIONS
          contains_error,
      set_layout_dat
        RETURNING
          value(rv_layoutdat) TYPE lvc_s_layo,
      set_exclude_dat
        RETURNING
          value(rv_excludedat) TYPE ui_functions,
      attach_handlers
        IMPORTING
          value(im_grid) TYPE REF TO cl_gui_alv_grid
        EXCEPTIONS
          contains_error,
      refresh_alv
        EXCEPTIONS
          contains_error.


ENDCLASS.                    "lcl_mvc_view DEFINITION
*&---------------------------------------------------------------------*
*&  Class           LCL_MVC_CONTROLLER           Definition
*&---------------------------------------------------------------------*
CLASS lcl_mvc_controller DEFINITION.
  PUBLIC SECTION.

    TYPES: ty_lqday_rng TYPE RANGE OF datum.

    TYPES: BEGIN OF ty_keydat,
             bukrs TYPE bukrs,
             pkind TYPE yfin_lp_e001,
             waers TYPE waers,
             lqpos TYPE flqpos,
             lqday TYPE ty_lqday_rng,
             ebene TYPE fdlev,
             ltype TYPE char1,
           END OF ty_keydat.

    CONSTANTS:
      mc_model TYPE seoclsname VALUE 'LCL_MVC_MODEL',
      mc_view  TYPE seoclsname VALUE 'LCL_MVC_VIEW'.

    METHODS:
      constructor,
      instantiate_app
        IMPORTING
          iv_model             TYPE seoclsname
          iv_view              TYPE seoclsname
        RETURNING
          value(ro_controller) TYPE REF TO lcl_mvc_controller,
      initialization,
      at_selection_screen,
      user_commmand_selscr
        CHANGING
          cv_ucomm TYPE sy-ucomm,
      start_of_selection,
      listbox_build,
      alv_session
        EXCEPTIONS
          contains_error,
      display_linedat
        IMPORTING
          iv_rejectbtn TYPE char1 DEFAULT abap_true
          iv_actualdat TYPE yfin_lp_tt01
          iv_manueldat TYPE yfin_lp_tt02
        EXPORTING
          ev_action    TYPE sychar70
          ev_idenr     TYPE idenr
        EXCEPTIONS
          contains_error,
      reject_manueldat
        IMPORTING
          iv_keydat    TYPE ty_keydat
          iv_column_id TYPE lvc_s_col
          iv_row_no    TYPE lvc_s_roid
        EXCEPTIONS
          contains_error,
      delete_manueldat
        IMPORTING
          iv_keydat    TYPE ty_keydat
          iv_idenr     TYPE idenr
          iv_column_id TYPE lvc_s_col
          iv_row_no    TYPE lvc_s_roid
        EXCEPTIONS
          contains_error,
      save_logdat
        EXCEPTIONS
          contains_error,
      get_icon
        IMPORTING
          iv_type        TYPE char1
        RETURNING
          value(rv_icon) TYPE text40,
      get_long_date
        IMPORTING
          !im_dat         TYPE datum
        RETURNING
          value(r_dattxt) TYPE text40,
      _expand_all_alvdat
        RETURNING
          value(rv_refresh) TYPE char1,
      _collapse_all_alvdat
        RETURNING
          value(rv_refresh) TYPE char1,
      _editable_alvdat
        RETURNING
          value(rv_refresh) TYPE char1,
      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
            e_column_id
            es_row_no,
      handle_toolbar_set FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
            e_object
            e_interactive,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
            e_ucomm,
      handle_data_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING
            e_modified
            et_good_cells,
      handle_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING
            e_dyndoc_id,
      event_top_of_page
        CHANGING
          dg_dyndoc_id TYPE REF TO cl_dd_document,
      display_top_of_page
        IMPORTING
          dg_dyndoc_id TYPE REF TO cl_dd_document.

  PRIVATE SECTION.
    DATA:
      mo_model TYPE REF TO lcl_mvc_model,
      mo_view  TYPE REF TO lcl_mvc_view.

ENDCLASS.                    "lcl_mvc_controller DEFINITION
*&---------------------------------------------------------------------*
*&  Class           LCL_MVC_VIEW           Implementation
*&---------------------------------------------------------------------*
CLASS lcl_mvc_model IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.                    "constructor
  METHOD retrieve_dat.
    FIELD-SYMBOLS: <ft_alvdat>  TYPE STANDARD TABLE,
                   <fs_cellcolor>  TYPE lvc_t_scol,
                   <fs_alvdat>  TYPE ANY,
                   <fs_celltab>  TYPE lvc_t_styl,"""""""""
*                   <fs_celltab>  TYPE STANDARD TABLE,
                   <fs_cellline>  TYPE ANY,
                   <ft_basedat> TYPE STANDARD TABLE,
                   <ft_linedat> TYPE STANDARD TABLE,
                   <fs_basedat> TYPE ANY,
                   <fs_linedat> TYPE ANY,
                   <fs_value>   TYPE ANY.


    TYPES: BEGIN OF ty_bkpfdat,
             bukrs TYPE bkpf-bukrs,
             belnr TYPE bkpf-belnr,
             gjahr TYPE bkpf-gjahr,
             waers TYPE bkpf-waers,
             xblnr TYPE bkpf-xblnr,
             bldat TYPE bkpf-bldat,
           END OF ty_bkpfdat,
           tt_bkpfdat TYPE HASHED TABLE OF ty_bkpfdat WITH UNIQUE KEY bukrs belnr gjahr.

    TYPES: tt_logdat TYPE STANDARD TABLE OF yfin_lp_t03 WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_lfa1dat,
             lifnr TYPE lfa1-lifnr,
             name1 TYPE lfa1-name1,
             name2 TYPE lfa1-name2,
           END OF ty_lfa1dat,
           tt_lfa1dat TYPE STANDARD TABLE OF ty_lfa1dat WITH NON-UNIQUE KEY lifnr." WITH UNIQUE KEY lifnr.

    TYPES: BEGIN OF ty_kna1dat,
             kunnr TYPE kna1-kunnr,
             name1 TYPE kna1-name1,
             name2 TYPE kna1-name2,
           END OF ty_kna1dat,
           tt_kna1dat TYPE STANDARD TABLE OF ty_kna1dat WITH NON-UNIQUE  KEY kunnr." WITH UNIQUE KEY kunnr.

    TYPES: BEGIN OF ty_skatdat,
             saknr TYPE skat-saknr,
             txt50 TYPE skat-txt50,
           END OF ty_skatdat,
           tt_skatdat TYPE HASHED TABLE OF ty_skatdat WITH UNIQUE KEY saknr.

    TYPES: BEGIN OF ty_bsegdat,
      bukrs  TYPE bseg-bukrs,
      belnr  TYPE bseg-belnr,
      gjahr  TYPE bseg-gjahr,
      buzei  TYPE bseg-buzei,
      gsber  TYPE bseg-gsber,
      kunnr  TYPE bseg-kunnr,
      lifnr  TYPE bseg-lifnr,
      lqday  TYPE bseg-zfbdt,
      zbd1t  TYPE bseg-zbd1t,
      zbd2t  TYPE bseg-zbd2t,
      zbd3t  TYPE bseg-zbd3t,
      ebene  TYPE bseg-fdlev,
      wrbtr  TYPE bseg-wrbtr,
      zuonr  TYPE bseg-zuonr,
      sgtxt  TYPE bseg-sgtxt,
      augbl  TYPE bseg-augbl,
      END OF ty_bsegdat.

    TYPES: BEGIN OF ty_flqdat_fc,
     ltype   TYPE  ltype,
     bukrs   TYPE  bukrs,
     belnr   TYPE  belnr_d,
     gjahr   TYPE  gjahr,
     buzei   TYPE  buzei,
     gsber   TYPE  gsber,
     lqpos   TYPE  yfin_lp_t02-lqpos,
     ebene   TYPE  yfin_lp_t02-ebene,
     lqday   TYPE  flqitemfi_fc-lqday,
     twaer   TYPE  twaer,
     wrbtr   TYPE  wrbtr,
     pkoart  TYPE  flqitemfi_fc-pkoart,
     partner TYPE  flqitemfi_fc-partner,
   END OF ty_flqdat_fc.

    TYPES: BEGIN OF ty_fdesdat,
        ltype TYPE ltype,
        bukrs TYPE bukrs,
        idenr TYPE idenr,
        gsber TYPE gsber,
        ebene TYPE fdes-ebene,
        lqpos TYPE yfin_lp_t02-lqpos,
        lqday TYPE datum,
        twaer TYPE dispw,
        wrbtr TYPE wrshb,
        zuonr TYPE fdes-zuonr,
        sgtxt TYPE sgtxt,
      END OF ty_fdesdat.

    TYPES: BEGIN OF ty_flqdat_fi,
        ltype TYPE ltype,
        bukrs TYPE bukrs,
        belnr TYPE belnr_d,
        gjahr TYPE gjahr,
        buzei TYPE buzei,
        gsber TYPE gsber,
        lqpos TYPE yfin_lp_t02-lqpos,
        ebene TYPE fdes-ebene,
        lqday TYPE flqitemfi_fc-lqday,
        twaer TYPE twaer,
        wrbtr TYPE wrbtr,
        pkoart TYPE flqitemfi_fc-pkoart,
        partner TYPE flqitemfi_fc-partner,
      END OF ty_flqdat_fi.

    TYPES: BEGIN OF ty_logdat,
        bukrs TYPE bukrs,
        pkind TYPE yfin_lp_e001,
        twaer TYPE twaer,
        lqpos TYPE yfin_lp_t02-lqpos,
        ebene TYPE fdes-ebene,
        lqday TYPE flqitemfi_fc-lqday,
        ltype TYPE ltype,
        prsid TYPE yfin_lp_t03-prsid,
        wrbtr TYPE wrbtr,
        erdat TYPE erdat,
        erzet TYPE erzet,
        ernam TYPE ernam,
      END OF ty_logdat.

*    TYPES:
*          BEGIN OF ty_valdat,
*            value TYPE char30,
*            range TYPE RANGE OF datum,
*          END OF ty_valdat .
*    TYPES:
*      tt_valdat TYPE STANDARD TABLE OF ty_valdat WITH DEFAULT KEY .
*    TYPES:
*      BEGIN OF ty_alvdat,
*        fieldcat  TYPE lvc_t_fcat,
*        alvoutput TYPE REF TO data,
*        keydate   TYPE tt_valdat,
*      END OF ty_alvdat .

    DATA: t_logdat TYPE TABLE OF ty_logdat,
          wa_log TYPE  ty_logdat,
          ls_logdat TYPE  ty_logdat,
    _logdat TYPE REF TO ty_logdat.


    DATA: t_flqdat_fi TYPE TABLE OF ty_flqdat_fi,
          _flqdat_fi TYPE REF TO ty_flqdat_fi.

    DATA: t_fdesdat TYPE TABLE OF ty_fdesdat,
          _fdesdat TYPE REF TO ty_fdesdat.

    DATA: t_flqdat_fc TYPE TABLE OF ty_flqdat_fc,
          _flqdat_fc TYPE REF TO ty_flqdat_fc.

    DATA: t_bsegdat TYPE TABLE OF ty_bsegdat,
          _bsegdat  TYPE REF TO ty_bsegdat.

    DATA: t_celldat   TYPE TABLE OF lvc_s_styl,
          s_celldat   TYPE lvc_s_styl,
          t_cellcolor TYPE lvc_t_scol,
          s_cellcolor TYPE lvc_s_scol,
          r_basedat   TYPE REF TO data,
          r_linedat   TYPE REF TO data,
          _lqday_rng  TYPE RANGE OF datum,
          ls_lqday_rng  LIKE LINE OF  _lqday_rng,
          t_basicdat  TYPE TABLE OF rgsbv,
          t_budat_rng TYPE RANGE OF datum,
          t_fdlev_rng TYPE RANGE OF bseg-fdlev,
          t_blart_rng TYPE RANGE OF blart,
          t_actualdat TYPE STANDARD TABLE OF yfin_lp_s01,
          ls_flqdat TYPE yfin_lp_s01,
          ls_docdat TYPE yfin_lp_s01,
          ls_root_logdat TYPE  yfin_lp_t03,
          wa_doc TYPE  yfin_lp_s01,
          _actualdat TYPE REF TO yfin_lp_s01,
          t_bkpfdat   TYPE tt_bkpfdat,
          _bkpfdat   TYPE  REF TO ty_bkpfdat,
          t_plandat   TYPE STANDARD TABLE OF yfin_lp_s01,
          lt_bseg   TYPE STANDARD TABLE OF bseg,
          ls_bseg   TYPE  bseg,
          ls_plandat   TYPE yfin_lp_s01,
          wa_plandat   TYPE yfin_lp_s01,
          _plandat   TYPE REF TO yfin_lp_s01,
          t_lfa1dat   TYPE tt_lfa1dat,
          _lfa1dat   TYPE REF TO ty_lfa1dat,
          t_kna1dat   TYPE tt_kna1dat,
          _kna1dat   TYPE  REF TO ty_kna1dat,
          t_skatdat   TYPE tt_skatdat,
          _skatdat   TYPE REF TO ty_skatdat.

    DATA: _basicdat TYPE REF TO rgsbv,
          _index TYPE i,
          ls_linedat LIKE LINE OF mt_linedat,
          ls_blart_rng LIKE LINE OF t_blart_rng.

    DATA _appdat TYPE REF TO ty_appdat.
    DATA _basedat TYPE REF TO yfin_lp_t01.
    DATA _keydat TYPE REF TO yfin_lp_cl01=>ty_valdat.

    DATA _linedat TYPE REF TO yfin_lp_t02.
    DATA v_value TYPE string.
    DATA : v_kunnr TYPE kunnr,
           v_lifnr TYPE lifnr,
           v_saknr TYPE saknr.
    FIELD-SYMBOLS: <fieldname> TYPE ANY,
                   <style> TYPE ANY,
                   <style2> TYPE ANY,
                   <style3> TYPE ANY,
                   <style4> TYPE ANY,
                   <maxlen> TYPE ANY.
    FIELD-SYMBOLS <fs_budat_rng> LIKE LINE OF t_budat_rng.
    FREE: mt_basedat.
    SELECT bukrs pkind waers hname rowno
      FROM yfin_lp_t01
        INTO CORRESPONDING FIELDS OF TABLE mt_basedat
          WHERE bukrs = p_bukrs
            AND waers IN s_waers.

    SORT mt_basedat BY rowno.

    FREE: mt_linedat.
    SELECT bukrs pkind waers rowno hname lqpos ebene edita
      FROM yfin_lp_t02
        INTO CORRESPONDING FIELDS OF TABLE mt_linedat
          WHERE bukrs = p_bukrs.

    SORT mt_linedat BY bukrs waers rowno.

    FREE: mt_sourcedat.

    SELECT bukrs pkind waers lqpos field_src pkind_src waers_src lqpos_src ebene_src
      FROM yfin_lp_t06
        INTO CORRESPONDING FIELDS OF TABLE mt_sourcedat
          WHERE bukrs = p_bukrs.

*--------------------------------------------------------------------*
*-&Planlama Verilerinin Çekilmesi->
*--------------------------------------------------------------------*
    FREE: t_basicdat.
    CALL FUNCTION 'G_SET_FETCH'
      EXPORTING
        class              = '0000'
        no_authority_check = 'X'
        setnr              = 'LP_BELGE_TURU'
      TABLES
        set_lines_basic    = t_basicdat
      EXCEPTIONS
        no_authority       = 1
        set_is_broken      = 2
        set_not_found      = 3
        OTHERS             = 4.
    FREE: t_blart_rng.
    LOOP AT t_basicdat REFERENCE INTO _basicdat.
      CLEAR ls_blart_rng.
      ls_blart_rng-sign = 'I'.
      ls_blart_rng-option = 'EQ'.
      ls_blart_rng-low = _basicdat->from.
      APPEND ls_blart_rng TO t_blart_rng.
    ENDLOOP.

    FREE: t_budat_rng.
    t_budat_rng[] = im_keyda[].

    READ TABLE t_budat_rng ASSIGNING <fs_budat_rng> INDEX 1.
    IF sy-subrc IS INITIAL.
      <fs_budat_rng>-low = <fs_budat_rng>-low - im_ldays.
    ENDIF.

    FREE: t_bkpfdat.

    SELECT bukrs belnr gjahr waers xblnr bldat
      FROM bkpf
        INTO CORRESPONDING FIELDS OF TABLE t_bkpfdat
          WHERE bukrs EQ p_bukrs
            AND budat IN t_budat_rng
            AND blart IN t_blart_rng.

    IF NOT t_bkpfdat[] IS INITIAL.
      FREE: t_fdlev_rng.
      DATA: lt_t02 TYPE TABLE OF yfin_lp_t02-ebene,
            ls_t02 LIKE LINE OF lt_t02,
            ls_fdlev_rng LIKE LINE OF t_fdlev_rng.

      SELECT ebene
        FROM yfin_lp_t02
          INTO TABLE lt_t02
            WHERE bukrs = p_bukrs.

      LOOP AT lt_t02 INTO ls_t02.
        CLEAR ls_fdlev_rng.
        ls_fdlev_rng-sign = 'I'.
        ls_fdlev_rng-option = 'EQ'.
        ls_fdlev_rng-low = ls_t02.
        APPEND ls_fdlev_rng TO t_fdlev_rng.
      ENDLOOP.

      SELECT bukrs belnr gjahr buzei gsber kunnr lifnr
             zfbdt AS lqday
             zbd1t zbd2t zbd3t
             fdlev AS ebene
             wrbtr zuonr sgtxt augbl
        FROM bseg
          INTO TABLE t_bsegdat
            FOR ALL ENTRIES IN t_bkpfdat
              WHERE bukrs = t_bkpfdat-bukrs
                AND gjahr = t_bkpfdat-gjahr
                AND belnr = t_bkpfdat-belnr
                AND koart IN ('D', 'K')
*                AND augbl EQ ''
                AND fdlev IN t_fdlev_rng.

      FREE: t_lfa1dat, t_kna1dat.
      IF NOT t_bsegdat[] IS INITIAL.
        SELECT DISTINCT lifnr name1 name2
          FROM lfa1
            INTO TABLE t_lfa1dat
              FOR ALL ENTRIES IN t_bsegdat
                WHERE lifnr = t_bsegdat-lifnr.

        SELECT DISTINCT kunnr name1 name2
          FROM kna1
            INTO TABLE t_kna1dat
              FOR ALL ENTRIES IN t_bsegdat
                WHERE kunnr = t_bsegdat-kunnr.
      ENDIF.

      LOOP AT t_bsegdat REFERENCE INTO _bsegdat WHERE augbl IS INITIAL.
        _index = sy-tabix.
        _bsegdat->lqday = _bsegdat->lqday + _bsegdat->zbd1t + _bsegdat->zbd2t + _bsegdat->zbd3t.
        IF _bsegdat->lqday NOT IN im_keyda.
          DELETE t_bsegdat INDEX _index. CONTINUE.
        ENDIF.
        APPEND INITIAL LINE TO t_plandat REFERENCE INTO _plandat.

        MOVE-CORRESPONDING _bsegdat->* TO _plandat->*.
        READ TABLE t_bkpfdat REFERENCE INTO _bkpfdat WITH TABLE KEY bukrs = _plandat->bukrs belnr = _plandat->belnr gjahr = _plandat->gjahr.
        IF sy-subrc IS INITIAL.
          _plandat->xblnr = _bkpfdat->xblnr.
        ENDIF.

        READ TABLE mt_linedat INTO ls_linedat WITH KEY ebene = _plandat->ebene.
        IF sy-subrc EQ 0.
          _plandat->lqpos = ls_linedat-lqpos.
        ENDIF.

        _plandat->ltype = 'P'.
        READ TABLE t_bkpfdat REFERENCE INTO _bkpfdat WITH TABLE KEY bukrs = _bsegdat->bukrs belnr = _bsegdat->belnr gjahr = _bsegdat->gjahr.
        IF sy-subrc IS INITIAL.
          _plandat->twaer = _bkpfdat->waers.
          _plandat->xblnr = _bkpfdat->xblnr.
          _plandat->bldat = _bkpfdat->bldat.
        ENDIF.
        IF _bsegdat->lifnr <> space.
          READ TABLE t_lfa1dat REFERENCE INTO _lfa1dat WITH TABLE KEY lifnr = _bsegdat->lifnr.
          IF sy-subrc IS INITIAL.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = _lfa1dat->lifnr
              IMPORTING
                output = _lfa1dat->lifnr.

            _plandat->accnum = _lfa1dat->lifnr.

            CONCATENATE _lfa1dat->name1 _lfa1dat->name2 INTO _plandat->name1.
          ENDIF.
        ENDIF.
        IF _bsegdat->kunnr <> space.
          READ TABLE t_kna1dat REFERENCE INTO _kna1dat WITH TABLE KEY kunnr = _bsegdat->kunnr.
          IF sy-subrc IS INITIAL.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = _kna1dat->kunnr
              IMPORTING
                output = _kna1dat->kunnr.

            _plandat->accnum = _kna1dat->kunnr.

            CONCATENATE  _kna1dat->name1 _kna1dat->name2 INTO _plandat->name1.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.


    SELECT DISTINCT a~zbukr AS bukrs  a~belnr a~gjahr a~buzei a~gsber
                    b~lqpos  b~ebene a~lqday a~twaer a~wrbtr a~pkoart a~partner
      FROM flqitemfi_fc AS a
        INNER JOIN yfin_lp_t02 AS b ON b~bukrs = a~bukrs
                                   AND b~lqpos = a~lqpos
                                   AND b~waers = a~twaer

          INTO CORRESPONDING FIELDS OF TABLE t_flqdat_fc
            WHERE a~bukrs EQ p_bukrs
              AND a~lqday IN s_keydat.

    FREE: t_lfa1dat, t_kna1dat, t_skatdat.
    IF NOT t_flqdat_fc[] IS INITIAL.
      SELECT DISTINCT lifnr name1 name2
        FROM lfa1
          INTO TABLE t_lfa1dat
            FOR ALL ENTRIES IN t_flqdat_fc
              WHERE lifnr = t_flqdat_fc-partner.
      SELECT * FROM bseg AS a
        INTO TABLE lt_bseg
        FOR ALL ENTRIES IN t_flqdat_fc
                    WHERE a~belnr = t_flqdat_fc-belnr
                      AND a~bukrs = t_flqdat_fc-bukrs
                      AND a~gjahr = t_flqdat_fc-gjahr.
      SELECT DISTINCT kunnr name1 name2
      FROM kna1
        INTO TABLE t_kna1dat
          FOR ALL ENTRIES IN t_flqdat_fc
            WHERE kunnr = t_flqdat_fc-partner.

      SELECT DISTINCT saknr txt50
        FROM skat
          INTO TABLE t_skatdat
            FOR ALL ENTRIES IN t_flqdat_fc
              WHERE saknr = t_flqdat_fc-partner
                AND spras = sy-langu.
    ENDIF.

    LOOP AT t_flqdat_fc REFERENCE INTO _flqdat_fc.
      _flqdat_fc->ltype = 'P'.
      LOOP AT lt_bseg INTO ls_bseg WHERE belnr = _flqdat_fc->belnr
                                     AND bukrs = _flqdat_fc->bukrs
                                     AND gjahr = _flqdat_fc->gjahr
                                     AND buzei = _flqdat_fc->buzei
                                     AND augbl NE ''.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO t_plandat REFERENCE INTO _plandat.
        MOVE-CORRESPONDING _flqdat_fc->* TO _plandat->*.
        READ TABLE t_bkpfdat REFERENCE INTO _bkpfdat WITH TABLE KEY bukrs = _plandat->bukrs belnr = _plandat->belnr gjahr = _plandat->gjahr.
        IF sy-subrc IS INITIAL.
          _plandat->xblnr = _bkpfdat->xblnr.
        ENDIF.
        CASE _flqdat_fc->pkoart.
          WHEN 'D'.
            READ TABLE t_kna1dat REFERENCE INTO _kna1dat WITH TABLE KEY kunnr = _flqdat_fc->partner.
            IF sy-subrc IS INITIAL.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = _kna1dat->kunnr
                IMPORTING
                  output = _kna1dat->kunnr.
              _plandat->accnum = _kna1dat->kunnr.
              CONCATENATE _kna1dat->name1 _kna1dat->name2 INTO _plandat->name1.

            ENDIF.
          WHEN 'K'.
            READ TABLE t_lfa1dat REFERENCE INTO _lfa1dat WITH TABLE KEY lifnr = _flqdat_fc->partner.
            IF sy-subrc IS INITIAL.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = _lfa1dat->lifnr
                IMPORTING
                  output = _lfa1dat->lifnr.
              _plandat->accnum = _lfa1dat->lifnr.
              CONCATENATE _lfa1dat->name1 _lfa1dat->name2 INTO _plandat->name1.
            ENDIF.
          WHEN 'S'.
            READ TABLE t_skatdat REFERENCE INTO _skatdat WITH TABLE KEY saknr = _flqdat_fc->partner.
            IF sy-subrc IS INITIAL.
              CLEAR v_saknr.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = _skatdat->saknr
                IMPORTING
                  output = v_saknr.
              _plandat->accnum = v_saknr.
              _plandat->name1 = _skatdat->txt50.

            ENDIF.
        ENDCASE.
      ENDIF.
    ENDLOOP.

    SELECT DISTINCT
       a~bukrs a~idenr a~gsber a~ebene b~lqpos a~datum AS lqday a~dispw AS twaer a~wrshb AS wrbtr a~zuonr a~sgtxt
      FROM fdes AS a
        INNER JOIN yfin_lp_t02 AS b ON b~bukrs = a~bukrs AND b~ebene = a~ebene AND b~waers = a~dispw
          INTO CORRESPONDING FIELDS OF TABLE t_fdesdat
            WHERE a~bukrs EQ p_bukrs
              AND a~datum IN im_keyda
              AND a~merkm NE 'CANCEL'.

    LOOP AT t_fdesdat REFERENCE INTO _fdesdat.
      _fdesdat->ltype = 'P'.
      APPEND INITIAL LINE TO t_plandat REFERENCE INTO _plandat.
      MOVE-CORRESPONDING _fdesdat->* TO _plandat->*.
    ENDLOOP.

*--------------------------------------------------------------------*
*-&Fiili Verilerin Çekilmesi->
*--------------------------------------------------------------------*

    SELECT DISTINCT a~zbukr AS bukrs a~belnr a~gjahr a~buzei a~gsber a~lqpos b~ebene a~lqday a~twaer a~wrbtr a~pkoart a~partner
      FROM flqitemfi AS a
        INNER JOIN yfin_lp_t02 AS b ON b~bukrs = a~bukrs AND b~lqpos = a~lqpos AND b~waers = a~twaer
          INTO CORRESPONDING FIELDS OF TABLE t_flqdat_fi
            WHERE a~zbukr EQ p_bukrs
              AND a~lqday IN s_keydat
              AND a~xdelete EQ space.

    FREE: t_lfa1dat, t_kna1dat, t_skatdat.
    IF NOT t_flqdat_fi[] IS INITIAL.
      SELECT DISTINCT lifnr name1 name2
        FROM lfa1
          INTO TABLE t_lfa1dat
            FOR ALL ENTRIES IN t_flqdat_fi
              WHERE lifnr = t_flqdat_fi-partner.

      SELECT DISTINCT kunnr name1 name2
        FROM kna1
          INTO TABLE t_kna1dat
            FOR ALL ENTRIES IN t_flqdat_fi
              WHERE kunnr = t_flqdat_fi-partner.

      SELECT DISTINCT saknr txt50
        FROM skat
          INTO TABLE t_skatdat
            FOR ALL ENTRIES IN t_flqdat_fi
              WHERE saknr = t_flqdat_fi-partner
                AND spras = sy-langu.
    ENDIF.

    LOOP AT t_flqdat_fi REFERENCE INTO _flqdat_fi.
      _flqdat_fi->ltype = 'A'.
      APPEND INITIAL LINE TO t_actualdat REFERENCE INTO _actualdat.
      MOVE-CORRESPONDING _flqdat_fi->* TO _actualdat->*.
      CASE _flqdat_fi->pkoart.
        WHEN 'D'.
          READ TABLE t_kna1dat REFERENCE INTO _kna1dat WITH TABLE KEY kunnr = _flqdat_fi->partner.
          IF sy-subrc IS INITIAL.

            CLEAR v_kunnr.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = _kna1dat->kunnr
              IMPORTING
                output = v_kunnr.

            _actualdat->accnum = v_kunnr.

            CONCATENATE _kna1dat->name1 _kna1dat->name2  INTO _actualdat->name1.

          ENDIF.
        WHEN 'K'.
          READ TABLE t_lfa1dat REFERENCE INTO _lfa1dat WITH TABLE KEY lifnr = _flqdat_fi->partner.
          IF sy-subrc IS INITIAL.
            CLEAR v_lifnr.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = _lfa1dat->lifnr
              IMPORTING
                output = v_lifnr.
            _actualdat->accnum = v_lifnr.

            CONCATENATE _lfa1dat->name1 _lfa1dat->name2  INTO _actualdat->name1.

          ENDIF.
        WHEN 'S'.
          READ TABLE t_skatdat REFERENCE INTO _skatdat WITH TABLE KEY saknr = _flqdat_fi->partner.
          IF sy-subrc IS INITIAL.
            CLEAR v_saknr.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = _skatdat->saknr
              IMPORTING
                output = v_saknr.
            _actualdat->accnum = v_saknr.
            _actualdat->name1 = _skatdat->txt50.
          ENDIF.
      ENDCASE.
      READ TABLE t_bkpfdat REFERENCE INTO _bkpfdat WITH TABLE KEY bukrs = _actualdat->bukrs belnr = _actualdat->belnr gjahr = _actualdat->gjahr.
      IF sy-subrc IS INITIAL.
        _actualdat->bldat = _bkpfdat->bldat.
      ENDIF.

    ENDLOOP.

*->begin of - fkarakas -05.10.2022
    DATA: _date TYPE sy-datum.
    DATA: _date_actual TYPE sy-datum.

*--------------------------------------------------------------------*
*-&Banka Bakiyeleri Verileri->
*--------------------------------------------------------------------*

    TYPES:BEGIN OF ty_report,
        bukrs     TYPE yfin_eks_t_014-bukrs,
        sirketadi TYPE t001-butxt,
        bankakodu TYPE yfin_eks_t_014-bankakodu,
        bankaadi  TYPE yfin_eks_s_009-bankaadi,
        dovizkodu TYPE yfin_eks_t_013-dovizkodu,
        sonbakiye TYPE yfin_eks_t_014-sonbakiye,
        date      TYPE sy-datum,
      END OF ty_report.

    DATA : lt_headers TYPE TABLE OF yfin_eks_t_013,
           ls_headers TYPE yfin_eks_t_013,
           lt_item    TYPE TABLE OF yfin_eks_t_014,
           ls_item    TYPE yfin_eks_t_014,
           gt_report TYPE TABLE OF ty_report,
           gs_report TYPE ty_report.

    SELECT DISTINCT  h~bukrs
                     h~bankakodu
                     h~subekodu
                     h~hesapno
                     h~subeadi
                     h~dovizkodu
                     h~hbkid
                     h~hktid
                     h~hkont
                     h~bakiye
                     h~bloketutar
                     h~sonharekettar
                     h~hesapturukodu
                     h~hesapturuadi
      FROM yfin_eks_t_013 AS h
      INTO CORRESPONDING FIELDS OF TABLE lt_headers
      WHERE h~bukrs EQ p_bukrs.

    LOOP AT mt_keydat REFERENCE INTO _keydat .

      _date_actual = _keydat->value.
      _date = _date_actual - 1.
      LOOP AT lt_headers INTO ls_headers.
        CLEAR: ls_item, gs_report.

        MOVE-CORRESPONDING ls_headers TO gs_report.

        SELECT * INTO ls_item
          FROM yfin_eks_t_014
          WHERE bukrs     = ls_headers-bukrs AND
                bankakodu = ls_headers-bankakodu AND
                subekodu  = ls_headers-subekodu AND
                hesapno   = ls_headers-hesapno AND
                tarih     LE _date_actual
          ORDER BY tarih DESCENDING
                   saat DESCENDING
                   timestamp DESCENDING.

          MOVE-CORRESPONDING ls_item TO gs_report.
          EXIT.
        ENDSELECT.
        gs_report-date = _keydat->value.
        COLLECT gs_report INTO gt_report.
      ENDLOOP.
    ENDLOOP.

    LOOP AT gt_report INTO gs_report.
      APPEND INITIAL LINE TO t_actualdat REFERENCE INTO _actualdat.
      _actualdat->ltype = 'A'.
      _actualdat->bukrs = gs_report-bukrs.
      _actualdat->gjahr = gs_report-date+0(4).
      _actualdat->lqpos = 'B_BANKA_BAKIYE'.
      _actualdat->ebene = 14.
      _actualdat->lqday = gs_report-date.
      _actualdat->wrbtr = gs_report-sonbakiye.
      _actualdat->twaer = gs_report-dovizkodu.
      APPEND INITIAL LINE TO t_plandat REFERENCE INTO _plandat.
      MOVE-CORRESPONDING _actualdat->* TO _plandat->*.
      _plandat->ltype = 'P'.
    ENDLOOP.

*--------------------------------------------------------------------*
*-&Manuel Banka Bakiyeleri->
*--------------------------------------------------------------------*

    TYPES:BEGIN OF ty_ekstre,
          bukrs       TYPE bkpf-bukrs,
          dovizkodu   TYPE bkpf-waers,
          hkont       TYPE ska1-saknr,
          borc_alacak TYPE bseg-shkzg,
          tutar       TYPE yfin_eks_t_014-tutar,
          budat       TYPE bkpf-budat,
          aciklama    TYPE yfin_eks_t_014-aciklama,
      END OF ty_ekstre,
    tt_ekstre TYPE TABLE OF ty_ekstre WITH DEFAULT KEY.

    DATA: r_tcode   TYPE RANGE OF sy-tcode,
          rng_tcode LIKE LINE OF r_tcode,
          r_data    TYPE REF TO data,
          t_data  TYPE tt_ekstre,
          t_ekstre  TYPE tt_ekstre,
          s_ekstre  TYPE ty_ekstre.

    rng_tcode-sign = 'I'.
    rng_tcode-option = 'EQ'.
    rng_tcode-low = 'TDP'.
    APPEND rng_tcode TO r_tcode.

    FIELD-SYMBOLS: <t_data>   TYPE ANY TABLE,
                   <data>     TYPE data.

    cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
                                            metadata = abap_false
                                            data     = abap_true ).

    SUBMIT yfin_eks_p_008 WITH p_bukrs = p_bukrs
                          WITH s_islmkd IN r_tcode
                          WITH s_tarih IN t_budat_rng[]
                          WITH r_alv EQ abap_true
                          WITH r_kay EQ abap_true
                          WITH p_mz EQ abap_true AND RETURN.
    TRY.
        cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = r_data ).
        ASSIGN r_data->* TO <t_data>.
      CATCH cx_salv_bs_sc_runtime_info.
        EXIT.
    ENDTRY.
    cl_salv_bs_runtime_info=>clear_all( ).
    IF <t_data> IS ASSIGNED.
      LOOP AT <t_data> ASSIGNING <data>.
        CLEAR: s_ekstre.
        MOVE-CORRESPONDING <data> TO s_ekstre.
        APPEND s_ekstre TO t_ekstre.
      ENDLOOP.
    ENDIF.
    SORT t_ekstre BY dovizkodu.
    LOOP AT t_ekstre INTO s_ekstre WHERE hkont CP '102*' AND aciklama EQ 'MEVDUAT HESAP AÇILIŞI'.
      s_ekstre-tutar = s_ekstre-tutar * -1.
      COLLECT s_ekstre INTO t_data.
    ENDLOOP.

    LOOP AT t_data INTO s_ekstre.
      APPEND INITIAL LINE TO t_actualdat REFERENCE INTO _actualdat.
      _actualdat->ltype = 'A'.
      _actualdat->bukrs = s_ekstre-bukrs.
      _actualdat->gjahr = s_keydat-low+0(4).
      _actualdat->lqpos = 'B_BANKA_MANUEL'.
      _actualdat->ebene = 17.
      _actualdat->lqday = s_ekstre-budat.
      _actualdat->wrbtr = s_ekstre-tutar.
      _actualdat->twaer = s_ekstre-dovizkodu.
      APPEND INITIAL LINE TO t_plandat REFERENCE INTO _plandat.
      MOVE-CORRESPONDING _actualdat->* TO _plandat->*.
      _plandat->ltype = 'P'.
    ENDLOOP.

*<-end of - fkarakas -05.10.2022

*--------------------------------------------------------------------*
*-&Manuel Düzeltme Kayıtları->
*--------------------------------------------------------------------*

    SELECT a~bukrs a~pkind a~twaer a~lqpos a~ebene a~lqday a~ltype a~prsid a~wrbtr a~erdat a~erzet a~ernam
      FROM yfin_lp_t03 AS a
        INTO TABLE t_logdat
          WHERE bukrs EQ p_bukrs
            AND lqday IN s_keydat
            AND xdele EQ space.

    DATA _helper_cl01 TYPE REF TO yfin_lp_cl01.
    CREATE OBJECT _helper_cl01 TYPE  yfin_lp_cl01.
    DATA: _basealv TYPE yfin_lp_cl01=>ty_alvdat,
          _linealv TYPE yfin_lp_cl01=>ty_alvdat.

    LOOP AT mt_basedat REFERENCE INTO _basedat.
      FREE: mv_appdat.
      MOVE-CORRESPONDING _basedat->* TO mv_appdat.

      _basealv  = _helper_cl01->dynamic_alvdat(
            iv_tmunit = p_tunit
            iv_begdat = s_keydat-low
            iv_enddat = s_keydat-high
            iv_celdat = abap_true ).

      mv_appdat-base_tab ?= _basealv-alvoutput.
      ASSIGN mv_appdat-base_tab->* TO <ft_basedat>.
      CREATE DATA r_basedat LIKE LINE OF <ft_basedat>. ASSIGN r_basedat->* TO <fs_basedat>.

      _linealv = _helper_cl01->dynamic_alvdat(
          iv_tmunit = p_tunit
          iv_begdat = s_keydat-low
          iv_enddat = s_keydat-high
          iv_celdat = abap_true ).

      mv_appdat-line_tab ?= _linealv-alvoutput.
      ASSIGN mv_appdat-line_tab->* TO <ft_linedat>.
      CREATE DATA r_linedat LIKE LINE OF <ft_linedat>. ASSIGN r_linedat->* TO <fs_linedat>.
      FREE: <ft_basedat>, <ft_linedat>.
      LOOP AT mt_linedat REFERENCE INTO _linedat
                         WHERE bukrs = _basedat->bukrs AND
                               waers = _basedat->waers AND
                               pkind = _basedat->pkind.

        FREE: <fs_linedat>, <fs_basedat>.
        ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_linedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          <fs_value> = _linedat->bukrs.
        ENDIF.
        ASSIGN COMPONENT 'PKIND' OF STRUCTURE <fs_linedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          <fs_value> = _linedat->pkind.
        ENDIF.
        ASSIGN COMPONENT 'WAERS' OF STRUCTURE <fs_linedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          <fs_value> = _linedat->waers.
        ENDIF.
        ASSIGN COMPONENT 'HEADER' OF STRUCTURE <fs_linedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          <fs_value> = _linedat->hname.
        ENDIF.
        ASSIGN COMPONENT 'LQPOS' OF STRUCTURE <fs_linedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          <fs_value> = _linedat->lqpos.
        ENDIF.
        ASSIGN COMPONENT 'EBENE' OF STRUCTURE <fs_linedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          <fs_value> = _linedat->ebene.
        ENDIF.
*--------------------------------------------------------------------*
*-& Actual Dat->
*--------------------------------------------------------------------*
        DATA: _amtlog TYPE wrbtr,
              _amtdoc TYPE wrbtr.
        LOOP AT mt_keydat REFERENCE INTO _keydat.

          FREE: _lqday_rng.
          LOOP AT t_logdat REFERENCE INTO _logdat WHERE bukrs = _linedat->bukrs AND pkind = _linedat->pkind AND twaer = _linedat->waers AND lqpos = _linedat->lqpos AND ltype = 'A'.
            CLEAR ls_lqday_rng.
            ls_lqday_rng-sign = 'I'.
            ls_lqday_rng-option = 'EQ'.
            ls_lqday_rng-low = _logdat->lqday.
            APPEND ls_lqday_rng TO _lqday_rng.
          ENDLOOP.
          CLEAR v_value.
          CONCATENATE 'A_' _keydat->value INTO v_value.
          ASSIGN COMPONENT v_value OF STRUCTURE <fs_linedat> TO <fs_value>.
          IF sy-subrc IS INITIAL.

            IF _lqday_rng[] IS INITIAL.
              LOOP AT t_actualdat INTO wa_doc WHERE lqpos EQ _linedat->lqpos
                                                AND twaer EQ _linedat->waers
                                                AND lqday IN _keydat->range.
                <fs_value> = <fs_value> + wa_doc-wrbtr.
              ENDLOOP.
*              <fs_value> = reduce WRBTR( init VAL type WRBTR for WA_DOC in T_ACTUALDAT
*                                         where ( LQPOS eq _LINEDAT->LQPOS and TWAER eq _LINEDAT->WAERS and LQDAY in _KEYDAT->RANGE ) NEXT val = val + wa_doc-wrbtr ).
            ELSE.
              LOOP AT t_actualdat INTO wa_doc WHERE lqpos EQ _linedat->lqpos
                                                AND twaer EQ _linedat->waers
                                                AND lqday IN _keydat->range
                                                AND lqday NOT IN _lqday_rng.
                _amtdoc = _amtdoc + wa_doc-wrbtr.
              ENDLOOP.

*              data(_amtdoc) = reduce WRBTR( init VAL type WRBTR for WA_DOC in T_ACTUALDAT
*                                            where ( LQPOS eq _LINEDAT->LQPOS and TWAER eq _LINEDAT->WAERS and LQDAY in _KEYDAT->RANGE and LQDAY not IN _lqday_rng ) NEXT val = val + wa_doc-wrbtr ).


              LOOP AT t_logdat INTO wa_log WHERE pkind = _linedat->pkind
                                                AND twaer = _linedat->waers
                                                AND lqpos = _linedat->lqpos
                                                AND ltype = 'A'
                                                AND lqday IN _keydat->range.
                _amtlog = _amtlog + wa_log-wrbtr.
              ENDLOOP.

*              data(_amtlog) = reduce WRBTR( init VAL type WRBTR for WA_LOG in T_LOGDAT
*                                            where ( PKIND = _LINEDAT->PKIND and TWAER = _LINEDAT->WAERS and LQPOS = _LINEDAT->LQPOS and LTYPE = 'A' and LQDAY in _KEYDAT->RANGE ) NEXT val = val + wa_log-wrbtr ).

              <fs_value> = _amtdoc + _amtlog.
              CLEAR: _amtdoc, _amtlog.
            ENDIF.
            CLEAR: ls_logdat, ls_docdat.
            LOOP AT t_actualdat INTO ls_flqdat WHERE bukrs EQ _linedat->bukrs
                                                 AND twaer EQ _linedat->waers
                                                 AND lqpos EQ _linedat->lqpos
                                                 AND lqday IN _keydat->range.
              MOVE-CORRESPONDING ls_flqdat TO ls_docdat.
              APPEND ls_docdat TO mv_appdat-root_dat-_docdat.
            ENDLOOP.

*            APPEND LINES OF value yfin_lp_tt01( for ls_flqdat in t_actualdat
*              where ( bukrs eq _linedat->bukrs and twaer eq _linedat->waers and lqpos eq _linedat->lqpos and lqday in _keydat->range )
*                    ( corresponding #( ls_flqdat ) ) ) TO mv_appdat-root_dat-_docdat.

            LOOP AT t_logdat INTO ls_logdat WHERE bukrs EQ _linedat->bukrs
                                                 AND pkind EQ _linedat->pkind
                                                 AND twaer EQ _linedat->waers
                                                 AND lqpos EQ _linedat->lqpos
                                                 AND ltype = 'A'
                                                 AND lqday IN _keydat->range.

              MOVE-CORRESPONDING ls_logdat TO ls_root_logdat.
              APPEND ls_root_logdat TO mv_appdat-root_dat-_logdat.
            ENDLOOP.

*            APPEND LINES OF value tt_logdat( for ls_logdat in t_logdat
*              where ( bukrs eq _linedat->bukrs and pkind eq _linedat->pkind and twaer eq _linedat->waers and lqpos eq _linedat->lqpos and ltype = 'A' and lqday in _keydat->range )
*                    ( corresponding #( ls_logdat ) ) ) TO mv_appdat-root_dat-_logdat.
          ENDIF.

*--------------------------------------------------------------------*
*-& Planner Dat->
*--------------------------------------------------------------------*
          FREE: _lqday_rng.
          LOOP AT t_logdat REFERENCE INTO _logdat WHERE bukrs = _linedat->bukrs AND pkind = _linedat->pkind AND twaer = _linedat->waers AND ebene = _linedat->ebene AND ltype = 'P'.
            CLEAR ls_lqday_rng.
            ls_lqday_rng-sign = 'I'.
            ls_lqday_rng-option = 'EQ'.
            ls_lqday_rng-low = _logdat->lqday .
            APPEND ls_lqday_rng TO _lqday_rng.
*            APPEND value #( sign = 'I' option = 'EQ' low = _logdat->lqday ) TO _lqday_rng.
          ENDLOOP.
          CLEAR v_value.
          CONCATENATE 'P_' _keydat->value INTO v_value.
          ASSIGN COMPONENT v_value OF STRUCTURE <fs_linedat> TO <fs_value>.
          IF sy-subrc IS INITIAL.

            IF _lqday_rng[] IS INITIAL.
              LOOP AT t_plandat INTO wa_plandat WHERE ebene = _linedat->ebene
                                                  AND twaer EQ _linedat->waers
                                                  AND lqday IN _keydat->range.
                <fs_value> = <fs_value> + wa_plandat-wrbtr.
              ENDLOOP.
*              <fs_value> = reduce WRBTR( init VAL type WRBTR for WA_PLANDAT in T_PLANDAT
*                                         where ( EBENE = _LINEDAT->EBENE and TWAER eq _LINEDAT->WAERS and LQDAY in _KEYDAT->RANGE ) NEXT val = val + wa_plandat-wrbtr ).
            ELSE.
              CLEAR: _amtdoc.
              LOOP AT t_plandat INTO wa_plandat WHERE ebene = _linedat->ebene
                                                  AND twaer EQ _linedat->waers
                                                  AND lqday IN _keydat->range
                                                  AND lqday NOT IN _lqday_rng.
                _amtdoc = _amtdoc + wa_plandat-wrbtr.
              ENDLOOP.
*              _amtdoc = reduce WRBTR( init VAL type WRBTR for WA_PLANDAT in T_PLANDAT
*                                       where ( EBENE = _LINEDAT->EBENE and TWAER eq _LINEDAT->WAERS and LQDAY in _KEYDAT->RANGE and LQDAY not IN _lqday_rng ) NEXT val = val + wa_plandat-wrbtr ).

              CLEAR: _amtlog.

              LOOP AT t_logdat INTO wa_log WHERE pkind = _linedat->pkind
                                                  AND twaer = _linedat->waers
                                                  AND ebene = _linedat->ebene
                                                  AND ltype = 'P'
                                                  AND lqday IN _keydat->range.

                _amtlog = _amtlog + wa_log-wrbtr.

              ENDLOOP.
*              _amtlog = reduce WRBTR( init VAL type WRBTR for WA_LOG in T_LOGDAT
*                                      where ( PKIND = _LINEDAT->PKIND and TWAER = _LINEDAT->WAERS and EBENE = _LINEDAT->EBENE and LTYPE = 'P' and LQDAY in _KEYDAT->RANGE ) NEXT val = val + wa_log-wrbtr ).

              <fs_value> = _amtdoc + _amtlog.
              CLEAR: _amtdoc, _amtlog.
            ENDIF.

            CLEAR: ls_logdat, ls_docdat.
            LOOP AT t_plandat INTO ls_plandat WHERE bukrs EQ _linedat->bukrs
                                                AND twaer EQ _linedat->waers
                                                AND ebene = _linedat->ebene
                                                AND lqday IN _keydat->range.
              MOVE-CORRESPONDING ls_plandat TO ls_docdat.
              APPEND ls_docdat TO mv_appdat-root_dat-_docdat.
            ENDLOOP.

*            APPEND LINES OF value yfin_lp_tt01( for ls_plandat in t_plandat
*              where ( bukrs eq _linedat->bukrs and twaer eq _linedat->waers and ebene = _linedat->ebene and lqday in _keydat->range )
*                    ( corresponding #( ls_plandat ) ) ) TO mv_appdat-root_dat-_docdat.
            LOOP AT t_logdat INTO ls_logdat WHERE bukrs EQ _linedat->bukrs
                                              AND pkind EQ _linedat->pkind
                                              AND twaer EQ _linedat->waers
                                              AND ebene = _linedat->ebene
                                              AND ltype = 'P'
                                              AND lqday IN _keydat->range.
              MOVE-CORRESPONDING ls_logdat TO ls_root_logdat.
              APPEND ls_root_logdat TO mv_appdat-root_dat-_logdat.
            ENDLOOP.
            CLEAR ls_logdat.
*            APPEND LINES OF value tt_logdat( for ls_logdat in t_logdat
*              where ( bukrs eq _linedat->bukrs and pkind eq _linedat->pkind and twaer eq _linedat->waers and ebene = _linedat->ebene and ltype = 'P' and lqday in _keydat->range )
*                    ( corresponding #( ls_logdat ) ) ) TO mv_appdat-root_dat-_logdat.
          ENDIF.

        ENDLOOP.

        MOVE-CORRESPONDING <fs_linedat> TO <fs_basedat>.
        ASSIGN COMPONENT 'HEADER' OF STRUCTURE <fs_basedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          <fs_value> = _basedat->hname.
        ENDIF.
        ASSIGN COMPONENT 'LQPOS' OF STRUCTURE <fs_basedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          CLEAR: <fs_value>.
        ENDIF.
        ASSIGN COMPONENT 'EBENE' OF STRUCTURE <fs_basedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          CLEAR: <fs_value>.
        ENDIF.
        ASSIGN COMPONENT 'EXPAND' OF STRUCTURE <fs_basedat> TO <fs_value>.
        IF sy-subrc IS INITIAL.
          <fs_value> = _controller->get_icon( iv_type =  'E' ).
        ENDIF.
        COLLECT <fs_basedat> INTO <ft_basedat>.
        APPEND <fs_linedat> TO <ft_linedat>.
      ENDLOOP.
      APPEND mv_appdat TO mt_appdat.
    ENDLOOP.

    ASSIGN mt_alvdat->* TO <ft_alvdat>.
    LOOP AT mt_appdat REFERENCE INTO _appdat.
      UNASSIGN: <ft_basedat>.
      ASSIGN _appdat->base_tab->* TO <ft_basedat>.
      LOOP AT <ft_basedat> ASSIGNING <fs_basedat>.
        APPEND INITIAL LINE TO <ft_alvdat> ASSIGNING <fs_alvdat>.
        MOVE-CORRESPONDING <fs_basedat> TO <fs_alvdat>.
*        <fs_alvdat> = corresponding #( <fs_basedat> ).
        ASSIGN COMPONENT 'CELLTAB' OF STRUCTURE <fs_alvdat> TO <fs_celltab>.
        IF sy-subrc IS INITIAL.
          FREE: t_celldat.
          CLEAR s_celldat.
          s_celldat-fieldname  = 'EXPAND'.
          s_celldat-style      = cl_gui_alv_grid=>mc_style_hotspot.
          APPEND s_celldat TO t_celldat.
          CLEAR s_celldat.
          s_celldat-fieldname  = 'HEADER'.
          s_celldat-style      = '00000020'.
          APPEND s_celldat TO t_celldat.
*          t_celldat = value #( ( fieldname = 'EXPAND' STYLE = CL_GUI_ALV_GRID=>MC_STYLE_HOTSpot )
*                               ( FIELDNAME = 'HEADER' style = '00000020' ) ).
          LOOP AT mt_keydat REFERENCE INTO _keydat.
            CLEAR s_celldat.
            CONCATENATE 'A_' _keydat->value INTO s_celldat-fieldname.
            s_celldat-style      = '00000020'.
            APPEND s_celldat TO t_celldat.

            CLEAR s_celldat.
            CONCATENATE 'P_' _keydat->value INTO s_celldat-fieldname.
            s_celldat-style      = '00000020'.
            APPEND s_celldat TO t_celldat.
*            INSERT value #(  fieldname = |a_{ _keydat->value }| style = '00000020' ) INTO TABLE t_celldat.

*            INSERT value #(  fieldname = |P_{ _keydat->value }| style = '00000020' ) INTO TABLE t_celldat.
          ENDLOOP.

          CLEAR s_celldat.
          LOOP AT t_celldat INTO s_celldat .
*            APPEND INITIAL LINE TO <fs_celltab> ASSIGNING <fs_cellline>.
            INSERT s_celldat INTO TABLE <fs_celltab>.
*            ASSIGN COMPONENT 'FIELDNAME' OF STRUCTURE <fs_cellline> TO <fieldname>.
*            ASSIGN COMPONENT 'STYLE' OF STRUCTURE <fs_cellline> TO <style>.
*            ASSIGN COMPONENT 'STYLE2' OF STRUCTURE <fs_cellline> TO <style2>.
*            ASSIGN COMPONENT 'STYLE3' OF STRUCTURE <fs_cellline> TO <style3>.
*            ASSIGN COMPONENT 'STYLE4' OF STRUCTURE <fs_cellline> TO <style4>.
*            ASSIGN COMPONENT 'MAXLEN' OF STRUCTURE <fs_cellline> TO <maxlen>.
*            <fieldname>  = s_celldat-fieldname.
*            <style>      = s_celldat-style .
*            <style2>     = s_celldat-style2 .
*            <style3>     = s_celldat-style3 .
*            <style4>     = s_celldat-style4 .
*            <maxlen>     = s_celldat-maxlen .
*            UNASSIGN: <fieldname>, <style>,<style2>,<style3>,<style4>,<maxlen>.
*            MOVE-CORRESPONDING s_celldat TO <fs_cellline>.
          ENDLOOP.
        ENDIF.
        ASSIGN COMPONENT 'CELLCOLOR' OF STRUCTURE <fs_alvdat> TO <fs_cellcolor>.
        IF sy-subrc IS INITIAL.
          FREE: t_celldat.
          CLEAR s_cellcolor.
          s_cellcolor-fname = 'EXPAND'.
          s_cellcolor-color-col = 3.
          s_cellcolor-color-int = 0.
          s_cellcolor-color-inv = 0.
          APPEND s_cellcolor TO  t_cellcolor.
          CLEAR s_cellcolor.
          s_cellcolor-fname = 'HEADER'.
          s_cellcolor-color-col = 3.
          s_cellcolor-color-int = 0.
          s_cellcolor-color-inv = 0.
          APPEND s_cellcolor TO  t_cellcolor.
*          t_cellcolor = value #( ( fname  = 'EXPAND' COLOR = VALUE #( col = 3 int = 0 inv = 0 ) )
*                                 ( FNAME  = 'HEADER' color = value #( col = 3 INT = 0 inv = 0 ) ) ).
          LOOP AT mt_keydat REFERENCE INTO _keydat.
            CLEAR s_cellcolor.
            CONCATENATE 'A_' _keydat->value INTO s_cellcolor-fname.
            s_cellcolor-color-col = 3.
            s_cellcolor-color-int = 0.
            s_cellcolor-color-inv = 0.
            APPEND s_cellcolor TO t_cellcolor.

            CLEAR s_cellcolor.
            CONCATENATE 'P_' _keydat->value INTO s_cellcolor-fname.
            s_cellcolor-color-col = 3.
            s_cellcolor-color-int = 0.
            s_cellcolor-color-inv = 0.
            APPEND s_cellcolor TO t_cellcolor.

*            INSERT value #(  fname = |a_{ _keydat->value }| color = value #( col = 3 int = 0 inv = 0 ) ) INTO TABLE t_cellcolor.
*            INSERT value #(  fname = |p_{ _keydat->value }| color = value #( col = 3 int = 0 inv = 0 ) ) INTO TABLE t_cellcolor.
          ENDLOOP.
          UNASSIGN <fs_cellline>.
          CLEAR s_cellcolor.
          LOOP AT t_cellcolor INTO s_cellcolor .
            APPEND INITIAL LINE TO <fs_cellcolor> ASSIGNING <fs_cellline>.
            MOVE-CORRESPONDING s_cellcolor TO <fs_cellline>.
          ENDLOOP.
*          <fs_cellcolor> = corresponding #( t_cellcolor ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    FIELD-SYMBOLS: <mt_coldat> TYPE STANDARD TABLE,
                   <coldat>    TYPE ANY,
                   <mt_alvdat> TYPE STANDARD TABLE,
                   <alvdat>    TYPE ANY.

    DATA: _helper TYPE REF TO yfin_lp_cl01,
    _alvdat TYPE yfin_lp_cl01=>ty_alvdat,
    lr_data TYPE REF TO data.

    DATA : it_details TYPE abap_compdescr_tab,
           wa_details LIKE LINE OF it_details,
           ref_descr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <value> TYPE ANY.

    CREATE OBJECT _helper.
    _helper->dynamic_alvdat_collect(
     EXPORTING
       iv_tmunit = p_tunit
       iv_begdat = s_keydat-low
       iv_enddat = s_keydat-high
     RECEIVING
     rv_alvdat = _alvdat ).

    ASSIGN mt_alvdat->* TO <mt_alvdat>.
    ASSIGN _alvdat-alvoutput->* TO <mt_coldat>.

    LOOP AT <mt_alvdat> ASSIGNING <alvdat>.
      ASSIGN COMPONENT 'PKIND' OF STRUCTURE <alvdat> TO <value>.
      CHECK <value> <> 'BNKBAKIYE'.

      CREATE DATA lr_data LIKE LINE OF <mt_coldat>.
      ASSIGN lr_data->* TO <coldat>.
      MOVE-CORRESPONDING <alvdat> TO <coldat>.
      COLLECT <coldat> INTO <mt_coldat>.
    ENDLOOP.

    FIELD-SYMBOLS: <waers>  TYPE ANY,
                   <expand> TYPE ANY,
                   <pkind>  TYPE ANY,
                   <cellcolor> TYPE lvc_t_scol,
                   <structure> TYPE any.

    TYPES: BEGIN OF ty_dyndat,
      bukrs TYPE bukrs,
      waers TYPE waers,
      pkind TYPE char10,
    END OF ty_dyndat.
    DATA: _temp01 TYPE ty_dyndat,
          _temp02 TYPE ty_dyndat,
          _insert  TYPE sy-tabix,
          _cellcolor TYPE lvc_s_scol.

    READ TABLE <mt_alvdat> ASSIGNING <structure> INDEX 1.
    IF <structure> IS ASSIGNED.
      ref_descr ?= cl_abap_typedescr=>describe_by_data( <structure> ).
      it_details[] = ref_descr->components[].
    ENDIF.

    LOOP AT <mt_coldat> ASSIGNING <coldat>.
      CLEAR: _temp01, _insert.
      MOVE-CORRESPONDING <coldat> TO _temp01.
      LOOP AT <mt_alvdat> ASSIGNING <alvdat>.
        _insert = sy-tabix.
        CLEAR: _temp02.
        MOVE-CORRESPONDING <alvdat> TO _temp02.
        IF _temp01-bukrs EQ _temp02-bukrs AND _temp01-waers EQ _temp02-waers AND _temp02-pkind EQ 'OUT'.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF _insert IS NOT INITIAL.
        ADD 1 TO _insert.
        INSERT INITIAL LINE INTO <mt_alvdat> ASSIGNING <alvdat> INDEX _insert.
        MOVE-CORRESPONDING <coldat> TO <alvdat>.
        ASSIGN COMPONENT 'WAERS' OF STRUCTURE <coldat> TO <waers>.
        ASSIGN COMPONENT 'HEADER' OF STRUCTURE <alvdat> TO <value>.
        IF sy-subrc IS INITIAL.
          CONCATENATE 'GİRİŞ ÇIKIŞ NET' <waers> INTO <value> SEPARATED BY '-'.
        ENDIF.
        ASSIGN COMPONENT 'EXPAND' OF STRUCTURE <alvdat> TO <expand>.
        IF <expand> IS ASSIGNED.
          <expand> = icon_sum.
        ENDIF.
        ASSIGN COMPONENT 'PKIND' OF STRUCTURE <alvdat> TO <pkind>.
        IF <pkind> IS ASSIGNED.
          <pkind> = 'SUM_A'.
        ENDIF.
        ASSIGN COMPONENT 'CELLCOLOR' OF STRUCTURE <alvdat> TO <cellcolor>.
        IF <cellcolor> IS ASSIGNED.
          LOOP AT it_details INTO wa_details WHERE name NE 'CELLCOLOR' AND name NE 'CELLTAB'.
            CLEAR _cellcolor.
            _cellcolor-fname = wa_details-name.
            _cellcolor-color-col = '5'.
            _cellcolor-color-int = 0.
            _cellcolor-color-inv = 0.
            APPEND _cellcolor TO <cellcolor>.
          ENDLOOP.
        ENDIF.

      ENDIF.
    ENDLOOP.

*--------------------------------------------------------------------*
* Gerçekleşecek Toplam Bakiye
*--------------------------------------------------------------------*
    FREE: <mt_coldat>.
    LOOP AT <mt_alvdat> ASSIGNING <alvdat>.
      ASSIGN COMPONENT 'EXPAND' OF STRUCTURE <alvdat> TO <value>.
      CHECK <value> <> icon_sum.
      CREATE DATA lr_data LIKE LINE OF <mt_coldat>.
      ASSIGN lr_data->* TO <coldat>.
      MOVE-CORRESPONDING <alvdat> TO <coldat>.
      COLLECT <coldat> INTO <mt_coldat>.
    ENDLOOP.

    FIELD-SYMBOLS: <bukrs> TYPE ANY.
*                   <pkind> TYPE ANY.

    TYPES: BEGIN OF ty_keydat,
      bukrs TYPE bukrs,
      waers TYPE waers,
      pkind TYPE char20,
      END OF ty_keydat.
    DATA: _keycol TYPE ty_keydat,
          _keyalv TYPE ty_keydat,
          _addsum TYPE flag.

    LOOP AT <mt_coldat> ASSIGNING <coldat>.
      CLEAR: _keycol, _insert.
      MOVE-CORRESPONDING <coldat> TO _keycol.

      CLEAR: _addsum.
      LOOP AT <mt_alvdat> ASSIGNING <alvdat>.
        CLEAR: _keyalv.
        _insert = sy-tabix.
        MOVE-CORRESPONDING <alvdat> TO _keyalv.
        IF _keyalv-bukrs = _keycol-bukrs AND _keyalv-waers = _keycol-waers.
          IF _keyalv-pkind EQ 'BNKBAKIYE'.
            _addsum = abap_true.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
      CHECK _addsum IS NOT INITIAL.
      CHECK _insert IS NOT INITIAL.
      ADD 1 TO _insert.
      INSERT INITIAL LINE INTO <mt_alvdat> ASSIGNING <alvdat> INDEX _insert.
*        APPEND INITIAL LINE TO <mt_alvdat> ASSIGNING <alvdat>.
      MOVE-CORRESPONDING <coldat> TO <alvdat>.
      ASSIGN COMPONENT 'WAERS' OF STRUCTURE <coldat> TO <waers>.
      ASSIGN COMPONENT 'HEADER' OF STRUCTURE <alvdat> TO <value>.
      IF sy-subrc IS INITIAL.
        CONCATENATE 'GERÇEKLEŞECEK BAKIYE' <waers> INTO <value> SEPARATED BY '-'.
      ENDIF.
      ASSIGN COMPONENT 'EXPAND' OF STRUCTURE <alvdat> TO <expand>.
      IF <expand> IS ASSIGNED.
        <expand> = icon_sum_red.
      ENDIF.
      ASSIGN COMPONENT 'PKIND' OF STRUCTURE <alvdat> TO <pkind>.
      IF <pkind> IS ASSIGNED.
        <pkind> = 'SUM_B'.
      ENDIF.
    ENDLOOP.

*->~aatan & karakas was here
    TYPES: BEGIN OF ty_totaldat,
      fieldname TYPE char20,
      sum_b     TYPE wrbtr,
      END OF ty_totaldat.

    TYPES: BEGIN OF ty_coldat,
      bukrs TYPE bukrs,
      waers TYPE waers,
      END OF ty_coldat,
      tt_coldat TYPE TABLE OF ty_coldat.

    DATA: t_totaldat TYPE STANDARD TABLE OF ty_totaldat WITH DEFAULT KEY,
          _totaldat TYPE ty_totaldat,
          r_totaldat TYPE REF TO ty_totaldat,
          r_beforedat TYPE REF TO ty_totaldat.

    DATA: t_coldat TYPE tt_coldat,
          _coldat  TYPE ty_coldat,
          _localdat TYPE ty_coldat.

    DATA: _total  TYPE wrbtr,
          _bindex TYPE sy-tabix,
          _success TYPE c.

    FIELD-SYMBOLS: <total>     TYPE ANY,
                   <sum_a>     TYPE ANY,
                   <sum_b>     TYPE ANY.

    READ TABLE <mt_alvdat> ASSIGNING <structure> INDEX 1.
    IF <structure> IS ASSIGNED.
      ref_descr ?= cl_abap_typedescr=>describe_by_data( <structure> ).
      it_details[] = ref_descr->components[].

      LOOP AT it_details INTO wa_details WHERE name(2) EQ 'P_'.
        APPEND INITIAL LINE TO t_totaldat REFERENCE INTO r_totaldat.
        r_totaldat->fieldname = wa_details-name.
      ENDLOOP.

      LOOP AT <mt_alvdat> ASSIGNING <alvdat>.
        CLEAR: _coldat.
        MOVE-CORRESPONDING <alvdat> TO _coldat.
        COLLECT _coldat INTO t_coldat.
      ENDLOOP.

      LOOP AT t_coldat INTO _coldat.

        FREE: _success, _total.
        LOOP AT <mt_alvdat> ASSIGNING <alvdat>.
          ASSIGN COMPONENT 'PKIND' OF STRUCTURE <alvdat> TO <sum_b>.
          CLEAR: _localdat.
          MOVE-CORRESPONDING <alvdat> TO _localdat.
          IF <sum_b> EQ 'SUM_B' AND _localdat EQ _coldat.
            _success = abap_true. EXIT.
          ENDIF.
        ENDLOOP.
        CHECK _success EQ abap_true.
        LOOP AT t_totaldat REFERENCE INTO r_totaldat.
          _bindex = sy-tabix - 1.
          READ TABLE t_totaldat REFERENCE INTO r_beforedat INDEX _bindex.
          IF sy-subrc IS NOT INITIAL.
            ASSIGN COMPONENT r_totaldat->fieldname OF STRUCTURE <alvdat> TO <total>.
            IF <total> IS ASSIGNED.
              _total = <total> = r_totaldat->sum_b = <total>.
            ENDIF.
          ELSE.
            FREE: _success.
            LOOP AT <mt_alvdat> ASSIGNING <alvdat>.
              ASSIGN COMPONENT 'PKIND' OF STRUCTURE <alvdat> TO <sum_a>.
              CLEAR: _localdat.
              MOVE-CORRESPONDING <alvdat> TO _localdat.
              IF <sum_a> EQ 'SUM_A' AND _localdat EQ _coldat.
                _success = abap_true. EXIT.
              ENDIF.
            ENDLOOP.
            CHECK _success EQ abap_true.
            ASSIGN COMPONENT r_totaldat->fieldname OF STRUCTURE <alvdat> TO <total>.
            IF <total> IS ASSIGNED.
              _total = _total + <total>.
              r_totaldat->sum_b = _total.
            ENDIF.
          ENDIF.
        ENDLOOP.

        LOOP AT <mt_alvdat> ASSIGNING <alvdat>.
          CLEAR: _keyalv.
          MOVE-CORRESPONDING <alvdat> TO _keyalv.
          IF _keyalv-bukrs = _coldat-bukrs AND _keyalv-waers = _coldat-waers AND _keyalv-pkind EQ 'SUM_B'.
            LOOP AT t_totaldat INTO _totaldat.
              ASSIGN COMPONENT _totaldat-fieldname OF STRUCTURE <alvdat> TO <sum_b>.
              IF <sum_b> IS ASSIGNED.
                <sum_b> = _totaldat-sum_b.
              ENDIF.
              ASSIGN COMPONENT 'CELLCOLOR' OF STRUCTURE <alvdat> TO <cellcolor>.
              IF <cellcolor> IS ASSIGNED.
                LOOP AT it_details INTO wa_details WHERE name NE 'CELLCOLOR' AND name NE 'CELLTAB'.
                  CLEAR _cellcolor.
                  _cellcolor-fname = wa_details-name.
                  _cellcolor-color-col = '6'.
                  _cellcolor-color-int = 0.
                  _cellcolor-color-inv = 0.
                  APPEND _cellcolor TO <cellcolor>.
                ENDLOOP.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDLOOP.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.                    "retrieve_dat
ENDCLASS.                    "lcl_mvc_model IMPLEMENTATION
*&---------------------------------------------------------------------*
*&  Class           LCL_MVC_VIEW           Implementation
*&---------------------------------------------------------------------*
CLASS lcl_mvc_view IMPLEMENTATION.

  METHOD constructor.
    mo_model = io_model.
    mo_controller = io_controller.
  ENDMETHOD.                    "constructor
  METHOD display_alvdat.
    DATA: is_layout TYPE lvc_s_layo,
          it_toolbar_ex TYPE ui_functions.
    IF mo_grid IS NOT BOUND.
*-&Create TOP-Document
      CREATE OBJECT mo_dyndoc_id
        EXPORTING
          style = 'ALV_GRID'.

      CREATE OBJECT mo_splitter
        EXPORTING
          parent  = cl_gui_custom_container=>screen0
          rows    = 2
          columns = 1.

      CALL METHOD mo_splitter->get_container
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = mo_parent_top.

      CALL METHOD mo_splitter->get_container
        EXPORTING
          row       = 2
          column    = 1
        RECEIVING
          container = mo_parent_grid.

      CALL METHOD mo_splitter->set_row_height
        EXPORTING
          id     = 1
          height = 12.

      CREATE OBJECT mo_grid
        EXPORTING
          i_parent          = mo_parent_grid
          i_lifetime        = cl_gui_alv_grid=>lifetime_dynpro
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

*      create_fieldcat(
*        EXPORTING
*          im_strname     = mc_strname
*        EXCEPTIONS
*          contains_error = 1
*          OTHERS         = 2 ).
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
*      ENDIF.

      update_fieldcat(
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      attach_handlers(
        EXPORTING
          im_grid = mo_grid
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      FIELD-SYMBOLS: <ft_alvdat> TYPE STANDARD TABLE.
      ASSIGN mo_model->mt_alvdat->* TO <ft_alvdat>.

      is_layout = set_layout_dat( ).
      it_toolbar_ex = set_exclude_dat( ).
      mo_grid->set_table_for_first_display(
      EXPORTING
        i_buffer_active               = space
        i_bypassing_buffer            = abap_true
        is_layout                     = is_layout
        it_toolbar_excluding          = it_toolbar_ex
      CHANGING
        it_fieldcatalog               = mt_fieldcat
        it_outtab                     = <ft_alvdat>
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
      OTHERS                          = 4 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      mo_grid->set_ready_for_input(
        EXPORTING
          i_ready_for_input = 1 ).

      CALL METHOD mo_dyndoc_id->initialize_document
        EXPORTING
          background_color = cl_dd_area=>col_textarea.

      CALL METHOD mo_grid->list_processing_events
        EXPORTING
          i_event_name = 'TOP_OF_PAGE'
          i_dyndoc_id  = mo_dyndoc_id.
    ELSE.
      refresh_alv(
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "display
  METHOD create_fieldcat.

    FREE: mt_fieldcat.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = im_strname
      CHANGING
        ct_fieldcat            = mt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.

  ENDMETHOD.                    "create_fieldcat
  METHOD update_fieldcat.
    DATA r_fieldcat TYPE REF TO lvc_s_fcat.
    DATA: _text(40) TYPE c.
    LOOP AT mt_fieldcat REFERENCE INTO r_fieldcat.
      CLEAR: _text.
      TRANSLATE r_fieldcat->fieldname TO UPPER CASE.
      CASE r_fieldcat->fieldname .
        WHEN 'EXPAND'.
          r_fieldcat->just = 'C'.
          r_fieldcat->key = abap_true.
        WHEN 'HEADER'.
          r_fieldcat->key = abap_true.
        WHEN 'BUKRS' OR 'PKIND' OR 'WAERS' OR 'LQPOS' OR 'EBENE'.
          r_fieldcat->no_out = abap_true.
      ENDCASE.
*      CASE r_fieldcat->fieldname(2) .
*        WHEN 'A_' OR 'P_'.
*          r_fieldcat->hotspot = 'X'.
*      ENDCASE.
      IF _text <> space.
        MOVE _text TO: r_fieldcat->scrtext_l, r_fieldcat->scrtext_m, r_fieldcat->scrtext_s, r_fieldcat->reptext.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "update_fieldcat
  METHOD set_layout_dat.


    rv_layoutdat-col_opt = abap_true.
    rv_layoutdat-cwidth_opt = abap_true.
    rv_layoutdat-zebra = abap_true.
    rv_layoutdat-stylefname = 'CELLTAB'.
    rv_layoutdat-ctab_fname = 'CELLCOLOR'.
*    rv_layoutdat = value LVC_S_LAYO( col_opt = abap_true CWIDTH_OPT = ABAP_TRUE zebra = abap_true STYLEFNAME = 'CELLTAB' ctab_fname = 'CELLCOLOR' ).

  ENDMETHOD.                    "set_layout_dat
  METHOD set_exclude_dat.
    DATA ls_exclude TYPE ui_func.

    DEFINE exclude_func.
      clear ls_exclude.
      ls_exclude = &1.
      append ls_exclude to rv_excludedat.
    END-OF-DEFINITION.

    exclude_func : cl_gui_alv_grid=>mc_fc_f4 ,       cl_gui_alv_grid=>mc_fc_view_lotus,   cl_gui_alv_grid=>mc_fc_current_variant ,
                   cl_gui_alv_grid=>mc_fc_auf ,      cl_gui_alv_grid=>mc_fc_pc_file ,     cl_gui_alv_grid=>mc_fc_call_report ,
                   cl_gui_alv_grid=>mc_fc_sum ,      cl_gui_alv_grid=>mc_fc_refresh ,     cl_gui_alv_grid=>mc_fc_fix_columns ,
                   cl_gui_alv_grid=>mc_mb_sum ,      cl_gui_alv_grid=>mc_mb_variant ,     cl_gui_alv_grid=>mc_fc_to_rep_tree ,
                   cl_gui_alv_grid=>mc_fc_find ,     cl_gui_alv_grid=>mc_fc_call_abc ,    cl_gui_alv_grid=>mc_fc_back_classic ,
                   cl_gui_alv_grid=>mc_fc_help ,     cl_gui_alv_grid=>mc_fc_maximum ,     cl_gui_alv_grid=>mc_fc_call_crbatch ,
                   cl_gui_alv_grid=>mc_fc_info ,     cl_gui_alv_grid=>mc_fc_loc_copy ,    cl_gui_alv_grid=>mc_fc_deselect_all ,
                   cl_gui_alv_grid=>mc_fc_send ,     cl_gui_alv_grid=>mc_fc_loc_undo ,    cl_gui_alv_grid=>mc_fc_load_variant ,
                   cl_gui_alv_grid=>mc_fc_sort ,     cl_gui_alv_grid=>mc_fc_sort_asc ,    cl_gui_alv_grid=>mc_fc_loc_copy_row ,
                   cl_gui_alv_grid=>mc_mb_view ,     cl_gui_alv_grid=>mc_fc_sort_dsc ,    cl_gui_alv_grid=>mc_fc_loc_move_row ,
                   cl_gui_alv_grid=>mc_fc_check ,    cl_gui_alv_grid=>mc_fc_call_more ,   cl_gui_alv_grid=>mc_fc_save_variant ,
                   cl_gui_alv_grid=>mc_fc_count ,    cl_gui_alv_grid=>mc_fc_call_xint ,   cl_gui_alv_grid=>mc_fc_view_crystal ,
                   cl_gui_alv_grid=>mc_fc_graph ,    cl_gui_alv_grid=>mc_fc_data_save ,   cl_gui_alv_grid=>mc_fc_col_invisible ,
                   cl_gui_alv_grid=>mc_fc_print ,    cl_gui_alv_grid=>mc_fc_expcrdata ,   cl_gui_alv_grid=>mc_fc_delete_filter ,
                   cl_gui_alv_grid=>mc_fc_views ,    cl_gui_alv_grid=>mc_fc_find_more ,   cl_gui_alv_grid=>mc_fc_unfix_columns ,
                   cl_gui_alv_grid=>mc_mb_paste ,    cl_gui_alv_grid=>mc_fc_loc_paste ,   cl_gui_alv_grid=>mc_fc_variant_admin ,
                   cl_gui_alv_grid=>mc_fc_detail ,   cl_gui_alv_grid=>mc_fc_separator ,   cl_gui_alv_grid=>mc_fc_call_lineitems ,
                   cl_gui_alv_grid=>mc_fc_expmdb ,   cl_gui_alv_grid=>mc_fc_to_office ,   cl_gui_alv_grid=>mc_fc_loc_append_row ,
                   cl_gui_alv_grid=>mc_fc_extend ,   cl_gui_alv_grid=>mc_fc_view_grid ,   cl_gui_alv_grid=>mc_fc_loc_delete_row ,
                   cl_gui_alv_grid=>mc_fc_filter ,   cl_gui_alv_grid=>mc_fc_call_chain ,  cl_gui_alv_grid=>mc_fc_loc_insert_row ,
                   cl_gui_alv_grid=>mc_fc_reprep ,   cl_gui_alv_grid=>mc_fc_call_crweb ,  cl_gui_alv_grid=>mc_fc_word_processor ,
                   cl_gui_alv_grid=>mc_fc_subtot ,   cl_gui_alv_grid=>mc_fc_expcrdesig ,  cl_gui_alv_grid=>mc_fc_call_xml_export ,
                   cl_gui_alv_grid=>mc_mb_filter ,   cl_gui_alv_grid=>mc_fc_print_back ,  cl_gui_alv_grid=>mc_fc_call_master_data ,
                   cl_gui_alv_grid=>mc_mb_subtot ,   cl_gui_alv_grid=>mc_fc_print_prev ,  cl_gui_alv_grid=>mc_fc_maintain_variant ,
                   cl_gui_alv_grid=>mc_fc_average ,  cl_gui_alv_grid=>mc_fc_select_all ,  cl_gui_alv_grid=>mc_fc_loc_paste_new_row ,
                   cl_gui_alv_grid=>mc_fc_loc_cut ,  cl_gui_alv_grid=>mc_fc_view_excel ,  cl_gui_alv_grid=>mc_fc_url_copy_to_clipboard ,
                   cl_gui_alv_grid=>mc_fc_minimum ,  cl_gui_alv_grid=>mc_fc_expcrtempl ,  cl_gui_alv_grid=>mc_fc_col_optimize .
  ENDMETHOD.                    "set_exclude_dat
  METHOD attach_handlers.

    CALL METHOD im_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    SET HANDLER mo_controller->handle_hotspot_click FOR im_grid.
    SET HANDLER mo_controller->handle_toolbar_set   FOR im_grid.
    SET HANDLER mo_controller->handle_user_command  FOR im_grid.
    SET HANDLER mo_controller->handle_data_finished FOR im_grid.
    SET HANDLER mo_controller->handle_top_of_page   FOR im_grid.
  ENDMETHOD.                    "attach_handlers
  METHOD refresh_alv.
    DATA is_stable TYPE lvc_s_stbl.
    is_stable-row = abap_true.
    is_stable-col = abap_true.

    mo_grid->refresh_table_display(
      EXPORTING
        is_stable      = is_stable
        i_soft_refresh = abap_true
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.

  ENDMETHOD.                    "refresh_alv
ENDCLASS.                    "lcl_mvc_view IMPLEMENTATION
*&---------------------------------------------------------------------*
*&  Class           LCL_MVC_CONTROLLER           Implementation
*&---------------------------------------------------------------------*
CLASS lcl_mvc_controller IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.                    "constructor
  METHOD instantiate_app.

    DATA: lo_object TYPE REF TO object.
    DATA: lo_controller TYPE REF TO lcl_mvc_controller.

    CREATE OBJECT lo_controller.
*    ro_controller = new LCL_MVC_CONTROLLER( ).
    ro_controller = lo_controller.

    FREE: lo_object.
    CREATE OBJECT lo_object TYPE (iv_model).
    IF lo_object IS BOUND.
      mo_model ?= lo_object.
      ro_controller->mo_model ?= mo_model.
    ENDIF.

    FREE: lo_object.
    CREATE OBJECT lo_object
      TYPE
        (iv_view)
      EXPORTING
        io_model      = ro_controller->mo_model
        io_controller = me.
    IF lo_object IS BOUND.
      mo_view ?= lo_object.
      ro_controller->mo_view ?= mo_view.
    ENDIF.

  ENDMETHOD.                    "instantiate_app
  METHOD initialization.
    DATA ls_keydat LIKE LINE OF s_keydat.
    FREE: s_keydat[].
    ls_keydat-sign = 'I'.
    ls_keydat-option = 'BT'.
    ls_keydat-low = sy-datum.
    ls_keydat-high = sy-datum.
    APPEND ls_keydat TO s_keydat[].
*    s_keydat[] = value #( sign = 'I' OPTION = 'BT' ( low = sy-datum HIGH = SY-DATUM ) ).
    listbox_build( ).

  ENDMETHOD.                    "initialization
  METHOD at_selection_screen.
    user_commmand_selscr(
      CHANGING
          cv_ucomm = sy-ucomm ).
  ENDMETHOD.                    "at_selection_screen
  METHOD user_commmand_selscr.
    DATA: _helper TYPE REF TO yfin_lp_cl01,
          _alvdat TYPE yfin_lp_cl01=>ty_alvdat.
    CREATE OBJECT _helper.
    CASE cv_ucomm.
      WHEN 'ONLI'.
        _helper->dynamic_alvdat(
          EXPORTING
            iv_tmunit      = p_tunit
            iv_begdat      = s_keydat-low
            iv_enddat      = s_keydat-high
          RECEIVING
            rv_alvdat      = _alvdat
          EXCEPTIONS
            contains_error = 1
            OTHERS         = 2 ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          mo_model->mt_alvdat ?= _alvdat-alvoutput.
          mo_model->mt_keydat = _alvdat-keydate.
          mo_view->mt_fieldcat = _alvdat-fieldcat.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "user_commmand_selscr
  METHOD start_of_selection.

    mo_model->retrieve_dat(
      EXPORTING
        im_bukrs       = p_bukrs
        im_tunit       = p_tunit
        im_keyda       = s_keydat[]
        im_ldays       = p_ldays
      EXCEPTIONS
        contains_error = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    CALL SCREEN 100.

  ENDMETHOD.                    "start_of_selection
  METHOD listbox_build.

    DATA: _values TYPE vrm_values,
          _value TYPE vrm_value,
          _id     TYPE vrm_id.

    FREE: _values, _id.
    _id = 'P_TUNIT'.
    _value-key = 'D'.
    _value-text = 'Günlük'.
    APPEND _value TO _values.
    _value-key = 'W'.
    _value-text = 'Haftalık'.
    APPEND _value TO _values.
    _value-key = 'M'.
    _value-text = 'Aylık'.
    APPEND _value TO _values.
    _value-key = 'Y'.
    _value-text = 'Yıllık'.
    APPEND _value TO _values.

*    _values = value #( base _VALUES ( key = 'D' TEXT = 'Günlük' )
*                                    ( KEY = 'W' text = 'Haftalık' )
*                                    ( KEY = 'M' text = 'Aylık' )
*                                    ( KEY = 'Y' text = 'Yıllık' ) ).
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = _id
        values = _values.

  ENDMETHOD.                    "listbox_build
  METHOD alv_session.

    IF NOT mo_model->mt_alvdat IS INITIAL.
      mo_view->display_alvdat(
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "alv_session
  METHOD display_linedat.
    CALL FUNCTION 'YFIN_LP_FM01'
      EXPORTING
        iv_strname_tab1 = 'YFIN_LP_S01'
        iv_strname_tab2 = 'YFIN_LP_S02'
        iv_rejectbtn    = iv_rejectbtn
      IMPORTING
        ev_action       = ev_action
        ev_idenr        = ev_idenr
      TABLES
        t_alvdat_tab1   = iv_actualdat
        t_alvdat_tab2   = iv_manueldat.
  ENDMETHOD.                    "display_linedat
  METHOD reject_manueldat.

    TYPES: BEGIN OF ty_keycol,
             bukrs  TYPE yfin_lp_t01-bukrs,
             pkind  TYPE yfin_lp_t01-pkind,
             waers  TYPE yfin_lp_t01-waers,
             expand TYPE text40,
             lqpos  TYPE yfin_lp_t02-lqpos,
             ebene  TYPE yfin_lp_t02-ebene,
           END OF ty_keycol.

    DATA: _keycol TYPE ty_keycol,
          _total  TYPE wrbtr,
          _amount  TYPE wrbtr,
          wa_docdat  TYPE yfin_lp_s01,
          _appdat TYPE REF TO lcl_mvc_model=>ty_appdat,
          _where  TYPE string.

    FIELD-SYMBOLS: <ft_alvdat>  TYPE STANDARD TABLE,
                   <fs_alvdat> TYPE ANY,
                   <amount> TYPE ANY,
                   <ft_linedat> TYPE STANDARD TABLE,
                   <fs_linedat> TYPE ANY,
                   <fs_lineval> TYPE ANY,
                   <ft_basedat> TYPE STANDARD TABLE,
                   <fs_basedat> TYPE ANY,
                   <fs_baseval> TYPE ANY.

    FIELD-SYMBOLS : <bukrs> TYPE ANY,
                   <pkind> TYPE ANY,
                   <waers> TYPE ANY,
                   <lqpos> TYPE ANY.
    READ TABLE mo_model->mt_appdat REFERENCE INTO _appdat WITH KEY bukrs = iv_keydat-bukrs waers = iv_keydat-waers pkind = iv_keydat-pkind.
    IF sy-subrc IS INITIAL.
      GET TIME.
      UPDATE yfin_lp_t03 SET xdele = abap_true erdat = sy-datum erzet = sy-uzeit ernam = sy-uname
        WHERE bukrs = iv_keydat-bukrs
          AND pkind = iv_keydat-pkind
          AND twaer = iv_keydat-waers
          AND lqpos = iv_keydat-lqpos
          AND lqday = iv_column_id-fieldname+2(27)
          AND ltype = iv_keydat-ltype.
      IF sy-subrc IS INITIAL.
        DELETE _appdat->root_dat-_logdat
          WHERE bukrs = iv_keydat-bukrs
            AND pkind = iv_keydat-pkind
            AND twaer = iv_keydat-waers
            AND lqpos = iv_keydat-lqpos
            AND lqday = iv_column_id-fieldname+2(27)
            AND ltype = iv_keydat-ltype.

        ASSIGN mo_model->mt_alvdat->* TO <ft_alvdat>.
        READ TABLE <ft_alvdat> ASSIGNING <fs_alvdat> INDEX iv_row_no-row_id .
*        ASSIGN <ft_alvdat>[ iv_row_no-row_id ] TO field-symbol(<fs_alvdat>).
        IF <fs_alvdat> IS ASSIGNED.
          ASSIGN COMPONENT iv_column_id-fieldname OF STRUCTURE <fs_alvdat> TO <amount>.
          LOOP AT _appdat->root_dat-_docdat INTO wa_docdat WHERE bukrs = iv_keydat-bukrs
                                                             AND twaer = iv_keydat-waers
                                                             AND lqpos = iv_keydat-lqpos
                                                             AND ltype = iv_keydat-ltype
                                                             AND lqday IN iv_keydat-lqday[].
            <amount> = <amount> + wa_docdat-wrbtr.
          ENDLOOP.
*          <amount> = reduce WRBTR( init VAL type WRBTR for WA_DOCDAT in _APPDAT->ROOT_DAT-_DOCDAT
*                      where ( BUKRS = IV_KEYDAT-BUKRS and TWAER = IV_KEYDAT-WAERS and LQPOS = IV_KEYDAT-LQPOS and LTYPE = IV_KEYDAT-LTYPE and LQDAY in IV_KEYDAT-LQDAY[] ) NEXT val = val + wa_docdat-wrbtr ).
        ELSE.
          CLEAR: _where.

          LOOP AT <ft_alvdat> ASSIGNING <fs_alvdat>.
            ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_alvdat> TO <bukrs>.
            ASSIGN COMPONENT 'PKIND' OF STRUCTURE <fs_alvdat> TO <pkind>.
            ASSIGN COMPONENT 'WAERS' OF STRUCTURE <fs_alvdat> TO <waers>.
            ASSIGN COMPONENT 'LQPOS' OF STRUCTURE <fs_alvdat> TO <lqpos>.
            IF <bukrs> = iv_keydat-bukrs AND
               <pkind> = iv_keydat-pkind AND
               <waers> = iv_keydat-waers AND
               <lqpos> = iv_keydat-lqpos.
              LOOP AT _appdat->root_dat-_docdat INTO wa_docdat WHERE bukrs = iv_keydat-bukrs
                                                                 AND twaer = iv_keydat-waers
                                                                 AND lqpos = iv_keydat-lqpos
                                                                 AND ltype = iv_keydat-ltype
                                                                 AND lqday IN iv_keydat-lqday[].
                <amount> = <amount> + wa_docdat-wrbtr.
              ENDLOOP.
            ENDIF.
          ENDLOOP.


          IF NOT sy-subrc IS INITIAL.
            LOOP AT _appdat->root_dat-_docdat INTO wa_docdat WHERE bukrs = iv_keydat-bukrs
                                                               AND twaer = iv_keydat-waers
                                                               AND lqpos = iv_keydat-lqpos
                                                               AND ltype = iv_keydat-ltype
                                                               AND lqday IN iv_keydat-lqday[].
              _amount = _amount + wa_docdat-wrbtr.
            ENDLOOP.
            ASSIGN _amount TO <amount>.
          ENDIF.
        ENDIF.

        UNASSIGN: <ft_linedat>.
        ASSIGN _appdat->line_tab->* TO <ft_linedat>.
        LOOP AT <ft_linedat> ASSIGNING <fs_linedat>.
          ASSIGN COMPONENT iv_column_id-fieldname OF STRUCTURE <fs_linedat> TO <fs_lineval>.
          IF <fs_lineval> IS ASSIGNED.
            CLEAR: _keycol.
            MOVE-CORRESPONDING <fs_linedat> TO _keycol.
            IF _keycol-bukrs EQ iv_keydat-bukrs AND _keycol-pkind = iv_keydat-pkind AND _keycol-waers = iv_keydat-waers AND _keycol-lqpos = iv_keydat-lqpos.
              <fs_lineval> = <amount>.
            ENDIF.
            _total = _total + <fs_lineval>.
          ENDIF.
        ENDLOOP.

        LOOP AT <ft_alvdat> ASSIGNING <fs_alvdat>.
          CLEAR: _keycol.
          MOVE-CORRESPONDING <fs_alvdat> TO _keycol.
          CHECK _keycol-bukrs = iv_keydat-bukrs AND _keycol-pkind = iv_keydat-pkind AND _keycol-waers = iv_keydat-waers AND _keycol-expand <> space.
          UNASSIGN: <amount>.
          ASSIGN COMPONENT iv_column_id-fieldname OF STRUCTURE <fs_alvdat> TO <amount>.
          IF <amount> IS ASSIGNED.
            <amount> = _total. EXIT.
          ENDIF.
        ENDLOOP.

        UNASSIGN: <ft_basedat>.
        ASSIGN _appdat->base_tab->* TO <ft_basedat>.
        READ TABLE <ft_basedat> ASSIGNING <fs_basedat> INDEX 1.
        IF sy-subrc IS INITIAL.
          ASSIGN COMPONENT iv_column_id-fieldname OF STRUCTURE <fs_basedat> TO <fs_baseval>.
          IF <fs_baseval> IS ASSIGNED.
            <fs_baseval> = _total.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE e011(yfin_lp) RAISING contains_error.
      ENDIF.
    ELSE.
      MESSAGE e011(yfin_lp) RAISING contains_error.
    ENDIF.

  ENDMETHOD.                    "reject_manueldat

  METHOD delete_manueldat.
    TYPES: BEGIN OF ty_keycol,
         bukrs  TYPE yfin_lp_t01-bukrs,
         pkind  TYPE yfin_lp_t01-pkind,
         waers  TYPE yfin_lp_t01-waers,
         expand TYPE text40,
         lqpos  TYPE yfin_lp_t02-lqpos,
         ebene  TYPE yfin_lp_t02-ebene,
       END OF ty_keycol.

    DATA: _keycol TYPE ty_keycol,
          _total  TYPE wrbtr,
          _amount  TYPE wrbtr,
          wa_docdat  TYPE yfin_lp_s01,
          _appdat TYPE REF TO lcl_mvc_model=>ty_appdat,
          _where  TYPE string.


    FIELD-SYMBOLS: <ft_alvdat>  TYPE STANDARD TABLE,
                   <fs_alvdat> TYPE ANY,
                   <amount> TYPE ANY,
                   <ft_linedat> TYPE STANDARD TABLE,
                   <fs_linedat> TYPE ANY,
                   <fs_lineval> TYPE ANY,
                   <ft_basedat> TYPE STANDARD TABLE,
                   <fs_basedat> TYPE ANY,
                   <fs_baseval> TYPE ANY.

    FIELD-SYMBOLS : <bukrs> TYPE ANY,
                    <pkind> TYPE ANY,
                    <waers> TYPE ANY,
                    <lqpos> TYPE ANY.

    READ TABLE mo_model->mt_appdat REFERENCE INTO _appdat WITH KEY bukrs = iv_keydat-bukrs waers = iv_keydat-waers pkind = iv_keydat-pkind.
    IF sy-subrc IS INITIAL.
      DELETE _appdat->root_dat-_docdat
            WHERE bukrs = iv_keydat-bukrs
              AND idenr = iv_idenr
              AND twaer = iv_keydat-waers
              AND lqpos = iv_keydat-lqpos
              AND lqday = iv_column_id-fieldname+2(27)
              AND ltype = iv_keydat-ltype.

      ASSIGN mo_model->mt_alvdat->* TO <ft_alvdat>.
      READ TABLE <ft_alvdat> ASSIGNING <fs_alvdat> INDEX iv_row_no-row_id .
*        ASSIGN <ft_alvdat>[ iv_row_no-row_id ] TO field-symbol(<fs_alvdat>).
      IF <fs_alvdat> IS ASSIGNED.
        ASSIGN COMPONENT iv_column_id-fieldname OF STRUCTURE <fs_alvdat> TO <amount>.
        CLEAR <amount>.
        LOOP AT _appdat->root_dat-_docdat INTO wa_docdat WHERE bukrs = iv_keydat-bukrs
                                                           AND twaer = iv_keydat-waers
                                                           AND lqpos = iv_keydat-lqpos
                                                           AND ltype = iv_keydat-ltype
                                                           AND lqday IN iv_keydat-lqday[].
          <amount> = <amount> + wa_docdat-wrbtr.
        ENDLOOP.

      ELSE.
        CLEAR: _where.

        LOOP AT <ft_alvdat> ASSIGNING <fs_alvdat>.
          ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_alvdat> TO <bukrs>.
          ASSIGN COMPONENT 'PKIND' OF STRUCTURE <fs_alvdat> TO <pkind>.
          ASSIGN COMPONENT 'WAERS' OF STRUCTURE <fs_alvdat> TO <waers>.
          ASSIGN COMPONENT 'LQPOS' OF STRUCTURE <fs_alvdat> TO <lqpos>.
          IF <bukrs> = iv_keydat-bukrs AND
             <pkind> = iv_keydat-pkind AND
             <waers> = iv_keydat-waers AND
             <lqpos> = iv_keydat-lqpos.
            CLEAR <amount>.
            LOOP AT _appdat->root_dat-_docdat INTO wa_docdat WHERE bukrs = iv_keydat-bukrs
                                                               AND twaer = iv_keydat-waers
                                                               AND lqpos = iv_keydat-lqpos
                                                               AND ltype = iv_keydat-ltype
                                                               AND lqday IN iv_keydat-lqday[].
              <amount> = <amount> + wa_docdat-wrbtr.
            ENDLOOP.
          ENDIF.
        ENDLOOP.


        IF NOT sy-subrc IS INITIAL.
          LOOP AT _appdat->root_dat-_docdat INTO wa_docdat WHERE bukrs = iv_keydat-bukrs
                                                             AND twaer = iv_keydat-waers
                                                             AND lqpos = iv_keydat-lqpos
                                                             AND ltype = iv_keydat-ltype
                                                             AND lqday IN iv_keydat-lqday[].
            _amount = _amount + wa_docdat-wrbtr.
          ENDLOOP.
          ASSIGN _amount TO <amount>.
        ENDIF.
      ENDIF.

      UNASSIGN: <ft_linedat>.
      ASSIGN _appdat->line_tab->* TO <ft_linedat>.
      LOOP AT <ft_linedat> ASSIGNING <fs_linedat>.
        ASSIGN COMPONENT iv_column_id-fieldname OF STRUCTURE <fs_linedat> TO <fs_lineval>.
        IF <fs_lineval> IS ASSIGNED.
          CLEAR: _keycol.
          MOVE-CORRESPONDING <fs_linedat> TO _keycol.
          IF _keycol-bukrs EQ iv_keydat-bukrs AND _keycol-pkind = iv_keydat-pkind AND _keycol-waers = iv_keydat-waers AND _keycol-lqpos = iv_keydat-lqpos.
            <fs_lineval> = <amount>.
          ENDIF.
          _total = _total + <fs_lineval>.
        ENDIF.
      ENDLOOP.

      LOOP AT <ft_alvdat> ASSIGNING <fs_alvdat>.
        CLEAR: _keycol.
        MOVE-CORRESPONDING <fs_alvdat> TO _keycol.
        CHECK _keycol-bukrs = iv_keydat-bukrs AND _keycol-pkind = iv_keydat-pkind AND _keycol-waers = iv_keydat-waers AND _keycol-expand <> space.
        UNASSIGN: <amount>.
        ASSIGN COMPONENT iv_column_id-fieldname OF STRUCTURE <fs_alvdat> TO <amount>.
        IF <amount> IS ASSIGNED.
          <amount> = _total. EXIT.
        ENDIF.
      ENDLOOP.

      UNASSIGN: <ft_basedat>.
      ASSIGN _appdat->base_tab->* TO <ft_basedat>.
      READ TABLE <ft_basedat> ASSIGNING <fs_basedat> INDEX 1.
      IF sy-subrc IS INITIAL.
        ASSIGN COMPONENT iv_column_id-fieldname OF STRUCTURE <fs_basedat> TO <fs_baseval>.
        IF <fs_baseval> IS ASSIGNED.
          <fs_baseval> = _total.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE e011(yfin_lp) RAISING contains_error.
    ENDIF.



  ENDMETHOD.                    "delete_manueldat

  METHOD save_logdat.
    DATA _logdat TYPE REF TO yfin_lp_t03.

    LOOP AT mo_model->mt_appdat INTO mo_model->mv_appdat.
      LOOP AT mo_model->mv_appdat-root_dat-_logdat REFERENCE INTO _logdat.
        UPDATE yfin_lp_t03 SET xdele = abap_true erdat = sy-datum erzet = sy-uzeit ernam = sy-uname
          WHERE bukrs = _logdat->bukrs
            AND pkind = _logdat->pkind
            AND twaer = _logdat->twaer
            AND lqpos = _logdat->lqpos
            AND ebene = _logdat->ebene
            AND lqday = _logdat->lqday
            AND ltype = _logdat->ltype.
        COMMIT WORK.

        MODIFY yfin_lp_t03 FROM _logdat->*.
        COMMIT WORK.

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.                    "save_logdat
  METHOD get_icon.
    DATA: v_name TYPE char4,
          v_info TYPE text40.

    IF iv_type EQ 'E'.
      v_name = icon_expand.
      v_info = 'Expand Details'(e02).
    ELSEIF iv_type EQ 'C'.
      v_name = icon_collapse.
      v_info = 'Collapse Details'(c02).
    ENDIF.

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = v_name
        info                  = v_info
        add_stdinf            = ' '
      IMPORTING
        RESULT                = rv_icon
      EXCEPTIONS
        icon_not_found        = 0
        outputfield_too_short = 1
        OTHERS                = 2.

  ENDMETHOD.                    "get_icon
  METHOD get_long_date.

    DATA: t_month_names TYPE TABLE OF t247,
          r_month_names TYPE REF TO t247,
          l_daytxt      TYPE hrvsched-daytxt,
          l_year(4)     TYPE c,
          l_month(2)    TYPE c,
          l_day(2)      TYPE c.

    FREE: t_month_names.
    CALL FUNCTION 'MONTH_NAMES_GET'
      EXPORTING
        language    = sy-langu
      TABLES
        month_names = t_month_names.

    l_year = im_dat+0(4).
    l_month = im_dat+4(2).
    l_day = im_dat+6(2).

    CLEAR: l_daytxt.
    CALL FUNCTION 'RH_GET_DATE_DAYNAME'
      EXPORTING
        langu               = sy-langu
        date                = im_dat
      IMPORTING
        daytxt              = l_daytxt
      EXCEPTIONS
        no_langu            = 1
        no_date             = 2
        no_daytxt_for_langu = 3
        invalid_date        = 4
        OTHERS              = 5.

    READ TABLE t_month_names REFERENCE INTO r_month_names INDEX l_month.
    IF sy-subrc IS INITIAL.
      CONCATENATE l_day r_month_names->ltx l_year l_daytxt INTO r_dattxt SEPARATED BY space.
    ENDIF.

  ENDMETHOD.              "get_long_date
  METHOD _expand_all_alvdat.

    DATA: t_celldat TYPE TABLE OF  lvc_s_styl,
          s_celldat TYPE lvc_s_styl,
          _tabix TYPE i,
          lv_add_subrows_index TYPE i,
          _keydat TYPE REF TO yfin_lp_cl01=>ty_valdat.

    FIELD-SYMBOLS: <ft_alvdat> TYPE STANDARD TABLE,
                   <alvdat> TYPE ANY,
                   <fs_alvdat> TYPE ANY,
                   <fs_celltab>     TYPE ANY,
                   <expand>     TYPE ANY,
                   <bukrs>     TYPE ANY,
                   <pkind>     TYPE ANY,
                   <waers>     TYPE ANY.

    FIELD-SYMBOLS: <ft_linedat> TYPE STANDARD TABLE,
                   <fs_linedat> TYPE ANY.
    ASSIGN mo_model->mt_alvdat->* TO <ft_alvdat>.
    LOOP AT <ft_alvdat> ASSIGNING <fs_alvdat>.
      _tabix = sy-tabix.
      ASSIGN COMPONENT 'EXPAND' OF STRUCTURE <fs_alvdat> TO <expand>.
      IF <expand>(3) EQ icon_expand(3).
        rv_refresh = abap_true.
        <expand> = get_icon( iv_type = 'C' ).
        ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_alvdat> TO <bukrs>.
        ASSIGN COMPONENT 'PKIND' OF STRUCTURE <fs_alvdat> TO <pkind>.
        ASSIGN COMPONENT 'WAERS' OF STRUCTURE <fs_alvdat> TO <waers>.
        FREE: mo_model->mv_appdat.
        READ TABLE mo_model->mt_appdat INTO mo_model->mv_appdat WITH KEY bukrs = <bukrs> pkind = <pkind> waers = <waers>.
        IF sy-subrc IS INITIAL.
          lv_add_subrows_index = _tabix + 1.
          ASSIGN mo_model->mv_appdat-line_tab->* TO <ft_linedat>.
          LOOP AT <ft_linedat> ASSIGNING <fs_linedat>.
            INSERT INITIAL LINE INTO <ft_alvdat> ASSIGNING <alvdat> INDEX lv_add_subrows_index.
            MOVE-CORRESPONDING <fs_linedat> TO <alvdat>.
            ASSIGN COMPONENT 'CELLTAB' OF STRUCTURE <alvdat> TO <fs_celltab>.
            IF sy-subrc IS INITIAL.
              FREE: t_celldat.
              LOOP AT mo_model->mt_keydat REFERENCE INTO _keydat.
                CLEAR s_celldat.
                CONCATENATE 'A_' _keydat->value INTO s_celldat-fieldname.
                s_celldat-style = cl_gui_alv_grid=>mc_style_hotspot.
                APPEND s_celldat TO t_celldat.
                CLEAR s_celldat.
                CONCATENATE 'P_' _keydat->value INTO s_celldat-fieldname.
                s_celldat-style = cl_gui_alv_grid=>mc_style_hotspot.
                APPEND s_celldat TO t_celldat.
              ENDLOOP.
              <fs_celltab> = t_celldat.
*              MOVE-CORRESPONDING t_celldat TO <fs_celltab>.
            ENDIF.
            ADD 1 TO lv_add_subrows_index.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.              "_expand_all_alvdat
  METHOD _collapse_all_alvdat.

    TYPES:
      BEGIN OF ty_deldat,
        bukrs  TYPE yfin_lp_t01-bukrs,
        pkind  TYPE yfin_lp_t01-pkind,
        waers  TYPE yfin_lp_t01-waers,
        expand TYPE text40,
      END OF ty_deldat.

    DATA: _deldat   TYPE ty_deldat,
          _index    TYPE i.

    FIELD-SYMBOLS: <ft_alvdat> TYPE STANDARD TABLE,
                   <fs_deldat> TYPE ANY,
                   <fs_alvdat> TYPE ANY,
                   <expand>     TYPE ANY,
                   <bukrs>     TYPE ANY,
                   <pkind>     TYPE ANY,
                   <waers>     TYPE ANY.

    ASSIGN mo_model->mt_alvdat->* TO <ft_alvdat>.
    LOOP AT <ft_alvdat> ASSIGNING <fs_alvdat>.
      ASSIGN COMPONENT 'EXPAND' OF STRUCTURE <fs_alvdat> TO <expand>.
      IF <expand>(3) EQ icon_collapse(3).
        rv_refresh = abap_true.
        <expand> = get_icon( iv_type = 'E' ).
        ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_alvdat> TO <bukrs>.
        ASSIGN COMPONENT 'PKIND' OF STRUCTURE <fs_alvdat> TO <pkind>.
        ASSIGN COMPONENT 'WAERS' OF STRUCTURE <fs_alvdat> TO <waers>.
        LOOP AT <ft_alvdat> ASSIGNING <fs_deldat>.
          _index = sy-tabix.
          MOVE-CORRESPONDING <fs_deldat> TO _deldat.
          IF _deldat-bukrs EQ <bukrs> AND _deldat-pkind EQ <pkind> AND _deldat-waers EQ <waers> AND _deldat-expand IS INITIAL.
            DELETE <ft_alvdat> INDEX _index.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.              "_collapse_all_alvdat
  METHOD _editable_alvdat.

    TYPES:
      BEGIN OF ty_keyvalue,
        bukrs TYPE bukrs,
        pkind TYPE yfin_lp_e001,
        waers TYPE waers,
        lqpos TYPE flqpos,
        ebene TYPE fdlev,
      END OF ty_keyvalue.

    DATA: t_celldat TYPE TABLE OF lvc_s_styl,
          s_celldat TYPE lvc_s_styl,
          _linedat TYPE  REF TO yfin_lp_t02,
          _keydat TYPE REF TO yfin_lp_cl01=>ty_valdat,
          _keyvalue TYPE ty_keyvalue.

    FIELD-SYMBOLS: <ft_alvdat> TYPE STANDARD TABLE,
                   <fs_alvdat> TYPE ANY,
                   <celltab>   TYPE ANY,
                   <fs_celltab>     TYPE STANDARD TABLE,
                   <expand>     TYPE ANY,
                   <bukrs>      TYPE ANY,
                   <pkind>      TYPE ANY,
                   <waers>      TYPE ANY.

    ASSIGN mo_model->mt_alvdat->* TO <ft_alvdat>.
    LOOP AT <ft_alvdat> ASSIGNING <fs_alvdat>.
      ASSIGN COMPONENT 'EXPAND' OF STRUCTURE <fs_alvdat> TO <expand>.
      IF <expand> IS INITIAL.
        ASSIGN COMPONENT 'CELLTAB' OF STRUCTURE <fs_alvdat> TO <celltab>.
        IF sy-subrc IS INITIAL.
          FREE: t_celldat.
          LOOP AT mo_model->mt_keydat REFERENCE INTO _keydat.
            CLEAR: _keyvalue.

            MOVE-CORRESPONDING <fs_alvdat> TO _keyvalue.
            READ TABLE _controller->mo_model->mt_linedat REFERENCE INTO _linedat WITH KEY bukrs = _keyvalue-bukrs waers = _keyvalue-waers lqpos = _keyvalue-lqpos ebene = _keyvalue-ebene.
            IF sy-subrc IS INITIAL.
              CASE _linedat->edita.
                WHEN space.
                  CLEAR s_celldat.
                  CONCATENATE 'A_' _keydat->value INTO s_celldat-fieldname.
                  s_celldat-style =  cl_gui_alv_grid=>mc_style_enabled.
                  APPEND s_celldat TO t_celldat.
                  CLEAR s_celldat.
                  CONCATENATE 'P_' _keydat->value INTO s_celldat-fieldname.
                  s_celldat-style =  cl_gui_alv_grid=>mc_style_enabled.
                  APPEND s_celldat TO t_celldat.
                WHEN 'A'.
                  CLEAR s_celldat.
                  CONCATENATE 'A_' _keydat->value INTO s_celldat-fieldname.
                  s_celldat-style =  cl_gui_alv_grid=>mc_style_enabled.
                  APPEND s_celldat TO t_celldat.
                WHEN 'P'.
                  CLEAR s_celldat.
                  CONCATENATE 'P_' _keydat->value INTO s_celldat-fieldname.
                  s_celldat-style =  cl_gui_alv_grid=>mc_style_enabled.
                  APPEND s_celldat TO t_celldat.
              ENDCASE.
            ENDIF.
          ENDLOOP.
          SORT t_celldat BY fieldname ASCENDING.
          <celltab> = t_celldat.
*          APPEND LINES OF t_celldat TO <celltab>.
*          MOVE t_celldat TO <celltab>.
          rv_refresh = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.              "_editable_alvdat
  METHOD handle_hotspot_click.

    FIELD-SYMBOLS: <ft_alvdat> TYPE STANDARD TABLE,
                   <fs_deldat> TYPE ANY,
                   <alvdat> TYPE ANY,
                   <fs_alvdat> TYPE ANY,
                   <expand>     TYPE ANY,
                   <fs_celltab>     TYPE SORTED TABLE ,
                   <bukrs>     TYPE ANY,
                   <pkind>     TYPE ANY,
                   <lqpos>     TYPE ANY,
                   <ebene>     TYPE ANY,
                   <waers>     TYPE ANY.

    TYPES: BEGIN OF ty_delcolmn,
             bukrs  TYPE yfin_lp_t01-bukrs,
             pkind  TYPE yfin_lp_t01-pkind,
             waers  TYPE yfin_lp_t01-waers,
             expand TYPE text40,
           END OF ty_delcolmn.

    TYPES: BEGIN OF ty_keycolmn,
             bukrs  TYPE yfin_lp_t01-bukrs,
             pkind  TYPE yfin_lp_t01-pkind,
             waers  TYPE yfin_lp_t01-waers,
             expand TYPE text40,
             lqpos  TYPE yfin_lp_t02-lqpos,
             ebene  TYPE yfin_lp_t02-ebene,
           END OF ty_keycolmn.

    DATA: _delcolmn TYPE ty_delcolmn,
          _keycolmn TYPE ty_keycolmn,
           _keydat    TYPE REF TO yfin_lp_cl01=>ty_valdat,
          _total    TYPE wrbtr,
          _appdat      TYPE REF TO lcl_mvc_model=>ty_appdat,
          _index    TYPE i,
          lv_add_subrows_index    TYPE i,
          t_celldat TYPE TABLE OF lvc_s_styl,
          _srcdat TYPE REF TO yfin_lp_t06,
          s_celldat TYPE lvc_s_styl,
          lt_index_rows	TYPE lvc_t_row,
          ls_index_rows TYPE lvc_s_row,
          lt_row_no  TYPE lvc_t_roid.

    DATA: s_actualdat TYPE yfin_lp_s01,
          s_manueldat TYPE yfin_lp_s02.

    DATA: ls_docdat TYPE yfin_lp_s01,
          s_keydat TYPE ty_keydat,
          s_rowid TYPE lvc_s_roid,
          _linedat TYPE yfin_lp_t02,
          _action TYPE sy-ucomm,
          _idenr  TYPE idenr,
          ls_logdat TYPE yfin_lp_t03.

    FIELD-SYMBOLS: <ft_linedat> TYPE STANDARD TABLE,
                   <fs_linedat> TYPE ANY.
    CASE e_column_id.
      WHEN 'EXPAND'.
        ASSIGN mo_model->mt_alvdat->* TO <ft_alvdat>.
        READ TABLE <ft_alvdat> ASSIGNING <fs_alvdat> INDEX es_row_no-row_id.

        ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_alvdat> TO <bukrs>.
        ASSIGN COMPONENT 'PKIND' OF STRUCTURE <fs_alvdat> TO <pkind>.
        ASSIGN COMPONENT 'WAERS' OF STRUCTURE <fs_alvdat> TO <waers>.
        ASSIGN COMPONENT e_column_id OF STRUCTURE <fs_alvdat> TO <expand>.
        IF <expand>(3) EQ icon_expand(3).
          <expand> = get_icon( iv_type = 'C' ).
          FREE: mo_model->mv_appdat.
          READ TABLE mo_model->mt_appdat INTO mo_model->mv_appdat WITH KEY bukrs = <bukrs> pkind = <pkind> waers = <waers>.
          IF sy-subrc IS INITIAL.
            lv_add_subrows_index = es_row_no-row_id + 1.
            ASSIGN mo_model->mv_appdat-line_tab->* TO <ft_linedat>.
            LOOP AT <ft_linedat> ASSIGNING <fs_linedat>.
              INSERT INITIAL LINE INTO <ft_alvdat> ASSIGNING <alvdat> INDEX lv_add_subrows_index.
              MOVE-CORRESPONDING <fs_linedat> TO <alvdat>.
              ASSIGN COMPONENT 'CELLTAB' OF STRUCTURE <alvdat> TO <fs_celltab>.
              IF sy-subrc IS INITIAL.
                FREE: t_celldat.
                LOOP AT mo_model->mt_keydat REFERENCE INTO _keydat.
                  CLEAR s_celldat.
                  CONCATENATE 'A_' _keydat->value INTO s_celldat-fieldname.
                  s_celldat-style =  cl_gui_alv_grid=>mc_style_hotspot.
                  APPEND s_celldat TO t_celldat.
                  CLEAR s_celldat.
                  CONCATENATE 'P_' _keydat->value INTO s_celldat-fieldname.
                  s_celldat-style =  cl_gui_alv_grid=>mc_style_hotspot.
                  APPEND s_celldat TO t_celldat.
                ENDLOOP.
                CLEAR s_celldat.
                SORT t_celldat .
                APPEND LINES OF t_celldat TO <fs_celltab>.
*                MOVE-CORRESPONDING t_celldat TO <fs_celltab>.
              ENDIF.
              ADD 1 TO lv_add_subrows_index.
            ENDLOOP.
          ENDIF.
        ELSE.
          <expand> = get_icon( iv_type = 'E' ).
          LOOP AT <ft_alvdat> ASSIGNING <fs_deldat>.
            _index = sy-tabix.
            MOVE-CORRESPONDING <fs_deldat> TO _delcolmn.
            IF _delcolmn-bukrs EQ <bukrs> AND _delcolmn-pkind EQ <pkind> AND _delcolmn-waers EQ <waers> AND _delcolmn-expand IS INITIAL.
              DELETE <ft_alvdat> INDEX _index.
            ENDIF.
          ENDLOOP.
        ENDIF.
      WHEN OTHERS.
        CASE e_column_id(2).
          WHEN 'A_'.
            ASSIGN mo_model->mt_alvdat->* TO <ft_alvdat>.
            READ TABLE <ft_alvdat> ASSIGNING <fs_alvdat> INDEX  es_row_no-row_id.
            ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_alvdat> TO <bukrs>.
            ASSIGN COMPONENT 'WAERS' OF STRUCTURE <fs_alvdat> TO <waers>.
            ASSIGN COMPONENT 'PKIND' OF STRUCTURE <fs_alvdat> TO <pkind>.
            ASSIGN COMPONENT 'LQPOS' OF STRUCTURE <fs_alvdat> TO <lqpos>.
            ASSIGN COMPONENT 'EBENE' OF STRUCTURE <fs_alvdat> TO <ebene>.
            READ TABLE mo_model->mt_keydat REFERENCE INTO _keydat WITH KEY value = e_column_id-fieldname+2(27).
            IF sy-subrc IS INITIAL.
              READ TABLE mo_model->mt_appdat REFERENCE INTO _appdat WITH KEY bukrs = <bukrs> waers = <waers> pkind = <pkind>.
              IF sy-subrc IS INITIAL.
                FREE: mo_model->mt_actualdat, mo_model->mt_manueldat.

                LOOP AT _appdat->root_dat-_docdat INTO ls_docdat WHERE ltype = 'A' AND lqday IN _keydat->range AND lqpos = <lqpos>.
                  MOVE-CORRESPONDING ls_docdat TO s_actualdat.
                  APPEND s_actualdat TO mo_model->mt_actualdat.
                ENDLOOP.

                LOOP AT _appdat->root_dat-_logdat INTO ls_logdat WHERE ltype = 'A' AND lqday IN _keydat->range AND lqpos = <lqpos>.
                  MOVE-CORRESPONDING ls_logdat TO s_manueldat.
                  APPEND s_manueldat TO mo_model->mt_manueldat.
                ENDLOOP.

                CLEAR: mo_model->mv_rejectbtn.
                IF p_tunit EQ 'D'.
                  READ TABLE mo_model->mt_linedat INTO _linedat WITH KEY bukrs = <bukrs> pkind = <pkind> waers = <waers> lqpos = <lqpos>.
                  IF mo_model->mt_manueldat[] IS NOT INITIAL AND ( _linedat-edita <> 'P' AND _linedat-edita <> 'N' ).
                    mo_model->mv_rejectbtn = abap_true.
                  ENDIF.
                ENDIF.

                me->display_linedat(
                  EXPORTING
                    iv_actualdat = mo_model->mt_actualdat
                    iv_manueldat = mo_model->mt_manueldat
                    iv_rejectbtn = mo_model->mv_rejectbtn
                  IMPORTING
                    ev_action = _action
                    ev_idenr  = _idenr
                  EXCEPTIONS
                    contains_error = 1
                    OTHERS         = 2 ).
                IF sy-subrc <> 0.
                  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                ELSE.
                  CASE _action.
                    WHEN '&RECALL'.
                      CLEAR s_keydat.
                      s_keydat-bukrs = <bukrs>.
                      s_keydat-pkind = <pkind>.
                      s_keydat-waers = <waers>.
                      s_keydat-lqpos = <lqpos>.
                      s_keydat-ebene = <ebene>.
                      s_keydat-lqday = _keydat->range.
                      s_keydat-ltype = 'A'.
                      reject_manueldat(
                        EXPORTING
                          iv_keydat    = s_keydat
                          iv_column_id = e_column_id
                          iv_row_no    = es_row_no
                       EXCEPTIONS
                         contains_error = 1
                         OTHERS         = 2 ).
                      IF sy-subrc <> 0.
                        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                      ELSE.
*->begin of -aatan {
                        READ TABLE mo_model->mt_sourcedat REFERENCE INTO _srcdat WITH KEY bukrs = <bukrs> pkind = <pkind> waers = <waers> lqpos = <lqpos> field_src =  e_column_id(2).
                        IF sy-subrc IS INITIAL.
                          CLEAR s_keydat.
                          s_keydat-bukrs = _srcdat->bukrs.
                          s_keydat-pkind = _srcdat->pkind_src.
                          s_keydat-waers = _srcdat->waers_src.
                          s_keydat-lqpos = _srcdat->lqpos_src.
                          s_keydat-ebene = _srcdat->ebene_src.
                          s_keydat-lqday = _keydat->range.
                          s_keydat-ltype = 'A'.
                          s_rowid-row_id = 0.
                          reject_manueldat(
                            EXPORTING
                              iv_keydat    = s_keydat
                              iv_column_id = e_column_id
                              iv_row_no    = s_rowid
                            EXCEPTIONS
                              contains_error = 1
                              OTHERS         = 2 ).
                          IF sy-subrc <> 0.
                            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                          ENDIF.
                        ENDIF.
*<-end of -aatan }
                      ENDIF.
                    WHEN '&DELETE'.
                      CLEAR s_keydat.
                      s_keydat-bukrs = <bukrs>.
                      s_keydat-pkind = <pkind>.
                      s_keydat-waers = <waers>.
                      s_keydat-lqpos = <lqpos>.
                      s_keydat-ebene = <ebene>.
                      s_keydat-lqday = _keydat->range.
                      s_keydat-ltype = 'A'.
                      delete_manueldat(
                        EXPORTING
                          iv_keydat    = s_keydat
                          iv_idenr     = _idenr
                          iv_column_id = e_column_id
                          iv_row_no    = es_row_no
                       EXCEPTIONS
                         contains_error = 1
                         OTHERS         = 2 ).
                      IF sy-subrc <> 0.
                        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                      ELSE.
*->begin of -aatan {
                        READ TABLE mo_model->mt_sourcedat REFERENCE INTO _srcdat WITH KEY bukrs = <bukrs> pkind = <pkind> waers = <waers> lqpos = <lqpos> field_src =  e_column_id(2) .
                        IF sy-subrc IS INITIAL.

                          CLEAR s_keydat.
                          s_keydat-bukrs = <bukrs>.
                          s_keydat-pkind = <pkind>.
                          s_keydat-waers = <waers>.
                          s_keydat-lqpos = <lqpos>.
                          s_keydat-ebene = <ebene>.
                          s_keydat-lqday = _keydat->range.
                          s_keydat-ltype = 'A'.
                          s_rowid-row_id = 0.
                          delete_manueldat(
                            EXPORTING
                              iv_keydat    = s_keydat
                              iv_idenr     = _idenr
                              iv_column_id = e_column_id
                              iv_row_no    = s_rowid
                            EXCEPTIONS
                              contains_error = 1
                              OTHERS         = 2 ).
                          IF sy-subrc <> 0.
                            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                          ENDIF.
                        ENDIF.
*<-end of -aatan }
                      ENDIF.
                  ENDCASE.
                ENDIF.
              ENDIF.
            ENDIF.
          WHEN 'P_'.
            ASSIGN mo_model->mt_alvdat->* TO <ft_alvdat>.
            READ TABLE <ft_alvdat> ASSIGNING <fs_alvdat> INDEX  es_row_no-row_id.
            ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_alvdat> TO <bukrs>.
            ASSIGN COMPONENT 'WAERS' OF STRUCTURE <fs_alvdat> TO <waers>.
            ASSIGN COMPONENT 'PKIND' OF STRUCTURE <fs_alvdat> TO <pkind>.
            ASSIGN COMPONENT 'LQPOS' OF STRUCTURE <fs_alvdat> TO <lqpos>.
            ASSIGN COMPONENT 'EBENE' OF STRUCTURE <fs_alvdat> TO <ebene>.
            READ TABLE mo_model->mt_keydat REFERENCE INTO _keydat WITH KEY value = e_column_id-fieldname+2(27).
            IF sy-subrc IS INITIAL.

              READ TABLE mo_model->mt_appdat REFERENCE INTO _appdat WITH KEY bukrs = <bukrs> waers = <waers> pkind = <pkind>.
              IF sy-subrc IS INITIAL.
                FREE: mo_model->mt_actualdat, mo_model->mt_manueldat.
                LOOP AT _appdat->root_dat-_docdat INTO ls_docdat WHERE ltype = 'P' AND lqday IN _keydat->range AND ebene = <ebene>.
                  MOVE-CORRESPONDING ls_docdat TO s_actualdat.
                  APPEND s_actualdat TO mo_model->mt_actualdat.
                ENDLOOP.

                LOOP AT _appdat->root_dat-_logdat INTO ls_logdat WHERE ltype = 'P' AND lqday IN _keydat->range AND ebene = <ebene>.
                  MOVE-CORRESPONDING ls_logdat TO s_manueldat.
                  APPEND s_manueldat TO mo_model->mt_manueldat.
                ENDLOOP.

                CLEAR: mo_model->mv_rejectbtn.
                IF p_tunit EQ 'D'.
                  READ TABLE mo_model->mt_linedat INTO _linedat WITH KEY bukrs = <bukrs> pkind = <pkind> waers = <waers> lqpos = <lqpos>.
                  IF mo_model->mt_manueldat[] IS NOT INITIAL AND ( _linedat-edita <> 'A' AND _linedat-edita <> 'N' ).
                    mo_model->mv_rejectbtn = abap_true.
                  ENDIF.
                ENDIF.

                me->display_linedat(
                  EXPORTING
                    iv_actualdat = mo_model->mt_actualdat
                    iv_manueldat = mo_model->mt_manueldat
                    iv_rejectbtn = mo_model->mv_rejectbtn
                  IMPORTING
                    ev_action = _action
                    ev_idenr  = _idenr
                  EXCEPTIONS
                    contains_error = 1
                    OTHERS         = 2 ).
                IF sy-subrc <> 0.
                  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                ELSE.
                  CASE _action.
                    WHEN '&RECALL'.
                      CLEAR s_keydat.
                      s_keydat-bukrs = <bukrs>.
                      s_keydat-pkind = <pkind>.
                      s_keydat-waers = <waers>.
                      s_keydat-lqpos = <lqpos>.
                      s_keydat-ebene = <ebene>.
                      s_keydat-lqday = _keydat->range.
                      s_keydat-ltype = 'P'.
                      reject_manueldat(
                        EXPORTING
                          iv_keydat    = s_keydat
                          iv_column_id = e_column_id
                          iv_row_no    = es_row_no
                       EXCEPTIONS
                         contains_error = 1
                         OTHERS         = 2 ).
                      IF sy-subrc <> 0.
                        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                      ELSE.
*->begin of -aatan {
                        READ TABLE mo_model->mt_sourcedat REFERENCE INTO _srcdat WITH KEY bukrs = <bukrs> pkind = <pkind> waers = <waers> lqpos = <lqpos> field_src =  e_column_id(2) .
                        IF sy-subrc IS INITIAL.

                          CLEAR s_keydat.
                          s_keydat-bukrs = <bukrs>.
                          s_keydat-pkind = <pkind>.
                          s_keydat-waers = <waers>.
                          s_keydat-lqpos = <lqpos>.
                          s_keydat-ebene = <ebene>.
                          s_keydat-lqday = _keydat->range.
                          s_keydat-ltype = 'P'.
                          s_rowid-row_id = 0.
                          reject_manueldat(
                            EXPORTING
                              iv_keydat    = s_keydat
                              iv_column_id = e_column_id
                              iv_row_no    = s_rowid
                            EXCEPTIONS
                              contains_error = 1
                              OTHERS         = 2 ).
                          IF sy-subrc <> 0.
                            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                          ENDIF.
                        ENDIF.
*<-end of -aatan }
                      ENDIF.
                    WHEN '&DELETE'.
                      CLEAR s_keydat.
                      s_keydat-bukrs = <bukrs>.
                      s_keydat-pkind = <pkind>.
                      s_keydat-waers = <waers>.
                      s_keydat-lqpos = <lqpos>.
                      s_keydat-ebene = <ebene>.
                      s_keydat-lqday = _keydat->range.
                      s_keydat-ltype = 'P'.
                      delete_manueldat(
                        EXPORTING
                          iv_keydat    = s_keydat
                          iv_idenr     = _idenr
                          iv_column_id = e_column_id
                          iv_row_no    = es_row_no
                       EXCEPTIONS
                         contains_error = 1
                         OTHERS         = 2 ).
                      IF sy-subrc <> 0.
                        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                      ELSE.
*->begin of -aatan {
                        READ TABLE mo_model->mt_sourcedat REFERENCE INTO _srcdat WITH KEY bukrs = <bukrs> pkind = <pkind> waers = <waers> lqpos = <lqpos> field_src =  e_column_id(2) .
                        IF sy-subrc IS INITIAL.

                          CLEAR s_keydat.
                          s_keydat-bukrs = <bukrs>.
                          s_keydat-pkind = <pkind>.
                          s_keydat-waers = <waers>.
                          s_keydat-lqpos = <lqpos>.
                          s_keydat-ebene = <ebene>.
                          s_keydat-lqday = _keydat->range.
                          s_keydat-ltype = 'P'.
                          s_rowid-row_id = 0.
                          delete_manueldat(
                            EXPORTING
                              iv_keydat    = s_keydat
                              iv_idenr     = _idenr
                              iv_column_id = e_column_id
                              iv_row_no    = s_rowid
                            EXCEPTIONS
                              contains_error = 1
                              OTHERS         = 2 ).
                          IF sy-subrc <> 0.
                            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                          ENDIF.
                        ENDIF.
*<-end of -aatan }
                      ENDIF.

                  ENDCASE.
                ENDIF.
              ENDIF.
            ENDIF.
        ENDCASE.
    ENDCASE.

    DATA ls_stable TYPE lvc_s_stbl.

    ls_stable-row = abap_true.
    ls_stable-col = abap_true.

    mo_view->mo_grid->refresh_table_display(
      EXPORTING
        is_stable      = ls_stable
        i_soft_refresh = abap_true
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.              "handle_hotspot_click
  METHOD handle_toolbar_set.
    DATA: ls_toolbar  TYPE stb_button.

    DELETE e_object->mt_toolbar WHERE function = '&LOCAL&COPY_ROW'
                                   OR function = '&LOCAL&APPEND'
                                   OR function = '&LOCAL&INSERT_ROW'
                                   OR function = '&LOCAL&DELETE_ROW'
                                   OR function = '&LOCAL&CUT'
                                   OR function = '&LOCAL&COPY'
                                   OR function = '&LOCAL&PASTE'.
    ls_toolbar-function = 'EXPALL'.
    ls_toolbar-icon = icon_expand_all.
    ls_toolbar-quickinfo = 'Tümünü'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    ls_toolbar-function = 'COLALL'.
    ls_toolbar-icon = icon_collapse_all.
    ls_toolbar-quickinfo = 'Tümünü Kapat'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    IF p_tunit EQ 'D'.
      ls_toolbar-function = '&EDITDAT'.
      ls_toolbar-icon = icon_budget_update.
      ls_toolbar-text = 'Güncelle'.
      ls_toolbar-quickinfo = 'Güncelleme işlemleri'.
      APPEND ls_toolbar TO e_object->mt_toolbar.
    ENDIF.

  ENDMETHOD.                    "handle_toolbar
  METHOD handle_user_command.
    DATA ls_stable TYPE lvc_s_stbl.

    ls_stable-row = abap_true.
    ls_stable-col = abap_true.
    CASE e_ucomm.
      WHEN 'EXPALL'.
        IF _expand_all_alvdat( ) EQ abap_true.
          mo_view->mo_grid->refresh_table_display(
            EXPORTING
              is_stable      = ls_stable
              i_soft_refresh = abap_true
            EXCEPTIONS
              finished       = 1
              OTHERS         = 2 ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
      WHEN 'COLALL'.
        IF _collapse_all_alvdat( ) EQ abap_true.
          mo_view->mo_grid->refresh_table_display(
            EXPORTING
              is_stable      = ls_stable
              i_soft_refresh = abap_true
            EXCEPTIONS
              finished       = 1
              OTHERS         = 2 ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.

      WHEN '&EDITDAT'.
        IF _editable_alvdat( ) EQ abap_true.
          mo_view->mo_grid->refresh_table_display(
            EXPORTING
              is_stable      = ls_stable
              i_soft_refresh = abap_true
            EXCEPTIONS
              finished       = 1
              OTHERS         = 2 ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "handle_user_command
  METHOD handle_data_finished.

    TYPES: BEGIN OF ty_keydat,
             bukrs  TYPE yfin_lp_t01-bukrs,
             pkind  TYPE yfin_lp_t01-pkind,
             waers  TYPE yfin_lp_t01-waers,
             lqpos  TYPE yfin_lp_t02-lqpos,
             ebene  TYPE yfin_lp_t02-ebene,
             expand TYPE text40,
           END OF ty_keydat.

    DATA: _keydat TYPE ty_keydat,
          _cntdat TYPE ty_keydat,
          _appdat  TYPE REF TO lcl_mvc_model=>ty_appdat,
          _srcdat  TYPE REF TO yfin_lp_t06,
          l_change  TYPE flag,
          _wrbtr  TYPE wrbtr,
          _index  TYPE i,
          _ltype  TYPE char1,
          _lqday  TYPE char8,
          _logdat  TYPE yfin_lp_t03,
          _prsid  TYPE timestamp,
          _where  TYPE string.

    DATA ls_stable TYPE lvc_s_stbl.

    ls_stable-row = abap_true.
    ls_stable-col = abap_true.

    FIELD-SYMBOLS: <ft_alvdat> TYPE STANDARD TABLE,
                   <fs_sumdat> TYPE ANY,
                   <amount> TYPE ANY,
                   <value> TYPE ANY,
                   <bukrs> TYPE ANY,
                   <pkind> TYPE ANY,
                   <waers> TYPE ANY,
                   <expand> TYPE ANY,
                   <lqpos> TYPE ANY,
                   <fs_alvdat> TYPE ANY,
                   <fs_good_cells> TYPE lvc_s_modi.

    ASSIGN mo_model->mt_alvdat->* TO <ft_alvdat>.
    LOOP AT et_good_cells ASSIGNING <fs_good_cells>.

      READ TABLE <ft_alvdat> ASSIGNING <fs_alvdat> INDEX <fs_good_cells>-row_id.
      IF sy-subrc IS INITIAL.
        CLEAR: _keydat.
        MOVE-CORRESPONDING <fs_alvdat> TO _keydat.
        LOOP AT <ft_alvdat> ASSIGNING <fs_alvdat>.
          _index = sy-tabix.
          MOVE-CORRESPONDING <fs_alvdat> TO _cntdat.
          IF _cntdat-bukrs = _keydat-bukrs AND _cntdat-pkind = _keydat-pkind AND _cntdat-waers = _keydat-waers AND _cntdat-expand IS NOT INITIAL.
            EXIT.
          ENDIF.
        ENDLOOP.
        READ TABLE <ft_alvdat> ASSIGNING <fs_alvdat> INDEX _index.
        IF sy-subrc IS INITIAL.
          ASSIGN COMPONENT <fs_good_cells>-fieldname OF STRUCTURE <fs_alvdat> TO <value>.
          IF sy-subrc IS INITIAL.
            l_change = abap_true.
            CLEAR: _wrbtr.
            LOOP AT <ft_alvdat> ASSIGNING <fs_sumdat>.
              ASSIGN COMPONENT <fs_good_cells>-fieldname OF STRUCTURE <fs_sumdat> TO <amount>.
              IF sy-subrc IS INITIAL.
                MOVE-CORRESPONDING <fs_sumdat> TO _cntdat.
                IF _cntdat-bukrs = _keydat-bukrs AND _cntdat-pkind = _keydat-pkind AND _cntdat-waers = _keydat-waers AND _cntdat-expand IS INITIAL.
                  _wrbtr = _wrbtr + <amount>.
                ENDIF.
              ENDIF.
            ENDLOOP.
            <value> = _wrbtr.

            READ TABLE mo_model->mt_appdat REFERENCE INTO _appdat WITH KEY bukrs = _keydat-bukrs pkind = _keydat-pkind waers = _keydat-waers.
            IF sy-subrc IS INITIAL.
              FIELD-SYMBOLS: <ft_linedat> TYPE STANDARD TABLE,
                             <fs_linedat> TYPE ANY.
              DATA: r_linedat TYPE REF TO data.

              ASSIGN _appdat->line_tab->* TO <ft_linedat>.
              IF <ft_linedat> IS ASSIGNED.
                FREE: <ft_linedat>.
                CREATE DATA r_linedat LIKE LINE OF <ft_linedat>. ASSIGN r_linedat->* TO <fs_linedat>.
                LOOP AT <ft_alvdat> ASSIGNING <fs_alvdat>.
                  MOVE-CORRESPONDING <fs_alvdat> TO _cntdat.
                  IF _cntdat-bukrs = _keydat-bukrs AND _cntdat-pkind = _keydat-pkind AND _cntdat-waers = _keydat-waers AND _cntdat-expand IS INITIAL.
                    APPEND INITIAL LINE TO <ft_linedat> ASSIGNING <fs_linedat>.

                    MOVE-CORRESPONDING <fs_alvdat> TO <fs_linedat>.
                  ENDIF.
                ENDLOOP.
              ENDIF.
            ENDIF.

*->begin of -aatan {
            READ TABLE mo_model->mt_sourcedat REFERENCE INTO _srcdat WITH KEY bukrs = _keydat-bukrs pkind = _keydat-pkind waers = _keydat-waers lqpos = _keydat-lqpos field_src =  <fs_good_cells>-fieldname(2).
            IF sy-subrc IS INITIAL.
              CLEAR: _where.
              LOOP AT <ft_alvdat> ASSIGNING <fs_alvdat>.
                ASSIGN COMPONENT 'bukrs' OF STRUCTURE <fs_alvdat> TO <bukrs>.
                ASSIGN COMPONENT 'pkind' OF STRUCTURE <fs_alvdat> TO <pkind>.
                ASSIGN COMPONENT 'waers' OF STRUCTURE <fs_alvdat> TO <waers>.
                ASSIGN COMPONENT 'lqpos' OF STRUCTURE <fs_alvdat> TO <lqpos>.
                IF <bukrs> = _srcdat->bukrs AND
                   <pkind> = _srcdat->pkind_src AND
                   <waers> = _srcdat->waers_src AND
                   <lqpos> = _srcdat->lqpos_src.
                  ASSIGN COMPONENT <fs_good_cells>-fieldname OF STRUCTURE <fs_alvdat> TO <value>.
                  IF sy-subrc IS INITIAL.
                    <value> = <fs_good_cells>-value.
                  ENDIF.
                ENDIF.
              ENDLOOP.

              READ TABLE mo_model->mt_appdat REFERENCE INTO _appdat WITH KEY bukrs = _srcdat->bukrs pkind = _srcdat->pkind_src waers = _srcdat->waers_src.
              IF sy-subrc IS INITIAL.
                ASSIGN _appdat->line_tab->* TO <ft_linedat>.
                IF <ft_linedat> IS ASSIGNED.
                  LOOP AT <ft_linedat> ASSIGNING <fs_linedat>.
                    ASSIGN COMPONENT 'bukrs' OF STRUCTURE <fs_linedat> TO <bukrs>.
                    ASSIGN COMPONENT 'pkind' OF STRUCTURE <fs_linedat> TO <pkind>.
                    ASSIGN COMPONENT 'waers' OF STRUCTURE <fs_linedat> TO <waers>.
                    ASSIGN COMPONENT 'lqpos' OF STRUCTURE <fs_linedat> TO <lqpos>.
                    IF <bukrs> = _srcdat->bukrs AND
                       <pkind> = _srcdat->pkind_src AND
                       <waers> = _srcdat->waers_src AND
                       <lqpos> = _srcdat->lqpos_src.
                      ASSIGN COMPONENT <fs_good_cells>-fieldname OF STRUCTURE <fs_alvdat> TO <value>.
                      IF sy-subrc IS INITIAL.
                        <value> = <fs_good_cells>-value.
                      ENDIF.
                    ENDIF.
                  ENDLOOP.

                  IF sy-subrc IS INITIAL.
                    CLEAR: _where.

                    LOOP AT <ft_alvdat> ASSIGNING <fs_alvdat>.
                      ASSIGN COMPONENT 'bukrs' OF STRUCTURE <fs_linedat> TO <bukrs>.
                      ASSIGN COMPONENT 'pkind' OF STRUCTURE <fs_linedat> TO <pkind>.
                      ASSIGN COMPONENT 'waers' OF STRUCTURE <fs_linedat> TO <waers>.
                      ASSIGN COMPONENT 'expand' OF STRUCTURE <fs_linedat> TO <expand>.
                      IF <bukrs> = _srcdat->bukrs AND
                         <pkind> = _srcdat->pkind_src AND
                         <waers> = _srcdat->waers_src AND
                         <expand> <> space.
                        ASSIGN COMPONENT <fs_good_cells>-fieldname OF STRUCTURE <fs_alvdat> TO <value>.
                        IF sy-subrc IS INITIAL.
                          CLEAR: _wrbtr.
                          LOOP AT <ft_linedat> ASSIGNING <fs_linedat>.
                            ASSIGN COMPONENT <fs_good_cells>-fieldname OF STRUCTURE <fs_linedat> TO <amount>.
                            IF sy-subrc IS INITIAL.
                              _wrbtr = _wrbtr + <amount>.
                            ENDIF.
                          ENDLOOP.
                          <value> = _wrbtr.
                        ENDIF.

                      ENDIF.
                    ENDLOOP.
                  ENDIF.
                ENDIF.
              ENDIF.
*--------------------------------------------------------------------*
*-& Insert Log Dat->
*--------------------------------------------------------------------*
              READ TABLE mo_model->mt_appdat REFERENCE INTO _appdat WITH KEY bukrs = _srcdat->bukrs pkind = _srcdat->pkind_src waers = _srcdat->waers_src.
              IF sy-subrc IS INITIAL.
                IF <fs_good_cells>-fieldname(2) EQ 'A_'.
                  _ltype = 'A'.
                ELSEIF <fs_good_cells>-fieldname(2) EQ 'P_'.
                  _ltype = 'P'.
                ELSE.
                  _ltype = space.
                ENDIF.

                IF _ltype <> space.
                  CLEAR: _prsid.
                  GET TIME STAMP FIELD _prsid.
                  _lqday = <fs_good_cells>-fieldname+2(8).
                  DELETE _appdat->root_dat-_logdat WHERE bukrs = _srcdat->bukrs AND pkind = _srcdat->pkind_src AND twaer = _srcdat->waers_src AND lqpos = _srcdat->lqpos_src AND lqday = _lqday AND ltype = _ltype.
                  CLEAR _logdat.
                  _logdat-bukrs = _srcdat->bukrs.
                  _logdat-pkind = _srcdat->pkind_src.
                  _logdat-twaer = _srcdat->waers_src.
                  _logdat-lqpos = _srcdat->lqpos_src.
                  _logdat-ebene = _srcdat->ebene_src.
                  _logdat-lqday = _lqday.
                  _logdat-ltype = _ltype.
                  _logdat-prsid = _prsid.
                  _logdat-wrbtr = <fs_good_cells>-value.
                  _logdat-erdat = sy-datum.
                  _logdat-erzet = sy-uzeit.
                  _logdat-ernam = sy-uname.
                  APPEND _logdat TO _appdat->root_dat-_logdat.
                ENDIF.
              ENDIF.
            ENDIF.
*<-end of -aatan  }
          ENDIF.
        ENDIF.
*--------------------------------------------------------------------*
*-& Insert Log Dat->
*--------------------------------------------------------------------*
        READ TABLE mo_model->mt_appdat REFERENCE INTO _appdat WITH KEY bukrs = _keydat-bukrs pkind = _keydat-pkind waers = _keydat-waers.
        IF sy-subrc IS INITIAL.
          IF <fs_good_cells>-fieldname(2) EQ 'A_'.
            _ltype = 'A'.
          ELSEIF <fs_good_cells>-fieldname(2) EQ 'P_'.
            _ltype = 'P'.
          ELSE.
            _ltype = space.
          ENDIF.
          IF _ltype <> space.
            CLEAR: _prsid.
            GET TIME STAMP FIELD _prsid.
            _lqday = <fs_good_cells>-fieldname+2(8).
            DELETE _appdat->root_dat-_logdat WHERE bukrs = _keydat-bukrs AND pkind = _keydat-pkind AND twaer = _keydat-waers AND lqpos = _keydat-lqpos AND lqday = _lqday AND ltype = _ltype.
            CLEAR _logdat.
            _logdat-bukrs = _keydat-bukrs.
            _logdat-pkind = _keydat-pkind.
            _logdat-twaer = _keydat-waers.
            _logdat-lqpos = _keydat-lqpos.
            _logdat-ebene = _keydat-ebene.
            _logdat-lqday = _lqday.
            _logdat-ltype = _ltype.
            _logdat-prsid = _prsid.
            _logdat-wrbtr = <fs_good_cells>-value.
            _logdat-erdat = sy-datum.
            _logdat-erzet = sy-uzeit.
            _logdat-ernam = sy-uname.
            APPEND _logdat TO _appdat->root_dat-_logdat.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF l_change <> abap_false.
      mo_view->mo_grid->refresh_table_display(
        EXPORTING
          is_stable      = ls_stable
          i_soft_refresh = abap_true
        EXCEPTIONS
          finished       = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "handle_data_finished
  METHOD handle_top_of_page.
    event_top_of_page(
      CHANGING
        dg_dyndoc_id = e_dyndoc_id ).
  ENDMETHOD.                    "handle_top_of_page
  METHOD event_top_of_page.

    DATA : dl_text(255) TYPE c,
           v_name_last  TYPE adrp-name_last,
           v_name_first TYPE adrp-name_first.

    CONSTANTS: c_date_from TYPE adrp-date_from VALUE '00010101'.

*-&Free class and create top of page document;
    FREE dg_dyndoc_id.
    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

*--------------------------------------------------------------------*
*-&HEADER ->
*--------------------------------------------------------------------*
    CLEAR: dl_text.
    CONCATENATE 'Başlık:' text-h01 INTO dl_text.
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_style    = cl_dd_area=>heading
        sap_fontsize = cl_dd_area=>standard
        sap_color    = cl_dd_area=>list_heading_int.

    CLEAR: dl_text.
    dl_text = text-h02.
    CALL METHOD dg_dyndoc_id->new_line.
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_style    = cl_dd_area=>heading
        sap_fontsize = cl_dd_area=>large
        sap_emphasis = cl_dd_area=>strong.

    CLEAR: dl_text.
    dl_text = 'Kullanıcı Adı :'.
    CALL METHOD dg_dyndoc_id->new_line.
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_emphasis = cl_dd_area=>strong
        sap_color    = cl_dd_area=>list_heading_int.

    CLEAR: dl_text, v_name_last, v_name_first.
    SELECT SINGLE name_last name_first
      FROM usr21 JOIN adrp ON adrp~persnumber = usr21~persnumber AND
                              adrp~date_from  = c_date_from AND
                              adrp~nation     = space
        INTO (v_name_last, v_name_first)
        WHERE usr21~bname = sy-uname.
    CONCATENATE v_name_first v_name_last INTO dl_text SEPARATED BY space.

    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_emphasis = cl_dd_area=>heading
        sap_color    = cl_dd_area=>list_negative_inv.

    CLEAR: dl_text.
    dl_text = 'Tarih :'.
    CALL METHOD dg_dyndoc_id->new_line.
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_emphasis = cl_dd_area=>strong
        sap_color    = cl_dd_area=>list_heading_int.

    CLEAR dl_text.
    dl_text = get_long_date( im_dat = sy-datum ).
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_emphasis = cl_dd_area=>heading
        sap_color    = cl_dd_area=>list_negative_inv.

    display_top_of_page(
      EXPORTING
        dg_dyndoc_id = dg_dyndoc_id ).

  ENDMETHOD.              "event_top_of_page
  METHOD display_top_of_page.

    IF mo_view->mo_html_cntrl IS INITIAL.
      CREATE OBJECT mo_view->mo_html_cntrl
        EXPORTING
          parent = mo_view->mo_parent_top.
    ENDIF.
    CALL METHOD dg_dyndoc_id->merge_document.
    dg_dyndoc_id->html_control = mo_view->mo_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = abap_true
        parent             = mo_view->mo_parent_top
      EXCEPTIONS
        html_display_error = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.              "display_top_of_page

ENDCLASS.                    "lcl_mvc_controller IMPLEMENTATION
