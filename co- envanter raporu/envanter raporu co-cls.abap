*&---------------------------------------------------------------------*
*& Include          ZCO_R_001_CLS
*&---------------------------------------------------------------------*
CLASS lcl_report DEFINITION.
  PUBLIC SECTION .
    TYPES : BEGIN OF ty_data,
              sjahrper          TYPE mldoc-jahrper,
              ejahrper          TYPE mldoc-jahrper,
              bukrs             TYPE t001k-bukrs,
              kalnr             TYPE mldoc-kalnr,
              matnr             TYPE ckmlhd-matnr,
              maktx             TYPE makt-maktx,
              mtart             TYPE mara-mtart,
              matkl             TYPE mara-matkl,
              mtbez             TYPE t134t-mtbez,
              bwkey             TYPE ckmlhd-bwkey,
              bwtar             TYPE ckmlhd-bwtar,
              vbeln             TYPE ckmlhd-vbeln,
              posnr             TYPE ckmlhd-posnr,
              curtp             TYPE mldoc-curtp,
              acc_principle     TYPE mldoc-acc_principle,
              konts             TYPE t030-konts,
              meins             TYPE meins,
              waers             TYPE waers,
              openstock_amount  TYPE mldoc-salk3,
              openstock_quan    TYPE mldoc-lbkum,
              acilis_rvz_mikt   TYPE mldoc-lbkum,
              acilis_rvz_ttr    TYPE mldoc-salk3,
              acilis_rvz_mikt_t TYPE mldoc-lbkum,
              acilis_rvz_ttr_t  TYPE mldoc-salk3,
              closestock_amount TYPE mldoc-salk3,
              closestock_quan   TYPE mldoc-lbkum,
              ferth             TYPE mara-ferth,
              mwstock_quan      TYPE mldoc-lbkum,
              kalan_quan        TYPE mldoc-lbkum,
            END OF ty_data,
            tt_data TYPE STANDARD TABLE OF ty_data.

    TYPES :BEGIN OF ty_submit,
             kalnr       TYPE ckmlhd-kalnr,
             mlast       TYPE ckmlhd-mlast,
             matnr       TYPE ckmlhd-matnr,
             bwkey       TYPE ckmlhd-bwkey,
             bwtar       TYPE ckmlhd-bwtar,
             sobkz       TYPE ckmlhd-sobkz,
             vbeln       TYPE ckmlhd-vbeln,
             posnr       TYPE ckmlhd-posnr,
             pspnr       TYPE ckmlhd-pspnr,
             bdatj       TYPE ckmlpp-bdatj,
             poper       TYPE ckmlpp-poper,
             status      TYPE ckmlpp-status,
             status_text TYPE dd07v-ddtext,
             curtp       TYPE ckmlcr-curtp,
             bklas       TYPE mbew-bklas,
             mtart       TYPE mara-mtart,
             mtbez       TYPE t134t-mtbez,
             matkl       TYPE mara-matkl,
             spart       TYPE mara-spart,
             ktext       TYPE makt-maktx,
             vprsv       TYPE ckmlcr-vprsv,
             lbkum       TYPE ckmlpp-lbkum,
             meins       TYPE mara-meins,
             salk3       TYPE ckmlcr-salk3,
             salkv       TYPE ckmlcr-salkv,
             eb_dif      TYPE cki_doc_ml-eb_dif,
             stprs       TYPE ckmlcr-stprs,
             pvprs       TYPE ckmlcr-pvprs,
             prabw_prz   TYPE ck_prabw_prz,
             peinh       TYPE ckmlcr-peinh,
             waers       TYPE waers,
           END OF ty_submit,
           tt_submit TYPE STANDARD TABLE OF ty_submit WITH KEY kalnr.

    TYPES :BEGIN OF ty_acdoca_m_extract,
             kalnr TYPE acdoca_m_extract-kalnr,
             matnr TYPE acdoca_m_extract-matnr,
             bwkey TYPE acdoca_m_extract-bwkey,
             bwtar TYPE acdoca_m_extract-bwtar,
             vmsl  TYPE acdoca_m_extract-vmsl,
             hsl   TYPE acdoca_m_extract-hsl,
           END OF ty_acdoca_m_extract,
           tt_acdoca_m_extract TYPE STANDARD TABLE OF ty_acdoca_m_extract WITH KEY kalnr.

    TYPES: BEGIN OF ty_dyn_fields,
             categ     TYPE zco_010_t001-categ,
             ptyp      TYPE zco_010_t001-ptyp,
             bwart     TYPE bwart,
             hkont     TYPE hkont,
             ftype     TYPE c LENGTH 1, " Q -> Quantity, A -> Amount
             bv        TYPE xfeld,
             sign      TYPE sign,
             fieldname TYPE fieldname,
             coltext   TYPE lvc_txtcol,
           END OF ty_dyn_fields,
           tt_dyn_fields TYPE STANDARD TABLE OF ty_dyn_fields.

    CONSTANTS: lc_x TYPE xfeld VALUE 'X'.
    DATA: mt_fieldcat TYPE lvc_t_fcat,
          mt_data     TYPE tt_data.

    DATA : mt_open  TYPE tt_submit,
           mt_close TYPE tt_submit,
           mt_all   TYPE tt_submit.

    DATA : mt_open_acdoca_m_extract  TYPE tt_acdoca_m_extract,
           mt_close_acdoca_m_extract TYPE tt_acdoca_m_extract,
           mt_all_acdoca_m_extract   TYPE tt_acdoca_m_extract.

    DATA : go_dyn_table TYPE REF TO data,
           go_line      TYPE REF TO data,
           go_alv       TYPE REF TO cl_salv_table.

    DATA : ls_layout            TYPE lvc_s_layo,
           ls_variant           TYPE disvariant,
           lr_docking_container TYPE REF TO cl_gui_docking_container,
           lr_custom_container  TYPE REF TO cl_gui_custom_container,
           lr_grid              TYPE REF TO cl_gui_alv_grid,
           lv_repid             TYPE sy-repid.

    METHODS :
      start,
      get_data,
      rkkbmlmat
        IMPORTING VALUE(ir_matnr) TYPE range_t_matnr
                  VALUE(iv_bwkey) TYPE werks_d
                  VALUE(iv_poper) TYPE poper
                  VALUE(iv_bdatj) TYPE bdatj
                  VALUE(iv_type)  TYPE char1
                  VALUE(iv_curtp) TYPE curtp,

      read_acdoca_m_extract
        IMPORTING VALUE(ir_matnr)   TYPE range_t_matnr
                  VALUE(iv_bwkey)   TYPE werks_d
                  VALUE(iv_jahrper) TYPE jahrper
                  VALUE(iv_type)    TYPE char1,
      display,
      build_fcat
        IMPORTING iv_type   TYPE REF TO data
        EXPORTING ev_colpos TYPE lvc_colpos,
      create_dynamic_table
        IMPORTING VALUE(it_dyn_fields) TYPE tt_dyn_fields,
      on_user_command
        FOR EVENT added_function OF cl_salv_events_table
        IMPORTING e_salv_function,
      crate_grid,
      refresh_grid
        IMPORTING
          VALUE(iv_grid)   TYPE REF TO cl_gui_alv_grid
          VALUE(iv_stable) TYPE char1
          VALUE(iv_soft)   TYPE char1,

      handle_user_command  FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING sender e_ucomm .                   .
    CLASS-METHODS: init.

ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
  METHOD init.
    DATA: lv_perio TYPE cest1-perio .
    CALL FUNCTION 'RKE_CONVERT_DATE_TO_PERIOD'
      EXPORTING
        date              = sy-datum
        periv             = 'K4'
      IMPORTING
        perio             = lv_perio
      EXCEPTIONS
        i_error           = 1
        i_perflag_invalid = 2
        i_periv_notedited = 3
        i_periv_notfound  = 4
        OTHERS            = 5.
    p_sper = p_eper = lv_perio.
  ENDMETHOD.

  METHOD build_fcat.
    DATA:
      lr_structdescr TYPE REF TO cl_abap_structdescr,
      lr_typedescr   TYPE REF TO cl_abap_typedescr,
      lr_elemdescr   TYPE REF TO cl_abap_elemdescr.

    DATA: lv_colpos TYPE lvc_colpos VALUE IS INITIAL.

    TRY.
        lr_structdescr ?= cl_abap_structdescr=>describe_by_data_ref( iv_type ).
* Get structure fields
        DATA(lt_comp) = lr_structdescr->get_components( ).
      CATCH cx_root.
    ENDTRY.

    LOOP AT lt_comp INTO DATA(ls_comp).
      ADD 1 TO lv_colpos.
      lr_typedescr ?= ls_comp-type.
      IF lr_typedescr->is_ddic_type( ) = 'X'.
        lr_elemdescr ?= ls_comp-type.
        DATA(ls_dfies) = lr_elemdescr->get_ddic_field( sy-langu ).
        APPEND INITIAL LINE TO me->mt_fieldcat REFERENCE INTO DATA(ls_fieldcat).
        ls_fieldcat->col_pos     = lv_colpos.
        ls_fieldcat->tabname     = '1'.
        ls_fieldcat->fieldname   = ls_comp-name.
        ls_fieldcat->scrtext_l   = ls_dfies-scrtext_l.
        ls_fieldcat->scrtext_m   = ls_dfies-scrtext_m.
        ls_fieldcat->scrtext_s   = ls_dfies-scrtext_s.
        ls_fieldcat->reptext     = ls_dfies-reptext.
        ls_fieldcat->domname     = ls_dfies-domname.
        ls_fieldcat->rollname    = ls_dfies-rollname.
        ls_fieldcat->intlen      = ls_dfies-leng.
        ls_fieldcat->outputlen   = ls_dfies-outputlen.
        ls_fieldcat->decimals    = ls_dfies-decimals.
        ls_fieldcat->datatype    = ls_dfies-datatype.
        ls_fieldcat->inttype     = ls_dfies-inttype.
*            ls_fieldcat->convexit    = ls_dfies-convexit.
      ELSE.
        ls_fieldcat->intlen      = lr_typedescr->length.
        ls_fieldcat->inttype     = lr_typedescr->type_kind.
        ls_fieldcat->fieldname   = ls_comp-name.
      ENDIF.

      CASE ls_fieldcat->fieldname.
        WHEN 'SJAHRPER'.
          ls_fieldcat->ref_table   = 'MLDOC'.
          ls_fieldcat->ref_field   = 'JAHRPER'.
          ls_fieldcat->scrtext_l   = TEXT-f01.
          ls_fieldcat->scrtext_m   = TEXT-f01.
          ls_fieldcat->scrtext_s   = TEXT-f01.
          ls_fieldcat->reptext     = TEXT-f01.
        WHEN 'EJAHRPER'.
          ls_fieldcat->ref_table   = 'MLDOC'.
          ls_fieldcat->ref_field   = 'JAHRPER'.
          ls_fieldcat->scrtext_l   = TEXT-f02.
          ls_fieldcat->scrtext_m   = TEXT-f02.
          ls_fieldcat->scrtext_s   = TEXT-f02.
          ls_fieldcat->reptext     = TEXT-f02.
        WHEN 'KONTS'.
          ls_fieldcat->scrtext_l   = TEXT-f08.
          ls_fieldcat->scrtext_m   = TEXT-f08.
          ls_fieldcat->scrtext_s   = TEXT-f08.
          ls_fieldcat->reptext     = TEXT-f08.
        WHEN 'OPENSTOCK_AMOUNT'.
          ls_fieldcat->scrtext_l   = TEXT-f04.
          ls_fieldcat->scrtext_m   = TEXT-f04.
          ls_fieldcat->scrtext_s   = TEXT-f04.
          ls_fieldcat->reptext     = TEXT-f04.
          ls_fieldcat->cfieldname = 'WAERS'.
        WHEN 'OPENSTOCK_QUAN'.
          ls_fieldcat->scrtext_l   = TEXT-f05.
          ls_fieldcat->scrtext_m   = TEXT-f05.
          ls_fieldcat->scrtext_s   = TEXT-f05.
          ls_fieldcat->reptext     = TEXT-f05.
          ls_fieldcat->qfieldname = 'MEINS'.
        WHEN 'CLOSESTOCK_AMOUNT'.
          ls_fieldcat->scrtext_l   = TEXT-f06.
          ls_fieldcat->scrtext_m   = TEXT-f06.
          ls_fieldcat->scrtext_s   = TEXT-f06.
          ls_fieldcat->reptext     = TEXT-f06.
          ls_fieldcat->cfieldname = 'WAERS'.
        WHEN 'CLOSESTOCK_QUAN'.
          ls_fieldcat->scrtext_l   = TEXT-f07.
          ls_fieldcat->scrtext_m   = TEXT-f07.
          ls_fieldcat->scrtext_s   = TEXT-f07.
          ls_fieldcat->reptext     = TEXT-f07.
          ls_fieldcat->qfieldname = 'MEINS'.
        WHEN 'ACILIS_RVZ_MIKT'.
          ls_fieldcat->scrtext_l   = TEXT-f09.
          ls_fieldcat->scrtext_m   = TEXT-f09.
          ls_fieldcat->scrtext_s   = TEXT-f09.
          ls_fieldcat->reptext     = TEXT-f09.
          ls_fieldcat->qfieldname = 'MEINS'.
        WHEN 'ACILIS_RVZ_TTR'.
          ls_fieldcat->scrtext_l   = TEXT-f10.
          ls_fieldcat->scrtext_m   = TEXT-f10.
          ls_fieldcat->scrtext_s   = TEXT-f10.
          ls_fieldcat->reptext     = TEXT-f10.
          ls_fieldcat->cfieldname = 'WAERS'.
        WHEN 'ACILIS_RVZ_MIKT_T'.
          ls_fieldcat->scrtext_l   = TEXT-f11.
          ls_fieldcat->scrtext_m   = TEXT-f11.
          ls_fieldcat->scrtext_s   = TEXT-f11.
          ls_fieldcat->reptext     = TEXT-f11.
          ls_fieldcat->qfieldname = 'MEINS'.
        WHEN 'ACILIS_RVZ_TTR_T'.
          ls_fieldcat->scrtext_l   = TEXT-f12.
          ls_fieldcat->scrtext_m   = TEXT-f12.
          ls_fieldcat->scrtext_s   = TEXT-f12.
          ls_fieldcat->reptext     = TEXT-f12.
          ls_fieldcat->cfieldname = 'WAERS'.
        WHEN 'MWSTOCK_QUAN'.
          ls_fieldcat->scrtext_l   = TEXT-f13.
          ls_fieldcat->scrtext_m   = TEXT-f13.
          ls_fieldcat->scrtext_s   = TEXT-f13.
          ls_fieldcat->reptext     = TEXT-f13.
        WHEN 'KALAN_QUAN'.
          ls_fieldcat->scrtext_l   = TEXT-f14.
          ls_fieldcat->scrtext_m   = TEXT-f14.
          ls_fieldcat->scrtext_s   = TEXT-f14.
          ls_fieldcat->reptext     = TEXT-f14.

        WHEN 'CURTP'.
        WHEN 'CURTP'.
          ls_fieldcat->no_out = lc_x.
      ENDCASE.
    ENDLOOP.
    ev_colpos = lv_colpos.
  ENDMETHOD.

  METHOD create_dynamic_table.
    DATA: lr_type      TYPE REF TO data,
          lv_colpos    TYPE lvc_colpos,
          lr_elemdescr TYPE REF TO cl_abap_elemdescr.

    CREATE DATA lr_type TYPE ty_data.
*   //Create Fieldcat
    me->build_fcat(
      EXPORTING
        iv_type   = lr_type
      IMPORTING
        ev_colpos = lv_colpos
    ).

    LOOP AT it_dyn_fields INTO DATA(ls_dynfields).

      APPEND INITIAL LINE TO me->mt_fieldcat REFERENCE INTO DATA(ls_fieldcat).
      ADD 1 TO lv_colpos.
      ls_fieldcat->col_pos     = lv_colpos.
      ls_fieldcat->tabname     = '1'.
      ls_fieldcat->fieldname   = ls_dynfields-fieldname.
      ls_fieldcat->scrtext_l   = ls_dynfields-coltext.
      ls_fieldcat->scrtext_m   = ls_dynfields-coltext.
      ls_fieldcat->scrtext_s   = ls_dynfields-coltext.
      ls_fieldcat->reptext     = ls_dynfields-coltext.

      CASE ls_dynfields-ftype.
        WHEN 'Q'.
          cl_abap_elemdescr=>describe_by_name(
            EXPORTING
              p_name         = 'ML4H_QUANTITY'
            RECEIVING
              p_descr_ref    = DATA(lo_type)
            EXCEPTIONS
              type_not_found = 1
              OTHERS         = 2
          ).
          ls_fieldcat->qfieldname = 'MEINS'.
        WHEN 'A'.
          cl_abap_elemdescr=>describe_by_name(
            EXPORTING
              p_name         = 'ML4H_STVAL'
            RECEIVING
              p_descr_ref    = lo_type
            EXCEPTIONS
              type_not_found = 1
              OTHERS         = 2
          ).
          ls_fieldcat->cfieldname = 'WAERS'.
      ENDCASE.

      lr_elemdescr ?= lo_type.
      DATA(ls_dfies) = lr_elemdescr->get_ddic_field( sy-langu ).

      ls_fieldcat->domname     = ls_dfies-domname.
      ls_fieldcat->rollname    = ls_dfies-rollname.
      ls_fieldcat->intlen      = ls_dfies-leng.
      ls_fieldcat->outputlen   = ls_dfies-outputlen.
      ls_fieldcat->decimals    = ls_dfies-decimals.
      ls_fieldcat->datatype    = ls_dfies-datatype.
      ls_fieldcat->inttype     = ls_dfies-inttype.
      ls_fieldcat->rollname    = ls_dfies-rollname.
      ls_fieldcat->convexit    = ls_dfies-convexit.
      ls_fieldcat->f4availabl  = ls_dfies-f4availabl.
    ENDLOOP.

    TRY .
        CALL METHOD cl_alv_table_create=>create_dynamic_table
          EXPORTING
            it_fieldcatalog           = mt_fieldcat
          IMPORTING
            ep_table                  = go_dyn_table
          EXCEPTIONS
            generate_subpool_dir_full = 1
            OTHERS                    = 2.
        IF sy-subrc = 0.
*        ASSIGN gt_dyn_table->* TO <fs_dyn_table>.
*
*        CREATE DATA gs_line LIKE LINE OF <fs_dyn_table>.
*        ASSIGN gs_line->* TO <fs_line>.
        ENDIF.
      CATCH cx_root.

    ENDTRY.

    IF go_dyn_table IS INITIAL.
      MESSAGE e000(zpp) WITH TEXT-e01.
    ENDIF.

  ENDMETHOD.                                             "#EC CI_VALPAR

  METHOD start.
    get_data( ).
  ENDMETHOD.

  METHOD get_data.

    DATA: lr_jahrper      TYPE RANGE OF mldoc-jahrper,
          lr_categ        TYPE RANGE OF mldoc-categ,
          lr_bwart        TYPE RANGE OF mldoc-bwart,
          lv_endper       TYPE mldoc-jahrper,
          lv_open_period  TYPE mldoc-jahrper,
          lv_close_period TYPE mldoc-jahrper,
          lv_date         TYPE sy-datum,
          lv_cari         TYPE xfeld.
    DATA: ls_hd TYPE ckmlhd.

    FIELD-SYMBOLS: <fs_s_yyyy>    TYPE t009b-bdatj,
                   <fs_s_ppp>     TYPE t009b-poper,
                   <fs_e_yyyy>    TYPE t009b-bdatj,
                   <fs_e_ppp>     TYPE t009b-poper,
                   <fs_dyn_table> TYPE STANDARD TABLE,
                   <fs_line>      TYPE any,
                   <fs_matnr>     TYPE any,
                   <fs_mtbez>     TYPE any.

    FIELD-SYMBOLS : <fs_sjahrper>          TYPE any,
                    <fs_ejahrper>          TYPE any,
                    <fs_bukrs>             TYPE any,
                    <fs_waers>             TYPE any,
                    <fs_meins>             TYPE any,
                    <fs_curtp>             TYPE any,
                    <fs_acc_principle>     TYPE any,
                    <fs_openstock_amount>  TYPE any,
                    <fs_openstock_quan>    TYPE any,
                    <fs_acilis_rvz_mikt>   TYPE any,
                    <fs_acilis_rvz_ttr>    TYPE any,
                    <fs_acilis_rvz_mikt_t> TYPE any,
                    <fs_acilis_rvz_ttr_t>  TYPE any,
                    <fs_closestock_amount> TYPE any,
                    <fs_mwstock_quan>      TYPE any,
                    <fs_kalan_quan>        TYPE any,
                    <fs_ferth>             TYPE any,
                    <fs_closestock_quan>   TYPE any,
                    <fs_val_q>             TYPE any,
                    <fs_val>               TYPE any,
                    <fs_konts>             TYPE any.

    DATA: lt_dynfields TYPE tt_dyn_fields,
          ls_dynfields TYPE ty_dyn_fields,
          lv_counter   TYPE numc2.

    DATA: BEGIN OF ls_kalnr,
            bvalt TYPE mldoc-bvalt,
          END OF ls_kalnr,
          lt_kalnr     LIKE TABLE OF ls_kalnr,
          ls_data_line TYPE ty_data.

    DATA : lr_matnr TYPE range_t_matnr,
           ls_matnr TYPE range_s_matnr.

    SELECT SINGLE * FROM marv
                    INTO @DATA(ls_marv)
                   WHERE bukrs IN @s_bukrs.
    IF p_eper(4) EQ ls_marv-lfgja AND
       p_eper+5(2) EQ ls_marv-lfmon .
      lv_cari = lc_x.
    ENDIF.

    SELECT 'I'        AS sign,
           'EQ'       AS option,
           domvalue_l AS low,
           ' '        AS high
      FROM dd07l
      INTO CORRESPONDING FIELDS OF TABLE @lr_categ
     WHERE domname  = 'ZCO_D_CKML_CATEG'
       AND as4local = 'A'.

    SELECT *
      FROM zco_010_t001
      INTO TABLE @DATA(lt_mlcateg).                     "#EC CI_NOWHERE

    ASSIGN p_sper+0(4) TO <fs_s_yyyy>.
    ASSIGN p_sper+4(3) TO <fs_s_ppp>.

    CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
      EXPORTING
        i_periv        = 'K4'
        i_gjahr        = <fs_s_yyyy>
        i_poper        = <fs_s_ppp>
      IMPORTING
        e_date         = lv_date
      EXCEPTIONS
        input_false    = 1
        t009_notfound  = 2
        t009b_notfound = 3.

    CALL FUNCTION 'BKK_ADD_MONTH_TO_DATE'
      EXPORTING
        months  = -1
        olddate = lv_date
      IMPORTING
        newdate = lv_date.

    CALL FUNCTION 'RKE_CONVERT_DATE_TO_PERIOD'
      EXPORTING
        date              = lv_date
        periv             = 'K4'
      IMPORTING
        perio             = lv_open_period
      EXCEPTIONS
        i_error           = 1
        i_perflag_invalid = 2
        i_periv_notedited = 3
        i_periv_notfound  = 4
        OTHERS            = 5.

    ASSIGN p_eper+0(4) TO <fs_e_yyyy>.
    ASSIGN p_eper+4(3) TO <fs_e_ppp>.
    lv_close_period = p_eper.

    CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
      EXPORTING
        i_periv        = 'K4'
        i_gjahr        = <fs_e_yyyy>
        i_poper        = <fs_e_ppp>
      IMPORTING
        e_date         = lv_date
      EXCEPTIONS
        input_false    = 1
        t009_notfound  = 2
        t009b_notfound = 3.

    CALL FUNCTION 'BKK_ADD_MONTH_TO_DATE'
      EXPORTING
        months  = 1
        olddate = lv_date
      IMPORTING
        newdate = lv_date.

    CALL FUNCTION 'RKE_CONVERT_DATE_TO_PERIOD'
      EXPORTING
        date              = lv_date
        periv             = 'K4'
      IMPORTING
        perio             = lv_endper
      EXCEPTIONS
        i_error           = 1
        i_perflag_invalid = 2
        i_periv_notedited = 3
        i_periv_notfound  = 4
        OTHERS            = 5.

    ASSIGN lv_endper+0(4) TO <fs_e_yyyy>.
    ASSIGN lv_endper+4(3) TO <fs_e_ppp>.
    lr_jahrper = VALUE #( sign = 'I' option = 'BT' ( high = p_sper ) ).
*    lr_jahrper = VALUE #( sign = 'I' option = 'BT' ( low = p_sper high = lv_endper ) ).

    SELECT k~bukrs, k~bwmod, c~kalnr, c~matnr, c~bwkey, c~bwtar,
           c~vbeln, c~posnr, t~waers, m~meins, x~maktx, m~mtart,
           m~ferth, m~matkl
      FROM ckmlhd AS c
      INNER JOIN t001k AS k ON k~bwkey EQ c~bwkey
*                     AND k~bukrs EQ @p_bukrs
*      INNER JOIN t001 AS t ON t~bukrs EQ k~bukrs
      INNER JOIN ckmlcr AS t ON t~curtp EQ @p_curtp
                            AND t~kalnr EQ c~kalnr
                            AND t~bdatj EQ @p_sper+0(4)
                            AND t~poper EQ @p_sper+4(3)
      INNER JOIN mara AS m ON m~matnr EQ c~matnr
      LEFT OUTER JOIN makt AS x ON x~matnr EQ m~matnr
                          AND x~spras EQ @sy-langu
      INTO TABLE @DATA(lt_ckmlhd)
     WHERE c~kalnr IN @s_kalnr
       AND c~matnr IN @s_matnr
       AND c~bwkey IN @s_bwkey
       AND c~bwtar IN @s_bwtar
       AND c~vbeln IN @s_vbeln
       AND c~posnr IN @s_posnr
       AND m~mtart IN @s_mtart                         "#EC CI_BUFFJOIN
       AND k~bukrs IN @s_bukrs.


    IF lt_ckmlhd IS INITIAL.
      MESSAGE i789(m7).
      RETURN.
    ELSE.

      SELECT *
        FROM t134t
        INTO TABLE @DATA(lt_t134t)
        FOR ALL ENTRIES IN @lt_ckmlhd
        WHERE mtart EQ @lt_ckmlhd-mtart
          AND spras EQ @sy-langu.

      LOOP AT lt_ckmlhd INTO DATA(ls_hd_)
                        GROUP BY ( bwkey = ls_hd_-bwkey )
                        INTO DATA(lr_grp).
        CLEAR: lr_matnr.
        LOOP AT GROUP lr_grp INTO DATA(ls_line).
          ls_matnr-sign   = 'I'.
          ls_matnr-option = 'EQ'.
          ls_matnr-low    = ls_line-matnr.
          COLLECT ls_matnr INTO lr_matnr.
        ENDLOOP.



* //  Açılış stok
*        read_acdoca_m_extract(
*          EXPORTING
*            ir_matnr = lr_matnr
*            iv_bwkey = lr_grp-bwkey
*            iv_jahrper = lv_open_period
*            iv_type  = 'A'
*            ).

        rkkbmlmat(
          EXPORTING
            ir_matnr = lr_matnr
            iv_bwkey = lr_grp-bwkey
            iv_poper = lv_open_period+4(3)
            iv_bdatj = lv_open_period(4)
            iv_type  = 'A'
            iv_curtp = p_curtp ).

* //  Kapanış stok

*        read_acdoca_m_extract(
*          EXPORTING
*            ir_matnr = lr_matnr
*            iv_bwkey = lr_grp-bwkey
*            iv_jahrper = lv_close_period
*            iv_type  = 'K'
*            ).

        rkkbmlmat(
          EXPORTING
            ir_matnr = lr_matnr
            iv_bwkey = lr_grp-bwkey
            iv_poper = lv_close_period+4(3)
            iv_bdatj = lv_close_period(4)
            iv_type  = 'K'
            iv_curtp = p_curtp ).
      ENDLOOP.
*      SORT mt_open_acdoca_m_extract BY kalnr.
*      SORT mt_close_acdoca_m_extract BY kalnr.

      SORT mt_open BY kalnr.
      SORT mt_close BY kalnr.

      DATA: r_matkl TYPE RANGE OF mara-matkl.

      APPEND VALUE #( sign = 'I' option = 'EQ' low = '2321' ) TO r_matkl.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = '2322' ) TO r_matkl.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = '2332' ) TO r_matkl.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = '2333' ) TO r_matkl.
    ENDIF.

    SELECT m~awref, m~awitem, m~aworg, k~bukrs, k~bwmod, c~kalnr, c~matnr, c~bwkey,
           c~bwtar, c~vbeln, c~posnr, m~curtp, m~acc_principle, m~jahrper, m~categ,
           m~ptyp, m~bvalt, m~process,m~quant, m~meins, m~stval, m~prd, m~kdm, m~tpprd,
           m~waers ,'000' AS bwart
      FROM ckmlhd AS c
      INNER JOIN t001k AS k  ON k~bwkey EQ c~bwkey
*                     AND k~bukrs EQ @p_bukrs
      INNER JOIN mldoc  AS m ON m~kalnr EQ c~kalnr
*     INNER JOIN matdoc AS d ON d~mblnr EQ m~awref
*                                and d~matnr EQ c~matnr
*                                AND d~werks EQ c~bwkey
*                                AND d~bwtar EQ c~bwtar
*     LEFT OUTER JOIN matdoc AS d ON d~mblnr EQ m~awref
**                                AND d~matnr EQ c~matnr
**                                AND d~werks EQ c~bwkey
**                                AND d~bwtar EQ c~bwtar
*                                AND d~mjahr EQ m~aworg
*                                AND d~zeile EQ cast( m~awitem as char( 4 ) )
*                                             AND d~bstaus_sg EQ 'A'
      INTO TABLE @DATA(lt_data)
     WHERE c~kalnr   IN @s_kalnr
       AND c~matnr   IN @s_matnr
       AND c~bwkey   IN @s_bwkey
       AND c~bwtar   IN @s_bwtar
       AND c~vbeln   IN @s_vbeln
       AND c~posnr   IN @s_posnr
       AND m~jahrper IN @lr_jahrper
*       AND d~gjper   IN @lr_jahrper
*       AND d~gjper   EQ @p_eper
       AND m~categ   IN @lr_categ
       AND m~curtp   EQ @p_curtp
*       AND  m~acc_principle EQ ''.
       AND ( m~runref  EQ ''
       OR  m~runref  EQ 'ACT' )
       AND k~bukrs IN @s_bukrs."fkaraguzel | 02.08.2021   "#EC CI_BUFFJOIN


    CHECK lt_data[] IS NOT INITIAL.

    SELECT mblnr,zeile,mjahr,bwart
      FROM matdoc
      INTO TABLE @DATA(lt_matdoc)
      FOR ALL ENTRIES IN @lt_data
      WHERE mblnr EQ @lt_data-awref
        AND zeile EQ @lt_data-awitem+2(4)
        AND mjahr BETWEEN @p_sper(4) AND @p_eper(4)
        AND bstaus_sg EQ 'A'.

    SORT lt_matdoc BY mblnr mjahr zeile.

*    IF lt_data IS NOT INITIAL.
*      SELECT w~matnr,
*             w~bwkey,
*             w~bwtar,
*             w~bklas,
*             t~konts
*        FROM mbew AS w
*       INNER JOIN t030 AS t
*          ON t~ktosl = 'BSX'
*         AND t~bklas = w~bklas
*         FOR ALL ENTRIES IN @lt_data
*       WHERE w~matnr = @lt_data-matnr
*         AND w~bwkey = @lt_data-bwkey
*         AND w~bwtar = @lt_data-bwtar
*         AND t~bwmod = @lt_data-bwmod
*        INTO TABLE @DATA(lt_konts).
*      SORT lt_konts BY matnr bwkey.
*
*    ENDIF.

    IF mt_all IS NOT INITIAL.
      SELECT w~matnr, w~bwkey, w~bwtar, w~bklas, t~konts
        FROM mbew AS w INNER JOIN t030 AS t ON t~ktosl = 'BSX'
                                           AND t~bklas = w~bklas
                       INNER JOIN t001k ON t001k~bwmod = t~bwmod
                                       AND t001k~bwkey = w~bwkey
        INTO TABLE @DATA(lt_konts)
         FOR ALL ENTRIES IN @mt_all
       WHERE w~matnr EQ @mt_all-matnr
         AND w~bwkey EQ @mt_all-bwkey
         AND w~bwtar EQ @mt_all-bwtar.                 "#EC CI_BUFFJOIN
*         AND t~bwmod = @mt_all_acdoca_m_extract-bwkey

      SORT lt_konts BY matnr bwkey bwtar.
    ENDIF.


    LOOP AT lt_data INTO DATA(ls_data) WHERE jahrper       GE p_sper
                                         AND jahrper       LT lv_endper
                                         AND curtp         EQ p_curtp
                                         AND acc_principle EQ p_acc
                                         AND categ         IN lr_categ.

      READ TABLE lt_matdoc INTO DATA(ls_matdoc) WITH KEY mblnr = ls_data-awref
                                                         mjahr = ls_Data-aworg
                                                         zeile = ls_data-awitem+2(4)
                                                         BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_data-bwart = ls_matdoc-bwart.
        MODIFY lt_data FROM ls_data TRANSPORTING bwart.
      ENDIF.

      READ TABLE lt_mlcateg INTO DATA(ls_mlcateg) WITH KEY categ = ls_data-categ
                                                           ptyp  = ls_data-ptyp.
      IF sy-subrc = 0.
        READ TABLE lt_dynfields INTO ls_dynfields WITH KEY categ = ls_data-categ
                                                           ptyp  = ls_data-ptyp.
        IF sy-subrc NE 0.
          LOOP AT lt_mlcateg INTO ls_mlcateg WHERE categ = ls_data-categ
                                               AND ptyp  = ls_data-ptyp.
            CLEAR: ls_dynfields.
            ADD 1 TO lv_counter.
            ls_dynfields-categ     = ls_data-categ.
            ls_dynfields-ptyp      = ls_data-ptyp.
            ls_dynfields-bwart     = ls_mlcateg-bwart.
            ls_dynfields-bv        = ls_mlcateg-bv.
            ls_dynfields-fieldname = |{ ls_data-categ }_QUAN_{ lv_counter }|.
            ls_dynfields-ftype     = 'Q'.
            ls_dynfields-hkont     = ls_mlcateg-hkont.
            ls_dynfields-sign      = ls_mlcateg-sign.
            ls_dynfields-coltext   = |{ ls_mlcateg-coltext } { TEXT-qnt }|.
            APPEND ls_dynfields TO lt_dynfields.

            CLEAR: ls_dynfields.
            ADD 1 TO lv_counter.
            ls_dynfields-categ     = ls_data-categ.
            ls_dynfields-ptyp      = ls_data-ptyp.
            ls_dynfields-bwart     = ls_mlcateg-bwart.
            ls_dynfields-fieldname = |{ ls_data-categ }_AMOUNT_{ lv_counter }|.
            ls_dynfields-ftype     = 'A'.
            ls_dynfields-sign      = ls_mlcateg-sign.
            ls_dynfields-coltext   = |{ ls_mlcateg-coltext } { TEXT-amt }|.
            APPEND ls_dynfields TO lt_dynfields.
          ENDLOOP.
        ENDIF.

        IF ls_mlcateg-bv IS NOT INITIAL AND ls_data-process IS NOT INITIAL.
          ls_kalnr-bvalt = ls_data-process.
          COLLECT ls_kalnr INTO lt_kalnr.
        ENDIF.
      ENDIF.
      CLEAR: ls_mlcateg.
    ENDLOOP.

    DATA(lt_data_sorted) = lt_data.
    SORT lt_data_sorted BY kalnr
                           matnr
                           bwkey
                           bwtar
                           vbeln
                           posnr.


    IF lt_kalnr IS NOT INITIAL.
      SELECT kalnr, saknr_nd
        FROM ckmlmv005
        INTO TABLE @DATA(lt_ckmlmv005)
         FOR ALL ENTRIES IN @lt_kalnr
       WHERE kalnr EQ @lt_kalnr-bvalt.
      SORT lt_ckmlmv005 BY kalnr.
    ENDIF.

    create_dynamic_table( it_dyn_fields = lt_dynfields ).
    CHECK NOT go_dyn_table IS INITIAL.
    ASSIGN go_dyn_table->* TO <fs_dyn_table>.
    CREATE DATA go_line LIKE LINE OF <fs_dyn_table>.
    ASSIGN go_line->* TO <fs_line>.

    ASSIGN ('<fs_line>-sjahrper')          TO <fs_sjahrper>.
    ASSIGN ('<fs_line>-ejahrper')          TO <fs_ejahrper>.
    ASSIGN ('<fs_line>-meins')             TO <fs_meins>.
    ASSIGN ('<fs_line>-waers')             TO <fs_waers>.
    ASSIGN ('<fs_line>-openstock_amount')  TO <fs_openstock_amount>.
    ASSIGN ('<fs_line>-openstock_quan')    TO <fs_openstock_quan>.
    ASSIGN ('<fs_line>-closestock_amount') TO <fs_closestock_amount>.
    ASSIGN ('<fs_line>-closestock_quan')   TO <fs_closestock_quan>.
    ASSIGN ('<fs_line>-KONTS')             TO <fs_konts>.
    ASSIGN ('<fs_line>-CURTP')             TO <fs_curtp>.
    ASSIGN ('<fs_line>-ACILIS_RVZ_MIKT')   TO <fs_acilis_rvz_mikt>.
    ASSIGN ('<fs_line>-ACILIS_RVZ_TTR')    TO <fs_acilis_rvz_ttr>.
    ASSIGN ('<fs_line>-ACILIS_RVZ_MIKT_T') TO <fs_acilis_rvz_mikt_t>.
    ASSIGN ('<fs_line>-ACILIS_RVZ_TTR_T')  TO <fs_acilis_rvz_ttr_t>.

    DATA: lv_open,
          lv_close.

    LOOP AT lt_ckmlhd INTO DATA(ls_ckmlhd).
      CLEAR: <fs_line>.
      <fs_line> = CORRESPONDING #( ls_ckmlhd ).
      <fs_sjahrper> = p_sper.
      <fs_ejahrper> = p_eper.
      <fs_curtp>    = p_curtp.

*      READ TABLE mt_open INTO DATA(ls_open) WITH KEY kalnr = ls_ckmlhd-kalnr
*                                                                      BINARY SEARCH.
*      IF sy-subrc = 0.
*        DATA(lv_tabix) = sy-tabix.
*        LOOP AT mt_open INTO ls_open FROM lv_tabix.
*          IF ls_open-kalnr NE ls_ckmlhd-kalnr.
*            EXIT.
*          ENDIF.
*          ls_open-hsl = ls_open-hsl * -1.
*          ls_open-vmsl = ls_open-vmsl * -1.
*          ADD ls_open-salk3 TO <fs_openstock_amount>.
*          ADD ls_open-lbkum TO <fs_openstock_quan>.
*        ENDLOOP.
*      ENDIF.

      READ TABLE mt_open INTO DATA(ls_open)
                         WITH KEY kalnr = ls_ckmlhd-kalnr
                                  BINARY SEARCH.
      IF sy-subrc = 0.
        DATA(lv_tabix) = sy-tabix.
        LOOP AT mt_open INTO ls_open FROM lv_tabix.
          IF ls_open-kalnr NE ls_ckmlhd-kalnr.
            EXIT.
          ENDIF.
          ADD ls_open-salk3 TO <fs_openstock_amount>.
          ADD ls_open-lbkum TO <fs_openstock_quan>.
        ENDLOOP.
      ENDIF.

*      READ TABLE mt_close INTO DATA(ls_close) WITH KEY kalnr = ls_ckmlhd-kalnr
*                                                                         BINARY SEARCH.
*      IF sy-subrc = 0.
*        lv_tabix = sy-tabix.
*        LOOP AT mt_close INTO ls_close FROM lv_tabix.
*          IF ls_close-kalnr NE ls_ckmlhd-kalnr.
*            EXIT.
*          ENDIF.
*          ls_close-hsl = ls_close-hsl * -1.
*          ls_close-vmsl = ls_close-vmsl * -1.
*          ADD ls_close-hsl TO <fs_closestock_amount>.
*          ADD ls_close-vmsl TO <fs_closestock_quan>.
*        ENDLOOP.
*      ENDIF.

      READ TABLE mt_close INTO DATA(ls_close)
                         WITH KEY kalnr = ls_ckmlhd-kalnr
                                  BINARY SEARCH.
      IF sy-subrc = 0.
        lv_tabix = sy-tabix.
        LOOP AT mt_close INTO ls_close FROM lv_tabix.
          IF ls_close-kalnr NE ls_ckmlhd-kalnr.
            EXIT.
          ENDIF.
          ADD ls_close-salk3 TO <fs_closestock_amount>.
          ADD ls_close-lbkum TO <fs_closestock_quan>.
        ENDLOOP.
      ENDIF.

      READ TABLE lt_t134t INTO DATA(ls_t134t) WITH KEY mtart = ls_ckmlhd-mtart.
      IF sy-subrc IS INITIAL.
        ASSIGN ('<fs_line>-MTBEZ') TO <fs_mtbez>.
        <fs_mtbez> = ls_t134t-mtbez.
      ENDIF.

      APPEND <fs_line> TO <fs_dyn_table>.
    ENDLOOP.

    DATA: lv_hkont_related TYPE char2.

    LOOP AT <fs_dyn_table> ASSIGNING <fs_line>.
      CLEAR: ls_hd.
      ls_hd = CORRESPONDING #( <fs_line> ).

      ASSIGN ('<fs_line>-MATNR') TO <fs_matnr>.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = <fs_matnr>
        IMPORTING
          output = <fs_matnr>.

      READ TABLE lt_data_sorted INTO DATA(ls_sorted) WITH KEY kalnr = ls_hd-kalnr
                                                              matnr = ls_hd-matnr
                                                              bwkey = ls_hd-bwkey
                                                              bwtar = ls_hd-bwtar
                                                              vbeln = ls_hd-vbeln
                                                              posnr = ls_hd-posnr BINARY SEARCH.
      IF sy-subrc = 0.
        lv_tabix = sy-tabix.
        LOOP AT lt_data_sorted INTO ls_sorted FROM lv_tabix.
          IF ls_sorted-kalnr NE ls_hd-kalnr OR
             ls_sorted-matnr NE ls_hd-matnr OR
             ls_sorted-bwkey NE ls_hd-bwkey OR
             ls_sorted-bwtar NE ls_hd-bwtar OR
             ls_sorted-vbeln NE ls_hd-vbeln OR
             ls_sorted-posnr NE ls_hd-posnr.
            EXIT.
          ENDIF.
          IF  ls_sorted-jahrper GE p_sper  AND
              ls_sorted-jahrper LE p_eper  AND
              ls_sorted-curtp   EQ p_curtp AND
              ls_sorted-acc_principle EQ p_acc AND
              ls_sorted-categ   IN lr_categ.

            FREE lr_bwart. CLEAR lr_bwart.

            READ TABLE lt_dynfields INTO ls_dynfields
                                    WITH KEY categ = ls_sorted-categ
                                             ptyp  = ls_sorted-ptyp
                                             bwart = ls_sorted-bwart.
            IF sy-subrc IS INITIAL.
              SELECT 'I' AS sign,
                     'EQ' AS option,
                     bwart AS low,
                     ' ' AS high
                     INTO TABLE @lr_bwart
                FROM t156
                WHERE bustr EQ @ls_sorted-bwart.
            ENDIF.
            IF lr_bwart IS INITIAL.
              lr_bwart = VALUE #( sign = 'I' option = 'EQ' ( low = ' ' high = ' '  ) ).
            ENDIF.

            LOOP AT lt_dynfields INTO ls_dynfields
                                WHERE categ = ls_sorted-categ
                                  AND ptyp  = ls_sorted-ptyp
                                  AND bwart IN lr_bwart.
              CASE ls_dynfields-ftype.
                WHEN 'Q'. " Miktar
                  CLEAR lv_hkont_related.
                  IF ls_dynfields-bv IS NOT INITIAL.
                    IF ls_dynfields-hkont NE space.
                      lv_hkont_related+0(1) = 'X'.
                    ENDIF.
                    READ TABLE lt_ckmlmv005 INTO DATA(ls_005) WITH KEY kalnr = ls_sorted-process
                                                                       BINARY SEARCH.

*                    LOOP AT lt_mlcateg INTO ls_mlcateg
*                                      WHERE categ = ls_sorted-categ
*                                        AND ptyp  = ls_sorted-ptyp.
*                      IF ls_005-saknr_nd(3) EQ ls_mlcateg-hkont(3).
                    IF ls_005-saknr_nd(3) EQ ls_dynfields-hkont(3).
                      lv_hkont_related+1(1) = 'X'.
                      IF <fs_val_q> IS ASSIGNED . UNASSIGN <fs_val_q>. ENDIF.
                      DATA(lv_fname) = |<fs_line>-{ ls_dynfields-fieldname }|.

                      IF ( ls_sorted-jahrper = '2022001' OR ls_sorted-jahrper = '2022002' ) AND
                         ( ( ls_dynfields-categ = 'VP' AND ls_dynfields-ptyp = 'DC' ) OR
                           ( ( ls_dynfields-categ = 'ZU' AND ls_dynfields-ptyp = 'B+' ) AND
                             ( ls_sorted-bwart EQ '511' OR ls_sorted-bwart EQ '512' OR
                               ls_sorted-bwart EQ '561' OR ls_sorted-bwart EQ '562' ) ) ).

                        lv_fname = '<fs_line>-ACILIS_RVZ_MIKT'.
                        ASSIGN (lv_fname) TO <fs_val_q>.
                        IF <fs_val_q> IS ASSIGNED.
                          ADD ls_sorted-quant TO <fs_val_q>.
                        ENDIF.

                      ELSE.

                        ASSIGN (lv_fname) TO <fs_val_q>.
                        IF <fs_val_q> IS ASSIGNED.
                          ADD ls_sorted-quant TO <fs_val_q>.
                        ENDIF.

                      ENDIF.

                    ENDIF.
*                    ENDLOOP.
                  ELSE.
                    IF <fs_val_q> IS ASSIGNED . UNASSIGN <fs_val_q>. ENDIF.

                    IF ( ls_sorted-jahrper = '2022001' OR ls_sorted-jahrper = '2022002' ) AND
                       ( ( ls_dynfields-categ = 'VP' AND ls_dynfields-ptyp = 'DC' ) OR
                         ( ( ls_dynfields-categ = 'ZU' AND ls_dynfields-ptyp = 'B+' ) AND
                           ( ls_sorted-bwart EQ '511' OR ls_sorted-bwart EQ '512' OR
                             ls_sorted-bwart EQ '561' OR ls_sorted-bwart EQ '562' ) ) ).

                      lv_fname = '<fs_line>-ACILIS_RVZ_MIKT'.
                      ASSIGN (lv_fname) TO <fs_val_q>.
                      IF <fs_val_q> IS ASSIGNED.
                        ADD ls_sorted-quant TO <fs_val_q>.
                      ENDIF.

                    ELSE.

                      lv_fname = |<fs_line>-{ ls_dynfields-fieldname }|.
                      ASSIGN (lv_fname) TO <fs_val_q>.
                      IF <fs_val_q> IS ASSIGNED.
                        ADD ls_sorted-quant TO <fs_val_q>.
                      ENDIF.

                    ENDIF.

                  ENDIF.


                WHEN 'A'. " Tutar
                  IF lv_hkont_related EQ 'XX' OR
                     lv_hkont_related EQ space.
                    IF <fs_val> IS ASSIGNED. UNASSIGN <fs_val>. ENDIF.

                    IF ( ls_sorted-jahrper = '2022001' OR ls_sorted-jahrper = '2022002' ) AND
                       ( ( ls_dynfields-categ = 'VP' AND ls_dynfields-ptyp = 'DC' ) OR
                         ( ( ls_dynfields-categ = 'ZU' AND ls_dynfields-ptyp = 'B+' ) AND
                           ( ls_sorted-bwart EQ '511' OR ls_sorted-bwart EQ '512' OR
                             ls_sorted-bwart EQ '561' OR ls_sorted-bwart EQ '562' ) ) ).

                      lv_fname = '<fs_line>-ACILIS_RVZ_TTR'.
                      ASSIGN (lv_fname) TO <fs_val>.
                      IF <fs_val> IS ASSIGNED.
                        ADD ls_sorted-stval  TO <fs_val>.
                        ADD ls_sorted-prd    TO <fs_val>.
                        ADD ls_sorted-kdm    TO <fs_val>.
                        ADD ls_sorted-tpprd  TO <fs_val>.
                      ENDIF.

                    ELSE.

                      lv_fname = |<fs_line>-{ ls_dynfields-fieldname }|.
                      ASSIGN (lv_fname) TO <fs_val>.
                      IF <fs_val> IS ASSIGNED.
                        ADD ls_sorted-stval  TO <fs_val>.
                        ADD ls_sorted-prd    TO <fs_val>.
                        ADD ls_sorted-kdm    TO <fs_val>.
                        ADD ls_sorted-tpprd  TO <fs_val>.
                      ENDIF.

                    ENDIF.

                  ENDIF.
              ENDCASE.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
      ENDIF.

      READ TABLE lt_konts INTO DATA(ls_konts) WITH KEY matnr = ls_hd-matnr
                                                       bwkey = ls_hd-bwkey
                                                       bwtar = ls_hd-bwtar BINARY SEARCH.
      IF sy-subrc = 0.
        ASSIGN ('<fs_line>-KONTS')  TO <fs_konts>.
        <fs_konts> = ls_konts-konts.
      ENDIF.
      CLEAR: ls_sorted, ls_005, ls_konts.
    ENDLOOP.



    LOOP AT <fs_dyn_table> ASSIGNING <fs_line>.
      LOOP AT lt_dynfields INTO ls_dynfields
                           GROUP BY ( categ = ls_dynfields-categ
                                      ptyp  = ls_dynfields-ptyp )
                           INTO DATA(lr_fgrp).

        IF <fs_val_q> IS ASSIGNED . UNASSIGN <fs_val_q>. ENDIF.
        IF <fs_val>   IS ASSIGNED. UNASSIGN <fs_val>. ENDIF.

        LOOP AT GROUP lr_fgrp INTO DATA(ls_f_line).
          CASE ls_f_line-ftype.
            WHEN 'Q'.
              lv_fname = |<fs_line>-{ ls_f_line-fieldname }|.
              ASSIGN (lv_fname) TO <fs_val_q>.
            WHEN 'A'.
              lv_fname = |<fs_line>-{ ls_f_line-fieldname }|.
              ASSIGN (lv_fname) TO <fs_val>.
          ENDCASE.
        ENDLOOP.

        IF <fs_val_q> IS ASSIGNED AND
           ls_f_line-sign = '-'.
          <fs_val_q> = <fs_val_q> * -1.
        ENDIF.

        IF <fs_val> IS ASSIGNED AND
           ls_f_line-sign = '-'.
          <fs_val> = <fs_val> * -1.
        ENDIF.

        IF <fs_val_q> IS ASSIGNED AND
            <fs_val> IS ASSIGNED.
          IF <fs_val_q> IS INITIAL AND
             ( lr_fgrp-categ NE 'ND' AND lr_fgrp-categ NE 'ZU' AND lr_fgrp-categ NE 'VP' ).
            CLEAR: <fs_val>.
          ENDIF.
        ENDIF.
      ENDLOOP.

      lv_fname = '<fs_line>-OPENSTOCK_QUAN'.
      ASSIGN (lv_fname) TO <fs_openstock_quan>.
      lv_fname = '<fs_line>-OPENSTOCK_AMOUNT'.
      ASSIGN (lv_fname) TO <fs_openstock_amount>.
      lv_fname = '<fs_line>-ACILIS_RVZ_MIKT'.
      ASSIGN (lv_fname) TO <fs_acilis_rvz_mikt>.
      lv_fname = '<fs_line>-ACILIS_RVZ_TTR'.
      ASSIGN (lv_fname) TO <fs_acilis_rvz_ttr>.
      lv_fname = '<fs_line>-ferth'.
      ASSIGN (lv_fname) TO <fs_ferth>.
      lv_fname = '<fs_line>-MWSTOCK_QUAN'.
      ASSIGN (lv_fname) TO <fs_mwstock_quan>.

      lv_fname = '<fs_line>-ACILIS_RVZ_MIKT_T'.
      ASSIGN (lv_fname) TO <fs_val_q>.
      IF <fs_val_q> IS ASSIGNED.
        <fs_val_q> = <fs_openstock_quan> + <fs_acilis_rvz_mikt>.
      ENDIF.

      lv_fname = '<fs_line>-ACILIS_RVZ_TTR_T'.
      ASSIGN (lv_fname) TO <fs_val>.
      IF <fs_val> IS ASSIGNED.
        <fs_val> = <fs_openstock_amount> + <fs_acilis_rvz_ttr>.
      ENDIF.

      REPLACE ',' IN <fs_ferth> WITH '.'.

      DATA: _matnr TYPE char18.
      ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<matnr>).
      _matnr = |{ <matnr> ALPHA = IN }|.
      ASSIGN COMPONENT 'KALNR' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<kalnr>).

      lv_fname = '<fs_line>-MWSTOCK_QUAN'.
      ASSIGN (lv_fname) TO <fs_val>.
      IF <fs_val> IS ASSIGNED.
        LOOP AT lt_ckmlhd ASSIGNING FIELD-SYMBOL(<ckmlhd>) WHERE kalnr = <kalnr>
                                                             AND matnr = _matnr.
          ASSIGN COMPONENT 'CLOSESTOCK_QUAN' OF STRUCTURE <fs_line> TO <fs_closestock_quan>.
          IF <ckmlhd>-matkl IN r_matkl.
            <fs_val> = <fs_closestock_quan> * ( <fs_ferth> / 1000000 ).
          ELSE.
            IF <fs_ferth> IS INITIAL.
              <fs_val> = <fs_closestock_quan>.
            ELSE.
              <fs_val> = <fs_closestock_quan> / <fs_ferth>.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

      lv_fname = '<fs_line>-KALAN_QUAN'.
      ASSIGN (lv_fname) TO <fs_val>.
      IF <fs_val> IS ASSIGNED.
        <fs_val> = <fs_mwstock_quan> / p_kalgun.
      ENDIF.
    ENDLOOP.

    display( ).

  ENDMETHOD.

  METHOD rkkbmlmat.

    DATA: lt_submit TYPE tt_submit,
          ls_submit TYPE ty_submit.

    DATA: lr_memory TYPE REF TO data,
          lv_index  TYPE i.

    FIELD-SYMBOLS: <lt_memory> TYPE ANY TABLE,
                   <ls_memory> TYPE any,
                   <fs_field>  TYPE any.

    cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
                                            metadata = abap_false
                                            data     = abap_true ).
    IF lines( ir_matnr ) GT 1000.
      CLEAR: ir_matnr, ir_matnr[].
    ENDIF.
    SUBMIT rkkbmlmat WITH r_matnr IN ir_matnr
                     WITH p_werks EQ iv_bwkey
                     WITH p_poper EQ iv_poper
                     WITH p_bdatj EQ iv_bdatj
                     WITH p_curtp EQ iv_curtp
                     AND RETURN.                         "#EC CI_SUBMIT
    CLEAR lr_memory.
    UNASSIGN <lt_memory>.
    TRY.
        cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lr_memory ).
        ASSIGN lr_memory->* TO <lt_memory>.
      CATCH cx_salv_bs_sc_runtime_info.
    ENDTRY.
    cl_salv_bs_runtime_info=>clear_all( ).

    CHECK <lt_memory> IS ASSIGNED.
    LOOP AT <lt_memory> ASSIGNING <ls_memory>.
      CLEAR ls_submit.
      ls_submit = CORRESPONDING #( <ls_memory> ).
      CASE iv_type.
        WHEN 'A'.
          APPEND ls_submit TO mt_open.
        WHEN 'K'.
          APPEND ls_submit TO mt_close.
      ENDCASE.
      APPEND ls_submit TO mt_all.
    ENDLOOP.
  ENDMETHOD.                                             "#EC CI_VALPAR

  METHOD read_acdoca_m_extract.

    DATA:lt_tmp_acdoca_m_extract TYPE tt_acdoca_m_extract.

    SELECT kalnr matnr bwkey bwtar vmsl hsl
      FROM acdoca_m_extract
      INTO TABLE lt_tmp_acdoca_m_extract
      WHERE bwkey = iv_bwkey AND
            matnr IN ir_matnr AND
            fiscyearper <= iv_jahrper.

    CASE iv_type.
      WHEN 'A'.
        APPEND LINES OF lt_tmp_acdoca_m_extract TO mt_open_acdoca_m_extract.
      WHEN 'K'.
        APPEND LINES OF lt_tmp_acdoca_m_extract TO mt_close_acdoca_m_extract.
    ENDCASE.
    APPEND LINES OF lt_tmp_acdoca_m_extract TO mt_all_acdoca_m_extract.

  ENDMETHOD.                                             "#EC CI_VALPAR

  METHOD display.
    FIELD-SYMBOLS : <fs_dyn_table> TYPE STANDARD TABLE.
    CHECK NOT go_dyn_table IS INITIAL.
    ASSIGN go_dyn_table->* TO <fs_dyn_table>.
    IF <fs_dyn_table> IS INITIAL.
      MESSAGE i789(m7). RETURN.
    ENDIF.

    CALL SCREEN 0100.

  ENDMETHOD.

  METHOD on_user_command.
    DATA: lo_selections TYPE REF TO cl_salv_selections,
          lt_rows       TYPE salv_t_row.

    lo_selections = go_alv->get_selections( ).
    lt_rows = lo_selections->get_selected_rows( ).

  ENDMETHOD.

  METHOD crate_grid.

    FIELD-SYMBOLS : <fs_dyn_table> TYPE STANDARD TABLE.

    CHECK NOT go_dyn_table IS INITIAL.
    ASSIGN go_dyn_table->* TO <fs_dyn_table>.

    ls_layout-zebra      = 'X'.
    ls_layout-sel_mode   = 'C'.
    ls_layout-cwidth_opt = 'X'.

    ls_variant-report = sy-repid.
    ls_variant-username = sy-uname.

    lv_repid = sy-repid.
    CREATE OBJECT lr_docking_container
      EXPORTING
        side      = cl_gui_docking_container=>dock_at_left
        repid     = lv_repid
        dynnr     = '0100'
        extension = 2250.
*      ratio = 95.

    CREATE OBJECT lr_grid
      EXPORTING
        i_parent = lr_docking_container.

*    SET HANDLER lcl_report=->handle_toolbar      FOR g_grid.
    SET HANDLER me->handle_user_command FOR lr_grid.

    CALL METHOD lr_grid->set_table_for_first_display
      EXPORTING
        i_save          = 'A'
        is_layout       = ls_layout
        is_variant      = ls_variant
      CHANGING
        it_outtab       = <fs_dyn_table>
        it_fieldcatalog = mt_fieldcat.

*    CALL METHOD g_grid->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*    CALL METHOD g_grid->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD lr_grid->set_toolbar_interactive.

  ENDMETHOD.

  METHOD refresh_grid.
    DATA: ls_stable TYPE lvc_s_stbl,
          ls_layout TYPE lvc_s_layo.

    CHECK NOT iv_grid IS INITIAL.
    IF iv_stable EQ lc_x.
      ls_stable-row = lc_x.
      ls_stable-col = lc_x.
    ENDIF.

    CASE iv_grid.
      WHEN lr_grid.
        ls_layout = ls_layout.
    ENDCASE.

    CALL METHOD iv_grid->set_frontend_layout
      EXPORTING
        is_layout = ls_layout.

    CALL METHOD iv_grid->refresh_table_display
      EXPORTING
        is_stable      = ls_stable
        i_soft_refresh = iv_soft
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2.
  ENDMETHOD.

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'BACK' OR 'EXIT'.
        SET SCREEN 0.
        LEAVE SCREEN.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.