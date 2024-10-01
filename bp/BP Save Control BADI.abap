SE18->BUPA_FURTHER_CHECKS=>CHECK_CENTRAL

  METHOD if_ex_bupa_further_checks~check_central.

    CONSTANTS: BEGIN OF _cons,
                 pnam_01 TYPE progname  VALUE 'ZFI_000_I_BUPA_FURTHER_CHECKS',
                 form_01 TYPE form_name VALUE 'TAX_ID_NUMBER_CHECKS',
               END OF _cons.

    PERFORM (_cons-form_01)
      IN PROGRAM (_cons-pnam_01) USING iv_activity
                                       iv_partner
                                       iv_category
                                       iv_group
                                       is_data
                                       is_data_person
                                       is_data_organ
                                       is_data_group
                                       is_data_x
                                       is_data_person_x
                                       is_data_organ_x
                                       is_data_group_x
                                       is_but000
                                       iv_testrun
                                       iv_x_save
                                       iv_check_only_external_data
                              CHANGING et_return.

  ENDMETHOD.


  *&---------------------------------------------------------------------*
*& INCLUDE ZFI_000_I_BUPA_FURTHER_CHECKS
*&---------------------------------------------------------------------*
PROGRAM zfi_000_i_bupa_further_checks.

*&---------------------------------------------------------------------*
*& Form TAX_ID_NUMBER_CHECKS
*&---------------------------------------------------------------------*
FORM tax_id_number_checks  USING iv_activity TYPE bu_aktdb
                                 iv_partner TYPE bu_partner
                                 iv_category TYPE bapibus1006_head-partn_cat
                                 iv_group TYPE bapibus1006_head-partn_grp
                                 is_data TYPE bapibus1006_central
                                 is_data_person TYPE bapibus1006_central_person
                                 is_data_organ TYPE bapibus1006_central_organ
                                 is_data_group TYPE bapibus1006_central_group
                                 is_data_x TYPE bapibus1006_central_x
                                 is_data_person_x TYPE bapibus1006_central_person_x
                                 is_data_organ_x TYPE bapibus1006_central_organ_x
                                 is_data_group_x TYPE bapibus1006_central_group_x
                                 is_but000 TYPE but000
                                 iv_testrun TYPE boole-boole
                                 iv_x_save TYPE ad_save
                                 iv_check_only_external_data
                         CHANGING et_return TYPE bapiret2_t.

  DATA: t_but000        TYPE TABLE OF but000,
        s_but000        TYPE but000,
        t_taxdat        TYPE TABLE OF dfkkbptaxnum,
        t_land_rng      TYPE RANGE OF t005-land1,
        v_addr_data(40) TYPE c VALUE '(SAPLSZA1)ADDR1_DATA-COUNTRY',
        v_country       TYPE addr2_data-country,
        v_addr_dat      TYPE bapibus1006_address.

  FIELD-SYMBOLS: <fs_country> TYPE any.

  CONSTANTS: BEGIN OF _msg,
               id      TYPE symsgid    VALUE 'ZFI_000',
               success TYPE bapi_mtype VALUE 'S',
               error   TYPE bapi_mtype VALUE 'E',
               warning TYPE bapi_mtype VALUE 'W',
               info    TYPE bapi_mtype VALUE 'I',
               abort   TYPE bapi_mtype VALUE 'A',
             END OF _msg.

  CONSTANTS: BEGIN OF _cons,
               create      TYPE numc2 VALUE '01',
               change      TYPE numc2 VALUE '02',
               display     TYPE numc2 VALUE '06',
               except_tax  TYPE bptaxnum VALUE '1111111111',
               land_tr     TYPE land1 VALUE 'TR',
               taxtype_tr2 TYPE bptaxtype VALUE 'TR2',
             END OF _cons.


*-&Aktivite tipi: 01 = Yarat, 02 = Değiştir, 06 = Görüntüle
  CHECK iv_activity EQ _cons-create OR iv_activity EQ _cons-change.

*-&Muhatap genel bilgileri(Read memory)->
  CLEAR: s_but000, t_but000[].
  CALL FUNCTION 'BUP_BUPA_BUT000_GET'
    IMPORTING
      e_but000  = s_but000
    TABLES
      et_but000 = t_but000.

*-&Muhataba ait adres bilgileri->
  CLEAR: v_country.
  ASSIGN (v_addr_data) TO <fs_country>.
  IF <fs_country> IS ASSIGNED AND <fs_country> <> space.
    MOVE <fs_country> TO v_country.
  ELSE.
    CLEAR: v_addr_dat.
    CALL FUNCTION 'BAPI_BUPA_ADDRESS_GETDETAIL'
      EXPORTING
        businesspartner = iv_partner
      IMPORTING
        addressdata     = v_addr_dat.
    v_country = v_addr_dat-country.
  ENDIF.

*-&Muhataba giriş yapılan vergi kimlik bilgilerinin getirilmesi->
  FREE: t_taxdat.
  CALL FUNCTION 'BUP_BUPA_TAX_GET'
    TABLES
      et_tax              = t_taxdat
    EXCEPTIONS
      no_taxnumbers_found = 1
      OTHERS              = 2.

*-&KONTROL-1: İlgili 8 ülkeden farklı değerler için vergi kimlik no girişi zorunlu olmalıdır. (UA, MA, PS, IQ, QA, LY, SY, JO)->
  FREE: t_land_rng.
  t_land_rng = VALUE #( sign = 'E' option = 'EQ' ( low = 'UA' ) ( low = 'MA' ) ( low = 'PS' ) ( low = 'IQ' ) ( low = 'QA' ) ( low = 'LY' ) ( low = 'SY' ) ( low = 'JO' ) ).
  IF v_country IN t_land_rng AND t_taxdat[] IS INITIAL.
    APPEND VALUE #( type = _msg-error id = _msg-id number = 001 message_v1 = v_country ) TO et_return. EXIT.
  ENDIF.

  LOOP AT t_taxdat REFERENCE INTO DATA(r_taxdat).
*-&KONTROL-2: Ülkesi TR olanlar için ise 1111111111 olmayan değerler için mükerrerlik kontrolü gerçekleştirilmelidir->
    IF v_country EQ _cons-land_tr AND r_taxdat->taxtype EQ _cons-taxtype_tr2 AND r_taxdat->taxnum <> _cons-except_tax.
      SELECT SINGLE * FROM dfkkbptaxnum
        INTO @DATA(v_taxnumold)
          WHERE partner NE @r_taxdat->partner
            AND taxtype EQ @r_taxdat->taxtype
            AND taxnum  EQ @r_taxdat->taxnum.
      IF sy-subrc EQ 0.
        APPEND VALUE #( type = _msg-error id = _msg-id number = 002 message_v1 = r_taxdat->taxnum message_v2 = v_taxnumold-partner ) TO et_return.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.