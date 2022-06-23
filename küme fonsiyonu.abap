*-> begin of fkarakas 16.06.2022

  DATA: lt_lines_basic TYPE TABLE OF rgsbv,
        t_blart_rng    TYPE RANGE OF bkpf-blart,
        tabix          TYPE sy-tabix.

*küme verileri çekilir.
  CALL FUNCTION 'G_SET_FETCH'
    EXPORTING
      class              = '0000'
      no_authority_check = 'X'
      setnr              = 'ZFI_BLART'
    TABLES
      set_lines_basic    = lt_lines_basic
    EXCEPTIONS
      no_authority       = 1
      set_is_broken      = 2
      set_not_found      = 3
      OTHERS             = 4.

  LOOP AT lt_lines_basic REFERENCE INTO DATA(lines).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = lines->from ) TO t_blart_rng.
  ENDLOOP.

  READ TABLE t_bkpf ASSIGNING FIELD-SYMBOL(<bkpf>) INDEX 1.
  IF sy-subrc IS INITIAL.
    IF <bkpf>-blart IN t_blart_rng.
      LOOP AT t_bseg ASSIGNING FIELD-SYMBOL(<bseg>) WHERE hkont CP '360*'.
        tabix = sy-tabix.
        READ TABLE t_bseg INTO DATA(s_bseg) WITH KEY koart = 'K'.
        IF sy-subrc IS INITIAL.
          MODIFY t_bsegsub FROM VALUE #( zuonr = s_bseg-lifnr ) INDEX tabix TRANSPORTING zuonr.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
*<- end of fkarakas 16.06.2022