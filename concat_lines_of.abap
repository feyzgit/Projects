METHOD _write_other_personal.

    TYPES:
      BEGIN OF ty_ename,
        ename TYPE emnam,
      END OF ty_ename,
      tt_ename TYPE STANDARD TABLE OF ty_ename WITH DEFAULT KEY.

    _display_other_personal(
      EXPORTING
        iv_persondat = iv_persondat
        iv_dispay    = abap_false
      IMPORTING
        ev_person    = DATA(t_persondat)
      EXCEPTIONS
        handle_error = 1
        OTHERS       = 2 ).
    IF sy-subrc IS INITIAL.
      rv_persondat = concat_lines_of( table = VALUE tt_ename( FOR <wa> IN t_persondat ( CONV #( <wa>-ename ) ) ) sep = '; ' ).
    ENDIF.

  ENDMETHOD.


  