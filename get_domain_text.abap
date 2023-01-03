get_domain_text
        IMPORTING
          im_dname       TYPE dd07l-domname
          im_value       TYPE clike
        RETURNING
          VALUE(rv_text) TYPE val_text
        EXCEPTIONS
          contains_error.


 METHOD get_domain_text.
    DATA: lt_tab TYPE TABLE OF dd07v.

    FREE: lt_tab.
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = im_dname
        text           = abap_true
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_tab
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    READ TABLE lt_tab INTO DATA(wa_tab) WITH KEY domvalue_l = im_value.
    IF sy-subrc IS INITIAL.
      rv_text = wa_tab-ddtext.
    ELSE.
    ENDIF.
  ENDMETHOD.



          get_domain_text(
          EXPORTING
            im_dname       = 'ZFI_017_D007'
            im_value       = <outdat>-statu
          RECEIVING
            rv_text        = <outdat>-statu_desc
          EXCEPTIONS
            contains_error = 1
            OTHERS         = 2 ).          