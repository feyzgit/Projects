FORM assignment_change TABLES t_bkpf    STRUCTURE bkpf
                              t_bseg    STRUCTURE bseg
                              t_bkpfsub STRUCTURE bkpf_subst
                              t_bsegsub STRUCTURE bseg_subst
                              t_bsec    STRUCTURE bsec
                        USING i_bkdf    TYPE bkdf
                     CHANGING i_bkdfsub TYPE bkdf_subst.


  LOOP AT t_bsegsub REFERENCE INTO DATA(lr_bsegsub) WHERE hkont+00(03) EQ '646'
                                                       OR hkont+00(03) EQ '656'.

    LOOP AT t_bseg WHERE koart EQ 'K'
                      OR koart EQ 'D'
                      OR koart EQ 'S'
                     AND hkont+00(03) NE '646'
                     AND hkont+00(03) NE '656' .

      READ TABLE t_bsegsub ASSIGNING FIELD-SYMBOL(<bsegsub>) WITH KEY tabix = sy-tabix.
      IF sy-subrc IS INITIAL.
        CASE t_bseg-koart .
          WHEN 'K'.
            <bsegsub>-zuonr = |K - { t_bseg-lifnr }|.
          WHEN 'D'.
            <bsegsub>-zuonr = |D - { t_bseg-kunnr }|.
          WHEN 'S'.
            <bsegsub>-zuonr = |S - { t_bseg-hkont }|.
        ENDCASE.
      ENDIF.
      EXIT.
    ENDLOOP.

    CASE t_bseg-koart .
      WHEN 'K'.
        lr_bsegsub->zuonr = |K - { t_bseg-lifnr }|.
      WHEN 'D'.
        lr_bsegsub->zuonr = |D - { t_bseg-kunnr }|.
      WHEN 'S'.
        lr_bsegsub->zuonr = |S - { t_bseg-hkont }|.
    ENDCASE.

  ENDLOOP.
ENDFORM.