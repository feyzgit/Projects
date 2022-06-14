 SELECT
         CASE WHEN @iv_mtype EQ @mc_msg-success THEN @icon_green_light ELSE @icon_red_light END AS light,
         yloomis_t02~bukrs, yloomis_t02~prs_id, yloomis_t02~prs_kind, yloomis_t02~guid,
         yloomis_t02~awkey, yloomis_t02~erdat, yloomis_t02~erzet, yloomis_t02~ernam,
         @icon_message_faulty_orphan AS msgshw,
         substring( yloomis_t02~awkey,1,10 ) AS belnr,
         CAST( substring( yloomis_t02~awkey,15,4 ) AS NUMC( 4 ) ) AS gjahr
           FROM yloomis_t02
               WHERE yloomis_t02~bukrs EQ @iv_bukrs
                 AND yloomis_t02~erdat IN @iv_erdat
                 AND yloomis_t02~prs_id IN @s_prsid
                 AND yloomis_t02~erzet IN @iv_erzet
                 AND yloomis_t02~ernam IN @iv_ernam
                 AND yloomis_t02~mtype EQ @iv_mtype
             INTO TABLE @DATA(t_basedat).

    IF NOT t_basedat IS INITIAL.
      SELECT bukrs, belnr, gjahr, stblg, stjah
        FROM bkpf
        FOR ALL ENTRIES IN @t_basedat
        WHERE bukrs EQ @iv_bukrs
          AND belnr EQ @t_basedat-belnr
          AND gjahr EQ @t_basedat-gjahr
          AND stblg NE @space
          INTO TABLE @mt_revdat.
    ENDIF.

    LOOP AT t_basedat ASSIGNING FIELD-SYMBOL(<basedat>).

      APPEND INITIAL LINE TO mt_outdat ASSIGNING FIELD-SYMBOL(<outdat>).
      <outdat> = CORRESPONDING #( <basedat> ).

      READ TABLE mt_revdat ASSIGNING FIELD-SYMBOL(<revdat>) WITH KEY bukrs = <basedat>-bukrs
                                                                     belnr = <basedat>-belnr
                                                                     gjahr = <basedat>-gjahr.
      IF sy-subrc IS INITIAL.
        <outdat>-awkey_rev = |{ <revdat>-stblg }{ <revdat>-bukrs }{ <revdat>-stjah }|.
      ENDIF.

    ENDLOOP.