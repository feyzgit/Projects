   DATA: t_sbewebe_rng TYPE RANGE OF vtbfhapo-sbewebe.



   t_sbewebe_rng = VALUE #( sign = 'I' option = 'NE' ( low = '3' )
                                                      ( low = '4' ) ).
    SELECT vtpo~bukrs,
           t001~butxt,
           vtpo~rfha AS trm_islmno,
           vtpo~dzterm AS trm_odmtrh,
           vtpo~present_id AS trm_ibraz,
           vtpo~bzbetr AS tutar,
           vtpo~wzbetr AS waers,
           vtpo~dcrdat,
           vtpo~tcrtim,
           tlct~lc_number AS trm_akrno,
           tlct~benficiary AS lifnr,
           lfa1~name1,
           '04' AS odm_tur
           FROM vtbfha AS vtha
           INNER JOIN vtbfhapo AS vtpo ON vtpo~bukrs = vtha~bukrs AND
                                          vtpo~rfha = vtha~rfha
           INNER JOIN tlct_activity AS tlct ON tlct~bukrs = vtha~bukrs AND
                                               tlct~rfha = vtha~rfha AND
                                               tlct~rfhazu = vtha~rfhazul
           LEFT JOIN t001 ON t001~bukrs = vtha~bukrs
           LEFT JOIN lfa1 ON lfa1~lifnr = tlct~benficiary
           INTO TABLE @DATA(t_trmdat)
           WHERE vtha~bukrs EQ @p_comp02
             AND vtha~rfha IN @s_rfha
             AND vtha~sgsart EQ 'A02'
             AND vtpo~dzterm IN @s_dzterm
             AND vtpo~sfhazba EQ '1850'
             AND vtpo~sbewebe IN @t_sbewebe_rng
             AND tlct~benficiary IN @s_vndr01
             AND vtpo~dcrdat EQ ( SELECT MAX( dcrdat ) FROM vtbfhapo AS a WHERE a~bukrs = vtpo~bukrs AND
                                                                                a~rfha = vtpo~rfha AND
                                                                                a~rfhazu = vtpo~rfhazu )
             AND vtpo~tcrtim EQ ( SELECT MAX( tcrtim ) FROM vtbfhapo AS b WHERE b~bukrs = vtpo~bukrs AND
                                                                                b~rfha = vtpo~rfha AND
                                                                                b~rfhazu = vtpo~rfhazu ).