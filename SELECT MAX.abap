    SELECT envno, timez, apprv, name_text
      FROM zfi_017_t09 AS t09
      LEFT OUTER JOIN puser002 AS usr ON usr~bname = t09~apprv
      WHERE t09~bukrs IN @iv_bukrs
        AND t09~envno IN @iv_envno
        AND t09~timez = ( SELECT MAX( maxd~timez ) FROM zfi_017_t09 AS maxd WHERE maxd~envno = t09~envno )
      INTO TABLE @DATA(t_owner).