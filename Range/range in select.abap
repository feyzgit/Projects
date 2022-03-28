REPORT zfk_calisma7.

DATA lt_t001 TYPE STANDARD TABLE OF t001 WITH DEFAULT KEY.
DATA r_bukrs TYPE RANGE OF t001-bukrs.
DATA r_bukrs2 TYPE RANGE OF t001-bukrs.
DATA v_sayac TYPE numc4.
DATA flag TYPE flag.

DO 100000 TIMES.

  ADD 1 TO v_sayac.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = v_sayac ) TO r_bukrs.


ENDDO.

WHILE flag IS INITIAL.

  FREE r_bukrs2.

  LOOP AT r_bukrs INTO DATA(s_bukrs) FROM 1 TO 5000.
    APPEND s_bukrs TO r_bukrs2.
  ENDLOOP.
  DELETE r_bukrs FROM 1 TO 5000.

  SELECT bukrs,
         butxt,
         ort01,
         land1,
         waers,
         spras
    FROM t001
    APPENDING CORRESPONDING FIELDS OF TABLE @lt_t001
    WHERE bukrs IN @r_bukrs2.

  IF r_bukrs IS INITIAL.
    flag = 'X'.
  ENDIF.


ENDWHILE.