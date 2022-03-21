*&---------------------------------------------------------------------*
*& Report ZFK_TUTOR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_tutor.

TYPES:
  BEGIN OF itab,
    id      TYPE int4,
    name    TYPE char10,
    surname TYPE char10,
  END OF itab.

DATA: i_itab   TYPE STANDARD TABLE OF itab WITH DEFAULT KEY,
      s_itab   TYPE itab,
      r_itab   TYPE REF TO itab,
      lv_sayac TYPE int4.

DO 5 TIMES.
  APPEND INITIAL LINE TO i_itab REFERENCE INTO r_itab.

  ADD 1 TO lv_sayac.
  r_itab->id = lv_sayac.
  r_itab->name = 'Feyza'.
  r_itab->surname = 'Karakas'.

ENDDO.

*DO 5 TIMES.
*
*  ADD 1 TO lv_sayac.
*  s_itab-id = lv_sayac.
*  s_itab-name = 'Feyza'.
*  s_itab-surname = 'Karakas'.
*
*  APPEND s_itab TO i_itab.
*
*ENDDO.

*DO 5 TIMES.
*
*  ADD 1 TO lv_sayac.
*  s_itab-id = lv_sayac.
*  s_itab-name = 'Feyza'.
*  s_itab-surname = 'Karakas'.
*
*  MODIFY i_itab FROM s_itab  TRANSPORTING id name surname.
*
*
*ENDDO.

*&---------------------------------------------------------------------*
*& Report ZAATAN_TUTORIAL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*REPORT zaatan_tutorial.

*TYPES: BEGIN OF ty_itab,
*         id      TYPE int4,
*         name    TYPE char20,
*         surname TYPE char10,
*       END OF ty_itab,
*       tt_itab TYPE STANDARD TABLE OF ty_itab WITH DEFAULT KEY.
*
*DATA: t_itab TYPE tt_itab,
*      s_itab TYPE ty_itab,
*      r_itab TYPE REF TO ty_itab,
*      _id    TYPE int4.
*
*FIELD-SYMBOLS: <fs_itab> TYPE ty_itab.
*
*DO 10 TIMES.
*  CLEAR: s_itab.
*  ADD 1 TO _id.
*  t_itab = VALUE #( BASE t_itab ( id = _id name = 'Ali' surname = 'Cin' ) ).
**  APPEND INITIAL LINE TO t_itab REFERENCE INTO r_itab.
**  r_itab->id = _id.
**  r_itab->name = 'Ali'.
**  r_itab->surname = 'Cin'.
**  s_itab-id = _id.
**  s_itab-name = 'Ali'.
**  s_itab-surname = 'Cin'.
**  APPEND s_itab TO t_itab.
*ENDDO.
*
**LOOP AT t_itab INTO s_itab WHERE id = 4.
**  s_itab-surname = 'ABC'.
**  MODIFY t_itab FROM s_itab.
**ENDLOOP.
*
*LOOP AT t_itab ASSIGNING <fs_itab> WHERE id = 4.
*  <fs_itab>-surname = 'ABC'.
*ENDLOOP.






BREAK-POINT.