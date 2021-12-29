*&---------------------------------------------------------------------*
*& Report ZFK_OO_ODEV1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_oo_odev1.

INCLUDE zfk_oo_odev1_top_data.
INCLUDE zfk_oo_odev1_top_screen.
INCLUDE zfk_oo_odev1_top_class.
INCLUDE zfk_oo_odev1_pai.
INCLUDE zfk_oo_odev1_pbo.

INITIALIZATION.

  CONCATENATE icon_display 'Tabloyu Görüntüle'
  INTO btn1 .
  CONCATENATE icon_change 'Tabloyu Değiştir'
  INTO btn2 .

AT SELECTION-SCREEN.
CASE sy-ucomm.
WHEN 'BTN1'.
CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
EXPORTING
          action    = 'S'
          view_name = 'ZFK_T_ISKONTO'
*       TABLES
EXCEPTIONS
OTHERS    = 1.
WHEN 'BTN2'.
CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
EXPORTING
          action    = 'U'
          view_name = 'ZFK_T_ISKONTO'
*       TABLES
EXCEPTIONS
OTHERS    = 1.
WHEN OTHERS.
ENDCASE.


START-OF-SELECTION.

  CREATE OBJECT go_bkpf.
  CREATE OBJECT go_bseg.

  CALL METHOD go_bkpf->get_data.
  CALL METHOD go_bseg->get_data.

  CALL SCREEN 0100.
