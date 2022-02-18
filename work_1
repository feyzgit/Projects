REPORT zfk_calisma.

DATA: gt_ogrenci TYPE TABLE OF zfk_odev1,
      gs_ogrenci TYPE zfk_odev1.





DATA : BEGIN OF lt_line OCCURS 0,
         column1 TYPE i, column2 TYPE i,
       END OF lt_line.

DATA : ls_line  LIKE lt_line,
       ls_line2 LIKE LINE OF lt_line.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SKIP 5.
PARAMETERS:
  sayı  TYPE i,
  topla RADIOBUTTON GROUP g1 DEFAULT 'X',
  cıkar RADIOBUTTON GROUP g1,
  carp  RADIOBUTTON GROUP g1,
  bol   RADIOBUTTON GROUP g1.

SELECTION-SCREEN END OF BLOCK b1.


START-OF-SELECTION.

  gs_ogrenci-stuıd = 1.
  gs_ogrenci-stuname = 'Ali'.
  gs_ogrenci-stusurname = 'Veli'.
  gs_ogrenci-midterm1 = '50'.

  APPEND gs_ogrenci TO gt_ogrenci.

  gs_ogrenci-stuıd = 2.
  gs_ogrenci-stuname = 'Ayşe'.
  gs_ogrenci-stusurname = 'Türk'.
  gs_ogrenci-midterm1 = '70'.

  APPEND gs_ogrenci TO gt_ogrenci.

  IF topla = 'X'.
    PERFORM ogrenci_loop1.
    PERFORM topla.
*    PERFORM topla USING sayı.
  ELSEIF cıkar = 'X'.
    PERFORM ogrenci_loop1.
    PERFORM cıkar USING sayı.
  ELSEIF carp ='X'.
    PERFORM ogrenci_loop1.
    PERFORM carp USING sayı.
  ELSEIF bol ='X'.
    PERFORM ogrenci_loop1.
    PERFORM bol USING sayı.

  ENDIF.


*WRITE: ' No', 10 'Adı', 20 'Soyadı',30 'Puanı'.
*  ULINE.
*  LOOP AT  gt_ogrenci INTO gs_ogrenci.
*
*  WRITE :/ gs_ogrenci-stuıd,10 gs_ogrenci-stuname, 20 gs_ogrenci-stusurname, 30 gs_ogrenci-midterm1.
*    MODIFY gt_ogrenci FROM gs_ogrenci.
*  ENDLOOP.

*  PERFORM ogrenci_loop1.
*
*  LOOP AT  gt_ogrenci INTO gs_ogrenci.
**    gs_ogrenci-midterm1 = gs_ogrenci-midterm1 / 2.
*    WRITE :/ gs_ogrenci-stuıd,10 gs_ogrenci-stuname, 20 gs_ogrenci-stusurname, 30 gs_ogrenci-midterm1 .
*    MODIFY gt_ogrenci FROM gs_ogrenci.
*  ENDLOOP.


*  PERFORM ogrenci_loop1.
  LOOP AT  gt_ogrenci INTO gs_ogrenci.

*    gs_ogrenci-midterm1 = gs_ogrenci-midterm1 * 2.
    WRITE :/ gs_ogrenci-stuıd,10 gs_ogrenci-stuname, 20 gs_ogrenci-stusurname, 30 gs_ogrenci-midterm1.
    MODIFY gt_ogrenci FROM gs_ogrenci.
  ENDLOOP.

*  PERFORM ogrenci_loop1.

FORM ogrenci_loop1.

  WRITE: ' No', 10 'Adı', 20 'Soyadı',30 'Puanı'.
  CLEAR gs_ogrenci.
  LOOP AT gt_ogrenci INTO gs_ogrenci.
    WRITE :/ gs_ogrenci-stuıd,10 gs_ogrenci-stuname, 20 gs_ogrenci-stusurname, 30 gs_ogrenci-midterm1.

  ENDLOOP.

  SKIP 3.

ENDFORM.


*
*FORM topla USING sayı.
*  LOOP AT gt_ogrenci INTO gs_ogrenci.
*    gs_ogrenci-midterm1 = gs_ogrenci-midterm1 + sayı.
*    MODIFY gt_ogrenci FROM gs_ogrenci.
*  ENDLOOP.
*
*ENDFORM.

FORM topla.
  LOOP AT gt_ogrenci INTO gs_ogrenci.
    gs_ogrenci-midterm1 = gs_ogrenci-midterm1 + sayı.
    MODIFY gt_ogrenci FROM gs_ogrenci.
  ENDLOOP.

ENDFORM.

FORM cıkar USING sayı.
  LOOP AT gt_ogrenci INTO gs_ogrenci.
    gs_ogrenci-midterm1 = gs_ogrenci-midterm1 - sayı.
    MODIFY gt_ogrenci FROM gs_ogrenci.
  ENDLOOP.
ENDFORM.

FORM carp USING sayı.
  LOOP AT gt_ogrenci INTO gs_ogrenci.
    gs_ogrenci-midterm1 = gs_ogrenci-midterm1 * sayı.
    MODIFY gt_ogrenci FROM gs_ogrenci.
  ENDLOOP.
ENDFORM.

FORM bol USING sayı.
  LOOP AT gt_ogrenci INTO gs_ogrenci.
    gs_ogrenci-midterm1 = gs_ogrenci-midterm1 / sayı.
    MODIFY gt_ogrenci FROM gs_ogrenci.
  ENDLOOP.
ENDFORM.
