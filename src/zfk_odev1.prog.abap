*&---------------------------------------------------------------------*
*& Report ZFK_ODEV1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_odev1.

DATA: gt_student TYPE TABLE OF zfk_odev1,
      gs_student TYPE zfk_odev1,
      final      TYPE i.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: id       TYPE i,
            name     TYPE char20,
            surname  TYPE char20,
            midterm1 TYPE i,
            midterm2 TYPE i.

SELECTION-SCREEN END OF BLOCK b1.



gs_student-stuid = id.
gs_student-stuname = name.
gs_student-stusurname = surname.
gs_student-midterm1 = midterm1.
gs_student-midterm2 = midterm2.
gs_student-final = final.

AT SELECTION-SCREEN OUTPUT.
  PERFORM f_kayıt_basarılı.
  CLEAR : id, name, surname, midterm1, midterm2.



START-OF-SELECTION.


  SELECT SINGLE * FROM zfk_odev1 INTO gs_student WHERE stuid EQ id.
  IF id eq gs_student-stuid.
    CALL SCREEN 0100 STARTING AT 10 10
                 ENDING AT   100 10.
    CLEAR gs_student.
  ELSE.
    INSERT zfk_odev1 FROM gs_student.

  ENDIF.


FORM f_kayıt_basarılı.
  WRITE : 30 ' Kayıt başarılı.' , /5 ' No:',10' Adı',35' Soyadı',60' Midterm 1',70' Midterm 2',85' Final'.

  final = ( midterm1 + midterm2 ) / 2.
  WRITE:/ gs_student-stuid,gs_student-stuname,gs_student-stusurname,gs_student-midterm1,gs_student-midterm2, final.


ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR 'Aynı Öğrenci Numarası Girildi.'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN '&CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN '&OK'.
      MODIFY zfk_odev1 FROM gs_student .
      LEAVE TO SCREEN 0.


  ENDCASE.

ENDMODULE.
