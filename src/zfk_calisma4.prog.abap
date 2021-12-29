*&---------------------------------------------------------------------*
*& Report ZFK_CALISMA4
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_calisma4.

CLASS student DEFINITION.

  PUBLIC SECTION.

    DATA : name(15) TYPE c.


    METHODS :
      set_name
        IMPORTING
          p1 TYPE char25
          p2 TYPE char25
        EXPORTING
          p3 TYPE char25
        CHANGING
          p4 TYPE char40,
      write_name,
      returning_name
        RETURNING VALUE(p5) TYPE char50.
ENDCLASS.

CLASS student IMPLEMENTATION.

  METHOD set_name.
    name = p1.
    p3 = |{ p1 } { p2 }|.
    p4 = |{ p1 } && { p2 }|.

  ENDMETHOD.

  METHOD write_name.
    WRITE / name.
  ENDMETHOD.

  METHOD returning_name.
    p5 = 'return denemesi'.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA fullname  TYPE char25.
  DATA fullname2 TYPE char40.
  DATA returnvalue TYPE char40.

  DATA go_student TYPE REF TO student.

  CREATE OBJECT go_student.

  CALL METHOD go_student->set_name(
    EXPORTING
      p1 = 'feyzanur'
      p2 = 'karakaş'
    IMPORTING
      p3 = fullname
    CHANGING
      p4 = fullname2 ).

  CALL METHOD go_student->write_name.

  WRITE / fullname.
  WRITE / fullname2.

*  returnvalue = go_student->returning_name( ).
  go_student->returning_name(
  RECEIVING
  p5 = returnvalue ).
  WRITE / returnvalue.
