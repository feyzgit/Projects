CLASS zfk_cl_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA mv_name TYPE char25 .

    METHODS set_name
      IMPORTING
        !im_name TYPE char25 .
    METHODS get_name .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZFK_CL_TEST IMPLEMENTATION.


  METHOD get_name.
    WRITE mv_name.
  ENDMETHOD.


  METHOD set_name.
    mv_name = im_name.
  ENDMETHOD.
ENDCLASS.
