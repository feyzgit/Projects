*&---------------------------------------------------------------------*
*& Include          ZFK_ODEV7_TOP_CLASS
*&---------------------------------------------------------------------*
CLASS lcl_base DEFINITION.
  PUBLIC SECTION.
    METHODS:
      get_data,
      merge,
      display,
      build_html
        RETURNING VALUE(rt_html) TYPE w3_htmltab.
ENDCLASS.

CLASS lcl_base IMPLEMENTATION.
  METHOD get_data.
  ENDMETHOD.

  METHOD merge.


  ENDMETHOD.

  METHOD display.

    CHECK go_cust IS NOT BOUND.

    CREATE OBJECT go_cust
      EXPORTING
        container_name = 'CC_ALV'
      EXCEPTIONS
        OTHERS         = 1.

    CHECK sy-subrc IS INITIAL.

    CREATE OBJECT go_split
      EXPORTING
        parent  = go_cust
        rows    = 2
        columns = 2
      EXCEPTIONS
        OTHERS  = 1.

    CHECK sy-subrc IS INITIAL.

    CALL METHOD go_split->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = go_cont1.

*    DATA: doc_url(80), lt_html TYPE TABLE OF w3_html.
*
*    lt_html = me->build_html( ).
*
*    go_html->load_data(
*    IMPORTING
*      assigned_url = doc_url
*    CHANGING
*      data_table = lt_html ).
*
*    go_html->show_url(
*      EXPORTING
*        url = doc_url ).


    CHECK sy-subrc EQ 0.
*    me->merge( ).
*    CALL METHOD go_alv->set_table_for_first_display
*      EXPORTING
*        is_layout       = gs_layout
*      CHANGING
*        it_fieldcatalog = gt_fieldcat[]
*        it_outtab       = gt_banka.

    CALL METHOD go_split->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = go_cont2.

    CALL METHOD go_split->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = go_cont3.

    CALL METHOD go_split->get_container
      EXPORTING
        row       = 2
        column    = 2
      RECEIVING
        container = go_cont4.

    CREATE OBJECT go_alv
      EXPORTING
        i_parent = go_cont4
      EXCEPTIONS
        OTHERS   = 1.



  ENDMETHOD.

  METHOD build_html.



  ENDMETHOD.

ENDCLASS.
