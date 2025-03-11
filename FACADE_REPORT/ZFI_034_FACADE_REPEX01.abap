*&---------------------------------------------------------------------*
*& Include          ZFI_034_FACADE_REPEX01
*&---------------------------------------------------------------------*
CLASS lcl_repex01_application DEFINITION CREATE PUBLIC INHERITING FROM lcl_abstract_reporting.

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF mc_msg,
        id      TYPE symsgid    VALUE 'ZFI_034',
        success TYPE bapi_mtype VALUE 'S',
        error   TYPE bapi_mtype VALUE 'E',
        warning TYPE bapi_mtype VALUE 'W',
        info    TYPE bapi_mtype VALUE 'I',
        abort   TYPE bapi_mtype VALUE 'A',
      END OF mc_msg.

    CLASS-DATA:
      mt_outputdat TYPE STANDARD TABLE OF zfi_034_s01.

    CLASS-METHODS:
      class_constructor.

    METHODS:
      _handle_hotspot_click REDEFINITION,
      _handle_toolbar_set REDEFINITION,
      _handle_user_command REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      _retrieve_dat REDEFINITION,
      _processing REDEFINITION,
      _update_fieldcat REDEFINITION.

ENDCLASS.

CLASS lcl_repex01_application IMPLEMENTATION.
  METHOD class_constructor.

  ENDMETHOD.
  METHOD _retrieve_dat.

    BREAK-POINT.
  ENDMETHOD.
  METHOD _processing.

    TRY.
        table = REF #( mt_outputdat ).
      CATCH zcx_tjk.
        RAISE EXCEPTION TYPE zcx_tjk MESSAGE s007.
    ENDTRY.

  ENDMETHOD.
  METHOD _update_fieldcat.
    LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<fcat>).
      CASE <fcat>-fieldname.
        WHEN 'XXX'.
        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.
  ENDMETHOD.
  METHOD _handle_hotspot_click.

  ENDMETHOD.
  METHOD _handle_toolbar_set.

    DELETE e_object->mt_toolbar
           WHERE function = '&LOCAL&COPY_ROW'
              OR function = '&LOCAL&APPEND'
              OR function = '&LOCAL&INSERT_ROW'
              OR function = '&LOCAL&DELETE_ROW'
              OR function = '&LOCAL&CUT'
              OR function = '&LOCAL&COPY'
              OR function = '&LOCAL&PASTE' .

  ENDMETHOD.
  METHOD _handle_user_command.

    CASE e_ucomm.
      WHEN 'DUMMY'.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.