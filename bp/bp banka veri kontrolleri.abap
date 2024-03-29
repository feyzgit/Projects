FUNCTION zfi_000_fm_bp_payment_control.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------

*--------------------------------------------------------------------*
*& Title     : BP Banka Verileri Kontrolleri
*& Object ID : FI000
*--------------------------------------------------------------------*
*& ->DEV: Abdullah ATAN / abdullah.atan@dataliva.com.tr
*& ->MOD:
*--------------------------------------------------------------------*
  DATA: s_but000     TYPE but000,
        l_timest     TYPE timestamp,
        l_aktyp      TYPE tbz0k-aktyp,
        sy_datlo     TYPE timestamp,
        t_but0bk     TYPE TABLE OF but0bk,
        t_but0bk_old TYPE TABLE OF but0bk.

  CONSTANTS: BEGIN OF _cons,
               create   TYPE numc2 VALUE '01',
               change   TYPE numc2 VALUE '02',
               display  TYPE numc2 VALUE '06',
               hr_group TYPE but000-bu_group VALUE 'HR',
             END OF _cons.

  CONSTANTS: BEGIN OF _msg,
               id      TYPE symsgid    VALUE 'ZFI_000',
               success TYPE bapi_mtype VALUE 'S',
               error   TYPE bapi_mtype VALUE 'E',
               warning TYPE bapi_mtype VALUE 'W',
               info    TYPE bapi_mtype VALUE 'I',
               abort   TYPE bapi_mtype VALUE 'A',
             END OF _msg.

*-&Aktivite tipi: 01 = Yarat, 02 = Değiştir, 06 = Görüntüle
  CLEAR: l_aktyp.
  CALL FUNCTION 'BUS_PARAMETERS_ISSTA_GET'
    IMPORTING
      e_aktyp = l_aktyp.

  IF l_aktyp = _cons-create OR l_aktyp = _cons-change.

*-&Muhatap genel bilgileri(Read memory)->
    CLEAR: s_but000.
    CALL FUNCTION 'BUP_BUPA_BUT000_GET'
      IMPORTING
        e_but000 = s_but000.

*-&HR grubuna ait muhattaplar için kontrol yapılmayacak->
    CHECK s_but000-bu_group <> _cons-hr_group.

*-&Muhattap banka verilerinin okunması->
    FREE: t_but0bk, t_but0bk_old.
    CALL FUNCTION 'BUP_BUPA_BUT0BK_GET'
      TABLES
        t_but0bk     = t_but0bk
        t_but0bk_old = t_but0bk_old.

*-&Sistemde tanımlı para birimleri->
    SELECT waers
      FROM tcurc
        INTO TABLE @DATA(t_currdat).

    LOOP AT t_but0bk ASSIGNING FIELD-SYMBOL(<fs_bankdat>).

*-&Kontrol-1: Referans verileri boş geçilemez->
      IF <fs_bankdat>-bkref EQ space.
        CALL FUNCTION 'BUS_MESSAGE_STORE'
          EXPORTING
            arbgb = _msg-id
            msgty = _msg-error
            txtnr = 016.
        CONTINUE.
      ENDIF.

*-&Kontrol-1: Referans verileri alanı PB ile doldurulmalı->
      IF NOT line_exists( t_currdat[ waers = <fs_bankdat>-bkref ] ).
        CALL FUNCTION 'BUS_MESSAGE_STORE'
          EXPORTING
            arbgb = _msg-id
            msgty = _msg-error
            txtnr = 017
            msgv1 = <fs_bankdat>-bkref.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFUNCTION.