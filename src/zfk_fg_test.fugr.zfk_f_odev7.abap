FUNCTION zfk_f_odev7.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(USERID) TYPE  SY-UNAME
*"  EXPORTING
*"     REFERENCE(PERMISSION) TYPE  CHAR1
*"  TABLES
*"      PERSONELTAB TYPE  ZFK_TT_SIRKET
*"----------------------------------------------------------------------
  DATA ls_table LIKE personeltab.
  READ TABLE personeltab INTO ls_table WITH KEY kullaniciadi = userid.

  IF ls_table IS INITIAL.
    MESSAGE 'Kullanıcı tanımlı değil !!!' TYPE 'I'.
    LEAVE PROGRAM.
  ELSE.
    MESSAGE 'Kullanıcı tanımlı !!!' TYPE 'I'.
  ENDIF.





ENDFUNCTION.
