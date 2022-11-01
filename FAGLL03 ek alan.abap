FAGLL03 ek alan
FAGLPOSX ek alan CI_FAGLPOSX append structure yapılarak alanlar eklenilir.


Badi:
FAGL_ITEMS_CH_DATA,



METHOD if_ex_fagl_items_ch_data~change_items.



   DATA ls_items LIKE LINE OF ct_items.

   LOOP AT ct_items INTO ls_items.
     IF ls_items–hkont IS NOT INITIAL.
        SELECT SINGLE txt50
        FROM gl_acct_ca_text
        INTO ls_items–zztxt50
        WHERE spras EQ sy–langu
          AND ktopl EQ ‘MAHP’
          AND saknr EQ ls_items–hkont.
          modify ct_items from ls_items.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.



Sonra da ek alanlar aktif olması için aşağıdaki işlemi yapıyoruz.
se37 den ITEM_STRUC_EXTENSION
BASIC_STRUCNAME                FAGLPOSY
EXT_STRUCNAME                  FAGLPOSYEXT
EXT_FIELDS_TABNAME             T021S
I_LSTCL                        D
X_TRANSPORT                    X