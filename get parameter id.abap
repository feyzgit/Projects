    FREE: p_bukrs.
    p_bukrs = COND #( WHEN zfi_020_helper=>_get_user_params_dat( 'BUK' ) IS INITIAL THEN '1000' ELSE zfi_020_helper=>_get_user_params_dat( 'BUK' ) ).

   METHOD _get_user_params_dat.

    IF im_parid IS INITIAL.
      MESSAGE e144(zfi_020_msg01) RAISING handle_error.
    ENDIF.

    DATA(mo_helper) = NEW zfi_020_helper( ).
    GET PARAMETER ID im_parid FIELD rv_parva.
    IF sy-subrc IS NOT INITIAL.
      ev_msgdat = mo_helper->msg_fill(
        EXPORTING
          im_msgty = mc_msg-error
          im_msgid = mc_msg-id
          im_msgnr = '143'
          im_msgv1 = im_parid ).
    ENDIF.

  ENDMETHOD.


IM_PARID    Importing   Type    MEMORYID   Set-/Get-parametresi tanıtıcısı                                                  
EV_MSGDAT   Exporting   Type    BAPIRET2   Dönüş parametresi
RV_PARVA    Returning   Type    XUVALUE    Parametre değeri