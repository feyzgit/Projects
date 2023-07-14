  METHOD get_sni_pdf.

    TYPES:
      BEGIN OF ty_keydat,
        bukrs  TYPE zsftmain-bukrs,
        gjahr  TYPE zsftmain-gjahr,
        objkey TYPE zsftmain-objkey,
      END OF ty_keydat.

    TYPES:
      BEGIN OF ty_impdat,
        zsftn  TYPE zsftmain-zsftn,
        gjahr  TYPE zsftmain-gjahr,
        bukrs  TYPE zsftmain-bukrs,
        appl   TYPE zsftmain-appl,
        uid_id TYPE zsftmain-uid_id,
        serip  TYPE zsftmain-seri_prefix,
        seri   TYPE zsftmain-seri_no,
      END OF ty_impdat.

    DATA: _t004dat TYPE zlink_t004,
          _keydat  TYPE ty_keydat,
          _impdat  TYPE ty_impdat,
          _xfile   TYPE xstring.

*--------------------------------------------------------------------*
* INITIALIZATION CONTROLS->
*--------------------------------------------------------------------*
    DATA(t_required) = me->required_field(
      EXPORTING
        im_fieldval = VALUE #( ( value = REF #( im_pdfdat-id ) text = text-t16 )
                               ( value = REF #( im_pdfdat-method ) text = text-t19 ) ) ).
    IF line_exists( t_required[ type = mc_msg-error ] ).
      rv_pdfdat = VALUE #( BASE rv_pdfdat return-status_code = 500 return-description = VALUE #( t_required[ type = mc_msg-error ]-message ) ). RETURN.
    ENDIF.

    CASE im_pdfdat-method.
      WHEN mc_cons-get.
        CLEAR: _t004dat.
        SELECT SINGLE id, transid FROM zlink_t004
          INTO @_t004dat
            WHERE id = @im_pdfdat-id
              AND method = @mc_cons-get
              AND transid = @im_pdfdat-transid.
        IF sy-subrc <> 0.
          rv_pdfdat = VALUE #(  BASE rv_pdfdat return-status_code = 500 return-description = _msg_text( im_msgid = mc_msg-id im_msgnr = '034' im_msgv1 = im_pdfdat-id ) ). RETURN.
        ENDIF.
      WHEN mc_cons-create.
        CLEAR: _t004dat.
        SELECT SINGLE id, transid FROM zlink_t004
          INTO @_t004dat
            WHERE id = @im_pdfdat-id
              AND method = @mc_cons-create
              AND transid = @im_pdfdat-transid.
        IF sy-subrc <> 0.
          rv_pdfdat = VALUE #(  BASE rv_pdfdat return-status_code = 500 return-description = _msg_text( im_msgid = mc_msg-id im_msgnr = '034' im_msgv1 = im_pdfdat-id ) ). RETURN.
        ENDIF.
      WHEN OTHERS.
        rv_pdfdat = VALUE #(  BASE rv_pdfdat return-status_code = 500 return-description =  _msg_text( im_msgid = mc_msg-id im_msgnr = '033' ) ). RETURN.
    ENDCASE.

*--------------------------------------------------------------------*
*- GET PDF BASE64 DAT. STEPS->
*--------------------------------------------------------------------*
*<Mock Data Not Transport!!!>
    IF sy-sysid EQ 'RNT' OR sy-sysid EQ 'RNK'.
      DATA: _pdfdat TYPE string.
      CASE sy-sysid.
        WHEN 'RNT'.
          OPEN DATASET 'L:\Kyriba\mockdat.txt' FOR INPUT IN TEXT MODE ENCODING NON-UNICODE.
          IF sy-subrc = 0.
            READ DATASET 'L:\Kyriba\mockdat.txt' INTO _pdfdat.
            IF sy-subrc IS INITIAL.
              rv_pdfdat = VALUE #(  pdfdat = _pdfdat return-status_code = 200 return-description =  _msg_text( im_msgid = mc_msg-id im_msgnr = '032' im_msgv1 = im_pdfdat-id ) ). RETURN.
            ELSE.
              rv_pdfdat = VALUE #(  BASE rv_pdfdat return-status_code = 500 return-description =  _msg_text( im_msgid = mc_msg-id im_msgnr = '031' im_msgv1 = im_pdfdat-id ) ). RETURN.
            ENDIF.
          ELSE.
            rv_pdfdat = VALUE #(  BASE rv_pdfdat return-status_code = 500 return-description =  _msg_text( im_msgid = mc_msg-id im_msgnr = '031' im_msgv1 = im_pdfdat-id ) ). RETURN.
          ENDIF.

        WHEN 'RNK'.
          OPEN DATASET 'L:\DigitalArch\mockdat.txt' FOR INPUT IN TEXT MODE ENCODING NON-UNICODE.
          IF sy-subrc = 0.
            READ DATASET 'L:\DigitalArch\mockdat.txt' INTO _pdfdat.
            IF sy-subrc IS INITIAL.
              rv_pdfdat = VALUE #(  pdfdat = _pdfdat return-status_code = 200 return-description =  _msg_text( im_msgid = mc_msg-id im_msgnr = '032' im_msgv1 = im_pdfdat-id ) ). RETURN.
            ELSE.
              rv_pdfdat = VALUE #(  BASE rv_pdfdat return-status_code = 500 return-description =  _msg_text( im_msgid = mc_msg-id im_msgnr = '031' im_msgv1 = im_pdfdat-id ) ). RETURN.
            ENDIF.
          ELSE.
            rv_pdfdat = VALUE #(  BASE rv_pdfdat return-status_code = 500 return-description =  _msg_text( im_msgid = mc_msg-id im_msgnr = '031' im_msgv1 = im_pdfdat-id ) ). RETURN.
          ENDIF.
      ENDCASE.
      RETURN.
    ENDIF.

    CLEAR: _keydat.
    _keydat = VALUE #( bukrs = im_pdfdat-id(3)
                       gjahr = im_pdfdat-id+13(4)
                       objkey = |{ im_pdfdat-id(3) } { im_pdfdat-id+3(10) }{ im_pdfdat-id+13(4) }| ).

    SELECT SINGLE zsftn, gjahr, bukrs, appl, uid_id, seri_prefix, seri_no
      FROM zsftmain
        INTO @DATA(_zsftmain)
          WHERE bukrs  EQ @_keydat-bukrs
            AND fyon   EQ 'O'
            AND object EQ 'BKPF'
            AND objkey EQ @_keydat-objkey.
    IF sy-subrc EQ 0.
      CLEAR: _impdat.
      _impdat = VALUE #( zsftn = _zsftmain-zsftn gjahr = _zsftmain-gjahr bukrs = _zsftmain-bukrs appl = _zsftmain-appl uid_id = _zsftmain-uid_id serip = _zsftmain-seri_prefix seri = _zsftmain-seri_no ).
      CALL FUNCTION '/SNI/EF_GETPDF'
        EXPORTING
          p_zsftn   = _impdat-zsftn
          p_gjahr   = _impdat-gjahr
          po_bukrs  = _impdat-bukrs
          po_appl   = _impdat-appl
          po_uid_id = _impdat-uid_id
          po_serip  = _impdat-serip
          po_seri   = _impdat-seri
        IMPORTING
          px_file   = _xfile.
      IF NOT _xfile IS INITIAL.
        CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
          EXPORTING
            input  = _xfile
          IMPORTING
            output = rv_pdfdat-pdfdat.
        rv_pdfdat = VALUE #(  BASE rv_pdfdat return-status_code = 200 return-description =  _msg_text( im_msgid = mc_msg-id im_msgnr = '032' im_msgv1 = im_pdfdat-id ) ). RETURN.
      ELSE.
        rv_pdfdat = VALUE #(  BASE rv_pdfdat return-status_code = 500 return-description =  _msg_text( im_msgid = mc_msg-id im_msgnr = '031' im_msgv1 = im_pdfdat-id ) ). RETURN.
      ENDIF.
    ELSE.
      rv_pdfdat = VALUE #(  BASE rv_pdfdat return-status_code = 500 return-description =  _msg_text( im_msgid = mc_msg-id im_msgnr = '031' im_msgv1 = im_pdfdat-id ) ). RETURN.
    ENDIF.

  ENDMETHOD.