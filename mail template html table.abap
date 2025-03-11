METHOD set_mailbody.

    DEFINE add_line.
      APPEND &1 TO rt_mailtab.
    END-OF-DEFINITION.

    DATA: _body        TYPE string,
          lv_table_row TYPE string.

    _body = |{ TEXT-m03 }{ TEXT-m04 }|.

    add_line:
      TEXT-m02,  " Sayın Yetkili,
      '<br>',
      '<br>',
      _body,  "     Aşağıda yer alan tüm Rönesans Grubu şirketlerimizin mevduatlarının aşağıda yer alan vadeli mevduat oranları dikkate alınarak vadeli mevduat hesabına transferini rica ederiz.
      '<br>',
      '<br>',
      '<br>',
      '<br>',
      '<table border="1" style="border-collapse: collapse; width: auto; font-size: 12px; color: #333;">',
      '<tr>',
      '<th style="background-color: #808080; padding: 8px; width: auto;">BANKA</th>',
      '<th style="background-color: #808080; padding: 8px; width: auto;">ŞİRKET</th>',
      '<th style="background-color: #808080; padding: 8px; width: auto;">HESAP NO</th>',
      '<th style="background-color: #808080; padding: 8px; width: auto;">TUTAR</th>',
      '<th style="background-color: #808080; padding: 8px; width: auto;">PARA BİRİMİ</th>',
      '<th style="background-color: #808080; padding: 8px; width: auto;">ORAN</th>',
      '<th style="background-color: #808080; padding: 8px; width: auto;">VADE</th>',
      '</tr>'.
    LOOP AT im_report_detail ASSIGNING FIELD-SYMBOL(<report_detail>).
      rt_mailtab = VALUE #( BASE rt_mailtab
        ( line = '<tr>' )
        ( line = |<td style="padding: 8px; width: auto; ">{ <report_detail>-banka }</td>| )
        ( line = |<td style="padding: 8px; width: auto;">{ <report_detail>-butxt }</td>| )
        ( line = |<td style="padding: 8px; width: auto; text-align: center;">{ <report_detail>-bankn }</td>| )
        ( line = |<td style="padding: 8px; width: auto; text-align: right;">{ <report_detail>-wrbtr NUMBER = USER }</td>| )
        ( line = |<td style="padding: 8px; width: auto; text-align: center;">{ <report_detail>-waers }</td>| )
        ( line = |<td style="padding: 8px; width: auto;">{ <report_detail>-irate1 }</td>| )
        ( line = |<td style="padding: 8px; width: auto;">{ <report_detail>-expstr+6(2) }.{ <report_detail>-expstr+4(2) }.{ <report_detail>-expstr(4) }</td>| )
        ( line = |</tr>| ) ).
    ENDLOOP.
    add_line: '</table>'.

  ENDMETHOD.
  
  
	  set_mailbody
        IMPORTING
          im_report_detail  TYPE tt_report_detail
        RETURNING
          VALUE(rt_mailtab) TYPE soli_tab,  
		  

	lt_mailtab = set_mailbody( im_report_detail = <report>-t_mail_report_detail ).



    DATA: lo_send_request TYPE REF TO cl_bcs,
          lo_document     TYPE REF TO cl_document_bcs,
          lo_sender       TYPE REF TO if_sender_bcs,
          lo_recipient    TYPE REF TO if_recipient_bcs,
          lo_recipient_cc TYPE REF TO if_recipient_bcs,
          lt_mailtab      TYPE soli_tab,
          lv_rec          TYPE ad_smtpadr,
          lv_cc           TYPE ad_smtpadr,
          lv_subject      TYPE sood-objdes,
          lv_result       TYPE os_boolean,
          lv_size         TYPE i,
          lt_solix        TYPE solix_tab.


    LOOP AT t_report ASSIGNING <report>.

      TRY.
          IF lo_send_request IS BOUND.
            FREE lo_send_request.
            CLEAR lo_send_request.
          ENDIF.

          lo_send_request = cl_bcs=>create_persistent( ).

          TRY.
              LOOP AT lt_mails INTO DATA(ls_mails).
* Gönderen
                CALL METHOD cl_cam_address_bcs=>create_internet_address
                  EXPORTING
                    i_address_string = 'feyzanur.karakas@finpro.com.tr' "ls_mails-sender
*                   i_address_name   = 'SAP INFO'
                  RECEIVING
                    result           = lo_sender.

                lo_send_request->set_sender( i_sender = lo_sender ).

* Alıcı
                lo_recipient = cl_cam_address_bcs=>create_internet_address( 'feyzanur.karakas@finpro.com.tr' ).
                lo_send_request->add_recipient(
                  EXPORTING
                    i_recipient = lo_recipient
                    i_express   = abap_true ).

              ENDLOOP.
            CATCH cx_address_bcs.
            CATCH cx_root.
          ENDTRY.

          lv_subject = TEXT-m01.
          lt_mailtab = set_mailbody( im_report_detail = <report>-t_mail_report_detail ).

          lo_document = cl_document_bcs=>create_document(
                      i_type       = 'HTM'
                      i_importance = '5'
                      i_text       = lt_mailtab
                      i_subject    = lv_subject ).

          lo_send_request->set_document( lo_document ).

          lo_send_request->set_send_immediately( 'X' ).

          lv_result = lo_send_request->send( i_with_error_screen = 'X' ).

          IF lv_result IS INITIAL.
            MESSAGE e001(00) WITH 'Mail gönderimi başarısız!'.
          ELSE.
            MESSAGE s001(00) WITH 'Mail gönderildi!'.
            COMMIT WORK AND WAIT.
          ENDIF.
      ENDTRY.

    ENDLOOP.		  
		  