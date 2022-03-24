*&---------------------------------------------------------------------*
*& Report  ZER_R_SAT_MAIL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  zer_r_sat_mail.
*--Include
INCLUDE ole2incl.

* Tables                                                               *
*----------------------------------------------------------------------*
TABLES: sscrfields,eban, mara, makt, zer_t_028.
*----------------------------------------------------------------------*
* Data Definition                                                      *
*----------------------------------------------------------------------*
* Type-pools *
TYPE-POOLS: slis.

INCLUDE <icon>.

DATA: d_ok.
*DATA BEGIN OF i_report OCCURS 0 .
*DATA :  banfn  LIKE eban-banfn,
*        bnfpo  LIKE eban-bnfpo,
*        matnr  LIKE eban-matnr,
*        maktx  LIKE makt-maktx,
*        menge  LIKE eban-menge,
*        meins  LIKE eban-meins,
*    END OF i_report.

DATA:  i_report  TYPE TABLE OF zer_t_028 WITH HEADER LINE,
       gs_report TYPE zer_t_028.

DATA: gv_count     TYPE i,
      i_color_tab  TYPE slis_t_specialcol_alv,
      wa_color_tab TYPE slis_specialcol_alv,
      g_cell_color TYPE slis_color,
      gv_control   TYPE i.
** Internal table for list catalog
DATA: i_fieldcat   TYPE slis_t_fieldcat_alv,
      gs_fieldcat  TYPE LINE OF slis_t_fieldcat_alv.
DATA: d_heading    TYPE slis_t_listheader.
DATA: it_events    TYPE slis_alv_event OCCURS 0 WITH HEADER LINE,
      it_excluding TYPE slis_t_extab,
      d_repname    LIKE sy-repid.
DATA: l_layout     TYPE slis_layout_alv.

DATA:gt_mailadd TYPE TABLE OF  zer_t_023 WITH HEADER LINE.

*--Excel Varible
DATA: gv_filename TYPE string,
      gv_path     TYPE string,
      gv_fullpath TYPE string,
      gv_ln       TYPE i,
      gv_val      TYPE string.

DATA: e_activesheet TYPE ole2_object,
      e_appl        TYPE ole2_object,
      e_work        TYPE ole2_object,
      e_cell        TYPE ole2_object,
      e_color       TYPE ole2_object,
      e_bold        TYPE ole2_object.

*--Mail Varible
DATA:   it_message TYPE STANDARD TABLE OF solisti1
                   INITIAL SIZE 0 WITH HEADER LINE.
DATA:   it_attach TYPE STANDARD TABLE OF solisti1
                   INITIAL SIZE 0 WITH HEADER LINE.
DATA: gd_error    TYPE sy-subrc,
      gd_reciever TYPE sy-subrc.

DATA: lv_intad LIKE knb1-intad,
      lv_zsabe LIKE knb1-zsabe,
      lv_datum TYPE c LENGTH 10,
      lv_title TYPE c LENGTH 50,
      lv_email LIKE somlreci1-receiver.

DATA: ls_report LIKE LINE OF i_report.

************************************************************************
*  C o n s t a n t s                                                   *
************************************************************************
CONSTANTS: formname_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE',
           c_bukrs TYPE bukrs VALUE '1000'.
************************************************************************
*                       FIELD-SYMBOLS
************************************************************************

************************************************************************
* Parameters
************************************************************************
*  selection screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS     : p_badat LIKE eban-badat  OBLIGATORY.
SELECT-OPTIONS : s_flief FOR eban-flief.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_mail AS CHECKBOX DEFAULT 'X',
            p_auth LIKE  zer_t_023-zauth OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.

************************************************************************
*  INITIALIZATION
************************************************************************
INITIALIZATION.

  PERFORM initial_values.

************************************************************************
*  AT SELECTION-SCREEN                                                 *
AT SELECTION-SCREEN OUTPUT.
  PERFORM get_screen_output.
************************************************************************

************************************************************************
*  M a i n  P r o c e s s                                              *
************************************************************************
START-OF-SELECTION.

  CALL FUNCTION 'DB_COMMIT'.

  PERFORM main_sel.

************************************************************************
END-OF-SELECTION.

  CALL FUNCTION 'DB_COMMIT'.

  IF d_ok EQ space.
    MESSAGE s208(fz).
    "No data records found for these selection criteria
    EXIT.
  ELSEIF d_ok EQ 'E'.
    MESSAGE s024(zsd).
    "Bayi Sistemine Eri?im Hatasy!
    EXIT.
  ENDIF.

*- ALV output
  PERFORM report.

*&---------------------------------------------------------------------*
*&      Form  INITIAL_VALUES
*&---------------------------------------------------------------------*
FORM initial_values.

  CLEAR: d_ok.

  CLEAR: i_report, i_report[].

  CLEAR: i_fieldcat[], l_layout, d_heading, it_events[], it_excluding[].

ENDFORM.                    " INITIAL_VALUES
*&---------------------------------------------------------------------*
*&      Form  REPORT
*&---------------------------------------------------------------------*
FORM report.
*
  PERFORM list_layout_specification.
  PERFORM create_field_catalog USING 'I_REPORT'.
  PERFORM call_alv_display TABLES i_report .

*
ENDFORM.                    " REPORT
*&---------------------------------------------------------------------*
*&      Form  create_field_catalog
*&---------------------------------------------------------------------*
FORM create_field_catalog USING  alvtabname TYPE slis_tabname .

  DATA: i_fcat TYPE slis_t_fieldcat_alv,
        l_fcat TYPE LINE OF slis_t_fieldcat_alv,
        r_fieldcat TYPE slis_fieldcat_alv,
        d_text(40) TYPE c.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-cprog
      i_internal_tabname     = alvtabname
      i_inclname             = sy-cprog
    CHANGING
      ct_fieldcat            = i_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
*
  l_fcat-key = space.
  MODIFY i_fcat FROM l_fcat TRANSPORTING key WHERE key NE space.
*
*
  i_fieldcat[] = i_fcat[].
*
  LOOP AT i_fieldcat INTO r_fieldcat.
    CLEAR d_text.

    CASE r_fieldcat-fieldname .
      WHEN 'BANFN'.
        d_text = 'Satýn Alma Talep No '.
      WHEN 'BNFPO'.
        d_text = 'Kalem No'.
      WHEN 'MATNR'.
        d_text = 'Malzeme'.
      WHEN 'MAKTX'.
        d_text = 'Malzeme Tanýmý'.
      WHEN 'MENGE'.
        d_text = 'Miktar'.
      WHEN 'MEINS'.
        d_text = 'Ölçü Birimi'.
    ENDCASE.
*

    IF d_text NE space.
      MOVE d_text TO: r_fieldcat-seltext_l,
                      r_fieldcat-seltext_m,
                      r_fieldcat-seltext_s,
                      r_fieldcat-reptext_ddic.
    ENDIF.
    MODIFY i_fieldcat FROM r_fieldcat.
  ENDLOOP.

  PERFORM build_header.

  PERFORM build_eventtab.

ENDFORM.                    " create_field_catalog
*---------------------------------------------------------------------*
*       FORM BUILD_HEADER                                             *
*---------------------------------------------------------------------*
FORM build_header.

  DATA: hline TYPE slis_listheader,
         text(60)     TYPE c,
         w_inx        LIKE sy-tabix,
         wa_usr       LIKE v_usr_name,
         lv_datum(10) TYPE c,
         lv_uzeit(8)  TYPE c.

  SELECT SINGLE * FROM  v_usr_name
                  INTO  wa_usr
                  WHERE bname = sy-uname.

  CLEAR d_heading[].

  CLEAR: hline, text.
  hline-typ  = 'H'.
  hline-info = sy-title.
  APPEND hline TO d_heading.

  CLEAR: hline, text.
  hline-typ  = 'S'.
  CONCATENATE 'Kulanýcý Adý :' wa_usr-name_text
                               INTO text SEPARATED BY space.
  hline-info = text.
  APPEND hline TO d_heading.

  CLEAR: hline, text, lv_datum, lv_uzeit.
  hline-typ  = 'S'.

  CONCATENATE sy-datum+6(2) '.'
              sy-datum+4(2) '.'
              sy-datum(4)   INTO lv_datum.

  CONCATENATE   sy-uzeit(2)   ':'
                sy-uzeit+2(2) ':'
                sy-uzeit+4(2) INTO lv_uzeit.

  CONCATENATE 'Tarih / Saat :' lv_datum '/' lv_uzeit
                               INTO text SEPARATED BY space.
  hline-info = text.
  APPEND hline TO d_heading.

  DATA lv_count(10) TYPE c.
  lv_count = gv_count.
  hline-typ  = 'S'.
  CONCATENATE 'Kayýt Sayýsý:' lv_count
                              INTO text SEPARATED BY space.
  hline-info = text.
  APPEND hline TO d_heading.

ENDFORM.                    " BUILD_HEADER
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENTTAB
*&---------------------------------------------------------------------*
FORM build_eventtab.

  DATA: ls_event TYPE slis_alv_event,
        ls_exclu TYPE LINE OF slis_t_extab.

  CLEAR: it_events[], it_excluding[].

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = it_events[].

  READ TABLE it_events WITH KEY name = slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE formname_top_of_page TO ls_event-form.
    APPEND ls_event TO it_events.
  ENDIF.

* Menu Functions to be excluded
  ls_exclu-fcode = '&CRB'. APPEND ls_exclu TO it_excluding.
  ls_exclu-fcode = '&CRL'. APPEND ls_exclu TO it_excluding.
  ls_exclu-fcode = '&CRR'. APPEND ls_exclu TO it_excluding.
  ls_exclu-fcode = '&CRE'. APPEND ls_exclu TO it_excluding.
  ls_exclu-fcode = '&AQW'. APPEND ls_exclu TO it_excluding.
  ls_exclu-fcode = '%SL'. APPEND ls_exclu TO it_excluding.
  ls_exclu-fcode = '&ABC'. APPEND ls_exclu TO it_excluding.

ENDFORM.                    " BUILD_EVENTTAB
*---------------------------------------------------------------------*
*       FORM LIST_LAYOUT_SPECIFICATION                                *
*---------------------------------------------------------------------*
FORM list_layout_specification.

  CLEAR l_layout.
  MOVE:  'X'    TO l_layout-colwidth_optimize,
         'X'    TO l_layout-zebra,
         'X'    TO l_layout-detail_initial_lines.

*  l_layout-coltab_fieldname = 'COLOR'. "info_fieldname = 'COLOR'.
*  l_layout-box_fieldname = 'SELKZ'.

ENDFORM.                    " list_layout_specification
*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_DISPLAY
*&---------------------------------------------------------------------*
FORM call_alv_display TABLES alvtable TYPE STANDARD TABLE.
*
  DATA: w_func LIKE tfdir-funcname.
*
* w_func = 'REUSE_ALV_LIST_DISPLAY'.
  w_func = 'REUSE_ALV_GRID_DISPLAY'.
*
  CALL FUNCTION w_func
    EXPORTING
      i_callback_program       = sy-cprog
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = l_layout
      i_default                = 'X'
      i_save                   = 'X'
*     IS_VARIANT               = D_VARIANT1
      it_fieldcat              = i_fieldcat[]
      it_excluding             = it_excluding[]
*     IT_SORT                  = I_SORT[]
*     IT_FILTER                = I_FILTER[]
      it_events                = it_events[]
    TABLES
      t_outtab                 = alvtable
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
*
ENDFORM.                    " CALL_ALV_DISPLAY
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = d_heading.
ENDFORM.                    "TOP_OF_PAGE
*----------------------------------------------------------------------*
*&      Form  MAIN_SEL
*----------------------------------------------------------------------*
FORM main_sel .

  PERFORM get_data .

  CHECK NOT i_report[] IS INITIAL.
**
  d_ok = 'X'.

ENDFORM.                    " MAIN_SEL
*---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
FORM set_pf_status USING p_status.

  SET PF-STATUS 'STANDARD' EXCLUDING it_excluding.
ENDFORM.                    " SET_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*

FORM get_data .

  SELECT e~banfn e~bnfpo
         e~matnr e~menge
         e~meins m~maktx FROM eban AS e
                         LEFT JOIN makt AS m ON e~matnr = m~matnr
                         INTO CORRESPONDING FIELDS OF TABLE i_report
                         WHERE e~badat EQ p_badat AND
                               e~flief IN s_flief.

  IF p_mail = 'X'. "Mail gönderilsin.

    PERFORM sent_mail.

  ENDIF.

ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command USING v_ucomm  LIKE sy-ucomm
                        v_selfld TYPE slis_selfield.

  CASE v_ucomm.
    WHEN '&EXCEL'.
      PERFORM file_dialog.
      PERFORM excel_download.
    WHEN '&MAIL'.
      PERFORM sent_mail.
  ENDCASE.

ENDFORM .                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  EXCEL_DOWNLOAD
*&---------------------------------------------------------------------*
FORM excel_download .

  CLEAR: i_report.

  CREATE OBJECT e_appl 'EXCEL.APPLICATION'.
  SET PROPERTY OF e_appl 'VISIBLE' = 1.

  CALL METHOD OF
      e_appl
      'WORKBOOKS' = e_work.
  CALL METHOD OF
      e_work
      'Add'  = e_work.

  GET PROPERTY OF e_appl        'ActiveSheet' = e_activesheet.
  SET PROPERTY OF e_activesheet 'Name'        = 'SAT'.

  PERFORM set_cell USING 3 1 'Satýn Alma Talep No' 50 1 20.
  PERFORM set_cell USING 3 2 'Kalem No'            50 1 12.
  PERFORM set_cell USING 3 3 'Malzeme Kodu'        50 1 20.
  PERFORM set_cell USING 3 4 'Malzeme Tanýmý'      50 1 30.
  PERFORM set_cell USING 3 5 'Miktar'              50 1 17.
  PERFORM set_cell USING 3 6 'Ölçü Birimi'         50 1 12.

  LOOP AT i_report.
    gv_ln =  sy-tabix + 3.
    gv_val = i_report-banfn.
    PERFORM set_cell USING gv_ln 1 gv_val 0 0 0.
    gv_val = i_report-bnfpo.
    PERFORM set_cell USING gv_ln 2 gv_val 0 0 0.
    gv_val = i_report-matnr.
    PERFORM set_cell USING gv_ln 3 gv_val 0 0 0.
    gv_val = i_report-maktx.
    PERFORM set_cell USING gv_ln 4 gv_val 0 0 0.
    gv_val = i_report-menge.
    PERFORM set_cell USING gv_ln 5 gv_val 0 0 0.
    gv_val = i_report-meins.
    PERFORM set_cell USING gv_ln 6 gv_val 0 0 0.
  ENDLOOP.

  CALL METHOD OF
      e_work
      'SAVEAS'

    EXPORTING
      #1       = gv_fullpath.

  CALL METHOD OF
      e_work
      'close'.
  CALL METHOD OF
      e_appl
      'QUIT'.
  FREE OBJECT e_appl.


ENDFORM.                    " EXCEL_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  FILE_DIALOG
*&---------------------------------------------------------------------*
FORM file_dialog .


  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Select file'
      default_extension = 'xls'
      file_filter       = '*.xls'
    CHANGING
      filename          = gv_filename
      path              = gv_path
      fullpath          = gv_fullpath.

  CHECK sy-subrc IS INITIAL.

ENDFORM.                    " FILE_DIALOG
*&---------------------------------------------------------------------*
*&      Form  SET_CELL
*&---------------------------------------------------------------------*
FORM set_cell  USING p_row    TYPE sy-tabix
                     p_column TYPE sy-tabix
                     p_val    TYPE string
                     p_int    TYPE i
                     p_bold   TYPE i
                     p_width  TYPE i.

  CALL METHOD OF
      e_appl
      'Cells' = e_cell
    EXPORTING
      #1      = p_row
      #2      = p_column.
  SET PROPERTY OF e_cell 'Value'       =  p_val.
  GET PROPERTY OF e_cell 'Interior'    =  e_color.
  SET PROPERTY OF e_color 'ColorIndex' =  p_int.

  GET PROPERTY OF e_cell 'Font' = e_bold.
  SET PROPERTY OF e_bold 'Bold' = p_bold.
  IF p_width > 0.
    SET PROPERTY OF e_cell 'ColumnWidth' = p_width.
  ENDIF.

ENDFORM.                    " SET_CELL
*&---------------------------------------------------------------------*
*&      Form  SENT_MAIL
*&---------------------------------------------------------------------*
FORM sent_mail .

  SELECT * FROM zer_t_023
           INTO CORRESPONDING FIELDS OF TABLE gt_mailadd
           WHERE zauth EQ p_auth.

  IF gt_mailadd[] IS INITIAL.
    MESSAGE 'Lütfen mail bakýmýný yapýnýz.' TYPE 'W'.
  ELSE.

*--Baþlýk bilgisi
    CONCATENATE p_badat 'Tarihli Talepler Hk.'
                INTO lv_title SEPARATED BY space.

    PERFORM populate_email_message_body TABLES it_message
                                               i_report
                                         USING gs_report.

    PERFORM send_file_as_email_attachment
                                 TABLES it_message
                                        it_attach
                                        i_report
                                  USING lv_email
                                        lv_title
                                        'XLS'
                                        'Ek'
                                        ' '
                                        ''
                                        ' '
                               CHANGING gd_error
                                        gd_reciever.

  ENDIF.



ENDFORM.                    " SENT_MAIL
*&---------------------------------------------------------------------*
*&      Form  GET_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
FORM get_screen_output .

  LOOP AT SCREEN.
    IF p_mail NE 'X'.
      IF screen-name = 'P_AUTH'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  POPULATE_EMAIL_MESSAGE_BODY
*&---------------------------------------------------------------------*

FORM populate_email_message_body   TABLES pit_message
                                          pit_items STRUCTURE zer_t_028
                                   USING  ls_item   TYPE      zer_t_028.

  DATA: lv_datum    TYPE c LENGTH 10,
        lv_menge_c  TYPE c LENGTH 15.

  CONCATENATE '<html><head><style>'
  '.table1 { background-color:#FFFFFF;text-align:center;width:100%; }'
  'th, td { border-bottom:1px solid #000000;'
  'line-height:2;}</style></head><body>'
  INTO pit_message.
  APPEND pit_message.


  CONCATENATE 'Sayýn'
              'ilgililer,'
              INTO pit_message SEPARATED BY space.
  APPEND pit_message.

  CONCATENATE p_badat+6(2) '.' p_badat+4(2) '.'
              p_badat+0(4)
              INTO lv_datum.

  CONCATENATE '<p>' lv_datum 'tarihli'
              ' maðaza sipariþleri aþaðýdaki gibidir;</p>'
              INTO pit_message SEPARATED BY space.
  APPEND pit_message.

  pit_message = '<p>Bilginize,</p></p>'.
  APPEND pit_message.


  pit_message = '<table class="table1">'.
  APPEND pit_message.

  CONCATENATE '<tr class="row">'
              '<th>Maðaza Kodu</th>'
              '<th>Maðaza Tanýmý</th>'
              '<th>Satýn Alma Talep No</th>'
              '<th>Talep Kalem No</th>'
              '<th>Ürün Kodu</th>'
              '<th>Ürün Tanýmý</th>'
              '<th>Miktar</th></tr>'
              '<th>Ölçü Birimi</th></tr>'
              INTO pit_message.
  APPEND pit_message.

  SORT pit_items DESCENDING BY banfn bnfpo.

  DATA : lv_name TYPE char10 VALUE 'Bilinmiyor'.

  CLEAR :ls_item.
  LOOP AT pit_items INTO ls_item.

    CLEAR : lv_menge_c.
    WRITE ls_item-menge TO lv_menge_c.
    CONDENSE lv_menge_c NO-GAPS.

    CONCATENATE '<tr class="row">'
                '<td>' lv_name '</td>'
                '<td>' lv_name '</td>'
                '<td>' ls_item-banfn '</td>'
                '<td>' ls_item-bnfpo '</td>'
                '<td>' ls_item-matnr '</td>'
                '<td>' ls_item-maktx '</td>'
                '<td>' lv_menge_c    '</td></tr>'
                '<td>' ls_item-meins '</td></tr>'
                INTO pit_message.
    APPEND pit_message.

  ENDLOOP.

  pit_message = '</table>'.
  APPEND pit_message.

ENDFORM.                    " POPULATE_EMAIL_MESSAGE_BODY
*&---------------------------------------------------------------------*
*&      Form  SEND_FILE_AS_EMAIL_ATTACHMENT
*&---------------------------------------------------------------------*
FORM send_file_as_email_attachment TABLES pit_message
                                          pit_attach
                                          pit_items
                                   USING  p_email
                                          p_mtitle
                                          p_format
                                          p_filename
                                          p_attdescription
                                          p_sender_address
                                          p_sender_addres_type
                                CHANGING  p_error
                                          p_reciever.

  DATA:   t_packing_list LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
          t_contents LIKE solisti1 OCCURS 0 WITH HEADER LINE,
          t_receivers LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
          t_attachment LIKE solisti1 OCCURS 0 WITH HEADER LINE,
          t_object_header LIKE solisti1 OCCURS 0 WITH HEADER LINE,
          w_cnt TYPE i,
          w_sent_all(1) TYPE c,
          w_doc_data LIKE sodocchgi1.


  DATA: ld_error    TYPE sy-subrc,
        ld_reciever TYPE sy-subrc,
        ld_mtitle LIKE sodocchgi1-obj_descr,
        ld_email LIKE  somlreci1-receiver,
        ld_format TYPE  so_obj_tp ,
        ld_attdescription TYPE  so_obj_nam ,
        ld_attfilename TYPE  so_obj_des ,
        ld_sender_address LIKE  soextreci1-receiver,
        ld_sender_address_type LIKE  soextreci1-adr_typ,
        lv_lines_body TYPE i,
        ld_receiver LIKE  sy-subrc,
        ls_item TYPE mereq_item,
        ls_adress TYPE bapiaddr3,
        lt_return TYPE TABLE OF bapiret2.

  DEFINE m_fill_receiver.
    clear t_receivers.
    t_receivers-receiver = &1.
    t_receivers-rec_type = 'U'.
    t_receivers-com_type = 'INT'.
    t_receivers-notif_del = 'X'.
    t_receivers-notif_ndel = 'X'.
    t_receivers-copy = &2.
    append t_receivers.
  END-OF-DEFINITION.

  ld_email   = p_email.
  ld_mtitle = p_mtitle.
  ld_format              = p_format.
  ld_attdescription      = p_attdescription.
  ld_attfilename         = p_filename.
  ld_sender_address      = p_sender_address.
  ld_sender_address_type = p_sender_addres_type.


* Fill the document data.
  w_doc_data-doc_size = 1.

* Populate the subject/generic message attributes
  w_doc_data-obj_langu = sy-langu.
  w_doc_data-obj_name  = 'SAPRPT'.
  w_doc_data-obj_descr = ld_mtitle .
  w_doc_data-sensitivty = 'F'.

* Fill the document data and get size of attachment
  CLEAR w_doc_data.
*  READ TABLE pit_attach INDEX w_cnt.
  DESCRIBE TABLE pit_message LINES lv_lines_body.
  w_doc_data-doc_size =
     ( lv_lines_body - 1 ) * 255 + strlen( pit_message ).
  w_doc_data-obj_langu  = sy-langu.
  w_doc_data-obj_name   = 'SAPRPT'.
  w_doc_data-obj_descr  = ld_mtitle.
  w_doc_data-sensitivty = 'F'.
  CLEAR t_attachment.
  REFRESH t_attachment.
  t_attachment[] = pit_attach[].

* Describe the body of the message
  CLEAR t_packing_list.
  REFRESH t_packing_list.
  t_packing_list-transf_bin = space.
  t_packing_list-head_start = 1.
  t_packing_list-head_num = 0.
  t_packing_list-body_start = 1.
  DESCRIBE TABLE pit_message LINES t_packing_list-body_num.
  t_packing_list-doc_type = 'HTM'.
  APPEND t_packing_list.


  LOOP AT gt_mailadd .
    m_fill_receiver  gt_mailadd-zmail  ''.
    CLEAR: gt_mailadd.
  ENDLOOP.


  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = w_doc_data
      put_in_outbox              = 'X'
      sender_address             = ld_sender_address
      sender_address_type        = ld_sender_address_type
      commit_work                = 'X'
    IMPORTING
      sent_to_all                = w_sent_all
    TABLES
      packing_list               = t_packing_list
*     contents_bin               = t_attachment
      contents_txt               = pit_message
      receivers                  = t_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

* Populate zerror return code
  ld_error = sy-subrc.

* Populate zreceiver return code
  LOOP AT t_receivers.
    ld_receiver = t_receivers-retrn_code.
  ENDLOOP.
ENDFORM.                    "SEND_FILE_AS_EMAIL_ATTACHMENT
 
