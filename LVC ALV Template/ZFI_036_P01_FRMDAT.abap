*&---------------------------------------------------------------------*
*& Include          ZFI_036_P01_FRMDAT
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command USING v_ucomm  LIKE sy-ucomm
                        v_selfld TYPE slis_selfield.

  CASE v_ucomm.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN '&IC1'.
      READ TABLE mt_outdat REFERENCE INTO DATA(lr_out) INDEX v_selfld-tabindex.
      CHECK sy-subrc IS INITIAL.
      CASE v_selfld-fieldname.
        WHEN 'MSGSHW'.

        WHEN OTHERS.
      ENDCASE.
    WHEN 'SAVE'.
      application=>app->show_message_tool(
        EXPORTING
          iv_msgdat = application=>app->save_dat( ) ).
  ENDCASE .

  v_selfld-refresh = abap_true.
  v_selfld-row_stable = abap_true.

ENDFORM .                    "user_command
*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
FORM set_pf_status USING p_status.
  SET PF-STATUS 'STANDARD' EXCLUDING it_excluding.
ENDFORM .                    "set_pf_status
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.

  DATA: d_heading    TYPE slis_t_listheader,
        hline        TYPE slis_listheader,
        text(60)     TYPE c,
        w_inx        LIKE sy-tabix,
        wa_usr       LIKE v_usr_name,
        lv_datum(10) TYPE c,
        lv_uzeit(8)  TYPE c,
        lv_count(10) TYPE c.

  SELECT SINGLE * FROM  v_usr_name INTO wa_usr WHERE bname = sy-uname.

  CLEAR: d_heading[], hline, text.
  hline-typ  = 'H'.
  hline-info = sy-title.
  APPEND hline TO d_heading.

  CLEAR: hline, text.
  hline-typ  = 'S'.
  CONCATENATE 'Kulanıcı Adı :' wa_usr-name_text
         INTO text SEPARATED BY space.
  hline-info = text.
  APPEND hline TO d_heading.

  CLEAR: hline, text, lv_datum, lv_uzeit.
  hline-typ  = 'S'.

  CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum(4)
         INTO lv_datum.

  CONCATENATE sy-uzeit(2)   ':' sy-uzeit+2(2) ':' sy-uzeit+4(2)
         INTO lv_uzeit.

  CONCATENATE 'Tarih / Saat :' lv_datum '/' lv_uzeit
         INTO text SEPARATED BY space.
  hline-info = text.
  APPEND hline TO d_heading.

  lv_count = lines( mt_outdat ).
  hline-typ  = 'S'.
  CONCATENATE 'Kayıt Sayısı:' lv_count
         INTO text SEPARATED BY space.
  hline-info = text.
  APPEND hline TO d_heading.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = d_heading.

ENDFORM .                    "top_of_page