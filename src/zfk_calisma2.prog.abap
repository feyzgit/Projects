*&---------------------------------------------------------------------*
*& Report ZCALISMA2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_calisma2.

DATA: gt_card TYPE TABLE OF zodev_odeme_kart,
      gs_card TYPE zodev_odeme_kart,
      message type c.




SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01 .

PARAMETERS:reg_card RADIOBUTTON GROUP g1 USER-COMMAND cmd1 DEFAULT 'X',
           card_nam TYPE c LENGTH 20 MODIF ID abc.
SELECTION-SCREEN SKIP     3.
PARAMETERS:new_card RADIOBUTTON GROUP g1,
           card_no  TYPE  zodev_odeme_kart-cardid MODIF ID xyz,
           to_date  TYPE zodev_odeme_kart-todate MODIF ID xyz,
           cc2_code TYPE zodev_odeme_kart-cc2code MODIF ID xyz,
           3d_sec   AS CHECKBOX USER-COMMAND cb1 MODIF ID xyz,
           saved    AS CHECKBOX USER-COMMAND cb1 MODIF ID xyz,
           card_na  TYPE c LENGTH 20 MODIF ID asd.


SELECTION-SCREEN END OF BLOCK b1.






AT SELECTION-SCREEN OUTPUT.

  PERFORM f_selection_screen_output.



FORM f_selection_screen_output.
*  BREAK fkarakas.
  LOOP AT SCREEN.
    IF reg_card EQ 'X' AND screen-group1 = 'ABC'.
      screen-active = 1.
      MODIFY SCREEN.

    ELSEIF new_card EQ 'X' AND screen-group1 = 'XYZ' .
      screen-active = 1.
      MODIFY SCREEN.

    ELSEIF reg_card EQ ' ' AND screen-group1 = 'ABC'.
      screen-active = 0.
      MODIFY SCREEN.

    ELSEIF new_card EQ ' ' AND screen-group1 = 'XYZ' .
      screen-active = 0.
      MODIFY SCREEN.

    ENDIF.
    IF saved = 'X' AND reg_card EQ ' ' .
      IF screen-group1 EQ 'ASD'.
        screen-active =  1.
        MODIFY SCREEN.

      ENDIF.
    ELSE.
      IF screen-group1 EQ 'ASD'.
        screen-active =  0.
        MODIFY SCREEN.

      ENDIF.
    ENDIF.
  ENDLOOP.

*  IF new_card EQ ' ' AND saved = ' ' .
*    LOOP AT SCREEN.
*      IF screen-name cs 'CARD_NA'.
*        screen-active =  0.
*        MODIFY SCREEN.
*
*
*      ENDIF.
*
*    ENDLOOP.
*  ELSEIF new_card EQ 'X' AND saved = 'X'.
*    LOOP AT SCREEN.
*      IF screen-name cs 'CARD_NA'.
*        screen-active = 1 .
*        MODIFY SCREEN.
*
*
*      ENDIF.
*
*    ENDLOOP.
*
*
*  ENDIF.
ENDFORM.

START-OF-SELECTION.


  IF reg_card = 'X'..

    IF card_nam IS INITIAL.
      WRITE 'kart adı giriniz'.

    ELSE.
      SELECT SINGLE * FROM zodev_odeme_kart INTO gs_card WHERE cardname EQ card_nam.
      IF sy-subrc = 0.
        WRITE 'karttan 100 tl çekildi.'.
      ELSE.
        WRITE 'kayıtlı değil.'.
      ENDIF.


    ENDIF.
  ELSE.

    IF saved = 'X' or 3d_sec = 'X'..
      gs_card-cardid = card_no.
      gs_card-cardname = card_na.
      gs_card-cc2code = cc2_code.
      gs_card-todate = to_date.
      INSERT zodev_odeme_kart FROM gs_card.
      CONCATENATE card_no 'kart numarası' INTO message .
      WRITE: message.
      WRITE 'telefona mesaj gönderildi.'.


    ENDIF.
    CLEAR gs_card.
*    IF 3d_sec = 'X'.
*      WRITE 'telefona mesaj gönderildi.'.
*    ENDIF.


  ENDIF.
