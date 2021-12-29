*&---------------------------------------------------------------------*
*& Report ZCALISMA1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfk_calisma1.
DATA: lv_num1 TYPE i,
      oper    TYPE c,
      gs_num2 TYPE i.


PARAMETERS:
  number  TYPE c LENGTH 10 .



SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME  ,
                  SKIP 3,
                  BEGIN OF LINE,
                  PUSHBUTTON 2(10)  TEXT-001 USER-COMMAND f1,
                  PUSHBUTTON 12(10) TEXT-002 USER-COMMAND f2 VISIBLE LENGTH 10,
                  PUSHBUTTON 22(10) TEXT-003 USER-COMMAND f3,
                  END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE,
                  PUSHBUTTON 2(10)  TEXT-004 USER-COMMAND f4,
                  PUSHBUTTON 12(10) TEXT-005 USER-COMMAND f5 VISIBLE LENGTH 10,
                  PUSHBUTTON 22(10) TEXT-006 USER-COMMAND f6,
                  END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE,
                  PUSHBUTTON 2(10)  TEXT-007 USER-COMMAND f7,
                  PUSHBUTTON 12(10) TEXT-008 USER-COMMAND f8 VISIBLE LENGTH 10,
                  PUSHBUTTON 22(10) TEXT-009 USER-COMMAND f9,
                  END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE,
                  PUSHBUTTON 12(10) TEXT-010 USER-COMMAND f10 VISIBLE LENGTH 10,
                  END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE,
                  PUSHBUTTON 30(5) push1 USER-COMMAND f11,
                  PUSHBUTTON 35(5) push2 USER-COMMAND f12,
                  END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE,
                  PUSHBUTTON 30(5) push3 USER-COMMAND f13,
                  PUSHBUTTON 35(5) push4 USER-COMMAND f14,
                  END OF LINE.
SELECTION-SCREEN: BEGIN OF LINE,
                  PUSHBUTTON 33(5) push5 USER-COMMAND f15,


                  END OF LINE.

SELECTION-SCREEN END OF BLOCK b2.

LOAD-OF-PROGRAM.
*  BREAK fkarakas.

INITIALIZATION.
*  BREAK fkarakas.
  PERFORM f_initialization.


AT SELECTION-SCREEN .
*  BREAK fkarakas.
  PERFORM f_at_selection_screen .


AT SELECTION-SCREEN OUTPUT.
*  BREAK fkarakas.
  PERFORM f_selection_screen_output.

START-OF-SELECTION.
*  BREAK fkarakas.


END-OF-SELECTION .
*  BREAK fkarakas.
*&---------------------------------------------------------------------*
*& Form F_INITIALIZATION
*&---------------------------------------------------------------------*
FORM f_initialization .
  push1 = '+'.
  push2 = '-'.
  push3 = '*'.
  push4 = '/'.
  push5 = '='.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM f_at_selection_screen .
  CASE sy-ucomm .
    WHEN 'F1'.
      CONCATENATE number '1' INTO number.
    WHEN 'F2'.
      CONCATENATE number '2' INTO number.
    WHEN 'F3'.
      CONCATENATE number '3' INTO number.
    WHEN 'F4'.
      CONCATENATE number '4' INTO number.
    WHEN 'F5'.
      CONCATENATE number '5' INTO number.
    WHEN 'F6'.
      CONCATENATE number '6' INTO number.
    WHEN 'F7'.
      CONCATENATE number '7' INTO number.
    WHEN 'F8'.
      CONCATENATE number '8' INTO number.
    WHEN 'F9'.
      CONCATENATE number '9' INTO number.
    WHEN 'F10'.
      CONCATENATE number '0' INTO number.

  ENDCASE.


  CASE sy-ucomm.
    WHEN 'F11'.
      oper = '+'.
      IF lv_num1 IS INITIAL.
        lv_num1 = number .
        CLEAR: number.
      ELSE.
        gs_num2 = number.
        number = lv_num1 + gs_num2.
        CLEAR lv_num1.
      ENDIF.

    WHEN 'F12'.
      oper = '-'.
      IF lv_num1 IS INITIAL.
        lv_num1 = number.
        CLEAR number.
      ELSE.
        gs_num2 = number.
        number = lv_num1 - gs_num2.
        CLEAR lv_num1.

      ENDIF.

    WHEN 'F13'.
      oper = '*'.
      IF lv_num1 IS INITIAL.
        lv_num1 = number.
        CLEAR number.
      ELSE.
        gs_num2 = number.
        number = lv_num1 * gs_num2.
        CLEAR lv_num1.

      ENDIF.


    WHEN 'F14'.
      oper = '/'.
      IF lv_num1 IS INITIAL.
        lv_num1 = number.
        CLEAR number.
      ELSE.
        gs_num2 = number.
        number = lv_num1 / gs_num2.
        CLEAR lv_num1.

      ENDIF.

    WHEN 'F15'.
      CASE oper.
        WHEN '+'.
          IF lv_num1 IS INITIAL.
            lv_num1 = number .
            CLEAR: number.
          ELSE.
            gs_num2 = number.
            number = lv_num1 + gs_num2.
            CLEAR lv_num1.
          ENDIF.
        WHEN '-'.
          IF lv_num1 IS INITIAL.
            lv_num1 = number.
            CLEAR number.
          ELSE.
            gs_num2 = number.
            number = lv_num1 - gs_num2.
            CLEAR lv_num1.
          ENDIF.

        WHEN '*'.
          IF lv_num1 IS INITIAL.
            lv_num1 = number.
            CLEAR number.
          ELSE.
            gs_num2 = number.
            number = lv_num1 * gs_num2.
            CLEAR lv_num1.
          ENDIF.

        WHEN '/'.
          IF lv_num1 IS INITIAL.
            lv_num1 = number.
            CLEAR number.
          ELSE.
            gs_num2 = number.
            number = lv_num1 / gs_num2.
            CLEAR lv_num1.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.

  ENDCASE.


  CONDENSE number NO-GAPS.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SELECTION_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
FORM f_selection_screen_output .

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'NUMBER'.
        screen-input = '0'.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
