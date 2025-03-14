CLASS zcl_bc_file_templ_toolkit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS display_templ
      IMPORTING
        !im_objname     TYPE w3objid
        !im_mtype       TYPE w3conttype DEFAULT 'Excel'
        !im_appli       TYPE w3filename DEFAULT 'Excel.exe'
      RETURNING
        VALUE(r_return) TYPE bapiret2_tab .
    METHODS download_templ
      IMPORTING
        !im_objname     TYPE w3objid
      EXPORTING
        !ev_path        TYPE localfile
      RETURNING
        VALUE(r_return) TYPE bapiret2_tab .
    METHODS upload_templ
      IMPORTING
        !im_objname     TYPE w3objid
        !im_objtext     TYPE w3_text
        !im_package     TYPE devclass
      RETURNING
        VALUE(r_return) TYPE bapiret2_tab .
    METHODS show_message
      IMPORTING
        !im_retdat TYPE bapiret2_tab .
    METHODS crud_minetypes .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF _cons,
        relid_mi      TYPE wwwdata-relid VALUE 'MI',
        pgmid_r3tr    TYPE tadir-pgmid VALUE 'R3TR',
        object_w3mi   TYPE tadir-object VALUE 'W3MI',
        minetypes_prg TYPE progname VALUE '/1BCDWB/DBMIMETYPES',
      END OF _cons .
    CONSTANTS:
      BEGIN OF _msg,
        id      TYPE symsgid VALUE 'ZFI_028',
        success TYPE bapi_mtype VALUE 'S',
        error   TYPE bapi_mtype VALUE 'E',
        warning TYPE bapi_mtype VALUE 'W',
        info    TYPE bapi_mtype VALUE 'I',
        abort   TYPE bapi_mtype VALUE 'A',
      END OF _msg .
ENDCLASS.



CLASS ZCL_BC_FILE_TEMPL_TOOLKIT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILE_TEMPL_TOOLKIT->CRUD_MINETYPES
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD crud_minetypes.

    SUBMIT (_cons-minetypes_prg) AND RETURN.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILE_TEMPL_TOOLKIT->DISPLAY_TEMPL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_OBJNAME                     TYPE        W3OBJID
* | [--->] IM_MTYPE                       TYPE        W3CONTTYPE (default ='Excel')
* | [--->] IM_APPLI                       TYPE        W3FILENAME (default ='Excel.exe')
* | [<-()] R_RETURN                       TYPE        BAPIRET2_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD display_templ.

    DATA: l_wwwdata_tab TYPE wwwdatatab,
          l_objid       TYPE wwwdataid,
          t_mimeapps    TYPE TABLE OF w3mimeappl WITH DEFAULT KEY.

*-&Make sure template exists in SMW0 as binary object->
    CLEAR: l_wwwdata_tab.
    SELECT * FROM wwwdata
             INNER JOIN tadir ON wwwdata~objid = tadir~obj_name
             INTO CORRESPONDING FIELDS OF @l_wwwdata_tab UP TO 1 ROWS
             WHERE wwwdata~srtf2  = 0
               AND wwwdata~relid  = @_cons-relid_mi
               AND tadir~pgmid    = @_cons-pgmid_r3tr
               AND tadir~object   = @_cons-object_w3mi
               AND tadir~obj_name = @im_objname.
    ENDSELECT.
    IF sy-subrc <> 0.
      r_return = VALUE #( BASE r_return ( id = _msg-id
                                          number = '022'
                                          type = _msg-error
                                          message_v1 = im_objname ) ).
      EXIT.
    ENDIF.

    CONCATENATE 'mimeappl' sy-uname INTO l_objid-objid.
    IMPORT templ = t_mimeapps FROM DATABASE wwwdata(st) ID l_objid.

    IF sy-subrc = 0.
      READ TABLE t_mimeapps INTO DATA(l_mimeapps) WITH KEY mtype = im_mtype.
      IF sy-subrc <> 0.
        l_mimeapps-mtype = im_mtype.
        l_mimeapps-appli = im_appli.
        APPEND l_mimeapps TO t_mimeapps.
        EXPORT templ = t_mimeapps TO DATABASE wwwdata(st) ID l_objid.
      ENDIF.
    ELSE.
      l_mimeapps-mtype = im_mtype.
      l_mimeapps-appli = im_appli.
      APPEND l_mimeapps TO t_mimeapps.
      EXPORT templ = t_mimeapps TO DATABASE wwwdata(st) ID l_objid.
    ENDIF.

    CALL FUNCTION 'SHOW_WEB_OBJECT'
      EXPORTING
        key               = l_wwwdata_tab
      EXCEPTIONS
        canceled_by_user  = 2
        program_not_found = 3.
    IF sy-subrc <> 0.
      r_return = VALUE #( BASE r_return ( id = _msg-id
                                          number = '023'
                                          type = _msg-error
                                          message_v1 = im_objname ) ).
      EXIT.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILE_TEMPL_TOOLKIT->DOWNLOAD_TEMPL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_OBJNAME                     TYPE        W3OBJID
* | [<---] EV_PATH                        TYPE        LOCALFILE
* | [<-()] R_RETURN                       TYPE        BAPIRET2_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download_templ.

    DATA: l_wwwdata_tab TYPE wwwdatatab,
          l_destination TYPE rlgrap-filename,
          l_rc          TYPE sy-subrc,
          l_filename    TYPE string,
          l_filepath    TYPE string,
          l_fullpath    TYPE string,
          l_file_filter TYPE string,
          l_user_action TYPE i.

*-&Make sure template exists in SMW0 as binary object;
    CLEAR: l_wwwdata_tab.
    SELECT * FROM wwwdata
             INNER JOIN tadir ON wwwdata~objid = tadir~obj_name
             INTO CORRESPONDING FIELDS OF @l_wwwdata_tab  UP TO 1 ROWS
             WHERE wwwdata~srtf2  = 0
               AND wwwdata~relid  = @_cons-relid_mi
               AND tadir~pgmid    = @_cons-pgmid_r3tr
               AND tadir~object   = @_cons-object_w3mi
               AND tadir~obj_name = @im_objname.
    ENDSELECT.
    IF sy-subrc <> 0.
      r_return = VALUE #( BASE r_return ( id = _msg-id
                                          number = '022'
                                          type = _msg-error
                                          message_v1 = im_objname ) ).
      EXIT.
    ENDIF.

    CLEAR: l_filename,
           l_filepath,
           l_fullpath,
           l_file_filter,
           l_user_action.

    l_file_filter = 'Excel(*.xlsx)|*.xlsx|Excel(*.xlsm)|*.xlsm' &&
                    '|Excel 97-2003(*.xls)|*.xls'.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title         = CONV #( TEXT-t01 )
        file_filter          = l_file_filter
      CHANGING
        filename             = l_filename
        path                 = l_filepath
        fullpath             = l_fullpath
        user_action          = l_user_action
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    IF l_user_action <> 0.
      RETURN.
    ENDIF.

    CLEAR: l_destination.
    l_destination = l_fullpath.

    CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
      EXPORTING
        key         = l_wwwdata_tab
        destination = l_destination
      IMPORTING
        rc          = l_rc
      CHANGING
        temp        = l_destination.

    ev_path = l_destination.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILE_TEMPL_TOOLKIT->SHOW_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_RETDAT                      TYPE        BAPIRET2_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD show_message.

    CHECK NOT im_retdat[] IS INITIAL.
    CALL FUNCTION 'SUSR_DISPLAY_LOG'
      EXPORTING
        display_in_popup = abap_true
        log_title        = TEXT-t02
      TABLES
        it_log_bapiret2  = im_retdat[]
      EXCEPTIONS
        OTHERS           = 1.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILE_TEMPL_TOOLKIT->UPLOAD_TEMPL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_OBJNAME                     TYPE        W3OBJID
* | [--->] IM_OBJTEXT                     TYPE        W3_TEXT
* | [--->] IM_PACKAGE                     TYPE        DEVCLASS
* | [<-()] R_RETURN                       TYPE        BAPIRET2_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD upload_templ.

    DATA: l_key TYPE wwwdatatab,
          l_rc  TYPE sy-subrc.

    CLEAR: l_key.
    l_key-relid = _cons-relid_mi.
    l_key-objid = im_objname.
    l_key-tdate = sy-datum.
    l_key-ttime = sy-uzeit.
    l_key-text = im_objtext.
    l_key-devclass = im_package.

    CALL FUNCTION 'UPLOAD_WEB_OBJECT'
      EXPORTING
        key = l_key
      IMPORTING
        rc  = l_rc.

    r_return = VALUE #( BASE r_return ( id = sy-msgid
                                        type = sy-msgty
                                        number = sy-msgno
                                        message_v1 = sy-msgv1
                                        message_v2 = sy-msgv2
                                        message_v3 = sy-msgv3
                                        message_v4 = sy-msgv4 ) ).

  ENDMETHOD.
ENDCLASS.