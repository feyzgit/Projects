mc_url     TYPE string VALUE 'https://dijital.gib.gov.tr/apigateway/verification/gecikmeZammiFaiziHesaplama/gecikmeZammiFaiziHesapla'


*&---------------------------------------------------------------------*
*&  Class           LCL_GIB_POST           Definition
*&---------------------------------------------------------------------*
CLASS lcl_http_post DEFINITION.

  PUBLIC SECTION.
    CLASS-DATA:
      mv_request  TYPE string,
      mv_response TYPE string.
    CLASS-METHODS:
      run_post
        IMPORTING
          im_url             TYPE string
          VALUE(im_request)  TYPE string
        RETURNING
          VALUE(rv_response) TYPE string,
      run_json_viewer
        IMPORTING
          im_jsondat TYPE string.

ENDCLASS.
*&---------------------------------------------------------------------*
*&  Class           LCL_HTTP_POST           Implementation
*&---------------------------------------------------------------------*
CLASS lcl_http_post IMPLEMENTATION.
  METHOD run_post.

    DATA: http_client   TYPE REF TO if_http_client,
          base64encoder TYPE REF TO cl_http_utility,
          _requestdat   TYPE xstring,
          _responsedat  TYPE xstring,
          _code         TYPE i,
          _reason       TYPE string,
          _basic        TYPE string,
          _base64       TYPE string,
          _subrc        TYPE i.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url    = im_url
      IMPORTING
        client = http_client.

    http_client->request->set_header_field( name = '~request_method' value = 'POST' ).
    http_client->request->set_header_field( name = 'Content-Type' value = 'application/json;charset=UTF-8' ).

    CLEAR: _requestdat.
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = im_request
      IMPORTING
        buffer = _requestdat
      EXCEPTIONS
        failed = 1
        OTHERS = 2.

    http_client->request->set_data( _requestdat ).

    CALL METHOD http_client->send
      EXPORTING
        timeout                    = 5000
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.
    IF sy-subrc <> 0.
      _subrc = sy-subrc.
      WRITE: 'HTTP communication failure (SEND failed: ', _subrc, '); aborting.'.
      http_client->close( ).
      EXIT.
    ENDIF.

    CALL METHOD http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.
    IF sy-subrc <> 0.
      _subrc = sy-subrc.
      WRITE: 'HTTP communication failure (RECV failed: ', _subrc, '); aborting.'.
      http_client->close( ).
      EXIT.
    ENDIF.

    CALL METHOD http_client->response->get_status
      IMPORTING
        code   = _code
        reason = _reason.

    FREE: _responsedat.
    _responsedat = http_client->response->get_data( ).

    IF _code < 200 OR _code >= 300.
      _subrc = _code.
      WRITE: 'HTTP communication failure (code:', _subrc, '); aborting.'.
      http_client->close( ).
      EXIT.
    ELSE.
      CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
        EXPORTING
          im_xstring = _responsedat
        IMPORTING
          ex_string  = rv_response.
    ENDIF.
    http_client->close( ).

  ENDMETHOD.                    "run_post
  METHOD run_json_viewer.

    cl_demo_output=>display_json( im_jsondat ).

  ENDMETHOD.                    "run_xml_viewer
ENDCLASS.                    "http_post IMPLEMENTATION





      FREE:lcl_http_post=>mv_request.
      lcl_http_post=>mv_request = '{"data":[{"gecikmeTipi":1,"odenecekMiktar":"#<OUTDAT>-DMBTR#","vadeTarihi":"#<OUTDAT>-ZFBDT#","odemeTarihi":"#<OUTDAT>-KEYDAT#"}]}'.

      REPLACE ALL OCCURRENCES OF '#<OUTDAT>-DMBTR#'  IN lcl_http_post=>mv_request WITH |{ <outdat>-dmbtr }|.
      REPLACE ALL OCCURRENCES OF '#<OUTDAT>-ZFBDT#'  IN lcl_http_post=>mv_request WITH |{ <outdat>-zfbdt }|.
      REPLACE ALL OCCURRENCES OF '#<OUTDAT>-KEYDAT#' IN lcl_http_post=>mv_request WITH |{ <outdat>-keydat }|.

      lcl_http_post=>run_post(
        EXPORTING
          im_url      = mc_url
          im_request  = lcl_http_post=>mv_request
        RECEIVING
          rv_response = lcl_http_post=>mv_response ).

      FREE: _values.
      cl_fdt_json=>json_to_data(
        EXPORTING
          iv_json = lcl_http_post=>mv_response
        CHANGING
          ca_data = _values ).

      <outdat> = CORRESPONDING #( BASE ( <outdat> ) _values ).