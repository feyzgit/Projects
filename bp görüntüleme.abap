DATA(request) = NEW cl_bupa_navigation_request( ).
    request->set_partner_number( i_bp ).                   " import your BP number here
    DATA(options) = NEW cl_bupa_dialog_joel_options( ).
    options->set_navigation_disabled( abap_true ).
    cl_bupa_dialog_joel=>start_with_navigation( iv_request = request
                                                iv_options = options ).