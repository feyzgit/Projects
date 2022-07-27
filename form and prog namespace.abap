CONSTANTS: BEGIN OF _cons,
             pnam_01 TYPE progname  VALUE 'ZQM_026_I_EXIT_SAPLQPAP_001',
             form_01 TYPE form_name VALUE 'MII_CONTROL_RESULT',
           END OF _cons.


PERFORM (_cons-form_01)
  IN PROGRAM (_cons-pnam_01) TABLES t_plmk
                                    t_qapo
                              USING i_qals IF FOUND.

*&---------------------------------------------------------------------*
*& INCLUDE ZQM_026_I_EXIT_SAPLQPAP_001
*&---------------------------------------------------------------------*
PROGRAM zqm_026_i_exit_saplqpap_001.

*&---------------------------------------------------------------------*
*& Form MII_CONTROL_RESULT
*&---------------------------------------------------------------------*
FORM mii_control_result TABLES t_plmk STRUCTURE plmkb
                               t_qapo STRUCTURE qapo
                         USING i_qals TYPE qals.


ENDFORM.