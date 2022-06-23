*&---------------------------------------------------------------------*
*& Report  ZCO_P_BUNDLE_DGT
*&---------------------------------------------------------------------*
*& Bundle Süreci Dağıtım Programı
*&---------------------------------------------------------------------*
*& -> DEV: Ayhan CENĞER
*& -> MOD: Ahmet GÖKTAŞ
*&---------------------------------------------------------------------*

REPORT zco_p_bundle_dgt.

INCLUDE zco_i_bundle_dgt_data.
INCLUDE zco_i_bundle_dgt_clsd.
INCLUDE zco_i_bundle_dgt_clsi.
INCLUDE zco_i_bundle_dgt_pbo.
INCLUDE zco_i_bundle_dgt_pai.

LOAD-OF-PROGRAM.
  lcl_main=>load_of_program( ).

INITIALIZATION.
  lcl_main=>initialization( ).

AT SELECTION-SCREEN OUTPUT.
  lcl_main=>at_selection_screen_output( ).


AT SELECTION-SCREEN.
  lcl_main=>at_selection_screen( ).

START-OF-SELECTION.
  lcl_main=>start_of_selection( ).

END-OF-SELECTION.
  lcl_main=>end_of_selection( ).