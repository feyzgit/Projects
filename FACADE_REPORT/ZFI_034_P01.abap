*&---------------------------------------------------------------------*
*& Program ZFI_034_P01
*&---------------------------------------------------------------------*
*& @author Abdullah ATAN <abdullah.atan@forcode.com.tr>
*&---------------------------------------------------------------------*
PROGRAM zfi_034_p01 MESSAGE-ID zfi_034.

INCLUDE zfi_034_p01_topdat.
INCLUDE zfi_034_p01_clsdat.
INCLUDE zfi_034_p01_scrdat.
INCLUDE zfi_034_p01_facade.

INITIALIZATION.
  TRY.
      lcl_selection_screen_control=>initialization( ).
    CATCH zcx_tjk INTO mr_message.
      MESSAGE mr_message.
  ENDTRY.

AT SELECTION-SCREEN OUTPUT.
  TRY.
      lcl_selection_screen_control=>at_selection_screen_output( ).
    CATCH zcx_tjk INTO mr_message.
      MESSAGE mr_message.
  ENDTRY.

AT SELECTION-SCREEN.
  TRY.
      lcl_selection_screen_control=>at_selection_screen( ).
    CATCH zcx_tjk INTO mr_message.
      MESSAGE mr_message.
  ENDTRY.

START-OF-SELECTION.
  TRY.
      lcl_selection_screen_control=>start_of_selection( ).
    CATCH zcx_tjk INTO mr_message.
      MESSAGE mr_message.
  ENDTRY.


SELECT-TEXT
B01 @4T@ » RAPOR TÜRÜ SEÇİMİ:
B02 @B8@ » RAPOR VERİ SEÇİMLERİ:
H02 ____________________________________

P_BDATE Başlangıç tarihi
P_EDATE Bitiş tarihi
P_HSVAL ?...
P_PASSW Şifre
P_REPEX Rapor türü
P_UNAME Kullanıcı adı 


ZFI_034
000 &1 &2 &3 &4
001 Login servis hatası: Hata: &1
002 Login cookie verisine erişilemedi! Teknik sorumluya başvurun!
003 Kullanıcı adı ve şifre geçersiz!
004 Bayi satış raporu servis hatası: Hata: &1 &2 &3 &4
005 Fieldcatalog oluşturulamadı! Lütfen sistem yöneticinize başvurun.
006 Rapor türü: &1 için &2 sınıf geliştirmesi bulunamadı!
007 Geçersiz adım yürütüldü! Sistem yöneticinize başvurun!
___ _________________________________________________________________________