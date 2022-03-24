*->Begin of -aatan/09.02.2021 {

DATA: lv_col TYPE i,

ls_excel TYPE zzalsmex_tabline.



FIELD-SYMBOLS: <fs_mapdat>, <dyn_field>.

LOOP AT intern INTO ls_excel.

lv_col = ls_excel-col.

ASSIGN it_mapdat TO <fs_mapdat>.

ASSIGN COMPONENT lv_col OF STRUCTURE <fs_mapdat> TO <dyn_field>.

IF sy-subrc = 0.

<dyn_field> = ls_excel-value.

ENDIF.

AT END OF row.

APPEND <fs_mapdat> TO it_mapdat.

CLEAR <fs_mapdat>.

ENDAT.

ENDLOOP.

*->End of -aatan/09.02.2021 }