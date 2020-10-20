DEF INPUT PARAMETER v-empresa AS INTEGER.
DEF INPUT PARAMETER v-sector AS INTEGER.
DEF INPUT PARAMETER v-sucursal AS INTEGER.
DEF INPUT PARAMETER v-desde-fecha AS DATE.
DEF INPUT PARAMETER v-hasta-fecha AS DATE.
DEF INPUT PARAMETER v-finca AS LOGICAL.
DEF INPUT PARAMETER v-tipo AS CHARACTER.
DEF INPUT PARAMETER v-impresora AS CHARACTER.
 
 define var v_general as character.
 DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
 DEFINE VAR v_nro_reporte AS INTEGER.

define var v_filtro as character initial "".
define var v_nombre_empresa as character.

    v_nro_reporte = RANDOM(1,1000000).
      
    
    {s_varsis.i}


    v_filtro = "rb_items_tarja.id_empresa = " + STRING(v-empresa).
       
      IF v-sucursal <> 0 THEN
        v_filtro = v_filtro + " and rb_items_tarja.id_sucursal = " + string(v-sucursal).

      IF v-sector <> 0 THEN
        v_filtro = v_filtro + " and rb_items_tarja.id_sector = " + STRING(v-sector).

      RB-MEMO-FILE = SESSION:TEMP-DIRECTORY + STRING(v_nro_reporte) + '.TXT'.
      
      RUN  aderb\_prntrb2(
                (if index(propath,"supervisor.pl") > 0 then vlc_dir_objetos else vlc_dir_fuentes) +  "agricola~\agricola.prl", /* RB-REPORT-LIBRARY */
         "resumen_tarjas_legajo2", /* RB-REPORT-NAME */
         "",                             /* RB-DB-CONNECTION */
         "O",                             /* RB-INCLUDE-RECORDS */
         v_filtro,                              /* RB-FILTER */
         RB-MEMO-FILE,                              /* RB-MEMO-FILE */
         "D",                             /* RB-PRINT-DESTINATION */
         v-impresora,                   /* RB-PRINTER-NAME */
         "",                              /* RB-PRINTER-PORT */
         "",                              /* RB-OUTPUT-FILE */
          1,                              /* RB-NUMBER-COPIES  - zero */                  
          0,                              /* RB-BEGIN-PAGE - zero */
          0,                              /* RB-END-PAGE - zero */
         no,                              /* RB-TEST-PATTERN */
         "Tarja Personal",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "v_general = " + STRING(v-desde-fecha) + "|" + STRING(v-hasta-fecha) + "|" 
         /* RB-OTHER-PARAMETERS */,
         ""
         ).
  os-delete value(RB-MEMO-FILE).

