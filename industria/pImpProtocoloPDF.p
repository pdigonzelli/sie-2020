define input parameter p_nombre_rep as char.
define input parameter pprinter     as char.
define input parameter p_filtro_rep as char.
define input parameter p_param_rep as char.

DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".

/************************************************************************************************************/
/********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
/************************************************************************************************************/
                    
                    
                          RUN  aderb\_prntrb2(
                           "..\industria\reports_9.prl", /* RB-REPORT-LIBRARY */
                           p_nombre_rep,                    /* RB-REPORT-NAME */
                           "",                             /* RB-DB-CONNECTION */
                           "O",                             /* RB-INCLUDE-RECORDS */
                           p_filtro_rep,                              /* RB-FILTER */
                           RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                           "",                             /* RB-PRINT-DESTINATION */
                           pprinter,                              /* RB-PRINTER-NAME */
                           "",                              /* RB-PRINTER-PORT */
                           "",                              /* RB-OUTPUT-FILE */
                            1,                              /* RB-NUMBER-COPIES  - zero */                  
                            0,                              /* RB-BEGIN-PAGE - zero */
                            0,                              /* RB-END-PAGE - zero */
                           no,                              /* RB-TEST-PATTERN */
                           "",         /* RB-WINDOW-TITLE */
                           yes,                           /* RB-DISPLAY-ERRORS */
                           yes,                           /* RB-DISPLAY-STATUS */
                           no,                              /* RB-NO-WAIT */
                           "v_general = " + p_param_rep /* RB-OTHER-PARAMETERS */,
                           ""
                           ).   
                           
                    
                    
/************************************************************************************************************/
                     
