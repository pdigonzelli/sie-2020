&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-callCompleted) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE callCompleted Procedure 
PROCEDURE callCompleted :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY "F10" TO TARGET-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-cierreStockMensual) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cierreStockMensual Procedure 
PROCEDURE cierreStockMensual :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hApp    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hAsy    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE lFlag   AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cPdf    AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE cSub AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMes AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTo  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFil AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDir AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE j    AS INTEGER    NO-UNDO.


  cTo  = "gprochaz@sa-sanmiguel.com,rvelez@sa-sanmiguel.com,maxivm@sa-sanmiguel.com,facundoj@sa-sanmiguel.com".
  cPrt = "PdfFactory Pro".
  cDir = "..\industria\stocks\" + STRING(YEAR(TODAY)) + "-" + STRING(MONTH(TODAY) - 1, "99") + "\".

  OS-COMMAND SILENT VALUE('sc \\meta2 stop lpdsvc').
  OS-COMMAND SILENT VALUE('sc \\meta2 stop spooler').
  OS-COMMAND SILENT VALUE('sc \\meta2 start spooler').
  
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  hApp = DYNAMIC-FUNCTION('connectApp' IN hLibCom).

  IF VALID-HANDLE(hApp) THEN 
    RUN libReportes.p PERSISTENT SET hLib ON SERVER hApp TRANSACTION DISTINCT.
  ELSE 
    RUN libReportes.p PERSISTENT SET hLib .
  
  PROCESS EVENTS.
  lFlag = VALID-HANDLE(hApp).
  /*lFlag = FALSE.*/

  define var v_filtro as char.
  define var v_sucursal as integer.
  define var v_articulo as integer.
  define var v_calidad as integer.
  define var v_fecha_desde as date.
  define var v_fecha_hasta as date.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  DEFINE VARIABLE cReport AS CHARACTER  NO-UNDO.
  

  
  v_fecha_desde = DATE( "01/" + STRING(MONTH(TODAY - 1)) + "/" + STRING(YEAR(TODAY)) ).
  v_fecha_hasta = TODAY.
  
  /*******CASCARA ************/
  
  v_sucursal = 0.
  v_articulo = 54.
  v_calidad = 0.


  PROCESS EVENTS.

  IF lFlag THEN DO:  
    RUN callProduccionIndustria IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" 
                                        (0, 
                                         v_articulo, 
                                         v_fecha_desde, 
                                         v_fecha_hasta, 
                                         "Lotes", 
                                         FALSE,
                                         FALSE).
    WAIT-FOR F10 OF THIS-PROCEDURE.
  END.
  ELSE
    RUN callProduccionIndustria IN hLib (0, 
                                         v_articulo, 
                                         v_fecha_desde, 
                                         v_fecha_hasta, 
                                         "Lotes", 
                                         FALSE,
                                         FALSE).


  
   cReport = "parte_produccion_rapido".

   cPdf = DYNAMIC-FUNCTION('reportToPdf' IN hLibCom, "reports.prl",
                                                      cReport,
                                                      cPrt, 
                                                      "").

   /*********************************************************************************/
       
  cSub = "Cierre Automatico de Stock de Cascara Mes " + STRING(MONTH(TODAY) - 1).
  cMes = "Cierre Automatico de Stock de Cascara Mes " + STRING(MONTH(TODAY) - 1).
  cFil = cPdf.
  
  /*delay*/
  DO i = 1 TO 50000:
    j = j + 1.
  END.
  
  RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).
  OS-COPY VALUE(cPdf)  VALUE(cDir + "produccionCascara.pdf").

  

  /*************************************************************************************/

  
  
  /***** JUGO TURBIO ****/
  v_sucursal = 0.
  v_articulo = 52.
  v_calidad = 0.


  PROCESS EVENTS.

  IF lFlag THEN DO:  
    RUN callProduccionIndustria IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" 
                                        (0, 
                                         v_articulo, 
                                         v_fecha_desde, 
                                         v_fecha_hasta, 
                                         "Lotes", 
                                         FALSE,
                                         FALSE).
    WAIT-FOR F10 OF THIS-PROCEDURE.
  END.
  ELSE
    RUN callProduccionIndustria IN hLib (0, 
                                         v_articulo, 
                                         v_fecha_desde, 
                                         v_fecha_hasta, 
                                         "Lotes", 
                                         FALSE,
                                         FALSE).


  
   cReport = "parte_produccion_rapido".

   cPdf = DYNAMIC-FUNCTION('reportToPdf' IN hLibCom, "reports.prl",
                                                      cReport,
                                                      cPrt, 
                                                      "").



 
  /************************************************************************************************************/

  cSub = "Cierre Automatico de Stock de Jugo Turbio Mes " + STRING(MONTH(TODAY) - 1).
  cMes = "Cierre Automatico de Stock de Jugo Turbio Mes " + STRING(MONTH(TODAY) - 1).
  cFil = cPdf.
  
  /*delay*/
  DO i = 1 TO 50000:
    j = j + 1.
  END.
  
  RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).
  OS-COPY VALUE(cPdf)  VALUE(cDir + "produccionTurbio.pdf").

  

  /*************************************************************************************/


  /***** JUGO CLARO ****/
  v_sucursal = 0.
  v_articulo = 53.
  v_calidad = 0.


  PROCESS EVENTS.

  IF lFlag THEN DO:  
    RUN callProduccionIndustria IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" 
                                        (0, 
                                         v_articulo, 
                                         v_fecha_desde, 
                                         v_fecha_hasta, 
                                         "Lotes", 
                                         FALSE,
                                         FALSE).
    WAIT-FOR F10 OF THIS-PROCEDURE.
  END.
  ELSE
    RUN callProduccionIndustria IN hLib (0, 
                                         v_articulo, 
                                         v_fecha_desde, 
                                         v_fecha_hasta, 
                                         "Lotes", 
                                         FALSE,
                                         FALSE).


  
   cReport = "parte_produccion_rapido".

   cPdf = DYNAMIC-FUNCTION('reportToPdf' IN hLibCom, "reports.prl",
                                                      cReport,
                                                      cPrt, 
                                                      "").

 
  /************************************************************************************************************/

   cSub = "Cierre Automatico de Stock de Jugo Claro Mes " + STRING(MONTH(TODAY) - 1).
   cMes = "Cierre Automatico de Stock de Jugo Claro Mes " + STRING(MONTH(TODAY) - 1).
   cFil = cPdf.
     
   /*delay*/
   DO i = 1 TO 50000:
     j = j + 1.
   END.
  
   RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).
   OS-COPY VALUE(cPdf)  VALUE(cDir + "produccionClaro.pdf").


   /*************************************************************************************/


  /***** ACEITE ****/
  v_sucursal = 0.
  v_articulo = 51.
  v_calidad = 0.


  PROCESS EVENTS.

  IF lFlag THEN DO:  
    RUN callProduccionIndustria IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" 
                                        (0, 
                                         v_articulo, 
                                         v_fecha_desde, 
                                         v_fecha_hasta, 
                                         "Lotes", 
                                         FALSE,
                                         FALSE).
    WAIT-FOR F10 OF THIS-PROCEDURE.
  END.
  ELSE
    RUN callProduccionIndustria IN hLib (0, 
                                         v_articulo, 
                                         v_fecha_desde, 
                                         v_fecha_hasta, 
                                         "Lotes", 
                                         FALSE,
                                         FALSE).


  
   cReport = "parte_produccion_rapido".

   cPdf = DYNAMIC-FUNCTION('reportToPdf' IN hLibCom, "reports.prl",
                                                      cReport,
                                                      cPrt, 
                                                      "").

 
  /************************************************************************************************************/

  cSub = "Cierre Automatico de Stock de Aceite Mes " + STRING(MONTH(TODAY) - 1).
  cMes = "Cierre Automatico de Stock de Aceite Mes " + STRING(MONTH(TODAY) - 1).
  cFil = cPdf.
     
  /*delay*/
  DO i = 1 TO 50000:
    j = j + 1.
  END.
  
  RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).
  OS-COPY VALUE(cPdf)  VALUE(cDir + "produccionAceite.pdf").


  /*************************************************************************************/


  /***** PULPA ****/
  v_sucursal = 0.
  v_articulo = 71.
  v_calidad = 0.


  PROCESS EVENTS.

  IF lFlag THEN DO:  
    RUN callProduccionIndustria IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" 
                                        (0, 
                                         v_articulo, 
                                         v_fecha_desde, 
                                         v_fecha_hasta, 
                                         "Lotes", 
                                         FALSE,
                                         FALSE).
    WAIT-FOR F10 OF THIS-PROCEDURE.
  END.
  ELSE
    RUN callProduccionIndustria IN hLib (0, 
                                         v_articulo, 
                                         v_fecha_desde, 
                                         v_fecha_hasta, 
                                         "Lotes", 
                                         FALSE,
                                         FALSE).


  
   cReport = "parte_produccion_rapido".

   cPdf = DYNAMIC-FUNCTION('reportToPdf' IN hLibCom, "reports.prl",
                                                      cReport,
                                                      cPrt, 
                                                      "").

 
  /************************************************************************************************************/

   cSub = "Cierre Automatico de Stock de Pulpa Mes " + STRING(MONTH(TODAY) - 1).
   cMes = "Cierre Automatico de Stock de Pulpa Mes " + STRING(MONTH(TODAY) - 1).
   cFil = cPdf.
    
  /*delay*/
  DO i = 1 TO 80000:
    j = j + 1.
  END.  

   RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).
  OS-COPY VALUE(cPdf)  VALUE(cDir + "produccionPulpa.pdf").


 /*************************************************************************************/

  /***REINICIO SERVICIOS*****/
  OS-COMMAND SILENT VALUE('sc \\meta2 stop lpdsvc').
  OS-COMMAND SILENT VALUE('sc \\meta2 stop spooler').
  OS-COMMAND SILENT VALUE('sc \\meta2 start spooler').
  /********/


   /*Reporte de Stock Lavalle */

   IF lFlag THEN DO:    
     RUN callReporteStock IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" (96, FALSE) .
     WAIT-FOR F10 OF THIS-PROCEDURE.
   END.
   ELSE
     RUN callReporteStock IN hLib (96, FALSE).

   cReport = "reporte_stock_rapido_citrix_ord".

   cPdf = DYNAMIC-FUNCTION('reportToPdf' IN hLibCom, "reports.prl",
                                                      cReport,
                                                      cPrt, 
                                                      "").

   


   /************************************************************************************************************/

   cSub = "Reporte Stock Lavalle Mes " + STRING(MONTH(TODAY) - 1).
   cMes = "Reporte Stock Lavalle Mes " + STRING(MONTH(TODAY) - 1).
   cFil = cPdf.

    

   RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).
   OS-COPY VALUE(cPdf)  VALUE(cDir + "stockLavalle.pdf").

   /*************************************************************************************/

   /*Reporte de Stock Famailla */

   IF lFlag THEN DO:         
     RUN callReporteStock IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" (95, FALSE) .
     WAIT-FOR F10 OF THIS-PROCEDURE.
   END.
   ELSE
     RUN callReporteStock IN hLib (95, FALSE).

     cReport = "reporte_stock_rapido_citrix_ord".

     cPdf = DYNAMIC-FUNCTION('reportToPdf' IN hLibCom, "reports.prl",
                                                        cReport,
                                                        cPrt, 
                                                        "").

    PAUSE 50 NO-MESSAGE.


   /************************************************************************************************************/

   cSub = "Reporte Stock Famailla Mes " + STRING(MONTH(TODAY) - 1).
   cMes = "Reporte Stock Famailla Mes " + STRING(MONTH(TODAY) - 1).
   cFil = cPdf.


   RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).
   OS-COPY VALUE(cPdf)  VALUE(cDir + "stockFamailla.pdf").

   /*************************************************************************************/

  OS-DELETE VALUE(cPdf).
  OS-COMMAND SILENT VALUE('sc \\meta2 stop lpdsvc').
  OS-COMMAND SILENT VALUE('sc \\meta2 stop spooler').
  OS-COMMAND SILENT VALUE('sc \\meta2 start spooler').


  /*************************************************************************************/

  /*CASCARA FAMAILLA*/

  FOR EACH stock_tambores.
    DELETE stock_tambores.
  END.

  RUN p_calculo_stock_cascara.p (95, 54).

  cReport = "reporte_stock_rapido_cascara".

  cPdf = DYNAMIC-FUNCTION('reportToPdf' IN hLibCom, "reports.prl",
                                                        cReport,
                                                        cPrt, 
                                                        "").
  cSub = "Reporte Stock Cascara Famailla Mes " + STRING(MONTH(TODAY) - 1).
  cMes = "Reporte Stock Cascara Famailla Mes " + STRING(MONTH(TODAY) - 1).
  cFil = cPdf.


  RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).
  OS-COPY VALUE(cPdf)  VALUE(cDir + "stockCascaraFamailla.pdf").

  /***************************************************************************************/

  /*CASCARA LAVALLE*/

  FOR EACH stock_tambores.
    DELETE stock_tambores.
  END.

  RUN p_calculo_stock_cascara.p (96, 54).

  cReport = "reporte_stock_rapido_cascara".

  cPdf = DYNAMIC-FUNCTION('reportToPdf' IN hLibCom, "reports.prl",
                                                        cReport,
                                                        cPrt, 
                                                        "").

  cSub = "Reporte Stock Cascara Lavalle Mes " + STRING(MONTH(TODAY) - 1).
  cMes = "Reporte Stock Cascara Lavalle Mes " + STRING(MONTH(TODAY) - 1).
  cFil = cPdf.


  RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).
  OS-COPY VALUE(cPdf)  VALUE(cDir + "stockCascaraLavalle.pdf").

  /*******************************************************************************/

  /*CASCARA LIBERTAD*/

  FOR EACH stock_tambores.
    DELETE stock_tambores.
  END.

  RUN p_calculo_stock_cascara.p (159, 54).

  cReport = "reporte_stock_rapido_cascara".

  cPdf = DYNAMIC-FUNCTION('reportToPdf' IN hLibCom, "reports.prl",
                                                        cReport,
                                                        cPrt, 
                                                        "").
  cSub = "Reporte Stock Cascara Libertad Mes " + STRING(MONTH(TODAY) - 1).
  cMes = "Reporte Stock Cascara Libertad Mes " + STRING(MONTH(TODAY) - 1).
  cFil = cPdf.


  RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).
  OS-COPY VALUE(cPdf)  VALUE(cDir + "stockCascaraLibertad.pdf").

  /*******************************************************************************/

  /*CASCARA MERCOTUC*/

  FOR EACH stock_tambores.
    DELETE stock_tambores.
  END.

  RUN p_calculo_stock_cascara.p (92, 54).

  cReport = "reporte_stock_rapido_cascara".

  cPdf = DYNAMIC-FUNCTION('reportToPdf' IN hLibCom, "reports.prl",
                                                        cReport,
                                                        cPrt, 
                                                        "").
  cSub = "Reporte Stock Cascara Libertad Mes " + STRING(MONTH(TODAY) - 1).
  cMes = "Reporte Stock Cascara Libertad Mes " + STRING(MONTH(TODAY) - 1).
  cFil = cPdf.


  RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).
  OS-COPY VALUE(cPdf)  VALUE(cDir + "stockCascaraMercotuc.pdf").




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-stockDiario) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stockDiario Procedure 
PROCEDURE stockDiario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hApp    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hAsy    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hApi    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE lFlag   AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cPdf    AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE cSub AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMes AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTo  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFil AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDir AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cHst AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE j    AS INTEGER    NO-UNDO.

  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hApi = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libWindowsApi.p').
  DELETE OBJECT hLibCom.
  
  cHst = DYNAMIC-FUNCTION('getHost' IN hApi).

  cTo  = "gprochaz@sa-sanmiguel.com,rvelez@sa-sanmiguel.com,maxivm@sa-sanmiguel.com,facundoj@sa-sanmiguel.com".
  cTo  = "facundoj@sa-sanmiguel.com".
  cPrt = IF cHst = "META2" THEN "PdfFactory Pro" ELSE "pdfreleases".
  cDir = "..\industria\stocks\diarios\" + STRING(YEAR(TODAY), "9999") + "-" + STRING(MONTH(TODAY), "99") + "-" + STRING(DAY(TODAY), "99") + "\".

  OS-COMMAND SILENT VALUE('mkdir ' + cDir).
  
  OS-COMMAND SILENT VALUE('sc \\meta2 stop lpdsvc').
  OS-COMMAND SILENT VALUE('sc \\meta2 stop spooler').
  OS-COMMAND SILENT VALUE('sc \\meta2 start spooler').
  
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  hApp = DYNAMIC-FUNCTION('connectApp' IN hLibCom).

  IF VALID-HANDLE(hApp) THEN 
    RUN libReportes.p PERSISTENT SET hLib ON SERVER hApp TRANSACTION DISTINCT.
  ELSE 
    RUN libReportes.p PERSISTENT SET hLib .
  
  PROCESS EVENTS.
  lFlag = VALID-HANDLE(hApp).
  /*lFlag = FALSE.*/

  define var v_filtro as char.
  define var v_sucursal as integer.
  define var v_articulo as integer.
  define var v_calidad as integer.
  define var v_fecha_desde as date.
  define var v_fecha_hasta as date.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  DEFINE VARIABLE cReport AS CHARACTER  NO-UNDO.
  

  
  v_fecha_desde = TODAY.
  v_fecha_hasta = TODAY.
  
  /***REINICIO SERVICIOS*****/
  OS-COMMAND SILENT VALUE('sc \\meta2 stop lpdsvc').
  OS-COMMAND SILENT VALUE('sc \\meta2 stop spooler').
  OS-COMMAND SILENT VALUE('sc \\meta2 start spooler').
  /********/


   /*Reporte de Stock Lavalle */

   IF lFlag THEN DO:    
     RUN callReporteStock IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" (96, FALSE) .
     WAIT-FOR F10 OF THIS-PROCEDURE.
   END.
   ELSE
     RUN callReporteStock IN hLib (96, FALSE).

   cReport = "reporte_stock_rapido_citrix_ord".

   cPdf = DYNAMIC-FUNCTION('reportToPdf' IN hLibCom, "reports.prl",
                                                      cReport,
                                                      cPrt, 
                                                      "").

   


   /************************************************************************************************************/

   cSub = "Reporte Stock Lavalle fecha " + STRING(TODAY).
   cMes = "Reporte Stock Lavalle fecha " + STRING(TODAY).
   cFil = cPdf.

    

   /*RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).*/
   OS-COPY VALUE(cPdf)  VALUE(cDir + "stockLavalle.pdf").

   /*************************************************************************************/

   /*Reporte de Stock Famailla */

   IF lFlag THEN DO:         
     RUN callReporteStock IN hLib ASYNCHRONOUS SET hAsy EVENT-PROCEDURE "callCompleted" (95, FALSE) .
     WAIT-FOR F10 OF THIS-PROCEDURE.
   END.
   ELSE
     RUN callReporteStock IN hLib (95, FALSE).

     cReport = "reporte_stock_rapido_citrix_ord".

     cPdf = DYNAMIC-FUNCTION('reportToPdf' IN hLibCom, "reports.prl",
                                                        cReport,
                                                        cPrt, 
                                                        "").

    PAUSE 50 NO-MESSAGE.


   /************************************************************************************************************/

   cSub = "Reporte Stock Famailla fecha " + STRING(TODAY).
   cMes = "Reporte Stock Famailla fecha " + STRING(TODAY).
   cFil = cPdf.


   /*RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).*/
   OS-COPY VALUE(cPdf)  VALUE(cDir + "stockFamailla.pdf").

   /*************************************************************************************/

  OS-DELETE VALUE(cPdf).
  OS-COMMAND SILENT VALUE('sc \\meta2 stop lpdsvc').
  OS-COMMAND SILENT VALUE('sc \\meta2 stop spooler').
  OS-COMMAND SILENT VALUE('sc \\meta2 start spooler').


  /*************************************************************************************/

  /*CASCARA FAMAILLA*/

  FOR EACH stock_tambores.
    DELETE stock_tambores.
  END.

  RUN p_calculo_stock_cascara.p (95, 54).

  cReport = "reporte_stock_rapido_cascara".

  cPdf = DYNAMIC-FUNCTION('reportToPdf' IN hLibCom, "reports.prl",
                                                        cReport,
                                                        cPrt, 
                                                        "").
  cSub = "Reporte Stock Cascara Famailla Mes " + STRING(MONTH(TODAY) - 1).
  cMes = "Reporte Stock Cascara Famailla Mes " + STRING(MONTH(TODAY) - 1).
  cFil = cPdf.


  /*RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).*/
  OS-COPY VALUE(cPdf)  VALUE(cDir + "stockCascaraFamailla.pdf").

  /***************************************************************************************/

  /*CASCARA LAVALLE*/

  FOR EACH stock_tambores.
    DELETE stock_tambores.
  END.

  RUN p_calculo_stock_cascara.p (96, 54).

  cReport = "reporte_stock_rapido_cascara".

  cPdf = DYNAMIC-FUNCTION('reportToPdf' IN hLibCom, "reports.prl",
                                                        cReport,
                                                        cPrt, 
                                                        "").

  cSub = "Reporte Stock Cascara Lavalle fecha " + STRING(TODAY).
  cMes = "Reporte Stock Cascara Lavalle fecha " + STRING(TODAY).
  cFil = cPdf.


  /*RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).*/
  OS-COPY VALUE(cPdf)  VALUE(cDir + "stockCascaraLavalle.pdf").

  /*******************************************************************************/

  /*CASCARA LIBERTAD*/

  FOR EACH stock_tambores.
    DELETE stock_tambores.
  END.

  RUN p_calculo_stock_cascara.p (159, 54).

  cReport = "reporte_stock_rapido_cascara".

  cPdf = DYNAMIC-FUNCTION('reportToPdf' IN hLibCom, "reports.prl",
                                                        cReport,
                                                        cPrt, 
                                                        "").
  cSub = "Reporte Stock Cascara Libertad fecha " + STRING(TODAY).
  cMes = "Reporte Stock Cascara Libertad fecha " + STRING(TODAY).
  cFil = cPdf.


  /*RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).*/
  OS-COPY VALUE(cPdf)  VALUE(cDir + "stockCascaraLibertad.pdf").

  /*******************************************************************************/

  /*CASCARA MERCOTUC*/

  FOR EACH stock_tambores.
    DELETE stock_tambores.
  END.

  RUN p_calculo_stock_cascara.p (92, 54).

  cReport = "reporte_stock_rapido_cascara".

  cPdf = DYNAMIC-FUNCTION('reportToPdf' IN hLibCom, "reports.prl",
                                                        cReport,
                                                        cPrt, 
                                                        "").
  cSub = "Reporte Stock Cascara Libertad fecha " + STRING(TODAY).
  cMes = "Reporte Stock Cascara Libertad fecha " + STRING(TODAY).
  cFil = cPdf.


  /*RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).*/
  OS-COPY VALUE(cPdf)  VALUE(cDir + "stockCascaraMercotuc.pdf").




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

