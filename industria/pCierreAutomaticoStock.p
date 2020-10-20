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
  cPrt = "pdfreleases".
  cDir = "..\industria\stocks\" + STRING(YEAR(TODAY)) + "-" + STRING(MONTH(TODAY) - 1, "99") + "\".
  MESSAGE cDir
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  /*  
  OS-COMMAND SILENT VALUE('sc \\meta2 stop lpdsvc').
  OS-COMMAND SILENT VALUE('sc \\meta2 stop spooler').
  OS-COMMAND SILENT VALUE('sc \\meta2 start spooler').*/

  PROPATH = PROPATH + ",..\industria\prowinapi".
  
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  /*hApp = DYNAMIC-FUNCTION('connectApp' IN hLibCom).*/

  IF VALID-HANDLE(hApp) THEN 
    RUN libReportes.p PERSISTENT SET hLib ON SERVER hApp TRANSACTION DISTINCT.
  ELSE 
    RUN libReportes.p PERSISTENT SET hLib .
  
  PROCESS EVENTS.
  lFlag = VALID-HANDLE(hApp).

  define var v_filtro as char.
  define var v_sucursal as integer.
  define var v_articulo as integer.
  define var v_calidad as integer.
  define var v_fecha_desde as date.
  define var v_fecha_hasta as date.
  DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".
  DEFINE VARIABLE cReport AS CHARACTER  NO-UNDO.
  

  
  v_fecha_desde = DATE( "01/" + STRING(MONTH(TODAY - 1)) + "/" + STRING(YEAR(TODAY)) ).
  v_fecha_desde = DATE("01/04/07").
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
  
  /*  
  RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).*/
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
  
  /*  
  RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).*/
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
     
   /*  
   RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).*/
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
  
  /*  
  RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).*/
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
    
   /* 
   RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil). */
  OS-COPY VALUE(cPdf)  VALUE(cDir + "produccionPulpa.pdf").


 /*************************************************************************************/

  /***REINICIO SERVICIOS*****/
  /*  
  OS-COMMAND SILENT VALUE('sc \\meta2 stop lpdsvc').
  OS-COMMAND SILENT VALUE('sc \\meta2 stop spooler').
  OS-COMMAND SILENT VALUE('sc \\meta2 start spooler').*/
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

    
   /* 
   RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil). */
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


   /************************************************************************************************************/

   cSub = "Reporte Stock Famailla Mes " + STRING(MONTH(TODAY) - 1).
   cMes = "Reporte Stock Famailla Mes " + STRING(MONTH(TODAY) - 1).
   cFil = cPdf.

   /*  
   RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).*/
   OS-COPY VALUE(cPdf)  VALUE(cDir + "stockFamailla.pdf").

   /*************************************************************************************/

  OS-DELETE VALUE(cPdf).
  /*  
  OS-COMMAND SILENT VALUE('sc \\meta2 stop lpdsvc').
  OS-COMMAND SILENT VALUE('sc \\meta2 stop spooler').
  OS-COMMAND SILENT VALUE('sc \\meta2 start spooler').*/


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

  /*  
  RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).*/
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

  /* 
  RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil). */
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

  /*  
  RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil).*/
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

  /* 
  RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, cFil). */
  OS-COPY VALUE(cPdf)  VALUE(cDir + "stockCascaraMercotuc.pdf").


QUIT.


PROCEDURE callCompleted:
  APPLY "F10" TO TARGET-PROCEDURE.
END PROCEDURE.
