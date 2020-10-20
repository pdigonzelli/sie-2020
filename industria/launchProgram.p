
DEFINE VARIABLE hApp     AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLib     AS HANDLE  NO-UNDO.
DEFINE VARIABLE lFlag    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cPath    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cProgram AS CHARACTER   NO-UNDO.

DEFINE VARIABLE hMnu AS WIDGET-HANDLE     NO-UNDO.
  

PROPATH = "..\,..\industria\,,src,..\industria\prowinapi," + PROPATH + "l:\desarrollo\ventas".
 
RUN ..\industria\wInicioIndustria.w PERSISTENT SET hMnu.
RUN initializeObject IN hMnu.
WAIT-FOR CLOSE OF hMnu.
QUIT.




/*RUN ..\industria\wBorrar.w.*/


/*
RUN libImpresionEtiquetas.p PERSISTENT SET hLib.
/*RUN etqLabelPepsiPreimpresa IN hLib ("\\192.168.1.104\ibm4400", 6).*/

RUN etqInspeccionesLoteRango IN hLib (1, 
                                      95, 
                                      3, 
                                      70012, 
                                      1, 
                                      1, 
                                      "Not Applicable", TRUE).

*/
  
/*
DEFINE VARIABLE hLibCom AS HANDLE.
RUN libCommonFunctions.p PERSISTENT SET hLibCom.
hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libImpresionEtiquetas.p').
DELETE OBJECT hLibCom.


RUN etqPruebaEtqInviolable IN hLib ("\\facundoj\ibm4400", "facu").
/*RUN etqLabelPrueba IN hLib ("\\192.168.1.104\ibm4400").*/

  */

/*
DEFINE VARIABLE hLibCom AS HANDLE.
RUN libCommonFunctions.p PERSISTENT SET hLibCom.
hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
DELETE OBJECT hLibCom.

DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.

cLot = dynamic-function('getNextNroLote' IN hLib, 95,53,2005).

MESSAGE cLot
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

*/

/*
RUN libProceduresReglas.p PERSISTENT SET hLib.

RUN motorInferencia IN hLib ("..\industria\reglas.rul",
                             "11784").
*/
/*
DEFINE VARIABLE dKg4 AS DECIMAL    NO-UNDO.
RUN libTamboresIndustria.p PERSISTENT SET hLib.
dKg4 = DYNAMIC-FUNCTION('getKilos400Gpl' IN hLib, 9.21, 6.84, 20000, TRUE).
*/


/*
/*mailing*/
RUN libRemitos.p PERSISTENT SET hLib.
RUN mailingProcesamientoRemito IN hLib (95, 123, 12327).
*/


/*
RUN libImpresionEtiquetas.p PERSISTENT SET hLib.
RUN etqLabelPrueba IN hLib ("\\192.168.1.117\zebra105").
*/

/*
RUN libReportes.p PERSISTENT SET hLib.
DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iTime AS INTEGER    NO-UNDO.

iTime = ETIME(TRUE).

RUN callReporteEstadosTambores IN hLib (95).


MESSAGE "listo" SKIP STRING(ETIME / 1000) VIEW-AS ALERT-BOX INFO BUTTONS OK.

*/


/*
FIND LAST items_factura WHERE id_tipotambor = 3 AND id_sucursal = 95 NO-LOCK NO-ERROR.
FIND FIRST remitos OF items_factura NO-LOCK NO-ERROR.
RUN wqOrigenTambores.w (items_factura.id_sucursal, 
                        items_factura.id_tipotambor, 
                        ROWID(remitos), 
                        "remitos", 
                        OUTPUT cProgram).
*/


/*
find orden_entrega where id_orden_entrega = 11099 no-lock no-error.
RUN ..\industria\w_items_orden_entrega.w (input rowid(orden_entrega)).
*/
  



/*
FIND lotes_jugo WHERE nromov = 53154.

RUN zebra_inspecciones_lote.p (ROWID(lotes_jugo)).
*/



/*

CREATE SERVER hApp.
hApp:CONNECT("-AppService asindustria -H 192.168.1.4").

IF lFlag THEN 
  RUN libReportes.p PERSISTENT SET hLib ON SERVER hApp TRANSACTION DISTINCT.
ELSE 
  RUN libReportes.p PERSISTENT SET hLib .
  
  
                   

/*RUN callStockFecha IN hLib (95, DATE("05/04/2005"), DATE("06/04/2005")).*/
/*RUN callReporteStock IN hLib (96).*/
/*RUN callComposicionLoteJugo IN hLib (1, 96, 3, 56291).*/
  RUN callReporteReprocesos IN hLib (DATE("01/05/2005"), DATE("12/05/2005"), 95, 3).


  
hApp:DISCONNECT().
DELETE OBJECT hApp.

*/

/*RUN ..\industria\w_calculo_stock_nuevo_tambores_opt.p (96).*/




/*
      RUN  aderb\_prntrb2(
         "l:\industria\reports.prl", /* RB-REPORT-LIBRARY */
         "reporte_stock_rapido_citrix_ord",                    /* RB-REPORT-NAME */
         "",                             /* RB-DB-CONNECTION */
         "O",                             /* RB-INCLUDE-RECORDS */
         "",                              /* RB-FILTER */
         "",                              /* RB-MEMO-FILE */
         "D",                             /* RB-PRINT-DESTINATION */
         "?",                              /* RB-PRINTER-NAME */
         "",                              /* RB-PRINTER-PORT */
         "",                              /* RB-OUTPUT-FILE */
         1,                              /* RB-NUMBER-COPIES  - zero */                  
         0,                              /* RB-BEGIN-PAGE - zero */
         0,                              /* RB-END-PAGE - zero */
         no,                              /* RB-TEST-PATTERN */
         "Reporte de Stock",         /* RB-WINDOW-TITLE */
         yes,                           /* RB-DISPLAY-ERRORS */
         yes,                           /* RB-DISPLAY-STATUS */
         no,                              /* RB-NO-WAIT */
         "" /* RB-OTHER-PARAMETERS */,
         ""
         ).  
         
         
  */
  









/*
DEFINE VARIABLE viEmpresa    AS INTEGER    NO-UNDO.
DEFINE VARIABLE viSucursal   AS INTEGER    NO-UNDO.
DEFINE VARIABLE viTipoTambor AS INTEGER    NO-UNDO.
DEFINE VARIABLE viNroMov     AS INTEGER    NO-UNDO.
DEFINE VARIABLE viCantidad AS INTEGER    NO-UNDO.
DEFINE VARIABLE viLote AS INTEGER    NO-UNDO.
DEFINE VARIABLE viAnio AS INTEGER    NO-UNDO.

RUN wGetLoteCascaraRemito.w (92,
                                    OUTPUT viEmpresa, 
                                     OUTPUT viSucursal, 
                                     OUTPUT viTipoTambor, 
                                     OUTPUT viNroMov, 
                                     OUTPUT viCantidad, 
                                     OUTPUT viLote, 
                                     OUTPUT viAnio).

MESSAGE string(viEmpresa) + CHR(10) +
        string(viSucursal) + CHR(10) +
        string(viTipoTambor) + CHR(10) +
        string(viNroMov)  + CHR(10) +
        string(viCantidad) + CHR(10) +
        string(viLote) + CHR(10) +
        string(viAnio)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/
/*RUN w_new_orden_entrega.w.*/

/*
find orden_entrega where id_orden_entrega = 10166 no-lock no-error.
RUN ..\industria\w_items_orden_entrega.w (input rowid(orden_entrega)).
*/





QUIT.

