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
DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

RUN libLogistica.p PERSISTENT SET hLib.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hLib, SEARCH-SELF).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-buttonAction2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buttonAction2 Procedure 
PROCEDURE buttonAction2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcArgs AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE cRgo AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dDes AS DATE       NO-UNDO.
  DEFINE VARIABLE dHas AS DATE       NO-UNDO.
  DEFINE VARIABLE hRep AS HANDLE     NO-UNDO.
  
  DEFINE VARIABLE iSemDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSemHas AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAnio   AS INTEGER    NO-UNDO.

  DEFINE VARIABLE RB-MEMO-FILE AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hRep   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p'). 

  iSemDes = INTEGER(ENTRY(1, pcArgs)).
  iSemHas = INTEGER(ENTRY(2, pcArgs)).
  iAnio   = INTEGER(ENTRY(3, pcArgs)).



  cRgo   = DYNAMIC-FUNCTION('getFechasSemana' IN hLibCom, iSemDes, iAnio).
  dDes   = DATE(ENTRY(1, cRgo)).
  cRgo   = DYNAMIC-FUNCTION('getFechasSemana' IN hLibCom, iSemHas, iAnio).
  dHas   = DATE(ENTRY(2, cRgo)).
  

  RUN callSubdDespachos IN hRep (dDes, dHas).
  
  RUN  aderb\_prntrb2("..\industria\reports_9.prl", /* RB-REPORT-LIBRARY */
                        "rptSubdDespachos",                    /* RB-REPORT-NAME */
                        "",                             /* RB-DB-CONNECTION */
                        "O",                             /* RB-INCLUDE-RECORDS */
                        "",                              /* RB-FILTER */
                        RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                        "D",                             /* RB-PRINT-DESTINATION */
                        "?",                              /* RB-PRINTER-NAME */
                        "",                              /* RB-PRINTER-PORT */
                        "",                              /* RB-OUTPUT-FILE */
                        1,                              /* RB-NUMBER-COPIES  - zero */                  
                        0,                              /* RB-BEGIN-PAGE - zero */
                        0,                              /* RB-END-PAGE - zero */
                        no,                              /* RB-TEST-PATTERN */
                        "Tabla Despachos",         /* RB-WINDOW-TITLE */
                        yes,                           /* RB-DISPLAY-ERRORS */
                        yes,                           /* RB-DISPLAY-STATUS */
                        no,                              /* RB-NO-WAIT */
                        "" /* RB-OTHER-PARAMETERS */,
                        "").   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillTreeView) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTreeView Procedure 
PROCEDURE fillTreeView :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phTree AS COM-HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER pcArgs AS CHARACTER  NO-UNDO.

  RUN SUPER (phTree, pcArgs).

  DEFINE VARIABLE dDesde  AS DATE       NO-UNDO.
  DEFINE VARIABLE dHasta  AS DATE       NO-UNDO.
  DEFINE VARIABLE cRgo    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTxt    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIco    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCli    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE.
  DEFINE VARIABLE pchTree AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chOpen  AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chClose AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chOE    AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chNode  AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chRto   AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chLot   AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE i       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE piWeek  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAnio   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSemDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSemHas AS INTEGER    NO-UNDO.

  
  
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  iSemDes = INTEGER(ENTRY(1, pcArgs)).
  iSemHas = INTEGER(ENTRY(2, pcArgs)).
  iAnio   = INTEGER(ENTRY(3, pcArgs)).
  piWeek  = iSemDes.
  pchTree = phTree.

  cRgo   = DYNAMIC-FUNCTION('getFechasSemana' IN hLibCom, iSemDes, iAnio).
  dDesde = DATE(ENTRY(1, cRgo)).
  cRgo   = DYNAMIC-FUNCTION('getFechasSemana' IN hLibCom, iSemHas, iAnio).
  dHasta = DATE(ENTRY(2, cRgo)).

  i = 1.

  cTxt    = "Logistica para Semana "  + STRING(iSemDes) + " a Semana " + STRING(iSemHas) + " de " + STRING(iAnio).
  chNode  = pchTree:Nodes:ADD(, , "Despachos Semana" + STRING(piWeek), cTxt, "semana").  
  chOpen  = pchTree:Nodes:ADD(chNode:KEY, 4, "PendientesLogi", "Pendientes", "pendientes").
  chClose = pchTree:Nodes:ADD(chNode:KEY, 4, "DespachadasLogi", "Despachadas", "cerradas").

  FOR EACH orden_entrega
      WHERE orden_entrega.fecha_embarque  >= dDesde
        AND orden_entrega.fecha_embarque  <= dHasta
      BY orden_entrega.id_orden_entrega.

    FOR EACH items_orden_entrega 
            OF orden_entrega 
          NO-LOCK.

      FIND FIRST remitos 
         WHERE remitos.id_orden_entrega = orden_entrega.id_orden_entrega
           AND remitos.ITEM_oe          = items_orden_entrega.ITEM_oe
           AND remitos.id_operacion     = 311
         NO-LOCK NO-ERROR.

     


        FIND FIRST contratos OF items_orden_entrega NO-LOCK NO-ERROR.
        FIND FIRST clientes OF contratos NO-LOCK NO-ERROR.
        
        cCli   = IF AVAILABLE clientes THEN clientes.razon_social ELSE "NONE".
        cKey   = "orden" + items_orden_entrega.id_contrato + "-" + STRING(items_orden_entrega.ITEM) + "-" + STRING(items_orden_entrega.id_orden_entrega) + "-" + STRING(items_orden_entrega.ITEM_oe).
        cTxt   = STRING(items_orden_entrega.id_orden_entrega) + " - " + 
                 STRING(items_orden_entrega.ITEM_oe) + 
                 " cliente: " + cCli + 
                 " contrato: " + items_orden_entrega.id_contrato + " parte: " + STRING(items_orden_entrega.item) + 
                 " Por " + STRING(items_orden_entrega.cantidad_tambores) + " en semana " + STRING(items_orden_entrega.semana_entrega)  .
        
        IF AVAILABLE remitos THEN DO:   

          chOE = pchTree:Nodes:ADD(chClose:KEY, 4, cKey, cTxt, "oe").

          cTxt = "Remito: " + 
                 STRING(remitos.nro_comp, "9999-99999999") + " " +
                 "Fecha: " + 
                 string(remitos.fecha).

          cIco       = "camion".
          cKey       = "remito" + "-" + STRING(remitos.id_sucursal) + "-" + STRING(remitos.nro).
          chRto      = pchTree:Nodes:ADD(chOE:KEY, 4, cKey, cTxt, cIco).
          chRto:TAG  = STRING(remitos.nro).

          FOR EACH items_factura
                OF remitos
              NO-LOCK.
            FIND FIRST productos_terminados OF items_factura NO-LOCK NO-ERROR.
            FIND FIRST calidades OF items_factura NO-LOCK NO-ERROR.
            FIND FIRST envases_prod OF items_factura NO-LOCK NO-ERROR.

            cTxt = "Lote: " + items_factura.nro_lote + " " +
                   "Articulo " + productos_terminados.descripcion + " " + 
                   "Calidad " + calidades.descripcion + " " + 
                   "Cantidad: " + STRING(items_factura.cantidad) + " " +
                   "Kilos " + STRING(items_factura.kilos) .
            cIco = "lote".
            cKey = "Tambor-" + STRING(remitos.nro) + STRING(items_factura.ITEM).
            chLot = pchTree:Nodes:ADD(chRto:KEY, 4, cKey, cTxt, cIco).
            

                   

          END.
          
          
        END.
        ELSE
          chOE   = pchTree:Nodes:ADD(chOpen:KEY, 4, cKey, cTxt, "oe").
 
      END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillTreeViewOld) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTreeViewOld Procedure 
PROCEDURE fillTreeViewOld :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phTree AS COM-HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER pcArgs AS CHARACTER  NO-UNDO.

  RUN SUPER (phTree, pcArgs).

  DEFINE VARIABLE dDesde  AS DATE       NO-UNDO.
  DEFINE VARIABLE dHasta  AS DATE       NO-UNDO.
  DEFINE VARIABLE cRgo    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKey    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTxt    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIco    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCli    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRoo AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLibCom AS HANDLE.
  DEFINE VARIABLE pchTree AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chOpen  AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chClose AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chOE    AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chNode  AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chRto   AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE chLot   AS COM-HANDLE     NO-UNDO.
  DEFINE VARIABLE i       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE piWeek  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAnio   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSemDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSemHas AS INTEGER    NO-UNDO.

  
  
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  iSemDes = INTEGER(ENTRY(1, pcArgs)).
  iSemHas = INTEGER(ENTRY(2, pcArgs)).
  iAnio   = INTEGER(ENTRY(3, pcArgs)).
  piWeek  = iSemDes.
  pchTree = phTree.

  pchTree:Nodes:CLEAR().

  cRgo   = DYNAMIC-FUNCTION('getFechasSemana' IN hLibCom, iSemDes, iAnio).
  dDesde = DATE(ENTRY(1, cRgo)).
  cRgo   = DYNAMIC-FUNCTION('getFechasSemana' IN hLibCom, iSemHas, iAnio).
  dHasta = DATE(ENTRY(2, cRgo)).

  i = 1.

  cTxt    = "Logistica de Despachos para Semana "  + STRING(iSemDes) + " a Semana " + STRING(iSemHas) + " de " + STRING(iAnio).
  chNode  = pchTree:Nodes:ADD(, , "Despachos Semana" + STRING(piWeek), cTxt, "semana").  
  chNode:TAG = pcArgs.
  chOpen  = pchTree:Nodes:ADD(chNode:KEY, 4, "PendientesLogi", "Pendientes", "pendientes").
  chClose = pchTree:Nodes:ADD(chNode:KEY, 4, "DespachadasLogi", "Despachadas", "cerradas").

  FOR EACH subd_despachos_industria
      WHERE subd_despachos_industria.fecha_despacho >= dDesde
        AND subd_despachos_industria.fecha_despacho <= dHasta
      BY subd_despachos_industria.id_orden_entrega.

    /* finds */
    FIND FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = subd_despachos_industria.id_orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = subd_despachos_industria.id_orden_entrega AND items_orden_entrega.ITEM_oe = subd_despachos_industria.ITEM_oe NO-LOCK NO-ERROR.
    FIND FIRST items_contratos OF items_orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST contratos OF items_contratos NO-LOCK NO-ERROR.
    FIND FIRST clientes OF contratos NO-LOCK NO-ERROR.
    FIND FIRST tambores_industria OF items_orden_entrega NO-LOCK NO-ERROR.
    FIND FIRST productos_terminados OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
    FIND FIRST remitos WHERE remitos.id_sucursal = subd_despachos_industria.id_sucursal_remito AND remitos.id_tipo_movsto = subd_despachos_industria.id_tipo_movsto AND remitos.nro = subd_despachos_industria.nro_remito  NO-LOCK NO-ERROR.

    cCli = IF AVAILABLE clientes THEN clientes.razon_social ELSE "NO INFO".

    cIco = "camion".
    cTxt = STRING(subd_despachos_industria.id_orden_entrega) + " parte " + 
           STRING(subd_despachos_industria.ITEM_oe) + " Cliente: " + 
           cCli + " Fecha Despacho: " + 
           STRING(subd_despachos_industria.fecha_despacho).
    cKey = "subd" + STRING(subd_despachos_industria.id_orden_entrega) + "-" + STRING(subd_despachos_industria.ITEM_oe).

    IF subd_despachos_industria.nro_remito = 0 THEN DO:
      cRoo = chOpen:KEY.
      cIco = "not".
    END.
    ELSE DO: 
      cRoo = chClose:KEY. 
      cIco = "yes".
    END.

    chOE = pchTree:Nodes:ADD(cRoo, 4, cKey, cTxt, cIco).
    cTxt = "Lote " + STRING(tambores_industria.id_lote) + "/" + STRING(tambores_industria.anio) + " " + 
           productos_terminados.descripcion + " " + 
           calidades.abreviatura + " " + 
           envases_prod.abreviatura.
    cKey = "lote" + STRING(tambores_industria.nromov) + "-" + STRING(tambores_industria.id_etiqueta).    
    chNode = pchTree:Nodes:ADD(chOE:KEY, 4, cKey, cTxt, "lote").
 
    IF AVAILABLE remitos THEN DO:
      cKey = "remi" + STRING(remitos.id_sucursal) + "-" + STRING(remitos.nro) + "-" + STRING(remitos.id_operacion) + "-" + remitos.nro_comp + "-" + STRING(tambores_industria.nromov).
      cTxt = "Remito " + STRING(remitos.nro_comp, "9999-99999999") + " Fecha: " + STRING(remitos.fecha).
      chNode = pchTree:Nodes:ADD(chOE:KEY, 4, cKey, cTxt, "data").
    END.
    ELSE
      ASSIGN cKey = "".




    
  END.

  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

