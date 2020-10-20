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

  
  
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.

  piWeek  = INTEGER(pcArgs).
  pchTree = phTree.


  pchTree:NODES:CLEAR().

  cRgo   = DYNAMIC-FUNCTION('getFechasSemana' IN hLibCom, piWeek, YEAR(TODAY)).
  dDesde = DATE(ENTRY(1, cRgo)).
  dHasta = DATE(ENTRY(2, cRgo)).

  i = 1.

  
  chNode  = pchTree:Nodes:ADD(, , "Semana" + STRING(piWeek), "Semana "  + STRING(piWeek), "semana").  
  chOpen  = pchTree:Nodes:ADD(chNode:KEY, 4, "Pendientes", "Pendientes", "pendientes").
  chClose = pchTree:Nodes:ADD(chNode:KEY, 4, "Despachadas", "Despachadas", "cerradas").

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

/*

FOR EACH documentos_oe
            WHERE documentos_oe.id_orden_entrega = items_orden_entrega.id_orden_entrega
            NO-LOCK.
  
          cIco = "no".
          IF documentos_oe.fecha_pedido <> ? AND documentos_oe.fecha_cumplido <> ? THEN cIco = "si".
          IF documentos_oe.fecha_pedido = ? AND documentos_oe.fecha_cumplido = ? THEN cIco = "no".
          IF documentos_oe.fecha_pedido <> ? AND documentos_oe.fecha_cumplido = ? THEN cIco = "pedido".
          IF documentos_oe.fecha_pedido = ? AND documentos_oe.fecha_cumplido <> ? THEN cIco = "no".
          
          cTxt   = documentos_oe.descripcion.
          IF documentos_oe.fecha_pedido <> ? THEN cTxt = cTxt + " pedido el: " + STRING(documentos_oe.fecha_pedido).
          IF documentos_oe.fecha_cumplido <> ? THEN cTxt = cTxt + " cumplido el: " + STRING(documentos_oe.fecha_cumplido).
          
          cKey   = "documento" + STRING(documentos_oe.id_orden_entrega) + STRING(items_orden_entrega.ITEM_oe) + STRING(documentos_oe.id_documento_oe).
  
          chNode     = pchTree:Nodes:ADD(chOE:KEY, 4, cKey, cTxt, cIco).
          chNode:TAG = STRING(documentos_oe.id_documento_oe).
  
        END. 
 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

