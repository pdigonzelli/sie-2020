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


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getCantidadTambores) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantidadTambores Procedure 
FUNCTION getCantidadTambores RETURNS INTEGER
  (piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getControlCalidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getControlCalidad Procedure 
FUNCTION getControlCalidad RETURNS LOGICAL
  (piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDatosOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDatosOE Procedure 
FUNCTION getDatosOE RETURNS CHARACTER
  (piOE    AS INTEGER, 
   piParte AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDatosOERemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDatosOERemito Procedure 
FUNCTION getDatosOERemito RETURNS CHARACTER
  (prRemito AS ROWID)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosItemRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilosItemRemito Procedure 
FUNCTION getKilosItemRemito RETURNS CHARACTER
  (piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER, 
   piPte AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLotesRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLotesRemito Procedure 
FUNCTION getLotesRemito RETURNS CHARACTER
  (piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNextItemRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextItemRemito Procedure 
FUNCTION getNextItemRemito RETURNS INTEGER
  (piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNextNroRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextNroRemito Procedure 
FUNCTION getNextNroRemito RETURNS CHARACTER
  (piSuc AS INTEGER, 
   piTip AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPesoRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPesoRemito Procedure 
FUNCTION getPesoRemito RETURNS DECIMAL
  (piSuc AS INTEGER,
   piTip AS INTEGER, 
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSucursalDestino) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSucursalDestino Procedure 
FUNCTION getSucursalDestino RETURNS INTEGER
  (piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSucursalTransito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSucursalTransito Procedure 
FUNCTION getSucursalTransito RETURNS INTEGER
  (piSucOri AS INTEGER,
   piSucDes AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
         HEIGHT             = 14.86
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-addItemRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addItemRemito Procedure 
PROCEDURE addItemRemito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc       AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip       AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro       AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcStreamTbs AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE iPte AS INTEGER    NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLot AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRow AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.
  
  DO TRANSACTION ON ERROR UNDO, LEAVE:

    DO i = 1 TO NUM-ENTRIES(pcStreamTbs, CHR(10)):
      cRow = ENTRY(i, pcStreamTbs, CHR(10)).
  
      iLot = IF INTEGER(ENTRY(12, cRow, CHR(1))) <> 0 THEN INTEGER(ENTRY(12, cRow, CHR(1))) ELSE INTEGER(ENTRY(5, cRow,CHR(1))).
      cLot = STRING(iLot, "9999") + "/" + SUBSTRING(STRING(INTEGER(ENTRY(13, cRow, CHR(1)))), 3, 2).
      iPte = getNextItemRemito(piSuc, piTip, piNro).
      
      CREATE items_factura.
      ASSIGN items_factura.id_sucursal       = piSuc
             items_factura.id_tipo_movsto    = piTip
             items_factura.nro               = piNro
             items_factura.ITEM              = iPte
             items_factura.id_articulo       = INTEGER(ENTRY(7, cRow,CHR(1)))
             items_factura.id_calidad        = INTEGER(ENTRY(8, cRow,CHR(1)))
             items_factura.id_envase         = INTEGER(ENTRY(9, cRow,CHR(1)))
             items_factura.id_tipotambor     = INTEGER(ENTRY(3, cRow,CHR(1)))
             items_factura.desde_lote        = INTEGER(ENTRY(5, cRow,CHR(1)))
             items_factura.hasta_lote        = INTEGER(ENTRY(6, cRow,CHR(1)))
             items_factura.cantidad          = items_factura.hasta_lote - items_factura.desde_lote + 1
             items_factura.kilos             = ROUND(DECIMAL(ENTRY(10, cRow, CHR(1))), 2)
             items_factura.peso              = items_factura.kilos / items_factura.cantidad
             items_factura.nro_lote          = cLot
             items_factura.c_usuario         = USERID('userdb')
             items_factura.c_fecha           = TODAY
             items_factura.c_hora            = STRING(TIME, 'HH:MM:SS')
             .
      
      /*relacion tambores con item_factura*/
      RUN setRelTamboresRemito (INTEGER(ENTRY(1, cRow, CHR(1))),
                                INTEGER(ENTRY(2, cRow, CHR(1))), 
                                INTEGER(ENTRY(3, cRow, CHR(1))), 
                                INTEGER(ENTRY(4, cRow, CHR(1))), 
                                INTEGER(ENTRY(5, cRow, CHR(1))), 
                                INTEGER(ENTRY(6, cRow, CHR(1))),
                                piSuc,
                                piTip,
                                piNro,
                                iPte).
  
  
    END. /*do i = 1 to num-entries ...*/
  
    /*RUN recalcKilosRemito (piSuc, piTip, piNro).*/

  END. /*do transaction*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addItemRemitoCascara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addItemRemitoCascara Procedure 
PROCEDURE addItemRemitoCascara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucRto AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipRto AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroRto AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piEmpLot AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucLot AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipLot AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroLot AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piCanLot AS INTEGER    NO-UNDO.

  DEFINE VARIABLE iPte AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.

  iPte = getNextItemRemito(piSucRto, piTipRto, piNroRto).  

  DO TRANSACTION ON ERROR UNDO, LEAVE:
    FOR FIRST lotes_cascara
        WHERE lotes_cascara.id_empresa    = piEmpLot
          AND lotes_cascara.id_sucursal   = piSucLot
          AND lotes_cascara.id_tipotambor = piTipLot
          AND lotes_cascara.nromov        = piNroLot
        NO-LOCK.
      
      cLot = STRING(lotes_cascara.id_lote,"9999") + "/" + STRING(SUBSTRING(STRING(lotes_cascara.anio),3,2),"99").

      CREATE items_factura.
      ASSIGN items_factura.id_sucursal       = piSucRto
             items_factura.id_tipo_movsto    = piTipRto
             items_factura.nro               = piNroRto
             items_factura.ITEM              = iPte
             items_factura.id_articulo       = lotes_cascara.id_articulo
             items_factura.id_calidad        = lotes_cascara.id_calidad
             items_factura.id_envase         = lotes_cascara.id_envase
             items_factura.id_tipotambor     = lotes_cascara.id_tipotambor
             items_factura.desde_lote        = 1
             items_factura.hasta_lote        = piCanLot
             items_factura.cantidad          = items_factura.hasta_lote - items_factura.desde_lote + 1
             items_factura.kilos             = (50 * items_factura.cantidad)
             items_factura.peso              = 50
             items_factura.nro_lote          = cLot
             items_factura.c_usuario         = USERID('userdb')
             items_factura.c_fecha           = TODAY
             items_factura.c_hora            = STRING(TIME, 'HH:MM:SS')
             .

      RUN setRelLoteCascaraRemito (piSucRto, piTipRto, piNroRto, iPte,
                                   piEmpLot, piSucLot, piTipLot, piNroLot).


    END.


    RUN recalcKilosRemito (piSucRto, piTipRto, piNroRto).

    

  END. /*do transaction*/


  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-anulacionRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anulacionRemito Procedure 
PROCEDURE anulacionRemito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER prRemito AS ROWID      NO-UNDO.


  DEFINE VARIABLE r_remito AS ROWID      NO-UNDO.

  r_remito = prRemito.

  FIND FIRST remitos WHERE ROWID(remitos) = r_remito NO-ERROR.
  IF AVAILABLE remitos THEN DO:
    FOR EACH items_factura OF remitos.
      IF items_factura.id_articulo <> 54 THEN
        RUN anularRemitoTambores (prRemito).
      ELSE
        RUN anularRemitoCascara (prRemito).
    END.
  END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-anularRemitoCascara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anularRemitoCascara Procedure 
PROCEDURE anularRemitoCascara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER prRemito AS ROWID      NO-UNDO.


  DEFINE VARIABLE hProg    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE r_remito AS ROWID      NO-UNDO.

  RUN libLotesUbicacion.p PERSISTENT SET hProg.
  
  DO TRANSACTION ON ERROR UNDO , LEAVE:
    r_remito = prRemito.
    FIND FIRST remitos WHERE ROWID(remitos) = r_remito NO-ERROR.
    IF AVAILABLE remitos THEN DO:
      FOR EACH items_factura OF remitos.
        /*CASCARA*/
        DEFINE VARIABLE vlAnul AS LOGICAL    NO-UNDO.
        FIND FIRST r_lote_cascara_remito WHERE r_lote_cascara_remito.nro_remito         = ITEMs_factura.nro
                                           AND r_lote_cascara_remito.id_sucursal_remito = ITEMs_factura.id_sucursal
                                           AND r_lote_cascara_remito.id_tipo_movsto     = ITEMs_factura.id_tipo_movsto
                                           AND r_lote_cascara_remito.ITEM_factura       = ITEMs_factura.ITEM
                                         NO-LOCK NO-ERROR.
        IF AVAILABLE r_lote_cascara_remito THEN DO:
          vlAnul = DYNAMIC-FUNCTION('anulacionRemitoCascara' IN hProg, r_lote_cascara_remito.nro_remito,
                                                                       r_lote_cascara_remito.id_sucursal_remito,
                                                                       r_lote_cascara_remito.id_tipo_movsto, 
                                                                       r_lote_cascara_remito.ITEM_factura, 
                                                                       r_lote_cascara_remito.id_empresa, 
                                                                       r_lote_cascara_remito.id_sucursal,
                                                                       r_lote_cascara_remito.id_tipotambor, 
                                                                       r_lote_cascara_remito.nromov).
          IF NOT vlAnul THEN 
            RETURN "error en anulacion de remito de cascara.".
        END.
        /*fin CASCARA*/
      END.
      /*aqui va la actualizacion al remito cambi a estado anulado*/
      FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = remitos.id_orden_entrega 
                                       AND items_orden_entrega.item_oe          = remitos.item_oe
                                     NO-ERROR.
      IF AVAILABLE items_orden_entrega THEN 
        ASSIGN items_orden_entrega.id_estado = 1.
            
      ASSIGN remitos.estado    = FALSE
             remitos.c_fecha   = TODAY
             remitos.c_usuario = USERID("userdb")
             remitos.c_hora    = STRING(TIME,"hh:mm:ss").
      MESSAGE "Se a anulado satisfactoriamente el remito " remitos.nro_comp VIEW-AS ALERT-BOX.
      
    END.
  END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-anularRemitoTambores) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anularRemitoTambores Procedure 
PROCEDURE anularRemitoTambores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER prRemito AS ROWID      NO-UNDO.

  DEFINE VARIABLE hProg AS HANDLE     NO-UNDO.

  DEFINE VAR r_remito AS ROWID.
  DEFINE VAR v_nro_comp AS INTEGER.
  DEFINE VAR v_punto_venta AS INTEGER.
  DEFINE VAR v_nro_comprobante AS CHAR.
  DEFINE VAR v_sucursal_ubicacion AS INTEGER.
  DEFINE VAR v_nrocopia AS INTEGER.
  DEFINE VAR wpanta AS LOGICAL.
  DEFINE VAR v_respuesta AS LOGICAL.
  DEFINE VAR v_tipo_remito AS LOGICAL.
  DEFINE VAR v_suc_destino AS INTEGER.
  
  
  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hProg = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libLotesUbicacion.p').
  DELETE OBJECT hLibCom.
  
  r_remito = prRemito.
  
  DO TRANSACTION ON ERROR UNDO , LEAVE:
  FIND FIRST remitos WHERE ROWID(remitos) = r_remito NO-ERROR.
  IF AVAILABLE remitos THEN DO:
      IF remitos.estado <> FALSE THEN DO:
        IF (remitos.id_tipocomp_compra <> 0 OR
            remitos.id_operacion_compra <> 0 OR
            remitos.nromov_compra <> 0) THEN DO:
            MESSAGE "NO SE PUEDE ANULAR, POR ESTAR YA FACTURADO O CONTROLADO.. !!!" VIEW-AS ALERT-BOX.
            RETURN "ADM-ERROR".
        END.
        ELSE DO:

            v_sucursal_ubicacion = remitos.id_sucursal.
            
            FOR EACH items_factura OF remitos.
                FIND FIRST tipostambor OF items_factura NO-LOCK NO-ERROR.
                IF tipostambor.tabla <> "tambores_industria" THEN DO:
                    FIND FIRST tambores_industria WHERE tambores_industria.id_sucursal_remito = items_factura.id_sucursal
                                                    AND tambores_industria.id_tipo_movsto     = items_factura.id_tipo_movsto
                                                    AND tambores_industria.nro_remito         = items_factura.nro
                                                    AND tambores_industria.ITEM_factura       = items_factura.ITEM
                                                    NO-LOCK NO-ERROR.
                    IF AVAILABLE tambores_industria THEN DO:
                        v_suc_destino = tambores_industria.id_sucursal_ubicacion.
                    END.
                    ELSE DO:
                        v_suc_destino = 85.
                    END.

                    RUN despachoRemitos IN hProg (INPUT tambores_industria.id_empresa,
                                                  INPUT tambores_industria.id_sucursal,
                                                  INPUT tambores_industria.id_tipotambor,
                                                  INPUT tambores_industria.nromov,
                                                  INPUT v_suc_destino,         /* SUC ORIGEN */
                                                  INPUT v_sucursal_ubicacion,  /* SUC DESTINO */
                                                  INPUT items_factura.fecha,
                                                  INPUT items_factura.cantidad,
                                                  INPUT items_factura.desde_lote,
                                                  INPUT items_factura.hasta_lote,
                                                  INPUT 4
                                                  ).
                    
                    /*
                    RUN ../industria/y_gstkrem_actualizado.p (INPUT remitos.id_sucursal,
                                                              INPUT remitos.id_tipo_movsto,
                                                              INPUT remitos.nro,
                                                              INPUT items_factura.item,
                                                              INPUT v_sucursal_ubicacion, /* SUC DESTINO */
                                                              INPUT v_suc_destino, /* SUC ORIGEN */
                                                              INPUT 4) "items_factura". */
                    IF RETURN-VALUE <> "" THEN DO:
                        MESSAGE "Error en el Procesamiento de Remitos" VIEW-AS ALERT-BOX.
                        RETURN "ADM-ERROR".
                    END.
                    FOR EACH tambores_industria WHERE tambores_industria.id_sucursal_remito = items_factura.id_sucursal
                                                  AND tambores_industria.id_tipo_movsto     = items_factura.id_tipo_movsto
                                                  AND tambores_industria.nro_remito         = items_factura.nro
                                                  AND tambores_industria.ITEM_factura       = items_factura.ITEM
                                                  .
                        ASSIGN tambores_industria.id_sucursal_ubicacion = v_sucursal_ubicacion
                               tambores_industria.id_sucursal_remito    = 0
                               tambores_industria.id_tipo_movsto        = 0
                               tambores_industria.nro_remito            = 0
                               tambores_industria.ITEM_factura          = 0.
                    END.
                END.
                ELSE DO:
                    FOR EACH tambores_industria WHERE tambores_industria.id_sucursal_remito = items_factura.id_sucursal
                                                  AND tambores_industria.id_tipo_movsto     = items_factura.id_tipo_movsto
                                                  AND tambores_industria.nro_remito         = items_factura.nro
                                                  AND tambores_industria.ITEM_factura       = items_factura.ITEM
                                                  .
                        IF tipostambor.tabla = "tambores_industria" THEN DO:
                            run ../industria/y_gstkrprod.p (input tambores_industria.id_empresa,
                                                            input tambores_industria.id_sucursal,
                                                            input tambores_industria.id_tipotambor,
                                                            input tambores_industria.nromov,
                                                            input tambores_industria.id_tambor,
                                                            input tambores_industria.id_tambor,
                                                            INPUT remitos.id_sucursal,
                                                            input tambores_industria.id_sucursal_ubicacion,
                                                            input 4).
        
                            IF RETURN-VALUE <> "" THEN DO:
                                MESSAGE "Error en el Procesamiento de Remitos" VIEW-AS ALERT-BOX.
                                /*RETURN "ADM-ERROR".*/
                            END.
                        END.
                         ASSIGN tambores_industria.id_sucursal_ubicacion = v_sucursal_ubicacion
                                tambores_industria.id_sucursal_remito    = 0
                                tambores_industria.id_tipo_movsto        = 0
                                tambores_industria.nro_remito            = 0
                                tambores_industria.ITEM_factura          = 0.
                    END.
                END.
            END.
    
            FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = remitos.id_orden_entrega 
                                             AND items_orden_entrega.item_oe          = remitos.item_oe
                                            NO-ERROR.
            IF AVAILABLE items_orden_entrega THEN 
                ASSIGN items_orden_entrega.id_estado = 1.
            
           ASSIGN remitos.estado    = FALSE
                  remitos.c_fecha   = TODAY
                  remitos.c_usuario = USERID("userdb")
                  remitos.c_hora    = STRING(TIME,"hh:mm:ss").
           MESSAGE "Se a anulado satisfactoriamente el remito " remitos.nro_comp VIEW-AS ALERT-BOX.
           
        END.
      END. /*if remitos.estado <> false ...*/
      ELSE DO:
          MESSAGE "No se puede anular el remito " remitos.nro_comp " porque ya fue anulado por "
                   remitos.c_usuario " el dia " STRING(remitos.c_fecha) VIEW-AS ALERT-BOX.
      END.
  END. /*if availble remitos ...*/
 END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-confirmacionArribo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE confirmacionArribo Procedure 
PROCEDURE confirmacionArribo :
/*------------------------------------------------------------------------------
  Purpose:     transfiere los productos del remito de la sucursal en transito a la sucursal destino
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  DEFINE VARIABLE iSucOri AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSucDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSucTra AS INTEGER    NO-UNDO.


  ASSIGN iSucOri = piSuc
         iSucDes = getSucursalDestino(piSuc, piTip, piNro)
         iSucTra = getSucursalTransito(iSucOri, iSucDes)
         iSucDes = 85 /*esto lo hago fijo, porque trae problemas para hacer el pl */
         .

  
  FOR FIRST remitos 
      WHERE remitos.id_sucursal    = piSuc
        AND remitos.id_tipo_movsto = piTip
        AND remitos.nro            = piNro.
/* pedido por maxi y saravia para que esto no trabe el packing list*/
    FOR EACH r_tambor_remito 
          WHERE r_tambor_remito.id_sucursal_remito  = remitos.id_sucursal
            AND r_tambor_remito.id_tipo_movsto      = remitos.id_tipo_movsto
            AND r_tambor_remito.nro_remito          = remitos.nro.

      RUN transferirTambor(r_tambor_remito.id_empresa,
                           r_tambor_remito.id_sucursal,
                           r_tambor_remito.id_tipotambor,
                           r_tambor_remito.nromov,
                           r_tambor_remito.id_tambor,
                           iSucDes).

    END. /*each r_tambor_remito*/

    /*marco fecha llegada en documento oe remito*/
    IF remitos.mercado <> 0 THEN DO:
      FOR FIRST documentos_oe
          WHERE documentos_oe.id_orden_entrega  = remitos.id_orden_entrega
            AND documentos_oe.id_tipo_documento = 5.  
        ASSIGN documentos_oe.fecha_cumplido = TODAY.        
      END.
    END.

  END. /*for first remitos*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-consolidarRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE consolidarRemito Procedure 
PROCEDURE consolidarRemito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  FOR FIRST remitos
      WHERE remitos.id_sucursal    = piSuc
        AND remitos.id_tipo_movsto = piTip
        AND remitos.nro            = piNro.
    IF remitos.mercado = 1 THEN DO:
      FOR EACH r_tambor_remito 
          WHERE r_tambor_remito.id_sucursal    = remitos.id_sucursal
            AND r_tambor_remito.id_tipo_movsto = remitos.id_tipo_movsto
            AND r_tambor_remito.nro_remito     = remitos.nro.
        
        RUN transferirTambor(r_tambor_remito.id_empresa,
                             r_tambor_remito.id_sucursal,
                             r_tambor_remito.id_tipotambor,
                             r_tambor_remito.nromov,
                             r_tambor_remito.id_tambor,
                             511). /*exolgan*/

      END. /*feach r_tambor_remito*/
    END. /*then if remitos.mercado = 1*/
  END. /*for first remitos*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-esperaRefrigeradaEnPuerto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE esperaRefrigeradaEnPuerto Procedure 
PROCEDURE esperaRefrigeradaEnPuerto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  FOR FIRST remitos
      WHERE remitos.id_sucursal    = piSuc
        AND remitos.id_tipo_movsto = piTip
        AND remitos.nro            = piNro.
    IF remitos.mercado = 1 THEN DO:
      FOR EACH r_tambor_remito 
          WHERE r_tambor_remito.id_sucursal    = remitos.id_sucursal
            AND r_tambor_remito.id_tipo_movsto = remitos.id_tipo_movsto
            AND r_tambor_remito.nro_remito     = remitos.nro.
        
        RUN transferirTambor(r_tambor_remito.id_empresa,
                             r_tambor_remito.id_sucursal,
                             r_tambor_remito.id_tipotambor,
                             r_tambor_remito.nromov,
                             r_tambor_remito.id_tambor,
                             512). /*espera refrigerada en puerto*/

      END. /*feach r_tambor_remito*/
    END. /*then if remitos.mercado = 1*/
  END. /*for first remitos*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillReportTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillReportTable Procedure 
PROCEDURE fillReportTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.


  DEFINE VARIABLE cCuitSami   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIngBrutos  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cNroCai     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCodAfip    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cIvaSami    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dFecInic    AS DATE       NO-UNDO.
  DEFINE VARIABLE iLote       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAnio       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLoteCte    AS INTEGER    NO-UNDO.


  ASSIGN cCuitSami  = "30-51119023-8"
         cIngBrutos = "924-830036-6"
         dFecInic   = DATE("01/12/1997")
         cCodAfip   = "091"
         cIvaSami   = "I.V.A. Responsable Inscripto"
         .

  /*limpio tabla rpt*/

  FOR EACH rptRemito.
    DELETE rptRemito.
  END.


  FOR FIRST remitos
      WHERE remitos.id_sucursal    = piSuc
        AND remitos.id_tipo_movsto = piTip
        AND remitos.nro            = piNro
      NO-LOCK.

    /*joins*/
    FIND FIRST clientes       OF remitos NO-LOCK NO-ERROR.
    FIND FIRST proveedores    OF remitos NO-LOCK NO-ERROR.
    FIND FIRST lugar_descarga OF remitos NO-LOCK NO-ERROR.
    FIND FIRST destinos       OF remitos NO-LOCK NO-ERROR.
    FIND FIRST vapores        OF remitos NO-LOCK NO-ERROR.
    FIND FIRST cia_seguro     OF remitos NO-LOCK NO-ERROR.
    FIND FIRST sucursales     OF remitos NO-LOCK NO-ERROR.

    FIND FIRST tipo_iva_contribuyente 
         WHERE tipo_iva_contribuyente.id_iva_contribuyente = clientes.id_iva_contribuyente
         NO-LOCK NO-ERROR.

    FIND FIRST tipo_numero 
         WHERE tipo_numero.id_sucursal    = piSuc
           AND tipo_numero.id_tipo_movsto = piTip
         NO-LOCK NO-ERROR.


    /*items*/
    FOR EACH items_factura 
          OF remitos
        NO-LOCK.

      /*joins*/
      FIND FIRST productos_terminados OF items_factura NO-LOCK NO-ERROR.
      FIND FIRST calidades            OF items_factura NO-LOCK NO-ERROR.
      FIND FIRST envases_prod         OF items_factura NO-LOCK NO-ERROR.

      /*lotes tambores*/
      FOR FIRST r_tambor_remito
          WHERE r_tambor_remito.id_sucursal_remito = items_factura.id_sucursal
            AND r_tambor_remito.id_tipo_movsto     = items_factura.id_tipo_movsto
            AND r_tambor_remito.nro_remito         = items_factura.nro
            AND r_tambor_remito.ITEM_factura       = items_factura.ITEM
          NO-LOCK.

        FOR FIRST tambores_industria 
            WHERE tambores_industria.nromov = r_tambor_remito.nromov
            NO-LOCK.

          ASSIGN iLote    = tambores_industria.id_lote
                 iAnio    = tambores_industria.anio
                 iLoteCte = 0
                 .
        END.
      END.

      /*lotes cascara*/
      FOR FIRST r_lote_cascara_remito
          WHERE r_lote_cascara_remito.id_sucursal_remito = items_factura.id_sucursal
            AND r_lote_cascara_remito.id_tipo_movsto     = items_factura.id_tipo_movsto
            AND r_lote_cascara_remito.nro_remito         = items_factura.nro
            AND r_lote_cascara_remito.ITEM_factura       = items_factura.ITEM
          NO-LOCK.

        FOR FIRST lotes_cascara
            WHERE lotes_cascara.nromov = r_lote_cascara_remito.nromov
            NO-LOCK.

          ASSIGN iLote    = lotes_cascara.id_lote
                 iAnio    = lotes_cascara.anio
                 iLoteCte = lotes_cascara.id_lote_cliente
                 .
        END.
      END.



      /*create*/

      CREATE rptRemito.
      ASSIGN rptRemito.id_sucursal_remito     = remitos.id_sucursal
             rptRemito.id_tipo_movsto         = remitos.id_tipo_movsto
             rptRemito.nro_remito             = remitos.nro
             rptRemito.nro_comp               = remitos.nro_comp
             rptRemito.fecha_remito           = remitos.fecha
             rptRemito.cuit_sami              = cCuitSami
             rptRemito.ingresos_brutos        = cIngBrutos
             rptRemito.fecha_inicio_activ     = IF AVAILABLE sucursales THEN sucursales.fecha_inicio_act ELSE DATE("")
             rptRemito.codigo_afip            = cCodAfip
             rptRemito.nro_copia              = STRING(remitos.impresion) /*poner leyenda*/
             rptRemito.condicion_iva_sami     = cIvaSami
             rptRemito.cliente                = IF AVAILABLE clientes THEN clientes.razon_social ELSE "NONE"
             rptRemito.domicilio_cliente      = IF AVAILABLE clientes THEN clientes.domicilio ELSE "NONE"
             rptRemito.cuit_cliente           = IF AVAILABLE clientes THEN clientes.cuit ELSE "NONE"
             rptRemito.id_cliente             = IF AVAILABLE clientes THEN clientes.id_cliente ELSE 0
             rptRemito.condicion_iva_cliente  = IF AVAILABLE tipo_iva_contribuyente THEN tipo_iva_contribuyente.descripcion ELSE "NONE"
             rptRemito.orden_carga            = STRING(remitos.nro_ord_carga)
             rptRemito.transporte             = IF AVAILABLE proveedores THEN proveedores.razon_social ELSE "NONE"
             rptRemito.cuit_transporte        = IF AVAILABLE proveedores THEN proveedores.cuit ELSE "NONE"
             rptRemito.nombre_chofer          = remitos.chofer
             rptRemito.patente_chasis         = remitos.pat_chasis
             rptRemito.patente_acoplado       = remitos.pat_acopla
             rptRemito.cia_seguro             = IF AVAILABLE cia_seguro THEN cia_seguro.descripcion ELSE "NONE"
             rptRemito.valor_declarado        = remitos.valor_declarado
             rptRemito.peso_neto              = remitos.peso_neto
             rptRemito.peso_bruto             = remitos.peso_bruto
             rptRemito.tara                   = remitos.tara
             rptRemito.lugar_descarga         = IF AVAILABLE lugar_descarga THEN lugar_descarga.descripcion ELSE "NONE"
             rptRemito.destino                = IF AVAILABLE destinos THEN destinos.descripcion ELSE "NONE"
             rptRemito.vapor                  = IF AVAILABLE vapores THEN vapores.descripcion ELSE "NONE"
             rptRemito.anio_produccion        = remitos.anio_produccion
             rptRemito.permiso_embarque       = remitos.nro_per_embarque
             rptRemito.id_orden_entrega       = remitos.id_orden_entrega
             rptRemito.contenedor             = remitos.nro_contenedor
             rptRemito.orden_compra_cliente   = ""
             rptRemito.orden_fabricacion      = remitos.nro_orden_fab
             rptRemito.iascav                 = remitos.nro_iascav
             rptRemito.nro_release            = ""
             rptRemito.ITEM                   = items_factura.ITEM
             rptRemito.articulo               = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "NONE"
             rptRemito.calidad                = IF AVAILABLE calidades THEN calidades.abreviatura ELSE "NONE"
             rptRemito.envase                 = IF AVAILABLE envases_prod THEN envases_prod.abreviatura ELSE "NONE"
             rptRemito.id_lote                = iLote
             rptRemito.anio                   = iAnio
             rptRemito.lote_cliente           = STRING(iLoteCte)
             rptRemito.desde                  = items_factura.desde_lote
             rptRemito.hasta                  = items_factura.hasta_lote
             rptRemito.cantidad               = items_factura.cantidad
             rptRemito.kilos                  = items_factura.peso
             rptRemito.kilos_total            = items_factura.peso * items_factura.cantidad
             rptRemito.nro_cai                = IF AVAILABLE tipo_numero THEN tipo_numero.nro_cai ELSE "NONE"
             rptRemito.fecha_vto_cai          = IF AVAILABLE tipo_numero THEN tipo_numero.fecha_vencimiento ELSE DATE("")
             rptRemito.fecha_proceso          = remitos.fecha_proceso
             rptRemito.observaciones          = remitos.observaciones
             rptRemito.precintos              = remitos.nro_precinto
             rptRemito.domicilio_sucursal     = IF AVAILABLE sucursales THEN sucursales.domicilio ELSE "NONE"
             rptRemito.localidad_sucursal     = IF AVAILABLE sucursales THEN sucursales.localidad ELSE "NONE"
             rptRemito.provincia_sucursal     = IF AVAILABLE sucursales THEN sucursales.provincia ELSE "NONE"
             rptRemito.telefono_sucursal      = IF AVAILABLE sucursales THEN sucursales.telefono[1] ELSE "NONE"
             rptRemito.impresion              = remitos.impresion
             rptRemito.fecha_proceso          = remitos.fecha_proceso
             .


    END.







  END. /*for first remitos ...*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-liberarTambores) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE liberarTambores Procedure 
PROCEDURE liberarTambores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piPte AS INTEGER    NO-UNDO.

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  RUN libTamboresIndustria.p PERSISTENT SET hLib.

  FOR EACH tambores_industria WHERE tambores_industria.id_sucursal_remito = piSuc
                                AND tambores_industria.id_tipo_movsto     = piTip
                                AND tambores_industria.nro_remito         = piNro
                                AND tambores_industria.ITEM_factura       = piPte.

    /*borro la clave del remito de los tambores*/
    RUN setClaveRemitoTambor IN hLib (tambores_industria.id_empresa,
                                      tambores_industria.id_sucursal,
                                      tambores_industria.id_tipotambor,
                                      tambores_industria.nromov,
                                      tambores_industria.id_tambor,
                                      0, 0, 0, 0).
    
    /*borro la relacion tambor-item_factura*/
    RUN deleteRelTamborRemito IN hLib (tambores_industria.id_empresa,
                                       tambores_industria.id_sucursal,
                                       tambores_industria.id_tipotambor,
                                       tambores_industria.nromov,
                                       tambores_industria.id_tambor,
                                       piNro, 
                                       piSuc,
                                       piTip,
                                       piPte).

  END.

  /*RUN recalcKilosRemito (piSuc, piTip, piNro).*/
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-mailingProcesamientoRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mailingProcesamientoRemito Procedure 
PROCEDURE mailingProcesamientoRemito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  DEFINE VARIABLE cTo  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSub AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMes AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE crlf AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAux AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFSa AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFLl AS CHARACTER  NO-UNDO.
  
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iLot AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAno AS INTEGER    NO-UNDO.

  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibTam AS HANDLE     NO-UNDO.

  RUN libTamboresIndustria PERSISTENT SET hLibTam.
  RUN libReportes.p PERSISTENT SET hLib.
  crlf = CHR(13) + CHR(10).


  FOR FIRST remitos NO-LOCK WHERE remitos.id_sucursal     = piSuc
                              AND remitos.id_tipo_movsto  = piTip
                              AND remitos.nro             = piNro.
    IF remitos.id_orden_entrega = 0 THEN 
      RETURN.
    
    FIND FIRST clientes OF remitos NO-LOCK NO-ERROR.

    cFSa = IF remitos.fecha_salida <> ? THEN STRING(remitos.fecha_salida) ELSE "".
    cFLl = IF remitos.fecha_llegada <> ? THEN STRING(remitos.fecha_llegada) ELSE "".
    

    cMes = " Nro Rto: "         + string(remitos.nro_comp, "9999-99999999") + crlf
         + " Sucursal: "        + DYNAMIC-FUNCTION('getDescSucursal' IN hLib, remitos.id_sucursal) + crlf 
         + " Fecha: "           + STRING(remitos.fecha) + crlf
         + " Lugar Descarga: "  + DYNAMIC-FUNCTION('getDescLugarDescarga' IN hLib, remitos.id_lugdes) + crlf
         + " Kilos Netos: "     + STRING(getPesoRemito(piSuc, piTip, piNro)) + crlf   
         + " Lotes: "           + REPLACE(getLotesRemito(piSuc, piTip, piNro), CHR(10), crlf)  + crlf
         + " OE: "              + STRING(remitos.id_orden_entrega) + crlf 
         + " OF: "              + STRING(remitos.nro_orden_fab) + crlf 
         + " Fecha Salida: "    + cFSa + crlf
         + " Hora Salida: "     + STRING(remitos.hora_salida) + crlf 
         + " Fecha Llegada: "   + cFLl + crlf 
         + " Hora Llegada: "    + STRING(remitos.hora_llegada) + crlf 
         + " Transporte: "      + DYNAMIC-FUNCTION('getDescProveedor' IN hLib, remitos.id_proveedor) + crlf 
         + " Chofer: "          + remitos.chofer + crlf
         + " Pat. Chasis: "     + remitos.pat_chasis + crlf 
         + " Pag. Acoplado: "   + remitos.pat_acopla + crlf 
         + " Cliente: "         + IF AVAILABLE clientes THEN clientes.razon_social ELSE "NO-INFO"
         + " Usuario: "         + remitos.c_usuario + crlf + crlf + crlf 
         + " ClaveRemito: "     + STRING(piSuc) + " - " + STRING(piTip) + " - " + STRING(piNro) + crlf.
         
  END.
  
  cTo  = DYNAMIC-FUNCTION('getUsuariosLista' IN hLibTam, 104).
  cSub = "aviso de procesamiento de remito ".

  RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, "").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-processRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processRemito Procedure 
PROCEDURE processRemito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER plRpt AS LOGICAL    NO-UNDO. /*imprime reporte?*/

  DEFINE VARIABLE iDes    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iEmp    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSuc    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTip    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iNro    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTra    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hLibLot AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibRep AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iNroRem AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iPtoVta AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lTipRem AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE dFecha  AS DATE       NO-UNDO.
  DEFINE VARIABLE cNroRem AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dPeso   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cSub    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cMes    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTo     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPdf    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFil    AS CHARACTER  NO-UNDO.



  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLibLot = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libLotesUbicacion.p').
  hLibRep = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  

  DEFINE BUFFER buRemitos FOR remitos.


  FOR FIRST buRemitos 
      WHERE buRemitos.id_sucursal    = piSuc
        AND buRemitos.id_tipo_movsto = piTip 
        AND buRemitos.nro            = piNro.

    /*control si ya fue procesado*/
    IF buRemitos.fecha_proceso <> ? THEN DO:
      MESSAGE "Este Remito ya fue Procesado el dia " + STRING(buRemitos.fecha_proceso) SKIP "OJO Munioz!!!"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
    END.

     /*conrol vencimiento cai*/
    FOR FIRST tipo_numero 
        WHERE tipo_numero.id_sucursal    = piSuc
          AND tipo_numero.id_tipo_movsto = piTip
        NO-LOCK.

      IF (tipo_numero.fecha_vencimiento - 5) < buRemitos.fecha THEN DO:
        MESSAGE "El Nro de Cai esta Vencido o Proximo a Vencerse." SKIP "El Remito no se emitira" VIEW-AS ALERT-BOX INFO BUTTONS OK.
        cSub = "Mail Automatico - Aviso de Vencimiento de Cai".
        cMes = "El nro de Cai " + STRING(tipo_numero.nro_cai) + " para el punto de venta " + STRING(tipo_numero.id_punto_venta) + " con vencimiento " + STRING(tipo_numero.fecha_vencimiento) + " se encuentra Vencido o Proximo a Vencer. Por Favor tomar las Medidas Correspondientes para corregir este Inconvieniente.".
        cTo  = "facundoj@sa-sanmiguel.com,maxivm@sa-sanmiguel.com,csalcedo@sa-sanmiguel.com,raitelli@sa-sanmiguel.com,rvelez@sa-sanmiguel.com".
        RUN ..\industria\sendMail.p("", 2, cSub, cMes, cTo, "").
        RETURN.
      END.
  
    END.
    
    /*control de pesos*/
    dPeso = getPesoRemito(piSuc, piTip, piNro).
    IF buRemitos.peso_neto <> dPeso THEN DO:
      MESSAGE "Error en el Peso del Remito" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN.
    END.
  
    /*control de calidad*/
    IF NOT getControlCalidad(piSuc, piTip, piNro) THEN DO:
      MESSAGE "Algun o Todos los items de este Remito no pasan los controles de Calidad o Microbiologia" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN.
    END.

    /* obtengo sucursal destino de los tambores */
    iDes = getSucursalDestino(piSuc, piTip, piNro).


    /*obtengo nros de comprobante*/
    cNroRem = buRemitos.nro_comp.
    dFecha  = TODAY.

    IF buRemitos.id_tipo_movsto = 123 AND buRemitos.nro_comp = "" THEN DO:
      cNroRem = getNextNroRemito(buRemitos.id_sucursal, buRemitos.id_tipo_movsto).
      iNroRem = INTEGER(ENTRY(2, cNroRem, CHR(1))).
      iPtoVta = INTEGER(ENTRY(3, cNroRem, CHR(1))).
      lTipRem = TRUE.
      cNroRem = STRING(iPtoVta,"9999") + STRING(iNroRem,"99999999").
    END. /*if buRemitos.id_tipo_movsto = 123 ...*/

    IF (buRemitos.id_tipo_movsto = 122 OR buRemitos.id_tipo_movsto = 121) AND buRemitos.nro_comp = "" THEN DO:
      RUN wc_nro_remito_manual.w (OUTPUT iNroRem,
                                  OUTPUT iPtoVta,
                                  OUTPUT dFecha).
      IF iNroRem = 0 OR iPtoVta = 0 OR dFecha = ? THEN DO:
        MESSAGE "No eligio un numero de comprobante, punto de venta o fecha de remito valido." VIEW-AS ALERT-BOX.
        UNDO , LEAVE.
      END.         
      lTipRem = FALSE.
      cNroRem = STRING(iPtoVta,"9999") + STRING(iNroRem,"99999999").      
    END. /*if buRemitos.id_tipo_movsto = 122 ...*/
    
    
    DO TRANSACTION ON ERROR UNDO, LEAVE:
      FOR EACH items_factura WHERE items_factura.id_sucursal    = piSuc
                               AND items_factura.id_tipo_movsto = piTip
                               AND items_factura.nro            = piNro
                             NO-LOCK.
        IF items_factura.id_tipotambor <> 11 THEN DO:
          /*transfiero tambores*/
          FOR EACH r_tambor_remito WHERE r_tambor_remito.id_sucursal_remito = items_factura.id_sucursal
                                     AND r_tambor_remito.id_tipo_movsto     = items_factura.id_tipo_movsto
                                     AND r_tambor_remito.nro_remito         = items_factura.nro
                                     AND r_tambor_remito.ITEM_factura       = items_factura.ITEM
                                   NO-LOCK.
            ASSIGN iEmp = r_tambor_remito.id_empresa
                   iSuc = r_tambor_remito.id_sucursal
                   iTip = r_tambor_remito.id_tipotambor
                   iNro = r_tambor_remito.nromov.
  
            RUN transferirTambor (r_tambor_remito.id_empresa,
                                  r_tambor_remito.id_sucursal,
                                  r_tambor_remito.id_tipotambor,
                                  r_tambor_remito.nromov,
                                  r_tambor_remito.id_tambor, 
                                  iDes). /*mando a transito*/
          END. /*if id_tipotambor <> 11*/
          /*actualizo lotes_ubicacion*/
          IF items_factura.id_tipotambor = 3 OR items_factura.id_tipotambor = 6 THEN
            RUN recalcLotesUbicacion(piSuc, piTip, piNro, iDes).
          
        END.
        ELSE DO: /*cascara*/
          FIND FIRST r_lote_cascara_remito WHERE r_lote_cascara_remito.id_sucursal_remito = items_factura.id_sucursal
                                             AND r_lote_cascara_remito.id_tipo_movsto     = items_factura.id_tipo_movsto
                                             AND r_lote_cascara_remito.nro_remito         = items_factura.nro
                                             AND r_lote_cascara_remito.ITEM_factura       = items_factura.ITEM
                                           NO-LOCK NO-ERROR.
          IF AVAILABLE r_lote_cascara_remito THEN 
            RUN transferenciaLoteCascara IN hLibLot (r_lote_cascara_remito.id_empresa,
                                                     r_lote_cascara_remito.id_sucursal,
                                                     r_lote_cascara_remito.nromov,
                                                     piSuc,
                                                     iDes,
                                                     items_factura.cantidad).
        END. /*else*/
      END. /*for each items_factura ...*/
  
      /*actualizo campos en remitos y oe*/          
      FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = buRemitos.id_orden_entrega 
                                       AND items_orden_entrega.item_oe          = buRemitos.item_oe
                                     NO-ERROR.
      IF AVAILABLE items_orden_entrega THEN 
        ASSIGN items_orden_entrega.id_estado = 2.
                  
      IF buRemitos.id_tipo_movsto = 122 THEN
        buRemitos.fecha         = dFecha.
      
      ASSIGN buRemitos.fecha_proceso = TODAY
             buRemitos.impresion     = 0
             buRemitos.c_usuario     = USERID("userdb")
             buRemitos.c_fecha       = TODAY
             buRemitos.c_hora        = STRING(TIME,"HH:MM:SS")
             buRemitos.nro_comp      = cNroRem
             buRemitos.tipo_remito   = lTipRem.
            
      /*observaciones para documento*/
      DEFINE VARIABLE cObs AS CHARACTER  NO-UNDO.
      FIND FIRST clientes OF buRemitos NO-LOCK NO-ERROR.
      cObs = "remito nro: " + buRemitos.nro_comp + " despachado el dia " + STRING(buRemitos.fecha_proceso) +  " para " + IF AVAILABLE clientes THEN clientes.razon_social ELSE "NONE".


     
    END. /*do transaction*/

    /*que me mande un mail para verificar el funcionamiento de la libreria*/
    RUN mailingProcesamientoRemito (piSuc, piTip, piNro).

    /*impresion*/
    IF plRpt THEN     
      RUN impresionRemito IN hLibRep (piSuc, piTip, piNro).

    /*marco fechas en documento oe remito*/
    IF remitos.mercado <> 0 THEN DO:
      FOR FIRST documentos_oe
        WHERE documentos_oe.id_orden_entrega  = remitos.id_orden_entrega
          AND documentos_oe.id_tipo_documento = 5.
  
        ASSIGN documentos_oe.fecha_pedido   = TODAY
               documentos_oe.observacionies = cObs.        
      END.
    END.
  
  END. /*for first remitos*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-recalcKilosRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalcKilosRemito Procedure 
PROCEDURE recalcKilosRemito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.

  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBru AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cAux AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iBul AS INTEGER    NO-UNDO.

  FOR EACH items_factura WHERE items_factura.id_sucursal    = piSuc
                           AND items_factura.id_tipo_movsto = piTip
                           AND items_factura.nro            = piNro
                         NO-LOCK.
    cAux = getKilosItemRemito(items_factura.id_sucursal,
                              items_factura.id_tipo_movsto,
                              items_factura.nro,
                              items_factura.ITEM).
    dKil = dKil + DECIMAL(ENTRY(1, cAux, CHR(1))).
    dBru = dBru + DECIMAL(ENTRY(2, cAux, CHR(1))).    
    iBul = iBul + INTEGER(ENTRY(3, cAux, CHR(1))).
    
  END.

  FIND FIRST remitos WHERE remitos.id_sucursal    = piSuc
                       AND remitos.id_tipo_movsto = piTip
                       AND remitos.nro            = piNro
                     NO-ERROR.
  IF AVAILABLE remitos THEN DO:
    ASSIGN remitos.peso_neto  = ROUND(dKil, 2)
           remitos.peso_bruto = ROUND(dBru, 2)
           remitos.bultos     = iBul.
  END.

  RELEASE remitos.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-recalcLotesUbicacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalcLotesUbicacion Procedure 
PROCEDURE recalcLotesUbicacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO. 
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO. 
  DEFINE INPUT  PARAMETER piDes AS INTEGER    NO-UNDO.

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iDes AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iHas AS INTEGER    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.
  
  
  FOR EACH r_tambor_remito WHERE r_tambor_remito.id_sucursal_remito = piSuc
                             AND r_tambor_remito.id_tipo_movsto     = piTip
                             AND r_tambor_remito.nro_remito         = piNro
                           BREAK BY r_tambor_remito.nromov.
    IF FIRST-OF(r_tambor_remito.nromov) THEN 
      iDes = r_tambor_remito.id_tambor.

    IF LAST-OF(r_tambor_remito.nromov) THEN DO:      
      RUN updateLoteUbicacion IN hLib (r_tambor_remito.id_empresa,
                                       r_tambor_remito.id_sucursal,
                                       r_tambor_remito.id_tipotambor,
                                       r_tambor_remito.nromov, 
                                       0).
      /*grabo movimientos (rutina de cacha)*/
      RUN Y_gstkmovdep.p (r_tambor_remito.id_empresa,
                          r_tambor_remito.id_sucursal,
                          r_tambor_remito.id_tipotambor,
                          r_tambor_remito.nromov, 
                          r_tambor_remito.id_sucursal,
                          piDes,
                          iDes,
                          r_tambor_remito.id_tambor,
                          3 ,
                          TODAY ).
            
      IF RETURN-VALUE <> "" THEN DO:
          UNDO , RETURN RETURN-VALUE.
      END.            

    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setKilosItemRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKilosItemRemito Procedure 
PROCEDURE setKilosItemRemito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piPte AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pdKil AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER pdPes AS DECIMAL    NO-UNDO.
  DEFINE INPUT  PARAMETER piCan AS INTEGER    NO-UNDO.


  FOR FIRST items_factura WHERE items_factura.id_sucursal     = piSuc
                            AND items_factura.id_tipo_movsto  = piTip
                            AND items_factura.nro             = piNro
                            AND items_factura.ITEM            = piPte.
    ASSIGN items_factura.kilos    = pdKil
           items_factura.peso     = pdPes
           items_factura.cantidad = piCan.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNroLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setNroLote Procedure 
PROCEDURE setNroLote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piPte AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piMov AS INTEGER    NO-UNDO.

  DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCod AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.
  

  FOR FIRST tambores_industria 
      WHERE tambores_industria.nromov = piMov 
      NO-LOCK.

    cLot = STRING(tambores_industria.id_lote, "9999") + "/" + SUBSTRING(STRING(tambores_industria.anio), 3, 2).
    IF tambores_industria.codigo_lote <> "" THEN
      cCod = tambores_industria.codigo_lote.
    ELSE 
      cCod = DYNAMIC-FUNCTION('getComposeNroLote' IN hLib, tambores_industria.id_sucursal,
                                                           tambores_industria.id_articulo,
                                                           tambores_industria.id_lote,
                                                           tambores_industria.anio).


  END.

  
  
  FOR FIRST items_factura 
      WHERE items_factura.id_sucursal     = piSuc
        AND items_factura.id_tipo_movsto  = piTip
        AND items_factura.nro             = piNro
        AND items_factura.ITEM            = piPte.
  
    ASSIGN items_factura.nro_lote    = cLot
           items_factura.descripcion = cCod
           .
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRelLoteCascaraRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setRelLoteCascaraRemito Procedure 
PROCEDURE setRelLoteCascaraRemito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piSucRto AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipRto AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroRto AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piPteRto AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piEmpLot AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSucLot AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTipLot AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNroLot AS INTEGER    NO-UNDO.
 



  FOR FIRST lotes_ubicacion 
       WHERE lotes_ubicacion.id_empresa    = piEmpLot
         AND lotes_ubicacion.id_sucursal   = piSucLot
         AND lotes_ubicacion.id_tipotambor = piTipLot
         AND lotes_ubicacion.nromov        = piNroLot
       NO-LOCK.

    FIND FIRST r_lote_cascara_remito 
         WHERE r_lote_cascara_remito.id_sucursal_remito = piSucRto
           AND r_lote_cascara_remito.id_tipo_movsto     = piTipRto
           AND r_lote_cascara_remito.nro_remito         = piNroRto
           AND r_lote_cascara_remito.id_sucursal        = lotes_ubicacion.id_sucursal
           AND r_lote_cascara_remito.id_empresa         = lotes_ubicacion.id_empresa
           AND r_lote_cascara_remito.id_tipotambor      = lotes_ubicacion.id_tipotambor
           AND r_lote_cascara_remito.nromov             = lotes_ubicacion.nromov
         NO-ERROR.

    IF NOT AVAILABLE r_lote_cascara_remito THEN 
      CREATE r_lote_cascara_remito.
            
    ASSIGN r_lote_cascara_remito.id_sucursal_remito    = piSucRto
           r_lote_cascara_remito.id_tipo_movsto        = piTipRto
           r_lote_cascara_remito.nro_remito            = piNroRto
           r_lote_cascara_remito.id_empresa            = lotes_ubicacion.id_empresa
           r_lote_cascara_remito.id_sucursal           = lotes_ubicacion.id_sucursal
           r_lote_cascara_remito.id_tipotambor         = lotes_ubicacion.id_tipotambor
           r_lote_cascara_remito.nromov                = lotes_ubicacion.nromov
           r_lote_cascara_remito.ITEM_factura          = piPteRto.

  END. /*for first lotes_ubicacion*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRelTamboresRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setRelTamboresRemito Procedure 
PROCEDURE setRelTamboresRemito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piDes AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piHas AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSRe AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTRe AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNRe AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piIRe AS INTEGER    NO-UNDO.

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  DELETE OBJECT hLibCom.
  

  DO i = piDes TO piHas:
    /*clave remito en tambor*/
    RUN setClaveRemitoTambor IN hLib (piEmp, piSuc, piTip, piNro, i, 
                                      piNRe, piTRe, piSRe, piIRe).

    /*relacion item_factura con tambor (necesaria para el stock a fecha)*/
    RUN setRelRemitoTambor IN hLib (piEmp, piSuc, piTip, piNro, i, 
                                    piNRe, piSRe, piTRe, piIRe).

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-transferirTambor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transferirTambor Procedure 
PROCEDURE transferirTambor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piEmp AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSuc AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTip AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piNro AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piTam AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piUbi AS INTEGER    NO-UNDO.


  
  FOR FIRST tambores_industria WHERE tambores_industria.id_empresa             = piEmp
                                 AND tambores_industria.id_sucursal            = piSuc
                                 AND tambores_industria.id_tipotambor          = piTip
                                 AND tambores_industria.nromov                 = piNro
                                 AND tambores_industria.id_tambor              = piTam
                                 /*AND tambores_industria.id_locacion_ubicacion  = 4*/.
    
    ASSIGN tambores_industria.id_sucursal_ubicacion = piUbi
           tambores_industria.id_estado             = 12
           .
  END.



  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getCantidadTambores) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantidadTambores Procedure 
FUNCTION getCantidadTambores RETURNS INTEGER
  (piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRet AS INTEGER    NO-UNDO.
  FOR EACH items_factura
      WHERE items_factura.id_sucursal    = piSuc
        AND items_factura.id_tipo_movsto = piTip
        AND items_factura.nro            = piNro
      NO-LOCK.
    iRet = iRet + (items_factura.hasta_lote - items_factura.desde_lote + 1).
  END.

  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getControlCalidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getControlCalidad Procedure 
FUNCTION getControlCalidad RETURNS LOGICAL
  (piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  RUN libTamboresIndustria.p PERSISTENT SET hLib.

  
  FOR FIRST remitos WHERE remitos.id_sucursal     = piSuc 
                      AND remitos.id_tipo_movsto  = piTip
                      AND remitos.nro             = piNro
                    NO-LOCK.
    /*si el remito es de lav a fam o viceversa ==> apruebo*/
    FIND FIRST lugar_descarga OF remitos NO-LOCK NO-ERROR.
    IF (remitos.id_sucursal = 95 OR remitos.id_sucursal = 96) AND (lugar_descarga.id_sucursal = 95 OR lugar_descarga.id_sucursal = 96) THEN DO:
      lRet = TRUE.
      LEAVE.
    END.

    /*para lotes de jugo, aceite, cascara y foldeado verificar el control de calidad y micro, para el resto los doy por aprobados*/
    FOR EACH items_factura OF remitos NO-LOCK.
      IF items_factura.id_tipotambor <> 11 THEN DO:
        FIND FIRST r_tambor_remito WHERE r_tambor_remito.id_sucursal_remito = items_factura.id_sucursal
                                     AND r_tambor_remito.id_tipo_movsto     = items_factura.id_tipo_movsto
                                     AND r_tambor_remito.nro_remito         = items_factura.nro
                                     AND r_tambor_remito.ITEM_factura       = items_factura.ITEM
                                   NO-LOCK NO-ERROR.
        lRet = DYNAMIC-FUNCTION('getControlCalidadLote' IN hLib, r_tambor_remito.id_empresa,
                                                                 r_tambor_remito.id_sucursal,
                                                                 r_tambor_remito.id_tipotambor,
                                                                 r_tambor_remito.nromov).
      END.
      ELSE DO: /*cascara*/
        FIND FIRST r_lote_cascara_remito WHERE r_lote_cascara_remito.id_sucursal_remito = items_factura.id_sucursal
                                           AND r_lote_cascara_remito.id_tipo_movsto     = items_factura.id_tipo_movsto
                                           AND r_lote_cascara_remito.nro_remito         = items_factura.nro
                                           AND r_lote_cascara_remito.ITEM_factura       = items_factura.ITEM
                                         NO-LOCK NO-ERROR.
        lRet = DYNAMIC-FUNCTION('getControlCalidadLote' IN hLib, r_lote_cascara_remito.id_empresa,
                                                                 r_lote_cascara_remito.id_sucursal,
                                                                 r_lote_cascara_remito.id_tipotambor,
                                                                 r_lote_cascara_remito.nromov).
      END.
     /* lo hice para salvar la urgencia- corregir luego.*/
     /*lRet = TRUE. */

      /*si lRet = false significa que algun item no paso el control*/
      IF lRet = FALSE THEN
        RETURN lRet.

    END.
  END.

  RETURN lRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDatosOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDatosOE Procedure 
FUNCTION getDatosOE RETURNS CHARACTER
  (piOE    AS INTEGER, 
   piParte AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = "0" + CHR(1) + 
         "0" + CHR(1) + 
         "0" + CHR(1) + 
         "0" + CHR(1) + 
         "0" + CHR(1) + 
         "0". 

  FIND FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = piOE
                           NO-LOCK NO-ERROR.
  IF AVAILABLE orden_entrega THEN DO:
    cRet = STRING(orden_entrega.id_destino) + CHR(1) + 
           STRING(orden_entrega.id_lugdes) + CHR(1) + 
           STRING(orden_entrega.id_vapor) + CHR(1).
    FIND FIRST items_orden_entrega OF orden_entrega NO-LOCK NO-ERROR.
    IF AVAILABLE items_orden_entrega THEN DO:
      cRet = cRet + STRING(items_orden_entrega.id_cliente) + CHR(1).
      FIND FIRST contratos OF items_orden_entrega NO-LOCK NO-ERROR.
      IF AVAILABLE contratos THEN DO:
        cRet = cRet + STRING(contratos.orden_fabricacion) + CHR(1) + 
                      STRING(contratos.anio).
      END.
      ELSE
        cRet = cRet + 
               "0" + CHR(1) +              
               "0". 
    END.
    ELSE
      cRet = cRet + 
             "0" + CHR(1) + 
             "0" + CHR(1) +              
             "0". 

      
  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDatosOERemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDatosOERemito Procedure 
FUNCTION getDatosOERemito RETURNS CHARACTER
  (prRemito AS ROWID) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  cRet = "0" + CHR(1) + "0".
  FOR FIRST remitos WHERE ROWID(remitos) = prRemito NO-LOCK.
    cRet = STRING(remitos.id_orden_entrega) + CHR(1) + STRING(remitos.ITEM_oe).
  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosItemRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilosItemRemito Procedure 
FUNCTION getKilosItemRemito RETURNS CHARACTER
  (piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER, 
   piPte AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dNet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBru AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iBul AS INTEGER    NO-UNDO.

  FOR FIRST items_factura WHERE items_factura.id_sucursal     = piSuc
                            AND items_factura.id_tipo_movsto  = piTip
                            AND items_factura.nro             = piNro
                            AND items_factura.ITEM            = piPte
                          NO-LOCK.
    IF items_factura.id_tipotambor <> 11 THEN DO:
      FOR EACH r_tambor_remito WHERE r_tambor_remito.id_sucursal_remito = piSuc
                                 AND r_tambor_remito.id_tipo_movsto     = piTip
                                 AND r_tambor_remito.nro_remito         = piNro
                                 AND r_tambor_remito.ITEM_factura       = piPte
                               NO-LOCK.
        FOR FIRST tambores_industria WHERE tambores_industria.nromov    = r_tambor_remito.nromov
                                       AND tambores_industria.id_tambor = r_tambor_remito.id_tambor
                                     NO-LOCK.
          dNet = dNet + tambores_industria.kilos_tambor.
          dBru = dBru + tambores_industria.kilos_tambor + tambores_industria.tara.
          iBul = iBul + 1.
        END.    
      END.
    END.
    ELSE DO:
      dNet = items_factura.cantidad * 50.
      dBru = items_factura.cantidad * 50.12.
      iBul = items_factura.cantidad.
    END.
  END.



  RETURN STRING(dNet) + CHR(1) + STRING(dBru) + CHR(1) + STRING(iBul).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLotesRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLotesRemito Procedure 
FUNCTION getLotesRemito RETURNS CHARACTER
  (piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

  RUN libReportes.p PERSISTENT SET hLib.

  FOR EACH items_factura WHERE items_factura.id_sucursal = piSuc 
                           AND items_factura.id_tipo_movsto = piTip
                           AND items_factura.nro = piNro
                         NO-LOCK.
    cRet = cRet + items_factura.nro_lote + " " + 
                  DYNAMIC-FUNCTION('getDescArticulo' IN hLib, items_factura.id_articulo) + " x " + 
                  STRING(items_factura.cantidad) + CHR(10).
  END.

  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNextItemRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextItemRemito Procedure 
FUNCTION getNextItemRemito RETURNS INTEGER
  (piSuc AS INTEGER,
   piTip AS INTEGER,
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE iRet AS INTEGER    NO-UNDO.

  FOR LAST items_factura 
      WHERE items_factura.id_sucursal    = piSuc
        AND items_factura.id_tipo_movsto = piTip
        AND items_factura.nro            = piNro
      BY items_factura.ITEM.

    iRet = items_factura.ITEM.
  END.

  iRet = iRet + 1.

  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNextNroRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextNroRemito Procedure 
FUNCTION getNextNroRemito RETURNS CHARACTER
  (piSuc AS INTEGER, 
   piTip AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iNro AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iSeq AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iPto AS INTEGER    NO-UNDO.

  DEFINE BUFFER buTipoNumero FOR tipo_numero.

  RELEASE buTipoNumero.
  FIND FIRST buTipoNumero WHERE buTipoNumero.id_sucursal    = piSuc
                            AND buTipoNumero.id_tipo_movsto = piTip
                          NO-ERROR.
  IF AVAILABLE buTipoNumero THEN DO:
    iSeq = buTipoNumero.nro + 1.
    iNro = buTipoNumero.nro_comprobante + 1.
    iPto = buTipoNumero.id_punto_venta.

    buTipoNumero.nro_comprobante = iNro.
    buTipoNumero.nro             = iSeq.
  END.
  ELSE DO: 
    iSeq = 1.
    iNro = 1.
  END.

  RELEASE buTipoNumero.

  RETURN STRING(iSeq) + CHR(1) + STRING(iNro) + CHR(1) + STRING(iPto).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPesoRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPesoRemito Procedure 
FUNCTION getPesoRemito RETURNS DECIMAL
  (piSuc AS INTEGER,
   piTip AS INTEGER, 
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cAux AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lCas AS LOGICAL    NO-UNDO.
  
  DEFINE BUFFER buItem FOR ITEMs_factura.

  FOR EACH buItem WHERE buItem.id_sucursal    = piSuc
                    AND buItem.id_tipo_movsto = piTip
                    AND buItem.nro            = piNro
                  NO-LOCK.
    IF buItem.id_tipotambor <> 11 THEN DO:    
      cAux = getKilosItemRemito(piSuc, piTip, piNro, buItem.ITEM).
      dKil = dKil + DECIMAL(ENTRY(1, cAux, CHR(1))).
    END.
    ELSE 
      lCas = TRUE.
  END.

  /*para cascara tomar el peso que figura en el remito porque es el detalle operativo de la aduana*/
  IF lCas THEN DO:
    FOR FIRST remitos WHERE remitos.id_sucursal     = piSuc
                        AND remitos.id_tipo_movsto  = piTip
                        AND remitos.nro             = piNro
                      NO-LOCK.
      dKil = remitos.peso_neto.

    END.
  END.

  RETURN dKil.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSucursalDestino) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSucursalDestino Procedure 
FUNCTION getSucursalDestino RETURNS INTEGER
  (piSuc AS INTEGER, 
   piTip AS INTEGER, 
   piNro AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRet    AS INTEGER    NO-UNDO. 
  DEFINE VARIABLE iSucDes AS INTEGER    NO-UNDO.

  FOR FIRST remitos WHERE remitos.id_sucursal     = piSuc
                      AND remitos.id_tipo_movsto  = piTip
                      AND remitos.nro             = piNro
                    NO-LOCK.
    FIND FIRST lugar_descarga OF remitos NO-LOCK NO-ERROR.
    IF AVAILABLE lugar_descarga THEN
      iRet = lugar_descarga.id_sucursal.

    FOR FIRST r_tambor_remito
        WHERE r_tambor_remito.id_sucursal_remito = remitos.id_sucursal
          AND r_tambor_remito.id_tipo_movsto     = remitos.id_tipo_movsto
          AND r_tambor_remito.nro_remito         = remitos.nro
        NO-LOCK, 
        FIRST tambores_industria 
        WHERE tambores_industria.nromov    = r_tambor_remito.nromov
          AND tambores_industria.id_tambor = r_tambor_remito.id_tambor
         NO-LOCK,
        FIRST items_contratos 
        WHERE items_contratos.id_contrato = tambores_industria.id_contrato
          AND items_contratos.ITEM        = tambores_industria.ITEM_of
          AND items_contrato.id_tipo_contrato = tambores_industria.id_tipocontrato_of
          AND items_contratos.anio            = tambores_industria.anio_of
          AND items_contratos.id_tipo_venta = 3
        NO-LOCK .

      iRet = 82.
        
    END.

  END.

  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSucursalTransito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSucursalTransito Procedure 
FUNCTION getSucursalTransito RETURNS INTEGER
  (piSucOri AS INTEGER,
   piSucDes AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE iRet AS INTEGER    NO-UNDO.

  /*origen lavalle*/
  IF piSucOri = 96 THEN DO:
    CASE piSucDes:
      WHEN 46 THEN
        iRet = 502.
      WHEN 95 THEN
        iRet = 500.
      WHEN 92 THEN
        iRet = 504.
      WHEN 87 THEN
        iRet = 506.
      WHEN 88 THEN
        iRet = 508.
    END CASE.
  END. /*if pisucori = 96*/

  /*origen famailla*/
  IF piSucOri = 95 THEN DO:
    CASE piSucDes:
      WHEN 46 THEN
        iRet = 503.
      WHEN 96 THEN
        iRet = 501.
      WHEN 92 THEN
        iRet = 505.
      WHEN 87 THEN
        iRet = 507.
      WHEN 88 THEN
        iRet = 509.
      /*
      WHEN 85 THEN
        iRet = 503.
      WHEN 511 THEN
        iRet = 503.
      */
    END CASE.
  END. /*if pisucori = 95*/

  /*origen mercotuc*/
  IF piSucOri = 92 THEN DO:
    CASE piSucDes:
      WHEN 46 THEN
        iRet = 0.
      WHEN 95 THEN
        iRet = 0.
      WHEN 96 THEN
        iRet = 0.
      WHEN 87 THEN
        iRet = 0.
      WHEN 88 THEN
        iRet = 0.
    END CASE.
  END. /*if pisucori = 92*/

  /*origen metan*/
  IF piSucOri = 87 THEN DO:
    CASE piSucDes:
      WHEN 46 THEN
        iRet = 0.
      WHEN 95 THEN
        iRet = 0.
      WHEN 92 THEN
        iRet = 0.
      WHEN 96 THEN
        iRet = 0.
      WHEN 88 THEN
        iRet = 0.
    END CASE.
  END. /*if pisucori = 87*/

  /*origen cool queen*/
  IF piSucOri = 88 THEN DO:
    CASE piSucDes:
      WHEN 46 THEN
        iRet = 0.
      WHEN 95 THEN
        iRet = 0.
      WHEN 92 THEN
        iRet = 0.
      WHEN 87 THEN
        iRet = 0.
      WHEN 96 THEN
        iRet = 0.
    END CASE.
  END. /*if pisucori = 88*/

  /*origen puerto bs as*/
  IF piSucOri = 46 THEN DO:
    CASE piSucDes:
      WHEN 95 THEN
        iRet = 0.
      WHEN 96 THEN
        iRet = 0.
      WHEN 92 THEN
        iRet = 0.
      WHEN 87 THEN
        iRet = 0.
      WHEN 88 THEN
        iRet = 0.
    END CASE.
  END. /*if pisucori = 46*/

  

  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

