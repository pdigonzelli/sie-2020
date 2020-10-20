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

&IF DEFINED(EXCLUDE-getArticulo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArticulo Procedure 
FUNCTION getArticulo RETURNS INTEGER
  (pcCon AS CHARACTER,
   piTip AS INTEGER,
   piAno AS INTEGER,
   piItm AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCalidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCalidad Procedure 
FUNCTION getCalidad RETURNS INTEGER
  (pcCon AS CHARACTER,
   piTip AS INTEGER,
   piAno AS INTEGER,
   piItm AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCondicionVenta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCondicionVenta Procedure 
FUNCTION getCondicionVenta RETURNS INTEGER
  (pcCon AS CHARACTER,
   piTip AS INTEGER,
   piAno AS INTEGER,
   piItm AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getContratosCliente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getContratosCliente Procedure 
FUNCTION getContratosCliente RETURNS CHARACTER
  (piCliente AS INTEGER, 
   piAnio    AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getItemsContrato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getItemsContrato Procedure 
FUNCTION getItemsContrato RETURNS CHARACTER
  (pcContrato AS CHARACTER, 
   piAnio     AS INTEGER, 
   piTip      AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLotesContrato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLotesContrato Procedure 
FUNCTION getLotesContrato RETURNS CHARACTER
  (pcContrato AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLotesItemContrato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLotesItemContrato Procedure 
FUNCTION getLotesItemContrato RETURNS CHARACTER
  (pcContrato AS CHARACTER, 
   piItem     AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMonedaVenta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMonedaVenta Procedure 
FUNCTION getMonedaVenta RETURNS INTEGER
  (pcCon AS CHARACTER,
   piTip AS INTEGER,
   piAno AS INTEGER,
   piItm AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPedidosCliente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPedidosCliente Procedure 
FUNCTION getPedidosCliente RETURNS CHARACTER
  (piCliente AS INTEGER, 
   piSemana  AS INTEGER, 
   piAnio    AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSemanaEntrega) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSemanaEntrega Procedure  _DB-REQUIRED
FUNCTION getSemanaEntrega RETURNS INTEGER
  (pcCon AS CHARACTER,
   piTip AS INTEGER,
   piAno AS INTEGER,
   piItm AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTamboresPedidos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTamboresPedidos Procedure 
FUNCTION getTamboresPedidos RETURNS INTEGER
  (pcCon AS CHARACTER,
   piTip AS INTEGER,
   piAno AS INTEGER,
   piItm AS INTEGER)  FORWARD.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getArticulo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArticulo Procedure 
FUNCTION getArticulo RETURNS INTEGER
  (pcCon AS CHARACTER,
   piTip AS INTEGER,
   piAno AS INTEGER,
   piItm AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRet AS INTEGER    NO-UNDO.


  FOR FIRST items_contratos
      WHERE items_contratos.id_contrato       = pcCon
        AND items_contratos.id_tipo_contrato  = piTip
        AND items_contratos.anio              = piAno
        AND items_contratos.ITEM              = piItm
      NO-LOCK.
    iRet = general.items_contratos.id_articulo.
  END.

  RETURN iRet.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCalidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCalidad Procedure 
FUNCTION getCalidad RETURNS INTEGER
  (pcCon AS CHARACTER,
   piTip AS INTEGER,
   piAno AS INTEGER,
   piItm AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRet AS INTEGER    NO-UNDO.


  FOR FIRST items_contratos
      WHERE items_contratos.id_contrato       = pcCon
        AND items_contratos.id_tipo_contrato  = piTip
        AND items_contratos.anio              = piAno
        AND items_contratos.ITEM              = piItm
      NO-LOCK.
    iRet = general.items_contratos.id_calidad.
  END.

  RETURN iRet.

  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCondicionVenta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCondicionVenta Procedure 
FUNCTION getCondicionVenta RETURNS INTEGER
  (pcCon AS CHARACTER,
   piTip AS INTEGER,
   piAno AS INTEGER,
   piItm AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRet AS INTEGER    NO-UNDO.


  FOR FIRST items_contratos
      WHERE items_contratos.id_contrato       = pcCon
        AND items_contratos.id_tipo_contrato  = piTip
        AND items_contratos.anio              = piAno
        AND items_contratos.ITEM              = piItm
      NO-LOCK.
    iRet = general.items_contratos.id_clausula.
  END.

  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getContratosCliente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getContratosCliente Procedure 
FUNCTION getContratosCliente RETURNS CHARACTER
  (piCliente AS INTEGER, 
   piAnio    AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.


  FOR EACH contratos
      WHERE contratos.id_cliente = piCliente
        AND (IF piAnio <> 0 THEN contratos.anio = piAnio ELSE TRUE) 
      BY contratos.id_contrato BY contratos.fecha.

      cRet = cRet + 
             contratos.id_contrato + "," + 
             STRING(contratos.fecha) + "," +
             STRING(contratos.orden_fabricacion) + CHR(10).    
  END.

  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).

  RETURN cRet.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getItemsContrato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getItemsContrato Procedure 
FUNCTION getItemsContrato RETURNS CHARACTER
  (pcContrato AS CHARACTER, 
   piAnio     AS INTEGER, 
   piTip      AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cEst AS CHARACTER  NO-UNDO.

  FOR EACH items_contratos 
      WHERE items_contratos.id_contrato       = pcContrato
        AND items_contratos.anio              = piAnio
        AND items_contratos.id_tipo_contrato  = piTip
      BY items_contrato.ITEM.

    cEst = IF items_contratos.pendiente = TRUE THEN "PENDIENTE DE OE" ELSE "YA TIENE OE".
    cRet = cRet + 
           STRING(items_contratos.ITEM) + "-" + 
           "Parte: " + STRING(items_contratos.ITEM) + 
           " Cant: "  + STRING(items_contratos.cantidad) + 
           " Semana: " + STRING(items_contratos.semana_entrega) + 
           " Estado: " + cEst + 
           ",".    
  END.

  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).
  
  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLotesContrato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLotesContrato Procedure 
FUNCTION getLotesContrato RETURNS CHARACTER
  (pcContrato AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iEst AS INTEGER    NO-UNDO.

  FOR EACH tambores_industria
      WHERE tambores_industria.id_contrato = pcContrato
        AND tambores_industria.ITEM_of     = 0
      BREAK BY tambores_industria.nromov.
    i = i + 1.
    k = k + tambores_industria.kilos_tambor.

    IF LAST-OF(tambores_industria.nromov) THEN DO:
      iEst = 0.
      IF tambores_industria.id_tipotambor = 3 THEN DO:
        FOR FIRST lotes_jugo OF tambores_industria NO-LOCK.
          IF lotes_jugo.CONTROL_calidad AND lotes_jugo.microbiologia THEN
            iEst = 1.
        END.

      END.
      IF tambores_industria.id_tipotambor = 6 THEN DO:
        FOR FIRST lotes_aceite OF tambores_industria NO-LOCK.
          IF lotes_aceite.CONTROL_calidad THEN
            iEst = 1.

        END.
      END.


      cRet = cRet + 
             STRING(tambores_industria.id_lote) + "," + 
             STRING(tambores_industria.anio) + "," + 
             STRING(i) + "," + 
             STRING(k) + "," +
             STRING(iEst) + "," + 
             STRING(tambores_industria.fecha) + CHR(10).

      i = 0.
      k = 0.



    END.
    
  END.

  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLotesItemContrato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLotesItemContrato Procedure 
FUNCTION getLotesItemContrato RETURNS CHARACTER
  (pcContrato AS CHARACTER, 
   piItem     AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iEst AS INTEGER    NO-UNDO.

  FOR EACH tambores_industria
      WHERE tambores_industria.id_contrato = pcContrato
        AND tambores_industria.ITEM_of     = piItem
      BREAK BY tambores_industria.nromov.
    i = i + 1.
    k = k + tambores_industria.kilos_tambor.

    IF LAST-OF(tambores_industria.nromov) THEN DO:
      iEst = 0.
      IF tambores_industria.id_tipotambor = 3 THEN DO:
        FOR FIRST lotes_jugo OF tambores_industria NO-LOCK.
          IF lotes_jugo.CONTROL_calidad AND lotes_jugo.microbiologia THEN
            iEst = 1.
        END.

      END.
      IF tambores_industria.id_tipotambor = 6 THEN DO:
        FOR FIRST lotes_aceite OF tambores_industria NO-LOCK.
          IF lotes_aceite.CONTROL_calidad THEN
            iEst = 1.

        END.
      END.

      IF tambores_industria.id_sucursal_ubicacion <> 95 OR 
         tambores_industria.id_sucursal_ubicacion <> 462 THEN
        iEst = 2.

      cRet = cRet + 
             STRING(tambores_industria.id_lote) + "," + 
             STRING(tambores_industria.anio) + "," + 
             STRING(i) + "," + 
             STRING(k) + "," +
             STRING(iEst) + CHR(10).

      i = 0.
      k = 0.



    END.
    
  END.

  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMonedaVenta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMonedaVenta Procedure 
FUNCTION getMonedaVenta RETURNS INTEGER
  (pcCon AS CHARACTER,
   piTip AS INTEGER,
   piAno AS INTEGER,
   piItm AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRet AS INTEGER    NO-UNDO.


  FOR FIRST items_contratos
      WHERE items_contratos.id_contrato       = pcCon
        AND items_contratos.id_tipo_contrato  = piTip
        AND items_contratos.anio              = piAno
        AND items_contratos.ITEM              = piItm
      NO-LOCK.
    iRet = items_contratos.id_moneda_origen.
  END.

  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPedidosCliente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPedidosCliente Procedure 
FUNCTION getPedidosCliente RETURNS CHARACTER
  (piCliente AS INTEGER, 
   piSemana  AS INTEGER, 
   piAnio    AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPro AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCal AS CHARACTER  NO-UNDO.


  FOR EACH contratos
      WHERE contratos.id_cliente = piCliente
        AND (IF piAnio <> 0 THEN contratos.anio = piAnio ELSE TRUE), 
      EACH items_contratos OF contratos
      WHERE (IF piSemana <> 0 THEN items_contratos.semana_entrega = piSemana ELSE TRUE)
      BY items_contratos.id_contrato BY items_contratos.fecha.

      FIND FIRST productos_terminados OF items_contratos NO-LOCK NO-ERROR.
      FIND FIRST calidades OF items_contratos NO-LOCK NO-ERROR.
      cPro = IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE "NONE".
      cCal = IF AVAILABLE calidades THEN calidades.descripcion ELSE "NONE".

      cRet = cRet + 
             items_contratos.id_contrato + "," + 
             STRING(items_contratos.ITEM) + "," +
             STRING(items_contratos.fecha) + "," + 
             STRING(items_contratos.cantidad) + "," + 
             "aqui tot factura" + "," + 
             STRING(contratos.orden_fabricacion) + "," + 
             cPro + "," +
             cCal + CHR(10).    
  END.

  cRet = SUBSTRING(cRet, 1, LENGTH(cRet) - 1).



  RETURN cRet.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSemanaEntrega) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSemanaEntrega Procedure 
FUNCTION getSemanaEntrega RETURNS INTEGER
  (pcCon AS CHARACTER,
   piTip AS INTEGER,
   piAno AS INTEGER,
   piItm AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRet AS INTEGER    NO-UNDO.


  FOR FIRST items_contratos
      WHERE items_contratos.id_contrato       = pcCon
        AND items_contratos.id_tipo_contrato  = piTip
        AND items_contratos.anio              = piAno
        AND items_contratos.ITEM              = piItm
      NO-LOCK.
    iRet = general.items_contratos.semana_entrega.
  END.

  RETURN iRet.

  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTamboresPedidos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTamboresPedidos Procedure 
FUNCTION getTamboresPedidos RETURNS INTEGER
  (pcCon AS CHARACTER,
   piTip AS INTEGER,
   piAno AS INTEGER,
   piItm AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRet AS INTEGER    NO-UNDO.


  FOR FIRST items_contratos
      WHERE items_contratos.id_contrato       = pcCon
        AND items_contratos.id_tipo_contrato  = piTip
        AND items_contratos.anio              = piAno
        AND items_contratos.ITEM              = piItm
      NO-LOCK.
    iRet = items_contratos.cantidad.
  END.

  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

