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

&IF DEFINED(EXCLUDE-calcularOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calcularOE Procedure 
FUNCTION calcularOE RETURNS CHARACTER
  (piOE  AS INTEGER, 
   plAsk AS LOGICAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAgenciaId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAgenciaId Procedure 
FUNCTION getAgenciaId RETURNS INTEGER
  (piOE AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCantPtesOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCantPtesOE Procedure 
FUNCTION getCantPtesOE RETURNS INTEGER
  (piOE AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCoefAduana) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCoefAduana Procedure 
FUNCTION getCoefAduana RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getComisionItemOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getComisionItemOE Procedure 
FUNCTION getComisionItemOE RETURNS DECIMAL
  (piOE AS INTEGER,
   piIt AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getContenedoresOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getContenedoresOE Procedure 
FUNCTION getContenedoresOE RETURNS DECIMAL
  (piOE AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFob) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFob Procedure 
FUNCTION getFob RETURNS DECIMAL
  (piOE AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFobUnitario) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFobUnitario Procedure 
FUNCTION getFobUnitario RETURNS DECIMAL
  (piOE AS INTEGER, 
   piIt AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFobUniTon) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFobUniTon Procedure 
FUNCTION getFobUniTon RETURNS DECIMAL
  (piOE AS INTEGER, 
   piIt AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getGalonesItemOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getGalonesItemOE Procedure 
FUNCTION getGalonesItemOE RETURNS DECIMAL
  (piOE AS INTEGER,
   piIt AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getGalonesOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getGalonesOE Procedure 
FUNCTION getGalonesOE RETURNS DECIMAL
  (piOE AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getGastoAgencia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getGastoAgencia Procedure 
FUNCTION getGastoAgencia RETURNS DECIMAL
  (piAgencia AS INTEGER, 
   piGasto   AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getGastoIks) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getGastoIks Procedure 
FUNCTION getGastoIks RETURNS DECIMAL
  (piOE AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getGastosItemOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getGastosItemOE Procedure 
FUNCTION getGastosItemOE RETURNS DECIMAL
  (piOE AS INTEGER,
   piIt AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosBrutosItemOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilosBrutosItemOE Procedure 
FUNCTION getKilosBrutosItemOE RETURNS DECIMAL
  (piOE AS INTEGER,
   piIt AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosBrutosOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilosBrutosOE Procedure 
FUNCTION getKilosBrutosOE RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosItemOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilosItemOE Procedure 
FUNCTION getKilosItemOE RETURNS DECIMAL
  (piOE AS INTEGER,
   piIt AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKilosOE Procedure 
FUNCTION getKilosOE RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNextItemOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextItemOE Procedure 
FUNCTION getNextItemOE RETURNS INTEGER
  (piOE AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPorcentajeReintegArti) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPorcentajeReintegArti Procedure 
FUNCTION getPorcentajeReintegArti RETURNS DECIMAL
  (piArt AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPrecio) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPrecio Procedure 
FUNCTION getPrecio RETURNS DECIMAL
  (piTip AS INTEGER, 
   pdKil AS DECIMAL, 
   pdPre AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTamboresItemOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTamboresItemOE Procedure 
FUNCTION getTamboresItemOE RETURNS INTEGER
  (piOE AS INTEGER,
   piIt AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTamboresKilosOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTamboresKilosOE Procedure 
FUNCTION getTamboresKilosOE RETURNS CHARACTER
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTotalFacturaItemOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTotalFacturaItemOE Procedure 
FUNCTION getTotalFacturaItemOE RETURNS DECIMAL
  (piOE AS INTEGER,
   piIt AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTotalFacturaOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTotalFacturaOE Procedure 
FUNCTION getTotalFacturaOE RETURNS DECIMAL
  (pcArgs AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTotalGastosOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTotalGastosOE Procedure 
FUNCTION getTotalGastosOE RETURNS DECIMAL
  (piOE AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUnidadPrecioContrato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUnidadPrecioContrato Procedure 
FUNCTION getUnidadPrecioContrato RETURNS CHARACTER
  (pcCon AS CHARACTER,
   piTip AS INTEGER,
   piAno AS INTEGER,
   piItm AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-puedeModificarOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD puedeModificarOE Procedure 
FUNCTION puedeModificarOE RETURNS LOGICAL
  (piOE  AS INTEGER,
   piPte AS INTEGER)  FORWARD.

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
         HEIGHT             = 14.62
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-createDocumentosOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createDocumentosOE Procedure 
PROCEDURE createDocumentosOE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piOE AS INTEGER    NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.

  FOR EACH documentos_oe
      BY documentos_oe.id_documento_oe DESC.
    i = documentos_oe.id_documento_oe.
    LEAVE.
  END.
  i = i + 1.

  FOR FIRST documentos_oe
      WHERE documentos_oe.id_orden_entrega = piOE
      NO-LOCK.
    RETURN.
  END.

  /*INAL*/
  CREATE documentos_oe.
  ASSIGN documentos_oe.c_fecha            = TODAY
         documentos_oe.c_hora             = STRING(TIME, "HH:MM")
         documentos_oe.c_usuario          = USERID('userdb')
         documentos_oe.descripcion        = "INAL"
         documentos_oe.fecha_cumplido     = DATE("")
         documentos_oe.fecha_pedido       = DATE("")
         documentos_oe.id_documento_oe    = i
         documentos_oe.id_estado          = 1
         documentos_oe.id_orden_entrega   = piOE
         documentos_oe.id_tipo_documento  = 1
         documentos_oe.observacionies     = ""
         i = i + 1.
         .

  /*CONTENEDORES*/
  CREATE documentos_oe.
  ASSIGN documentos_oe.c_fecha            = TODAY
         documentos_oe.c_hora             = STRING(TIME, "HH:MM")
         documentos_oe.c_usuario          = USERID('userdb')
         documentos_oe.descripcion        = "ORDEN RETIRO DE CONTENEDORES"
         documentos_oe.fecha_cumplido     = DATE("")
         documentos_oe.fecha_pedido       = DATE("")
         documentos_oe.id_documento_oe    = i
         documentos_oe.id_estado          = 1
         documentos_oe.id_orden_entrega   = piOE
         documentos_oe.id_tipo_documento  = 2
         documentos_oe.observacionies     = ""
         i = i + 1.
         .

  /*RESERVA*/
  CREATE documentos_oe.
  ASSIGN documentos_oe.c_fecha            = TODAY
         documentos_oe.c_hora             = STRING(TIME, "HH:MM")
         documentos_oe.c_usuario          = USERID('userdb')
         documentos_oe.descripcion        = "RESERVA ESPACIO EN BUQUE"
         documentos_oe.fecha_cumplido     = DATE("")
         documentos_oe.fecha_pedido       = DATE("")
         documentos_oe.id_documento_oe    = i
         documentos_oe.id_estado          = 1
         documentos_oe.id_orden_entrega   = piOE
         documentos_oe.id_tipo_documento  = 3
         documentos_oe.observacionies     = ""
         i = i + 1.
         .

  /*ASOCIACION TAMBORES*/
  CREATE documentos_oe.
  ASSIGN documentos_oe.c_fecha            = TODAY
         documentos_oe.c_hora             = STRING(TIME, "HH:MM")
         documentos_oe.c_usuario          = USERID('userdb')
         documentos_oe.descripcion        = "ASOCIACION TAMBORES"
         documentos_oe.fecha_cumplido     = DATE("")
         documentos_oe.fecha_pedido       = DATE("")
         documentos_oe.id_documento_oe    = i
         documentos_oe.id_estado          = 1
         documentos_oe.id_orden_entrega   = piOE
         documentos_oe.id_tipo_documento  = 4
         documentos_oe.observacionies     = ""
         i = i + 1.
         .

  /*REMITO*/
  CREATE documentos_oe.
  ASSIGN documentos_oe.c_fecha            = TODAY
         documentos_oe.c_hora             = STRING(TIME, "HH:MM")
         documentos_oe.c_usuario          = USERID('userdb')
         documentos_oe.descripcion        = "REMITO"
         documentos_oe.fecha_cumplido     = DATE("")
         documentos_oe.fecha_pedido       = DATE("")
         documentos_oe.id_documento_oe    = i
         documentos_oe.id_estado          = 1
         documentos_oe.id_orden_entrega   = piOE
         documentos_oe.id_tipo_documento  = 5
         documentos_oe.observacionies     = ""
         i = i + 1.
         .

  /*FAX*/
  CREATE documentos_oe.
  ASSIGN documentos_oe.c_fecha            = TODAY
         documentos_oe.c_hora             = STRING(TIME, "HH:MM")
         documentos_oe.c_usuario          = USERID('userdb')
         documentos_oe.descripcion        = "FAX DESPACHANTE"
         documentos_oe.fecha_cumplido     = DATE("")
         documentos_oe.fecha_pedido       = DATE("")
         documentos_oe.id_documento_oe    = i
         documentos_oe.id_estado          = 1
         documentos_oe.id_orden_entrega   = piOE
         documentos_oe.id_tipo_documento  = 6
         documentos_oe.observacionies     = ""
         i = i + 1.
         .

  /*POLIZA CAUCION CONSIGNACION*/
  CREATE documentos_oe.
  ASSIGN documentos_oe.c_fecha            = TODAY
         documentos_oe.c_hora             = STRING(TIME, "HH:MM")
         documentos_oe.c_usuario          = USERID('userdb')
         documentos_oe.descripcion        = "POLIZA CAUCION"
         documentos_oe.fecha_cumplido     = DATE("")
         documentos_oe.fecha_pedido       = TODAY
         documentos_oe.id_documento_oe    = i
         documentos_oe.id_estado          = 1
         documentos_oe.id_orden_entrega   = piOE
         documentos_oe.id_tipo_documento  = 7
         documentos_oe.observacionies     = ""
         i = i + 1.
         .
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillGastosOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillGastosOE Procedure 
PROCEDURE fillGastosOE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piOE   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pcRows AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER plAsk  AS LOGICAL    NO-UNDO. /*pregunta si pisa gastos anteriores o no*/

  DEFINE VARIABLE i           AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iOE         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iGasto      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCantPtes   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCont       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cGasto      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dImporte    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fProporcion AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cNro        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAnt        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCon        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRow        AS CHARACTER  NO-UNDO.


  IF piOE = 0 THEN DO:
    RETURN.
  END.

  FOR FIRST gastos_orden_entrega
      WHERE gastos_orden_entrega.id_orden_entrega = piOE
        AND plAsk                                 = TRUE
      NO-LOCK.    

      MESSAGE "Desea Actualizar los Gastos Que ya Existen para esta OE?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChoice AS LOGICAL.
      IF NOT lChoice THEN RETURN.      
    
  END.

  /*calculo cantidad de partes de la oe*/
  FOR EACH items_orden_entrega
      WHERE items_orden_entrega.id_orden_entrega = piOE
      NO-LOCK.
    iCantPtes = iCantPtes + 1.
  END.

  /*cantidad de contenedores*/
  iCont = getContenedoresOE(piOE).

  /*borro gastos si existieran*/
  FOR EACH gastos_orden_entrega
      WHERE gastos_orden_entrega.id_orden_entrega = piOE.
    DELETE gastos_orden_entrega.    
  END.
  
  
  /*creo gastos oe*/
  DO i = 1 TO NUM-ENTRIES(pcRows, CHR(10)).
    cRow = ENTRY(i, pcRows, CHR(10)).
    ASSIGN iOe      = INTEGER(ENTRY(1, cRow, CHR(1)))
           iGasto   = INTEGER(ENTRY(2, cRow, CHR(1)))
           cGasto   = ENTRY(3, cRow, CHR(1))
           dImporte = DECIMAL(ENTRY(4, cRow, CHR(1)))
           cNro     = ENTRY(5, cRow, CHR(1))
           cAnt     = ENTRY(6, cRow, CHR(1))
           cCon     = ENTRY(7, cRow, CHR(1)).

    /*IF dImporte > 0 THEN DO:    */
      CREATE gastos_orden_entrega.
      ASSIGN gastos_orden_entrega.id_orden_entrega = iOE
             gastos_orden_entrega.id_gasto         = iGasto
             gastos_orden_entrega.importe          = dImporte
             .
    /*END.*/
  END.


  /*replico los gastos en los items de la oe proporcionalmente*/
  
  FOR EACH gastos_items_orden_entrega
      WHERE gastos_items_orden_entrega.id_orden_entrega = piOE.
    DELETE gastos_items_orden_entrega.    
  END.
  
  FOR EACH items_orden_entrega 
      WHERE items_orden_entrega.id_orden_entrega = piOE.
    
    /*completo campo items_contratos.precio_origen*/
    FOR FIRST items_contratos
        OF items_orden_entrega 
        NO-LOCK.
      items_orden_entrega.importe_origen = items_contratos.precio_origen.
    END.

    /*calculo proporcion */
    IF items_orden_entrega.contenedores <> 0 THEN 
      fProporcion = items_orden_entrega.contenedores.
    ELSE
      fProporcion = 1 / iCantPtes.

    /*replico estructura de gastos en gastos_items*/
    FOR EACH gastos_orden_entrega 
        WHERE gastos_orden_entrega.id_orden_entrega = piOE.

      CREATE gastos_items_orden_entrega.
      ASSIGN gastos_items_orden_entrega.item_oe          = items_orden_entrega.ITEM_oe
             gastos_items_orden_entrega.importe          = (gastos_orden_entrega.importe * fProporcion) / iCont
             gastos_items_orden_entrega.id_orden_entrega = piOE
             gastos_items_orden_entrega.id_gasto         = gastos_orden_entrega.id_gasto
             .
      
    END.
    
  END.

  RELEASE items_orden_entrega.
  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-recalcKilosItemOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalcKilosItemOE Procedure 
PROCEDURE recalcKilosItemOE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piOE  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piPte AS INTEGER    NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE n AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE b AS DECIMAL    NO-UNDO.

  FOR EACH tambores_industria
      WHERE tambores_industria.id_orden_entrega = piOE
        AND tambores_industria.ITEM_oe          = piPte
      NO-LOCK.
    i = i + 1.
    n = n + tambores_industria.kilos_tambor.
    b = b + (tambores_industria.kilos_tambor + tambores_industria.tara).
  END.

  FOR FIRST items_orden_entrega
      WHERE items_orden_entrega.id_orden_entrega = piOE
        AND items_orden_entrega.ITEM_oe          = piPte.

    ASSIGN items_orden_entrega.cantidad_tambores = i
           items_orden_entrega.kgs_netos         = n
           items_orden_entrega.kgs_brutos        = b.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCantidadTamboresOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setCantidadTamboresOE Procedure 
PROCEDURE setCantidadTamboresOE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piOE AS INTEGER    NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.

  FOR EACH items_orden_entrega 
      WHERE items_orden_entrega.id_orden_entrega = piOE.

    FOR EACH tambores_industria 
          OF items_orden_entrega
        NO-LOCK.
      i = i + 1.
    END.

    ASSIGN items_orden_entrega.cantidad_tambores = i
           i = 0.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSemanaOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSemanaOE Procedure 
PROCEDURE setSemanaOE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piOE  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piSem AS INTEGER    NO-UNDO.

  FOR FIRST items_orden_entrega
      WHERE items_orden_entrega.id_orden_entrega = piOE.

    ASSIGN items_orden_entrega.semana_entrega = piSem.
/*
    FOR FIRST items_contratos 
        OF items_orden_entrega.

      ASSIGN items_contratos.semana_entrega = piSem
             items_contratos.c_usuario      = USERID("userdb")
             items_contratos.c_fecha        = TODAY
             items_contratos.c_hora         = STRING(TIME,"HH:MM:SS").
    END.
*/
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTotalesOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTotalesOE Procedure 
PROCEDURE setTotalesOE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piOE AS INTEGER    NO-UNDO.

  DEFINE VARIABLE dFob AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dDer AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRei AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCoe AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dTip AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCom AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dTot AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dGal AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCnt AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dBru AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dUni AS DECIMAL    NO-UNDO.
  
  DEFINE BUFFER buItems FOR items_orden_entrega.

  RUN setCantidadTamboresOE (piOE). /*hago esto aqui hasta que corrija en la asociacion de tbs a la oe*/

  dTot = getTotalFacturaOE(STRING(piOE)).
  dFob = getFob(piOE).
  dCoe = getCoefAduana().

  FOR EACH  buItems
      WHERE buItems.id_orden_entrega = piOE
      EXCLUSIVE-LOCK.
    
    ASSIGN  buItems.TOTAL_factura                = 0
            buItems.fob_ton                      = 0
            buItems.fob_unitario                 = 0
            buItems.TOTAL_galones                = 0
            buItems.tipo_cambio                  = 0
            buItems.importe_factura_dolar        = 0
            buItems.importe_fob_dolar            = 0
            buItems.importe_comision             = 0
            buItems.coeficiente                  = 0
            buItems.valor_aduana_derechos        = 0
            buItems.valor_aduana_reintegro       = 0
            buItems.importe_derechos_exportacion = 0
            buItems.importe_reintegro_fijo       = 0
            buItems.kgs_netos                    = 0
            buItems.kgs_brutos                   = 0
            .


    IF INTEGER(buItems.id_moneda_cambio) > 0 AND INTEGER(buItems.id_moneda_cambio) <> 2 THEN DO:
      FOR LAST moneda_cotizacion 
          WHERE moneda_cotizaciones.fecha <= TODAY
            AND moneda_cotizaciones.id_moneda_origen = buItems.id_moneda_cambio
          NO-LOCK.
        dTip = moneda_cotizaciones.tipo_cambio.
      END.
    END.
    ELSE dTip = 1.


    ASSIGN  dTot = getTotalFacturaItemOE(buItems.id_orden_entrega, buItems.ITEM_oe)
            dFob = dTot - (getGastosItemOE(buItems.id_orden_entrega, buItems.ITEM_oe)) /*dFob * dCnt*/ /*para proporcionar el fob por si la oe tuviera mas de un item pero en un solo contenedor*/
            dCom = getComisionItemOE(piOE, buItems.ITEM_oe)
            dDer = (dFob * dCoe) * 0.05
            dRei = (dFob * dCoe) * getPorcentajeReintegArti(buItems.id_articulo)
            dGal = getGalonesItemOE(piOE, buItems.ITEM_oe)
            dKil = getKilosItemOE(piOE, buItems.ITEM_oe)
            dBru = getKilosBrutosItemOE(piOE, buItems.ITEM_oe)
            dUni = getFobUnitario(piOe, buItems.ITEM_oe)
            .

    ASSIGN  buItems.TOTAL_factura                = dTot
            buItems.fob_ton                      = dFob
            buItems.fob_unitario                 = dUni
            buItems.TOTAL_galones                = dGal
            buItems.tipo_cambio                  = dTip
            buItems.importe_factura_dolar        = dFob / dTip
            buItems.importe_fob_dolar            = dFob / dTip
            buItems.importe_comision             = dCom
            buItems.coeficiente                  = dCoe
            buItems.valor_aduana_derechos        = dFob * dCoe
            buItems.valor_aduana_reintegro       = IF (dFob - dCom) > (dFob * dCoe) THEN (dFob * dCoe) ELSE (dFob - dCom) 
            buItems.importe_derechos_exportacion = dDer
            buItems.importe_reintegro_fijo       = dRei
            buItems.kgs_netos                    = IF buItems.kgs_netos = 0 THEN dKil ELSE buItems.kgs_netos
            buItems.kgs_brutos                   = IF buItems.kgs_brutos = 0 THEN dBru ELSE buItems.kgs_brutos
            .

    dCom = 0.
    dCnt = 0.
  END.

  RELEASE buItems.

END PROCEDURE.

/*             dCnt = IF buItems.contenedores <> 0 THEN (IF buItems.contenedores > 1 THEN buItems.contenedore - 1 ELSE buItems.contenedores) ELSE (1 / getCantPtesOE(piOE))*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-vincularTambores) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vincularTambores Procedure 
PROCEDURE vincularTambores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phSdo    AS HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER pcRowIds AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER piOE     AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER piPte    AS INTEGER    NO-UNDO.


  DEFINE VARIABLE cFields AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRows   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iRow    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE i       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE hLib    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE k       AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE p       AS DECIMAL    NO-UNDO.


  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p'). 
  DELETE OBJECT hLibCom.

  cRows = pcRowIds.
  i     = NUM-ENTRIES(cRows).

  DO iRow = 1 TO NUM-ENTRIES(cRows) ON ERROR UNDO, LEAVE:
    cFields = DYNAMIC-FUNCTION ('fetchRow' IN phSdo , INTEGER(ENTRY(iRow,cRows)), 'id_empresa,id_sucursal,id_tipotambor,nromov,id_tambor,kilos_tambor,tara').
    IF cFields = ? OR cFields = "" THEN RETURN "Error al obtener los datos del lote".

    k = k + DECIMAL(ENTRY(7, cFields, CHR(1))).
    p = p + (DECIMAL(ENTRY(7, cFields, CHR(1))) + (DECIMAL(ENTRY(8, cFields, CHR(1))))).

    RUN asocTamboresOE IN hLib (INTEGER(ENTRY(2, cFields, CHR(1))),
                                INTEGER(ENTRY(3, cFields, CHR(1))),
                                INTEGER(ENTRY(4, cFields, CHR(1))),
                                INTEGER(ENTRY(5, cFields, CHR(1))),
                                INTEGER(ENTRY(6, cFields, CHR(1))),
                                INTEGER(ENTRY(6, cFields, CHR(1))),
                                piOE, 
                                piPte) .
  END.

  
  calcularOE (piOE, FALSE).
  
  
  IF piOE <> 0 THEN RETURN.
  /*poner fecha en documento_oe asociacion de tambores*/
  FOR FIRST documentos_oe
      WHERE documentos_oe.id_orden_entrega  = piOE
        AND documentos_oe.id_tipo_documento = 4.

    ASSIGN documentos_oe.fecha_cumplido = TODAY.
      
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-calcularOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calcularOE Procedure 
FUNCTION calcularOE RETURNS CHARACTER
  (piOE  AS INTEGER, 
   plAsk AS LOGICAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    
  DEFINE VARIABLE hLib   AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibOE AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cRows  AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN libCommonFunctions.p PERSISTENT SET hLibCom.
  hLib   = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libProceduresReglas.p').
  DELETE OBJECT hLibCom.

  RUN motorInferencia IN hLib ("..\industria\desglose.rul",
                               piOE).

  RUN fillGastosOE (piOE, DYNAMIC-FUNCTION('getEntryttReglas' IN hLib), plAsk).
  RUN setTotalesOE (piOE).

  RETURN cRows.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAgenciaId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAgenciaId Procedure 
FUNCTION getAgenciaId RETURNS INTEGER
  (piOE AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS INTEGER    NO-UNDO.

  FOR FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = piOE
                          NO-LOCK.
    dRet = orden_entrega.id_agencia.
  END.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCantPtesOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCantPtesOE Procedure 
FUNCTION getCantPtesOE RETURNS INTEGER
  (piOE AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRet AS INTEGER    NO-UNDO.

  DEFINE BUFFER buItems FOR items_orden_entrega.

  FOR EACH buItems  
      WHERE buItems.id_orden_entrega = piOE 
      NO-LOCK.
    iRet = iRet + 1.
  END.

  iRet = IF iRet < 0 THEN 1 ELSE iRet.

  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCoefAduana) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCoefAduana Procedure 
FUNCTION getCoefAduana RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.

  FOR FIRST coeficientes_aduana.
    dRet = coeficientes_aduana.coeficiente.
  END.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getComisionItemOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getComisionItemOE Procedure 
FUNCTION getComisionItemOE RETURNS DECIMAL
  (piOE AS INTEGER,
   piIt AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dPje AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.

  dPje = 100.
  FOR FIRST items_orden_entrega
      WHERE items_orden_entrega.id_orden_entrega = piOE
        AND items_orden_entrega.ITEM_oe          = piIt
      NO-LOCK.

    FOR FIRST items_contratos 
           OF items_orden_entrega
        NO-LOCK.
      
      dPje = IF items_contratos.comision_broker <> 0 THEN items_contratos.comision_broker ELSE 100.
      
      CASE items_contratos.id_tipo_unidad_venta:
        WHEN 1 THEN dRet = items_contratos.precio_comision * (((getKilosItemOE(piOE, piIt)) / 1000) * (dPje / 100)).
        WHEN 2 THEN dRet = items_contratos.precio_comision * ((getKilosItemOE(piOE, piIt)) * (dPje / 100)). 
        WHEN 3 THEN dRet = items_contratos.precio_comision * ((getGalonesItemOE(piOE, piIt)) * (dPje / 100)).
        WHEN 4 THEN dRet = items_contratos.precio_comision * (((getKilosItemOE(piOE, piIt)) * 2.20462) * (dPje / 100)).
      END CASE.
    END.

  END.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getContenedoresOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getContenedoresOE Procedure 
FUNCTION getContenedoresOE RETURNS DECIMAL
  (piOE AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.

  FOR EACH items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = piOE
                               NO-LOCK.
    dRet = dRet + items_orden_entrega.contenedores.
  END.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFob) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFob Procedure 
FUNCTION getFob RETURNS DECIMAL
  (piOE AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dFob AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dTot AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dGas AS DECIMAL    NO-UNDO.

  dGas = getTotalGastosOE(piOE).
  dTot = getTotalFacturaOE(STRING(piOE)).
  dFob = dTot - dGas.

  RETURN dFob.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFobUnitario) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFobUnitario Procedure 
FUNCTION getFobUnitario RETURNS DECIMAL
  (piOE AS INTEGER, 
   piIt AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dFob AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCom AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fGal AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fPrp AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cDat AS CHARACTER  NO-UNDO.

  dFob = getFob(piOE).

  
  FOR FIRST items_orden_entrega 
      WHERE items_orden_entrega.id_orden_entrega = piOE
        AND items_orden_entrega.ITEM_oe          = piIt
      NO-LOCK.

    dCom = getComisionItemOE(piOE, piIt).
    fPrp = IF items_orden_entrega.contenedores < 1 THEN items_orden_entrega.contenedores  ELSE (1 / getCantPtesOE(piOE)).
    dFob = dFob * fPrp.

    cDat = DYNAMIC-FUNCTION('getUnidadPrecioContrato', items_orden_entrega.id_contrato, 
                                                       items_orden_entrega.id_tipo_contrato,
                                                       items_orden_entrega.anio,
                                                       items_orden_entrega.ITEM).

    IF cDat = ?  THEN RETURN 0.00.

    CASE INTEGER(ENTRY(1, cDat, CHR(1))):
      WHEN 1 THEN dRet = (dFob - dCom) / (items_orden_entrega.kgs_netos_tambores / 1000).
      WHEN 2 THEN dRet = (dFob - dCom) / items_orden_entrega.kgs_netos_tambores.
      WHEN 3 THEN dRet = (dFob - dCom) / (getGalonesItemOE(piOE, piIt)).
      WHEN 4 THEN dRet = (dFob - dCom) / (items_orden_entrega.kgs_netos_tambores / 1000).
    END CASE.

  END.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFobUniTon) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFobUniTon Procedure 
FUNCTION getFobUniTon RETURNS DECIMAL
  (piOE AS INTEGER, 
   piIt AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dFob AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dCom AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fPrp AS DECIMAL    NO-UNDO.

  dFob = getFob(piOE).

  FOR FIRST items_orden_entrega 
      WHERE items_orden_entrega.id_orden_entrega = piOE
        AND items_orden_entrega.ITEM_oe          = piIt
      NO-LOCK.

    fPrp = IF items_orden_entrega.contenedores < 1 THEN items_orden_entrega.contenedores  ELSE (1 / getCantPtesOE(piOE)).
    dFob = dFob * fPrp.

    dKil = getKilosItemOE(piOE, piIt).
    dCom = getComisionItemOE(piOE, piIt).

    dRet = (dFob - dCom) / (dKil / 1000).

  END.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getGalonesItemOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getGalonesItemOE Procedure 
FUNCTION getGalonesItemOE RETURNS DECIMAL
  (piOE AS INTEGER,
   piIt AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
-----------------------------------------------------------------------------*/
  DEFINE VARIABLE fGal    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fKil    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fRet    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fLit    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fBrx    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cAnl    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hLibTam AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hLibRep AS HANDLE     NO-UNDO.

  DEFINE VARIABLE hLibCom AS HANDLE.
  RUN ..\industria\libCommonFunctions.p PERSISTENT SET hLibCom.
  hLibTam = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
  hLibRep = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libReportes.p').
  DELETE OBJECT hLibCom.

  FOR EACH tambores_industria
      WHERE tambores_industria.id_orden_entrega = piOE
        AND tambores_industria.ITEM_oe          = piIt
      BREAK BY tambores_industria.nromov.
    
    fKil = fKil + tambores_industria.kilos_tambor.
    
    IF LAST-OF(tambores_industria.nromov) THEN DO:
      cAnl = DYNAMIC-FUNCTION('getValoresAnalisis' IN hLibRep, tambores_industria.id_empresa,
                                                               tambores_industria.id_sucursal,
                                                               tambores_industria.id_tipotambor,
                                                               tambores_industria.nromov).
      IF cAnl <> ? AND cAnl <> "" AND cAnl <> "Aceite" THEN
        fBrx = DECIMAL(ENTRY(4, cAnl, CHR(1))).
      ELSE
        RETURN 0.00.
    
      fLit = fKil / DYNAMIC-FUNCTION('getCoefPesoEspecifico' IN hLibTam, fBrx).
    
      fGal = fLit / 3.785.

      fRet = fRet + fGal.

      fGal = 0.
      fLit = 0.
      fBrx = 0.
      fKil = 0.
    END.
  END.
  

  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getGalonesOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getGalonesOE Procedure 
FUNCTION getGalonesOE RETURNS DECIMAL
  (piOE AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet    AS DECIMAL    NO-UNDO.


  FOR EACH items_orden_entrega 
      WHERE items_orden_entrega.id_orden_entrega = piOE
      NO-LOCK.
    dRet = dRet + getGalonesItemOE(piOE, items_orden_entrega.ITEM_oe).
  END.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getGastoAgencia) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getGastoAgencia Procedure 
FUNCTION getGastoAgencia RETURNS DECIMAL
  (piAgencia AS INTEGER, 
   piGasto   AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.

  FOR FIRST gastos_agencias WHERE gastos_agencias.id_agencia = piAgencia
                              AND gastos_agencias.id_gasto   = piGasto
                           NO-LOCK .
    dRet = gastos_agencias.importe.
  END.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getGastoIks) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getGastoIks Procedure 
FUNCTION getGastoIks RETURNS DECIMAL
  (piOE AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dIks AS DECIMAL    NO-UNDO.

  FOR EACH items_orden_entrega WHERE items_orden_entrega.id_orden_entrega   = piOE
                                 AND items_orden_entrega.id_condicion_venta = 22
                               NO-LOCK.
    FIND FIRST gastos_items_orden_entrega OF items_orden_entrega
                                          WHERE gastos_items_orden_entrega.id_gasto = 13 
                                          NO-LOCK NO-ERROR.
    IF AVAILABLE gastos_items_orden_entrega THEN DO:
      dIks = dIks + gastos_items_orden_entrega.importe.
    END.
  END.

  RETURN dIks.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getGastosItemOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getGastosItemOE Procedure 
FUNCTION getGastosItemOE RETURNS DECIMAL
  (piOE AS INTEGER,
   piIt AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  FOR EACH gastos_items_orden_entrega
      WHERE gastos_items_orden_entrega.id_orden_entrega = piOE
        AND gastos_items_orden_entrega.ITEM_oe          = piIt
      NO-LOCK.
    fRet = fRet + gastos_items_orden_entrega.importe.
  END.


  RETURN fRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosBrutosItemOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilosBrutosItemOE Procedure 
FUNCTION getKilosBrutosItemOE RETURNS DECIMAL
  (piOE AS INTEGER,
   piIt AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
-----------------------------------------------------------------------------*/
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  FOR EACH tambores_industria
      WHERE tambores_industria.id_orden_entrega = piOE
        AND tambores_industria.ITEM_oe          = piIt
      NO-LOCK.
    fKil = fKil + (tambores_industria.kilos_tambor + tambores_industria.tara).
  END.

  /*cascara*/
  FOR EACH r_lote_cascara_oe
      WHERE r_lote_cascara_oe.id_orden_entrega = piOE
        AND r_lote_cascara_oe.id_lote          = piIt
      NO-LOCK.
    fKil = fKil + (r_lote_cascara_oe.cantidad * 51). /*asumo que 1 bolsa cascara pesa 1 kg*/
  END.
  

  RETURN fKil.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosBrutosOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilosBrutosOE Procedure 
FUNCTION getKilosBrutosOE RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  tambores chr(1) kilos chr(1) kilos_brutos
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.

  DEFINE BUFFER buTam FOR tambores_industria.

  FOR EACH buTam WHERE buTam.id_orden_entrega = INTEGER(pcArgs)
                 NO-LOCK.
    k = k + (buTam.kilos_tambor + buTam.tara).
  END.


  RETURN k.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosItemOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilosItemOE Procedure 
FUNCTION getKilosItemOE RETURNS DECIMAL
  (piOE AS INTEGER,
   piIt AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
-----------------------------------------------------------------------------*/
  DEFINE VARIABLE fKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  /*tambores*/
  FOR EACH tambores_industria
      WHERE tambores_industria.id_orden_entrega = piOE
        AND tambores_industria.ITEM_oe          = piIt
      NO-LOCK.
    fKil = fKil + tambores_industria.kilos_tambor.
  END.

  /*cascara*/
  FOR EACH r_lote_cascara_oe
      WHERE r_lote_cascara_oe.id_orden_entrega = piOE
        AND r_lote_cascara_oe.id_lote          = piIt
      NO-LOCK.
    fKil = fKil + (r_lote_cascara_oe.cantidad * 50).
  END.
  

  RETURN fKil.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKilosOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKilosOE Procedure 
FUNCTION getKilosOE RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  tambores chr(1) kilos chr(1) kilos_brutos
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.

  DEFINE BUFFER buTam FOR tambores_industria.

  FOR EACH buTam WHERE buTam.id_orden_entrega = INTEGER(pcArgs)
                 NO-LOCK.
    k = k + buTam.kilos_tambor.
  END.

  RETURN k.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNextItemOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextItemOE Procedure 
FUNCTION getNextItemOE RETURNS INTEGER
  (piOE AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRet AS INTEGER    NO-UNDO.

  iRet = 1.
  FOR EACH items_orden_entrega 
      WHERE items_orden_entrega.id_orden_entrega = piOE
      BY items_orden_entrega.ITEM_oe DESC.

    iRet = items_orden_entrega.ITEM_oe + 1.
    LEAVE.
  END.

  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPorcentajeReintegArti) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPorcentajeReintegArti Procedure 
FUNCTION getPorcentajeReintegArti RETURNS DECIMAL
  (piArt AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.

  FIND LAST porcentaje_reint_articulo 
       WHERE porcentaje_reint_articulo.id_articulo = piArt
       NO-LOCK NO-ERROR.
  IF AVAILABLE porcentaje_reint_articulo THEN DO:
    dRet = porcentaje_reint_articulo.porcentaje / 100.
  END.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPrecio) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPrecio Procedure 
FUNCTION getPrecio RETURNS DECIMAL
  (piTip AS INTEGER, 
   pdKil AS DECIMAL, 
   pdPre AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dKil AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.


  CASE piTip:
    WHEN 1 THEN /* TONELADAS */ 
      dKil = pdKil / 1000.

    WHEN 2 THEN /* KILOS */
      dKil = pdKil.

    WHEN 3 THEN /* GALONES pdKil trae galones no kilos*/ 
      dKil = pdKil.
 
    WHEN 4 THEN /* LIBRAS */ 
      dKil = pdKil * 2.20462.
 
  END CASE. 

  dRet = dKil * pdPre.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTamboresItemOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTamboresItemOE Procedure 
FUNCTION getTamboresItemOE RETURNS INTEGER
  (piOE AS INTEGER,
   piIt AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  para belen  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iRet AS INTEGER    NO-UNDO.

  FOR EACH tambores_industria
      WHERE tambores_industria.id_orden_entrega = piOE
        AND tambores_industria.ITEM_oe          = piIt
      NO-LOCK.
    iRet = iRet + 1.
  END.
  


  RETURN iRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTamboresKilosOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTamboresKilosOE Procedure 
FUNCTION getTamboresKilosOE RETURNS CHARACTER
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  tambores chr(1) kilos chr(1) kilos_brutos
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iOe  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iItm AS INTEGER    NO-UNDO.

  DEFINE VARIABLE i AS INTEGER    NO-UNDO.
  DEFINE VARIABLE k AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE t AS DECIMAL    NO-UNDO.

  DEFINE BUFFER buTam FOR tambores_industria.

  FOR EACH buTam WHERE buTam.id_orden_entrega = INTEGER(pcArgs)
                 NO-LOCK.
    i = i + 1.
    k = k + buTam.kilos_tambor.
    t = t + buTam.kilos_tambor + buTam.tara.
  END.

  RETURN STRING(i) + CHR(1) + STRING(k) + CHR(1) + STRING(t).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTotalFacturaItemOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTotalFacturaItemOE Procedure 
FUNCTION getTotalFacturaItemOE RETURNS DECIMAL
  (piOE AS INTEGER,
   piIt AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE p    AS INTEGER    NO-UNDO.  
  DEFINE VARIABLE j    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE q    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.

  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fSub AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cPre AS CHARACTER  NO-UNDO.



  FOR FIRST items_orden_entrega 
      WHERE items_orden_entrega.id_orden_entrega = piOE
        AND items_orden_entrega.ITEM_oe          = piIt
      NO-LOCK.

    cPre = getUnidadPrecioContrato(items_orden_entrega.id_contrato,
                                   items_orden_entrega.id_tipo_contrato,
                                   items_orden_entrega.anio,
                                   items_orden_entrega.ITEM).

    IF cPre <> "" AND cPre <> ? THEN
      ASSIGN p = INTEGER(ENTRY(1, cPre, CHR(1)))  /*tipo unidad*/
             q = DECIMAL(ENTRY(2, cPre, CHR(1))). /*precio*/
    ELSE
      ASSIGN p = 0
             q = 0.

    IF p = 3 THEN /*galones*/
      j = getGalonesItemOE(items_orden_entrega.id_orden_entrega, 
                           items_orden_entrega.ITEM_oe).
    ELSE
      j= getKilosItemOE(items_orden_entrega.id_orden_entrega,
                        items_orden_entrega.ITEM_oe).

    fRet = getPrecio(p, j, q).              

  END.

 RETURN fRet.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTotalFacturaOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTotalFacturaOE Procedure 
FUNCTION getTotalFacturaOE RETURNS DECIMAL
  (pcArgs AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE fRet AS DECIMAL    NO-UNDO.
  


  FOR EACH items_orden_entrega
      WHERE items_orden_entrega.id_orden_entrega = INTEGER(pcArgs)
      NO-LOCK.

    fRet = fRet + getTotalFacturaItemOE(items_orden_entrega.id_orden_entrega, 
                                        items_orden_entrega.ITEM_oe).
  END.

  RETURN fRet.

  
END FUNCTION.


/*
  DEFINE VARIABLE p    AS INTEGER    NO-UNDO.  
  DEFINE VARIABLE j    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE k    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE q    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE fSub AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cPre AS CHARACTER  NO-UNDO.


  FOR EACH items_orden_entrega 
      WHERE items_orden_entrega.id_orden_entrega = INTEGER(pcArgs)
      NO-LOCK.

    cPre = getUnidadPrecioContrato(items_orden_entrega.id_contrato,
                                   items_orden_entrega.id_tipo_contrato,
                                   items_orden_entrega.anio,
                                   items_orden_entrega.ITEM).

    IF cPre <> "" AND cPre <> ? THEN
      ASSIGN p = INTEGER(ENTRY(1, cPre, CHR(1)))  /*tipo unidad*/
             q = DECIMAL(ENTRY(2, cPre, CHR(1))). /*precio*/
    ELSE
      ASSIGN p = 0
             q = 0.

    IF p = 3 THEN /*galones*/
      j = getGalonesItemOE(items_orden_entrega.id_orden_entrega, 
                           items_orden_entrega.ITEM_oe).
    ELSE
      j= getKilosItemOE(items_orden_entrega.id_orden_entrega,
                        items_orden_entrega.ITEM_oe).

    fSub = fSub + getPrecio(p, j, q).              

    j = 0.
  END.

 RETURN fSub.
*/



/*


  /*dIks = getGastoIks(INTEGER(pcArgs)).*/ /*by facundo 09/06/2006 todavia no tengo claro porque deberia restarse aqui el gasto iks*/
  
  dIks = 0.

  FOR FIRST orden_entrega WHERE orden_entrega.id_orden_entrega = INTEGER(pcArgs)
                          NO-LOCK.
    FOR EACH items_orden_entrega OF orden_entrega NO-LOCK.
      FOR EACH tambores_industria OF items_orden_entrega
                                  BREAK BY tambores_industria.nromov.
        i = i + 1.
        j = j + tambores_industria.kilos_tambor.
        k = k + tambores_industria.kilos_tambor + tambores_industria.tara.
        IF LAST-OF(tambores_industria.nromov) THEN DO:
          
          cPre = getUnidadPrecioContrato(items_orden_entrega.id_contrato,
                                         items_orden_entrega.id_tipo_contrato,
                                         items_orden_entrega.anio,
                                         items_orden_entrega.ITEM).
          IF cPre <> "" AND cPre <> ? THEN
            ASSIGN p = INTEGER(ENTRY(1, cPre, CHR(1)))  /*tipo unidad*/
                   q = DECIMAL(ENTRY(2, cPre, CHR(1))). /*precio*/
          ELSE
            ASSIGN p = 0
                   q = 0.
          
          IF p = 3 THEN /*galones*/
            j = fGal * items_orden_entrega.contenedores.

          
          fSub = fSub + getPrecio(p, j, q).              
          
          i = 0.
          j = 0.
          k = 0.
        END. /*if last-of(tambores_industria.nromov) ...*/       
      END. /*for each tambores_industria ...*/
      dTot = dTot + fSub.
    END. /*for each items_orden_entrega ...*/    
  END. /*for first orden_entrega ...*/
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTotalGastosOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTotalGastosOE Procedure 
FUNCTION getTotalGastosOE RETURNS DECIMAL
  (piOE AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dRet AS DECIMAL    NO-UNDO.

  FOR EACH gastos_orden_entrega
      WHERE gastos_orden_entrega.id_orden_entrega = piOE
      NO-LOCK.
    dRet = dRet + gastos_orden_entrega.importe.
  END.

  RETURN dRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUnidadPrecioContrato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUnidadPrecioContrato Procedure 
FUNCTION getUnidadPrecioContrato RETURNS CHARACTER
  (pcCon AS CHARACTER,
   piTip AS INTEGER,
   piAno AS INTEGER,
   piItm AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FOR FIRST items_contratos WHERE items_contrato.id_contrato        = pcCon
                              AND items_contratos.ITEM              = piItm
                              AND items_contratos.anio              = piAno
                              AND items_contratos.id_tipo_contrato  = piTip
                            NO-LOCK.
    IF items_contratos.id_articulo <> 54 THEN
      cRet = STRING(items_contratos.id_tipo_unidad_venta_origen) + CHR(1) + STRING(items_contratos.precio_origen).
    ELSE
      cRet = STRING(1) + CHR(1) + STRING(items_contratos.precio_origen).
  END.

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-puedeModificarOE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION puedeModificarOE Procedure 
FUNCTION puedeModificarOE RETURNS LOGICAL
  (piOE  AS INTEGER,
   piPte AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lRet AS LOGICAL INITIAL TRUE   NO-UNDO.

  /* control estado oe */
  FOR FIRST items_orden_entrega
      WHERE items_orden_entrega.id_orden_entrega = piOE
        AND items_orden_entrega.ITEM_oe          = piPte
        AND items_orden_entrega.id_estado       >= 3.
    
    MESSAGE "Los tambores de esta OE ya no pueden ser modificados!." SKIP "La Documantacion correspondiente ya fue generada."
      VIEW-AS ALERT-BOX WARNING BUTTONS OK.
    lRet = FALSE.
  END.

  RETURN lRet.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

