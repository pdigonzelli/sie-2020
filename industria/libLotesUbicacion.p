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

&IF DEFINED(EXCLUDE-getTipoMovimiento) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTipoMovimiento Procedure 
FUNCTION getTipoMovimiento RETURNS INTEGER
  ( INPUT piSucursalOrigen AS INTEGER, INPUT piSucursalDestino AS INTEGER)  FORWARD.

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
         HEIGHT             = 19
         WIDTH              = 61.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-actualizaTamboresDesdeHasta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE actualizaTamboresDesdeHasta Procedure 
PROCEDURE actualizaTamboresDesdeHasta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER  piEmpresa            AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursal           AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piTipoTambor         AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piNroMov             AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursalUbicacion  AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursalDestino    AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piDesde              AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piHasta              AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piCantidad           AS INTEGER NO-UNDO.


 DO TRANSACTION ON ERROR UNDO , RETURN "Error de Actulizacion de Tambores por Cantidad":

     FOR EACH tambores_industria  WHERE tambores_industria.id_empresa               = piEmpresa 
                                   AND tambores_industria.id_sucursal               = piSucursal 
                                   AND tambores_industria.id_tipotambor             = piTipoTambor 
                                   AND tambores_industria.nromov                    = piNroMov 
                                   AND tambores_industria.id_sucursal_ubicacion     = piSucursalUbicacion 
                                   BY tambores_industria.id_tambor.

        IF tambores_industria.id_tambor >= piDesde AND 
           tambores_industria.id_tambor <= piHasta THEN
            ASSIGN tambores_industria.id_sucursal_ubicacion = piSucursalDestino.
    END.

    IF pihasta - pidesde + 1 < piCantidad THEN
        UNDO , RETURN " No existe esa cantidad de tambores para mover".
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-actualizaTamboresPorCantidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE actualizaTamboresPorCantidad Procedure 
PROCEDURE actualizaTamboresPorCantidad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER  piEmpresa            AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursal           AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piTipoTambor         AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piNroMov             AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursalUbicacion  AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursalDestino    AS INTEGER NO-UNDO.
DEFINE OUTPUT       PARAMETER  piDesde              AS INTEGER NO-UNDO.
DEFINE OUTPUT       PARAMETER  piHasta              AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piCantidad           AS INTEGER NO-UNDO.
DEFINE OUTPUT       PARAMETER  pcLista              AS CHARACTER NO-UNDO.


DEFINE VAR viCantidad   AS INTEGER NO-UNDO INITIAL 0.
DEFINE VAR viDesde      AS INTEGER INITIAL 1 NO-UNDO.
DEFINE VAR viHasta      AS INTEGER NO-UNDO.
DEFINE VAR vLista       AS CHARACTER NO-UNDO.
DEFINE VAR fRango       AS LOGICAL NO-UNDO INITIAL FALSE.



  DO TRANSACTION ON ERROR UNDO , RETURN "Error de Actulizacion de Tambores por Cantidad":
    FOR EACH tambores_industria  WHERE tambores_industria.id_empresa            = piEmpresa 
                                   AND tambores_industria.id_sucursal           = piSucursal 
                                   AND tambores_industria.id_tipotambor         = piTipoTambor 
                                   AND tambores_industria.nromov                = piNroMov 
                                   AND tambores_industria.id_sucursal_ubicacion = piSucursalUbicacion 
                                   BY tambores_industria.id_tambor.
      
      IF viCantidad = 0 THEN viDesde = tambores_industria.id_tambor.      
      viCantidad = viCantidad + 1.

      IF viCantidad > piCantidad THEN
      DO:
        viCantidad = viCantidad - 1.
        LEAVE.
      END.

      IF NOT fRango  THEN
      DO:
         vLista = vLista + string(viDesde) + CHR(14).
         fRango = TRUE.
      END.
      ELSE
      DO:
        IF vihasta + 1 <> tambores_industria.id_tambor THEN
        DO:
          vLista = vLista + STRING(viHasta) + ",".
          viDesde = tambores_industria.id_tambor.
          fRango = FALSE.
        END.
      END.
      
      viHasta = tambores_industria.id_tambor.           
      ASSIGN tambores_industria.id_sucursal_ubicacion = piSucursalDestino.      
      /*
      viDesde = IF tambores_industria.id_tambor < videsde THEN tambores_industria.id_tambor ELSE viDesde.
      viHasta = IF tambores_industria.id_tambor > vihasta THEN tambores_industria.id_tambor ELSE viHasta.            ç
      */
    END.

    IF viCantidad <> piCantidad THEN
      UNDO , RETURN " No existe esa cantidad de tambores para mover " + STRING(viDesde) + " " + STRING(viHasta) + " " + STRING(piCantidad) + " " + STRING(viHasta - viDesde + 1).

    IF fRango THEN 
        vLista = vLista + STRING(viHasta) + ",".
    pcLista = SUBSTRING(vLista,1,LENGTH(vLista) - 1).
    
    piDesde = viDesde.
    piHasta = vihasta.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BasicTransferenciaLoteUbicacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BasicTransferenciaLoteUbicacion Procedure 
PROCEDURE BasicTransferenciaLoteUbicacion PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER  piEmpresa            AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursal           AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piTipoTambor         AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piNroMov             AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursalUbicacion  AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursalDestino    AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  pifecha              AS DATE    NO-UNDO.
DEFINE INPUT        PARAMETER  piCantidad           AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piTipoMovimiento     AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  plActualizaTambor    AS LOGICAL NO-UNDO.


DEFINE BUFFER lotes_ubi     FOR lotes_ubicacion.
DEFINE BUFFER lotes_ubi1    FOR lotes_ubicacion.
 
DEFINE VAR    viDesde           AS INTEGER NO-UNDO INITIAL 99999999.
DEFINE VAR    viHasta           AS INTEGER NO-UNDO.
DEFINE VAR    viTipoMovimiento  AS INTEGER NO-UNDO.  
DEFINE VAR    viCantidad        AS INTEGER NO-UNDO.
    
viTipoMovimiento = piTipoMovimiento.

/* SE VUELVE A REBISAR QUE EL LOTE EXISTA EN LA SUCURSAL ORIGEN */
RUN getLoteUbicacion (piEmpresa , 
                      piSucursal , 
                      piTipoTambor , 
                      piNroMov , 
                      piSucursalUbicacion , 
                      BUFFER lotes_ubi) NO-ERROR.  

IF RETURN-VALUE <> "" THEN RETURN "No Existe el Lote Origen".

IF lotes_ubi.cantidad < piCantidad  THEN RETURN "La cantidad a transferir es mayor al saldo existente".

DO TRANSACTION ON ERROR UNDO , RETURN "Error en Basic Transaction":
    /* ME FIJO SI EXISTE EL LOTE UBICACION EN LA SUCURSAL DESTINO */
    RUN getLoteUbicacion (piEmpresa , 
                          piSucursal , 
                          piTipoTambor , 
                          piNroMov , 
                          piSucursalDestino, 
                          BUFFER lotes_ubi1) NO-ERROR.

    IF RETURN-VALUE <> "" THEN DO:
        /* SINO EXISTE EL LOTE UBICACION EN LA SUCURSAL DESTINO LO CREO */
        RUN createLoteUbicacion (piEmpresa , 
                                 piSucursal , 
                                 piTipoTambor , 
                                 piNroMov , 
                                 piSucursalDestino , 
                                 BUFFER lotes_ubi1) NO-ERROR.
        IF RETURN-VALUE <> "" THEN
            UNDO , RETURN "Error de Creacion de Lote Destino".
        BUFFER-COPY lotes_ubi EXCEPT lotes_ubi.id_sucursal_ubicacion lotes_ubi.cantidad TO lotes_ubi1.
    END.

    CREATE  movimientos_lote_ubicacion.
    ASSIGN  movimientos_lote_ubicacion.id_empresa                       = lotes_ubi.id_empresa
            movimientos_lote_ubicacion.id_sucursal                      = lotes_ubi.id_sucursal
            movimientos_lote_ubicacion.id_tipotambor                    = lotes_ubi.id_tipotambor
            movimientos_lote_ubicacion.nromov                           = lotes_ubi.nromov
            movimientos_lote_ubicacion.id_sucursal_ubicacion            = lotes_ubi.id_sucursal_ubicacion
            movimientos_lote_ubicacion.cantidad                         = -1 * picantidad   
            movimientos_lote_ubicacion.fecha                            = piFecha
            movimientos_lote_ubicacion.id_empresa_destino               = lotes_ubi1.id_empresa
            movimientos_lote_ubicacion.id_sucursal_destino              = lotes_ubi1.id_sucursal
            movimientos_lote_ubicacion.id_tipotambor_destino            = lotes_ubi1.id_tipotambor
            movimientos_lote_ubicacion.nromov_destino                   = lotes_ubi1.nromov
            movimientos_lote_ubicacion.id_sucursal_ubicacion_destino    = lotes_ubi1.id_sucursal_ubicacion
            movimientos_lote_ubicacion.serial                           = NEXT-VALUE(seq-movimiento).

    CREATE  movimientos_lote_ubicacion.
    ASSIGN  movimientos_lote_ubicacion.id_empresa                       = lotes_ubi1.id_empresa
            movimientos_lote_ubicacion.id_sucursal                      = lotes_ubi1.id_sucursal
            movimientos_lote_ubicacion.id_tipotambor                    = lotes_ubi1.id_tipotambor
            movimientos_lote_ubicacion.nromov                           = lotes_ubi1.nromov
            movimientos_lote_ubicacion.id_sucursal_ubicacion            = lotes_ubi1.id_sucursal_ubicacion
            movimientos_lote_ubicacion.cantidad                         = picantidad   
            movimientos_lote_ubicacion.fecha                            = piFecha
            movimientos_lote_ubicacion.id_empresa_destino               = lotes_ubi.id_empresa
            movimientos_lote_ubicacion.id_sucursal_destino              = lotes_ubi.id_sucursal
            movimientos_lote_ubicacion.id_tipotambor_destino            = lotes_ubi.id_tipotambor
            movimientos_lote_ubicacion.nromov_destino                   = lotes_ubi.nromov
            movimientos_lote_ubicacion.id_sucursal_ubicacion_destino    = lotes_ubi.id_sucursal_ubicacion
            movimientos_lote_ubicacion.serial                           = NEXT-VALUE(seq-movimiento).

    FIND CURRENT lotes_ubi EXCLUSIVE-LOCK.
    FIND CURRENT lotes_ubi1 EXCLUSIVE-LOCK.
    ASSIGN lotes_ubi.cantidad   = lotes_ubi.cantidad  - piCantidad
           lotes_ubi1.cantidad  = lotes_ubi1.cantidad + piCantidad.
    FIND CURRENT lotes_ubi1 NO-LOCK.
    IF lotes_ubi.cantidad <= 0  THEN
        DELETE lotes_ubi.
    ELSE
        FIND CURRENT lotes_ubi NO-LOCK.

END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-completaLotesUbicacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE completaLotesUbicacion Procedure 
PROCEDURE completaLotesUbicacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*reocorre los registros de lotes_ubicacion para grabar el campo id_lote para
    que funcione el smartfilter en wIngresoLoteUbicacion.w*/
  
    
  FOR EACH lotes_ubicacion.
    FIND FIRST tambores_industria OF lotes_ubicacion  NO-ERROR.
    IF AVAILABLE tambores_industria THEN DO:
      ASSIGN lotes_ubicacion.id_lote = tambores_industria.id_lote.


    END.
    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-confirmacionRelease) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE confirmacionRelease Procedure 
PROCEDURE confirmacionRelease :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER  piEmpresa            AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursal           AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piTipoTambor         AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piNroMov             AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursalUbicacion  AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursalDestino    AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  pifecha              AS DATE    NO-UNDO.
DEFINE INPUT        PARAMETER  piCantidad           AS INTEGER NO-UNDO.

DEFINE BUFFER lotes_ubi     FOR lotes_ubicacion.
DEFINE BUFFER lotes_ubi_comp     FOR lotes_ubicacion.
DEFINE VARIABLE iDesde AS INTEGER    NO-UNDO.
DEFINE VARIABLE iHasta AS INTEGER    NO-UNDO.

RUN getLoteUbicacion (piEmpresa , 
                      piSucursal , 
                      piTipoTambor , 
                      piNroMov , 
                      piSucursalUbicacion , 
                      BUFFER lotes_ubi) NO-ERROR.

DEBUGGER:INITIATE().
DEBUGGER:SET-BREAK().
IF RETURN-VALUE <> "" THEN
   RETURN "No Existe el Lote Origen".

DO TRANSACTION ON ERROR UNDO , RETURN "Error de Confirmacion de Release":
    
    FIND FIRST lotes_ubi_comp WHERE lotes_ubi_comp.id_empresa               = lotes_ubi.id_empresa
                                AND lotes_ubi_comp.id_sucursal              = lotes_ubi.id_sucursal
                                AND lotes_ubi_comp.id_tipotambor            = lotes_ubi.id_tipotambor
                                AND lotes_ubi_comp.nromov                   = lotes_ubi.nromov
                                AND lotes_ubi_comp.id_sucursal_ubicacion    = lotes_ubi.id_sucursal_ubicacion
                                NO-ERROR.
    IF AVAILABLE lotes_ubi_comp THEN DO: 
        ASSIGN lotes_ubi_comp.cantidad_comprometida = lotes_ubi_comp.cantidad_comprometida - piCantidad.
    END.
    ELSE RETURN "No encontro el lote para grabar cantidad comprometida".


    RUN transferenciaLoteUbicacion(piEmpresa , 
                                   piSucursal , 
                                   piTipoTambor , 
                                   piNroMov , 
                                   piSucursalUbicacion , 
                                   182 , 
                                   piFecha , 
                                   piCantidad , 
                                   20 , 
                                   TRUE ) NO-ERROR.
    IF RETURN-VALUE <> "" THEN
        UNDO , RETURN RETURN-VALUE.
END.

RETURN "".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createFromClaveLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createFromClaveLote Procedure 
PROCEDURE createFromClaveLote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pEmpresa AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pSucursal AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pTipotambor AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pNromov AS INTEGER NO-UNDO.

DEFINE BUFFER lotes_ubi FOR lotes_ubicacion.


FIND FIRST lotes_jugo WHERE lotes_jugo.id_empresa     = pEmpresa
                        AND lotes_jugo.id_sucursal    = pSucursal
                        AND lotes_jugo.id_tipotambor  = pTipotambor
                        AND lotes_jugo.nromov         = pNromov
                        NO-LOCK NO-ERROR.

IF NOT AVAILABLE lotes_jugo THEN DO:

    FIND FIRST lotes_aceite WHERE lotes_aceite.id_empresa     = pEmpresa
                              AND lotes_aceite.id_sucursal    = pSucursal
                              AND lotes_aceite.id_tipotambor  = pTipotambor
                              AND lotes_aceite.nromov         = pNromov
                              NO-LOCK NO-ERROR.
    IF NOT AVAILABLE lotes_aceite  THEN
        RETURN "Lote Origen Inexistente".
    
    FOR EACH tambores_industria OF lotes_aceite WHERE tambores_industria.id_locacion_ubicacion <> 10 NO-LOCK 
        BREAK BY tambores_industria.id_sucursal_ubicacion.
        ACCUM tambores_industria.id_lote (COUNT BY tambores_industria.id_sucursal_ubicacion).

        IF LAST-OF(tambores_industria.id_sucursal_ubicacion) THEN
        DO:
            RUN createLoteUbicacion (lotes_aceite.id_empresa , lotes_aceite.id_sucursal , 
                                     lotes_aceite.id_tipotambor , lotes_aceite.nromov , tambores_industria.id_sucursal_ubicacion ,
                                     BUFFER lotes_ubi).
            IF RETURN-VALUE <> "" THEN
                UNDO , RETURN "Error de Creacion - Lote de Aceite".
            BUFFER-COPY lotes_aceite TO lotes_ubi.
            lotes_ubi.cantidad = ACCUM SUB-COUNT BY tambores_industria.id_sucursal_ubicacion tambores_industria.id_lote.
            lotes_ubi.lote     = string(lotes_aceite.id_lote,"999999") + STRING(lotes_aceite.anio,"9999").
            /*by facundo - cambie find of por find first ... where */
            FIND calidades WHERE calidades.id_calidad = tambores_industria.id_calidad NO-LOCK NO-ERROR.
            FIND envases_prod OF tambores_industria     NO-LOCK NO-ERROR.
            IF AVAILABLE calidades THEN
                lotes_ubi.calidad = calidades.descripcion.
            IF AVAILABLE envases_prod THEN
                lotes_ubi.envase  = envases_prod.descripcion.
        END.
    END.
END.
ELSE DO:
    FOR EACH tambores_industria OF lotes_jugo WHERE tambores_industria.id_locacion_ubicacion <> 10 NO-LOCK 
        BREAK BY tambores_industria.id_sucursal_ubicacion.
        ACCUM tambores_industria.id_lote (COUNT BY tambores_industria.id_sucursal_ubicacion).

        IF LAST-OF(tambores_industria.id_sucursal_ubicacion) THEN DO:
            RUN createLoteUbicacion (lotes_jugo.id_empresa , 
                                     lotes_jugo.id_sucursal , 
                                     lotes_jugo.id_tipotambor , 
                                     lotes_jugo.nromov , 
                                     tambores_industria.id_sucursal_ubicacion ,
                                     BUFFER lotes_ubi).
            IF RETURN-VALUE <> "" THEN
                UNDO , RETURN "Error de Creacion - Lote de Jugo " + RETURN-VALUE.
            BUFFER-COPY lotes_jugo TO lotes_ubi.
            lotes_ubi.cantidad = ACCUM SUB-COUNT BY tambores_industria.id_sucursal_ubicacion tambores_industria.id_lote.
            lotes_ubi.lote     = string(lotes_jugo.id_lote,"999999") + STRING(lotes_jugo.anio,"9999").
            /*by facundo*/
            FIND calidades WHERE calidades.id_calidad = tambores_industria.id_calidad  NO-LOCK NO-ERROR.
            FIND envases_prod OF tambores_industria     NO-LOCK NO-ERROR.
            IF AVAILABLE calidades THEN
                lotes_ubi.calidad = calidades.descripcion.
            IF AVAILABLE envases_prod THEN
                lotes_ubi.envase  = envases_prod.descripcion.
        END.
    END.
END.

RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createFromClaveLoteIn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createFromClaveLoteIn Procedure 
PROCEDURE createFromClaveLoteIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pEmpresa AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pSucursal AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pTipotambor AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pNromov AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pSucursalUbicacion AS INTEGER NO-UNDO.

DEFINE BUFFER lotes_ubi FOR lotes_ubicacion.

IF pTipotambor = 3 /* LOTES JUGO */ OR
   pTipotambor = 6 /* LOTES ACEITE */ OR
   pTipotambor = 7 /* LOTES FOLDEADO */ OR
   pTipotambor = 9 /* PROD. TERCEROS */ THEN DO:

    FOR EACH tambores_industria WHERE tambores_industria.id_empresa             = pEmpresa
                                  AND tambores_industria.id_sucursal            = pSucursal
                                  AND tambores_industria.id_tipotambor          = pTipotambor
                                  AND tambores_industria.nromov                 = pNromov
                                  AND tambores_industria.id_sucursal_ubicacion  = psucursalUbicacion 
                                  AND tambores_industria.id_locacion_ubicacion  = 4
                                NO-LOCK 
                                BREAK BY tambores_industria.id_sucursal_ubicacion.
    
        ACCUM tambores_industria.id_lote (COUNT BY tambores_industria.id_sucursal_ubicacion).
    
        IF LAST-OF(tambores_industria.id_sucursal_ubicacion) THEN DO:
            
            IF tambores_industria.id_tipotambor = 3 THEN DO:
                FIND FIRST lotes_jugo OF tambores_industria NO-LOCK NO-ERROR.
            END.
            ELSE DO:
                IF tambores_industria.id_tipotambor = 9 THEN DO:
                    FIND FIRST productos_terceros OF tambores_industria NO-LOCK NO-ERROR.
                END.
                ELSE DO: 
                    FIND FIRST lotes_aceite OF tambores_industria NO-LOCK NO-ERROR.
                END.
    
            END.
            IF AVAILABLE lotes_jugo OR 
               AVAILABLE lotes_aceite OR
               AVAILABLE productos_terceros THEN DO:
                
                RUN createLoteUbicacion (tambores_industria.id_empresa , 
                                         tambores_industria.id_sucursal , 
                                         tambores_industria.id_tipotambor , 
                                         tambores_industria.nromov , 
                                         pSucursalUbicacion ,
                                         BUFFER lotes_ubi) NO-ERROR.
                IF RETURN-VALUE <> "" THEN
                    UNDO , RETURN "Error de Creacion - lote".
                
                IF tambores_industria.id_tipotambor = 3 THEN 
                    BUFFER-COPY lotes_jugo TO lotes_ubi.
                ELSE DO:
                    IF tambores_industria.id_tipotambor = 9 THEN
                        BUFFER-COPY productos_terceros TO lotes_ubi.
                    ELSE
                        BUFFER-COPY lotes_aceite TO lotes_ubi.
                END.
                    
                
                lotes_ubi.cantidad = ACCUM SUB-COUNT BY tambores_industria.id_sucursal_ubicacion tambores_industria.id_lote.
                lotes_ubi.lote     = STRING(tambores_industria.id_lote,"999999") + 
                                     STRING(tambores_industria.anio,"9999").
    
                /*by facundo - cambie find of por find first ... where */
                FIND calidades WHERE calidades.id_calidad = tambores_industria.id_calidad NO-LOCK NO-ERROR.
                FIND envases_prod OF tambores_industria NO-LOCK NO-ERROR.
                IF AVAILABLE calidades THEN
                    lotes_ubi.calidad = calidades.descripcion.
                IF AVAILABLE envases_prod THEN
                    lotes_ubi.envase  = envases_prod.descripcion.
            END.
        END.
    END.
END.
RETURN "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createFromLoteIndustria) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createFromLoteIndustria Procedure 
PROCEDURE createFromLoteIndustria :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pr-lote AS ROWID NO-UNDO.
DEFINE BUFFER lotes_ubi FOR lotes_ubicacion.


FIND lotes_jugo WHERE ROWID(lotes_jugo) = pr-lote NO-LOCK NO-ERROR.

IF NOT AVAILABLE lotes_jugo THEN
DO:
    FIND lotes_aceite WHERE ROWID(lotes_aceite) = pr-lote NO-LOCK NO-ERROR.
    IF NOT AVAILABLE lotes_aceite  THEN
        RETURN "Lote Origen Inexistente".
    FOR EACH tambores_industria OF lotes_aceite WHERE tambores_industria.id_locacion_ubicacion <> 10 NO-LOCK 
        BREAK BY tambores_industria.id_sucursal_ubicacion.
        ACCUM tambores_industria.id_lote (COUNT BY tambores_industria.id_sucursal_ubicacion).

        IF LAST-OF(tambores_industria.id_sucursal_ubicacion) THEN
        DO:
            RUN createLoteUbicacion (lotes_aceite.id_empresa , lotes_aceite.id_sucursal , 
                                     lotes_aceite.id_tipotambor , lotes_aceite.nromov , tambores_industria.id_sucursal_ubicacion ,
                                     BUFFER lotes_ubi) NO-ERROR.
            IF RETURN-VALUE <> "" THEN
                UNDO , RETURN "Error de Creacion - Lote de Aceite".
            BUFFER-COPY lotes_aceite TO lotes_ubi.
            lotes_ubi.cantidad = ACCUM SUB-COUNT BY tambores_industria.id_sucursal_ubicacion tambores_industria.id_lote.
            lotes_ubi.lote     = string(lotes_aceite.id_lote,"999999") + STRING(lotes_aceite.anio,"9999").
            /*by facundo - cambie find of por find first ... where */
            FIND calidades WHERE calidades.id_calidad = tambores_industria.id_calidad NO-LOCK NO-ERROR.
            FIND envases_prod OF tambores_industria     NO-LOCK NO-ERROR.
            IF AVAILABLE calidades THEN
                lotes_ubi.calidad = calidades.descripcion.
            IF AVAILABLE envases_prod THEN
                lotes_ubi.envase  = envases_prod.descripcion.
        END.
    END.
END.
ELSE
DO:
    FOR EACH tambores_industria OF lotes_jugo WHERE tambores_industria.id_locacion_ubicacion <> 10 NO-LOCK 
        BREAK BY tambores_industria.id_sucursal_ubicacion.
        ACCUM tambores_industria.id_lote (COUNT BY tambores_industria.id_sucursal_ubicacion).

        IF LAST-OF(tambores_industria.id_sucursal_ubicacion) THEN
        DO:
            RUN createLoteUbicacion (lotes_jugo.id_empresa , lotes_jugo.id_sucursal , 
                                     lotes_jugo.id_tipotambor , lotes_jugo.nromov , tambores_industria.id_sucursal_ubicacion ,
                                     BUFFER lotes_ubi).
            IF RETURN-VALUE <> "" THEN
                UNDO , RETURN "Error de Creacion - Lote de Jugo" + RETURN-VALUE.
            BUFFER-COPY lotes_jugo TO lotes_ubi.
            lotes_ubi.cantidad = ACCUM SUB-COUNT BY tambores_industria.id_sucursal_ubicacion tambores_industria.id_lote.
            lotes_ubi.lote     = string(lotes_jugo.id_lote,"999999") + STRING(lotes_jugo.anio,"9999").
            /*by facundo*/
            FIND calidades WHERE calidades.id_calidad = tambores_industria.id_calidad  NO-LOCK NO-ERROR.
            FIND envases_prod OF tambores_industria     NO-LOCK NO-ERROR.
            IF AVAILABLE calidades THEN
                lotes_ubi.calidad = calidades.descripcion.
            IF AVAILABLE envases_prod THEN
                lotes_ubi.envase  = envases_prod.descripcion.
        END.
    END.
END.

RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createLoteUbicacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createLoteUbicacion Procedure 
PROCEDURE createLoteUbicacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER  piEmpresa            AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursal           AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piTipoTambor         AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piNroMov             AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursalUbicacion  AS INTEGER NO-UNDO.

DEFINE PARAMETER BUFFER lotes_ubi FOR lotes_ubicacion.

DEFINE VARIABLE cContrato     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iTipoContrato AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAnioContrato AS INTEGER    NO-UNDO.
DEFINE VARIABLE iItemContrato AS INTEGER    NO-UNDO.
DEFINE VARIABLE iLote         AS INTEGER    NO-UNDO.

    /* CODIGO COMENTADO POR ADRIAN EL 17/12/2003 A LAS 12:20.
       ME DA PROBLEMAS EL HECHO DE QUE ES LA SEGUNDA VEZ QUE LLAMA ESTA RUTINA 
    MESSAGE "Segunda vez que llama a getLoteUbcacion" VIEW-AS ALERT-BOX.
    RUN getLoteUbicacion (piEmpresa , 
                          piSucursal , 
                          piTipoTambor , 
                          piNroMov , 
                          piSucursalUbicacion , 
                          BUFFER lotes_ubi) NO-ERROR.
    IF RETURN-VALUE = "" THEN
        RETURN "Lote Ubicacion ya creado".
    */

    /*by facundo*/
    FOR FIRST tambores_industria WHERE tambores_industria.id_empresa    = piEmpresa  AND
                                       tambores_industria.id_sucursal   = piSucursal AND
                                       tambores_industria.id_tipotambor = piTipoTambor AND 
                                       tambores_industria.nromov        = piNroMov NO-LOCK.
      cContrato     = tambores_industria.id_contrato_of.
      iTipoContrato = tambores_industria.id_tipocontrato_of.
      iAnioContrato = tambores_industria.anio_of.
      iItemContrato = tambores_industria.ITEM_of.
      iLote         = tambores_industria.id_lote.
    END.
    
    DO TRANSACTION ON ERROR UNDO , RETURN "ADM-ERROR":
        CREATE  lotes_ubi.
        ASSIGN  lotes_ubi.id_empresa            = piEmpresa
                lotes_ubi.id_sucursal           = piSucursal
                lotes_ubi.id_tipotambor         = piTipoTambor
                lotes_ubi.nromov                = piNroMov
                lotes_ubi.id_sucursal_ubicacion = piSucursalUbicacion
                lotes_ubi.id_contrato           = cContrato
                lotes_ubi.id_tipo_contrato      = iTipoContrato
                lotes_ubi.anio_contrato         = iAnioContrato
                lotes_ubi.ITEM_contrato         = iItemContrato
                lotes_ubi.id_lote               = iLote.
    END.
    RETURN "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-despachoRemitos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE despachoRemitos Procedure 
PROCEDURE despachoRemitos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER  piEmpresa            AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursal           AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piTipoTambor         AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piNroMov             AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursalUbicacion  AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursalDestino    AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  pifecha              AS DATE    NO-UNDO.
DEFINE INPUT        PARAMETER  piCantidad           AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piDesde              AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piHasta              AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piTipoMovimiento     AS INTEGER NO-UNDO.


DEFINE BUFFER lotes_ubi     FOR lotes_ubicacion.

RUN getLoteUbicacion (piEmpresa , 
                      piSucursal , 
                      piTipoTambor , 
                      piNroMov , 
                      piSucursalUbicacion , 
                      BUFFER lotes_ubi).

IF RETURN-VALUE <> "" THEN DO:

    /* MESSAGE "Antes de createFromClaveLoteIN" VIEW-AS ALERT-BOX.  */
    RUN createFromClaveLoteIN(piEmpresa , 
                              piSucursal , 
                              piTipoTambor , 
                              piNroMov ,
                              piSucursalUBicacion
                              ).

    IF RETURN-VALUE <> "" THEN DO:
        MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
        RETURN RETURN-VALUE.
    END.
END.


DO TRANSACTION ON ERROR UNDO , RETURN "Error de Procesamiento de Remito":
  IF piTipoTambor <> 11 THEN DO:
    /*    ASSIGN lotes_ubi.cantidad = lotes_ubicacion.cantidad - piCantidad. */    
    RUN transferenciaLoteUbicacionDesdeHasta ( piEmpresa ,    /* CLAVE LOTE */
                                               piSucursal ,   /* CLAVE LOTE */    
                                               piTipoTambor , /* CLAVE LOTE */
                                               piNroMov ,     /* CLAVE LOTE */
                                               piSucursalUbicacion ,  /* SUC ORIGEN */
                                               piSucursalDestino ,    /* SUC DESTINO */
                                               piFecha ,      /* FECHA */
                                               piCantidad ,   /* CANTIDAD TAMBORES */
                                               piDesde,
                                               piHasta,
                                               piTipoMovimiento,            /* TIPO MOVIMIENTO */
                                               TRUE           /* SE MODIFICAN TAMBORES INDUSTRIA */
                                               ).
  END.
  ELSE DO:
    /*cascara*/

  END.
  IF RETURN-VALUE <> "" THEN
    UNDO , RETURN RETURN-VALUE.

END.

RETURN "".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getContenedor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getContenedor Procedure 
PROCEDURE getContenedor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER  piEmpresa            AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursal           AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piTipoTambor         AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piNroMov             AS INTEGER NO-UNDO.

FOR FIRST tambores_industria WHERE tambores_industria.id_empresa    = piEmpresa
                               AND tambores_industria.id_sucursal   = piSucursal 
                               AND tambores_industria.id_tipotambor = piTipoTambor
                               AND tambores_industria.nromov        = piNroMov
                             NO-LOCK.
  /*
  FOR FIRST items_packing_list WHERE items_packing_list.id_sucursal_remito = tambores_industria.id_sucursal_remito
                                 AND items_packing_list.id_tipo_movsto     = tambores_industria.id_tipo_movsto
                                 AND items_packing_list.nro                = tambores_industria.nro_remito
                                 AND items_packing_list.ITEM_remito        = tambores_industria.ITEM_factura
                               NO-LOCK.  
      RETURN items_packing_list.nro_contenedor.
  END.
  */
END.
RETURN "Sin Contenedor".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDesdeHasta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDesdeHasta Procedure 
PROCEDURE getDesdeHasta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER  piEmpresa            AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursal           AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piTipoTambor         AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piNroMov             AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursalUbicacion  AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piCantidad           AS INTEGER NO-UNDO.
DEFINE OUTPUT       PARAMETER  piDesde              AS INTEGER NO-UNDO.
DEFINE OUTPUT       PARAMETER  piHasta              AS INTEGER NO-UNDO.

DEFINE VAR viCantidad   AS INTEGER NO-UNDO.



piDesde = 999999999.

FOR EACH tambores_industria  WHERE tambores_industria.id_empresa = piEmpresa AND
                                   tambores_industria.id_sucursal = piSucursal AND
                                   tambores_industria.id_tipotambor = piTipoTambor AND
                                   tambores_industria.nromov = piNroMov AND
                                   tambores_industria.id_sucursal_ubicacion = piSucursalUbicacion 
    BY tambores_industria.id_tambor.
    viCantidad = viCantidad + 1.
    IF viCantidad > piCantidad THEN LEAVE.
    
    pidesde = IF tambores_industria.id_tambor < pidesde THEN tambores_industria.id_tambor ELSE pidesde.
    pihasta = IF tambores_industria.id_tambor > pihasta THEN tambores_industria.id_tambor ELSE pihasta.
END.


IF piHasta - piDesde + 1 <> piCantidad THEN
    RETURN "Error de cantidad" + string(piDesde) + STRING( piHasta).

RETURN "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoteUbicacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getLoteUbicacion Procedure 
PROCEDURE getLoteUbicacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER  piEmpresa            AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursal           AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piTipoTambor         AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piNroMov             AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursalUbicacion  AS INTEGER NO-UNDO.
DEFINE PARAMETER    BUFFER     lotes_ubi            FOR lotes_ubicacion.


IF piTipotambor = 3 /* LOTES JUGO */ OR
   piTipotambor = 6 /* LOTES ACEITE */ OR
   piTipotambor = 7 /* LOTES FOLDEADO */ OR
   piTipotambor = 9 /* PROD. TERCEROS */ THEN DO:
    
    FIND lotes_ubi WHERE lotes_ubi.id_empresa             = piEmpresa     
                     AND lotes_ubi.id_sucursal            = piSucursal    
                     AND lotes_ubi.id_tipotambor          = piTipoTambor  
                     AND lotes_ubi.nromov                 = piNroMov      
                     AND lotes_ubi.id_sucursal_ubicacion  = piSucursalUbicacion 
                    NO-LOCK NO-ERROR.
    
    IF AVAILABLE lotes_ubi THEN DO:
        RETURN "".
    END.          
    ELSE DO: 
        RETURN "Lote No Existente".
    END.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPackingList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPackingList Procedure 
PROCEDURE getPackingList :
/*------------------------------------------------------------------------------
  Purpose:  devuelve el id_vapor a partir la clave de lote
    Notes:  devuelve una lista con id_packing_list, 
                                   item_packing_list, 
                                   id_vapor,
                                   nro_contenedor, 
                                   nro_pack_list delimitada por ","
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER piIdEmpresa    AS INTEGER. 
DEFINE INPUT  PARAMETER piIdSucursal   AS INTEGER. 
DEFINE INPUT  PARAMETER piIdTipoTambor AS INTEGER. 
DEFINE INPUT  PARAMETER piNroMov       AS INTEGER.
DEFINE OUTPUT PARAMETER pcReturn       AS CHARACTER INITIAL ",,,,".

DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO INITIAL ",,,,".

FOR FIRST tambores_industria WHERE tambores_industria.id_empresa    = piIdEmpresa
                               AND tambores_industria.id_sucursal   = piIdSucursal 
                               AND tambores_industria.id_tipotambor = piIdTipoTambor
                               AND tambores_industria.nromov        = piNroMov
                             NO-LOCK.
  FOR FIRST items_packing_list WHERE items_packing_list.id_sucursal_remito = tambores_industria.id_sucursal_remito
                                 AND items_packing_list.id_tipo_movsto     = tambores_industria.id_tipo_movsto
                                 AND items_packing_list.nro                = tambores_industria.nro_remito
                                 AND items_packing_list.ITEM_remito        = tambores_industria.ITEM_factura
                               NO-LOCK.  
    FOR FIRST packing_list OF items_packing_list NO-LOCK.     
      ENTRY(1, cRet) = STRING(packing_list.id_packing_list).
      ENTRY(2, cRet) = STRING(items_packing_list.ITEM).
      ENTRY(3, cRet) = STRING(packing_list.id_vapor).
      ENTRY(4, cRet) = items_packing_list.nro_contenedor.
      ENTRY(5, cRet) = packing_list.nro_pack_list.
      /*DISP packing_list.nro_pack_list nro_contenedor id_vapor.*/
    END.
  END.
END.
 
pcReturn = cRet.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRangoTambores) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getRangoTambores Procedure 
PROCEDURE getRangoTambores :
/*------------------------------------------------------------------------------
  Purpose:     devuelve el primer y ultimo tambor del lote parametro
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER  piEmpresa            AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursal           AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piTipoTambor         AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piNroMov             AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursalUbicacion  AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piCantidad           AS INTEGER NO-UNDO.
DEFINE OUTPUT       PARAMETER  piDesde              AS INTEGER NO-UNDO.
DEFINE OUTPUT       PARAMETER  piHasta              AS INTEGER NO-UNDO.

DEFINE VAR i AS INTEGER.

FOR FIRST tambores_industria WHERE tambores_industria.id_empresa    = piEmpresa    AND
                                   tambores_industria.id_sucursal   = piSucursal   AND
                                   tambores_industria.id_tipotambor = piTipoTambor AND
                                   tambores_industria.nromov        = piNroMov     AND
                                   tambores_industria.id_sucursal_ubicacion = piSucursalUbicacion
                              BY tambores_industria.id_tambor.
    piDesde = tambores_industria.id_tambor.
END.

/*
FOR LAST tambores_industria WHERE tambores_industria.id_empresa    = piEmpresa    AND
                                  tambores_industria.id_sucursal   = piSucursal   AND
                                  tambores_industria.id_tipotambor = piTipoTambor AND
                                  tambores_industria.nromov        = piNroMov     AND
                                  tambores_industria.id_sucursal_ubicacion = piSucursalUbicacion.
  piHasta = tambores_industria.id_tambor.
END.
*/

piHasta = piDesde + piCantidad - 1.

DO i = piDesde TO piHasta:
  FIND FIRST tambores_industria WHERE tambores_industria.id_empresa            = piEmpresa
                                  AND tambores_industria.id_sucursal           = piSucursal
                                  AND tambores_industria.id_tipotambor         = piTipoTambor
                                  AND tambores_industria.nromov                = piNroMov     
                                  AND tambores_industria.id_sucursal_ubicacion = piSucursalUbicacion
                                  AND tambores_industria.id_tambor             = i 
                                NO-LOCK NO-ERROR.
  IF NOT AVAILABLE tambores_industria THEN DO:
    MESSAGE "No se puede establecer un rango de tambores valido" VIEW-AS ALERT-BOX.
    UNDO , RETURN " No existe esa cantidad de tambores para mover tbs: " + STRING(piDesde) + STRING(piHasta) + STRING(piCantidad) + STRING(i).
  END.
  i = i + 1.         
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getVapor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getVapor Procedure 
PROCEDURE getVapor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER  piEmpresa            AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursal           AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piTipoTambor         AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piNroMov             AS INTEGER NO-UNDO.
DEFINE OUTPUT       PARAMETER  pcVapor              AS CHARACTER NO-UNDO.

DEFINE VARIABLE cPL AS CHARACTER  NO-UNDO INITIAL ",,,,".

MESSAGE "Antes de ejecutar el getPackigList" VIEW-AS ALERT-BOX.

RUN getPackingList(piEmpresa, piSucursal, piTipoTambor, piNroMov, OUTPUT cPL).

MESSAGE "Despuesssssssssss de ejecutar el getPackigList" VIEW-AS ALERT-BOX.

IF cPL <> ",,,," THEN DO:
  MESSAGE "Antes del Find de Vapores" VIEW-AS ALERT-BOX.

  FIND FIRST vapores WHERE vapores.id_vapor = INTEGER(ENTRY(3, cPL)) NO-LOCK NO-ERROR.

  MESSAGE "Despuesss dellll ejecutar el getPackigList" VIEW-AS ALERT-BOX.
  IF AVAILABLE vapores THEN DO:
      pcVapor = vapores.descripcion.
  END.
  ELSE pcVapor = "No se encontro Vapor".
END.
ELSE 
  pcVapor =  "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-transferenciaLoteCascara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transferenciaLoteCascara Procedure 
PROCEDURE transferenciaLoteCascara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piEmpresa  AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER piSucursal AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER piNroMov   AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER piSucDes   AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER piCantidad AS INTEGER    NO-UNDO.

  DO TRANSACTION ON ERROR UNDO , RETURN "ADM-ERROR":
    FIND lotes_ubicacion WHERE lotes_ubicacion.id_empresa            = piEmpresa 
                           AND lotes_ubicacion.id_sucursal           = piSucursal
                           AND lotes_ubicacion.id_tipotambor         = 11
                           AND lotes_ubicacion.nromov                = piNroMov
                           AND lotes_ubicacion.id_sucursal_ubicacion = piSucursal
                         NO-ERROR.
    IF AVAILABLE lotes_ubicacion THEN DO:
      IF lotes_ubicacion.cantidad = piCantidad THEN /*si existe en lote ubicacion y muevo el lote completo*/
        ASSIGN lotes_ubicacion.id_sucursal_ubicacion = piSucDes.
      ELSE /*si existe en lote_ubicacion y no muevo el lote completo*/
        ASSIGN lotes_ubicacion.cantidad = lotes_ubicacion.cantidad - piCantidad.
        /*creo en la sucursal destino */
        FIND lotes_cascara WHERE lotes_cascara.nromov = piNroMov NO-LOCK NO-ERROR.
        FIND calidades WHERE calidades.id_calidad = lotes_cascara.id_calidad NO-LOCK NO-ERROR.
        FIND envases_prod WHERE lotes_cascara.id_envase = envases_prod.id_envase NO-LOCK NO-ERROR.
        CREATE  lotes_ubicacion.
        ASSIGN  lotes_ubicacion.id_empresa            = piEmpresa
                lotes_ubicacion.id_sucursal           = piSucursal
                lotes_ubicacion.id_tipotambor         = 11
                lotes_ubicacion.nromov                = piNroMov
                lotes_ubicacion.id_sucursal_ubicacion = piSucDes
                lotes_ubicacion.cantidad              = piCantidad
                lotes_ubicacion.id_lote               = lotes_cascara.id_lote
                lotes_ubicacion.calidad               = calidades.descripcion
                lotes_ubicacion.envase                = envases_prod.descripcion
                lotes_ubicacion.lote                  = STRING(lotes_cascara.id_lote,"999999") + STRING(lotes_cascara.anio,"9999").
    END.
    ELSE 
      RETURN "No existe en lote_ubicacion".
    /* no deberia sucerder deberia existir siempre
    ELSE DO:
      FIND lotes_cascara WHERE lotes_cascara.nromov = piNroMov NO-LOCK NO-ERROR.
      FIND calidades WHERE calidades.id_calidad = lotes_cascara.id_calidad NO-LOCK NO-ERROR.
      FIND envases_prod WHERE lotes_cascara.id_envase = envases_prod.id_envase NO-LOCK NO-ERROR.
      CREATE  lotes_ubicacion.
      ASSIGN  lotes_ubicacion.id_empresa            = piEmpresa
              lotes_ubicacion.id_sucursal           = piSucursal
              lotes_ubicacion.id_tipotambor         = 11
              lotes_ubicacion.nromov                = piNroMov
              lotes_ubicacion.id_sucursal_ubicacion = piSucursal
              lotes_ubicacion.cantidad              = piCantidad
              lotes_ubicacion.id_lote               = lotes_cascara.id_lote
              lotes_ubicacion.calidad               = calidades.descripcion
              lotes_ubicacion.envase                = envases_prod.descripcion
              lotes_ubicacion.lote                  = STRING(lotes_cascara.id_lote,"999999") + STRING(lotes_cascara.anio,"9999").              
    END.
    */
  
    
    RUN y_gstkmovdep_cascara.p (piEmpresa , 
                                piSucursal ,
                                11 ,
                                piNroMov ,
                                piSucursal,
                                piSucDes,
                                1,
                                piCantidad,
                                33 , 
                                TODAY ).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-transferenciaLoteUbicacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transferenciaLoteUbicacion Procedure 
PROCEDURE transferenciaLoteUbicacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER  piEmpresa            AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursal           AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piTipoTambor         AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piNroMov             AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursalUbicacion  AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursalDestino    AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  pifecha              AS DATE    NO-UNDO.
DEFINE INPUT        PARAMETER  piCantidad           AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piTipoMovimiento     AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  plActualizaTambor    AS LOGICAL NO-UNDO.


DEFINE VAR    viDesde           AS INTEGER NO-UNDO INITIAL 99999999.
DEFINE VAR    viHasta           AS INTEGER NO-UNDO.
DEFINE VAR    viTipoMovimiento  AS INTEGER NO-UNDO.  
DEFINE VAR    viCantidad        AS INTEGER NO-UNDO.
DEFINE VAR    vcLista           AS CHARACTER NO-UNDO.
DEFINE VAR    i                 AS INTEGER NO-UNDO.

IF piTipoMovimiento = ? OR piTipoMovimiento = 0 THEN DO:
    viTipoMovimiento = getTipoMovimiento ( piSucursalUbicacion , piSucursalDestino ).
    IF viTipoMovimiento = ?  THEN RETURN "No puede determinar el tipo de movimiento de Stock o existe mas de un tipo posible".

END.
ELSE
    viTipoMovimiento = piTipoMovimiento.
    DO TRANSACTION ON ERROR UNDO , RETURN "ADM-ERROR":
         /* MESSAGE " Ante de BasicTransferenciaLoteUbicacion" VIEW-AS ALERT-BOX. */
        RUN BasicTransferenciaLoteUbicacion (piEmpresa , 
                                             piSucursal , 
                                             piTipoTambor , 
                                             piNroMov , 
                                             piSucursalUbicacion , 
                                             piSucursalDestino , 
                                             piFecha , 
                                             piCantidad , 
                                             viTipoMovimiento , 
                                             plActualizaTambor).

        IF RETURN-VALUE <> ""  THEN UNDO , RETURN RETURN-VALUE.


/****** AGREGADO POR PABLO PARA MANEJAR TAMBORES Y STOCK HISTORICO TAMBORES **************************/

        IF plActualizaTambor THEN DO:
            
            /* MESSAGE " Antes de actualizaTamboresPorCantidad" VIEW-AS ALERT-BOX. */
            RUN actualizaTamboresPorCantidad (piEmpresa , 
                                              piSucursal , 
                                              piTipoTambor , 
                                              piNroMov , 
                                              piSucursalUbicacion , 
                                              piSucursalDestino , 
                                              OUTPUT viDesde , 
                                              OUTPUT viHasta  , 
                                              piCantidad ,
                                              OUTPUT vcLista).
            IF RETURN-VALUE <> "" THEN
                UNDO , RETURN RETURN-VALUE.


            DO i = 1 TO NUM-ENTRIES(vcLista):

              viDesde = integer(ENTRY(1,ENTRY(i , vcLista) , CHR(14))).
              vihasta = integer(ENTRY(2,ENTRY(i , vcLista) , CHR(14))).

              RUN Y_gstkmovdep.p (piEmpresa , 
                                  piSucursal ,
                                  piTipoTambor ,
                                  piNroMov ,
                                  piSucursalUbicacion,
                                  piSucursalDestino,
                                  viDesde,
                                  viHasta,
                                  viTipoMovimiento ,
                                  piFecha ).

              IF RETURN-VALUE <> "" THEN 
                  UNDO , RETURN RETURN-VALUE.

            END.
    
        END.
    END.

    RETURN "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-transferenciaLoteUbicacionDesdeHasta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transferenciaLoteUbicacionDesdeHasta Procedure 
PROCEDURE transferenciaLoteUbicacionDesdeHasta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER  piEmpresa            AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursal           AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piTipoTambor         AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piNroMov             AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursalUbicacion  AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursalDestino    AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  pifecha              AS DATE    NO-UNDO.
DEFINE INPUT        PARAMETER  piCantidad           AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piDesde              AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piHasta              AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piTipoMovimiento     AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  plActualizaTambor    AS LOGICAL NO-UNDO.

DEFINE BUFFER lotes_ubi     FOR lotes_ubicacion.
DEFINE BUFFER lotes_ubi1    FOR lotes_ubicacion.

DEFINE VAR    viTipoMovimiento  AS INTEGER NO-UNDO.  
DEFINE VAR    viCantidad        AS INTEGER NO-UNDO.

IF piTipoMovimiento = ? OR piTipoMovimiento = 0 THEN DO:
    viTipoMovimiento = getTipoMovimiento ( piSucursalUbicacion , piSucursalDestino ).
    IF viTipoMovimiento = ?  THEN RETURN "No puede determinar el tipo de movimiento de Stock o existe mas de un tipo posible".

END.
ELSE
    viTipoMovimiento = piTipoMovimiento.
    
    DO TRANSACTION ON ERROR UNDO , RETURN "ADM-ERROR":
        /* EN ESTA RUTINA SE CREA SI ES NECESARIO LOS LOTES UBICACION ORIGEN Y DESTINO
           Y SE MUEVE LA MERCADERIA DE UNA SUCURSAL A LA OTRA EN LA TABLA LOTES UBICACION */
        /* MESSAGE "Antes de BasiTransferenciaLoteUbicacion" VIEW-AS ALERT-BOX. */
        IF piTipotambor = 3 /* LOTES JUGO */ OR
           piTipotambor = 6 /* LOTES ACEITE */ OR
           piTipotambor = 7 /* LOTES FOLDEADO */ OR
           piTipotambor = 9 /* PROD. TERCEROS */ THEN DO:

            RUN BasicTransferenciaLoteUbicacion (piEmpresa , 
                                                 piSucursal , 
                                                 piTipoTambor , 
                                                 piNroMov , 
                                                 piSucursalUbicacion , 
                                                 piSucursalDestino , 
                                                 piFecha , 
                                                 piCantidad , 
                                                 viTipoMovimiento , 
                                                 plActualizaTambor).
    
            IF RETURN-VALUE <> ""  THEN UNDO , RETURN RETURN-VALUE.
        END.
/****** AGREGADO POR PABLO PARA MANEJAR TAMBORES Y STOCK HISTORICO TAMBORES **************************/

        IF plActualizaTambor THEN DO:
            /* EN ESTA RUTINA SE MUEVEN LOS TAMBORES INDUSTRIA DE LA SUC ORIGEN A LA SUC DESTINO */
            /* MESSAGE "Antes de actualizaTamboresDesdeHasta" VIEW-AS ALERT-BOX. */
            RUN actualizaTamboresDesdeHasta (piEmpresa , 
                                             piSucursal , 
                                             piTipoTambor , 
                                             piNroMov , 
                                             piSucursalUbicacion , 
                                             piSucursalDestino , 
                                             piDesde , 
                                             piHasta , 
                                             piCantidad).
            IF RETURN-VALUE <> "" THEN
                UNDO , RETURN RETURN-VALUE.
            /* MESSAGE "Antes de y_gstkmovdep.p" VIEW-AS ALERT-BOX. */
            RUN Y_gstkmovdep.p (piEmpresa , 
                                piSucursal ,
                                piTipoTambor ,
                                piNroMov ,
                                piSucursalUbicacion,
                                piSucursalDestino,
                                pidesde,
                                pihasta,
                                viTipoMovimiento ,
                                piFecha ).
            
            IF RETURN-VALUE <> "" THEN DO:
                UNDO , RETURN RETURN-VALUE.
            END.
        END.
    END.

    RETURN "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validaDesdehasta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validaDesdehasta Procedure 
PROCEDURE validaDesdehasta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER  piEmpresa            AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursal           AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piTipoTambor         AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piNroMov             AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piSucursalUbicacion  AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piDesde              AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER  piHasta              AS INTEGER NO-UNDO.

DEFINE VAR viCantidad   AS INTEGER NO-UNDO.




FOR EACH tambores_industria  WHERE tambores_industria.id_empresa = piEmpresa AND
                                   tambores_industria.id_sucursal = piSucursal AND
                                   tambores_industria.id_tipotambor = piTipoTambor AND
                                   tambores_industria.nromov = piNroMov AND
                                   tambores_industria.id_sucursal_ubicacion = piSucursalUbicacion AND
                                   tambores_industria.id_tambor >= piDesde AND
                                   tambores_industria.id_tambor <= pihasta
    BY tambores_industria.id_tambor.
    viCantidad = viCantidad + 1.
    
END.


IF piHasta - piDesde + 1 <> viCantidad THEN
    RETURN "Error de cantidad" + string(piDesde) + STRING( piHasta).

RETURN "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getTipoMovimiento) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTipoMovimiento Procedure 
FUNCTION getTipoMovimiento RETURNS INTEGER
  ( INPUT piSucursalOrigen AS INTEGER, INPUT piSucursalDestino AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR viMovimientos      AS INTEGER NO-UNDO.
  DEFINE VAR viTipoMovimiento   AS INTEGER  NO-UNDO.

  FOR EACH tipos_movimientos WHERE      LOOKUP(STRING(piSucursalOrigen),tipos_movimientos.suc_origen) > 0  AND
                                        LOOKUP(STRING(piSucursalDestino),tipos_movimientos.suc_destino) > 0 NO-LOCK.
    viMovimientos       = viMovimientos + 1.       
    viTipoMovimiento    = tipos_movimiento.id_tipo_movimiento.
  END.
  
  IF viMovimientos = 0 OR viMovimientos > 2 THEN
      RETURN ?.


  RETURN viMovimientos.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

