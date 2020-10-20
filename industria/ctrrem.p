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
DEFINE VAR v_sucursal_ubicacion AS INTEGER.

CURRENT-WINDOW:WIDTH = 200.

/*
FIND remitos WHERE remitos.nro_comp = '008700001236'.

DO TRANSACTION ON ERROR UNDO , LEAVE:
FOR EACH items_factura OF remitos NO-LOCK.
      FOR EACH r_lote_cascara_remito WHERE r_lote_cascara_remito.id_sucursal_remito = items_factura.id_sucursal
                                       AND r_lote_cascara_remito.id_tipo_movsto     = items_factura.id_tipo_movsto
                                       AND r_lote_cascara_remito.nro_remito         = remitos.nro
                                       AND r_lote_cascara_remito.ITEM_factura       = items_factura.ITEM
                                     NO-LOCK.
        FIND FIRST lugar_descarga OF remitos NO-LOCK NO-ERROR.
        IF AVAILABLE lugar_descarga THEN DO:
          v_sucursal_ubicacion = lugar_descarga.id_sucursal.
        END.


        DISPLAY r_lote_cascara_remito.id_empresa
                r_lote_cascara_remito.id_sucursal
                r_lote_cascara_remito.id_tipotambor
                r_lote_cascara_remito.nromov
                items_factura.id_sucursal
                v_sucursal_ubicacion
                items_factura.fecha
                items_factura.cantidad
                remitos.peso_neto
                items_factura.desde_lote
                items_factura.hasta_lote WITH FRAME X WIDTH 150.

        RUN transf                   (INPUT r_lote_cascara_remito.id_empresa,
                                      INPUT r_lote_cascara_remito.id_sucursal,
                                     /*  INPUT r_lote_cascara_remito.id_tipotambor, */
                                      INPUT r_lote_cascara_remito.nromov,
                                      INPUT items_factura.id_sucursal, /* SUC ORIGEN */
                                      INPUT v_sucursal_ubicacion,      /* SUC DESTINO */
                                    /*  INPUT items_factura.fecha, */
                                      INPUT items_factura.cantidad
                                    /*  INPUT items_factura.desde_lote,
                                      INPUT items_factura.hasta_lote,
                                      INPUT 3 */). 
        IF RETURN-VALUE <> '' THEN
        DO:
            MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX.
            UNDO , LEAVE.
        END.

      END.
END.
END.

*/

RUN verNro.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-transf) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transf Procedure 
PROCEDURE transf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piEmpresa   AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER piSucursal  AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER piNroMov    AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER piSucOri    AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER piSucDes    AS INTEGER    NO-UNDO.
  DEFINE INPUT PARAMETER piCantidad  AS INTEGER    NO-UNDO.

  DEFINE VARIABLE viCant    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE vcCali    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcEnva    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcLote    AS CHARACTER  NO-UNDO.

  DO TRANSACTION ON ERROR UNDO , RETURN "ADM-ERROR":
    /*busco si ya existe lote en esa ubicacion*/
    
    FIND FIRST lotes_ubicacion WHERE lotes_ubicacion.id_empresa            = piEmpresa
                                 AND lotes_ubicacion.id_sucursal           = piSucursal
                                 AND lotes_ubicacion.id_tipotambor         = 11
                                 AND lotes_ubicacion.nromov                = piNroMov
                                 AND lotes_ubicacion.id_sucursal_ubicacion = piSucDes
                               NO-ERROR.
    /*
    IF AVAILABLE lotes_ubicacion THEN DO:
      ASSIGN viCant = lotes_ubicacion.cantidad
             vcCali = lotes_ubicacion.calidad
             vcEnva = lotes_ubicacion.envase
             vcLote = lotes_ubicacion.lote.
      DELETE lotes_ubicacion.
    END.
    */

    FIND lotes_cascara WHERE lotes_cascara.nromov   = piNroMov NO-LOCK NO-ERROR.
    FIND calidades WHERE calidades.id_calidad       = lotes_cascara.id_calidad NO-LOCK NO-ERROR.
    FIND envases_prod WHERE envases_prod.id_envase  = lotes_cascara.id_envase NO-LOCK NO-ERROR.

    DISPLAY lotes_cascara.id_lote_cliente calidades.id_calidad envases_prod.id_envase.


    /*
    CREATE  lotes_ubicacion.
    ASSIGN  lotes_ubicacion.id_empresa            = piEmpresa
            lotes_ubicacion.id_sucursal           = piSucursal
            lotes_ubicacion.id_tipotambor         = 11
            lotes_ubicacion.nromov                = piNroMov
            lotes_ubicacion.id_sucursal_ubicacion = piSucDes
            lotes_ubicacion.cantidad              = piCantidad + viCant
            lotes_ubicacion.id_lote               = lotes_cascara.id_lote
            lotes_ubicacion.calidad               = calidades.descripcion
            lotes_ubicacion.envase                = envases_prod.descripcion
            lotes_ubicacion.lote                  = STRING(lotes_cascara.id_lote,"999999") + STRING(lotes_cascara.anio,"9999"). */


    /*busco el lote en la sucursal origen para restar la cantidad*/
    
    FIND FIRST lotes_ubicacion WHERE lotes_ubicacion.id_empresa            = piEmpresa
                                 AND lotes_ubicacion.id_sucursal           = piSucursal
                                 AND lotes_ubicacion.id_tipotambor         = 11
                                 AND lotes_ubicacion.nromov                = piNroMov
                                 AND lotes_ubicacion.id_sucursal_ubicacion = piSucOri
                               NO-ERROR.
    /*
    IF AVAILABLE lotes_ubicacion THEN DO:
      IF (lotes_ubicacion.cantidad - piCantidad) <= 0 THEN /*si la cantidad queda en 0 borro de lotes_ubciacion*/
        DELETE lotes_ubicacion.
      ELSE 
        lotes_ubicacion.cantidad = lotes_ubicacion.cantidad - piCantidad.
    END. */

    /*actualiza stock*/
    /*
    RUN y_gstkmovdep_cascara.p (piEmpresa , 
                                piSucursal ,
                                11 ,
                                piNroMov ,
                                piSucOri,
                                piSucDes,
                                1,
                                piCantidad,
                                33 , 
                                TODAY ). */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-verNro) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE verNro Procedure 
PROCEDURE verNro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR nro AS INTEGER.
nro = ?.
FOR EACH lotes_cascara NO-LOCK BY nromov.
    DISPLAY nromov.
    IF nro EQ lotes_cascara.nromov THEN
        DISPLAY 'el lote ' + string(nro) + ' esta duplicado'.
    nro = nromov.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

