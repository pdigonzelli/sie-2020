&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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
  DEFINE STREAM ped .
  DEFINE STREAM ite .
  DEFINE STREAM s.

  DEFINE TEMP-TABLE tt-of
      FIELD anio_contrato LIKE pedidos_packing.anio_contrato
      FIELD ano LIKE pedidos_packing.ano
      FIELD china LIKE pedidos_packing.china
      FIELD completo LIKE pedidos_packing.completo
      FIELD contramarca LIKE pedidos_packing.contramarca
      FIELD estado LIKE pedidos_packing.estado
      FIELD fecha LIKE pedidos_packing.fecha
      FIELD id_agencia LIKE pedidos_packing.id_agencia
      FIELD id_cliente LIKE pedidos_packing.id_cliente
      FIELD id_cliente_remito LIKE pedidos_packing.id_cliente_remito
      FIELD id_contrato LIKE pedidos_packing.id_contrato
      FIELD id_destino_final LIKE pedidos_packing.id_destino_final
      FIELD id_empresa LIKE pedidos_packing.id_empresa
      FIELD id_mercado LIKE pedidos_packing.id_mercado
      FIELD id_orden LIKE pedidos_packing.id_orden
      FIELD id_pedido_sap LIKE pedidos_packing.id_pedido_sap
      FIELD id_programa_despacho LIKE pedidos_packing.id_programa_despacho
      FIELD id_prov_usa LIKE pedidos_packing.id_prov_usa
      FIELD id_puerto_ent LIKE pedidos_packing.id_puerto_ent
      FIELD id_puerto_sal LIKE pedidos_packing.id_puerto_sal
      FIELD id_punto_emisor LIKE pedidos_packing.id_punto_emisor
      FIELD id_tipo_contrato LIKE pedidos_packing.id_tipo_contrato
      FIELD id_vapor LIKE pedidos_packing.id_vapor
      FIELD id_vapor_usa LIKE pedidos_packing.id_vapor_usa
      FIELD id_zona LIKE pedidos_packing.id_zona
      FIELD ITEM_contrato LIKE pedidos_packing.item_contrato
      FIELD pallets_hechos LIKE pedidos_packing.pallets_hechos
      FIELD semana LIKE pedidos_packing.semana
      FIELD semana_contrato LIKE pedidos_packing.semana_contrato
      FIELD TOTAL_pallets LIKE pedidos_packing.TOTAL_pallets
      FIELD UNION_europea LIKE pedidos_packing.UNION_europea
      FIELD bultos LIKE items_pedidos_packing.bultos
      FIELD calibre LIKE items_pedidos_packing.calibre
      FIELD cant_pallets LIKE items_pedidos_packing.cant_pallets
      FIELD id_articulo LIKE items_pedidos_packing.id_articulo
      FIELD id_calidad LIKE items_pedidos_packing.id_calidad
      FIELD id_caract LIKE items_pedidos_packing.id_caract
      FIELD id_categoria LIKE items_pedidos_packing.id_categoria
      FIELD id_confeccion LIKE items_pedidos_packing.id_confeccion
      FIELD id_envase LIKE items_pedidos_packing.id_envase
      FIELD id_marca LIKE items_pedidos_packing.id_marca
      FIELD id_tipo_esquinero LIKE items_pedidos_packing.id_tipo_esquinero
      FIELD id_tipo_pallet LIKE items_pedidos_packing.id_tipo_pallet
      FIELD id_variedad LIKE items_pedidos_packing.id_variedad
      FIELD ITEM1 LIKE items_pedidos_packing.ITEM
      FIELD ITEM_pedido_sap LIKE items_pedidos_packing.ITEM_pedido_sap.

  DEFINE STREAM salida.

  DEFINE STREAM a-chk.
  DEFINE STREAM b-chk.

  DEFINE TEMP-TABLE ttok LIKE balanza_pesadas.
  DEFINE TEMP-TABLE tterror LIKE balanza_pesadas.

  DEFINE STREAM ingok.
  DEFINE STREAM ingmal.

  DEFINE TEMP-TABLE ttentrega 
     FIELD tclote AS CHARACTER
     FIELD tcmaterial AS CHARACTER
     FIELD tdpeso AS DECIMAL.

  DEFINE TEMP-TABLE ttprogress 
     FIELD tccodbarra AS CHARACTER
     FIELD tdpeso AS DECIMAL.

  DEFINE TEMP-TABLE ttitems LIKE items_pallets.

  DEFINE TEMP-TABLE ttped   LIKE pedidos_packing.
  DEFINE TEMP-TABLE ttitped LIKE items_pedidos_packing.


  DEFINE TEMP-TABLE ttbp LIKE balanza_pesadas.
  DEFINE TEMP-TABLE ttbt LIKE balanza_tickets.


  DEFINE TEMP-TABLE  TTCLASIFICACION
    FIELD LOTE AS CHARACTER
    FIELD CARACTERISTICA    AS CHARACTER
    FIELD VALOR             AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-connectToSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD connectToSap Procedure 
FUNCTION connectToSap RETURNS COM-HANDLE
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-connectToSapCalidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD connectToSapCalidad Procedure 
FUNCTION connectToSapCalidad RETURNS COM-HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-connectToSapCapacitacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD connectToSapCapacitacion Procedure 
FUNCTION connectToSapCapacitacion RETURNS COM-HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-connectToSapDesarrollo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD connectToSapDesarrollo Procedure 
FUNCTION connectToSapDesarrollo RETURNS COM-HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-connectToSapInternet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD connectToSapInternet Procedure 
FUNCTION connectToSapInternet RETURNS COM-HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-connectToSapTipo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD connectToSapTipo Procedure 
FUNCTION connectToSapTipo RETURNS COM-HANDLE
  ( cTipo AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-disconnectFromSAP) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD disconnectFromSAP Procedure 
FUNCTION disconnectFromSAP RETURNS LOGICAL
    (phConn AS COM-HANDLE)  FORWARD.

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
         HEIGHT             = 16.81
         WIDTH              = 71.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-actFincas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE actFincas Procedure 
PROCEDURE actFincas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH TABlas_sap WHERE  tabla = 'Almacenes'  AND SUBSTRING(codigo,1,4) = 'A300' .
    DISP tablas_sap WITH WIDTH 80.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-anulacionIngresoFruta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anulacionIngresoFruta Procedure 
PROCEDURE anulacionIngresoFruta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pbalanza AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pPesada AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER PNROTICKET AS INTEGER NO-UNDO.

DEFINE VARIABLE hOCarga         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hPosicion       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE HDOC            AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE HERROR          AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE hFunctions      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hFunc           AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE xentrega_sap    AS CHARACTER NO-UNDO.

DEFINE VARIABLE CMES            AS CHARACTER NO-UNDO.
DEFINE VARIABLE CMES1           AS CHARACTER NO-UNDO.
DEFINE VARIABLE LERROR          AS LOGICAL NO-UNDO.
DEFINE VARIABLE CERROR          AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKAY            AS LOGICAL NO-UNDO.
DEFINE VARIABLE CTITULO         AS CHARACTER NO-UNDO.



    LERROR = FALSE.
    CERROR = ''.



    FIND balanza_pesadas WHERE  balanza_pesadas.id_balanza = pbalanza AND
                                balanza_pesadas.id_pesada  = pPesada  NO-LOCK NO-ERROR.

    IF NOT AVAILABLE balanza_pesadas THEN
    DO:
        LERROR = TRUE.
        CERROR = 'PESADA INEXISTENTE'.

    END.

    IF NOT LERROR  THEN
    DO:
        IF balanza_pesaDas.orden_carga_sap = ''  THEN
            RETURN ''.

        FIND BALANZA_TICKETS WHERE  BALANZA_TICKETS.ID_BALANZA  =   PBALANZA AND
                                    BALANZA_TICKETS.ID_PESADA   =   PPESADA  AND
                                    BALANZA_TICKETS.NRO_TICKET  =   PNROTICKET NO-ERROR.

        IF NOT AVAILABLE BALANZA_TICKETS THEN
        DO:
            LERROR = TRUE.
            CERROR = 'REMITO INEXISTENTE'.
        END.
        ELSE
        DO:
            hFunctions = connectToSap().
            IF NOT VALID-HANDLE(hFunctions) THEN DO:
                LERROR = TRUE.
                CERROR = "ERROR DE CONECCION A INTERFACE".
            END.
            ELSE
            DO:
                hFunc  = hFunctions:ADD('BAPIANULACIONINGRESOS').
                IF VALID-HANDLE(hFunc) THEN DO:

                    hOCarga             = hFunc:exports('I_ORDEN_CARGA').
                    hposicion           = hFunc:exports('I_POSICION').
                    hDOC                = hFunc:IMPORTS('NUMDOC').
                    hERROR              = hFunc:IMPORTS('ERROR').

                    HOCARGA:VALUE = STRING(BALANZA_PESADAS.ORDEN_CARGA_SAP , '999999999999999').
                    HPOSICION:VALUE = STRING(BALANZA_TICKETS.NRO_TICKET, '9999').



                    IF NOT hFunc:CALL() THEN  
                    DO:
                        LERROR = TRUE.
                        CERROR = CERROR + '~n' + 'ERROR ANULACION TICKET: ' + STRING(BALANZA_TICKETS.NRO_TICKET).
                    END.

                    IF herror:VALUE <> 'OK' AND herror:VALUE <> 'VL302'  AND herror:VALUE <> '' THEN 
                    DO:
                        LERROR = TRUE.
                        CERROR = CERROR + '~n' + 'ERROR ANULACION TICKET : ' + herror:VALUE .
                    END.
                    ELSE
                        ASSIGN lerror = FALSE.
                END.
                ELSE 
                DO:
                        LERROR = TRUE.
                        CERROR = CERROR + '~n' + 'ERROR DE LLAMADA FUNCION DE ANULACION EN SAP ' .
                END.
            END.
        END.
    END.

    IF VALID-HANDLE(hfunctions) THEN
        hFunctions:connection:logoff().


    IF CERROR = 'OK' OR CERROR = 'VL302' OR CERROR = '' THEN LERROR = FALSE.
    IF LERROR  THEN RETURN ERROR CERROR.

    RETURN CERROR.


    CATCH myerror AS PROGRESS.lang.apperror:
        RETURN ERROR myerror:getmessage(1).
    END CATCH.
    FINALLY:
        IF VALID-HANDLE(hfunctions) THEN
            hFunctions:connection:logoff().

    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-anulacionIngresoFrutaCalidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anulacionIngresoFrutaCalidad Procedure 
PROCEDURE anulacionIngresoFrutaCalidad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pbalanza AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pPesada AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER PNROTICKET AS INTEGER NO-UNDO.



DEFINE VARIABLE hOCarga         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hPosicion       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE HDOC            AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE HERROR          AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE hFunctions      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hFunc           AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE xentrega_sap    AS CHARACTER NO-UNDO.

DEFINE VARIABLE CMES            AS CHARACTER NO-UNDO.
DEFINE VARIABLE CMES1           AS CHARACTER NO-UNDO.
DEFINE VARIABLE LERROR          AS LOGICAL NO-UNDO.
DEFINE VARIABLE CERROR          AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKAY            AS LOGICAL NO-UNDO.
DEFINE VARIABLE CTITULO         AS CHARACTER NO-UNDO.
DEFINE VARIABLE voc             AS CHARACTER NO-UNDO.



    LERROR = FALSE.
    CERROR = ''.



    FIND balanza_pesadas WHERE  balanza_pesadas.id_balanza = pbalanza AND
                                balanza_pesadas.id_pesada  = pPesada  NO-LOCK NO-ERROR.

    IF NOT AVAILABLE balanza_pesadas THEN
    DO:
        LERROR = TRUE.
        CERROR = 'PESADA INEXISTENTE'.

    END.

    IF NOT LERROR  THEN
    DO:
        IF balanza_pesaDas.orden_carga_sap = ''  THEN
            RETURN ''.

        VOC = BALANZA_PESADAS.ORDEN_CARGA_SAP.

        FIND BALANZA_TICKETS WHERE  BALANZA_TICKETS.ID_BALANZA  =   PBALANZA AND
                                    BALANZA_TICKETS.ID_PESADA   =   PPESADA  AND
                                    BALANZA_TICKETS.NRO_TICKET  =   PNROTICKET NO-ERROR.

        IF NOT AVAILABLE BALANZA_TICKETS THEN
        DO:
            LERROR = TRUE.
            CERROR = 'REMITO INEXISTENTE'.
        END.
        ELSE
        DO:
            hFunctions = connectToSapCalidad().
            IF NOT VALID-HANDLE(hFunctions) THEN DO:
                LERROR = TRUE.
                CERROR = "ERROR DE CONECCION A INTERFACE".
            END.
            ELSE
            DO:

                hFunc  = hFunctions:ADD('BAPIANULACIONINGRESOS').
                IF VALID-HANDLE(hFunc) THEN DO:

                    hOCarga             = hFunc:exports('I_ORDEN_CARGA').
                    hposicion           = hFunc:exports('I_POSICION').
                    hDOC                = hFunc:IMPORTS('NUMDOC').
                    hERROR              = hFunc:IMPORTS('ERROR').

                    HOCARGA:VALUE = STRING(BALANZA_PESADAS.ORDEN_CARGA_SAP , '999999999999999').
                    HPOSICION:VALUE = STRING(BALANZA_TICKETS.NRO_TICKET, '9999').



                    IF NOT hFunc:CALL() THEN  
                    DO:
                        LERROR = TRUE.
                        CERROR = CERROR + '~n' + 'ERROR ANULACION TICKET: ' + STRING(BALANZA_TICKETS.NRO_TICKET).
                    END.

                    IF herror:VALUE <> 'OK' OR HERROR:VALUE <> 'vl302' THEN 
                    DO:
                        LERROR = TRUE.
                        CERROR = CERROR + '~n' + 'ERROR ANULACION TICKET : ' + herror:VALUE .
                    END.
                END.
                ELSE 
                DO:
                        LERROR = TRUE.
                        CERROR = CERROR + '~n' + 'ERROR DE LLAMADA FUNCION DE ANULACION EN SAP ' .
                END.
            END.
        END.
    END.

    IF LERROR  THEN RETURN ERROR CERROR.

    RETURN ''.


    CATCH myerror AS PROGRESS.lang.apperror:
        RETURN ERROR myerror:getmessage(1).
    END CATCH.
    FINALLY :
        IF CERROR = ''  THEN 
            CERROR = 'SE ANULO CORRECTAMENTE EN SAP'.
        ELSE
            CERROR = 'NO SE ANULO. LOS ERRORES FUERON : ' + '~n' + CERROR.
    
        IF LERROR THEN CTITULO = 'Anulaci¢n de ingreso de Fruta con error '. ELSE CTITULO = 'Anulaci¢n de ingreso de Fruta OK '.
        
        CMES = 
       '____________________________________________________________________________________________________________________________' + '~n' +
       'Hora : ' + STRING(NOW, '99/99/99 hh:mm:ss')  + '~n' +
       'El ticket de Balanza : ' + string( pbalanza ) + '~n' +
       '             Pesada  : ' + STRING( ppesada  ) + '~n' +
       '             Ticket  : ' + STRING(pnroticket) + '~n' + 
       '     Orden Carga SAP : ' + VOC + '~n' +
       '____________________________________________________________________________________________________________________________' + '~n' +
        CERROR.    
    
        RUN ffw\procs\smtpmail58.p ("www.softsargentina.com" , 
/*                     "pdigonzelli@gmail.com,gprochaz@sa-sanmiguel.com,pedrom@sa-sanmiguel.com,rtralice@sa-sanmiguel.com,vtabernero@sa-sanmiguel.com,gabriel@sa-sanmiguel.com" , */
                     "pdigonzelli@gmail.com" ,
                     "ingresofruta@softsargentina.com" ,
                     "",
                     "" ,
                     "" ,
                     ctitulo ,
                     Cmes ,
                     "",
                     "text" ,
                      1,
                      YES,
                      "base64" ,
                      "ingresofruta@softsargentina.com" ,
                      "ingresofruta2011",
                     OUTPUT okay ,
                     OUTPUT cmes1) NO-ERROR.
        IF VALID-HANDLE(hfunctions) THEN
            hFunctions:connection:logoff().
    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-anulacionIngresoFrutaCMensaje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anulacionIngresoFrutaCMensaje Procedure 
PROCEDURE anulacionIngresoFrutaCMensaje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pbalanza AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pPesada AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER PNROTICKET AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER CMES AS CHARACTER NO-UNDO.



DEFINE VARIABLE hOCarga         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hPosicion       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE HDOC            AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE HERROR          AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE hFunctions      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hFunc           AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE xentrega_sap    AS CHARACTER NO-UNDO.

/* DEFINE VARIABLE CMES            AS CHARACTER NO-UNDO. */
DEFINE VARIABLE CMES1           AS CHARACTER NO-UNDO.
DEFINE VARIABLE LERROR          AS LOGICAL NO-UNDO.
DEFINE VARIABLE CERROR          AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKAY            AS LOGICAL NO-UNDO.
/* DEFINE VARIABLE CTITULO         AS CHARACTER NO-UNDO. */
DEFINE VARIABLE voc             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cdoc            AS CHARACTER NO-UNDO.



    LERROR = FALSE.
    CERROR = ''.

    CMES = ''.


    FIND balanza_pesadas WHERE  balanza_pesadas.id_balanza = pbalanza AND
                                balanza_pesadas.id_pesada  = pPesada  NO-LOCK NO-ERROR.

    IF NOT AVAILABLE balanza_pesadas THEN
    DO:
        LERROR = TRUE.
        CERROR = 'PESADA INEXISTENTE'.

    END.

    IF NOT LERROR  THEN
    DO:
        IF balanza_pesaDas.orden_carga_sap = ''  THEN RETURN ''.

        VOC = BALANZA_PESADAS.ORDEN_CARGA_SAP.

        FIND BALANZA_TICKETS WHERE  BALANZA_TICKETS.ID_BALANZA  =   PBALANZA AND
                                    BALANZA_TICKETS.ID_PESADA   =   PPESADA  AND
                                    BALANZA_TICKETS.NRO_TICKET  =   PNROTICKET NO-ERROR.

        IF NOT AVAILABLE BALANZA_TICKETS THEN
        DO:
            LERROR = TRUE.
            CERROR = 'REMITO INEXISTENTE'.
        END.
        ELSE
        DO:
            hFunctions = connectToSap().
            IF NOT VALID-HANDLE(hFunctions) THEN DO:
                LERROR = TRUE.
                CERROR = "ERROR DE CONECCION A INTERFACE".
            END.
            ELSE
            DO:

                hFunc  = hFunctions:ADD('BAPIANULACIONINGRESOS').
                IF VALID-HANDLE(hFunc) THEN DO:

                    hOCarga             = hFunc:exports('I_ORDEN_CARGA').
                    hposicion           = hFunc:exports('I_POSICION').
                    hDOC                = hFunc:IMPORTS('NUMDOC').
                    hERROR              = hFunc:IMPORTS('ERROR').

                    HOCARGA:VALUE = STRING(BALANZA_PESADAS.ORDEN_CARGA_SAP , '999999999999999').
                    HPOSICION:VALUE = STRING(BALANZA_TICKETS.NRO_TICKET, '9999').



                    IF NOT hFunc:CALL() THEN  
                    DO:
                        LERROR = TRUE.
                        CERROR = CERROR + '~n' + 'ERROR ANULACION TICKET: ' + STRING(BALANZA_TICKETS.NRO_TICKET).
                    END.

                    IF herror:VALUE <> 'OK' AND HERROR:VALUE <> 'VL302' AND herror:VALUE <> '' THEN 
                    DO:
                        LERROR = TRUE.
                        CERROR = CERROR + '~n' + 'ERROR ANULACION TICKET : ' + herror:VALUE .
                    END.
                    ELSE
                        ASSIGN LERROR = FALSE CERROR = herror:VALUE CDOC = hdoc:VALUE.
                END.
                ELSE 
                DO:
                        LERROR = TRUE.
                        CERROR = CERROR + '~n' + 'ERROR DE LLAMADA FUNCION DE ANULACION EN SAP ' .
                END.
            END.
        END.
    END.

    
    
    IF CERROR = 'OK' OR CERROR = 'VL302' OR CERROR = '' THEN LERROR = FALSE.
/*    
    IF LERROR  THEN DO:
        RETURN ERROR CERROR.
    END.
*/

    RETURN.


    CATCH myerror AS PROGRESS.lang.syserror:
        CERROR = MYERROR:GETMESSAGE(1).
        LERROR = TRUE.
/*        RETURN ERROR cerror. */
    END CATCH.
    FINALLY :
    
        IF VALID-HANDLE(hfunctions) THEN
            hFunctions:connection:logoff().
        
        
        IF NOT LERROR THEN 
            CERROR = 'SE ANULO CORRECTAMENTE EN SAP CON CODIGO: ' + CERROR + ' NRO DOCUMENTO ANULACION: '+ CDOC .
        ELSE
            CERROR = 'NO SE ANULO EN SAP. LOS ERRORES FUERON : ' + '~n' + CERROR.
    
    
        CMES = 
       '____________________________________________________________________________________________________________________________' + '~n' +
       'Hora : ' + STRING(NOW, '99/99/99 hh:mm:ss')  + '~n' +
       'El ticket de Balanza : ' + string( pbalanza ) + '~n' +
       '             Pesada  : ' + STRING( ppesada  ) + '~n' +
       '             Ticket  : ' + STRING(pnroticket) + '~n' + 
       '     Orden Carga SAP : ' + VOC + '~n' +
       '____________________________________________________________________________________________________________________________' + '~n' +
        CERROR.    

    
    /*    
            RUN ffw\procs\smtpmail58.p ("www.softsargentina.com" , 
    /*                     "pdigonzelli@gmail.com,gprochaz@sa-sanmiguel.com,pedrom@sa-sanmiguel.com,rtralice@sa-sanmiguel.com,vtabernero@sa-sanmiguel.com,gabriel@sa-sanmiguel.com" , */
                         "pdigonzelli@gmail.com" ,
                         "ingresofruta@softsargentina.com" ,
                         "",
                         "" ,
                         "" ,
                         ctitulo ,
                         Cmes ,
                         "",
                         "text" ,
                          1,
                          YES,
                          "base64" ,
                          "ingresofruta@softsargentina.com" ,
                          "ingresofruta2011",
                         OUTPUT okay ,
                         OUTPUT cmes1) NO-ERROR. */

    END FINALLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-anulacionPesadaSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anulacionPesadaSap Procedure 
PROCEDURE anulacionPesadaSap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pOrdenCargaSap AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pITEM          AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER cmes           AS CHARACTER NO-UNDO.


DEFINE VARIABLE hOCarga         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hPosicion       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE HDOC            AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE HERROR          AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE hFunctions      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hFunc           AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE xentrega_sap    AS CHARACTER NO-UNDO.

/* DEFINE VARIABLE CMES            AS CHARACTER NO-UNDO. */
DEFINE VARIABLE CMES1           AS CHARACTER NO-UNDO.
DEFINE VARIABLE LERROR          AS LOGICAL NO-UNDO.
DEFINE VARIABLE CERROR          AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKAY            AS LOGICAL NO-UNDO.
/* DEFINE VARIABLE CTITULO         AS CHARACTER NO-UNDO. */
DEFINE VARIABLE voc             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cdoc            AS CHARACTER NO-UNDO.



    LERROR = FALSE.
    CERROR = ''.


    voc = STRING(pordencargasap).

    hFunctions = connectToSap().
    IF NOT VALID-HANDLE(hFunctions) THEN DO:
        LERROR = TRUE.
        CERROR = "ERROR DE CONECCION A INTERFACE".
    END.
    ELSE
    DO:

        hFunc  = hFunctions:ADD('BAPIANULACIONINGRESOS').
        IF VALID-HANDLE(hFunc) THEN DO:

            hOCarga             = hFunc:exports('I_ORDEN_CARGA').
            hposicion           = hFunc:exports('I_POSICION').
            hDOC                = hFunc:IMPORTS('NUMDOC').
            hERROR              = hFunc:IMPORTS('ERROR').

            HOCARGA:VALUE = STRING(pORDENCARGASAP , '999999999999999').
            HPOSICION:VALUE = STRING(pITEM, '9999').



            IF NOT hFunc:CALL() THEN  
            DO:
                LERROR = TRUE.
                CERROR = CERROR + '~n' + 'ERROR ANULACION TICKET: ' + STRING(BALANZA_TICKETS.NRO_TICKET).
            END.
           

            IF herror:VALUE <> 'OK' AND HERROR:VALUE <> 'VL302' AND herror:VALUE <> '' THEN 
            DO:
                LERROR = TRUE.
                CERROR = CERROR + '~n' + 'ERROR ANULACION TICKET : ' + herror:VALUE .
            END.
            ELSE
                ASSIGN LERROR = FALSE CERROR = herror:VALUE CDOC = hdoc:VALUE.
        END.
        ELSE 
        DO:
                LERROR = TRUE.
                CERROR = CERROR + '~n' + 'ERROR DE LLAMADA FUNCION DE ANULACION EN SAP ' .
        END.
    END.


    
    IF CERROR = 'OK' OR CERROR = 'VL302' OR CERROR = '' THEN LERROR = FALSE.
    

    IF LERROR  THEN DO:
        RETURN ERROR CERROR.
    END.
    RETURN ''.


    CATCH myerror AS PROGRESS.lang.apperror:
        RETURN ERROR myerror:getmessage(1).
    END CATCH.
    FINALLY :
    
        IF VALID-HANDLE(hfunctions) THEN
            hFunctions:connection:logoff().
        
        
        IF NOT LERROR THEN 
            CERROR = 'SE ANULO CORRECTAMENTE EN SAP CON CODIGO: ' + CERROR + 'NRO DOCUMENTO: '+ CDOC .
        ELSE
            CERROR = 'NO SE ANULO EN SAP. LOS ERRORES FUERON : ' + '~n' + CERROR.
    
    
        CMES = 
       '____________________________________________________________________________________________________________________________' + '~n' +
       'Hora : ' + STRING(NOW, '99/99/99 hh:mm:ss')  + '~n' +
       '     Orden Carga SAP : ' + voc + '~n' +
       '____________________________________________________________________________________________________________________________' + '~n' +
        CERROR.    


    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-anulaPalletSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anulaPalletSap Procedure 
PROCEDURE anulaPalletSap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER  pcMatLoteSap AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER  pdfecha      AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER cStatus AS CHARACTER.

  
  
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.

  DEFINE VARIABLE   hDocu       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE   hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRow        AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRows       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFecha      AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR iCodigo AS INTEGER NO-UNDO.
  DEFINE VAR cCentro AS CHARACTER NO-UNDO.





  DEFINE VARIABLE inum AS INTEGER NO-UNDO.
  DEFINE VARIABLE cestado AS CHARACTER NO-UNDO.

  DEFINE VAR par AS INTEGER INITIAL 0.

  DEFINE VARIABLE   cFecha          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cPallet         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cMaterial       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cFechaProd      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cMatLoteSap     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   hMatLoteSap     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFechaProd      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hEstado         AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   xcentro         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   xalmacen        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cCodBarra       AS CHARACTER NO-UNDO.

  hFunctions = connectToSap().

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc         = hFunctions:ADD('BAPI_PALLET_ANULAR').

      hMatLoteSap           = hFunc:exports('MATLOTE').  
      hFechaProd            = hFunc:exports('FECHA').  
      hEstado               = hFUnc:ImportS('ESTADO').

      cFechaProd =  TRIM(STRING(YEAR(PDFECHA),'9999')) + trim(string(MONTH(PDFECHA),'99')) + TRIM(STRING(DAY(PDFECHA),'99')).
      
      pcMatLoteSap = pcMatLoteSap + FILL ( '>' , 28 - LENGTH(pcMatLoteSap) ). 
      hFechaProd:VALUE = cFechaProd. 
      hMatloteSap:VALUE = pcMatLoteSap.

      IF hFunc:CALL() THEN  
      DO:
         IF hEstado:VALUE <> 'T' THEN 
         DO:
             hFunctions:connection:logoff().
             cStatus = 'Error en Anulacion de Pallet - valor ' + string(hEstado:VALUE) .
             UNDO , RETURN ERROR.
         END.
         ELSE
             cStatus = 'OK'.
      END. 
          ELSE 
          DO: 
              cStatus = 'Error interface de pallet'. 
              hFunctions:connection:logoff().
              UNDO , RETURN ERROR. 
          END.
          hFunctions:connection:logoff().
  END.
  ELSE DO: 
      cstatus = 'error de funcion SAP'. 
      UNDO , RETURN ERROR. 
  END.
  IF VALID-HANDLE(hfunctions) THEN  hFunctions:connection:logoff().
  RETURN.
  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-anulaPalletSapCalidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anulaPalletSapCalidad Procedure 
PROCEDURE anulaPalletSapCalidad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER  pcMatLoteSap AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER  pdfecha      AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER cStatus AS CHARACTER.

  
  
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.

  DEFINE VARIABLE   hDocu       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE   hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRow        AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRows       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFecha      AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR iCodigo AS INTEGER NO-UNDO.
  DEFINE VAR cCentro AS CHARACTER NO-UNDO.





  DEFINE VARIABLE inum AS INTEGER NO-UNDO.
  DEFINE VARIABLE cestado AS CHARACTER NO-UNDO.

  DEFINE VAR par AS INTEGER INITIAL 0.

  DEFINE VARIABLE   cFecha          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cPallet         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cMaterial       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cFechaProd      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cMatLoteSap     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   hMatLoteSap     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFechaProd      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hEstado         AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   xcentro         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   xalmacen        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cCodBarra       AS CHARACTER NO-UNDO.

  hFunctions = connectToSapCalidad().

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc         = hFunctions:ADD('BAPI_PALLET_ANULAR').

      hMatLoteSap           = hFunc:exports('MATLOTE').  
      hFechaProd            = hFunc:exports('FECHA').  
      hEstado               = hFUnc:ImportS('ESTADO').

      cFechaProd =  TRIM(STRING(YEAR(PDFECHA),'9999')) + trim(string(MONTH(PDFECHA),'99')) + TRIM(STRING(DAY(PDFECHA),'99')).
      
      pcMatLoteSap = pcMatLoteSap + FILL ( '>' , 28 - LENGTH(pcMatLoteSap) ). 
      hFechaProd:VALUE = cFechaProd. 
      hMatloteSap:VALUE = pcMatLoteSap.

      IF hFunc:CALL() THEN  
      DO:
         IF hEstado:VALUE <> 'T' THEN 
         DO:
             hFunctions:connection:logoff().
             cStatus = 'Error en Anulacion de Pallet - valor ' + string(hEstado:VALUE) .
             UNDO , RETURN ERROR.
         END.
         ELSE
             cStatus = 'OK'.
      END. 
          ELSE 
          DO: 
              cStatus = 'Error interface de pallet'. 
              hFunctions:connection:logoff().
              UNDO , RETURN ERROR. 
          END.
          hFunctions:connection:logoff().
  END.
  ELSE DO: 
      cstatus = 'error de funcion SAP'. 
      UNDO , RETURN ERROR. 
  END.
  IF VALID-HANDLE(hfunctions) THEN  hFunctions:connection:logoff().
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-anulaPalletSapPrueba) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anulaPalletSapPrueba Procedure 
PROCEDURE anulaPalletSapPrueba :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER  pcMatLoteSap AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER  pdfecha      AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER cStatus AS CHARACTER.

  
  
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.

  DEFINE VARIABLE   hDocu       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE   hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRow        AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRows       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFecha      AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR iCodigo AS INTEGER NO-UNDO.
  DEFINE VAR cCentro AS CHARACTER NO-UNDO.



  DEFINE VARIABLE inum AS INTEGER NO-UNDO.
  DEFINE VARIABLE cestado AS CHARACTER NO-UNDO.

  DEFINE VAR par AS INTEGER INITIAL 0.

  DEFINE VARIABLE   cFecha              AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cPallet             AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cMaterial           AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cFechaProd          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cMatLoteSap         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   hMatLoteSap         AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFechaProd          AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hEstado             AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   xcentro             AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   xalmacen            AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cCodBarra           AS CHARACTER NO-UNDO.

  /*
  FIND FIRST pallets WHERE pallets.id_pallet_sap = SUBSTRING(pcMatLoteSap , LENGTH (pcMatLoteSap) - 17) NO-LOCK NO-ERROR.
  IF NOT AVAILABLE pallets  THEN DO:
      cstatus = 'Pallet inexistente'.
      RETURN ERROR.
  END.
  */

  hFunctions = connectToSapCalidad().

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      
      hFunc         = hFunctions:ADD('BAPI_PALLET_ANULAR').

      hMatLoteSap           = hFunc:exports('MATLOTE').  
      hFechaProd            = hFunc:exports('FECHA').  
      hEstado               = hFUnc:ImportS('ESTADO').

      cFechaProd =  TRIM(STRING(YEAR(PDFECHA),'9999')) + trim(string(MONTH(PDFECHA),'99')) + TRIM(STRING(DAY(PDFECHA),'99')).
      
      pcMatLoteSap = pcMatLoteSap + FILL ( '>' , 28 - LENGTH(pcMatLoteSap) ). 
      hFechaProd:VALUE = cFechaProd. 
      hMatloteSap:VALUE = pcMatLoteSap.

      /*
      OUTPUT TO Z:\TEMP\PAR1.PAB.
      EXPORT DELIMITER ";" HFECHAPROD:VALUE() HMATLOTESAP:VALUE().
      OUTPUT CLOSE. */

      IF hFunc:CALL() THEN  
      DO:
/*         MESSAGE    hestado:VALUE 
                    hMatLoteSap:VALUE 
                    hFechaProd:VALUE VIEW-AS ALERT-BOX. */
         IF hEstado:VALUE <> 'T' THEN 
         DO:
             hFunctions:connection:logoff().
             cStatus = 'Error en Anulacion de Pallet - valor ' + string(hEstado:VALUE) .
             UNDO , RETURN ERROR.
         END.
         ELSE
             cStatus = 'OK'.
      END. 
          ELSE 
          DO: 
              cStatus = 'Error interface de pallet'. 
              hFunctions:connection:logoff().
              UNDO , RETURN ERROR. 
          END.
          hFunctions:connection:logoff().
  END.
  ELSE DO: 
      cstatus = 'error de funcion SAP'. 
      UNDO , RETURN ERROR. 
  END.

  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-balanzaPesadaTablero) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE balanzaPesadaTablero Procedure 
PROCEDURE balanzaPesadaTablero :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pbalanza AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pPesada AS INTEGER NO-UNDO.

DEFINE BUFFER bp FOR balanza_pesadas.

FIND bp WHERE   bp.id_balanza   = pbalanza AND
                bp.id_pesada    = ppesada .

CREATE balanza_pesadas_tablero.
BUFFER-COPY bp TO  balanza_pesadas_tablero.
balanza_pesadas_tablero.modificacion = NOW.

CATCH myerror AS PROGRESS.lang.apperror:
    UNDO , RETURN ERROR myerror:getmessage(1).
END CATCH.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-balanzaTicketsToSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE balanzaTicketsToSap Procedure 
PROCEDURE balanzaTicketsToSap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pbalanza AS INTEGER.
DEFINE INPUT PARAMETER ppesada AS INTEGER.
DEFINE INPUT PARAMETER pproceso AS CHARACTER.
DEFINE INPUT PARAMETER pserie   AS INTEGER NO-UNDO.

DEFINE VARIABLE cparametros AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.



DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hparametros AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hProceso    AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hTicket     AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE xservicio     AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcorte        AS CHARACTER NO-UNDO.
DEFINE VARIABLE xarticulo     AS CHARACTER NO-UNDO FORMAT 'x(18)'.
DEFINE VARIABLE xenvase       AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcolor        AS CHARACTER NO-UNDO.
DEFINE VARIABLE xlote         AS CHARACTER NO-UNDO.
DEFINE VARIABLE xmercado      AS CHARACTER NO-UNDO.
DEFINE VARIABLE xalmacen      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cticket       AS CHARACTER NO-UNDO.
DEFINE VARIABLE xunidadMedida AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcant         AS CHARACTER NO-UNDO.
DEFINE VARIABLE xlinea        AS CHARACTER NO-UNDO.
DEFINE VARIABLE xCentro       AS CHARACTER NO-UNDO.
DEFINE VARIABLE vlote         AS CHARACTER NO-UNDO.
DEFINE VARIABLE xestatus      AS CHARACTER NO-UNDO.

DEFINE VARIABLE hLote AS COM-HANDLE NO-UNDO.

DEFINE VAR vorigen    AS CHARACTER NO-UNDO.


/*
DEFINE VARIABLE pcode   AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE VARIABLE porden  AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE pposic  AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE perror  AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE ptexto  AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE ptipoe  AS CHARACTER FORMAT "x" NO-UNDO.
*/

DEFINE VARIABLE pcode     AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE VARIABLE porden    AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE pposic    AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE perror    AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE ptexto    AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE ptipoe    AS CHARACTER FORMAT "x" NO-UNDO.



DEFINE BUFFER bp FOR balanza_pesadas.



FIND bp WHERE  bp.id_balanza = pbalanza AND
               bp.id_pesada = pPesada  NO-LOCK NO-ERROR.

IF NOT AVAILABLE bp  THEN RETURN ERROR "NO ENCUENTRA LA PESADA".

hFunctions = connectToSap().

IF VALID-HANDLE(hFunctions) THEN DO ON ERROR UNDO , RETURN ERROR:
    

    hFunc  = hFunctions:ADD('BAPI_ZTMM_MOV_FLETE_CREATE_D').
    hparametros     = hFunc:exports('LPARAMETROS').
    hproceso        = hFunc:exports('ICODIGOPROCESO').
    hTicket         = hFunc:exports('ITICKET').
    hLote           = hFunc:imports('OLOTE').
/*    
    i = 0.
*/

    FOR EACH balanza_tickets    OF bp WHERE balanza_tickets.cod_barra_sap = ''.

        cparametros = ''.
/*        
        i = i + 1.
*/
        
        i = balanza_tickets.nro_ticket.

        FIND tipos_servicios    OF balanza_tickets NO-LOCK NO-ERROR.
        FIND tipo_cosecha       OF balanza_tickets NO-LOCK NO-ERROR. 
        FIND envases_prod       OF balanza_tickets NO-LOCK NO-ERROR.
        FIND colores            OF balanza_tickets NO-LOCK NO-ERROR.
        FIND lote               OF balanza_tickets NO-LOCK NO-ERROR.
        FIND proveedores        OF balanza_tickets NO-LOCK NO-ERROR.
        IF tipo_cosecha.id_tipo_cosecha <> 1  THEN
        DO:
            NEXT.
        END.
        /**** anterior
        IF tipo_cosecha.id_tipo_cosecha = 0 /* descarte */ OR tipo_cosecha.id_tipo_cosecha >= 4 /* procesado y otros */ THEN NEXT.
        *******/

        FIND productos_terminados WHERE productos_terminados.id_articulo = balanza_tickets.id_materia_prima NO-LOCK NO-ERROR.

        FIND r_envases_prod WHERE r_envases_prod.id_envase = balanza_tickets.id_envase AND
                                  r_envases_prod.id_articulo = balanza_tickets.id_materia_prima NO-LOCK NO-ERROR.
        
        
        IF AVAILABLE tipos_servicios    THEN  xservicio = tipos_servicios.id_tipo_servicio_sap. ELSE xservicio = '00'.
        IF AVAILABLE tipo_cosecha       THEN  xcorte = tipo_cosecha.id_tipo_cosecha_sap. ELSE xcorte = '00'.
        IF AVAILABLE envases_prod       THEN  xenvase = envases_prod.id_envase_sap. ELSE ''.
        IF AVAILABLE colores            THEN  xcolor = STRING(colores.id_color_sap) . ELSE xcolor = ''.
        IF AVAILABLE lote               THEN  xlote = STRING(lote.DESCripcion). ELSE xlote = 'SL'.
        FIND origenes OF lote NO-LOCK NO-ERROR.

        /**** almacen destino y numeracion de lote ****/
        /*** envases prod unidad de medida base ***/

        IF AVAILABLE productos_terminados THEN
            CASE productos_terminados.id_articulo:
                WHEN 1 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 2 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 3 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 4 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 5 THEN 
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 6 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 7 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'KG'.
                    END CASE.
            END CASE.
        ELSE
            CASE tipo_cosecha.id_tipo_cosecha.
                WHEN 4 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                WHEN 1 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'BIN'.
                OTHERWISE   ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
            END CASE.



        IF balanza_tickets.UNION_europea THEN  xmercado = 'UE' . ELSE xmercado = 'NOUE'.

        FIND balanzas OF balanza_tickets NO-LOCK NO-ERROR.

        /*** en caso de industria determinar el almacen ***/

        cTicket = string(bp.id_pesada_sap , '999999999999' ).
        
        xcentro = SUBSTRING(balanzas.id_balanza_sap,1,4).
        IF pproceso <> 'IN' THEN xalmacen = SUBSTRING(balanzas.id_balanza_sap,5,4).
        ELSE                     xalmacen = /*SUBSTRING(balanzas.id_balanza_sap,5,4)*/ string(bp.id_pesada_ctf).
        
        /* XALMACEN = '1600'.*/

        CASE xcentro:
            WHEN 'A200' THEN
                CASE  xalmacen:
                    WHEN '2009' THEN  xlinea = 'D'.
                    WHEN '2001' THEN  xlinea = 'L'.
                    WHEN '2002' THEN  xlinea = 'S'.
                END CASE.
        END CASE.

        IF pproceso = 'IN' THEN xcant = STRING(bp.peso_entrada). 
        ELSE 
        DO:
           IF tipo_cosecha.id_tipo_cosecha = 1 THEN
              xcant = STRING(produccion.balanza_tickets.cant_env_entrada).
           ELSE
              xcant = STRING(balanza_tickets.peso_neto_ticket).
        END.
        
/*
        MESSAGE 'LLEGO ACA' VIEW-AS ALERT-BOX.
*/

        porden = ''.
        RUN balanzaTicketsToSapValidacionCMsg 
            (INPUT balanza_tickets.id_balanza,
             INPUT balanza_tickets.id_pesada,
             INPUT balanza_tickets.nro_ticket,
             OUTPUT pcode,
             OUTPUT porden,
             OUTPUT pposic,
             OUTPUT perror,
             OUTPUT ptexto,
             OUTPUT ptipoe) NO-ERROR.

        IF porden <> '' THEN DO:
            ASSIGN balanza_tickets.orden_compra_sap        = porden
                   balanza_tickets.pos_orden_compra_sap    = STRING(INTEGER(pposic),"99999").
        END.

        IF balanza_tickets.orden_compra_sap = '' THEN
            xestatus    = "P".
        ELSE
            xestatus    = "O".

        /***
        MESSAGE "orden carga" bp.orden_carga_sap
                SKIP
                "tipo_cosecha" tipo_cosecha.id_tipo_cosecha
                SKIP
                "proceso" pproceso "     xcant" xcant
                VIEW-AS ALERT-BOX.
        ***/

        cparametros = bp.orden_carga_sap + '|' + STRING(i,'9999') + '|' + balanza_tickets.codigo_trazabilidad + '|' +
                      STRING(YEAR(balanza_tickets.fecha_cosecha) , '9999') + STRING(MONTH(balanza_tickets.fecha_cosecha) , '99') +
                      STRING(DAY(balanza_tickets.fecha_cosecha) , '99') + '|' + xservicio + '|' + xcorte + '|' + xarticulo + '|' + xenvase + '|' +
                      xcolor + '|' + xcant + '|' + xunidadmedida + '|' + upper(xcentro) 
                      + '|' + xalmacen + '|'+ upper(SUBSTRING(origenes.id_origen_sap,1,4)) 
                      + '|' + SUBSTRING(origenes.id_origen_sap,5,4) + '|||' + 
                      STRING(balanza_tickets.nro_remito, '9999-99999999')+ '|' + STRING(balanza_tickets.peso_neto_ticket) + '||' +
                      xlote + '|' + balanza_tickets.orden_compra_sap + '|||' + xmercado + '|' + xlinea + '|' + SUBSTRING(STRING(proveedores.cuit),1,10) +
                      '|P||' + balanza_tickets.pos_orden_compra_sap + '|' + xestatus + '|P||P|||P||||||||' + string(balanza_tickets.nro_partida).

        

        /*        
        OUTPUT TO e:\temp\ztmm_mov_flete_d.txt APPEND.
        PUT UNFORMATTED
            cparametros.
        PUT SKIP.
        OUTPUT CLOSE.
        */
        
        
/*         MESSAGE cparametros 
                skip
                "proceso" pproceso
                SKIP
                "ticket" cticket
                VIEW-AS ALERT-BOX. */

        hparametros:VALUE = cparametros.
        hPRoceso:VALUE = pproceso.
        hticket:VALUE = cticket.          

        IF NOT hFunc:CALL() THEN DO:
            hFunctions:connection:logoff().
            UNDO, RETURN ERROR 'ERROR DE ITEM ' .
        END.


        IF  hLote:VALUE = 1 THEN DO:
            hFunctions:connection:logoff().
            UNDO , RETURN ERROR 'ERROR DE LOTE EN ITEM'.
        END.
       
        vlote = trim(hLote:value).            
        balanza_tickets.cod_barra_sap = TRIM(xarticulo) + TRIM(vlote).
        
        vorigen = SOURCE-PROCEDURE:FILE-NAME.
        

        CREATE balanza_tickets2sap.
        BUFFER-COPY balanza_tickets TO balanza_tickets2sap.
        
        ASSIGN  balanza_tickets2sap.serie    = pserie
                balanza_tickets2sap.modificacion = NOW
                balanza_tickets2sap.origen = vorigen.
        
                    
    END.
END.


CATCH myerror AS PROGRESS.lang.apperror:
    UNDO , RETURN ERROR myerror:getmessage(1).
END.
FINALLY.
    IF VALID-HANDLE(HFUNCTIONS) THEN     hFunctions:connection:logoff().
END FINALLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-balanzaTicketsToSapPrueba) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE balanzaTicketsToSapPrueba Procedure 
PROCEDURE balanzaTicketsToSapPrueba :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pbalanza AS INTEGER.
DEFINE INPUT PARAMETER ppesada AS INTEGER.
DEFINE INPUT PARAMETER pproceso AS CHARACTER.
DEFINE INPUT PARAMETER pserie   AS INTEGER NO-UNDO.

DEFINE VARIABLE cparametros AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.



DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hparametros AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hProceso    AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hTicket     AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE xservicio     AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcorte        AS CHARACTER NO-UNDO.
DEFINE VARIABLE xarticulo     AS CHARACTER NO-UNDO FORMAT 'x(18)'.
DEFINE VARIABLE xenvase       AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcolor        AS CHARACTER NO-UNDO.
DEFINE VARIABLE xlote         AS CHARACTER NO-UNDO.
DEFINE VARIABLE xmercado      AS CHARACTER NO-UNDO.
DEFINE VARIABLE xalmacen      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cticket       AS CHARACTER NO-UNDO.
DEFINE VARIABLE xunidadMedida AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcant         AS CHARACTER NO-UNDO.
DEFINE VARIABLE xlinea        AS CHARACTER NO-UNDO.
DEFINE VARIABLE xCentro       AS CHARACTER NO-UNDO.
DEFINE VARIABLE vlote         AS CHARACTER NO-UNDO.
DEFINE VARIABLE xestatus      AS CHARACTER NO-UNDO.

DEFINE VARIABLE hLote AS COM-HANDLE NO-UNDO.

DEFINE VAR vorigen    AS CHARACTER NO-UNDO.


/*
DEFINE VARIABLE pcode   AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE VARIABLE porden  AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE pposic  AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE perror  AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE ptexto  AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE ptipoe  AS CHARACTER FORMAT "x" NO-UNDO.
*/

DEFINE VARIABLE pcode     AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE VARIABLE porden    AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE pposic    AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE perror    AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE ptexto    AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE ptipoe    AS CHARACTER FORMAT "x" NO-UNDO.


DEFINE BUFFER bp FOR balanza_pesadas.


FIND bp WHERE  bp.id_balanza = pbalanza AND
                            bp.id_pesada = pPesada  NO-LOCK NO-ERROR.

IF NOT AVAILABLE bp  THEN RETURN ERROR "NO ENCUENTRA LA PESADA".

hFunctions = connectToSapCalidad().

IF VALID-HANDLE(hFunctions) THEN DO ON ERROR UNDO , RETURN ERROR:
    

    hFunc  = hFunctions:ADD('BAPI_ZTMM_MOV_FLETE_CREATE_D').
    hparametros     = hFunc:exports('LPARAMETROS').
    hproceso        = hFunc:exports('ICODIGOPROCESO').
    hTicket         = hFunc:exports('ITICKET').
    hLote           = hFunc:imports('OLOTE').
/*    
    i = 0.
*/

    FOR EACH balanza_tickets    OF bp WHERE balanza_tickets.cod_barra_sap = ''.

        cparametros = ''.
/*        
        i = i + 1.
*/
        
        i = balanza_tickets.nro_ticket.

        FIND tipos_servicios    OF balanza_tickets NO-LOCK NO-ERROR.
        FIND tipo_cosecha       OF balanza_tickets NO-LOCK NO-ERROR. 
        FIND envases_prod       OF balanza_tickets NO-LOCK NO-ERROR.
        FIND colores            OF balanza_tickets NO-LOCK NO-ERROR.
        FIND lote               OF balanza_tickets NO-LOCK NO-ERROR.
        FIND proveedores        OF balanza_tickets NO-LOCK NO-ERROR.

        IF tipo_cosecha.id_tipo_cosecha = 0 /* descarte */ OR tipo_cosecha.id_tipo_cosecha >= 4 /* procesado y otros */ THEN NEXT.
        FIND productos_terminados WHERE productos_terminados.id_articulo = balanza_tickets.id_materia_prima NO-LOCK NO-ERROR.

        FIND r_envases_prod WHERE r_envases_prod.id_envase = balanza_tickets.id_envase AND
                                  r_envases_prod.id_articulo = balanza_tickets.id_materia_prima NO-LOCK NO-ERROR.
        
        
        IF AVAILABLE tipos_servicios    THEN  xservicio = tipos_servicios.id_tipo_servicio_sap. ELSE xservicio = '00'.
        IF AVAILABLE tipo_cosecha       THEN  xcorte = tipo_cosecha.id_tipo_cosecha_sap. ELSE xcorte = '00'.
        IF AVAILABLE envases_prod       THEN  xenvase = envases_prod.id_envase_sap. ELSE ''.
        IF AVAILABLE colores            THEN  xcolor = STRING(colores.id_color_sap) . ELSE xcolor = ''.
        IF AVAILABLE lote               THEN  xlote = STRING(lote.DESCripcion). ELSE xlote = 'SL'.
        FIND origenes OF lote NO-LOCK NO-ERROR.

        /**** almacen destino y numeracion de lote ****/
        /*** envases prod unidad de medida base ***/

        IF AVAILABLE productos_terminados THEN
            CASE productos_terminados.id_articulo:
                WHEN 1 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 2 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 3 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 4 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 5 THEN 
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 6 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 7 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'KG'.
                    END CASE.
            END CASE.
        ELSE
            CASE tipo_cosecha.id_tipo_cosecha.
                WHEN 4 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                WHEN 1 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'BIN'.
                OTHERWISE   ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
            END CASE.



        IF balanza_tickets.UNION_europea THEN  xmercado = 'UE' . ELSE xmercado = 'NOUE'.

        FIND balanzas OF balanza_tickets NO-LOCK NO-ERROR.

        /*** en caso de industria determinar el almacen ***/

        cTicket = string(bp.id_pesada_sap , '999999999999' ).
        
        xcentro = SUBSTRING(balanzas.id_balanza_sap,1,4).
        IF pproceso <> 'IN' THEN xalmacen = SUBSTRING(balanzas.id_balanza_sap,5,4).
        ELSE                     xalmacen = /*SUBSTRING(balanzas.id_balanza_sap,5,4)*/ string(bp.id_pesada_ctf).
        
        /* XALMACEN = '1600'.*/

        CASE xcentro:
            WHEN 'A200' THEN
                CASE  xalmacen:
                    WHEN '2009' THEN  xlinea = 'D'.
                    WHEN '2001' THEN  xlinea = 'L'.
                    WHEN '2002' THEN  xlinea = 'S'.
                END CASE.
        END CASE.

        IF pproceso = 'IN' THEN xcant = STRING(bp.peso_entrada). 
        ELSE 
        DO:
           IF tipo_cosecha.id_tipo_cosecha = 1 THEN
              xcant = STRING(produccion.balanza_tickets.cant_env_entrada).
           ELSE
              xcant = STRING(balanza_tickets.peso_neto_ticket).
        END.
        
/*
        MESSAGE 'LLEGO ACA' VIEW-AS ALERT-BOX.
*/

        porden = ''.
        RUN balanzaTicketsToSapValidacionCMsgPrueba 
            (INPUT balanza_tickets.id_balanza,
             INPUT balanza_tickets.id_pesada,
             INPUT balanza_tickets.nro_ticket,
             OUTPUT pcode,
             OUTPUT porden,
             OUTPUT pposic,
             OUTPUT perror,
             OUTPUT ptexto,
             OUTPUT ptipoe) NO-ERROR.

        IF porden <> '' THEN DO:
            ASSIGN balanza_tickets.orden_compra_sap        = porden
                   balanza_tickets.pos_orden_compra_sap    = STRING(INTEGER(pposic),"99999").
        END.

        IF balanza_tickets.orden_compra_sap = '' THEN
            xestatus    = "P".
        ELSE
            xestatus    = "O".

        /***
        MESSAGE "orden carga" bp.orden_carga_sap
                SKIP
                "tipo_cosecha" tipo_cosecha.id_tipo_cosecha
                SKIP
                "proceso" pproceso "     xcant" xcant
                VIEW-AS ALERT-BOX.
        ***/

        cparametros = bp.orden_carga_sap + '|' + STRING(i,'9999') + '|' + balanza_tickets.codigo_trazabilidad + '|' +
                      STRING(YEAR(balanza_tickets.fecha_cosecha) , '9999') + STRING(MONTH(balanza_tickets.fecha_cosecha) , '99') +
                      STRING(DAY(balanza_tickets.fecha_cosecha) , '99') + '|' + xservicio + '|' + xcorte + '|' + xarticulo + '|' + xenvase + '|' +
                      xcolor + '|' + xcant + '|' + xunidadmedida + '|' + upper(xcentro) 
                      + '|' + xalmacen + '|'+ upper(SUBSTRING(origenes.id_origen_sap,1,4)) 
                      + '|' + SUBSTRING(origenes.id_origen_sap,5,4) + '|||' + 
                      STRING(balanza_tickets.nro_remito, '9999-99999999')+ '|' + STRING(balanza_tickets.peso_neto_ticket) + '||' +
                      xlote + '|' + balanza_tickets.orden_compra_sap + '|||' + xmercado + '|' + xlinea + '|' + SUBSTRING(STRING(proveedores.cuit),1,10) +
                      '|P||' + balanza_tickets.pos_orden_compra_sap + '|' + xestatus + '|P||P|||P||||||||' + string(balanza_tickets.nro_partida).

        

        /*        
        OUTPUT TO e:\temp\ztmm_mov_flete_d.txt APPEND.
        PUT UNFORMATTED
            cparametros.
        PUT SKIP.
        OUTPUT CLOSE.
        */
        
        
/*         MESSAGE cparametros 
                skip
                "proceso" pproceso
                SKIP
                "ticket" cticket
                VIEW-AS ALERT-BOX. */

        hparametros:VALUE = cparametros.
        hPRoceso:VALUE = pproceso.
        hticket:VALUE = cticket.          

        IF NOT hFunc:CALL() THEN DO:
            hFunctions:connection:logoff().
            UNDO, RETURN ERROR 'ERROR DE ITEM ' .
        END.


        IF  hLote:VALUE = 1 THEN DO:
            hFunctions:connection:logoff().
            UNDO , RETURN ERROR 'ERROR DE LOTE EN ITEM'.
        END.
       
        vlote = trim(hLote:value).            
        balanza_tickets.cod_barra_sap = TRIM(xarticulo) + TRIM(vlote).
        
        vorigen = SOURCE-PROCEDURE:FILE-NAME.
        

        CREATE balanza_tickets2sap.
        BUFFER-COPY balanza_tickets TO balanza_tickets2sap.
        
        ASSIGN  balanza_tickets2sap.serie    = pserie
                balanza_tickets2sap.modificacion = NOW
                balanza_tickets2sap.origen = vorigen.
        
                    
    END.
    hFunctions:connection:logoff().
END.


CATCH myerror AS PROGRESS.lang.apperror:
    UNDO , RETURN ERROR myerror:getmessage(1).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-balanzaTicketsToSapValidacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE balanzaTicketsToSapValidacion Procedure 
PROCEDURE balanzaTicketsToSapValidacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pbalanza AS INTEGER.
    DEFINE INPUT PARAMETER ppesada  AS INTEGER.
    DEFINE INPUT PARAMETER pticket  AS INTEGER.
    DEFINE OUTPUT PARAMETER pcode   AS CHARACTER FORMAT "x".
    DEFINE OUTPUT PARAMETER porden  AS CHARACTER FORMAT "x(10)".
    DEFINE OUTPUT PARAMETER pposic  AS CHARACTER FORMAT "x(10)".
    DEFINE OUTPUT PARAMETER perror  AS CHARACTER FORMAT "x(3)".
    DEFINE OUTPUT PARAMETER ptexto  AS CHARACTER FORMAT "x(100)".
    DEFINE OUTPUT PARAMETER ptipoe  AS CHARACTER FORMAT "x".
    
    
    DEFINE VARIABLE cparametros AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStatus AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    
    
    DEFINE VARIABLE hFunctions  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hFunc       AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hparametros AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hProveedor  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hTipoCorte  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hServicio   AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hArticulo   AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hFinca      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hCentro     AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hCode       AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hOrden      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hPosic      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hError      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hTexto      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hTError     AS COM-HANDLE NO-UNDO.
    
    
    DEFINE VARIABLE xcorte      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE xservicio   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE xarticulo   AS CHARACTER NO-UNDO FORMAT 'x(18)'.
    DEFINE VARIABLE xCentro     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE xunidadMedida AS CHARACTER NO-UNDO.
    

    DEFINE BUFFER BT FOR BALANZA_TICKETS.

    
    FIND bt WHERE
        bt.id_balanza  = pbalanza  AND
        bt.id_pesada   = pPesada   AND
        bt.nro_ticket  = pticket   NO-LOCK NO-ERROR.
    
    IF NOT AVAILABLE bt THEN
    do:
     message "NO HAY BALANZA_TICKETS !!" VIEW-AS ALERT-BOX.
     RETURN ERROR.
    end.
    
    
    hFunctions = connectToSap().
    
    IF NOT VALID-HANDLE(hFunctions) THEN DO:
        RETURN ERROR "error de conecci¢n".
    END.
    
    DO:
        hFunc  = hFunctions:ADD('BAPIVALIDAINGRESOS').
        /*
        hparametros     = hFunc:exports('LPARAMETROS').
        */
        hProveedor      = hFunc:exports('I_PROVEEDOR').
        hTipoCorte      = hFunc:exports('I_TIPO_CORTE').
        hServicio       = hFunc:exports('I_TIPO_SERVICIO').
        hArticulo       = hFunc:exports('I_MATERIAL').
        hFinca          = hFunc:exports('I_FINCA').
        hCentro         = hFunc:exports('I_CENTRO_RECEPCION').
        
        hCode           = hFunc:imports('OK_CODE').
        hOrden          = hFunc:imports('ORDEN_COMPRA').
        hPosic          = hFunc:imports('POSICION').
        hError          = hFunc:imports('COD_ERROR').
        hTexto          = hFunc:imports('TXT_ERROR').
        hTError         = hFunc:imports('TIPO_ERROR').
            
        
        FIND proveedores        OF bt NO-LOCK NO-ERROR.
        FIND tipo_cosecha       OF bt NO-LOCK NO-ERROR. 
        FIND tipos_servicios    OF bt NO-LOCK NO-ERROR.
        FIND origenes           OF bt NO-LOCK NO-ERROR.
        FIND balanzas           OF bt NO-LOCK NO-ERROR.
    
        FIND productos_terminados WHERE
            productos_terminados.id_articulo = bt.id_materia_prima NO-LOCK NO-ERROR.
        
        IF AVAILABLE tipo_cosecha THEN
            xcorte = tipo_cosecha.id_tipo_cosecha_sap.
        ELSE
            xcorte = '00'.
        
        IF AVAILABLE tipos_servicios THEN
            xservicio = tipos_servicios.id_tipo_servicio_sap.
        ELSE
            xservicio = '00'.
        
        IF AVAILABLE productos_terminados THEN
            CASE productos_terminados.id_articulo:
                WHEN 1 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 2 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 3 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 4 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 5 THEN 
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 6 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 7 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'KG'.
                    END CASE.
            END CASE.
        ELSE
            CASE tipo_cosecha.id_tipo_cosecha.
                WHEN 4 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                WHEN 1 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'BIN'.
                OTHERWISE   ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
            END CASE.
        
        hProveedor:VALUE    = SUBSTRING(STRING(proveedores.cuit),1,10).
        hTipoCorte:VALUE    = xcorte.
        hServicio:VALUE     = xservicio.
        hArticulo:VALUE     = xarticulo.
        hFinca:VALUE        = UPPER(SUBSTRING(origenes.id_origen_sap,5,4)).
        hCentro:VALUE       = SUBSTRING(balanzas.id_balanza_sap,1,4).
    
        /*
        message     SUBSTRING(STRING(proveedores.cuit),1,10)
                    skip
                    xcorte
                    skip
                    xservicio
                    skip
                    xarticulo
                    skip
                    UPPER(SUBSTRING(origenes.id_origen_sap,5,4))
                    skip
                    SUBSTRING(balanzas.id_balanza_sap,1,4).
        */
    
        porden = "".
        IF hFunc:CALL() THEN
        DO:
            pcode   = hCode:VALUE().
            porden  = hOrden:VALUE().
            pposic  = hPosic:VALUE().
            perror  = hError:VALUE().
            ptexto  = hTexto:VALUE().
            ptipoe  = hTError:VALUE().
        END.
        ELSE
        DO:
            IF VALID-HANDLE(hFunctions) THEN hFunctions:connection:logoff().
            RETURN 'ERROR VALIDACION EN SAP '.
        END.
    

        OUTPUT TO "z:\temp\valida.txt" APPEND. 
        
            PUT UNFORMATTED 
                STRING(pbalanza) ";"
                STRING(ppesada)  ";"
                STRING(pticket)   ";"
                hProveedor:VALUE() ";"
                hTipoCorte:VALUE()
                hServicio:VALUE()   ";"
                hArticulo:VALUE() ";"
                hFinca:VALUE()    ";"
                hCentro:VALUE()    ";"
                hCode:VALUE() ";"
                hOrden:VALUE() ";"
                hPosic:VALUE() ";"
                hError:VALUE() ";"
                hTexto:VALUE() ";"
                hTError:VALUE() 
                CHR(13).
        OUTPUT CLOSE.
    END.
    IF VALID-HANDLE(hFunctions) THEN hFunctions:connection:logoff().

    RETURN.
    FINALLY:
        IF VALID-HANDLE(hFunctions) THEN hFunctions:connection:logoff().

    END FINALLY.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-balanzaTicketsToSapValidacionCMsg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE balanzaTicketsToSapValidacionCMsg Procedure 
PROCEDURE balanzaTicketsToSapValidacionCMsg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pbalanza AS INTEGER.
    DEFINE INPUT PARAMETER ppesada  AS INTEGER.
    DEFINE INPUT PARAMETER pticket  AS INTEGER.
    DEFINE OUTPUT PARAMETER pcode   AS CHARACTER FORMAT "x".
    DEFINE OUTPUT PARAMETER porden  AS CHARACTER FORMAT "x(10)".
    DEFINE OUTPUT PARAMETER pposic  AS CHARACTER FORMAT "x(10)".
    DEFINE OUTPUT PARAMETER perror  AS CHARACTER FORMAT "x(3)".
    DEFINE OUTPUT PARAMETER ptexto  AS CHARACTER FORMAT "x(100)".
    DEFINE OUTPUT PARAMETER ptipoe  AS CHARACTER FORMAT "x".
    
    
    DEFINE VARIABLE cparametros AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStatus AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    
    
    DEFINE VARIABLE hFunctions  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hFunc       AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hparametros AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hProveedor  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hTipoCorte  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hServicio   AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hArticulo   AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hFinca      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hCentro     AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hCode       AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hOrden      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hPosic      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hError      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hTexto      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hTError     AS COM-HANDLE NO-UNDO.
    
    
    DEFINE VARIABLE xcorte      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE xservicio   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE xarticulo   AS CHARACTER NO-UNDO FORMAT 'x(18)'.
    DEFINE VARIABLE xCentro     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE xunidadMedida AS CHARACTER NO-UNDO.
    

    DEFINE BUFFER BT FOR BALANZA_TICKETS.

    
    FIND bt WHERE
        bt.id_balanza  = pbalanza  AND
        bt.id_pesada   = pPesada   AND
        bt.nro_ticket  = pticket   NO-LOCK NO-ERROR.
    
    IF NOT AVAILABLE bt THEN
    do:
     message "NO HAY BALANZA_TICKETS !!" VIEW-AS ALERT-BOX.
     RETURN ERROR.
    end.
    
    hFunctions = connectToSap().
    
    IF NOT VALID-HANDLE(hFunctions) THEN DO:
        RETURN ERROR "error de conecci¢n".
    END.
    
    hFunc  = hFunctions:ADD('BAPIVALIDAINGRESOS').
    /*
    hparametros     = hFunc:exports('LPARAMETROS').
    */
    hProveedor      = hFunc:exports('I_PROVEEDOR').
    hTipoCorte      = hFunc:exports('I_TIPO_CORTE').
    hServicio       = hFunc:exports('I_TIPO_SERVICIO').
    hArticulo       = hFunc:exports('I_MATERIAL').
    hFinca          = hFunc:exports('I_FINCA').
    hCentro         = hFunc:exports('I_CENTRO_RECEPCION').
    
    hCode           = hFunc:imports('OK_CODE').
    hOrden          = hFunc:imports('ORDEN_COMPRA').
    hPosic          = hFunc:imports('POSICION').
    hError          = hFunc:imports('COD_ERROR').
    hTexto          = hFunc:imports('TXT_ERROR').
    hTError         = hFunc:imports('TIPO_ERROR').
        
    
    FIND proveedores        OF bt NO-LOCK NO-ERROR.
    FIND tipo_cosecha       OF bt NO-LOCK NO-ERROR. 
    FIND tipos_servicios    OF bt NO-LOCK NO-ERROR.
    FIND origenes           OF bt NO-LOCK NO-ERROR.
    FIND balanzas           OF bt NO-LOCK NO-ERROR.

    FIND productos_terminados WHERE
        productos_terminados.id_articulo = bt.id_materia_prima NO-LOCK NO-ERROR.
    
    IF AVAILABLE tipo_cosecha THEN
        xcorte = tipo_cosecha.id_tipo_cosecha_sap.
    ELSE
        xcorte = '00'.
    
    IF AVAILABLE tipos_servicios THEN
        xservicio = tipos_servicios.id_tipo_servicio_sap.
    ELSE
        xservicio = '00'.
    
    IF AVAILABLE productos_terminados THEN
        CASE productos_terminados.id_articulo:
            WHEN 1 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 2 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 3 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 4 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 5 THEN 
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 6 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 7 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'KG'.
                END CASE.
        END CASE.
    ELSE
        CASE tipo_cosecha.id_tipo_cosecha.
            WHEN 4 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
            WHEN 1 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'BIN'.
            OTHERWISE   ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
        END CASE.
    
    hProveedor:VALUE    = SUBSTRING(STRING(proveedores.cuit),1,10).
    hTipoCorte:VALUE    = xcorte.
    hServicio:VALUE     = xservicio.
    hArticulo:VALUE     = xarticulo.
    hFinca:VALUE        = UPPER(SUBSTRING(origenes.id_origen_sap,5,4)).
    hCentro:VALUE       = SUBSTRING(balanzas.id_balanza_sap,1,4).

    /*
    message     SUBSTRING(STRING(proveedores.cuit),1,10)
                skip
                xcorte
                skip
                xservicio
                skip
                xarticulo
                skip
                UPPER(SUBSTRING(origenes.id_origen_sap,5,4))
                skip
                SUBSTRING(balanzas.id_balanza_sap,1,4).
    */



/*    MESSAGE "Parametros bapi" SKIP
        "Proveedor: " hProveedor:VALUE() SKIP 
        "T.Corte: " hTipoCorte:VALUE() SKIP 
        "Servicio: " hServicio:VALUE() SKIP 
        "Material: " hArticulo:VALUE() SKIP 
        "Finca: " hFinca:VALUE() SKIP 
        "Centro: " hCentro:VALUE() VIEW-AS ALERT-BOX INFORMATION.
*/

    porden = "".
    IF hFunc:CALL() THEN
    DO:
        pcode   = hCode:VALUE().
        porden  = hOrden:VALUE().
        pposic  = hPosic:VALUE().
        perror  = hError:VALUE().
        ptexto  = hTexto:VALUE().
        ptipoe  = hTError:VALUE().

/*
        MESSAGE "Respuesta bapi" SKIP
            "ticket: " STRING(bt.nro_ticket) SKIP 
            "code: " pcode SKIP 
            "orden: " porden SKIP 
            "posic: " pposic SKIP 
            "error: " perror SKIP 
            "texto: " ptexto SKIP 
            "tipo: " ptipoe SKIP 
            "retorno: " RETURN-VALUE VIEW-AS ALERT-BOX INFORMATION.
*/
    END.
    ELSE
        RETURN 'ERROR VALIDACION EN SAP '.


        OUTPUT TO "z:\temp\valida.txt" APPEND. 
    
        PUT UNFORMATTED 
            STRING(pbalanza) ";"
            STRING(ppesada)  ";"
            STRING(pticket)   ";"
            hProveedor:VALUE() ";"
            hTipoCorte:VALUE()
            hServicio:VALUE()   ";"
            hArticulo:VALUE() ";"
            hFinca:VALUE()    ";"
            hCentro:VALUE()    ";"
            hCode:VALUE() ";"
            hOrden:VALUE() ";"
            hPosic:VALUE() ";"
            hError:VALUE() ";"
            hTexto:VALUE() ";"
            hTError:VALUE() 
            CHR(13).
        OUTPUT CLOSE.

    hFunctions:connection:logoff().

    
    RETURN.

    FINALLY:
        IF VALID-HANDLE(hFunctions) THEN hFunctions:connection:logoff().

    END FINALLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-balanzaTicketsToSapValidacionCMsgPrueba) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE balanzaTicketsToSapValidacionCMsgPrueba Procedure 
PROCEDURE balanzaTicketsToSapValidacionCMsgPrueba :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER pbalanza AS INTEGER.
    DEFINE INPUT PARAMETER ppesada  AS INTEGER.
    DEFINE INPUT PARAMETER pticket  AS INTEGER.
    DEFINE OUTPUT PARAMETER pcode   AS CHARACTER FORMAT "x".
    DEFINE OUTPUT PARAMETER porden  AS CHARACTER FORMAT "x(10)".
    DEFINE OUTPUT PARAMETER pposic  AS CHARACTER FORMAT "x(10)".
    DEFINE OUTPUT PARAMETER perror  AS CHARACTER FORMAT "x(3)".
    DEFINE OUTPUT PARAMETER ptexto  AS CHARACTER FORMAT "x(100)".
    DEFINE OUTPUT PARAMETER ptipoe  AS CHARACTER FORMAT "x".
    
    
    DEFINE VARIABLE cparametros AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStatus AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    
    
    DEFINE VARIABLE hFunctions  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hFunc       AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hparametros AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hProveedor  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hTipoCorte  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hServicio   AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hArticulo   AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hFinca      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hCentro     AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hCode       AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hOrden      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hPosic      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hError      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hTexto      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hTError     AS COM-HANDLE NO-UNDO.
    
    
    DEFINE VARIABLE xcorte      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE xservicio   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE xarticulo   AS CHARACTER NO-UNDO FORMAT 'x(18)'.
    DEFINE VARIABLE xCentro     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE xunidadMedida AS CHARACTER NO-UNDO.
    

    DEFINE BUFFER BT FOR BALANZA_TICKETS.

    
    FIND bt WHERE
        bt.id_balanza  = pbalanza  AND
        bt.id_pesada   = pPesada   AND
        bt.nro_ticket  = pticket   NO-LOCK NO-ERROR.
    
    IF NOT AVAILABLE bt THEN
    do:
     message "NO HAY BALANZA_TICKETS !!" VIEW-AS ALERT-BOX.
     RETURN ERROR.
    end.
    
    
    hFunctions = connectToSapCalidad().
    
    IF NOT VALID-HANDLE(hFunctions) THEN DO:
        RETURN ERROR "error de conecci¢n".
    END.
    
    hFunc  = hFunctions:ADD('BAPIVALIDAINGRESOS').
    /*
    hparametros     = hFunc:exports('LPARAMETROS').
    */
    hProveedor      = hFunc:exports('I_PROVEEDOR').
    hTipoCorte      = hFunc:exports('I_TIPO_CORTE').
    hServicio       = hFunc:exports('I_TIPO_SERVICIO').
    hArticulo       = hFunc:exports('I_MATERIAL').
    hFinca          = hFunc:exports('I_FINCA').
    hCentro         = hFunc:exports('I_CENTRO_RECEPCION').
    
    hCode           = hFunc:imports('OK_CODE').
    hOrden          = hFunc:imports('ORDEN_COMPRA').
    hPosic          = hFunc:imports('POSICION').
    hError          = hFunc:imports('COD_ERROR').
    hTexto          = hFunc:imports('TXT_ERROR').
    hTError         = hFunc:imports('TIPO_ERROR').
        
    
    FIND proveedores        OF bt NO-LOCK NO-ERROR.
    FIND tipo_cosecha       OF bt NO-LOCK NO-ERROR. 
    FIND tipos_servicios    OF bt NO-LOCK NO-ERROR.
    FIND origenes           OF bt NO-LOCK NO-ERROR.
    FIND balanzas           OF bt NO-LOCK NO-ERROR.

    FIND productos_terminados WHERE
        productos_terminados.id_articulo = bt.id_materia_prima NO-LOCK NO-ERROR.
    
    IF AVAILABLE tipo_cosecha THEN
        xcorte = tipo_cosecha.id_tipo_cosecha_sap.
    ELSE
        xcorte = '00'.
    
    IF AVAILABLE tipos_servicios THEN
        xservicio = tipos_servicios.id_tipo_servicio_sap.
    ELSE
        xservicio = '00'.
    
    IF AVAILABLE productos_terminados THEN
        CASE productos_terminados.id_articulo:
            WHEN 1 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 2 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 3 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 4 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 5 THEN 
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 6 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 7 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'KG'.
                END CASE.
        END CASE.
    ELSE
        CASE tipo_cosecha.id_tipo_cosecha.
            WHEN 4 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
            WHEN 1 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'BIN'.
            OTHERWISE   ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
        END CASE.
    
    hProveedor:VALUE    = SUBSTRING(STRING(proveedores.cuit),1,10).
    hTipoCorte:VALUE    = xcorte.
    hServicio:VALUE     = xservicio.
    hArticulo:VALUE     = xarticulo.
    hFinca:VALUE        = UPPER(SUBSTRING(origenes.id_origen_sap,5,4)).
    hCentro:VALUE       = SUBSTRING(balanzas.id_balanza_sap,1,4).

    /*
    message     SUBSTRING(STRING(proveedores.cuit),1,10)
                skip
                xcorte
                skip
                xservicio
                skip
                xarticulo
                skip
                UPPER(SUBSTRING(origenes.id_origen_sap,5,4))
                skip
                SUBSTRING(balanzas.id_balanza_sap,1,4).
    */



    MESSAGE "Parametros bapi" SKIP
        "Proveedor: " hProveedor:VALUE() SKIP 
        "T.Corte: " hTipoCorte:VALUE() SKIP 
        "Servicio: " hServicio:VALUE() SKIP 
        "Material: " hArticulo:VALUE() SKIP 
        "Finca: " hFinca:VALUE() SKIP 
        "Centro: " hCentro:VALUE() VIEW-AS ALERT-BOX INFORMATION.

    porden = "".
    IF hFunc:CALL() THEN
    DO:
        pcode   = hCode:VALUE().
        porden  = hOrden:VALUE().
        pposic  = hPosic:VALUE().
        perror  = hError:VALUE().
        ptexto  = hTexto:VALUE().
        ptipoe  = hTError:VALUE().


        MESSAGE "Respuesta bapi" SKIP
            "ticket: " STRING(bt.nro_ticket) SKIP 
            "code: " pcode SKIP 
            "orden: " porden SKIP 
            "posic: " pposic SKIP 
            "error: " perror SKIP 
            "texto: " ptexto SKIP 
            "tipo: " ptipoe SKIP 
            "retorno: " RETURN-VALUE VIEW-AS ALERT-BOX INFORMATION.

    END.
    ELSE
        RETURN 'ERROR VALIDACION EN SAP '.


        OUTPUT TO "z:\temp\valida.txt" APPEND. 
    
        PUT UNFORMATTED 
            STRING(pbalanza) ";"
            STRING(ppesada)  ";"
            STRING(pticket)   ";"
            hProveedor:VALUE() ";"
            hTipoCorte:VALUE()
            hServicio:VALUE()   ";"
            hArticulo:VALUE() ";"
            hFinca:VALUE()    ";"
            hCentro:VALUE()    ";"
            hCode:VALUE() ";"
            hOrden:VALUE() ";"
            hPosic:VALUE() ";"
            hError:VALUE() ";"
            hTexto:VALUE() ";"
            hTError:VALUE() 
            CHR(13).
        OUTPUT CLOSE.

    hFunctions:connection:logoff().

    OUTPUT CLOSE.

    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-balanzaTicketsToSapValidacionPrueba) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE balanzaTicketsToSapValidacionPrueba Procedure 
PROCEDURE balanzaTicketsToSapValidacionPrueba :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pbalanza AS INTEGER.
    DEFINE INPUT PARAMETER ppesada  AS INTEGER.
    DEFINE INPUT PARAMETER pticket  AS INTEGER.
    DEFINE OUTPUT PARAMETER pcode   AS CHARACTER FORMAT "x".
    DEFINE OUTPUT PARAMETER porden  AS CHARACTER FORMAT "x(10)".
    DEFINE OUTPUT PARAMETER pposic  AS CHARACTER FORMAT "x(10)".
    DEFINE OUTPUT PARAMETER perror  AS CHARACTER FORMAT "x(3)".
    DEFINE OUTPUT PARAMETER ptexto  AS CHARACTER FORMAT "x(100)".
    DEFINE OUTPUT PARAMETER ptipoe  AS CHARACTER FORMAT "x".
    
    
    DEFINE VARIABLE cparametros AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStatus AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    
    
    DEFINE VARIABLE hFunctions  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hFunc       AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hparametros AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hProveedor  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hTipoCorte  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hServicio   AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hArticulo   AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hFinca      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hCentro     AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hCode       AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hOrden      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hPosic      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hError      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hTexto      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hTError     AS COM-HANDLE NO-UNDO.
    
    
    DEFINE VARIABLE xcorte      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE xservicio   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE xarticulo   AS CHARACTER NO-UNDO FORMAT 'x(18)'.
    DEFINE VARIABLE xCentro     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE xunidadMedida AS CHARACTER NO-UNDO.
    

    DEFINE BUFFER BT FOR BALANZA_TICKETS.

    
    FIND bt WHERE
        bt.id_balanza  = pbalanza  AND
        bt.id_pesada   = pPesada   AND
        bt.nro_ticket  = pticket   NO-LOCK NO-ERROR.
    
    IF NOT AVAILABLE bt THEN
    do:
     message "NO HAY BALANZA_TICKETS !!" VIEW-AS ALERT-BOX.
     RETURN ERROR.
    end.
    
    
    hFunctions = connectToSapCalidad().
    
    IF NOT VALID-HANDLE(hFunctions) THEN DO:
        RETURN ERROR "error de conecci¢n".
    END.
    
    hFunc  = hFunctions:ADD('BAPIVALIDAINGRESOS').
    /*
    hparametros     = hFunc:exports('LPARAMETROS').
    */
    hProveedor      = hFunc:exports('I_PROVEEDOR').
    hTipoCorte      = hFunc:exports('I_TIPO_CORTE').
    hServicio       = hFunc:exports('I_TIPO_SERVICIO').
    hArticulo       = hFunc:exports('I_MATERIAL').
    hFinca          = hFunc:exports('I_FINCA').
    hCentro         = hFunc:exports('I_CENTRO_RECEPCION').
    
    hCode           = hFunc:imports('OK_CODE').
    hOrden          = hFunc:imports('ORDEN_COMPRA').
    hPosic          = hFunc:imports('POSICION').
    hError          = hFunc:imports('COD_ERROR').
    hTexto          = hFunc:imports('TXT_ERROR').
    hTError         = hFunc:imports('TIPO_ERROR').
        
    
    FIND proveedores        OF bt NO-LOCK NO-ERROR.
    FIND tipo_cosecha       OF bt NO-LOCK NO-ERROR. 
    FIND tipos_servicios    OF bt NO-LOCK NO-ERROR.
    FIND origenes           OF bt NO-LOCK NO-ERROR.
    FIND balanzas           OF bt NO-LOCK NO-ERROR.

    FIND productos_terminados WHERE
        productos_terminados.id_articulo = bt.id_materia_prima NO-LOCK NO-ERROR.
    
    IF AVAILABLE tipo_cosecha THEN
        xcorte = tipo_cosecha.id_tipo_cosecha_sap.
    ELSE
        xcorte = '00'.
    
    IF AVAILABLE tipos_servicios THEN
        xservicio = tipos_servicios.id_tipo_servicio_sap.
    ELSE
        xservicio = '00'.
    
    IF AVAILABLE productos_terminados THEN
        CASE productos_terminados.id_articulo:
            WHEN 1 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 2 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 3 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 4 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 5 THEN 
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 6 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 7 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'KG'.
                END CASE.
        END CASE.
    ELSE
        CASE tipo_cosecha.id_tipo_cosecha.
            WHEN 4 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
            WHEN 1 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'BIN'.
            OTHERWISE   ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
        END CASE.
    
    hProveedor:VALUE    = SUBSTRING(STRING(proveedores.cuit),1,10).
    hTipoCorte:VALUE    = xcorte.
    hServicio:VALUE     = xservicio.
    hArticulo:VALUE     = xarticulo.
    hFinca:VALUE        = UPPER(SUBSTRING(origenes.id_origen_sap,5,4)).
    hCentro:VALUE       = SUBSTRING(balanzas.id_balanza_sap,1,4).

    /*
    message     SUBSTRING(STRING(proveedores.cuit),1,10)
                skip
                xcorte
                skip
                xservicio
                skip
                xarticulo
                skip
                UPPER(SUBSTRING(origenes.id_origen_sap,5,4))
                skip
                SUBSTRING(balanzas.id_balanza_sap,1,4).
    */

    porden = "".
    IF hFunc:CALL() THEN
    DO:
        pcode   = hCode:VALUE().
        porden  = hOrden:VALUE().
        pposic  = hPosic:VALUE().
        perror  = hError:VALUE().
        ptexto  = hTexto:VALUE().
        ptipoe  = hTError:VALUE().
    END.
    ELSE
        RETURN 'ERROR VALIDACION EN SAP '.


        OUTPUT TO "z:\temp\valida.txt" APPEND. 
    
        PUT UNFORMATTED 
            STRING(pbalanza) ";"
            STRING(ppesada)  ";"
            STRING(pticket)   ";"
            hProveedor:VALUE() ";"
            hTipoCorte:VALUE()
            hServicio:VALUE()   ";"
            hArticulo:VALUE() ";"
            hFinca:VALUE()    ";"
            hCentro:VALUE()    ";"
            hCode:VALUE() ";"
            hOrden:VALUE() ";"
            hPosic:VALUE() ";"
            hError:VALUE() ";"
            hTexto:VALUE() ";"
            hTError:VALUE() 
            CHR(13).
        OUTPUT CLOSE.

    hFunctions:connection:logoff().

    OUTPUT CLOSE.

    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-balanzaTicketTablero) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE balanzaTicketTablero Procedure 
PROCEDURE balanzaTicketTablero :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pbalanza AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pPesada AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pnro AS INTEGER NO-UNDO.

DEFINE BUFFER bt FOR balanza_tickets.

FIND bt WHERE   bt.id_balanza   = pbalanza AND 
                bt.id_pesada    = ppesada AND
                bt.nro_ticket    = pnro.


CREATE balanza_tickets_tablero.
BUFFER-COPY bt TO  balanza_tickets_tablero.
balanza_tickets_tablero.modificacion = NOW.

CATCH myerror AS PROGRESS.lang.apperror:
    UNDO , RETURN ERROR myerror:getmessage(1).
END CATCH.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-balanzatickettosap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE balanzatickettosap Procedure 
PROCEDURE balanzatickettosap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pbalanza AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ppesada AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pticket AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pproceso AS CHARACTER NO-UNDO.

DEFINE VARIABLE cparametros AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.



DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hparametros AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hProceso    AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hTicket     AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE xservicio     AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcorte        AS CHARACTER NO-UNDO.
DEFINE VARIABLE xarticulo     AS CHARACTER NO-UNDO FORMAT 'x(18)'.
DEFINE VARIABLE xenvase       AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcolor        AS CHARACTER NO-UNDO.
DEFINE VARIABLE xlote         AS CHARACTER NO-UNDO.
DEFINE VARIABLE xmercado      AS CHARACTER NO-UNDO.
DEFINE VARIABLE xalmacen      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cticket       AS CHARACTER NO-UNDO.
DEFINE VARIABLE xunidadMedida AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcant         AS CHARACTER NO-UNDO.
DEFINE VARIABLE xlinea        AS CHARACTER NO-UNDO.
DEFINE VARIABLE xCentro       AS CHARACTER NO-UNDO.
DEFINE VARIABLE vlote         AS CHARACTER NO-UNDO.
DEFINE VARIABLE xestatus      AS CHARACTER NO-UNDO.

DEFINE VARIABLE hLote AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE vorigen AS CHARACTER NO-UNDO.


/*
DEFINE VARIABLE pcode   AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE VARIABLE porden  AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE pposic  AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE perror  AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE ptexto  AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE ptipoe  AS CHARACTER FORMAT "x" NO-UNDO.
*/

DEFINE VARIABLE pcode     AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE VARIABLE porden    AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE pposic    AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE perror    AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE ptexto    AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE ptipoe    AS CHARACTER FORMAT "x" NO-UNDO.

DEFINE VARIABLE iserie      AS INTEGER NO-UNDO.


DEFINE BUFFER bp FOR balanza_pesadas.


FIND bp WHERE  bp.id_balanza = pbalanza AND
                            bp.id_pesada = pPesada  NO-LOCK NO-ERROR.

IF NOT AVAILABLE bp  THEN RETURN ERROR "NO ENCUENTRA LA PESADA".

hFunctions = connectToSap().

IF VALID-HANDLE(hFunctions) THEN DO ON ERROR UNDO , RETURN ERROR:
    

    hFunc  = hFunctions:ADD('BAPI_ZTMM_MOV_FLETE_CREATE_D').
    hparametros     = hFunc:exports('LPARAMETROS').
    hproceso        = hFunc:exports('ICODIGOPROCESO').
    hTicket         = hFunc:exports('ITICKET').
    hLote           = hFunc:imports('OLOTE').
/*    
    i = 0.
*/    
    FOR EACH balanza_tickets    OF bp WHERE balanza_tickets.nro_ticket = pticket AND   balanza_tickets.cod_barra_sap = '' .
    
        cparametros = ''.
/*        
        i = i + 1.
*/
        i = pticket + 10.
        
        FIND tipos_servicios    OF balanza_tickets NO-LOCK NO-ERROR.
        FIND tipo_cosecha       OF balanza_tickets NO-LOCK NO-ERROR. 
        FIND envases_prod       OF balanza_tickets NO-LOCK NO-ERROR.
        FIND colores            OF balanza_tickets NO-LOCK NO-ERROR.
        FIND lote               OF balanza_tickets NO-LOCK NO-ERROR.
        FIND proveedores        OF balanza_tickets NO-LOCK NO-ERROR.

        IF tipo_cosecha.id_tipo_cosecha = 0 /* descarte */ OR tipo_cosecha.id_tipo_cosecha >= 4 /* procesado y otros */ THEN NEXT.
        FIND productos_terminados WHERE productos_terminados.id_articulo = balanza_tickets.id_materia_prima NO-LOCK NO-ERROR.

        FIND r_envases_prod WHERE r_envases_prod.id_envase = balanza_tickets.id_envase AND
                                  r_envases_prod.id_articulo = balanza_tickets.id_materia_prima NO-LOCK NO-ERROR.
        
        
        IF AVAILABLE tipos_servicios    THEN  xservicio = tipos_servicios.id_tipo_servicio_sap. ELSE xservicio = '00'.
        IF AVAILABLE tipo_cosecha       THEN  xcorte = tipo_cosecha.id_tipo_cosecha_sap. ELSE xcorte = '00'.
        IF AVAILABLE envases_prod       THEN  xenvase = envases_prod.id_envase_sap. ELSE ''.
        IF AVAILABLE colores            THEN  xcolor = STRING(colores.id_color_sap) . ELSE xcolor = ''.
        IF AVAILABLE lote               THEN  xlote = STRING(lote.DESCripcion). ELSE xlote = 'SL'.
        FIND origenes OF lote NO-LOCK NO-ERROR.

        /**** almacen destino y numeracion de lote ****/
        /*** envases prod unidad de medida base ***/

        IF AVAILABLE productos_terminados THEN
            CASE productos_terminados.id_articulo:
                WHEN 1 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 2 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 3 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 4 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 5 THEN 
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 6 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 7 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'KG'.
                    END CASE.
            END CASE.
        ELSE
            CASE tipo_cosecha.id_tipo_cosecha.
                WHEN 4 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                WHEN 1 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'BIN'.
                OTHERWISE   ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
            END CASE.



        IF balanza_tickets.UNION_europea THEN  xmercado = 'UE' . ELSE xmercado = 'NOUE'.

        FIND balanzas OF balanza_tickets NO-LOCK NO-ERROR.

        /*** en caso de industria determinar el almacen ***/

        cTicket = string(bp.id_pesada_sap , '999999999999' ).
        
        xcentro = SUBSTRING(balanzas.id_balanza_sap,1,4).
        IF pproceso <> 'IN' THEN xalmacen = SUBSTRING(balanzas.id_balanza_sap,5,4).
        ELSE                     xalmacen = /*SUBSTRING(balanzas.id_balanza_sap,5,4)*/ string(bp.id_pesada_ctf).
        
        /* XALMACEN = '1600'.*/

        CASE xcentro:
            WHEN 'A200' THEN
                CASE  xalmacen:
                    WHEN '2009' THEN  xlinea = 'D'.
                    WHEN '2001' THEN  xlinea = 'L'.
                    WHEN '2002' THEN  xlinea = 'S'.
                END CASE.
        END CASE.

        IF pproceso = 'IN' THEN xcant = STRING(bp.peso_entrada). 
        ELSE 
        DO:
           IF tipo_cosecha.id_tipo_cosecha = 1 THEN
              xcant = STRING(produccion.balanza_tickets.cant_env_entrada).
           ELSE
              xcant = STRING(balanza_tickets.peso_neto_ticket).
        END.
        porden = ''.
        RUN balanzaTicketsToSapValidacion 
            (INPUT balanza_tickets.id_balanza,
             INPUT balanza_tickets.id_pesada,
             INPUT balanza_tickets.nro_ticket,
             OUTPUT pcode,
             OUTPUT porden,
             OUTPUT pposic,
             OUTPUT perror,
             OUTPUT ptexto,
             OUTPUT ptipoe) NO-ERROR.
        

        IF porden <> '' THEN DO:
            ASSIGN balanza_tickets.orden_compra_sap        = porden
                   balanza_tickets.pos_orden_compra_sap    = STRING(INTEGER(pposic),"99999").
        END.

        IF balanza_tickets.orden_compra_sap = '' THEN
            xestatus    = "P".
        ELSE
            xestatus    = "O".

        /***
        MESSAGE "orden carga" bp.orden_carga_sap
                SKIP
                "tipo_cosecha" tipo_cosecha.id_tipo_cosecha
                SKIP
                "proceso" pproceso "     xcant" xcant
                VIEW-AS ALERT-BOX.
        ***/

        cparametros = bp.orden_carga_sap + '|' + STRING(i,'9999') + '|' + balanza_tickets.codigo_trazabilidad + '|' +
                      STRING(YEAR(balanza_tickets.fecha_cosecha) , '9999') + STRING(MONTH(balanza_tickets.fecha_cosecha) , '99') +
                      STRING(DAY(balanza_tickets.fecha_cosecha) , '99') + '|' + xservicio + '|' + xcorte + '|' + xarticulo + '|' + xenvase + '|' +
                      xcolor + '|' + xcant + '|' + xunidadmedida + '|' + upper(xcentro) 
                      + '|' + xalmacen + '|'+ upper(SUBSTRING(origenes.id_origen_sap,1,4)) 
                      + '|' + SUBSTRING(origenes.id_origen_sap,5,4) + '|||' + 
                      STRING(balanza_tickets.nro_remito, '9999-99999999')+ '|' + STRING(balanza_tickets.peso_neto_ticket) + '||' +
                      xlote + '|' + balanza_tickets.orden_compra_sap + '|||' + xmercado + '|' + xlinea + '|' + SUBSTRING(STRING(proveedores.cuit),1,10) +
                      '|P||' + balanza_tickets.pos_orden_compra_sap + '|' + xestatus + '|P||P|||P||||||||' + string(balanza_tickets.nro_partida).

        

        /*        
        OUTPUT TO e:\temp\ztmm_mov_flete_d.txt APPEND.
        PUT UNFORMATTED
            cparametros.
        PUT SKIP.
        OUTPUT CLOSE.
        */
        
        
/*         MESSAGE cparametros 
                skip
                "proceso" pproceso
                SKIP
                "ticket" cticket
                VIEW-AS ALERT-BOX. */


            




        hparametros:VALUE = cparametros.
        hPRoceso:VALUE = pproceso.
        hticket:VALUE = cticket.          

        IF NOT hFunc:CALL() THEN DO:
            hFunctions:connection:logoff().
            UNDO, RETURN ERROR 'ERROR DE ITEM'.
        END.

        IF  hLote:VALUE = 1 THEN DO:
            hFunctions:connection:logoff().
            UNDO , RETURN ERROR 'ERROR DE LOTE EN ITEM'.
        END.
       
        vlote = trim(hLote:value).            
        balanza_tickets.cod_barra_sap = TRIM(xarticulo) + TRIM(vlote).


        iserie = NEXT-VALUE(pesadas).

        vorigen = SOURCE-PROCEDURE:FILE-NAME.
        

        CREATE balanza_tickets2sap.
        BUFFER-COPY balanza_tickets TO balanza_tickets2sap.
        
        ASSIGN  balanza_tickets2sap.serie    = iserie
                balanza_tickets2sap.modificacion = NOW
                balanza_tickets2sap.origen = vorigen.


    END.

    hFunctions:connection:logoff().
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-borrarPalletSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE borrarPalletSap Procedure 
PROCEDURE borrarPalletSap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iSuc    AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iPallet AS INTEGER NO-UNDO.

DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE   hPallet      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hResult      AS COM-HANDLE NO-UNDO.


FIND pallets WHERE  id_suc_trabajo = iSuc AND
                    id_pallet   = ipallet NO-LOCK NO-ERROR.

IF NOT AVAILABLE pallets OR length(pallets.id_pallet_sap) <> 10 THEN RETURN ERROR.

hFunctions = connectToSap().
        
IF VALID-HANDLE(hFunctions) THEN DO:
  
  hFunc  = hFunctions:ADD('BAPI_PALLET_DELETE').
  hPallet   = hFunc:exports('IP_PALLET').
  hResult   = hFunc:imports('O_RESULT').


  hPallet:VALUE = pallets.id_pallet_sap.

  IF NOT hFunc:CALL() THEN DO:
      hFunctions:connection:logoff().
      RETURN ERROR.
  END.
  IF integer(hResult:VALUE) <> 0 THEN DO:
      hFunctions:connection:logoff().
      RETURN ERROR .
  END.
END.

FIND pallets WHERE  id_suc_trabajo = iSuc AND
                    id_pallet   = ipallet NO-ERROR.

pallets.id_pallet_sap = ''.
pallets.STATUS_sap = 0.

RELEASE pallets.
hFunctions:connection:logoff().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-cadenapallet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cadenapallet Procedure 
PROCEDURE cadenapallet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iSucTrabajo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER iPallet AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER ccabecera AS CHARACTER NO-UNDO.

  
  
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR iCodigo AS INTEGER NO-UNDO.
  DEFINE VAR cCentro AS CHARACTER NO-UNDO.





  DEFINE VARIABLE inum AS INTEGER NO-UNDO.
  DEFINE VARIABLE cestado AS CHARACTER NO-UNDO.
  DEFINE VARIABLE K AS INTEGER NO-UNDO.

  DEFINE VAR par AS INTEGER INITIAL 0.

  DEFINE VARIABLE   cFecha          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cPallet         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   xcentro         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   xalmacen        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   iEstado         AS INTEGER NO-UNDO.
  DEFINE VARIABLE   cItem           AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   ibultos         AS INTEGER NO-UNDO.
  DEFINE VARIABLE   iTraz           AS INTEGER NO-UNDO.
  DEFINE VARIABLE   iItem           AS INTEGER NO-UNDO.

  DEFINE VARIABLE   igln            AS INT64   NO-UNDO.

  DEFINE BUFFER bitems FOR items_pallets.


FIND pallets WHERE pallets.id_suc_trabajo = ISucTrabajo AND
                 pallets.id_pallet = iPallet NO-LOCK NO-ERROR.

IF AVAILABLE pallets  THEN DO ON ERROR UNDO , RETURN ERROR "Error en proceso de pallets":
  
  FIND items_pedidos_packing OF pallets NO-LOCK NO-ERROR.
  FIND pedidos_packing OF items_pedidos_packing NO-LOCK NO-ERROR.

  IF NOT AVAILABLE pedidos_packing OR NOT AVAILABLE items_pedidos_packing  THEN DO:
      UNDO , RETURN ERROR 'Error en los pedidos'.
  END.


  cPallet = string(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet , '99999999999').
  cfecha = string(YEAR(TODAY),'9999') + string(MONTH(TODAY),'99') + STRING(DAY(TODAY),'99').

  cCabecera =  cPallet + '|' + STRING(pedidos_packing.oe,'9999999999') + '|' + STRING(items_pedidos_packing.posicion_oe,'999999') + 
                '|' +  STRING(pedidos_packing.Of_sap,'999999999999') + '|' + STRING(items_pedidos_packing.material_sap,'X(18)') + '|xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|'.
 
  FIND FIRST clientes_ventas WHERE clientes_ventas.id_cliente = pedidos_packing.id_cliente NO-LOCK NO-ERROR.
 
  IF AVAILABLE clientes_ventas THEN
     IF clientes_ventas.mercado = 1 THEN cCabecera = cCabecera + 'EXTERNO|'.
     ELSE cCabecera = cCabecera + 'INTERNO|'.
     ELSE cCabecera = cCabecera + 'EXTERNO|'. /*MERCADO*/

  IF pedidos_packing.UNION_europea THEN  cCabecera = cCabecera + 'x|'. ELSE cCabecera = cCabecera + '|'.
  IF pedidos_packing.china THEN cCabecera = cCabecera + 'x|'. ELSE cCabecera = cCabecera + '|'.

  cCabecera = cCabecera  + cfecha + '|' + cfecha + '|'.

  
  FIND clientes_ventas WHERE clientes_ventas.id_cliente = pedidos_packing.id_cliente_remito NO-LOCK NO-ERROR.

  IF AVAILABLE clientes_ventas THEN cCabecera = cCabecera +  string(clientes_ventas.id_cliente_sap).
                               ELSE cCabecera = cCabecera + '9999999999'.
  cCabecera = cCabecera + '|' .

  IF AVAILABLE clientes_ventas THEN cCabecera = cCabecera + clientes_ventas.razon_social.
                               ELSE cCabecera = cCabecera + 'SIN CLIENTE'. 
  cCabecera = cCabecera +  '|'.


  FIND clientes_ventas WHERE clientes_ventas.id_cliente = pedidos_packing.id_cliente NO-LOCK NO-ERROR.
  

   
  IF AVAILABLE clientes_ventas THEN cCabecera = cCabecera + string(clientes_ventas.id_cliente_sap).
                               ELSE cCabecera = cCabecera + '9999999999'.

  cCabecera = cCabecera  + '|'.
  
  IF AVAILABLE clientes_ventas THEN cCabecera = cCabecera + clientes_ventas.razon_social.
                               ELSE cCabecera = cCabecera + 'SIN CLIENTE'.
  
  
  FIND vapores WHERE vapores.id_vapor = pedidos_packing.id_vapor NO-LOCK NO-ERROR.
  
  cCabecera = cCabecera + '|' + STRING(vapores.id_vapor_sap) + '|' + vapores.descripcion + '|'.


  FIND lugar_descarga WHERE lugar_descarga.id_lugdes = pedidos_packing.id_puerto_sal NO-LOCK NO-ERROR.

  IF AVAILABLE lugar_descarga THEN cCabecera = cCabecera + string(lugar_descarga.id_lugdes_sap).
                       ELSE cCabecera = cCabecera + '9999'.
  
  cCabecera = cCabecera +  '|' .

  IF AVAILABLE lugar_descarga THEN cCabecera = cCabecera + lugar_descarga.descripcion.
                       ELSE cCabecera = cCabecera + 'SIN PUERTO'.
  cCabecera = cCabecera +  '|'  .

  
  FIND destinos WHERE destinos.id_destino = pedidos_packing.id_destino_final NO-LOCK NO-ERROR.
  
  IF AVAILABLE destinos THEN cCabecera = cCabecera + destinos.id_destino_sap.
                        ELSE cCabecera = cCabecera + '999'. 
  cCabecera = cCabecera + '|' .

  IF AVAILABLE destinos THEN cCabecera = cCabecera + destinos.descripcion.
                        ELSE cCabecera = cCabecera + 'SIN DESTINO'.
  cCabecera = cCabecera  + '|' . 
                
  
  FIND destinos WHERE destinos.id_destino = pedidos_packing.id_puerto_ent NO-LOCK NO-ERROR.

  IF AVAILABLE destinos THEN cCabecera = cCabecera + string(destinos.id_destino_sap).
                        ELSE cCabecera = cCabecera + '9999'.

  cCabecera = cCabecera +  '|' .

  IF AVAILABLE destinos THEN cCabecera = cCabecera + destinos.descripcion.
                        ELSE cCabecera = cCabecera + 'SIN PUERTO'.
  cCabecera = cCabecera +  '|'  .

  FIND tipo_pallets OF pallets NO-LOCK NO-ERROR.
  FIND tipo_esquineros OF pallets NO-LOCK NO-ERROR.
  FIND calidades OF pallets NO-LOCK NO-ERROR.

  FIND categorias_packing OF pallets NO-LOCK NO-ERROR.
  FIND caracteristicas OF pallets NO-LOCK NO-ERROR.

  IF pedidos_packing.OF_sap = '' THEN DO:
      CASE pallets.id_suc_trabajo.
          WHEN 98   THEN    ASSIGN xcentro = 'A100' xalmacen = '1010'. /**** OJO *****/
          WHEN 97   THEN    ASSIGN xcentro = 'A200' xalmacen = '2010'.
          OTHERWISE         ASSIGN xcentro = 'A700'.
      END CASE.
  END.
  
  cCabecera = cCabecera + pallets.contramarca + '|' + string(tipo_pallets.id_tipo_pallet_sap) + '|' +
                string(tipo_esquineros.id_tipo_esquinero_sap) + '|' +  string(calidades.id_calidad_sap) + '|' +
                string(categorias_packing.id_categoria_sap)  + '|' + string(caracteristicas.id_caract_sap) + '|' + STRING(pallets.peso) + '||' +
                STRING(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet,'9999999999') + '||||' + xcentro + '|' + xalmacen + '|'.


  igln = pallets.gln.
  ccabecera = ccabecera +  STRING(igln, '9999999999999')  + "|".
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-controlPesadaEnSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE controlPesadaEnSap Procedure 
PROCEDURE controlPesadaEnSap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iBalanza AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipesada AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER cResult AS CHARACTER NO-UNDO.



DEFINE VARIABLE   hFunctions        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc             AS COM-HANDLE NO-UNDO.

DEFINE VAR hParametro               AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hReturn             AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE xlote               AS CHARACTER NO-UNDO.
DEFINE VARIABLE iResult             AS INTEGER NO-UNDO.


DEFINE VAR i AS INTEGER NO-UNDO.
DEFINE VAR mo AS INTEGER NO-UNDO.

FIND balanza_pesadas WHERE  balanza_pesadas.id_balanza = iBalanza AND 
                            balanza_pesadas.id_pesada = iPesada NO-LOCK NO-ERROR.

IF NOT AVAILABLE balanza_pesadas  THEN RETURN ERROR 'NO EXISTE LA PESADA'.


hFunctions = connectToSap().

IF VALID-HANDLE(hFunctions) THEN DO TRANSACTION ON ERROR UNDO , RETURN ERROR 'ERROR DE TRANSACCION':

    hFunc               = hFunctions:ADD('BAPI_CONTROLINGRESOFRUTA').
    hparametro          = hFunc:exports('ITICKET').
    hReturn             = hFUnc:imports('iRESULT').
    

    hParametro:VALUE = string(balanza_pesadas.id_pesada_sap,'999999999999').
    IF hFunc:CALL() THEN DO:
        iResult = integer(hReturn:VALUE).
 
        IF iResult = 999 THEN DO:
            cResult = 'ERROR GENERAL BAPI'.
            hFunctions:connection:logoff().
            RETURN ERROR 'ERROR GENERAL BAPI'.
        END.

        IF iResult <> 0  THEN DO:
                i = INTEGER(iResult / 100).
                mo = iResult MODULO 100.
                IF i = 1 THEN cResult = 'ERROR EN MOVFLETE'.
                IF MO > 0 THEN DO:
                    i = mo / 10.
                    mo =  mo MODULO 10.
                    IF i = 1 THEN cResult = cResult + ' SIN ITEMS '.
                    IF mo > 0 THEN
                        cResult = cResult + ' SIN PESADA '. 
                END.
        END.
        

        hFunctions:connection:logoff().
        RETURN.
    END.
    ELSE
    DO:
        hFunctions:connection:logoff().
        UNDO , RETURN ERROR 'ERROR DE CALL'.
    END.
END.
ELSE DO:
    RETURN ERROR 'ERROR DE COMUNICACION EN SAP'.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-controlTotalPesadaEnSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE controlTotalPesadaEnSap Procedure 
PROCEDURE controlTotalPesadaEnSap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iBalanza AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipesada AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER cResult AS CHARACTER NO-UNDO.



DEFINE VARIABLE   hFunctions        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc             AS COM-HANDLE NO-UNDO.

DEFINE VAR hParametro               AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hReturn             AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE xlote               AS CHARACTER NO-UNDO.
DEFINE VARIABLE iResult             AS INTEGER NO-UNDO.


DEFINE VAR i AS INTEGER NO-UNDO.
DEFINE VAR mo AS INTEGER NO-UNDO.


DEFINE VARIABLE pcode     AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE VARIABLE porden    AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE pposic    AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE perror    AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE ptexto    AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE ptipoe    AS CHARACTER FORMAT "x" NO-UNDO.

DEFINE VARIABLE fok       AS LOGICAL NO-UNDO.
DEFINE VARIABLE cMes      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMes1     AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKAY      AS LOGICAL NO-UNDO.
DEFINE VARIABLE cprod     AS CHARACTER NO-UNDO.
DEFINE VARIABLE FPR       AS LOGICAL NO-UNDO.

FIND balanza_pesadas WHERE  balanza_pesadas.id_balanza = iBalanza AND 
                            balanza_pesadas.id_pesada = iPesada NO-LOCK NO-ERROR.

IF NOT AVAILABLE balanza_pesadas  THEN RETURN ERROR 'NO EXISTE LA PESADA'.


hFunctions = connectToSap().

IF VALID-HANDLE(hFunctions) THEN DO TRANSACTION ON ERROR UNDO , RETURN ERROR 'ERROR DE TRANSACCION':

    hFunc               = hFunctions:ADD('BAPI_CONTROLINGRESOFRUTA').
    hparametro          = hFunc:exports('ITICKET').
    hReturn             = hFUnc:imports('iRESULT').
    

    hParametro:VALUE = string(balanza_pesadas.id_pesada_sap,'999999999999').
    IF hFunc:CALL() THEN DO:
        iResult = integer(hReturn:VALUE).
 
        IF iResult = 999 THEN DO:
            cResult = 'ERROR GENERAL BAPI'.
            hFunctions:connection:logoff().
            RETURN ERROR 'ERROR GENERAL BAPI'.
        END.

        IF iResult <> 0  THEN DO:
                i = INTEGER(iResult / 100).
                mo = iResult MODULO 100.
                IF i = 1 THEN cResult = 'ERROR EN MOVFLETE'.
                IF MO > 0 THEN DO:
                    i = mo / 10.
                    mo =  mo MODULO 10.
                    IF i = 1 THEN cResult = cResult + ' SIN ITEMS '.
                    IF mo > 0 THEN
                        cResult = cResult + ' SIN PESADA '. 
                END.
        END.
        
        FOR EACH balanza_tickets OF balanza_pesadas NO-LOCK.

            IF balanza_tickets.id_tipo_cosecha = 0 /* descarte */ OR balanza_tickets.id_tipo_cosecha >= 4 /* procesado y otros */ THEN 
            DO:
                FPR = TRUE.
                NEXT.
            END.

            /* RUN LibInterfaces.p PERSISTENT SET Hlib. */


            /******************************************/
            /******* Valida los Tickets con SAP *******/
            /******************************************/

            fok = TRUE.
            FPR = FALSE.

                porden = ''.
                RUN balanzaTicketsToSapValidacionCMsg 
                    (INPUT balanza_tickets.id_balanza,
                     INPUT balanza_tickets.id_pesada,
                     INPUT balanza_tickets.nro_ticket,
                     OUTPUT pcode,
                     OUTPUT porden,
                     OUTPUT pposic,
                     OUTPUT perror,
                     OUTPUT ptexto,                                                                           
                     OUTPUT ptipoe) NO-ERROR.                                                                 
                                                                                                              
                                                                                                              


                IF ERROR-STATUS:ERROR THEN
                DO:
                    MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
                    fok = FALSE.
                END.

        END.

        hFunctions:connection:logoff().
        RETURN.
    END.
    ELSE
    DO:
        hFunctions:connection:logoff().
        UNDO , RETURN ERROR 'ERROR DE CALL'.
    END.
END.
ELSE DO:
    RETURN ERROR 'ERROR DE COMUNICACION EN SAP'.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-devuelvemateriaprima) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelvemateriaprima Procedure 
PROCEDURE devuelvemateriaprima :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER cmateriaprima AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER iarticulo AS INTEGER NO-UNDO.

CASE cmateriaprima:
    WHEN 'LIMON001' THEN iarticulo = 1.
    WHEN 'POMELO001' THEN iarticulo = 2.
    WHEN 'NARANJA001' THEN iarticulo = 2.
    WHEN 'MANDARINA001' THEN iarticulo = 4.
    WHEN 'PALTA001' THEN iarticulo = 5.
    WHEN 'TANGELO001' THEN iarticulo = 6.
    WHEN 'KUNKUA001' THEN iarticulo = 7.
    OTHERWISE iarticulo = 0.
END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-devuelvev1v2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelvev1v2 Procedure 
PROCEDURE devuelvev1v2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER vsucursal AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER v1 AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER v2 AS INTEGER NO-UNDO.

IF vsucursal = 98 THEN  ASSIGN v1 = 110
                               v2 = 97.
                  ELSE  ASSIGN v1 = 130
                               v2 = 98. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-devuelvevaloresmovflete) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelvevaloresmovflete Procedure 
PROCEDURE devuelvevaloresmovflete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hTableMovFleted AS com-handle NO-UNDO.
DEFINE INPUT PARAMETER i AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER vsucursal AS INTEGER NO-UNDO.

DEFINE OUTPUT PARAMETER cmateriaprima AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER vcantenvasesentrada AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER vpesoenvasesentrada AS DECIMAL NO-UNDO DECIMALS 3.
DEFINE OUTPUT PARAMETER ctipocosecha        AS  CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ctiposervicio       AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER cenvase             AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER corigensap          AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ccodigotrazabilidad AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER vcolor              AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER cmercado            AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER vpesonetoticket     AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER vunioneuropea       AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER vchina              AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER vfinca              AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER vsucursaletiqueta   AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER vsucursalpacking    AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER vproveedor          AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER vproveedororigen    AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER vorigen             AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER vorigenorigen       AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER vtipocosecha        AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER vcalidadbalanza     AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER vtiposervicio       AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER venvase             AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER vlotesenasa         AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER vlote               AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER vfincasenasa        AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER vcertunion          AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER vcertchina          AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER vcertificado        AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER v1                  AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER v2                  AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER iarticulo           AS INTEGER NO-UNDO.

    cmateriaprima       = hTableMovFleteD:VALUE(i,8).   
    vcantenvasesentrada = INTEGER(hTableMovFleteD:VALUE(i,11)).
    vpesoenvasesentrada = vcantenvasesentrada * r_envases_prod.Kilos .
    ctipocosecha        = htableMovfleteD:VALUE(i,7).
    ctiposervicio       = hTableMovFleteD:VALUE(i,6).
    cenvase             = htableMovFleteD:VALUE(i,9).
    corigensap          = STRING(htableMovFleteD:VALUE(i,15),"x(4)") + STRING(htableMovFleteD:VALUE(i,16),"9999").
    ccodigoTrazabilidad = hTableMovFleted:VALUE(i,3).
    vcolor              = hTableMovFleted:VALUE(i,10).
    cmercado            = hTableMovFleted:VALUE(i,26).
    vpesonetoticket     = hTableMovFleted:VALUE(i,20).
    


    FIND FIRST      tipo_cosecha WHERE id_tipo_cosecha_sap      =  ctipocosecha NO-LOCK NO-ERROR.
    IF AVAILABLE    tipo_cosecha THEN ASSIGN vtipocosecha       = tipo_cosecha.id_tipo_cosecha
                                             vcalidadbalanza    = tipo_cosecha.id_calidad_balanza. ELSE ASSIGN vtipocosecha = 0 vcalidadbalanza = 0.
                                       
    FIND FIRST      tipos_servicios WHERE id_tipo_servicio_sap =  ctiposervicio NO-LOCK NO-ERROR.
    IF AVAILABLE    tipos_servicios THEN  vtiposervicio = tipos_servicios.id_tipo_servicio. ELSE vtiposervicio = 0.
    
    FIND FIRST      envases_prod WHERE id_envase_sap =  cenvase NO-LOCK NO-ERROR.
    IF AVAILABLE    envases_prod THEN venvase = envases_prod.id_envase . ELSE venvase = 0.

    FIND FIRST      colores  WHERE colores.id_color_sap = vcolor NO-LOCK NO-ERROR.
    IF AVAILABLE    colores  THEN vcolor = colores.id_color . ELSE vcolor = 0.

    FIND FIRST      lote WHERE lote.codigo_trazabilidad = ccodigotrazabilidad NO-LOCK NO-ERROR.
    IF AVAILABLE    lote THEN
        ASSIGN vlotesenasa = lote.id_lote_senasa
               vlote = lote.descripcion.
    ELSE
        RETURN ERROR 'Error de Trazabilidad de Lote'.

    FIND FIRST      origenes    OF lote NO-LOCK NO-ERROR.       
    IF AVAILABLE    origenes  THEN ASSIGN vfincasenasa = origenes.id_finca_senasa . ELSE vfincasenasa = 0.
    FIND FIRST      proveedores OF origenes  NO-LOCK NO-ERROR.
    IF AVAILABLE    proveedores THEN vproveedor = proveedores.id_proveedor. ELSE vproveedor = 0.

    CASE cmercado:
        WHEN 'UE' THEN
        DO:
            vunioneuropea = TRUE.
            vchina        = FALSE.
        END.
        WHEN 'CHINA' THEN
        DO:
            vunioneuropea = FALSE.
            vchina = TRUE.
        END.
        OTHERWISE 
        DO:
            vunioneuropea = FALSE.
            vchina        = FALSE.
        END.
    END CASE.
    IF hTableMovfleteD:VALUE(1,15) = 'A300' THEN
        vfinca = TRUE.
    ELSE
        vfinca = FALSE.
    if vfinca then do:

        IF vsucursal = 98 THEN
           vsucursaletiqueta = 101.
        ELSE
           vsucursaletiqueta = 111.

        ASSIGN vsucursalpacking = 0
               vproveedororigen = vproveedor
               vorigenorigen    = vorigen.
    end.

    if NOT vfinca OR vtipocosecha = 4 then do:

        IF vsucursal = 98 THEN
            vsucursaletiqueta = 110.
        ELSE
            vsucursaletiqueta = 130.

        assign
            vsucursalpacking = vsucursal
            vproveedororigen = 1
            vorigenorigen    = (IF vsucursal = 98 THEN 97 ELSE 98).
    end.
    
    vcertunion  = lote.certificado.
    vcertchina  = lote.cert_china.

    IF cmercado = 'UE' THEN
    DO:
          IF vcertunion = "" THEN UNDO , RETURN "El lote no posee certificado UE".
          vchina = NO.
          vunioneuropea = TRUE.
          vcertificado = vcertunion.
    END.

    IF cmercado  = 'CHINA' THEN
    DO:
          IF vcertchina = "" THEN UNDO , RETURN "El lote no posee certificado CH".
          vunioneuropea = NO.
          vchina = TRUE.
          vcertificado = vcertchina. 
    END.
    IF vsucursal = 98 THEN  ASSIGN v1 = 110
                                   v2 = 97.
                      ELSE  ASSIGN v1 = 130
                                   v2 = 98. 
  CASE cmateriaprima:
      WHEN 'LIMON001' THEN iarticulo = 1.
      WHEN 'POMELO001' THEN iarticulo = 2.
      WHEN 'NARANJA001' THEN iarticulo = 2.
      WHEN 'MANDARINA001' THEN iarticulo = 4.
      WHEN 'PALTA001' THEN iarticulo = 5.
      WHEN 'TANGELO001' THEN iarticulo = 6.
      WHEN 'KUNKUA001' THEN iarticulo = 7.
      OTHERWISE iarticulo = 0.
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCaracteristicasLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCaracteristicasLote Procedure 
PROCEDURE getCaracteristicasLote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER  hFunc1              AS COM-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER  pclote              AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER  pclocacion          AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER  pcmateriaprima      AS CHARACTER NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR      ttclasificacion .

DEFINE VAR k                    AS INTEGER NO-UNDO.
DEFINE VAR hMaterial            AS COM-HANDLE NO-UNDO.
DEFINE VAR hLote                AS COM-HANDLE NO-UNDO.
DEFINE VAR hLocacion            AS COM-HANDLE NO-UNDO.
DEFINE VAR hRegistroLote        AS COM-HANDLE NO-UNDO.
DEFINE VAR hTableClasificacion  AS COM-HANDLE NO-UNDO.

hmaterial                 = hFunc1:exports('CMATERIAL').
hlote                     = hFunc1:exports('CLOTE').
hlocacion                 = hFunc1:exports('CLOCACION').
hRegistroLote             = hFunc1:imports('TLOTE').
hTableClasificacion       = hFunc1:tables('TCLASIFICACION').

hmaterial:value           =  pcmateriaprima.
hLote:VALUE               =  pclote.
hLocacion:VALUE           =  pclocacion.
k = 0.
IF hfunc1:CALL() THEN
DO:
      k + 1.
      CREATE    ttclasificacion.
      ASSIGN    ttclasificacion.lote            = hLote:VALUE
                ttclasificacion.caracteristica  = hTableClasificacion:VALUE (k, 1)
                ttclasificacion.valor           = hTableClasificacion:VALUE (k , 2 ).
END.
ELSE UNDO , RETURN ERROR 'Error en Bapi de Caracteristicas de lote'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ingresFrutaToSapPrueba) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ingresFrutaToSapPrueba Procedure 
PROCEDURE ingresFrutaToSapPrueba :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
  Purpose: La Tabla balanza_pesada, tiene un campo id_entrega_sap, el cual si 
           contiene datos, la interface £nicamente va a crear un registro en SAP
           sobre ztmm_mov_flete llenando el campo correspondiente de la Entrega SAP     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pbalanza AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pPesada AS INTEGER NO-UNDO.


DEFINE VARIABLE cparametros     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStatus         AS CHARACTER NO-UNDO.
DEFINE VARIABLE hFunctions      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hFunc           AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hparametros     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hOCarga         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hPesada         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hLBalanza       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE i               AS INTEGER NO-UNDO.
DEFINE VARIABLE xservicio       AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcorte          AS CHARACTER NO-UNDO.
DEFINE VARIABLE xarticulo       AS CHARACTER NO-UNDO.
DEFINE VARIABLE xenvase         AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcolor          AS CHARACTER NO-UNDO.
DEFINE VARIABLE xlote           AS CHARACTER NO-UNDO.
DEFINE VARIABLE xmercado        AS CHARACTER NO-UNDO.
DEFINE VARIABLE xtipotransporte AS CHARACTER NO-UNDO.
DEFINE VARIABLE xproceso        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cant            AS DECIMAL NO-UNDO.
DEFINE VARIABLE xcant           AS CHARACTER NO-UNDO.
DEFINE VARIABLE xentrega_sap    AS CHARACTER NO-UNDO.

DEFINE VARIABLE VORIGEN         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cmes            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cmes1           AS CHARACTER NO-UNDO.
DEFINE VARIABLE okay            AS LOGICAL NO-UNDO.

/* OUTPUT STREAM a-chk TO e:\temp\ztmm_mov_flete.txt APPEND.*/

    FIND balanza_pesadas WHERE  balanza_pesadas.id_balanza = pbalanza AND
                                balanza_pesadas.id_pesada  = pPesada  NO-ERROR NO-WAIT.

    FIND FIRST balanza_tickets OF balanza_pesadas WHERE
        balanza_tickets.id_tipo_cosecha > 0 AND balanza_tickets.id_tipo_cosecha < 4 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE balanza_tickets THEN RETURN 'FRUTA PROCESADA O P/INDUSTRIA'.

    /*** Cuando viene con PESO_DESCARTE que es fruta para la Industria,
     no debe entrar en la Interface.
     SE QUITO ESTA CONDICIàN PARA LIQUIDAR TAMBIEN LOS VIAJES  
    IF AVAILABLE balanza_pesadas AND
                 balanza_pesadas.peso_descarte = 0 THEN
                 ****/
    IF balanza_pesadas.orden_carga_sap <> '' OR trim(balanza_pesadas.orden_carga_sap) = '1'  THEN UNDO  , RETURN ERROR 'NO CORRESPONDE TIENE ORDEN DE CARGA'.
    IF balanza_pesadas.id_pesada_sap <> '' OR 
       trim(balanza_pesadas.id_pesada_sap) = '1'  THEN UNDO , RETURN ERROR 'NO CORRESPONDE TIENE ORDEN DE CARGA'.

    DO ON ERROR UNDO , RETURN ERROR 'ERROR':

        IF pbalanza <> 2 AND pbalanza <> 4 THEN UNDO, RETURN ERROR 'BALANZA EQUIVOCADA'.
        
        FIND FIRST balanza_tickets OF balanza_pesadas NO-ERROR.
        IF NOT AVAILABLE balanza_tickets THEN UNDO , RETURN ERROR 'PESADA SIN ITEMS'.
        
        hFunctions = connectToSapCalidad().
        
        IF NOT VALID-HANDLE(hFunctions) THEN RETURN ERROR  "error de conecci¢n".
        
        IF VALID-HANDLE(hFunctions) THEN DO:

            hFunc  = hFunctions:ADD('BAPI_ZTMM_MOV_FLETE_CREATE').

            hparametros     = hFunc:exports('LPARAMETROS').
            hOCarga         = hFunc:imports('OCARGA').
            hPesada         = hFunc:imports('OTICKET').
            hLBalanza       = hFunc:exports('LBALANZA').

            /***** indicador acoplado  codigo de balanza **/ 
            /***** numeracion de ticket impreso: numero de pesada ***/
            IF balanza_pesadas.id_pesada_sap = ? OR balanza_pesadas.id_pesada_sap = '' THEN
                balanza_pesadas.id_pesada_sap = ''.



            ASSIGN xentrega_sap = IF balanza_pesadas.tara = 0 THEN ''
                                  ELSE TRIM(STRING(balanza_pesadas.tara)).
            
            cparametros = string(balanza_pesadas.id_pesada_sap , '999999999999') + '|||'.

            FIND transportes_proveedor WHERE transportes_proveedor.id_transporte =  balanza_pesadas.id_transporte NO-LOCK NO-ERROR.
            FIND proveedores OF transportes_proveedor NO-LOCK NO-ERROR.
            IF AVAILABLE proveedores  THEN
                cparametros = cparametros + substring(proveedores.cuit,1,10) + '|'.
            ELSE
                cparametros = cparametros + '0000000000|'.

            IF AVAILABLE transportes_proveedor THEN  cparametros = cparametros + STRING(transportes_proveedor.id_transporte). /* se reeemplazo id_transporte_sap que es lo mismo*/
                                               ELSE  cparametros = cparametros + '9999' .

            FIND tipo_transporte OF transportes_proveedor NO-LOCK NO-ERROR.

            IF AVAILABLE tipo_transporte  THEN xtipotransporte = tipo_transporte.id_tipo_transporte_sap.  ELSE xtipotransporte = ''.

            FIND balanzas OF balanza_pesadas NO-LOCK NO-ERROR. 

            cparametros = cparametros + '|0|CHOFER7890123456789012345||||00000000||||0||||0|0||'. 

            cparametros =   cparametros + string(year(produccion.balanza_pesadas.fecha_entrada),'9999') + 
                            STRING(MONTH(produccion.balanza_pesadas.fecha_entrada),'99') + STRING(DAY(produccion.balanza_pesadas.fecha_entrada),'99') + '|' +
                            REPLACE(balanza_pesadas.hora_entrada,':','') + '||||' + xtipotransporte + '||||||||||||||||' + 
                            trim(replace(substring(balanzas.id_balanza_sap,9,4),'0','')) + '||||'.

            IF AVAILABLE transportes_proveedor THEN cparametros = cparametros  + transportes_proveedor.patente + '|' + transportes_proveedor.patente_acop.
                                               ELSE cparametros = cparametros  + 'SP999|SP999'.

            CASE balanzas.id_sucursal:
                WHEN 97  THEN ASSIGN    xproceso = 'PF' 
                                        hLBalanza:VALUE = 'X'.
                WHEN 98  THEN ASSIGN    xproceso = 'PL' 
                                        hLBalanza:VALUE = 'X'.
                WHEN 99  THEN ASSIGN    xproceso = 'PU' 
                                        hLBalanza:VALUE = 'X'.
                WHEN 95  THEN xproceso = 'IN'.
                WHEN 96  THEN xproceso = 'IN'.
            END CASE.


            FIND FIRST balanza_tickets OF balanza_pesadas NO-LOCK.
            FIND envases_prod OF balanza_ticket NO-LOCK.

            
            FOR EACH balanza_tickets OF balanza_pesada NO-LOCK.
                cant = cant + produccion.balanza_tickets.cant_env_entrada.
            END.
            
            IF AVAILABLE envases_prod AND xproceso = 'IN' THEN 
                    ASSIGN  xenvase = envases_prod.id_envase_sap
                            xcant = STRING(cant).
            cant = 0.
            
            cparametros = cparametros + '|000000000000000||||' + xenvase + '|'  + xproceso + '|FI||' + xcant + '|0000000000|||||||||' .

            IF AVAILABLE proveedores THEN
                cparametros = cparametros + SUBSTRING(string(cuit),1,10) + '|||||' + string(produccion.balanza_pesadas.id_pesada_sap,'999999999999')  + '|||||'
                              + xentrega_sap.
            ELSE
                cparametros = cparametros + '0000000000|||||' + string(produccion.balanza_pesadas.id_pesada_sap,'999999999999')  + '|||||' 
                              + xentrega_sap.

            cparametros = cparametros + '|' + string(balanza_pesada.peso_entrada) + '|' + string(balanza_pesada.peso_salida) + '|' +
                     string(year(produccion.balanza_pesadas.fecha_salida),'9999') + 
                     STRING(MONTH(produccion.balanza_pesadas.fecha_salida),'99') + STRING(DAY(produccion.balanza_pesadas.fecha_salida),'99') + '|' +
                     /*** REPLACE(balanza_pesadas.hora_salida,':','') + '|' + string(balanza_pesada.peso_descarte) + '|' +  SE SACO EL PESO DESCARTE ***/
                     REPLACE(balanza_pesadas.hora_salida,':','') + '|' + '' + '|' +
                     STRING(cant) + '|' + string(balanza_pesada.peso_neto).

            OUTPUT TO VALUE("z:\temp\cparametros" + STRING(balanza_pesadas.id_pesada) + ".txt").
                EXPORT CPARAMETROS.
            OUTPUT CLOSE.

            
            hParametros:VALUE = cparametros.
            /* Si es un remito con Entrega SAP, no generamos la tabla
             SAP ztmm_mov_flete_d */
            IF hFunc:CALL() THEN  DO ON ERROR UNDO , RETURN ERROR 'ERROR GENERAL BALANZA PESADAS':
                balanza_pesadas.orden_carga_sap = hOCarga:VALUE. 
                balanza_pesadas.id_pesada_sap   = STRING(hPesada:VALUE,'999999999999').

                vorigen = SOURCE-PROCEDURE:FILE-NAME.

                CREATE balanza_pesadas2sap.
                BUFFER-COPY balanza_pesadas TO balanza_pesadas2sap.

                ASSIGN  balanza_pesadas2sap.serie    = NEXT-VALUE(pesadas)
                        balanza_pesadas2sap.modificacion = NOW
                        balanza_pesadas2sap.origen = vorigen.
            
           /*-----------------------------------------------------------------
            SE HABIA HABLADO, QUE CUANDO UN REMITO ES REALIZADO POR SAP, EL OPERADOR
            DEBE REGISTRAR EL NRO.ORDEN ENTREGA (CAMPO TARA).  DE ESTA FORMA LA INTERFACE
            DEBE LLENAR UNICAMENTE LA CABECERA (zmm_mov_flete) SIN PASAR LAS POSICIONES
            O ITEMS (zmm_mov_flete_d)
            -----------------------------------------------------------------*/
            
            IF xentrega_sap = '' THEN /***** no me acuerdo porque asi que lo saco *****/
            DO:
                IF trim(balanza_pesadas.orden_carga_sap) <> '1' THEN
                    RUN balanzaTicketsTosap ( balanza_pesadas.id_balanza , balanza_pesadas.id_pesada , xproceso , balanza_pesadas2sap.serie ) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN 
                DO:                        
                    hFunc  = hFunctions:ADD('BAPI_ZTMM_MOV_FLETE_delete').
                    hOCarga         = hFunc:exports('I_OCARGA').
                    hPesada         = hFunc:exports('I_TICKET').
                    hOcarga:VALUE = balanza_pesadas.orden_carga_sap.
                    hPesada:VALUE = balanza_pesadas.id_pesada_sap.
                    IF hFunc:CALL() THEN 
                        ASSIGN balanza_pesadas.orden_carga_sap = '1'.
                               balanza_pesadas.id_pesada_sap   = '1'.

                   CMES = 
                  '____________________________________________________________________________________________________________________________' + '~n' +
                  'Hora : ' + STRING(NOW, '99/99/99 hh:mm:ss')  + '~n' +
                  'El ticket de Balanza : ' + string( balanza_pesadas.id_balanza ) + '~n' +
                  '             Pesada  : ' + STRING( balanza_pesadas.id_pesada  ) + '~n' +
                  'no ingreso a SAP , con error en el procesamiento de item en ingreso de fruta ' + RETURN-VALUE + '~n' + 
                  '____________________________________________________________________________________________________________________________'.                   .


                   RUN ffw\procs\smtpmail58.p ("www.softsargentina.com" , 
                                "pdigonzelli@gmail.com,gprochaz@sa-sanmiguel.com,pedrom@sa-sanmiguel.com,rtralice@sa-sanmiguel.com,cValdes@sa-sanmiguel.com,gabriel@sa-sanmiguel.com" ,
                                "ingresofruta@softsargentina.com" ,
                                "",
                                "" ,
                                "" ,
                                "Ingreso de fruta con error" ,
                                Cmes ,
                                "",
                                "text" ,
                                 1,
                                 YES,
                                 "base64" ,
                                 "ingresofruta@softsargentina.com" ,
                                 "ingresofruta2011",
                                OUTPUT okay ,
                                OUTPUT cmes1) NO-ERROR.
                    UNDO, RETURN ERROR 'ERROR  BALANZATICKETS TO SAP' . 
                END.
                /***********************************
                CMES = 
               '____________________________________________________________________________________________________________________________' + '~n' +
               'Hora : ' + STRING(NOW, '99/99/99 hh:mm:ss')  + '~n' +
               'El ticket de Balanza : ' + string( balanza_pesadas.id_balanza ) + '~n' +
               '             Pesada  : ' + STRING( balanza_pesadas.id_pesada  ) + '~n' +
               'ingreso correctamente a SAP '  + '~n' +
               ' Orden de Carga SAP  : ' + STRING( balanza_pesadas.orden_carga_sap) + '~n' +
               '         Pesada SAP  : ' + STRING( balanza_pesadas.id_pesada_sap) + '~n' +
               '____________________________________________________________________________________________________________________________'.                   .
        

                RUN ffw\procs\smtpmail58.p ("www.softsargentina.com" , 
                             "pdigonzelli@gmail.com,gprochaz@sa-sanmiguel.com,pedrom@sa-sanmiguel.com,rtralice@sa-sanmiguel.com,vtabernero@sa-sanmiguel.com,gabriel@sa-sanmiguel.com" ,
                             "ingresofruta@softsargentina.com" ,
                             "",
                             "" ,
                             "" ,
                             "Ingreso de Fruta OK" ,
                             Cmes ,
                             "",
                             "text" ,
                              1,
                              YES,
                              "base64" ,
                              "ingresofruta@softsargentina.com" ,
                              "ingresofruta2011",
                             OUTPUT okay ,
                             OUTPUT cmes1) NO-ERROR.
                **********************************/
            END.

            END. 
            ELSE 
            DO:
                CMES = 
               '____________________________________________________________________________________________________________________________' + '~n' + 
               'Hora : ' + STRING(NOW, '99/99/99 hh:mm:ss')  + '~n' +
               'El ticket de Balanza : ' + string( balanza_pesadas.id_balanza ) + '~n' +
               '             Pesada  : ' + STRING( balanza_pesadas.id_pesada  ) + '~n' +
               'dio un error de procesamiento : '  + 'error de x_entrega_sap en ingreso de fruta' +  '~n' +
               '____________________________________________________________________________________________________________________________'.                   .


                RUN ffw\procs\smtpmail58.p ("www.softsargentina.com" , 
                             "pdigonzelli@gmail.com,gprochaz@sa-sanmiguel.com,pedrom@sa-sanmiguel.com,rtralice@sa-sanmiguel.com,cvaldes@sa-sanmiguel.com,gabriel@sa-sanmiguel.com" ,
                             "ingresofruta@softsargentina.com" ,
                             "",
                             "" ,
                             "" ,
                             "Ingreso de fruta con error" ,
                             Cmes ,
                             "",
                             "text" ,
                              1,
                              YES,
                              "base64" ,
                              "ingresofruta@softsargentina.com" ,
                              "ingresofruta2011",
                             OUTPUT okay ,
                             OUTPUT cmes1) NO-ERROR.
                UNDO , RETURN ERROR 'ERROR BALANZAPESADA|TOSAP'.
            END.
        END. 
        ELSE
        DO:
            CMES = 
           '____________________________________________________________________________________________________________________________' + '~n' +
           'Hora : ' + STRING(NOW, '99/99/99 hh:mm:ss')  + '~n' +
           'El ticket de Balanza : ' + string( balanza_pesadas.id_balanza ) + '~n' +
           '             Pesada  : ' + STRING( balanza_pesadas.id_pesada  ) + '~n' +
           'dio un error de procesamiento : '  + 'error de coneccion a SAP en ingreso de fruta' +  '~n' +
           '____________________________________________________________________________________________________________________________'.                   .


            RUN ffw\procs\smtpmail58.p ("www.softsargentina.com" , 
                         "pdigonzelli@gmail.com,gprochaz@sa-sanmiguel.com,pedrom@sa-sanmiguel.com,rtralice@sa-sanmiguel.com,cvaldes@sa-sanmiguel.com,gabriel@sa-sanmiguel.com" ,
                         "ingresofruta@softsargentina.com" ,
                         "",
                         "" ,
                         "" ,
                         "Ingreso de fruta con error" ,
                         Cmes ,
                         "",
                         "text" ,
                          1,
                          YES,
                          "base64" ,
                          "ingresofruta@softsargentina.com" ,
                          "ingresofruta2011",
                         OUTPUT okay ,
                         OUTPUT cmes1) NO-ERROR.
            UNDO , RETURN ERROR 'ERROR CONECCION SAP'.
        END.
    END.

    hFunctions:connection:logoff().
    RETURN balanza_pesadas.orden_carga_sap.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ingresoFrutaDesdeFecha) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ingresoFrutaDesdeFecha Procedure 
PROCEDURE ingresoFrutaDesdeFecha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pFecha AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER i AS INTEGER NO-UNDO.

DEFINE BUFFER b FOR balanza_pesadas.

FOR EACH b WHERE  b.id_balanza = 2 OR id_balanza = 4 AND 
                  b.fecha_entrada >= pFecha  AND
                  ( id_pesada_sap = '' AND orden_carga_sap = '' ) NO-LOCK.

    RUN ingresoFrutaToSap( INPUT b.id_balanza , INPUT b.id_pesada) NO-ERROR.
    IF ERROR-STATUS:ERROR  THEN NEXT.
    IF RETURN-VALUE <> '' OR RETURN-VALUE <> ''  THEN
        i = i + 1.
    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-IngresoFrutaToSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IngresoFrutaToSap Procedure 
PROCEDURE IngresoFrutaToSap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pbalanza AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pPesada AS INTEGER NO-UNDO.


DEFINE VARIABLE cparametros     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStatus         AS CHARACTER NO-UNDO.
DEFINE VARIABLE hFunctions      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hFunc           AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hparametros     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hOCarga         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hPesada         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hLBalanza       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE i               AS INTEGER NO-UNDO.
DEFINE VARIABLE xservicio       AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcorte          AS CHARACTER NO-UNDO.
DEFINE VARIABLE xarticulo       AS CHARACTER NO-UNDO.
DEFINE VARIABLE xenvase         AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcolor          AS CHARACTER NO-UNDO.
DEFINE VARIABLE xlote           AS CHARACTER NO-UNDO.
DEFINE VARIABLE xmercado        AS CHARACTER NO-UNDO.
DEFINE VARIABLE xtipotransporte AS CHARACTER NO-UNDO.
DEFINE VARIABLE xproceso        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cant            AS DECIMAL NO-UNDO.
DEFINE VARIABLE xcant           AS CHARACTER NO-UNDO.
DEFINE VARIABLE xentrega_sap    AS CHARACTER NO-UNDO.

DEFINE VARIABLE VORIGEN         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cmes            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cmes1           AS CHARACTER NO-UNDO.
DEFINE VARIABLE okay            AS LOGICAL NO-UNDO.
DEFINE VARIABLE hTabla          AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE fcall           AS LOGICAL NO-UNDO.

/* OUTPUT STREAM a-chk TO e:\temp\ztmm_mov_flete.txt APPEND.*/

    FIND balanza_pesadas WHERE  balanza_pesadas.id_balanza = pbalanza AND
                                balanza_pesadas.id_pesada  = pPesada  NO-ERROR NO-WAIT.
    ASSIGN xentrega_sap = IF balanza_pesadas.tara = 0 THEN '' ELSE TRIM(STRING(balanza_pesadas.tara,'9999999999')).

    IF xentrega_sap = '' THEN
    DO:
        FIND FIRST balanza_tickets OF balanza_pesadas WHERE
            balanza_tickets.id_tipo_cosecha > 0 AND balanza_tickets.id_tipo_cosecha < 4 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE balanza_tickets THEN RETURN 'FRUTA PROCESADA O P/INDUSTRIA'.
    END.

    /*** Cuando viene con PESO_DESCARTE que es fruta para la Industria,
     no debe entrar en la Interface.
     SE QUITO ESTA CONDICIàN PARA LIQUIDAR TAMBIEN LOS VIAJES  
    IF AVAILABLE balanza_pesadas AND
                 balanza_pesadas.peso_descarte = 0 THEN
                 ****/
    IF balanza_pesadas.orden_carga_sap <> '' OR trim(balanza_pesadas.orden_carga_sap) = '1'  THEN UNDO  , RETURN ERROR 'NO CORRESPONDE TIENE ORDEN DE CARGA'.
    IF balanza_pesadas.id_pesada_sap <> '' OR 
       trim(balanza_pesadas.id_pesada_sap) = '1'  THEN UNDO , RETURN ERROR 'NO CORRESPONDE TIENE ORDEN DE CARGA'.

    DO ON ERROR UNDO , RETURN ERROR 'ERROR':

        IF pbalanza <> 2 AND pbalanza <> 4 THEN UNDO, RETURN ERROR 'BALANZA EQUIVOCADA'.
        
        IF xentrega_sap = ''  THEN
        DO:
            FIND FIRST balanza_tickets OF balanza_pesadas NO-ERROR.
            IF NOT AVAILABLE balanza_tickets THEN UNDO , RETURN ERROR 'PESADA SIN ITEMS'.
        END.
        
        hFunctions = connectToSap().
        
        IF NOT VALID-HANDLE(hFunctions) THEN RETURN ERROR  "error de conecci¢n".
        
        IF VALID-HANDLE(hFunctions) THEN DO:

            hFunc  = hFunctions:ADD('BAPI_ZTMM_MOV_FLETE_CREATE').

            hparametros     = hFunc:exports('LPARAMETROS').
            hOCarga         = hFunc:imports('OCARGA').
            hPesada         = hFunc:imports('OTICKET').
            hLBalanza       = hFunc:exports('LBALANZA').
            hTabla          = hFunc:tables('PT_LOTES'). 
/**************************************************************
AGREGAR POR AQUI EL PARAMETRO TABLES
AGREGUE EN ESTA BAPI UN PARAMETRO TIPO TABLA QUE DEVUELVE UNA TABLITA
CON LOS LOTES QUE SE DAN DE ALTA CON LA RECEPCION DEL REMITO
EL PARAMETRO TABLA SE LLAMA PT_LOTES
*****************************************************************/

            /***** indicador acoplado  codigo de balanza **/ 
            /***** numeracion de ticket impreso: numero de pesada ***/
            IF balanza_pesadas.id_pesada_sap = ? OR balanza_pesadas.id_pesada_sap = '' THEN
                balanza_pesadas.id_pesada_sap = ''.



            
            cparametros = string(balanza_pesadas.id_pesada_sap , '999999999999') + '|||'.

            FIND transportes_proveedor WHERE transportes_proveedor.id_transporte =  balanza_pesadas.id_transporte NO-LOCK NO-ERROR.
            FIND proveedores OF transportes_proveedor NO-LOCK NO-ERROR.
            IF AVAILABLE proveedores  THEN
                cparametros = cparametros + substring(proveedores.cuit,1,10) + '|'.
            ELSE
                cparametros = cparametros + '0000000000|'.

            IF AVAILABLE transportes_proveedor THEN  cparametros = cparametros + STRING(transportes_proveedor.id_transporte). /* se reeemplazo id_transporte_sap que es lo mismo*/
                                               ELSE  cparametros = cparametros + '9999' .

            FIND tipo_transporte OF transportes_proveedor NO-LOCK NO-ERROR.

            IF AVAILABLE tipo_transporte  THEN xtipotransporte = tipo_transporte.id_tipo_transporte_sap.  ELSE xtipotransporte = ''.

            FIND balanzas OF balanza_pesadas NO-LOCK NO-ERROR. 

            cparametros = cparametros + '|0|CHOFER7890123456789012345||||00000000||||0||||0|0||'. 

            cparametros =   cparametros + string(year(produccion.balanza_pesadas.fecha_entrada),'9999') + 
                            STRING(MONTH(produccion.balanza_pesadas.fecha_entrada),'99') + STRING(DAY(produccion.balanza_pesadas.fecha_entrada),'99') + '|' +
                            REPLACE(balanza_pesadas.hora_entrada,':','') + '||||' + xtipotransporte + '||||||||||||||||' + 
                            trim(replace(substring(balanzas.id_balanza_sap,9,4),'0','')) + '||||'.

            IF AVAILABLE transportes_proveedor THEN cparametros = cparametros  + transportes_proveedor.patente + '|' + transportes_proveedor.patente_acop.
                                               ELSE cparametros = cparametros  + 'SP999|SP999'.

            CASE balanzas.id_sucursal:
                WHEN 97  THEN ASSIGN    xproceso = 'PF' 
                                        hLBalanza:VALUE = 'X'.
                WHEN 98  THEN ASSIGN    xproceso = 'PL' 
                                        hLBalanza:VALUE = 'X'.
                WHEN 99  THEN ASSIGN    xproceso = 'PU' 
                                        hLBalanza:VALUE = 'X'.
                WHEN 95  THEN xproceso = 'IN'.
                WHEN 96  THEN xproceso = 'IN'.
            END CASE.

            IF xentrega_sap = '' THEN
            DO:
                FIND FIRST balanza_tickets OF balanza_pesadas NO-LOCK NO-ERROR.
                FIND envases_prod OF balanza_ticket NO-LOCK.

                cant = 0.

                FOR EACH balanza_tickets OF balanza_pesada NO-LOCK.
                    cant = cant + produccion.balanza_tickets.cant_env_entrada.
                END.

                IF AVAILABLE envases_prod AND xproceso = 'IN' THEN 
                        ASSIGN  xenvase = envases_prod.id_envase_sap
                                xcant = STRING(cant).
            END.
            cant = 0.
            
            cparametros = cparametros + '|000000000000000||||' + xenvase + '|'  + xproceso + '|FI||' + xcant + '|0000000000|||||||||' .

            IF AVAILABLE proveedores THEN
                cparametros = cparametros + SUBSTRING(string(cuit),1,10) + '|||||' + string(produccion.balanza_pesadas.id_pesada_sap,'999999999999')  + '|||||'
                              + xentrega_sap.
            ELSE
                cparametros = cparametros + '0000000000|||||' + string(produccion.balanza_pesadas.id_pesada_sap,'999999999999')  + '|||||' 
                              + xentrega_sap.

            cparametros = cparametros + '|' + string(balanza_pesada.peso_entrada) + '|' + string(balanza_pesada.peso_salida) + '|' +
                     string(year(produccion.balanza_pesadas.fecha_salida),'9999') + 
                     STRING(MONTH(produccion.balanza_pesadas.fecha_salida),'99') + STRING(DAY(produccion.balanza_pesadas.fecha_salida),'99') + '|' +
                     /*** REPLACE(balanza_pesadas.hora_salida,':','') + '|' + string(balanza_pesada.peso_descarte) + '|' +  SE SACO EL PESO DESCARTE ***/
                     REPLACE(balanza_pesadas.hora_salida,':','') + '|' + '' + '|' +
                     STRING(cant) + '|' + string(balanza_pesada.peso_neto).

            OUTPUT TO VALUE("z:\temp\cparametros" + STRING(balanza_pesadas.id_pesada) + ".txt").
                EXPORT CPARAMETROS.
            OUTPUT CLOSE.

            
            hParametros:VALUE = cparametros.
            /* Si es un remito con Entrega SAP, no generamos la tabla
             SAP ztmm_mov_flete_d */
            
            
            IF hfunc:CALL() THEN  DO ON ERROR UNDO , RETURN ERROR 'ERROR GENERAL BALANZA PESADAS':
                balanza_pesadas.orden_carga_sap = hOCarga:VALUE. 
                balanza_pesadas.id_pesada_sap   = STRING(hPesada:VALUE,'999999999999').


                vorigen = SOURCE-PROCEDURE:FILE-NAME.

                CREATE balanza_pesadas2sap.
                BUFFER-COPY balanza_pesadas TO balanza_pesadas2sap.

                ASSIGN  balanza_pesadas2sap.serie    = NEXT-VALUE(pesadas)
                        balanza_pesadas2sap.modificacion = NOW
                        balanza_pesadas2sap.origen = vorigen.
            
           /*-----------------------------------------------------------------
            SE HABIA HABLADO, QUE CUANDO UN REMITO ES REALIZADO POR SAP, EL OPERADOR
            DEBE REGISTRAR EL NRO.ORDEN ENTREGA (CAMPO TARA).  DE ESTA FORMA LA INTERFACE
            DEBE LLENAR UNICAMENTE LA CABECERA (zmm_mov_flete) SIN PASAR LAS POSICIONES
            O ITEMS (zmm_mov_flete_d)
            -----------------------------------------------------------------*/
            
            IF xentrega_sap = '' THEN /***** no me acuerdo porque asi que lo saco *****/
            DO:
                IF trim(balanza_pesadas.orden_carga_sap) <> '1' THEN
                    RUN balanzaTicketsTosap ( balanza_pesadas.id_balanza , balanza_pesadas.id_pesada , xproceso , balanza_pesadas2sap.serie ) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN 
                DO:                        
                    MESSAGE 'ERROR INTERFACE DE ITEMS ' VIEW-AS ALERT-BOX ERROR.
                    hFunc  = hFunctions:ADD('BAPI_ZTMM_MOV_FLETE_delete').
                    hOCarga         = hFunc:exports('I_OCARGA').
                    hPesada         = hFunc:exports('I_TICKET').
                    hOcarga:VALUE = balanza_pesadas.orden_carga_sap.
                    hPesada:VALUE = balanza_pesadas.id_pesada_sap.
                    IF hFunc:CALL() THEN 
                        ASSIGN balanza_pesadas.orden_carga_sap = '1'.
                               balanza_pesadas.id_pesada_sap   = '1'.

                   CMES = 
                  '____________________________________________________________________________________________________________________________' + '~n' +
                  'Hora : ' + STRING(NOW, '99/99/99 hh:mm:ss')  + '~n' +
                  'El ticket de Balanza : ' + string( balanza_pesadas.id_balanza ) + '~n' +
                  '             Pesada  : ' + STRING( balanza_pesadas.id_pesada  ) + '~n' +
                  'no ingreso a SAP , con error en el procesamiento de item en ingreso de fruta ' + RETURN-VALUE + '~n' + 
                  '____________________________________________________________________________________________________________________________'.                   .


                   RUN ffw\procs\smtpmail58.p ("www.softsargentina.com" , 
                                "pdigonzelli@gmail.com,gprochaz@sa-sanmiguel.com,pedrom@sa-sanmiguel.com,rtralice@sa-sanmiguel.com,cvaldes@sa-sanmiguel.com,gabriel@sa-sanmiguel.com" ,
                                "ingresofruta@softsargentina.com" ,
                                "",
                                "" ,
                                "" ,
                                "Ingreso de fruta con error" ,
                                Cmes ,
                                "",
                                "text" ,
                                 1,
                                 YES,
                                 "base64" ,
                                 "ingresofruta@softsargentina.com" ,
                                 "ingresofruta2011",
                                OUTPUT okay ,
                                OUTPUT cmes1) NO-ERROR.
                    hFunctions:connection:logoff().
                    UNDO, RETURN ERROR 'ERROR  BALANZATICKETS TO SAP' . 
                END.
                /***********************************
                CMES = 
               '____________________________________________________________________________________________________________________________' + '~n' +
               'Hora : ' + STRING(NOW, '99/99/99 hh:mm:ss')  + '~n' +
               'El ticket de Balanza : ' + string( balanza_pesadas.id_balanza ) + '~n' +
               '             Pesada  : ' + STRING( balanza_pesadas.id_pesada  ) + '~n' +
               'ingreso correctamente a SAP '  + '~n' +
               ' Orden de Carga SAP  : ' + STRING( balanza_pesadas.orden_carga_sap) + '~n' +
               '         Pesada SAP  : ' + STRING( balanza_pesadas.id_pesada_sap) + '~n' +
               '____________________________________________________________________________________________________________________________'.                   .
        

                RUN ffw\procs\smtpmail58.p ("www.softsargentina.com" , 
                             "pdigonzelli@gmail.com,gprochaz@sa-sanmiguel.com,pedrom@sa-sanmiguel.com,rtralice@sa-sanmiguel.com,vtabernero@sa-sanmiguel.com,gabriel@sa-sanmiguel.com" ,
                             "ingresofruta@softsargentina.com" ,
                             "",
                             "" ,
                             "" ,
                             "Ingreso de Fruta OK" ,
                             Cmes ,
                             "",
                             "text" ,
                              1,
                              YES,
                              "base64" ,
                              "ingresofruta@softsargentina.com" ,
                              "ingresofruta2011",
                             OUTPUT okay ,
                             OUTPUT cmes1) NO-ERROR.
                **********************************/
            END.

            END. 
            ELSE 
            DO:
                CMES = 
               '____________________________________________________________________________________________________________________________' + '~n' + 
               'Hora : ' + STRING(NOW, '99/99/99 hh:mm:ss')  + '~n' +
               'El ticket de Balanza : ' + string( balanza_pesadas.id_balanza ) + '~n' +
               '             Pesada  : ' + STRING( balanza_pesadas.id_pesada  ) + '~n' +
               'dio un error de procesamiento : '  + 'error de x_entrega_sap en ingreso de fruta' +  '~n' +
               '____________________________________________________________________________________________________________________________'.                   .


                RUN ffw\procs\smtpmail58.p ("www.softsargentina.com" , 
                             "pdigonzelli@gmail.com,gprochaz@sa-sanmiguel.com,pedrom@sa-sanmiguel.com,rtralice@sa-sanmiguel.com,cvaldes@sa-sanmiguel.com,gabriel@sa-sanmiguel.com" ,
                             "ingresofruta@softsargentina.com" ,
                             "",
                             "" ,
                             "" ,
                             "Ingreso de fruta con error" ,
                             Cmes ,
                             "",
                             "text" ,
                              1,
                              YES,
                              "base64" ,
                              "ingresofruta@softsargentina.com" ,
                              "ingresofruta2011",
                             OUTPUT okay ,
                             OUTPUT cmes1) NO-ERROR.
                hFunctions:connection:logoff().
                UNDO , RETURN ERROR 'ERROR BALANZAPESADA|TOSAP'.
            END.
        END. 
        ELSE
        DO:
            CMES = 
           '____________________________________________________________________________________________________________________________' + '~n' +
           'Hora : ' + STRING(NOW, '99/99/99 hh:mm:ss')  + '~n' +
           'El ticket de Balanza : ' + string( balanza_pesadas.id_balanza ) + '~n' +
           '             Pesada  : ' + STRING( balanza_pesadas.id_pesada  ) + '~n' +
           'dio un error de procesamiento : '  + 'error de coneccion a SAP en ingreso de fruta' +  '~n' +
           '____________________________________________________________________________________________________________________________'.                   .


            RUN ffw\procs\smtpmail58.p ("www.softsargentina.com" , 
                         "pdigonzelli@gmail.com,gprochaz@sa-sanmiguel.com,pedrom@sa-sanmiguel.com,rtralice@sa-sanmiguel.com,cvaldes@sa-sanmiguel.com,gabriel@sa-sanmiguel.com" ,
                         "ingresofruta@softsargentina.com" ,
                         "",
                         "" ,
                         "" ,
                         "Ingreso de fruta con error" ,
                         Cmes ,
                         "",
                         "text" ,
                          1,
                          YES,
                          "base64" ,
                          "ingresofruta@softsargentina.com" ,
                          "ingresofruta2011",
                         OUTPUT okay ,
                         OUTPUT cmes1) NO-ERROR.
            UNDO , RETURN ERROR 'ERROR CONECCION SAP'.
        END.
    END.

    RETURN balanza_pesadas.orden_carga_sap.
    FINALLY.
        IF VALID-HANDLE(HFUNCTIONS) THEN
            hFunctions:connection:logoff().
    END FINALLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ingresoFrutaToSapEntreFechas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ingresoFrutaToSapEntreFechas Procedure 
PROCEDURE ingresoFrutaToSapEntreFechas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pFechaInicio AS DATE NO-UNDO.
DEFINE INPUT PARAMETER pFechaFin AS DATE NO-UNDO.

DEFINE BUFFER b FOR balanza_pesadas.



FOR EACH b WHERE  id_balanza = 2 OR id_balanza = 4 AND 
                  fecha_entrada >= pFechaInicio AND
                  fecha_entrada < pFechaFin AND
                  ( id_pesada_sap = '' AND orden_carga_sap = '') NO-LOCK.

    RUN ingresoFrutaToSap( INPUT b.id_balanza , INPUT b.id_pesada) NO-ERROR.

END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ingresoFrutaToSapPorLote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ingresoFrutaToSapPorLote Procedure 
PROCEDURE ingresoFrutaToSapPorLote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pBalanza AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pFechaInicio AS DATE NO-UNDO.
DEFINE INPUT PARAMETER pFechaFin AS DATE NO-UNDO.

DEFINE BUFFER b FOR balanza_pesadas.

FOR EACH b WHERE  id_balanza = pBalanza AND  
                                fecha_entrada >= pFechaInicio AND
                                fecha_entrada < pFechaFin AND
                                ( orden_carga_sap = '' OR id_pesada_sap = '')  NO-LOCK.
    RUN ingresoFrutaToSap( INPUT b.id_balanza , 
                           INPUT b.id_pesada).

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ingresoFrutaToSapt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ingresoFrutaToSapt Procedure 
PROCEDURE ingresoFrutaToSapt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pbalanza AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pPesada AS INTEGER NO-UNDO.




DEFINE VARIABLE cparametros     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStatus         AS CHARACTER NO-UNDO.
DEFINE VARIABLE hFunctions      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hFunc           AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hparametros     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hOCarga         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hPesada         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hLBalanza       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE i               AS INTEGER NO-UNDO.
DEFINE VARIABLE xservicio       AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcorte          AS CHARACTER NO-UNDO.
DEFINE VARIABLE xarticulo       AS CHARACTER NO-UNDO.
DEFINE VARIABLE xenvase         AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcolor          AS CHARACTER NO-UNDO.
DEFINE VARIABLE xlote           AS CHARACTER NO-UNDO.
DEFINE VARIABLE xmercado        AS CHARACTER NO-UNDO.
DEFINE VARIABLE xtipotransporte AS CHARACTER NO-UNDO.
DEFINE VARIABLE xproceso        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cant            AS DECIMAL NO-UNDO.
DEFINE VARIABLE xcant           AS CHARACTER NO-UNDO.
DEFINE VARIABLE xentrega_sap    AS CHARACTER NO-UNDO.

DEFINE VARIABLE VORIGEN         AS CHARACTER NO-UNDO.


/* OUTPUT STREAM a-chk TO e:\temp\ztmm_mov_flete.txt APPEND.*/

    FIND balanza_pesadas WHERE  balanza_pesadas.id_balanza = pbalanza AND
                                balanza_pesadas.id_pesada  = pPesada  NO-ERROR NO-WAIT.

    FIND FIRST balanza_tickets OF balanza_pesadas WHERE
        balanza_tickets.id_tipo_cosecha > 0 AND balanza_tickets.id_tipo_cosecha < 4 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE balanza_tickets THEN
        RETURN ERROR 'DESCARTE O PROCESADO !!'.

    /*** Cuando viene con PESO_DESCARTE que es fruta para la Industria,
     no debe entrar en la Interface.
     SE QUITO ESTA CONDICIàN PARA LIQUIDAR TAMBIEN LOS VIAJES  
    IF AVAILABLE balanza_pesadas AND
                 balanza_pesadas.peso_descarte = 0 THEN
                 ****/
    IF balanza_pesadas.orden_carga_sap <> '' OR trim(balanza_pesadas.orden_carga_sap) = '1'  THEN UNDO  , RETURN ERROR 'NO CORRESPONDE TIENE ORDEN DE CARGA'.
    IF balanza_pesadas.id_pesada_sap <> '' OR 
       trim(balanza_pesadas.id_pesada_sap) = '1'  THEN UNDO , RETURN ERROR 'NO CORRESPONDE TIENE ORDEN DE CARGA'.

    DO ON ERROR UNDO , RETURN ERROR 'ERROR':

        IF pbalanza <> 2 AND pbalanza <> 4 THEN UNDO, RETURN ERROR 'BALANZA EQUIVOCADA'.
        
        FIND FIRST balanza_tickets OF balanza_pesadas NO-ERROR.
        IF NOT AVAILABLE balanza_tickets THEN UNDO , RETURN ERROR 'PESADA SIN ITEMS'.
        
        hFunctions = connectToSap().
        
        IF NOT VALID-HANDLE(hFunctions) THEN RETURN ERROR  "error de conecci¢n".
        
        IF VALID-HANDLE(hFunctions) THEN DO:

            hFunc  = hFunctions:ADD('BAPI_ZTMM_MOV_FLETE_CREATE').

            hparametros     = hFunc:exports('LPARAMETROS').
            hOCarga         = hFunc:imports('OCARGA').
            hPesada         = hFunc:imports('OTICKET').
            hLBalanza       = hFunc:exports('LBALANZA').

            /***** indicador acoplado  codigo de balanza **/ 
            /***** numeracion de ticket impreso: numero de pesada ***/
            IF balanza_pesadas.id_pesada_sap = ? OR balanza_pesadas.id_pesada_sap = '' THEN
                balanza_pesadas.id_pesada_sap = ''.



            ASSIGN xentrega_sap = IF balanza_pesadas.tara = 0 THEN ''
                                  ELSE TRIM(STRING(balanza_pesadas.tara)).
            
            cparametros = string(balanza_pesadas.id_pesada_sap , '999999999999') + '|||'.

            FIND transportes_proveedor WHERE transportes_proveedor.id_transporte =  balanza_pesadas.id_transporte NO-LOCK NO-ERROR.
            FIND proveedores OF transportes_proveedor NO-LOCK NO-ERROR.
            IF AVAILABLE proveedores  THEN
                cparametros = cparametros + substring(proveedores.cuit,1,10) + '|'.
            ELSE
                cparametros = cparametros + '0000000000|'.

            IF AVAILABLE transportes_proveedor THEN  cparametros = cparametros + STRING(transportes_proveedor.id_transporte). /* se reeemplazo id_transporte_sap que es lo mismo*/
                                               ELSE  cparametros = cparametros + '9999' .

            FIND tipo_transporte OF transportes_proveedor NO-LOCK NO-ERROR.

            IF AVAILABLE tipo_transporte  THEN xtipotransporte = tipo_transporte.id_tipo_transporte_sap.  ELSE xtipotransporte = ''.

            FIND balanzas OF balanza_pesadas NO-LOCK NO-ERROR. 

            cparametros = cparametros + '|0|CHOFER7890123456789012345||||00000000||||0||||0|0||'. 

            cparametros =   cparametros + string(year(produccion.balanza_pesadas.fecha_entrada),'9999') + 
                            STRING(MONTH(produccion.balanza_pesadas.fecha_entrada),'99') + STRING(DAY(produccion.balanza_pesadas.fecha_entrada),'99') + '|' +
                            REPLACE(balanza_pesadas.hora_entrada,':','') + '||||' + xtipotransporte + '||||||||||||||||' + 
                            trim(replace(substring(balanzas.id_balanza_sap,9,4),'0','')) + '||||'.

            IF AVAILABLE transportes_proveedor THEN cparametros = cparametros  + transportes_proveedor.patente + '|' + transportes_proveedor.patente_acop.
                                               ELSE cparametros = cparametros  + 'SP999|SP999'.

            CASE balanzas.id_sucursal:
                WHEN 97  THEN ASSIGN    xproceso = 'PF' 
                                        hLBalanza:VALUE = 'X'.
                WHEN 98  THEN ASSIGN    xproceso = 'PL' 
                                        hLBalanza:VALUE = 'X'.
                WHEN 99  THEN ASSIGN    xproceso = 'PU' 
                                        hLBalanza:VALUE = 'X'.
                WHEN 95  THEN xproceso = 'IN'.
                WHEN 96  THEN xproceso = 'IN'.
            END CASE.


            FIND FIRST balanza_tickets OF balanza_pesadas NO-LOCK.
            FIND envases_prod OF balanza_ticket NO-LOCK.

            
            FOR EACH balanza_tickets OF balanza_pesada NO-LOCK.
                cant = cant + produccion.balanza_tickets.cant_env_entrada.
            END.
            
            IF AVAILABLE envases_prod AND xproceso = 'IN' THEN 
                    ASSIGN  xenvase = envases_prod.id_envase_sap
                            xcant = STRING(cant).
            cant = 0.
            
            cparametros = cparametros + '|000000000000000||||' + xenvase + '|'  + xproceso + '|FI||' + xcant + '|0000000000|||||||||' .

            IF AVAILABLE proveedores THEN
                cparametros = cparametros + SUBSTRING(string(cuit),1,10) + '|||||' + string(produccion.balanza_pesadas.id_pesada_sap,'999999999999')  + '|||||'
                              + xentrega_sap.
            ELSE
                cparametros = cparametros + '0000000000|||||' + string(produccion.balanza_pesadas.id_pesada_sap,'999999999999')  + '|||||' 
                              + xentrega_sap.

            cparametros = cparametros + '|' + string(balanza_pesada.peso_entrada) + '|' + string(balanza_pesada.peso_salida) + '|' +
                     string(year(produccion.balanza_pesadas.fecha_salida),'9999') + 
                     STRING(MONTH(produccion.balanza_pesadas.fecha_salida),'99') + STRING(DAY(produccion.balanza_pesadas.fecha_salida),'99') + '|' +
                     /*** REPLACE(balanza_pesadas.hora_salida,':','') + '|' + string(balanza_pesada.peso_descarte) + '|' +  SE SACO EL PESO DESCARTE ***/
                     REPLACE(balanza_pesadas.hora_salida,':','') + '|' + '' + '|' +
                     STRING(cant) + '|' + string(balanza_pesada.peso_neto).

            OUTPUT TO VALUE("z:\temp\cparametros" + STRING(balanza_pesadas.id_pesada) + ".txt").
                EXPORT CPARAMETROS.
            OUTPUT CLOSE.

            
            hParametros:VALUE = cparametros.
            /* Si es un remito con Entrega SAP, no generamos la tabla
             SAP ztmm_mov_flete_d */
            IF hFunc:CALL() THEN  DO ON ERROR UNDO , RETURN ERROR 'ERROR GENERAL BALANZA PESADAS':
                balanza_pesadas.orden_carga_sap = hOCarga:VALUE. 
                balanza_pesadas.id_pesada_sap   = STRING(hPesada:VALUE,'999999999999').


            vorigen = SOURCE-PROCEDURE:FILE-NAME.
            
            CREATE balanza_pesadas2sap.
            BUFFER-COPY balanza_pesadas TO balanza_pesadas2sap.

            ASSIGN  balanza_pesadas2sap.serie    = NEXT-VALUE(pesadas)
                    balanza_pesadas2sap.modificacion = NOW
                    balanza_pesadas2sap.origen = vorigen.
           /*-----------------------------------------------------------------
            SE HABIA HABLADO, QUE CUANDO UN REMITO ES REALIZADO POR SAP, EL OPERADOR
            DEBE REGISTRAR EL NRO.ORDEN ENTREGA (CAMPO TARA).  DE ESTA FORMA LA INTERFACE
            DEBE LLENAR UNICAMENTE LA CABECERA (zmm_mov_flete) SIN PASAR LAS POSICIONES
            O ITEMS (zmm_mov_flete_d)
            -----------------------------------------------------------------*/
            
            IF xentrega_sap = '' THEN /***** no me acuerdo porque asi que lo saco *****/
            DO:
                IF trim(balanza_pesadas.orden_carga_sap) <> '1' THEN
                    RUN balanzaTicketsTosap ( balanza_pesadas.id_balanza , balanza_pesadas.id_pesada , xproceso , balanza_pesadas2sap.serie) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN 
                DO:                        
                    MESSAGE 'ERROR INTERFACE DE ITEMS ' VIEW-AS ALERT-BOX ERROR.
                    hFunc  = hFunctions:ADD('BAPI_ZTMM_MOV_FLETE_delete').
                    hOCarga         = hFunc:exports('I_OCARGA').
                    hPesada         = hFunc:exports('I_TICKET').
                    hOcarga:VALUE = balanza_pesadas.orden_carga_sap.
                    hPesada:VALUE = balanza_pesadas.id_pesada_sap.
                    IF hFunc:CALL() THEN 
                        ASSIGN balanza_pesadas.orden_carga_sap = '1'.
                               balanza_pesadas.id_pesada_sap   = '1'.
                    UNDO, RETURN ERROR 'ERROR  BALANZATICKETS TO SAP' . 
                END.
            END.

            END. ELSE UNDO , RETURN ERROR 'ERROR BALANZAPESADA|TOSAP'.
        END. ELSE UNDO , RETURN ERROR 'ERROR CONECCION SAP'.
    END.

    hFunctions:connection:logoff().
    RETURN balanza_pesadas.orden_carga_sap.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ingresoFrutaToSapTablero) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ingresoFrutaToSapTablero Procedure 
PROCEDURE ingresoFrutaToSapTablero :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ingresoFrutaToSapviejo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ingresoFrutaToSapviejo Procedure 
PROCEDURE ingresoFrutaToSapviejo :
/*------------------------------------------------------------------------------
  Purpose: La Tabla balanza_pesada, tiene un campo id_entrega_sap, el cual si 
           contiene datos, la interface £nicamente va a crear un registro en SAP
           sobre ztmm_mov_flete llenando el campo correspondiente de la Entrega SAP     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pbalanza AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pPesada AS INTEGER NO-UNDO.


DEFINE VARIABLE cparametros     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStatus         AS CHARACTER NO-UNDO.
DEFINE VARIABLE hFunctions      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hFunc           AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hparametros     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hOCarga         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hPesada         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hLBalanza       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE i               AS INTEGER NO-UNDO.
DEFINE VARIABLE xservicio       AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcorte          AS CHARACTER NO-UNDO.
DEFINE VARIABLE xarticulo       AS CHARACTER NO-UNDO.
DEFINE VARIABLE xenvase         AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcolor          AS CHARACTER NO-UNDO.
DEFINE VARIABLE xlote           AS CHARACTER NO-UNDO.
DEFINE VARIABLE xmercado        AS CHARACTER NO-UNDO.
DEFINE VARIABLE xtipotransporte AS CHARACTER NO-UNDO.
DEFINE VARIABLE xproceso        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cant            AS DECIMAL NO-UNDO.
DEFINE VARIABLE xcant           AS CHARACTER NO-UNDO.
DEFINE VARIABLE xentrega_sap    AS CHARACTER NO-UNDO.

DEFINE VARIABLE VORIGEN         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cmes            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cmes1           AS CHARACTER NO-UNDO.
DEFINE VARIABLE okay            AS LOGICAL NO-UNDO.

/* OUTPUT STREAM a-chk TO e:\temp\ztmm_mov_flete.txt APPEND.*/

    FIND balanza_pesadas WHERE  balanza_pesadas.id_balanza = pbalanza AND
                                balanza_pesadas.id_pesada  = pPesada  NO-ERROR NO-WAIT.

    FIND FIRST balanza_tickets OF balanza_pesadas WHERE
        balanza_tickets.id_tipo_cosecha > 0 AND balanza_tickets.id_tipo_cosecha < 4 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE balanza_tickets THEN RETURN 'FRUTA PROCESADA O P/INDUSTRIA'.

    /*** Cuando viene con PESO_DESCARTE que es fruta para la Industria,
     no debe entrar en la Interface.
     SE QUITO ESTA CONDICIàN PARA LIQUIDAR TAMBIEN LOS VIAJES  
    IF AVAILABLE balanza_pesadas AND
                 balanza_pesadas.peso_descarte = 0 THEN
                 ****/
    IF balanza_pesadas.orden_carga_sap <> '' OR trim(balanza_pesadas.orden_carga_sap) = '1'  THEN UNDO  , RETURN ERROR 'NO CORRESPONDE TIENE ORDEN DE CARGA'.
    IF balanza_pesadas.id_pesada_sap <> '' OR 
       trim(balanza_pesadas.id_pesada_sap) = '1'  THEN UNDO , RETURN ERROR 'NO CORRESPONDE TIENE ORDEN DE CARGA'.

    DO ON ERROR UNDO , RETURN ERROR 'ERROR':

        IF pbalanza <> 2 AND pbalanza <> 4 THEN UNDO, RETURN ERROR 'BALANZA EQUIVOCADA'.
        
        FIND FIRST balanza_tickets OF balanza_pesadas NO-ERROR.
        IF NOT AVAILABLE balanza_tickets THEN UNDO , RETURN ERROR 'PESADA SIN ITEMS'.
        
        hFunctions = connectToSap().
        
        IF NOT VALID-HANDLE(hFunctions) THEN RETURN ERROR  "error de conecci¢n".
        
        IF VALID-HANDLE(hFunctions) THEN DO:

            hFunc  = hFunctions:ADD('BAPI_ZTMM_MOV_FLETE_CREATE').

            hparametros     = hFunc:exports('LPARAMETROS').
            hOCarga         = hFunc:imports('OCARGA').
            hPesada         = hFunc:imports('OTICKET').
            hLBalanza       = hFunc:exports('LBALANZA').

            /***** indicador acoplado  codigo de balanza **/ 
            /***** numeracion de ticket impreso: numero de pesada ***/
            IF balanza_pesadas.id_pesada_sap = ? OR balanza_pesadas.id_pesada_sap = '' THEN
                balanza_pesadas.id_pesada_sap = ''.



            ASSIGN xentrega_sap = IF balanza_pesadas.tara = 0 THEN ''
                                  ELSE TRIM(STRING(balanza_pesadas.tara)).
            
            cparametros = string(balanza_pesadas.id_pesada_sap , '999999999999') + '|||'.

            FIND transportes_proveedor WHERE transportes_proveedor.id_transporte =  balanza_pesadas.id_transporte NO-LOCK NO-ERROR.
            FIND proveedores OF transportes_proveedor NO-LOCK NO-ERROR.
            IF AVAILABLE proveedores  THEN
                cparametros = cparametros + substring(proveedores.cuit,1,10) + '|'.
            ELSE
                cparametros = cparametros + '0000000000|'.

            IF AVAILABLE transportes_proveedor THEN  cparametros = cparametros + STRING(transportes_proveedor.id_transporte). /* se reeemplazo id_transporte_sap que es lo mismo*/
                                               ELSE  cparametros = cparametros + '9999' .

            FIND tipo_transporte OF transportes_proveedor NO-LOCK NO-ERROR.

            IF AVAILABLE tipo_transporte  THEN xtipotransporte = tipo_transporte.id_tipo_transporte_sap.  ELSE xtipotransporte = ''.

            FIND balanzas OF balanza_pesadas NO-LOCK NO-ERROR. 

            cparametros = cparametros + '|0|CHOFER7890123456789012345||||00000000||||0||||0|0||'. 

            cparametros =   cparametros + string(year(produccion.balanza_pesadas.fecha_entrada),'9999') + 
                            STRING(MONTH(produccion.balanza_pesadas.fecha_entrada),'99') + STRING(DAY(produccion.balanza_pesadas.fecha_entrada),'99') + '|' +
                            REPLACE(balanza_pesadas.hora_entrada,':','') + '||||' + xtipotransporte + '||||||||||||||||' + 
                            trim(replace(substring(balanzas.id_balanza_sap,9,4),'0','')) + '||||'.

            IF AVAILABLE transportes_proveedor THEN cparametros = cparametros  + transportes_proveedor.patente + '|' + transportes_proveedor.patente_acop.
                                               ELSE cparametros = cparametros  + 'SP999|SP999'.

            CASE balanzas.id_sucursal:
                WHEN 97  THEN ASSIGN    xproceso = 'PF' 
                                        hLBalanza:VALUE = 'X'.
                WHEN 98  THEN ASSIGN    xproceso = 'PL' 
                                        hLBalanza:VALUE = 'X'.
                WHEN 99  THEN ASSIGN    xproceso = 'PU' 
                                        hLBalanza:VALUE = 'X'.
                WHEN 95  THEN xproceso = 'IN'.
                WHEN 96  THEN xproceso = 'IN'.
            END CASE.


            FIND FIRST balanza_tickets OF balanza_pesadas NO-LOCK.
            FIND envases_prod OF balanza_ticket NO-LOCK.

            
            FOR EACH balanza_tickets OF balanza_pesada NO-LOCK.
                cant = cant + produccion.balanza_tickets.cant_env_entrada.
            END.
            
            IF AVAILABLE envases_prod AND xproceso = 'IN' THEN 
                    ASSIGN  xenvase = envases_prod.id_envase_sap
                            xcant = STRING(cant).
            cant = 0.
            
            cparametros = cparametros + '|000000000000000||||' + xenvase + '|'  + xproceso + '|FI||' + xcant + '|0000000000|||||||||' .

            IF AVAILABLE proveedores THEN
                cparametros = cparametros + SUBSTRING(string(cuit),1,10) + '|||||' + string(produccion.balanza_pesadas.id_pesada_sap,'999999999999')  + '|||||'
                              + xentrega_sap.
            ELSE
                cparametros = cparametros + '0000000000|||||' + string(produccion.balanza_pesadas.id_pesada_sap,'999999999999')  + '|||||' 
                              + xentrega_sap.

            cparametros = cparametros + '|' + string(balanza_pesada.peso_entrada) + '|' + string(balanza_pesada.peso_salida) + '|' +
                     string(year(produccion.balanza_pesadas.fecha_salida),'9999') + 
                     STRING(MONTH(produccion.balanza_pesadas.fecha_salida),'99') + STRING(DAY(produccion.balanza_pesadas.fecha_salida),'99') + '|' +
                     /*** REPLACE(balanza_pesadas.hora_salida,':','') + '|' + string(balanza_pesada.peso_descarte) + '|' +  SE SACO EL PESO DESCARTE ***/
                     REPLACE(balanza_pesadas.hora_salida,':','') + '|' + '' + '|' +
                     STRING(cant) + '|' + string(balanza_pesada.peso_neto).

            OUTPUT TO VALUE("z:\temp\cparametros" + STRING(balanza_pesadas.id_pesada) + ".txt").
                EXPORT CPARAMETROS.
            OUTPUT CLOSE.

            
            hParametros:VALUE = cparametros.
            /* Si es un remito con Entrega SAP, no generamos la tabla
             SAP ztmm_mov_flete_d */
            IF hFunc:CALL() THEN  DO ON ERROR UNDO , RETURN ERROR 'ERROR GENERAL BALANZA PESADAS':
                balanza_pesadas.orden_carga_sap = hOCarga:VALUE. 
                balanza_pesadas.id_pesada_sap   = STRING(hPesada:VALUE,'999999999999').

                vorigen = SOURCE-PROCEDURE:FILE-NAME.

                CREATE balanza_pesadas2sap.
                BUFFER-COPY balanza_pesadas TO balanza_pesadas2sap.

                ASSIGN  balanza_pesadas2sap.serie    = NEXT-VALUE(pesadas)
                        balanza_pesadas2sap.modificacion = NOW
                        balanza_pesadas2sap.origen = vorigen.
            
           /*-----------------------------------------------------------------
            SE HABIA HABLADO, QUE CUANDO UN REMITO ES REALIZADO POR SAP, EL OPERADOR
            DEBE REGISTRAR EL NRO.ORDEN ENTREGA (CAMPO TARA).  DE ESTA FORMA LA INTERFACE
            DEBE LLENAR UNICAMENTE LA CABECERA (zmm_mov_flete) SIN PASAR LAS POSICIONES
            O ITEMS (zmm_mov_flete_d)
            -----------------------------------------------------------------*/
            
            IF xentrega_sap = '' THEN /***** no me acuerdo porque asi que lo saco *****/
            DO:
                IF trim(balanza_pesadas.orden_carga_sap) <> '1' THEN
                    RUN balanzaTicketsTosap ( balanza_pesadas.id_balanza , balanza_pesadas.id_pesada , xproceso , balanza_pesadas2sap.serie ) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN 
                DO:                        
                    hFunc  = hFunctions:ADD('BAPI_ZTMM_MOV_FLETE_delete').
                    hOCarga         = hFunc:exports('I_OCARGA').
                    hPesada         = hFunc:exports('I_TICKET').
                    hOcarga:VALUE = balanza_pesadas.orden_carga_sap.
                    hPesada:VALUE = balanza_pesadas.id_pesada_sap.
                    IF hFunc:CALL() THEN 
                        ASSIGN balanza_pesadas.orden_carga_sap = '1'.
                               balanza_pesadas.id_pesada_sap   = '1'.

                   CMES = 
                  '____________________________________________________________________________________________________________________________' + '~n' +
                  'Hora : ' + STRING(NOW, '99/99/99 hh:mm:ss')  + '~n' +
                  'El ticket de Balanza : ' + string( balanza_pesadas.id_balanza ) + '~n' +
                  '             Pesada  : ' + STRING( balanza_pesadas.id_pesada  ) + '~n' +
                  'no ingreso a SAP , con error en el procesamiento de item en ingreso de fruta ' + RETURN-VALUE + '~n' + 
                  '____________________________________________________________________________________________________________________________'.                   .


                   RUN ffw\procs\smtpmail58.p ("www.softsargentina.com" , 
                                "pdigonzelli@gmail.com,gprochaz@sa-sanmiguel.com,pedrom@sa-sanmiguel.com,rtralice@sa-sanmiguel.com,cValdes@sa-sanmiguel.com,gabriel@sa-sanmiguel.com" ,
                                "ingresofruta@softsargentina.com" ,
                                "",
                                "" ,
                                "" ,
                                "Ingreso de fruta con error" ,
                                Cmes ,
                                "",
                                "text" ,
                                 1,
                                 YES,
                                 "base64" ,
                                 "ingresofruta@softsargentina.com" ,
                                 "ingresofruta2011",
                                OUTPUT okay ,
                                OUTPUT cmes1) NO-ERROR.
                    UNDO, RETURN ERROR 'ERROR  BALANZATICKETS TO SAP' . 
                END.
                /***********************************
                CMES = 
               '____________________________________________________________________________________________________________________________' + '~n' +
               'Hora : ' + STRING(NOW, '99/99/99 hh:mm:ss')  + '~n' +
               'El ticket de Balanza : ' + string( balanza_pesadas.id_balanza ) + '~n' +
               '             Pesada  : ' + STRING( balanza_pesadas.id_pesada  ) + '~n' +
               'ingreso correctamente a SAP '  + '~n' +
               ' Orden de Carga SAP  : ' + STRING( balanza_pesadas.orden_carga_sap) + '~n' +
               '         Pesada SAP  : ' + STRING( balanza_pesadas.id_pesada_sap) + '~n' +
               '____________________________________________________________________________________________________________________________'.                   .
        

                RUN ffw\procs\smtpmail58.p ("www.softsargentina.com" , 
                             "pdigonzelli@gmail.com,gprochaz@sa-sanmiguel.com,pedrom@sa-sanmiguel.com,rtralice@sa-sanmiguel.com,vtabernero@sa-sanmiguel.com,gabriel@sa-sanmiguel.com" ,
                             "ingresofruta@softsargentina.com" ,
                             "",
                             "" ,
                             "" ,
                             "Ingreso de Fruta OK" ,
                             Cmes ,
                             "",
                             "text" ,
                              1,
                              YES,
                              "base64" ,
                              "ingresofruta@softsargentina.com" ,
                              "ingresofruta2011",
                             OUTPUT okay ,
                             OUTPUT cmes1) NO-ERROR.
                **********************************/
            END.

            END. 
            ELSE 
            DO:
                CMES = 
               '____________________________________________________________________________________________________________________________' + '~n' + 
               'Hora : ' + STRING(NOW, '99/99/99 hh:mm:ss')  + '~n' +
               'El ticket de Balanza : ' + string( balanza_pesadas.id_balanza ) + '~n' +
               '             Pesada  : ' + STRING( balanza_pesadas.id_pesada  ) + '~n' +
               'dio un error de procesamiento : '  + 'error de x_entrega_sap en ingreso de fruta' +  '~n' +
               '____________________________________________________________________________________________________________________________'.                   .


                RUN ffw\procs\smtpmail58.p ("www.softsargentina.com" , 
                             "pdigonzelli@gmail.com,gprochaz@sa-sanmiguel.com,pedrom@sa-sanmiguel.com,rtralice@sa-sanmiguel.com,cvaldes@sa-sanmiguel.com,gabriel@sa-sanmiguel.com" ,
                             "ingresofruta@softsargentina.com" ,
                             "",
                             "" ,
                             "" ,
                             "Ingreso de fruta con error" ,
                             Cmes ,
                             "",
                             "text" ,
                              1,
                              YES,
                              "base64" ,
                              "ingresofruta@softsargentina.com" ,
                              "ingresofruta2011",
                             OUTPUT okay ,
                             OUTPUT cmes1) NO-ERROR.
                UNDO , RETURN ERROR 'ERROR BALANZAPESADA|TOSAP'.
            END.
        END. 
        ELSE
        DO:
            CMES = 
           '____________________________________________________________________________________________________________________________' + '~n' +
           'Hora : ' + STRING(NOW, '99/99/99 hh:mm:ss')  + '~n' +
           'El ticket de Balanza : ' + string( balanza_pesadas.id_balanza ) + '~n' +
           '             Pesada  : ' + STRING( balanza_pesadas.id_pesada  ) + '~n' +
           'dio un error de procesamiento : '  + 'error de coneccion a SAP en ingreso de fruta' +  '~n' +
           '____________________________________________________________________________________________________________________________'.                   .


            RUN ffw\procs\smtpmail58.p ("www.softsargentina.com" , 
                         "pdigonzelli@gmail.com,gprochaz@sa-sanmiguel.com,pedrom@sa-sanmiguel.com,rtralice@sa-sanmiguel.com,cvaldes@sa-sanmiguel.com,gabriel@sa-sanmiguel.com" ,
                         "ingresofruta@softsargentina.com" ,
                         "",
                         "" ,
                         "" ,
                         "Ingreso de fruta con error" ,
                         Cmes ,
                         "",
                         "text" ,
                          1,
                          YES,
                          "base64" ,
                          "ingresofruta@softsargentina.com" ,
                          "ingresofruta2011",
                         OUTPUT okay ,
                         OUTPUT cmes1) NO-ERROR.
            UNDO , RETURN ERROR 'ERROR CONECCION SAP'.
        END.
    END.

    hFunctions:connection:logoff().
    RETURN balanza_pesadas.orden_carga_sap.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-itemsPalletsToSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE itemsPalletsToSap Procedure 
PROCEDURE itemsPalletsToSap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER iSucTrabajo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER iPallet AS INTEGER NO-UNDO. 
  DEFINE INPUT PARAMETER cPalletSap AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER hFunctions AS COM-HANDLE NO-UNDO.
  DEFINE OUTPUT PARAMETER iRecnum AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER cStatus AS CHARACTER.

  
  
/*  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO. */
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.

  DEFINE VARIABLE   hDocu       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE   hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRow        AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRows       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hParametros AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hReturn     AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR iCodigo AS INTEGER NO-UNDO.
  DEFINE VAR cCentro AS CHARACTER NO-UNDO.

  DEFINE VAR par AS INTEGER INITIAL 0.


  DEFINE VARIABLE cFecha AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPallet AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cParametros AS CHARACTER NO-UNDO.

/*  hFunctions = connectToSap(). */

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc         = hFunctions:ADD('BAPI_PALLET_POS_CREATE').
      
      hParametros   = hFunc:exports('LPARAMETROS'). /*ID PALLET*/
      hReturn       = hFunc:imports('o_return'). /*ID PALLET*/


      FIND pallets WHERE pallets.id_suc_trabajo = ISucTrabajo AND
                         pallets.id_pallet = iPallet NO-LOCK NO-ERROR.
      
      IF AVAILABLE pallets  THEN DO ON ERROR UNDO , RETURN ERROR:

          
          FIND items_pedidos_packing OF pallets NO-LOCK NO-ERROR.
          FIND pedidos_packing OF items_pedidos_packing NO-LOCK NO-ERROR.
          
                  FOR EACH items_pallets OF pallets NO-LOCK.

                      FIND fechas_produccion WHERE fechas_produccion.fecha = pallets.fecha_prod NO-LOCK NO-ERROR.
                      FIND tipos_procesos WHERE tipos_procesos.id_tipo_proceso = TRIM(pallets.tipo_proceso) NO-LOCK NO-ERROR.
                      FIND packing OF pallets NO-LOCK NO-ERROR.
                      FIND colores OF items_pallets NO-LOCK NO-ERROR.
                      FIND proveedores OF items_pallet NO-LOCK NO-ERROR.
                      FIND origenes OF items_pallet NO-LOCK NO-ERROR.
                      FIND lote OF items_pallet NO-LOCK NO-ERROR.


                      cparametros = string(cPalletSap,'x(10)') + '|' + string(items_pallets.ITEM_pallet,'999999') + '|' .
                      IF AVAILABLE fechas_produccion  THEN cparametros = cparametros + fechas_produccion.fecha_senasa. 
                                                      ELSE cparametros = cparametros  + 'SF'.

                      cparametros = cparametros + '|'.


                      IF AVAILABLE tipos_procesos    THEN cparametros = cparametros + tipos_procesos.id_tipo_proceso. 
                                                     ELSE cparametros = cparametros  + 'SIN PROCESO'.

                      cparametros = cparametros + '|'.


                      IF AVAILABLE colores          THEN cparametros = cparametros + string(colores.id_color_sap). 
                                                    ELSE cparametros = cparametros  + 'SIN COLOR'.

                     cparametros = cparametros + '|'.

                     IF pallets.testigo THEN  cparametros = cparametros + 'X'.
                                        ELSE  cparametros = cparametros + ' '.

                     cparametros = cparametros + '|'.
                      
                      cparametros = cparametros + string(items_pallets.id_turno_packing) + '|' + items_pallets.codigo_trazabilidad + '|' +
                                    SUBstring( STRING(items_pallet.calibre),1,LENGTH(STRING(items_pallet.calibre)) - 1 ) + '|' + string(items_pallets.bultos) + '|0|'.

                      IF AVAILABLE proveedores THEN  cparametros = cparametros + SUBSTRING(STRING(proveedores.cuit),1,10).
                                               ELSE  cparametros = cparametros + '9999999999'.

                      cparametros = cparametros + '|' .

                      IF AVAILABLE origenes THEN cparametros = cparametros + substring(origenes.id_origen_sap,5,4).
                                            ELSE cparametros = cparametros + '9999'.

                      cparametros = cparametros + '|' + lote.descripcion + '|'.
                      
                      IF  pallets.union_europea THEN  cparametros = cparametros + 'X'. ELSE cparametros = cparametros + ' '.
                      cparametros = cparametros + '|' .
                      IF  pallets.china THEN  cparametros = cparametros + 'X'. ELSE cparametros = cparametros + ' '.
                      cparametros = cparametros + '|' .

                      IF AVAILABLE origenes THEN cparametros = cparametros + string(origenes.id_finca_senasa,'9999').
                                            ELSE cparametros = cparametros + '9999'.

                                                 cparametros = cparametros + '|' .
                      IF AVAILABLE lote     THEN cparametros = cparametros + string(lote.id_lote_senasa,'999').
                                            ELSE cparametros = cparametros + '999'.

                                                 cparametros = cparametros + '|' .
                      IF AVAILABLE lote     THEN cparametros = cparametros + lote.certificado.
                                            ELSE cparametros = cparametros + ''.
                      
                                                 cparametros = cparametros + '|' .
                      IF AVAILABLE lote     THEN cparametros = cparametros + lote.cert_china.
                                            ELSE cparametros = cparametros + ''.
                                                 cparametros = cparametros + '|' .
                      cparametros = cparametros + string(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet , '99999999999').                                                 
                      hparametros:VALUE = cparametros.
                      IF hFunc:CALL() THEN 
                      DO:
                         IF hReturn:VALUE <> cPalletSap THEN  
                         DO:
                             cStatus = 'Error de numeracion de pallet pos'.
                             UNDO , RETURN ERROR.
                         END.
                         cStatus =  'OK'.
                      END.
                      ELSE 
                      DO: 
                          cstatus = 'Error de funcion de items'.
                          UNDO , RETURN ERROR.
                      END.
                  END.
          END. 
          ELSE
          DO:
              cstatus = 'Pallet inexistente'.
              UNDO , RETURN ERROR.
          END.

  END. 
  ELSE 
  DO:
          cstatus = 'Error de conneccion en items de pallet Sap'.
          UNDO , RETURN ERROR.
  END.
/*  hFunctions:connection:logoff(). */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-itemsPalletstoSap1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE itemsPalletstoSap1 Procedure 
PROCEDURE itemsPalletstoSap1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER iSucTrabajo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER iPallet AS INTEGER NO-UNDO. 
  DEFINE OUTPUT PARAMETER iRecnum AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER cStatus AS CHARACTER.

  
  
/*  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO. */
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.

  DEFINE VARIABLE   hDocu       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE   hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRow        AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRows       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hParametros AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hReturn     AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR iCodigo AS INTEGER NO-UNDO.
  DEFINE VAR cCentro AS CHARACTER NO-UNDO.

  DEFINE VAR par AS INTEGER INITIAL 0.


  DEFINE VARIABLE cFecha AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPallet AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cParametros AS CHARACTER NO-UNDO.

  DEFINE VARIABLE
       hFunctions AS COM-HANDLE NO-UNDO.

  hFunctions = connectToSap().

  IF VALID-HANDLE(hFunctions) THEN DO:
      

      hFunc         = hFunctions:ADD('BAPI_PALLET_POS_CREATE').
      
      hParametros   = hFunc:exports('LPARAMETROS'). /*ID PALLET*/
      hReturn       = hFunc:imports('o_return'). /*ID PALLET*/


      FIND pallets WHERE pallets.id_suc_trabajo = ISucTrabajo AND
                         pallets.id_pallet = iPallet NO-LOCK NO-ERROR.
      
      IF AVAILABLE pallets  THEN DO ON ERROR UNDO , RETURN ERROR:

          
          FIND items_pedidos_packing OF pallets NO-LOCK NO-ERROR.
          FIND pedidos_packing OF items_pedidos_packing NO-LOCK NO-ERROR.
          
                  FOR EACH items_pallets OF pallets NO-LOCK.

                      FIND fechas_produccion WHERE fechas_produccion.fecha = pallets.fecha_prod NO-LOCK NO-ERROR.
                      FIND tipos_procesos WHERE tipos_procesos.id_tipo_proceso = TRIM(pallets.tipo_proceso) NO-LOCK NO-ERROR.
                      FIND packing OF pallets NO-LOCK NO-ERROR.
                      FIND colores OF items_pallets NO-LOCK NO-ERROR.
                      FIND proveedores OF items_pallet NO-LOCK NO-ERROR.
                      FIND origenes OF items_pallet NO-LOCK NO-ERROR.
                      FIND lote OF items_pallet NO-LOCK NO-ERROR.


                      cparametros = string(pallets.id_pallet_sap,'x(10)') + '|' + string(items_pallets.ITEM_pallet,'999999') + '|' .
                      IF AVAILABLE fechas_produccion  THEN cparametros = cparametros + fechas_produccion.fecha_senasa. 
                                                      ELSE cparametros = cparametros  + 'SF'.

                      cparametros = cparametros + '|'.


                      IF AVAILABLE tipos_procesos    THEN cparametros = cparametros + tipos_procesos.id_tipo_proceso. 
                                                     ELSE cparametros = cparametros  + 'SIN PROCESO'.

                      cparametros = cparametros + '|'.


                      IF AVAILABLE colores          THEN cparametros = cparametros + string(colores.id_color_sap). 
                                                    ELSE cparametros = cparametros  + 'SIN COLOR'.

                     cparametros = cparametros + '|'.

                     IF pallets.testigo THEN  cparametros = cparametros + 'X'.
                                        ELSE  cparametros = cparametros + ' '.

                     cparametros = cparametros + '|'.
                      
                      cparametros = cparametros + string(items_pallets.id_turno_packing) + '|' + items_pallets.codigo_trazabilidad + '|' +
                                    SUBstring( STRING(items_pallet.calibre),1,LENGTH(STRING(items_pallet.calibre)) - 1 ) + '|' + string(items_pallets.bultos) + '|0|'.

                      IF AVAILABLE proveedores THEN  cparametros = cparametros + SUBSTRING(STRING(proveedores.cuit),1,10).
                                               ELSE  cparametros = cparametros + '9999999999'.

                      cparametros = cparametros + '|' .

                      IF AVAILABLE origenes THEN cparametros = cparametros + substring(origenes.id_origen_sap,5,4).
                                            ELSE cparametros = cparametros + '9999'.

                      cparametros = cparametros + '|' + lote.descripcion + '|'.
                      
                      IF  pallets.union_europea THEN  cparametros = cparametros + 'X'. ELSE cparametros = cparametros + ' '.
                      cparametros = cparametros + '|' .
                      IF  pallets.china THEN  cparametros = cparametros + 'X'. ELSE cparametros = cparametros + ' '.
                      cparametros = cparametros + '|' .

                      IF AVAILABLE origenes THEN cparametros = cparametros + string(origenes.id_finca_senasa,'9999').
                                            ELSE cparametros = cparametros + '9999'.

                                                 cparametros = cparametros + '|' .
                      IF AVAILABLE lote     THEN cparametros = cparametros + string(lote.id_lote_senasa,'999').
                                            ELSE cparametros = cparametros + '999'.

                                                 cparametros = cparametros + '|' .
                      IF AVAILABLE lote     THEN cparametros = cparametros + lote.certificado.
                                            ELSE cparametros = cparametros + ''.
                      
                                                 cparametros = cparametros + '|' .
                      IF AVAILABLE lote     THEN cparametros = cparametros + lote.cert_china.
                                            ELSE cparametros = cparametros + ''.
                                                 cparametros = cparametros + '|' .
                      cparametros = cparametros + string(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet , '99999999999').                                                 
                      hparametros:VALUE = cparametros.
                      IF hFunc:CALL() THEN 
                      DO:
                         IF hReturn:VALUE <> pallets.id_pallet_sap THEN  
                         DO:
                             cStatus = 'Error de numeracion de pallet pos'.
                             MESSAGE cstatus VIEW-AS ALERT-BOX.
                             
                             UNDO , RETURN ERROR.
                         END.
                         cStatus =  'OK'.
                      END.
                      ELSE 
                      DO: 
                          cstatus = 'Error de funcion de items'.
                          MESSAGE cstatus VIEW-AS ALERT-BOX.
                          UNDO , RETURN ERROR.
                      END.
                  END.
          END. 
          ELSE
          DO:
              cstatus = 'Pallet inexistente'.
              MESSAGE cstatus VIEW-AS ALERT-BOX.

              UNDO , RETURN ERROR.
          END.

  END. 
  ELSE 
  DO:
          cstatus = 'Error de funciones Sap'.
          MESSAGE cstatus VIEW-AS ALERT-BOX.
          
          UNDO , RETURN ERROR.
  END.
    hFunctions:connection:logoff().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-itemsPalletsToSapPrueba) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE itemsPalletsToSapPrueba Procedure 
PROCEDURE itemsPalletsToSapPrueba :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iSucTrabajo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER iPallet AS INTEGER NO-UNDO. 
  DEFINE INPUT PARAMETER cPalletSap AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER hFunctions AS COM-HANDLE NO-UNDO.
  DEFINE OUTPUT PARAMETER iRecnum AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER cStatus AS CHARACTER.

  
  
/*  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO. */
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.

  DEFINE VARIABLE   hDocu       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE   hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRow        AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRows       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hParametros AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hReturn     AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR iCodigo AS INTEGER NO-UNDO.
  DEFINE VAR cCentro AS CHARACTER NO-UNDO.

  DEFINE VAR par AS INTEGER INITIAL 0.


  DEFINE VARIABLE cFecha AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPallet AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cParametros AS CHARACTER NO-UNDO.

/*  hFunctions = connectToSap(). */

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc         = hFunctions:ADD('BAPI_PALLET_POS_CREATE').
      
      hParametros   = hFunc:exports('LPARAMETROS'). /*ID PALLET*/
      hReturn       = hFunc:imports('o_return'). /*ID PALLET*/


      FIND pallets WHERE pallets.id_suc_trabajo = ISucTrabajo AND
                         pallets.id_pallet = iPallet NO-LOCK NO-ERROR.
      
      IF AVAILABLE pallets  THEN DO ON ERROR UNDO , RETURN ERROR:

          
          FIND items_pedidos_packing OF pallets NO-LOCK NO-ERROR.
          FIND pedidos_packing OF items_pedidos_packing NO-LOCK NO-ERROR.
          
                  FOR EACH items_pallets OF pallets NO-LOCK.

                      FIND fechas_produccion WHERE fechas_produccion.fecha = pallets.fecha_prod NO-LOCK NO-ERROR.
                      FIND tipos_procesos WHERE tipos_procesos.id_tipo_proceso = TRIM(pallets.tipo_proceso) NO-LOCK NO-ERROR.
                      FIND packing OF pallets NO-LOCK NO-ERROR.
                      FIND colores OF items_pallets NO-LOCK NO-ERROR.
                      FIND proveedores OF items_pallet NO-LOCK NO-ERROR.
                      FIND origenes OF items_pallet NO-LOCK NO-ERROR.
                      FIND lote OF items_pallet NO-LOCK NO-ERROR.


                      cparametros = string(cPalletSap,'x(10)') + '|' + string(items_pallets.ITEM_pallet,'999999') + '|' .
                      IF AVAILABLE fechas_produccion  THEN cparametros = cparametros + fechas_produccion.fecha_senasa. 
                                                      ELSE cparametros = cparametros  + 'SF'.

                      cparametros = cparametros + '|'.


                      IF AVAILABLE tipos_procesos    THEN cparametros = cparametros + tipos_procesos.id_tipo_proceso. 
                                                     ELSE cparametros = cparametros  + 'SIN PROCESO'.

                      cparametros = cparametros + '|'.


                      IF AVAILABLE colores          THEN cparametros = cparametros + string(colores.id_color_sap). 
                                                    ELSE cparametros = cparametros  + 'SIN COLOR'.

                     cparametros = cparametros + '|'.

                     IF pallets.testigo THEN  cparametros = cparametros + 'X'.
                                        ELSE  cparametros = cparametros + ' '.

                     cparametros = cparametros + '|'.
                      
                      cparametros = cparametros + string(items_pallets.id_turno_packing) + '|' + items_pallets.codigo_trazabilidad + '|' +
                                    SUBstring( STRING(items_pallet.calibre),1,LENGTH(STRING(items_pallet.calibre)) - 1 ) + '|' + string(items_pallets.bultos) + '|0|'.

                      IF AVAILABLE proveedores THEN  cparametros = cparametros + SUBSTRING(STRING(proveedores.cuit),1,10).
                                               ELSE  cparametros = cparametros + '9999999999'.

                      cparametros = cparametros + '|' .

                      IF AVAILABLE origenes THEN cparametros = cparametros + substring(origenes.id_origen_sap,5,4).
                                            ELSE cparametros = cparametros + '9999'.

                      cparametros = cparametros + '|' + lote.descripcion + '|'.
                      
                      IF  pallets.union_europea THEN  cparametros = cparametros + 'X'. ELSE cparametros = cparametros + ' '.
                      cparametros = cparametros + '|' .
                      IF  pallets.china THEN  cparametros = cparametros + 'X'. ELSE cparametros = cparametros + ' '.
                      cparametros = cparametros + '|' .

                      IF AVAILABLE origenes THEN cparametros = cparametros + string(origenes.id_finca_senasa,'9999').
                                            ELSE cparametros = cparametros + '9999'.

                                                 cparametros = cparametros + '|' .
                      IF AVAILABLE lote     THEN cparametros = cparametros + string(lote.id_lote_senasa,'999').
                                            ELSE cparametros = cparametros + '999'.

                                                 cparametros = cparametros + '|' .
                      IF AVAILABLE lote     THEN cparametros = cparametros + lote.certificado.
                                            ELSE cparametros = cparametros + ''.
                      
                                                 cparametros = cparametros + '|' .
                      IF AVAILABLE lote     THEN cparametros = cparametros + lote.cert_china.
                                            ELSE cparametros = cparametros + ''.
                                                 cparametros = cparametros + '|' .
                      cparametros = cparametros + string(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet , '99999999999').                                                 
                      hparametros:VALUE = cparametros.
                      IF hFunc:CALL() THEN 
                      DO:
                         IF hReturn:VALUE <> cPalletSap THEN  
                         DO:
                             cStatus = 'Error de numeracion de pallet pos'.
                             UNDO , RETURN ERROR.
                         END.
                         cStatus =  'OK'.
                      END.
                      ELSE 
                      DO: 
                          cstatus = 'Error de funcion de items'.
                          UNDO , RETURN ERROR.
                      END.
                  END.
          END. 
          ELSE
          DO:
              cstatus = 'Pallet inexistente'.
              UNDO , RETURN ERROR.
          END.

  END. 
  ELSE 
  DO:
          cstatus = 'Error de funciones Sap'.
          UNDO , RETURN ERROR.
  END.
/*  hFunctions:connection:logoff(). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-modificaRemito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE modificaRemito Procedure 
PROCEDURE modificaRemito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER PIORDCARGA AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER PIPOSICION AS CHARACTER  NO-UNDO FORMAT '9999'.
DEFINE INPUT PARAMETER PIREMITO   AS CHARACTER  NO-UNDO.

DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hOCARGA     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hPOSICION   AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hREMITO     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   HSTATUS     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   HPARAM      AS COM-HANDLE NO-UNDO.

DEFINE VAR ccarga AS CHARACTER NO-UNDO.
DEFINE VAR cposicion AS CHARACTER NO-UNDO.
DEFINE VAR cremito AS CHARACTER NO-UNDO.


hFunctions = connectToSap().

IF LENGTH(piordcarga) < 15 THEN
    ccarga = FILL ('0', 15 - LENGTH(piordcarga)) + ccarga.
ELSE
    ccarga = piordcarga.

IF LENGTH(piposicion) < 4 THEN
    cposicion = FILL ('0', 4 - LENGTH(piposicion)) + cposicion.
ELSE
    cposicion = piposicion.

IF LENGTH(piremito) < 13 THEN
    cremito = FILL (' ', 13 - LENGTH(piremito)) + piremito.
ELSE
    cremito = piremito.


IF VALID-HANDLE(hFunctions) THEN DO:
  
  hFunc         = hFunctions:ADD('ZMODIFREMITOINGRESO').
  hOCARGA       = hFUnc:exports('IOCARGA').
  HPOSICION     = hFUnc:exports('IPOSICION').
  HREMITO       = hFUnc:exports('IREMITO').
  HSTATUS       = HFUNC:IMPORTS("ISTATUS").
  HPARAM        = HFUNC:IMPORTS("IPARAM"). 
  

  HOCARGA:VALUE     = ccarga.
  HPOSICION:VALUE   = piposicion.
  HREMITO:VALUE     = cremito.
  
  IF NOT hFunc:CALL() THEN DO:
      hFunctions:connection:logoff().
      RETURN ERROR  'Error en la ejecucion de la modificacion de remitos'.
  END.
  IF INTEGER(HSTATUS:VALUE) <> 0 THEN RETURN ERROR 'Funcion de modificacion retorna el valor ' + HSTATUS:VALUE /* + ' '  + HPARAM:VALUE */.
  hFunctions:connection:logoff().
END.
ELSE RETURN ERROR 'ERROR DE CONNECION CON SAP'.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MovInternos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MovInternos Procedure 
PROCEDURE MovInternos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pCodbarra            AS CHARACTER NO-UNDO FORMAT 'X(30)'.
DEFINE INPUT PARAMETER pFechaMov            AS DATE NO-UNDO.
DEFINE INPUT PARAMETER pCentro              AS CHARACTER NO-UNDO FORMAT 'X(4)'.
DEFINE INPUT PARAMETER pAlmacenOrigen       AS CHARACTER NO-UNDO FORMAT 'X(4)'.
DEFINE INPUT PARAMETER pAlmacenDestino      AS CHARACTER NO-UNDO FORMAT 'X(4)'.
DEFINE INPUT PARAMETER pCantidad            AS DECIMAL DECIMALS 3. /*Decimal 3*/
DEFINE INPUT PARAMETER pUnidadMedida        AS CHARACTER FORMAT 'X(3)'.  /*Integer*/
DEFINE INPUT PARAMETER pOperacion           AS CHARACTER NO-UNDO FORMAT 'X(2)'.
DEFINE INPUT PARAMETER pDocumen             AS CHARACTER NO-UNDO FORMAT 'X(20)'.
DEFINE INPUT PARAMETER pPArametros          AS CHARACTER NO-UNDO FORMAT 'X(100)'.
DEFINE OUTPUT PARAMETER pError              AS CHARACTER NO-UNDO FORMAT 'X(100)'.
DEFINE OUTPUT PARAMETER pNumDoc             AS CHARACTER NO-UNDO FORMAT 'X(20)'.

DEFINE VARIABLE hFunctions      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hFunc           AS COM-HANDLE NO-UNDO.

DEFINE VAR cCodbarra            AS CHARACTER NO-UNDO FORMAT 'X(20)'.
DEFINE VAR cFechaMov            AS CHARACTER NO-UNDO FORMAT 'X(10)'.
DEFINE VAR cCentro              AS CHARACTER NO-UNDO FORMAT 'X(4)'.
DEFINE VAR cAlmacenOrigen       AS CHARACTER NO-UNDO FORMAT 'X(4)'.
DEFINE VAR cAlmacenDestino      AS CHARACTER NO-UNDO FORMAT 'X(4)'.
DEFINE VAR cCantidad            AS CHARACTER NO-UNDO FORMAT 'X(13)'. /*Decimal 3*/
DEFINE VAR cUnidadMedida        AS CHARACTER NO-UNDO FORMAT 'X(3)'.  /*Integer*/
DEFINE VAR cOperacion           AS CHARACTER NO-UNDO FORMAT 'X'.
DEFINE VAR cDocumen             AS CHARACTER NO-UNDO FORMAT 'X(20)'.
DEFINE VAR cPArametros          AS CHARACTER NO-UNDO FORMAT 'X(100)'.
DEFINE VAR cError               AS CHARACTER NO-UNDO FORMAT 'X(100)'.
DEFINE VAR cNumDoc              AS CHARACTER NO-UNDO FORMAT 'X(20)'.


DEFINE VAR hCodbarra            AS COM-HANDLE NO-UNDO.
DEFINE VAR hFechaMov            AS COM-HANDLE NO-UNDO.
DEFINE VAR hCentro              AS COM-HANDLE NO-UNDO.
DEFINE VAR hAlmacenOrigen       AS COM-HANDLE NO-UNDO.
DEFINE VAR hAlmacenDestino      AS COM-HANDLE NO-UNDO.
DEFINE VAR hCantidad            AS COM-HANDLE NO-UNDO.
DEFINE VAR hUnidadMedida        AS COM-HANDLE NO-UNDO.
DEFINE VAR hOperacion           AS COM-HANDLE NO-UNDO.
DEFINE VAR hDocumen             AS COM-HANDLE NO-UNDO.
DEFINE VAR hPArametros          AS COM-HANDLE NO-UNDO.
DEFINE VAR hError               AS COM-HANDLE NO-UNDO.
DEFINE VAR hNumDoc              AS COM-HANDLE NO-UNDO.


    pOperacion = TRIM(pOperacion).
    
    cFechaMov     = STRING(YEAR(pFechaMov),"9999") + STRING(MONTH(pFechaMov),"99") + STRING(DAY(pFechaMov),"99").
    cCantidad     = TRIM(STRING(pCantidad, '>>>>>>>>>9.999')).
    cUnidadMedida = TRIM(pUnidadMedida).

    pParametros = pCodbarra + '|' +
              cFechamov + '|' +
              pCentro + '|' +
              pAlmacenorigen + '|' +
              pAlmacendestino + '|' +
              cCantidad + '|' +
              cUnidadmedida + '|' +
              pOperacion + '|' +
              pDocumen.

/*
    OUTPUT TO Z:\SISTEMAS10\TEMP\MOVINT.TXT.
    PUT  UNFORMATTED PPARAMETROS.
    OUTPUT CLOSE.
*/
    hFunctions = connectToSap().
    
    IF NOT VALID-HANDLE(hFunctions) THEN DO:
        message "NO SE CONECTO A LAS BASES SAP !!" view-as alert-box ERROR.
    END.
    
    hFunc  = hFunctions:ADD('BAPIMOVINTERNOS').

    hCodBarra       = hFunc:exports('I_COD_BARRA').
    hFechaMov       = hFunc:exports('I_FECHA_MOV').
    hCentro         = hFunc:exports('I_CENTRO').
    hAlmacenOrigen  = hFunc:exports('I_ALMACEN_ORIG').
    hAlmacenDestino = hFunc:exports('I_ALMACEN_DEST').
    hCantidad       = hFunc:exports('I_CANTIDAD').
    hUnidadMedida   = hFunc:exports('I_UM_BAS').
    hOperacion      = hFunc:exports('I_OPERACION').
    hDocumen        = hFunc:exports('I_DOCUM').
    hParametros     = hFunc:exports('I_PARAMETROS').

    hError          = hFunc:imports('ERROR').
    hNumDoc         = hFunc:imports('NUMDOC').

/*
    hCodBarra:VALUE       = pCodBarra.
    hFechaMov:VALUE       = cFechaMov.
    hCentro:VALUE         = pCentro.
    hAlmacenOrigen:VALUE  = pAlmacenOrigen.    
    hAlmacenDestino:VALUE = pAlmacenDestino. 
    hCantidad:VALUE       = cCantidad.
    hUnidadMedida:VALUE   = cUnidadMedida.    /* error */
    hOperacion:VALUE      = pOPeracion.    
    hDocumen:VALUE        = pDocumen.         /* error */
*/

   
    hParametros:VALUE     = pPArametros. 
    
    pError = "".
    IF hFunc:CALL() THEN
    DO:
        pError  = hError:VALUE().
        pNumDoc = hNumDoc:VALUE().
    END.
    ELSE
    DO:
        hFunctions:connection:logoff().
        RETURN ERROR "Error de interface".
    END.
    IF VALID-HANDLE(hFunctions) THEN hFunctions:connection:logoff().
    
    IF pError <> ''   OR pNumdoc = "" THEN RETURN ERROR 'Error en operacion de interfase de movimientos : ' + pError.

    RETURN 'OK'.

    FINALLY:
        IF VALID-HANDLE(hFunctions) THEN hFunctions:connection:logoff().
    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MovInternosPrueba) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MovInternosPrueba Procedure 
PROCEDURE MovInternosPrueba :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pCodbarra            AS CHARACTER NO-UNDO FORMAT 'X(30)'.
DEFINE INPUT PARAMETER pFechaMov            AS DATE NO-UNDO.
DEFINE INPUT PARAMETER pCentro              AS CHARACTER NO-UNDO FORMAT 'X(4)'.
DEFINE INPUT PARAMETER pAlmacenOrigen       AS CHARACTER NO-UNDO FORMAT 'X(4)'.
DEFINE INPUT PARAMETER pAlmacenDestino      AS CHARACTER NO-UNDO FORMAT 'X(4)'.
DEFINE INPUT PARAMETER pCantidad            AS DECIMAL DECIMALS 3. /*Decimal 3*/
DEFINE INPUT PARAMETER pUnidadMedida        AS CHARACTER FORMAT 'X(3)'.  /*Integer*/
DEFINE INPUT PARAMETER pOperacion           AS CHARACTER NO-UNDO FORMAT 'X(2)'.
DEFINE INPUT PARAMETER pDocumen             AS CHARACTER NO-UNDO FORMAT 'X(20)'.
DEFINE INPUT PARAMETER pPArametros          AS CHARACTER NO-UNDO FORMAT 'X(100)'.
DEFINE OUTPUT PARAMETER pError              AS CHARACTER NO-UNDO FORMAT 'X(100)'.
DEFINE OUTPUT PARAMETER pNumDoc             AS CHARACTER NO-UNDO FORMAT 'X(20)'.

DEFINE VARIABLE hFunctions      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hFunc           AS COM-HANDLE NO-UNDO.

DEFINE VAR cCodbarra            AS CHARACTER NO-UNDO FORMAT 'X(20)'.
DEFINE VAR cFechaMov            AS CHARACTER NO-UNDO FORMAT 'X(10)'.
DEFINE VAR cCentro              AS CHARACTER NO-UNDO FORMAT 'X(4)'.
DEFINE VAR cAlmacenOrigen       AS CHARACTER NO-UNDO FORMAT 'X(4)'.
DEFINE VAR cAlmacenDestino      AS CHARACTER NO-UNDO FORMAT 'X(4)'.
DEFINE VAR cCantidad            AS CHARACTER NO-UNDO FORMAT 'X(13)'. /*Decimal 3*/
DEFINE VAR cUnidadMedida        AS CHARACTER NO-UNDO FORMAT 'X(3)'.  /*Integer*/
DEFINE VAR cOperacion           AS CHARACTER NO-UNDO FORMAT 'X'.
DEFINE VAR cDocumen             AS CHARACTER NO-UNDO FORMAT 'X(20)'.
DEFINE VAR cPArametros          AS CHARACTER NO-UNDO FORMAT 'X(100)'.
DEFINE VAR cError               AS CHARACTER NO-UNDO FORMAT 'X(100)'.
DEFINE VAR cNumDoc              AS CHARACTER NO-UNDO FORMAT 'X(20)'.


DEFINE VAR hCodbarra            AS COM-HANDLE NO-UNDO.
DEFINE VAR hFechaMov            AS COM-HANDLE NO-UNDO.
DEFINE VAR hCentro              AS COM-HANDLE NO-UNDO.
DEFINE VAR hAlmacenOrigen       AS COM-HANDLE NO-UNDO.
DEFINE VAR hAlmacenDestino      AS COM-HANDLE NO-UNDO.
DEFINE VAR hCantidad            AS COM-HANDLE NO-UNDO.
DEFINE VAR hUnidadMedida        AS COM-HANDLE NO-UNDO.
DEFINE VAR hOperacion           AS COM-HANDLE NO-UNDO.
DEFINE VAR hDocumen             AS COM-HANDLE NO-UNDO.
DEFINE VAR hPArametros          AS COM-HANDLE NO-UNDO.
DEFINE VAR hError               AS COM-HANDLE NO-UNDO.
DEFINE VAR hNumDoc              AS COM-HANDLE NO-UNDO.

    pOperacion = TRIM(pOperacion).
    
    cFechaMov     = STRING(YEAR(pFechaMov),"9999") + STRING(MONTH(pFechaMov),"99") + STRING(DAY(pFechaMov),"99").
    cCantidad     = TRIM(STRING(pCantidad, '>>>>>>>>>9.999')).
    cUnidadMedida = TRIM(pUnidadMedida).

    pParametros = pCodbarra + '|' +
              cFechamov + '|' +
              pCentro + '|' +
              pAlmacenorigen + '|' +
              pAlmacendestino + '|' +
              cCantidad + '|' +
              cUnidadmedida + '|' +
              pOperacion + '|' +
              pDocumen.

    OUTPUT TO z:\temp\movin.pab.

    EXPORT pparametros.

    OUTPUT CLOSE.

/*

    hFunctions = connectToSap().
    
    IF NOT VALID-HANDLE(hFunctions) THEN DO:
        message "NO SE CONECTO A LAS BASES SAP !!" view-as alert-box ERROR.
    END.
    
    hFunc  = hFunctions:ADD('BAPIMOVINTERNOS').

    hCodBarra       = hFunc:exports('I_COD_BARRA').
    hFechaMov       = hFunc:exports('I_FECHA_MOV').
    hCentro         = hFunc:exports('I_CENTRO').
    hAlmacenOrigen  = hFunc:exports('I_ALMACEN_ORIG').
    hAlmacenDestino = hFunc:exports('I_ALMACEN_DEST').
    hCantidad       = hFunc:exports('I_CANTIDAD').
    hUnidadMedida   = hFunc:exports('I_UM_BAS').
    hOperacion      = hFunc:exports('I_OPERACION').
    hDocumen        = hFunc:exports('I_DOCUM').
    hParametros     = hFunc:exports('I_PARAMETROS').

    hError          = hFunc:imports('ERROR').
    hNumDoc         = hFunc:imports('NUMDOC').

    hCodBarra:VALUE       = pCodBarra.
    hFechaMov:VALUE       = cFechaMov.
    hCentro:VALUE         = pCentro.
    hAlmacenOrigen:VALUE  = pAlmacenOrigen.    
    hAlmacenDestino:VALUE = pAlmacenDestino. 
    hCantidad:VALUE       = cCantidad.
    hUnidadMedida:VALUE   = cUnidadMedida.    /* error */
    hOperacion:VALUE      = pOPeracion.    
    hDocumen:VALUE        = pDocumen.         /* error */
   
    hParametros:VALUE     = pPArametros. 
    



    hParametros:VALUE     = pPArametros. 


    IF NOT hFunc:CALL() THEN
            RETURN ERROR "Error en llamada de Interfase de movimientos".

    pError  = hError:VALUE().
    pNumDoc = hNumDoc:VALUE().

    hFunctions:connection:logoff().

    IF pError <> ''   THEN RETURN ERROR 'Error en operacion de interfase de movimientos : ' + pError.
*/


    RETURN 'OK'.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-notificacionPalletPrueba) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE notificacionPalletPrueba Procedure 
PROCEDURE notificacionPalletPrueba :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE DEBUG AS LOGICAL NO-UNDO.

DEFINE VAR        hTable     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunctions AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc      AS COM-HANDLE NO-UNDO.

DEFINE VAR hd1 AS COM-HANDLE NO-UNDO.
DEFINE VAR hd2 AS COM-HANDLE NO-UNDO.
DEFINE VAR hfila AS COM-HANDLE NO-UNDO.





hFunctions = connectToSapCalidad().

IF VALID-HANDLE(hFunctions) THEN DO:
  


  hFunc  = hFunctions:ADD('BAPI_NOTIFICAR_PALLET').
  hD1    = hFunc:exports('P_CABECERA').
  hD2    = hFunc:imports('O_RECCOUNT').
  hTable = hFunc:tables('P_POSICIONES').

  hD1:VALUE = "MARIANITO SOS TROLITO".
  

  HFILA = htable:APPENDROW().

  MESSAGE VALID-HANDLE(hfila) VIEW-AS ALERT-BOX.
  
 
  hfila:VALUE("PARAMETRO" ) = "1".
  htable:VALUE (1,1) = "11".



  IF hFunc:CALL() THEN DO: 
        MESSAGE   hD2:VALUE .
  END.      
  ELSE
      MESSAGE 'ERROR' VIEW-AS ALERT-BOX ERROR.

  hFunctions:connection:logoff().
  RELEASE OBJECT hFunctions.
END.
ELSE
    MESSAGE 'No se pudo conectar a Sap' VIEW-AS ALERT-BOX ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ordenCargaSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ordenCargaSap Procedure 
PROCEDURE ordenCargaSap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pbalanza AS INTEGER.
    DEFINE INPUT PARAMETER ppesada  AS INTEGER.
    DEFINE INPUT PARAMETER pticket  AS INTEGER.
    DEFINE OUTPUT PARAMETER pcode   AS CHARACTER FORMAT "x".
    DEFINE OUTPUT PARAMETER porden  AS CHARACTER FORMAT "x(10)".
    DEFINE OUTPUT PARAMETER pposic  AS CHARACTER FORMAT "x(10)".
    DEFINE OUTPUT PARAMETER perror  AS CHARACTER FORMAT "x(3)".
    DEFINE OUTPUT PARAMETER ptexto  AS CHARACTER FORMAT "x(100)".
    DEFINE OUTPUT PARAMETER ptipoe  AS CHARACTER FORMAT "x".
    
    
    DEFINE VARIABLE cparametros AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStatus AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    
    
    DEFINE VARIABLE hFunctions  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hFunc       AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hparametros AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hProveedor  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hTipoCorte  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hServicio   AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hArticulo   AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hFinca      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hCentro     AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hCode       AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hOrden      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hPosic      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hError      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hTexto      AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hTError     AS COM-HANDLE NO-UNDO.
    
    
    DEFINE VARIABLE xcorte      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE xservicio   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE xarticulo   AS CHARACTER NO-UNDO FORMAT 'x(18)'.
    DEFINE VARIABLE xCentro     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE xunidadMedida AS CHARACTER NO-UNDO.
    

    DEFINE BUFFER BT FOR BALANZA_TICKETS.

    
    FIND bt WHERE
        bt.id_balanza  = pbalanza  AND
        bt.id_pesada   = pPesada   AND
        bt.nro_ticket  = pticket   NO-LOCK NO-ERROR.
    
    IF NOT AVAILABLE bt THEN
    do:
     message "NO HAY BALANZA_TICKETS !!" VIEW-AS ALERT-BOX.
     RETURN ERROR.
    end.
    
    hFunctions = connectToSap().
    
    IF NOT VALID-HANDLE(hFunctions) THEN DO:
        RETURN ERROR "error de conecci¢n".
    END.
    
    hFunc  = hFunctions:ADD('BAPIVALIDAINGRESOS').
    /*
    hparametros     = hFunc:exports('LPARAMETROS').
    */
    hProveedor      = hFunc:exports('I_PROVEEDOR').
    hTipoCorte      = hFunc:exports('I_TIPO_CORTE').
    hServicio       = hFunc:exports('I_TIPO_SERVICIO').
    hArticulo       = hFunc:exports('I_MATERIAL').
    hFinca          = hFunc:exports('I_FINCA').
    hCentro         = hFunc:exports('I_CENTRO_RECEPCION').
    
    hCode           = hFunc:imports('OK_CODE').
    hOrden          = hFunc:imports('ORDEN_COMPRA').
    hPosic          = hFunc:imports('POSICION').
    hError          = hFunc:imports('COD_ERROR').
    hTexto          = hFunc:imports('TXT_ERROR').
    hTError         = hFunc:imports('TIPO_ERROR').
        
    
    FIND proveedores        OF bt NO-LOCK NO-ERROR.
    FIND tipo_cosecha       OF bt NO-LOCK NO-ERROR. 
    FIND tipos_servicios    OF bt NO-LOCK NO-ERROR.
    FIND origenes           OF bt NO-LOCK NO-ERROR.
    FIND balanzas           OF bt NO-LOCK NO-ERROR.

    FIND productos_terminados WHERE
        productos_terminados.id_articulo = bt.id_materia_prima NO-LOCK NO-ERROR.
    
    IF AVAILABLE tipo_cosecha THEN
        xcorte = tipo_cosecha.id_tipo_cosecha_sap.
    ELSE
        xcorte = '00'.
    
    IF AVAILABLE tipos_servicios THEN
        xservicio = tipos_servicios.id_tipo_servicio_sap.
    ELSE
        xservicio = '00'.
    
    IF AVAILABLE productos_terminados THEN
        CASE productos_terminados.id_articulo:
            WHEN 1 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 2 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 3 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 4 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 5 THEN 
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 6 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'KG'.
                END CASE.
            WHEN 7 THEN
                CASE tipo_cosecha.id_tipo_cosecha.
                    WHEN 4 THEN ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'KG'.
                    WHEN 1 THEN ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'BIN'.
                    OTHERWISE   ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'KG'.
                END CASE.
        END CASE.
    ELSE
        CASE tipo_cosecha.id_tipo_cosecha.
            WHEN 4 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
            WHEN 1 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'BIN'.
            OTHERWISE   ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
        END CASE.
    
    hProveedor:VALUE    = SUBSTRING(STRING(proveedores.cuit),1,10).
    hTipoCorte:VALUE    = xcorte.
    hServicio:VALUE     = xservicio.
    hArticulo:VALUE     = xarticulo.
    hFinca:VALUE        = UPPER(SUBSTRING(origenes.id_origen_sap,5,4)).
    hCentro:VALUE       = SUBSTRING(balanzas.id_balanza_sap,1,4).

    porden = "".
    IF hFunc:CALL() THEN
    DO:
        pcode   = hCode:VALUE().
        porden  = hOrden:VALUE().
        pposic  = hPosic:VALUE().
        perror  = hError:VALUE().
        ptexto  = hTexto:VALUE().
        ptipoe  = hTError:VALUE().

    END.
    ELSE
        RETURN 'ERROR VALIDACION EN SAP '.
    hFunctions:connection:logoff().
    RETURN.
    FINALLY:
        IF VALID-HANDLE(hFunctions) THEN hFunctions:connection:logoff().

    END FINALLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-palletLeidoToSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE palletLeidoToSap Procedure 
PROCEDURE palletLeidoToSap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iSucTrabajo  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iPAllet      AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iLector      AS INTEGER NO-UNDO.



DEFINE VARIABLE   hFunctions        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc             AS COM-HANDLE NO-UNDO.

DEFINE VAR hParametro               AS COM-HANDLE NO-UNDO.
DEFINE VAR cParametro               AS CHARACTER NO-UNDO.
DEFINE VARIABLE hReturn             AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE xlote               AS CHARACTER NO-UNDO.




hFunctions = connectToSap().

IF VALID-HANDLE(hFunctions) THEN
DO TRANSACTION ON ERROR UNDO , RETURN ERROR:


    hFunc               = hFunctions:ADD('BAPI_ZTSD_LECTURA_PAL').
    hparametro          = hFunc:exports('LPARAMETROS').
    hReturn             = hFUnc:ImportS('O_RETURN').

    FIND pallets WHERE pallets.id_suc_trabajo = ISucTrabajo AND
                       pallets.id_pallet = iPallet NO-LOCK NO-ERROR.

    IF AVAILABLE pallets  AND VALID-HANDLE(hFunc) THEN DO ON ERROR UNDO , RETURN ERROR:
        FIND items_pedidos_packing OF pallets NO-LOCK NO-ERROR.
        FIND pedidos_packing OF items_pedidos_packing NO-LOCK NO-ERROR.
        FIND lote OF pallets NO-LOCK NO-ERROR.

        cParametro = STRING(iLector,'99') + '|' + items_pedidos_packing.material_sap + '|' + string(pallets.id_pallet_sap) + '|001|PAL|' + 
                     string(pedidos_packing.oe) + '|' + string(items_pedidos_packing.posicion_oe,'999999') + '|' +
                     pedidos_packing.pedido_traslado_sap + '|' + string(pedidos_packing.posicion_pedido_sap,'999999') + '| |' + string(pallets.pallet_senasa).
        hParametro:VALUE = cparametro.
        IF hFunc:CALL() THEN DO:
            hFunctions:connection:logoff().
            RETURN.
        END.
        ELSE DO:
            hFunctions:connection:logoff().
            UNDO , RETURN ERROR.
        END.
    END.
END.

hFunctions:connection:logoff().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-palletLeidoToSapCalidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE palletLeidoToSapCalidad Procedure 
PROCEDURE palletLeidoToSapCalidad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iSucTrabajo  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iPAllet      AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iLector      AS INTEGER NO-UNDO.



DEFINE VARIABLE   hFunctions        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc             AS COM-HANDLE NO-UNDO.

DEFINE VAR hParametro               AS COM-HANDLE NO-UNDO.
DEFINE VAR cParametro               AS CHARACTER NO-UNDO.
DEFINE VARIABLE hReturn             AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE xlote               AS CHARACTER NO-UNDO.




hFunctions = connectToSapCalidad().

IF VALID-HANDLE(hFunctions) THEN
DO TRANSACTION ON ERROR UNDO , RETURN ERROR:


    hFunc               = hFunctions:ADD('BAPI_ZTSD_LECTURA_PAL').
    hparametro          = hFunc:exports('LPARAMETROS').
    hReturn             = hFUnc:ImportS('O_RETURN').

    FIND pallets WHERE pallets.id_suc_trabajo = ISucTrabajo AND
                       pallets.id_pallet = iPallet NO-LOCK NO-ERROR.

    IF AVAILABLE pallets  AND VALID-HANDLE(hFunc) THEN DO ON ERROR UNDO , RETURN ERROR:
        FIND items_pedidos_packing OF pallets NO-LOCK NO-ERROR.
        FIND pedidos_packing OF items_pedidos_packing NO-LOCK NO-ERROR.
        FIND lote OF pallets NO-LOCK NO-ERROR.

        cParametro = STRING(iLector,'99') + '|' + items_pedidos_packing.material_sap + '|' + string(pallets.id_pallet_sap) + '|001|PAL|' + 
                     string(pedidos_packing.oe) + '|' + string(items_pedidos_packing.posicion_oe,'999999') + '|' +
                     pedidos_packing.pedido_traslado_sap + '|' + string(pedidos_packing.posicion_pedido_sap,'999999') + '| |' + string(pallets.pallet_senasa).
        hParametro:VALUE = cparametro.
        IF hFunc:CALL() THEN DO:
            hFunctions:connection:logoff().
            RETURN.
        END.
        ELSE DO:
            hFunctions:connection:logoff().
            UNDO , RETURN ERROR.
        END.
    END.
END.

hFunctions:connection:logoff().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-palletLeidoToSapPrueba) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE palletLeidoToSapPrueba Procedure 
PROCEDURE palletLeidoToSapPrueba :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iSucTrabajo  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iPAllet      AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iLector      AS INTEGER NO-UNDO.



DEFINE VARIABLE   hFunctions        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc             AS COM-HANDLE NO-UNDO.

DEFINE VAR hParametro               AS COM-HANDLE NO-UNDO.
DEFINE VAR cParametro               AS CHARACTER NO-UNDO.
DEFINE VARIABLE hReturn             AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE xlote               AS CHARACTER NO-UNDO.




hFunctions = connectToSapCalidad().

IF VALID-HANDLE(hFunctions) THEN
DO TRANSACTION ON ERROR UNDO , RETURN ERROR:


    hFunc               = hFunctions:ADD('BAPI_ZTSD_LECTURA_PAL').
    hparametro          = hFunc:exports('LPARAMETROS').
    hReturn             = hFUnc:ImportS('O_RETURN').

    FIND pallets WHERE pallets.id_suc_trabajo = ISucTrabajo AND
                       pallets.id_pallet = iPallet NO-LOCK NO-ERROR.

    IF AVAILABLE pallets  AND VALID-HANDLE(hFunc) THEN DO ON ERROR UNDO , RETURN ERROR:
        FIND items_pedidos_packing OF pallets NO-LOCK NO-ERROR.
        FIND pedidos_packing OF items_pedidos_packing NO-LOCK NO-ERROR.
        FIND lote OF pallets NO-LOCK NO-ERROR.

        cParametro = STRING(iLector,'99') + '|' + items_pedidos_packing.material_sap + '|' + string(pallets.id_pallet_sap) + '|001|PAL|' + 
                     string(pedidos_packing.oe) + '|' + string(items_pedidos_packing.posicion_oe,'999999') + '|' +
                     pedidos_packing.pedido_traslado_sap + '|' + string(pedidos_packing.posicion_pedido_sap,'999999') + '| |' + string(pallets.pallet_senasa).
        hParametro:VALUE = cparametro.
        IF hFunc:CALL() THEN DO:
            hFunctions:connection:logoff().
            RETURN.
        END.
        ELSE DO:
            hFunctions:connection:logoff().
            UNDO , RETURN ERROR.
        END.
    END.
END.

hFunctions:connection:logoff().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-palletsToSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE palletsToSap Procedure 
PROCEDURE palletsToSap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iSucTrabajo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER iPallet AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER cStatus AS CHARACTER.

  
  
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.

  DEFINE VARIABLE   hDocu       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE   hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRow        AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRows       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hparametros AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR iCodigo AS INTEGER NO-UNDO.
  DEFINE VAR cCentro AS CHARACTER NO-UNDO.





  DEFINE VARIABLE inum AS INTEGER NO-UNDO.
  DEFINE VARIABLE cestado AS CHARACTER NO-UNDO.

  DEFINE VAR par AS INTEGER INITIAL 0.

  DEFINE VARIABLE   cFecha          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cPallet         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cParametros     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   hPalletSap      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   xcentro         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   xalmacen        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   iEstado         AS INTEGER NO-UNDO.



  DO:
      hFunctions = connectToSap().
    
      IF VALID-HANDLE(hFunctions) THEN DO:
          
          hFunc         = hFunctions:ADD('BAPI_PALLET_CREATE').
          
          hparametros   = hFunc:exports('LPARAMETROS').
          hPalletSap    = hFUnc:ImportS('O_PALLET').
    
    
          FIND pallets WHERE pallets.id_suc_trabajo = ISucTrabajo AND
                             pallets.id_pallet = iPallet NO-ERROR.
    
          IF AVAILABLE pallets  THEN DO ON ERROR UNDO , RETURN ERROR cstatus:
              
              FIND items_pedidos_packing OF pallets NO-LOCK NO-ERROR.
              FIND pedidos_packing OF items_pedidos_packing NO-LOCK NO-ERROR.
    
              IF NOT AVAILABLE pedidos_packing OR NOT AVAILABLE items_pedidos_packing  THEN DO:
                  cstatus = 'Error en los pedidos'.
                  hFunctions:connection:logoff().
                  UNDO , RETURN ERROR cstatus.
              END.
    
    
              cPallet = string(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet , '99999999999').
              cfecha = string(YEAR(TODAY),'9999') + string(MONTH(TODAY),'99') + STRING(DAY(TODAY),'99').
    
    
              cParametros =  cPallet + '|' + STRING(pedidos_packing.oe,'9999999999') + '|' + STRING(items_pedidos_packing.posicion_oe,'999999') + 
                            '|' +  STRING(pedidos_packing.Of_sap,'999999999999') + '|' + STRING(items_pedidos_packing.material_sap,'X(18)') + '|xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|'.
    
    /*          cParametros =  cPallet + '|' + STRING(11773,'9999999999') + '|' + STRING(100,'999999') + 
                            '|' +  STRING(1002401,'999999999999') + '|' + STRING('L01-001-005-072','X(18)') + '|xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|'.
    */         
                                                                                                                                                                                           
              FIND FIRST clientes_ventas WHERE clientes_ventas.id_cliente = pedidos_packing.id_cliente NO-LOCK NO-ERROR.
             
              IF AVAILABLE clientes_ventas THEN
                 IF clientes_ventas.mercado = 1 THEN cparametros = cparametros + 'EXTERNO|'.
                 ELSE cparametros = cparametros + 'INTERNO|'.
                 ELSE cparametros = cparametros + 'EXTERNO|'. /*MERCADO*/
    
              IF pedidos_packing.UNION_europea THEN  cparametros = cparametros + 'x|'. ELSE cparametros = cparametros + '|'.
              IF pedidos_packing.china THEN cparametros = cparametros + 'x|'. ELSE cparametros = cparametros + '|'.
    
              cparametros = cparametros  + cfecha + '|' + cfecha + '|'.
    
              
              FIND clientes_ventas WHERE clientes_ventas.id_cliente = pedidos_packing.id_cliente_remito NO-LOCK NO-ERROR.
    
              IF AVAILABLE clientes_ventas THEN cParametros = cparametros +  string(clientes_ventas.id_cliente_sap).
                                           ELSE cParametros = cparametros + '9999999999'.
              cParametros = cparametros + '|' .
    
              IF AVAILABLE clientes_ventas THEN cParametros = cparametros + clientes_ventas.razon_social.
                                           ELSE cParametros = cparametros + 'SIN CLIENTE'. 
              cParametros = cparametros +  '|'.
    
    
              FIND clientes_ventas WHERE clientes_ventas.id_cliente = pedidos_packing.id_cliente NO-LOCK NO-ERROR.
              
    
               
              IF AVAILABLE clientes_ventas THEN cParametros = cparametros + string(clientes_ventas.id_cliente_sap).
                                           ELSE cParametros = cparametros + '9999999999'.
    
              cparametros = cparametros  + '|'.
              
              IF AVAILABLE clientes_ventas THEN cParametros = cparametros + clientes_ventas.razon_social.
                                           ELSE cParametros = cparametros + 'SIN CLIENTE'.
              
              
              FIND vapores WHERE vapores.id_vapor = pedidos_packing.id_vapor NO-LOCK NO-ERROR.
              
              cparametros = cparametros + '|' + STRING(vapores.id_vapor_sap) + '|' + vapores.descripcion + '|'.
    
    
              FIND lugar_descarga WHERE lugar_descarga.id_lugdes = pedidos_packing.id_puerto_sal NO-LOCK NO-ERROR.
    
              IF AVAILABLE lugar_descarga THEN cParametros = cparametros + string(lugar_descarga.id_lugdes_sap).
                                   ELSE cParametros = cparametros + '9999'.
              
              cParametros = cparametros +  '|' .
    
              IF AVAILABLE lugar_descarga THEN cParametros = cparametros + lugar_descarga.descripcion.
                                   ELSE cParametros = cparametros + 'SIN PUERTO'.
              cParametros = cparametros +  '|'  .
    
              
              FIND destinos WHERE destinos.id_destino = pedidos_packing.id_destino_final NO-LOCK NO-ERROR.
              
              IF AVAILABLE destinos THEN cParametros = cparametros + destinos.id_destino_sap.
                                    ELSE cParametros = cparametros + '999'. 
              cParametros = cparametros + '|' .
    
              IF AVAILABLE destinos THEN cParametros = cparametros + destinos.descripcion.
                                    ELSE cParametros = cparametros + 'SIN DESTINO'.
              cParametros = cparametros  + '|' . 
                            
              
              FIND destinos WHERE destinos.id_destino = pedidos_packing.id_puerto_ent NO-LOCK NO-ERROR.
    
              IF AVAILABLE destinos THEN cParametros = cparametros + string(destinos.id_destino_sap).
                                    ELSE cParametros = cparametros + '9999'.
    
              cParametros = cparametros +  '|' .
    
              IF AVAILABLE destinos THEN cParametros = cparametros + destinos.descripcion.
                                    ELSE cParametros = cparametros + 'SIN PUERTO'.
              cParametros = cparametros +  '|'  .
    
              FIND tipo_pallets OF pallets NO-LOCK NO-ERROR.
              FIND tipo_esquineros OF pallets NO-LOCK NO-ERROR.
              FIND calidades OF pallets NO-LOCK NO-ERROR.
    
              FIND categorias_packing OF pallets NO-LOCK NO-ERROR.
              FIND caracteristicas OF pallets NO-LOCK NO-ERROR.
    
              IF pedidos_packing.OF_sap = '' THEN DO:
                  CASE pallets.id_suc_trabajo.
                      WHEN 98   THEN    ASSIGN xcentro = 'A100' xalmacen = '1010'. /**** OJO *****/
                      WHEN 97   THEN    ASSIGN xcentro = 'A200' xalmacen = '2010'.
                      OTHERWISE         ASSIGN xcentro = 'A700'.
                  END CASE.
              END.
              
              cParametros = cparametros + pallets.contramarca + '|' + string(tipo_pallets.id_tipo_pallet_sap) + '|' +
                            string(tipo_esquineros.id_tipo_esquinero_sap) + '|' +  string(calidades.id_calidad_sap) + '|' +
                            string(categorias_packing.id_categoria_sap)  + '|' + string(caracteristicas.id_caract_sap) + '|' + STRING(pallets.peso) + '||' +
                            STRING(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet,'9999999999') + '||||' + xcentro + '|' + xalmacen + '|'.
    
              hParametros:VALUE = cparametros .
             
              /*
              OUTPUT TO Z:\TEMP\PAR.PAB.
              EXPORT HPARAMETROS:VALUE.
              OUTPUT CLOSE.
              */
    
              ETIME(TRUE).
              REPEAT:
                  IF ETIME >= 500 THEN LEAVE.
              END.
              IF hFunc:CALL() THEN  DO:
    
                 pallets.id_pallet_sap = STRING(hPalletSap:VALUE). 
                 IF STRING(hPalletSap:VALUE) = '1' THEN  DO:
                     hFunctions:connection:logoff().
                     cStatus = 'Error en loteo de pallet '.
                     UNDO , RETURN ERROR cstatus.
                 END.
    /*             RUN itemsPalletsToSapPrueba ( pallets.id_suc_trabajo , pallets.id_pallet , string(hPalletSap:VALUE ), INPUT hFunctions , OUTPUT iNum , OUTPUT cEstado) NO-ERROR. */
                 RUN itemsPalletsToSap ( pallets.id_suc_trabajo , pallets.id_pallet , string(hPalletSap:VALUE ), INPUT hFunctions , OUTPUT iNum , OUTPUT cEstado) NO-ERROR.
                 IF ERROR-STATUS:ERROR THEN  DO:
                     cStatus = 'Error interface de items ' + cEstado.
                     hFunctions:connection:logoff().
                     UNDO, RETURN ERROR cstatus. 
                 END.
                 cStatus = 'OK'.
              END. 
              ELSE DO: 
                  cStatus = 'Error interface de pallet'. 
                  hFunctions:connection:logoff().
                  UNDO , RETURN ERROR cstatus. 
              END.    
              /*
              DISPLAY "Declarando Pallet Sap" pallets.id_pallet WITH FRAME informe CENTERED OVERLAY THREE-D.
              RUN procesarpallet(pallets.id_pallet_sap) NO-ERROR.   
              HIDE FRAME informe. */
              hFunctions:connection:logoff().
          END.
          ELSE DO: 
              cStatus = 'pallet inexistente'. 
              hFunctions:connection:logoff().
              UNDO, RETURN ERROR cstatus. 
          END.
      END.
      ELSE DO: 
          cstatus = 'error de coneccion SAP'. 
          UNDO , RETURN ERROR cstatus. 
      END.
  
  END.
  FINALLY:
    IF VALID-HANDLE(hFunctions) THEN hfunctions:connection:logoff().
  END FINALLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-palletsToSapNuevo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE palletsToSapNuevo Procedure 
PROCEDURE palletsToSapNuevo :
/*----------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER iSucTrabajo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER iPallet AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER cStatus AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER oPallet AS CHARACTER NO-UNDO.

  
  
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.

  DEFINE VARIABLE   hDocu       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE   hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRow        AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRows       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hparametros AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR iCodigo AS INTEGER NO-UNDO.
  DEFINE VAR cCentro AS CHARACTER NO-UNDO.





  DEFINE VARIABLE inum AS INTEGER NO-UNDO.
  DEFINE VARIABLE cestado AS CHARACTER NO-UNDO.
  DEFINE VARIABLE K AS INTEGER NO-UNDO.

  DEFINE VAR par AS INTEGER INITIAL 0.

  DEFINE VARIABLE   cFecha          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cPallet         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   hCabecera       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion01     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion02     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion03     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion04     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion05     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion06     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion07     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion08     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion09     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion10     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion11     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion12     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hReccount       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hLote           AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hStatus         AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   xcentro         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   xalmacen        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   iEstado         AS INTEGER NO-UNDO.
  DEFINE VARIABLE   cCabecera       AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE   cItem           AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   ibultos         AS INTEGER NO-UNDO.
  DEFINE VARIABLE   iTraz           AS INTEGER NO-UNDO.
  DEFINE VARIABLE   iItem           AS INTEGER NO-UNDO.

  DEFINE VARIABLE   igln            AS INT64   NO-UNDO.

  DEFINE BUFFER bitems FOR items_pallets.



  hFunctions = connectToSap().


  cstatus = "".
  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc         = hFunctions:ADD('BAPI_NOTIFICAR_PALLET').
      
      hCabecera      = hFunc:exports('P_CABECERA').
      hPosicion01    = hFUnc:EXPORTS('P_POSICION_01').
      hPosicion02    = hFUnc:EXPORTS('P_POSICION_02').
      hPosicion03    = hFUnc:EXPORTS('P_POSICION_03').
      hPosicion04    = hFUnc:EXPORTS('P_POSICION_04').
      hPosicion05    = hFUnc:EXPORTS('P_POSICION_05').
      hPosicion06    = hFUnc:EXPORTS('P_POSICION_06').
      hPosicion07    = hFUnc:EXPORTS('P_POSICION_07').
      hPosicion08    = hFUnc:EXPORTS('P_POSICION_08').
      hPosicion09    = hFUnc:EXPORTS('P_POSICION_09').
      hPosicion10    = hFUnc:EXPORTS('P_POSICION_10').
      hPosicion11    = hFUnc:EXPORTS('P_POSICION_11').
      hPosicion12    = hFUnc:EXPORTS('P_POSICION_12').
      hReccount      = hFUnc:ImportS('O_RECCOUNT').
      hLote          = hFUnc:ImportS('O_LOTE').
      hStatus        = hFUnc:imports('O_STATUS').


      FIND pallets WHERE pallets.id_suc_trabajo = ISucTrabajo AND
                         pallets.id_pallet = iPallet NO-LOCK NO-ERROR.

      IF AVAILABLE pallets  THEN DO ON ERROR UNDO , RETURN ERROR "Error en proceso de pallets":
          
          FIND items_pedidos_packing OF pallets NO-LOCK NO-ERROR.
          FIND pedidos_packing OF items_pedidos_packing NO-LOCK NO-ERROR.

          IF NOT AVAILABLE pedidos_packing OR NOT AVAILABLE items_pedidos_packing  THEN DO:
              hFunctions:connection:logoff().
              UNDO , RETURN ERROR 'Error en los pedidos'.
          END.


          cPallet = string(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet , '99999999999').
          cfecha = string(YEAR(TODAY),'9999') + string(MONTH(TODAY),'99') + STRING(DAY(TODAY),'99').

          cCabecera =  cPallet + '|' + STRING(pedidos_packing.oe,'9999999999') + '|' + STRING(items_pedidos_packing.posicion_oe,'999999') + 
                        '|' +  STRING(pedidos_packing.Of_sap,'999999999999') + '|' + STRING(items_pedidos_packing.material_sap,'X(18)') + '|xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|'.
         /*
          cCabecera =  cPallet + '|' + STRING(11773,'9999999999') + '|' + STRING(100,'999999') + 
                        '|' +  STRING(1002401,'999999999999') + '|' + STRING('L01-001-005-072','X(18)') + '|xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|'.
         */
         
          FIND FIRST clientes_ventas WHERE clientes_ventas.id_cliente = pedidos_packing.id_cliente NO-LOCK NO-ERROR.
         
          IF AVAILABLE clientes_ventas THEN
             IF clientes_ventas.mercado = 1 THEN cCabecera = cCabecera + 'EXTERNO|'.
             ELSE cCabecera = cCabecera + 'INTERNO|'.
             ELSE cCabecera = cCabecera + 'EXTERNO|'. /*MERCADO*/

          IF pedidos_packing.UNION_europea THEN  cCabecera = cCabecera + 'x|'. ELSE cCabecera = cCabecera + '|'.
          IF pedidos_packing.china THEN cCabecera = cCabecera + 'x|'. ELSE cCabecera = cCabecera + '|'.

          cCabecera = cCabecera  + cfecha + '|' + cfecha + '|'.

          
          FIND clientes_ventas WHERE clientes_ventas.id_cliente = pedidos_packing.id_cliente_remito NO-LOCK NO-ERROR.

          IF AVAILABLE clientes_ventas THEN cCabecera = cCabecera +  string(clientes_ventas.id_cliente_sap).
                                       ELSE cCabecera = cCabecera + '9999999999'.
          cCabecera = cCabecera + '|' .

          IF AVAILABLE clientes_ventas THEN cCabecera = cCabecera + clientes_ventas.razon_social.
                                       ELSE cCabecera = cCabecera + 'SIN CLIENTE'. 
          cCabecera = cCabecera +  '|'.


          FIND clientes_ventas WHERE clientes_ventas.id_cliente = pedidos_packing.id_cliente NO-LOCK NO-ERROR.
          

           
          IF AVAILABLE clientes_ventas THEN cCabecera = cCabecera + string(clientes_ventas.id_cliente_sap).
                                       ELSE cCabecera = cCabecera + '9999999999'.

          cCabecera = cCabecera  + '|'.
          
          IF AVAILABLE clientes_ventas THEN cCabecera = cCabecera + clientes_ventas.razon_social.
                                       ELSE cCabecera = cCabecera + 'SIN CLIENTE'.
          
          
          FIND vapores WHERE vapores.id_vapor = pedidos_packing.id_vapor NO-LOCK NO-ERROR.
          
          cCabecera = cCabecera + '|' + STRING(vapores.id_vapor_sap) + '|' + vapores.descripcion + '|'.


          FIND lugar_descarga WHERE lugar_descarga.id_lugdes = pedidos_packing.id_puerto_sal NO-LOCK NO-ERROR.

          IF AVAILABLE lugar_descarga THEN cCabecera = cCabecera + string(lugar_descarga.id_lugdes_sap).
                               ELSE cCabecera = cCabecera + '9999'.
          
          cCabecera = cCabecera +  '|' .

          IF AVAILABLE lugar_descarga THEN cCabecera = cCabecera + lugar_descarga.descripcion.
                               ELSE cCabecera = cCabecera + 'SIN PUERTO'.
          cCabecera = cCabecera +  '|'  .

          
          FIND destinos WHERE destinos.id_destino = pedidos_packing.id_destino_final NO-LOCK NO-ERROR.
          
          IF AVAILABLE destinos THEN cCabecera = cCabecera + destinos.id_destino_sap.
                                ELSE cCabecera = cCabecera + '999'. 
          cCabecera = cCabecera + '|' .

          IF AVAILABLE destinos THEN cCabecera = cCabecera + destinos.descripcion.
                                ELSE cCabecera = cCabecera + 'SIN DESTINO'.
          cCabecera = cCabecera  + '|' . 
                        
          
          FIND destinos WHERE destinos.id_destino = pedidos_packing.id_puerto_ent NO-LOCK NO-ERROR.

          IF AVAILABLE destinos THEN cCabecera = cCabecera + string(destinos.id_destino_sap).
                                ELSE cCabecera = cCabecera + '9999'.

          cCabecera = cCabecera +  '|' .

          IF AVAILABLE destinos THEN cCabecera = cCabecera + destinos.descripcion.
                                ELSE cCabecera = cCabecera + 'SIN PUERTO'.
          cCabecera = cCabecera +  '|'  .

          FIND tipo_pallets OF pallets NO-LOCK NO-ERROR.
          FIND tipo_esquineros OF pallets NO-LOCK NO-ERROR.
          FIND calidades OF pallets NO-LOCK NO-ERROR.

          FIND categorias_packing OF pallets NO-LOCK NO-ERROR.
          FIND caracteristicas OF pallets NO-LOCK NO-ERROR.

          IF pedidos_packing.OF_sap = '' THEN DO:
              CASE pallets.id_suc_trabajo.
                  WHEN 98   THEN    ASSIGN xcentro = 'A100' xalmacen = '1010'. /**** OJO *****/
                  WHEN 97   THEN    ASSIGN xcentro = 'A200' xalmacen = '2010'.
                  OTHERWISE         ASSIGN xcentro = 'A700'.
              END CASE.
          END.
          
          cCabecera = cCabecera + pallets.contramarca + '|' + string(tipo_pallets.id_tipo_pallet_sap) + '|' +
                        string(tipo_esquineros.id_tipo_esquinero_sap) + '|' +  string(calidades.id_calidad_sap) + '|' +
                        string(categorias_packing.id_categoria_sap)  + '|' + string(caracteristicas.id_caract_sap) + '|' + STRING(pallets.peso) + '||' +
                        STRING(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet,'9999999999') + '|||||' + xcentro + '|' + xalmacen + '|'.


          igln = pallets.gln.
          ccabecera = ccabecera + STRING(igln, '9999999999999') + '|'.

          IF pallets.testigo THEN DO:
              FOR EACH items_pallets OF pallets NO-LOCK:
                  IF items_pallets.con_testigo THEN LEAVE.
              END.
              ccabecera = ccabecera + 'X|' + items_pallets.codigo_trazabilidad.
          END.
          ELSE DO:   
              ccabecera = ccabecera + ' |       '.
          END.

          hCabecera:VALUE = cCabecera .



          inum  = 0.
          iTraz = 0.
          
          FOR EACH items_pallets OF pallets NO-LOCK
              BREAK
              BY items_pallets.calibre 
              BY items_pallets.codigo_trazabilidad
              BY items_pallets.bultos DESC:

              inum = inum + items_pallets.bultos.
              ibultos = iBultos + items_pallets.bultos.
              IF LAST-OF(items_pallets.codigo_trazabilidad) THEN DO:
                  IF inum > iTraz THEN DO:
                      iItem = items_pallets.ITEM_pallet.
                      iTraz = inum.
                  END.
                  inum = 0.
              END.

              IF LAST-OF(items_pallets.calibre) THEN DO:
                  IF iItem > 0 THEN DO:
                      FIND bitems WHERE
                          bitems.id_suc_trabajo = pallets.id_suc_trabajo AND
                          bitems.id_pallet      = pallets.id_pallet AND
                          bitems.ITEM_pallet    = iItem NO-LOCK NO-ERROR.
                      
                      CREATE ttitems.
                      BUFFER-COPY bitems TO ttitems.
                      ASSIGN ttitems.bultos = iBultos.
                  END.

                  iTraz = 0.
                  iBultos = 0.
                  iItem = 0.      
              END.
          END.

          K = 0.
          FOR EACH ttitems OF pallets NO-LOCK.
              K = K + 1.
          END.

          IF K > 12  THEN DO:
              hfunctions:connection:logoff().
              RETURN ERROR "No se puede generar pallet en Sap. El pallet tiene mas de 12 posiciones".
          END.

          inum = 0.
          FOR EACH ttitems OF pallets NO-LOCK.

              inum = inum + 1.

              FIND fechas_produccion WHERE fechas_produccion.fecha = pallets.fecha_prod NO-LOCK NO-ERROR.
              FIND tipos_procesos WHERE tipos_procesos.id_tipo_proceso = TRIM(pallets.tipo_proceso) NO-LOCK NO-ERROR.
              FIND packing OF pallets NO-LOCK NO-ERROR.
              FIND colores OF ttitems NO-LOCK NO-ERROR.
              FIND proveedores OF ttitems NO-LOCK NO-ERROR.
              FIND origenes OF ttitems NO-LOCK NO-ERROR.
              FIND lote OF ttitems NO-LOCK NO-ERROR.


              cItem =  '|' + string(ttitems.ITEM_pallet,'999999') + '|' .
              IF AVAILABLE fechas_produccion  THEN cItem = cItem + fechas_produccion.fecha_senasa. 
                                              ELSE cItem = cItem  + 'SF'.

              cItem = cItem + '|'.


              IF AVAILABLE tipos_procesos    THEN cItem = cItem + tipos_procesos.id_tipo_proceso. 
                                             ELSE cItem = cItem  + 'SIN PROCESO'.

              cItem = cItem + '|'.


              IF AVAILABLE colores          THEN cItem = cItem + string(colores.id_color_sap). 
                                            ELSE cItem = cItem  + 'SIN COLOR'.

             cItem = cItem + '|'.

             IF pallets.testigo THEN  cItem = cItem + 'X'.
                                ELSE  cItem = cItem + ' '.

             cItem = cItem + '|'.
              
              cItem = cItem + string(ttitems.id_turno_packing) + '|' + ttitems.codigo_trazabilidad + '|' +
                            SUBstring( STRING(ttitems.calibre),1,LENGTH(STRING(ttitems.calibre)) - 1 ) + '|' + string(ttitems.bultos) + '|0|'.

              IF AVAILABLE proveedores THEN  cItem = cItem + SUBSTRING(STRING(proveedores.cuit),1,10).
                                       ELSE  cItem = cItem + '9999999999'.

              cItem = cItem + '|' .

              IF AVAILABLE origenes THEN cItem = cItem + substring(origenes.id_origen_sap,5,4).
                                    ELSE cItem = cItem + '9999'.

              cItem = cItem + '|' + lote.descripcion + '|'.
              
              IF  pallets.union_europea THEN  cItem = cItem + 'X'. ELSE cItem = cItem + ' '.
              cItem = cItem + '|' .
              IF  pallets.china THEN  cItem = cItem + 'X'. ELSE cItem = cItem + ' '.
              cItem = cItem + '|' .

              IF AVAILABLE origenes THEN cItem = cItem + string(origenes.id_finca_senasa,'9999').
                                    ELSE cItem = cItem + '9999'.

                                         cItem = cItem + '|' .
              IF AVAILABLE lote     THEN cItem = cItem + string(lote.id_lote_senasa,'999').
                                    ELSE cItem = cItem + '999'.

                                         cItem = cItem + '|' .
              IF AVAILABLE lote     THEN cItem = cItem + lote.certificado.
                                    ELSE cItem = cItem + ''.
              
                                         cItem = cItem + '|' .
              IF AVAILABLE lote     THEN cItem = cItem + lote.cert_china.
                                    ELSE cItem = cItem + ''.
                                         cItem = cItem + '|' .
              cItem = cItem + string(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet , '99999999999'). 
              CASE inum:
                  WHEN 1  THEN   hPosicion01:VALUE = cItem.
                  WHEN 2  THEN   hPosicion02:VALUE = cItem.
                  WHEN 3  THEN   hPosicion03:VALUE = cItem.
                  WHEN 4  THEN   hPosicion04:VALUE = cItem.
                  WHEN 5  THEN   hPosicion05:VALUE = cItem.
                  WHEN 6  THEN   hPosicion06:VALUE = cItem.
                  WHEN 7  THEN   hPosicion07:VALUE = cItem.
                  WHEN 8  THEN   hPosicion08:VALUE = cItem.
                  WHEN 9  THEN   hPosicion09:VALUE = cItem.
                  WHEN 10 THEN   hPosicion10:VALUE = cItem.
                  WHEN 11 THEN   hPosicion11:VALUE = cItem.
                  WHEN 12 THEN   hPosicion12:VALUE = cItem.
                  OTHERWISE DO:                          
                      hFunctions:connection:logoff().
                      UNDO, RETURN ERROR "Error por mas de 12 ¡tems".
                  END.
              END CASE.
          END.
          cStatus = "".
          IF hFunc:CALL() THEN  
          DO:
             IF trim(string(hStatus:value)) <> "T" THEN DO:
                 hfunctions:connection:logoff().
                 UNDO, RETURN ERROR "Error de estado diferente a T".
             END.

             IF inum <> INTEGER(hReccount:VALUE) THEN DO:
                 hFunctions:connection:logoff().
                 UNDO , RETURN ERROR "La cantidad de registros no coincide".
             END.
             opallet = STRING(hLote:VALUE).
             cstatus = STRING(hStatus:VALUE).
          END. 
          ELSE DO: 
              hFunctions:connection:logoff().
              UNDO , RETURN ERROR 'Error interface de pallet'. 
          END.    
          hFunctions:connection:logoff().
      
          FOR EACH ttitems OF pallets.
              DELETE ttitems.
          END.
      END.
      ELSE DO: 
          hFunctions:connection:logoff().
          UNDO, RETURN ERROR 'pallet inexistente'. 
      END.
  END.
  ELSE DO: 
      UNDO , RETURN ERROR 'error de funcion SAP'. 
  END.
  RETURN cstatus.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-palletsToSapNuevoCalidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE palletsToSapNuevoCalidad Procedure 
PROCEDURE palletsToSapNuevoCalidad :
/*----------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER iSucTrabajo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER iPallet AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER cStatus AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER oPallet AS CHARACTER NO-UNDO.

  
  
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.

  DEFINE VARIABLE   hDocu       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE   hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRow        AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRows       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hparametros AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR iCodigo AS INTEGER NO-UNDO.
  DEFINE VAR cCentro AS CHARACTER NO-UNDO.





  DEFINE VARIABLE inum AS INTEGER NO-UNDO.
  DEFINE VARIABLE cestado AS CHARACTER NO-UNDO.
  DEFINE VARIABLE K AS INTEGER NO-UNDO.

  DEFINE VAR par AS INTEGER INITIAL 0.

  DEFINE VARIABLE   cFecha          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cPallet         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   hCabecera       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion01     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion02     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion03     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion04     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion05     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion06     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion07     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion08     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion09     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion10     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion11     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPosicion12     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hReccount       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hLote           AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hStatus         AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   xcentro         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   xalmacen        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   iEstado         AS INTEGER NO-UNDO.
  DEFINE VARIABLE   cCabecera       AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE   cItem           AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   ibultos         AS INTEGER NO-UNDO.
  DEFINE VARIABLE   iTraz           AS INTEGER NO-UNDO.
  DEFINE VARIABLE   iItem           AS INTEGER NO-UNDO.

  DEFINE VARIABLE   igln            AS INT64   NO-UNDO.

  DEFINE BUFFER bitems FOR items_pallets.



  hFunctions = connectToSapCalidad().


  cstatus = "".
  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc         = hFunctions:ADD('BAPI_NOTIFICAR_PALLET').
      
      hCabecera      = hFunc:exports('P_CABECERA').
      hPosicion01    = hFUnc:EXPORTS('P_POSICION_01').
      hPosicion02    = hFUnc:EXPORTS('P_POSICION_02').
      hPosicion03    = hFUnc:EXPORTS('P_POSICION_03').
      hPosicion04    = hFUnc:EXPORTS('P_POSICION_04').
      hPosicion05    = hFUnc:EXPORTS('P_POSICION_05').
      hPosicion06    = hFUnc:EXPORTS('P_POSICION_06').
      hPosicion07    = hFUnc:EXPORTS('P_POSICION_07').
      hPosicion08    = hFUnc:EXPORTS('P_POSICION_08').
      hPosicion09    = hFUnc:EXPORTS('P_POSICION_09').
      hPosicion10    = hFUnc:EXPORTS('P_POSICION_10').
      hPosicion11    = hFUnc:EXPORTS('P_POSICION_11').
      hPosicion12    = hFUnc:EXPORTS('P_POSICION_12').
      hReccount      = hFUnc:ImportS('O_RECCOUNT').
      hLote          = hFUnc:ImportS('O_LOTE').
      hStatus        = hFUnc:imports('O_STATUS').


      FIND pallets WHERE pallets.id_suc_trabajo = ISucTrabajo AND
                         pallets.id_pallet = iPallet NO-LOCK NO-ERROR.

      IF AVAILABLE pallets  THEN DO ON ERROR UNDO , RETURN ERROR "Error en proceso de pallets":
          
          FIND items_pedidos_packing OF pallets NO-LOCK NO-ERROR.
          FIND pedidos_packing OF items_pedidos_packing NO-LOCK NO-ERROR.

          IF NOT AVAILABLE pedidos_packing OR NOT AVAILABLE items_pedidos_packing  THEN DO:
              hFunctions:connection:logoff().
              UNDO , RETURN ERROR 'Error en los pedidos'.
          END.


          cPallet = string(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet , '99999999999').
          cfecha = string(YEAR(TODAY),'9999') + string(MONTH(TODAY),'99') + STRING(DAY(TODAY),'99').

          cCabecera =  cPallet + '|' + STRING(pedidos_packing.oe,'9999999999') + '|' + STRING(items_pedidos_packing.posicion_oe,'999999') + 
                        '|' +  STRING(pedidos_packing.Of_sap,'999999999999') + '|' + STRING(items_pedidos_packing.material_sap,'X(18)') + '|xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|'.
         /*
          cCabecera =  cPallet + '|' + STRING(11773,'9999999999') + '|' + STRING(100,'999999') + 
                        '|' +  STRING(1002401,'999999999999') + '|' + STRING('L01-001-005-072','X(18)') + '|xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|'.
         */
         
          FIND FIRST clientes_ventas WHERE clientes_ventas.id_cliente = pedidos_packing.id_cliente NO-LOCK NO-ERROR.
         
          IF AVAILABLE clientes_ventas THEN
             IF clientes_ventas.mercado = 1 THEN cCabecera = cCabecera + 'EXTERNO|'.
             ELSE cCabecera = cCabecera + 'INTERNO|'.
             ELSE cCabecera = cCabecera + 'EXTERNO|'. /*MERCADO*/

          IF pedidos_packing.UNION_europea THEN  cCabecera = cCabecera + 'x|'. ELSE cCabecera = cCabecera + '|'.
          IF pedidos_packing.china THEN cCabecera = cCabecera + 'x|'. ELSE cCabecera = cCabecera + '|'.

          cCabecera = cCabecera  + cfecha + '|' + cfecha + '|'.

          
          FIND clientes_ventas WHERE clientes_ventas.id_cliente = pedidos_packing.id_cliente_remito NO-LOCK NO-ERROR.

          IF AVAILABLE clientes_ventas THEN cCabecera = cCabecera +  string(clientes_ventas.id_cliente_sap).
                                       ELSE cCabecera = cCabecera + '9999999999'.
          cCabecera = cCabecera + '|' .

          IF AVAILABLE clientes_ventas THEN cCabecera = cCabecera + clientes_ventas.razon_social.
                                       ELSE cCabecera = cCabecera + 'SIN CLIENTE'. 
          cCabecera = cCabecera +  '|'.


          FIND clientes_ventas WHERE clientes_ventas.id_cliente = pedidos_packing.id_cliente NO-LOCK NO-ERROR.
          

           
          IF AVAILABLE clientes_ventas THEN cCabecera = cCabecera + string(clientes_ventas.id_cliente_sap).
                                       ELSE cCabecera = cCabecera + '9999999999'.

          cCabecera = cCabecera  + '|'.
          
          IF AVAILABLE clientes_ventas THEN cCabecera = cCabecera + clientes_ventas.razon_social.
                                       ELSE cCabecera = cCabecera + 'SIN CLIENTE'.
          
          
          FIND vapores WHERE vapores.id_vapor = pedidos_packing.id_vapor NO-LOCK NO-ERROR.
          
          cCabecera = cCabecera + '|' + STRING(vapores.id_vapor_sap) + '|' + vapores.descripcion + '|'.


          FIND lugar_descarga WHERE lugar_descarga.id_lugdes = pedidos_packing.id_puerto_sal NO-LOCK NO-ERROR.

          IF AVAILABLE lugar_descarga THEN cCabecera = cCabecera + string(lugar_descarga.id_lugdes_sap).
                               ELSE cCabecera = cCabecera + '9999'.
          
          cCabecera = cCabecera +  '|' .

          IF AVAILABLE lugar_descarga THEN cCabecera = cCabecera + lugar_descarga.descripcion.
                               ELSE cCabecera = cCabecera + 'SIN PUERTO'.
          cCabecera = cCabecera +  '|'  .

          
          FIND destinos WHERE destinos.id_destino = pedidos_packing.id_destino_final NO-LOCK NO-ERROR.
          
          IF AVAILABLE destinos THEN cCabecera = cCabecera + destinos.id_destino_sap.
                                ELSE cCabecera = cCabecera + '999'. 
          cCabecera = cCabecera + '|' .

          IF AVAILABLE destinos THEN cCabecera = cCabecera + destinos.descripcion.
                                ELSE cCabecera = cCabecera + 'SIN DESTINO'.
          cCabecera = cCabecera  + '|' . 
                        
          
          FIND destinos WHERE destinos.id_destino = pedidos_packing.id_puerto_ent NO-LOCK NO-ERROR.

          IF AVAILABLE destinos THEN cCabecera = cCabecera + string(destinos.id_destino_sap).
                                ELSE cCabecera = cCabecera + '9999'.

          cCabecera = cCabecera +  '|' .

          IF AVAILABLE destinos THEN cCabecera = cCabecera + destinos.descripcion.
                                ELSE cCabecera = cCabecera + 'SIN PUERTO'.
          cCabecera = cCabecera +  '|'  .

          FIND tipo_pallets OF pallets NO-LOCK NO-ERROR.
          FIND tipo_esquineros OF pallets NO-LOCK NO-ERROR.
          FIND calidades OF pallets NO-LOCK NO-ERROR.

          FIND categorias_packing OF pallets NO-LOCK NO-ERROR.
          FIND caracteristicas OF pallets NO-LOCK NO-ERROR.

          IF pedidos_packing.OF_sap = '' THEN DO:
              CASE pallets.id_suc_trabajo.
                  WHEN 98   THEN    ASSIGN xcentro = 'A100' xalmacen = '1010'. /**** OJO *****/
                  WHEN 97   THEN    ASSIGN xcentro = 'A200' xalmacen = '2010'.
                  OTHERWISE         ASSIGN xcentro = 'A700'.
              END CASE.
          END.
          
          cCabecera = cCabecera + pallets.contramarca + '|' + string(tipo_pallets.id_tipo_pallet_sap) + '|' +
                        string(tipo_esquineros.id_tipo_esquinero_sap) + '|' +  string(calidades.id_calidad_sap) + '|' +
                        string(categorias_packing.id_categoria_sap)  + '|' + string(caracteristicas.id_caract_sap) + '|' + STRING(pallets.peso) + '||' +
                        STRING(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet,'9999999999') + '|||||' + xcentro + '|' + xalmacen + '|'.


          igln = pallets.gln.

          ccabecera = ccabecera + STRING(igln, '9999999999999') + "|".  
          hCabecera:VALUE = cCabecera .



           

          inum = 0.
          iTraz = 0.
          FOR EACH items_pallets OF pallets NO-LOCK BREAK   BY items_pallets.calibre 
                                                            BY items_pallets.codigo_trazabilidad .
              inum = inum + items_pallets.bultos.
              ibultos = iBultos + items_pallets.bultos.
              IF LAST-OF(items_pallets.codigo_trazabilidad) THEN
              DO:
                  IF inum > iTraz THEN
                  DO:
                      iItem = items_pallets.ITEM_pallet.
                      iTraz = inum.
                  END.
                  inum = 0.
              END.

              IF LAST-OF(items_pallets.calibre) THEN
              DO:
                  IF iItem > 0  THEN
                  DO:
                      FIND bitems WHERE bitems.id_suc_trabajo = pallets.id_suc_trabajo AND
                                        bitems.id_pallet      = pallets.id_pallet AND
                                        bitems.ITEM_pallet    = iItem NO-LOCK NO-ERROR.
                      CREATE ttitems.
                      BUFFER-COPY bitems TO ttitems.
                      ASSIGN ttitems.bultos = iBultos.
                  END.

                  iTraz = 0.
                  iBultos = 0.
                  iItem = 0.      
              END.
          END.

          K = 0.
          FOR EACH ttitems OF pallets NO-LOCK.
            K = K + 1.
          END.

          IF K > 12  THEN
          DO:
              hfunctions:connection:logoff().
              RETURN ERROR "No se puede generar pallet en Sap. El pallet tiene mas de 12 posiciones".
          END.

          inum = 0.
          FOR EACH ttitems OF pallets NO-LOCK.

              inum = inum + 1.

              FIND fechas_produccion WHERE fechas_produccion.fecha = pallets.fecha_prod NO-LOCK NO-ERROR.
              FIND tipos_procesos WHERE tipos_procesos.id_tipo_proceso = TRIM(pallets.tipo_proceso) NO-LOCK NO-ERROR.
              FIND packing OF pallets NO-LOCK NO-ERROR.
              FIND colores OF ttitems NO-LOCK NO-ERROR.
              FIND proveedores OF ttitems NO-LOCK NO-ERROR.
              FIND origenes OF ttitems NO-LOCK NO-ERROR.
              FIND lote OF ttitems NO-LOCK NO-ERROR.


              cItem =  '|' + string(ttitems.ITEM_pallet,'999999') + '|' .
              IF AVAILABLE fechas_produccion  THEN cItem = cItem + fechas_produccion.fecha_senasa. 
                                              ELSE cItem = cItem  + 'SF'.

              cItem = cItem + '|'.


              IF AVAILABLE tipos_procesos    THEN cItem = cItem + tipos_procesos.id_tipo_proceso. 
                                             ELSE cItem = cItem  + 'SIN PROCESO'.

              cItem = cItem + '|'.


              IF AVAILABLE colores          THEN cItem = cItem + string(colores.id_color_sap). 
                                            ELSE cItem = cItem  + 'SIN COLOR'.

             cItem = cItem + '|'.

             IF pallets.testigo THEN  cItem = cItem + 'X'.
                                ELSE  cItem = cItem + ' '.

             cItem = cItem + '|'.
              
              cItem = cItem + string(ttitems.id_turno_packing) + '|' + ttitems.codigo_trazabilidad + '|' +
                            SUBstring( STRING(ttitems.calibre),1,LENGTH(STRING(ttitems.calibre)) - 1 ) + '|' + string(ttitems.bultos) + '|0|'.

              IF AVAILABLE proveedores THEN  cItem = cItem + SUBSTRING(STRING(proveedores.cuit),1,10).
                                       ELSE  cItem = cItem + '9999999999'.

              cItem = cItem + '|' .

              IF AVAILABLE origenes THEN cItem = cItem + substring(origenes.id_origen_sap,5,4).
                                    ELSE cItem = cItem + '9999'.

              cItem = cItem + '|' + lote.descripcion + '|'.
              
              IF  pallets.union_europea THEN  cItem = cItem + 'X'. ELSE cItem = cItem + ' '.
              cItem = cItem + '|' .
              IF  pallets.china THEN  cItem = cItem + 'X'. ELSE cItem = cItem + ' '.
              cItem = cItem + '|' .

              IF AVAILABLE origenes THEN cItem = cItem + string(origenes.id_finca_senasa,'9999').
                                    ELSE cItem = cItem + '9999'.

                                         cItem = cItem + '|' .
              IF AVAILABLE lote     THEN cItem = cItem + string(lote.id_lote_senasa,'999').
                                    ELSE cItem = cItem + '999'.

                                         cItem = cItem + '|' .
              IF AVAILABLE lote     THEN cItem = cItem + lote.certificado.
                                    ELSE cItem = cItem + ''.
              
                                         cItem = cItem + '|' .
              IF AVAILABLE lote     THEN cItem = cItem + lote.cert_china.
                                    ELSE cItem = cItem + ''.
                                         cItem = cItem + '|' .
              cItem = cItem + string(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet , '99999999999'). 
              CASE inum:
                  WHEN 1  THEN   hPosicion01:VALUE = cItem.
                  WHEN 2  THEN   hPosicion02:VALUE = cItem.
                  WHEN 3  THEN   hPosicion03:VALUE = cItem.
                  WHEN 4  THEN   hPosicion04:VALUE = cItem.
                  WHEN 5  THEN   hPosicion05:VALUE = cItem.
                  WHEN 6  THEN   hPosicion06:VALUE = cItem.
                  WHEN 7  THEN   hPosicion07:VALUE = cItem.
                  WHEN 8  THEN   hPosicion08:VALUE = cItem.
                  WHEN 9  THEN   hPosicion09:VALUE = cItem.
                  WHEN 10 THEN   hPosicion10:VALUE = cItem.
                  WHEN 11 THEN   hPosicion11:VALUE = cItem.
                  WHEN 12 THEN   hPosicion12:VALUE = cItem.
                  OTHERWISE 
                  DO:                          
                      hFunctions:connection:logoff().
                      UNDO, RETURN ERROR "Error por mas de 12 ¡tems".
                  END.
              END CASE.
          END.
          cStatus = "".
          IF hFunc:CALL() THEN  
          DO:
             IF trim(string(hStatus:value)) <> "T" THEN
             DO:
                 hfunctions:connection:logoff().
                 UNDO, RETURN ERROR "Error de estado diferente a T".
             END.

             IF inum <> INTEGER(hReccount:VALUE) THEN
             DO:
                 hFunctions:connection:logoff().
                 UNDO , RETURN ERROR "La cantidad de registros no coincide".
             END.
             opallet = STRING(hLote:VALUE).
             cstatus = STRING(hStatus:VALUE).
          END. 
          ELSE DO: 
              hFunctions:connection:logoff().
              UNDO , RETURN ERROR 'Error interface de pallet'. 
          END.    
          hFunctions:connection:logoff().
      
          FOR EACH ttitems OF pallets.
              DELETE ttitems.
          END.
      END.
      ELSE DO: 
          hFunctions:connection:logoff().
          UNDO, RETURN ERROR 'pallet inexistente'. 
      END.
  END.
  ELSE DO: 
      UNDO , RETURN ERROR 'error de funcion SAP'. 
  END.
  RETURN cstatus.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-palletsToSapPrueba) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE palletsToSapPrueba Procedure 
PROCEDURE palletsToSapPrueba :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
----------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iSucTrabajo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER iPallet AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER cStatus AS CHARACTER.

  
  
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.

  DEFINE VARIABLE   hDocu       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE   hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRow        AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRows       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hparametros AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR iCodigo AS INTEGER NO-UNDO.
  DEFINE VAR cCentro AS CHARACTER NO-UNDO.





  DEFINE VARIABLE inum AS INTEGER NO-UNDO.
  DEFINE VARIABLE cestado AS CHARACTER NO-UNDO.

  DEFINE VAR par AS INTEGER INITIAL 0.

  DEFINE VARIABLE   cFecha          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cPallet         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   cParametros     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   hPalletSap      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   xcentro         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   xalmacen        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE   iEstado         AS INTEGER NO-UNDO.


  hFunctions = connectToSapCalidad().

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc         = hFunctions:ADD('BAPI_PALLET_CREATE').
      
      hparametros   = hFunc:exports('LPARAMETROS').
      hPalletSap    = hFUnc:ImportS('O_PALLET').


      FIND pallets WHERE pallets.id_suc_trabajo = ISucTrabajo AND
                         pallets.id_pallet = iPallet NO-ERROR.

      IF AVAILABLE pallets  THEN DO ON ERROR UNDO , RETURN ERROR cstatus:
          
          FIND items_pedidos_packing OF pallets NO-LOCK NO-ERROR.
          FIND pedidos_packing OF items_pedidos_packing NO-LOCK NO-ERROR.

          IF NOT AVAILABLE pedidos_packing OR NOT AVAILABLE items_pedidos_packing  THEN DO:
              cstatus = 'Error en los pedidos'.
              hFunctions:connection:logoff().
              UNDO , RETURN ERROR cstatus.
          END.


          cPallet = string(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet , '99999999999').
          cfecha = string(YEAR(TODAY),'9999') + string(MONTH(TODAY),'99') + STRING(DAY(TODAY),'99').


          cParametros =  cPallet + '|' + STRING(pedidos_packing.oe,'9999999999') + '|' + STRING(items_pedidos_packing.posicion_oe,'999999') + 
                        '|' +  STRING(pedidos_packing.Of_sap,'999999999999') + '|' + STRING(items_pedidos_packing.material_sap,'X(18)') + '|xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|'.

/*          cParametros =  cPallet + '|' + STRING(11773,'9999999999') + '|' + STRING(100,'999999') + 
                        '|' +  STRING(1002401,'999999999999') + '|' + STRING('L01-001-005-072','X(18)') + '|xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|'.
*/         
                                                                                                                                                                                       
          FIND FIRST clientes_ventas WHERE clientes_ventas.id_cliente = pedidos_packing.id_cliente NO-LOCK NO-ERROR.
         
          IF AVAILABLE clientes_ventas THEN
             IF clientes_ventas.mercado = 1 THEN cparametros = cparametros + 'EXTERNO|'.
             ELSE cparametros = cparametros + 'INTERNO|'.
             ELSE cparametros = cparametros + 'EXTERNO|'. /*MERCADO*/

          IF pedidos_packing.UNION_europea THEN  cparametros = cparametros + 'x|'. ELSE cparametros = cparametros + '|'.
          IF pedidos_packing.china THEN cparametros = cparametros + 'x|'. ELSE cparametros = cparametros + '|'.

          cparametros = cparametros  + cfecha + '|' + cfecha + '|'.

          
          FIND clientes_ventas WHERE clientes_ventas.id_cliente = pedidos_packing.id_cliente_remito NO-LOCK NO-ERROR.

          IF AVAILABLE clientes_ventas THEN cParametros = cparametros +  string(clientes_ventas.id_cliente_sap).
                                       ELSE cParametros = cparametros + '9999999999'.
          cParametros = cparametros + '|' .

          IF AVAILABLE clientes_ventas THEN cParametros = cparametros + clientes_ventas.razon_social.
                                       ELSE cParametros = cparametros + 'SIN CLIENTE'. 
          cParametros = cparametros +  '|'.


          FIND clientes_ventas WHERE clientes_ventas.id_cliente = pedidos_packing.id_cliente NO-LOCK NO-ERROR.
          

           
          IF AVAILABLE clientes_ventas THEN cParametros = cparametros + string(clientes_ventas.id_cliente_sap).
                                       ELSE cParametros = cparametros + '9999999999'.

          cparametros = cparametros  + '|'.
          
          IF AVAILABLE clientes_ventas THEN cParametros = cparametros + clientes_ventas.razon_social.
                                       ELSE cParametros = cparametros + 'SIN CLIENTE'.
          
          
          FIND vapores WHERE vapores.id_vapor = pedidos_packing.id_vapor NO-LOCK NO-ERROR.
          
          cparametros = cparametros + '|' + STRING(vapores.id_vapor_sap) + '|' + vapores.descripcion + '|'.


          FIND lugar_descarga WHERE lugar_descarga.id_lugdes = pedidos_packing.id_puerto_sal NO-LOCK NO-ERROR.

          IF AVAILABLE lugar_descarga THEN cParametros = cparametros + string(lugar_descarga.id_lugdes_sap).
                               ELSE cParametros = cparametros + '9999'.
          
          cParametros = cparametros +  '|' .

          IF AVAILABLE lugar_descarga THEN cParametros = cparametros + lugar_descarga.descripcion.
                               ELSE cParametros = cparametros + 'SIN PUERTO'.
          cParametros = cparametros +  '|'  .

          
          FIND destinos WHERE destinos.id_destino = pedidos_packing.id_destino_final NO-LOCK NO-ERROR.
          
          IF AVAILABLE destinos THEN cParametros = cparametros + destinos.id_destino_sap.
                                ELSE cParametros = cparametros + '999'. 
          cParametros = cparametros + '|' .

          IF AVAILABLE destinos THEN cParametros = cparametros + destinos.descripcion.
                                ELSE cParametros = cparametros + 'SIN DESTINO'.
          cParametros = cparametros  + '|' . 
                        
          
          FIND destinos WHERE destinos.id_destino = pedidos_packing.id_puerto_ent NO-LOCK NO-ERROR.

          IF AVAILABLE destinos THEN cParametros = cparametros + string(destinos.id_destino_sap).
                                ELSE cParametros = cparametros + '9999'.

          cParametros = cparametros +  '|' .

          IF AVAILABLE destinos THEN cParametros = cparametros + destinos.descripcion.
                                ELSE cParametros = cparametros + 'SIN PUERTO'.
          cParametros = cparametros +  '|'  .

          FIND tipo_pallets OF pallets NO-LOCK NO-ERROR.
          FIND tipo_esquineros OF pallets NO-LOCK NO-ERROR.
          FIND calidades OF pallets NO-LOCK NO-ERROR.

          FIND categorias_packing OF pallets NO-LOCK NO-ERROR.
          FIND caracteristicas OF pallets NO-LOCK NO-ERROR.

          IF pedidos_packing.OF_sap = '' THEN DO:
              CASE pallets.id_suc_trabajo.
                  WHEN 98   THEN    ASSIGN xcentro = 'A100' xalmacen = '1010'. /**** OJO *****/
                  WHEN 97   THEN    ASSIGN xcentro = 'A200' xalmacen = '2010'.
                  OTHERWISE         ASSIGN xcentro = 'A700'.
              END CASE.
          END.
          
          cParametros = cparametros + pallets.contramarca + '|' + string(tipo_pallets.id_tipo_pallet_sap) + '|' +
                        string(tipo_esquineros.id_tipo_esquinero_sap) + '|' +  string(calidades.id_calidad_sap) + '|' +
                        string(categorias_packing.id_categoria_sap)  + '|' + string(caracteristicas.id_caract_sap) + '|' + STRING(pallets.peso) + '||' +
                        STRING(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet,'9999999999') + '||||' + xcentro + '|' + xalmacen + '|'.

          hParametros:VALUE = cparametros .
         
          /*
          OUTPUT TO Z:\TEMP\PAR.PAB.
          EXPORT HPARAMETROS:VALUE.
          OUTPUT CLOSE.
          */

          ETIME(TRUE).
          REPEAT:
              IF ETIME >= 500 THEN LEAVE.
          END.
          IF hFunc:CALL() THEN  DO:

             pallets.id_pallet_sap = STRING(hPalletSap:VALUE). 
             MESSAGE 'Numero de Lote Sap ' STRING(hPalletSap:VALUE) VIEW-AS ALERT-BOX INFORMATION.
             IF STRING(hPalletSap:VALUE) = '1' THEN  DO:
                 hFunctions:connection:logoff().
                 cStatus = 'Error en loteo de pallet '.
                 UNDO , RETURN ERROR cstatus.
             END.
             RUN itemsPalletsToSapPrueba ( pallets.id_suc_trabajo , pallets.id_pallet , string(hPalletSap:VALUE ), INPUT hFunctions , OUTPUT iNum , OUTPUT cEstado) NO-ERROR.
             IF ERROR-STATUS:ERROR THEN  DO:
                 cStatus = 'Error interface de items ' + cEstado.
                 hFunctions:connection:logoff().
                 UNDO, RETURN ERROR cstatus. 
             END.
             cStatus = 'OK'.
          END. 
          ELSE DO: 
              cStatus = 'Error interface de pallet'. 
              hFunctions:connection:logoff().
              UNDO , RETURN ERROR cstatus. 
          END.    
          /*
          DISPLAY "Declarando Pallet Sap" pallets.id_pallet WITH FRAME informe CENTERED OVERLAY THREE-D.
          RUN procesarpallet(pallets.id_pallet_sap) NO-ERROR.   
          HIDE FRAME informe. */
          hFunctions:connection:logoff().
      END.
      ELSE DO: 
          cStatus = 'pallet inexistente'. 
          hFunctions:connection:logoff().
          UNDO, RETURN ERROR. 
      END.
  END.
  ELSE DO: 
      cstatus = 'error de funcion SAP'. 
      UNDO , RETURN ERROR. 
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-parametroIngresoFruta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE parametroIngresoFruta Procedure 
PROCEDURE parametroIngresoFruta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pbalanza AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pPesada AS INTEGER NO-UNDO.


DEFINE VARIABLE cparametros     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStatus         AS CHARACTER NO-UNDO.
DEFINE VARIABLE hFunctions      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hFunc           AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hparametros     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hOCarga         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hPesada         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE i               AS INTEGER NO-UNDO.
DEFINE VARIABLE xservicio       AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcorte          AS CHARACTER NO-UNDO.
DEFINE VARIABLE xarticulo       AS CHARACTER NO-UNDO.
DEFINE VARIABLE xenvase         AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcolor          AS CHARACTER NO-UNDO.
DEFINE VARIABLE xlote           AS CHARACTER NO-UNDO.
DEFINE VARIABLE xmercado        AS CHARACTER NO-UNDO.
DEFINE VARIABLE xtipotransporte AS CHARACTER NO-UNDO.
DEFINE VARIABLE xproceso        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cant            AS DECIMAL NO-UNDO.
DEFINE VARIABLE xcant           AS CHARACTER NO-UNDO.
DEFINE VARIABLE xentrega_sap    AS CHARACTER NO-UNDO.


/* OUTPUT STREAM a-chk TO e:\temp\ztmm_mov_flete.txt APPEND.*/

    FIND balanza_pesadas WHERE  balanza_pesadas.id_balanza = pbalanza AND
                                balanza_pesadas.id_pesada  = pPesada  NO-ERROR NO-WAIT.

    FIND FIRST balanza_tickets OF balanza_pesadas WHERE
        balanza_tickets.id_tipo_cosecha > 0 AND balanza_tickets.id_tipo_cosecha < 4 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE balanza_tickets THEN
        RETURN ERROR 'DESCARTE O PROCESADO !!'.

    /*** Cuando viene con PESO_DESCARTE que es fruta para la Industria,
     no debe entrar en la Interface.
     SE QUITO ESTA CONDICIàN PARA LIQUIDAR TAMBIEN LOS VIAJES  
    IF AVAILABLE balanza_pesadas AND
                 balanza_pesadas.peso_descarte = 0 THEN
                 ****/

        IF pbalanza <> 2 AND pbalanza <> 4 THEN UNDO, RETURN ERROR 'BALANZA EQUIVOCADA'.
        
        FIND FIRST balanza_tickets OF balanza_pesadas NO-ERROR.
        IF NOT AVAILABLE balanza_tickets THEN UNDO , RETURN ERROR 'PESADA SIN ITEMS'.
        

        IF balanza_pesadas.id_pesada_sap = ? OR balanza_pesadas.id_pesada_sap = '' THEN
            balanza_pesadas.id_pesada_sap = '000000000000'.

        IF BALANZA_PESADAS.ID_PESADA_SAP =  '            ' THEN BALANZA_PESADAS.ID_PESADA_SAP = '000000000000'.
            
            cparametros = string(balanza_pesadas.id_pesada_sap , '999999999999') + '|||'.

            FIND transportes_proveedor WHERE transportes_proveedor.id_transporte =  balanza_pesadas.id_transporte NO-LOCK NO-ERROR.
            FIND proveedores OF transportes_proveedor NO-LOCK NO-ERROR.
            IF AVAILABLE proveedores  THEN
                cparametros = cparametros + substring(proveedores.cuit,1,10) + '|'.
            ELSE
                cparametros = cparametros + '0000000000|'.

            IF AVAILABLE transportes_proveedor THEN  cparametros = cparametros + STRING(transportes_proveedor.id_transporte). /* se reeemplazo id_transporte_sap que es lo mismo*/
                                               ELSE  cparametros = cparametros + '9999' .

            FIND tipo_transporte OF transportes_proveedor NO-LOCK NO-ERROR.

            IF AVAILABLE tipo_transporte  THEN xtipotransporte = tipo_transporte.id_tipo_transporte_sap.  ELSE xtipotransporte = ''.

            FIND balanzas OF balanza_pesadas NO-LOCK NO-ERROR. 

            cparametros = cparametros + '|0|CHOFER7890123456789012345||||00000000||||0||||0|0||'. 

            cparametros =   cparametros + string(year(produccion.balanza_pesadas.fecha_entrada),'9999') + 
                            STRING(MONTH(produccion.balanza_pesadas.fecha_entrada),'99') + STRING(DAY(produccion.balanza_pesadas.fecha_entrada),'99') + '|' +
                            REPLACE(balanza_pesadas.hora_entrada,':','') + '||||' + xtipotransporte + '||||||||||||||||' + 
                            trim(replace(substring(balanzas.id_balanza_sap,9,4),'0','')) + '||||'.

            IF AVAILABLE transportes_proveedor THEN cparametros = cparametros  + transportes_proveedor.patente + '|' + transportes_proveedor.patente_acop.
                                               ELSE cparametros = cparametros  + 'SP999|SP999'.

            CASE balanzas.id_sucursal:
                WHEN 97  THEN ASSIGN    xproceso = 'PF' .
                WHEN 98  THEN ASSIGN    xproceso = 'PL'  .
                WHEN 99  THEN ASSIGN    xproceso = 'PU' .
                WHEN 95  THEN xproceso = 'IN'.
                WHEN 96  THEN xproceso = 'IN'.
            END CASE.


            FIND FIRST balanza_tickets OF balanza_pesadas NO-LOCK.
            FIND envases_prod OF balanza_ticket NO-LOCK.

            
            FOR EACH balanza_tickets OF balanza_pesada NO-LOCK.
                cant = cant + produccion.balanza_tickets.cant_env_entrada.
            END.
            
            IF AVAILABLE envases_prod AND xproceso = 'IN' THEN 
                    ASSIGN  xenvase = envases_prod.id_envase_sap
                            xcant = STRING(cant).
            cant = 0.
            
            cparametros = cparametros + '|000000000000000||||' + xenvase + '|'  + xproceso + '|FI||' + xcant + '|0000000000|||||||||' .

            IF AVAILABLE proveedores THEN
                cparametros = cparametros + SUBSTRING(string(cuit),1,10) + '|||||' + string(produccion.balanza_pesadas.id_pesada_sap,'999999999999')  + '|||||'
                              + xentrega_sap.
            ELSE
                cparametros = cparametros + '0000000000|||||' + string(produccion.balanza_pesadas.id_pesada_sap,'999999999999')  + '|||||' 
                              + xentrega_sap.

            cparametros = cparametros + '|' + string(balanza_pesada.peso_entrada) + '|' + string(balanza_pesada.peso_salida) + '|' +
                     string(year(produccion.balanza_pesadas.fecha_salida),'9999') + 
                     STRING(MONTH(produccion.balanza_pesadas.fecha_salida),'99') + STRING(DAY(produccion.balanza_pesadas.fecha_salida),'99') + '|' +
                     /*** REPLACE(balanza_pesadas.hora_salida,':','') + '|' + string(balanza_pesada.peso_descarte) + '|' +  SE SACO EL PESO DESCARTE ***/
                     REPLACE(balanza_pesadas.hora_salida,':','') + '|' + '' + '|' +
                     STRING(cant) + '|' + string(balanza_pesada.peso_neto).


            OUTPUT TO VALUE("z:\temp\cparametros.txt").
                EXPORT CPARAMETROS.
            OUTPUT CLOSE.

            OS-COMMAND NO-WAIT (" notepad.exe  z:\temp\cparametros.txt" ).

            /* Si es un remito con Entrega SAP, no generamos la tabla
             SAP ztmm_mov_flete_d */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-parametrosBalanzaTicket) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE parametrosBalanzaTicket Procedure 
PROCEDURE parametrosBalanzaTicket :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pbalanza AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ppesada AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pticket AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pproceso AS CHARACTER NO-UNDO.

DEFINE VARIABLE cparametros AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.



DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hparametros AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hProceso    AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hTicket     AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE xservicio     AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcorte        AS CHARACTER NO-UNDO.
DEFINE VARIABLE xarticulo     AS CHARACTER NO-UNDO FORMAT 'x(18)'.
DEFINE VARIABLE xenvase       AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcolor        AS CHARACTER NO-UNDO.
DEFINE VARIABLE xlote         AS CHARACTER NO-UNDO.
DEFINE VARIABLE xmercado      AS CHARACTER NO-UNDO.
DEFINE VARIABLE xalmacen      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cticket       AS CHARACTER NO-UNDO.
DEFINE VARIABLE xunidadMedida AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcant         AS CHARACTER NO-UNDO.
DEFINE VARIABLE xlinea        AS CHARACTER NO-UNDO.
DEFINE VARIABLE xCentro       AS CHARACTER NO-UNDO.
DEFINE VARIABLE vlote         AS CHARACTER NO-UNDO.
DEFINE VARIABLE xestatus      AS CHARACTER NO-UNDO.

DEFINE VARIABLE hLote AS COM-HANDLE NO-UNDO.


/*
DEFINE VARIABLE pcode   AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE VARIABLE porden  AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE pposic  AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE perror  AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE ptexto  AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE ptipoe  AS CHARACTER FORMAT "x" NO-UNDO.
*/

DEFINE VARIABLE pcode     AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE VARIABLE porden    AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE pposic    AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE perror    AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE ptexto    AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE ptipoe    AS CHARACTER FORMAT "x" NO-UNDO.


DEFINE BUFFER bp FOR balanza_pesadas.


FIND bp WHERE  bp.id_balanza = pbalanza AND
                            bp.id_pesada = pPesada  NO-LOCK NO-ERROR.

IF NOT AVAILABLE bp  THEN RETURN ERROR "NO ENCUENTRA LA PESADA".

/*
    i = 0.
*/    
    FOR EACH balanza_tickets    OF bp WHERE balanza_tickets.nro_ticket = pticket.
    
        cparametros = ''.
/*        
        i = i + 1.
*/
        i = pticket + 10.

        FIND tipos_servicios    OF balanza_tickets NO-LOCK NO-ERROR.
        FIND tipo_cosecha       OF balanza_tickets NO-LOCK NO-ERROR. 
        FIND envases_prod       OF balanza_tickets NO-LOCK NO-ERROR.
        FIND colores            OF balanza_tickets NO-LOCK NO-ERROR.
        FIND lote               OF balanza_tickets NO-LOCK NO-ERROR.
        FIND proveedores        OF balanza_tickets NO-LOCK NO-ERROR.

        IF tipo_cosecha.id_tipo_cosecha = 0 /* descarte */ OR tipo_cosecha.id_tipo_cosecha >= 4 /* procesado y otros */ THEN NEXT.
        FIND productos_terminados WHERE productos_terminados.id_articulo = balanza_tickets.id_materia_prima NO-LOCK NO-ERROR.

        FIND r_envases_prod WHERE r_envases_prod.id_envase = balanza_tickets.id_envase AND
                                  r_envases_prod.id_articulo = balanza_tickets.id_materia_prima NO-LOCK NO-ERROR.
        
        
        IF AVAILABLE tipos_servicios    THEN  xservicio = tipos_servicios.id_tipo_servicio_sap. ELSE xservicio = '00'.
        IF AVAILABLE tipo_cosecha       THEN  xcorte = tipo_cosecha.id_tipo_cosecha_sap. ELSE xcorte = '00'.
        IF AVAILABLE envases_prod       THEN  xenvase = envases_prod.id_envase_sap. ELSE ''.
        IF AVAILABLE colores            THEN  xcolor = STRING(colores.id_color_sap) . ELSE xcolor = ''.
        IF AVAILABLE lote               THEN  xlote = STRING(lote.DESCripcion). ELSE xlote = 'SL'.
        FIND origenes OF lote NO-LOCK NO-ERROR.


        IF AVAILABLE productos_terminados THEN
            CASE productos_terminados.id_articulo:
                WHEN 1 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 2 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'POMELO001        ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 3 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'NARANJA001       ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 4 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'MANDARINA001     ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 5 THEN 
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'PALTA001         ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 6 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'TANGELO001       ' xunidadMedida = 'KG'.
                    END CASE.
                WHEN 7 THEN
                    CASE tipo_cosecha.id_tipo_cosecha.
                        WHEN 4 THEN ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'KG'.
                        WHEN 1 THEN ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'BIN'.
                        OTHERWISE   ASSIGN xarticulo   = 'KUNKUAT001       ' xunidadMedida = 'KG'.
                    END CASE.
            END CASE.
        ELSE
            CASE tipo_cosecha.id_tipo_cosecha.
                WHEN 4 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
                WHEN 1 THEN ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'BIN'.
                OTHERWISE   ASSIGN xarticulo   = 'LIMON001         ' xunidadMedida = 'KG'.
            END CASE.



        IF balanza_tickets.UNION_europea THEN  xmercado = 'UE' . ELSE xmercado = 'NOUE'.

        FIND balanzas OF balanza_tickets NO-LOCK NO-ERROR.

        /*** en caso de industria determinar el almacen ***/

        cTicket = string(bp.id_pesada_sap , '999999999999' ).
        
        xcentro = SUBSTRING(balanzas.id_balanza_sap,1,4).
        IF pproceso <> 'IN' THEN xalmacen = SUBSTRING(balanzas.id_balanza_sap,5,4).
        ELSE                     xalmacen = /*SUBSTRING(balanzas.id_balanza_sap,5,4)*/ string(bp.id_pesada_ctf).
        
        /* XALMACEN = '1600'.*/

        CASE xcentro:
            WHEN 'A200' THEN
                CASE  xalmacen:
                    WHEN '2009' THEN  xlinea = 'D'.
                    WHEN '2001' THEN  xlinea = 'L'.
                    WHEN '2002' THEN  xlinea = 'S'.
                END CASE.
        END CASE.

        IF pproceso = 'IN' THEN xcant = STRING(bp.peso_entrada). 
        ELSE 
        DO:
           IF tipo_cosecha.id_tipo_cosecha = 1 THEN
              xcant = STRING(produccion.balanza_tickets.cant_env_entrada).
           ELSE
              xcant = STRING(balanza_tickets.peso_neto_ticket).
        END.

        IF balanza_tickets.orden_compra_sap = '' THEN
            xestatus    = "P".
        ELSE
            xestatus    = "O".


        cparametros = bp.orden_carga_sap + '|' + STRING(i,'9999') + '|' + balanza_tickets.codigo_trazabilidad + '|' +
                      STRING(YEAR(balanza_tickets.fecha_cosecha) , '9999') + STRING(MONTH(balanza_tickets.fecha_cosecha) , '99') +
                      STRING(DAY(balanza_tickets.fecha_cosecha) , '99') + '|' + xservicio + '|' + xcorte + '|' + xarticulo + '|' + xenvase + '|' +
                      xcolor + '|' + xcant + '|' + xunidadmedida + '|' + upper(xcentro) 
                      + '|' + xalmacen + '|'+ upper(SUBSTRING(origenes.id_origen_sap,1,4)) 
                      + '|' + SUBSTRING(origenes.id_origen_sap,5,4) + '|||' + 
                      STRING(balanza_tickets.nro_remito, '9999-99999999')+ '|' + STRING(balanza_tickets.peso_neto_ticket) + '||' +
                      xlote + '|' + balanza_tickets.orden_compra_sap + '|||' + xmercado + '|' + xlinea + '|' + SUBSTRING(STRING(proveedores.cuit),1,10) +
                      '|P||' + balanza_tickets.pos_orden_compra_sap + '|' + xestatus + '|P||P|||P||||||||' + string(balanza_tickets.nro_partida).


        OUTPUT TO VALUE("Z:\TEMP\CPARAMETROS.TXT").
            EXPORT CPARAMETROS.
        OUTPUT CLOSE.

        OS-COMMAND NO-WAIT (" NOTEPAD.EXE Z:\TEMP\CPARAMETROS.TXT").

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pesadaToSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pesadaToSap Procedure 
PROCEDURE pesadaToSap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pbalanza AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pPesada AS INTEGER NO-UNDO.


DEFINE VARIABLE cparametros AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStatus AS CHARACTER NO-UNDO.



DEFINE VARIABLE   hFunctions    AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hparametros   AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hOCarga       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hPesada       AS COM-HANDLE NO-UNDO.

DEFINE VAR        i             AS INTEGER NO-UNDO.
DEFINE VARIABLE xservicio AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcorte AS CHARACTER NO-UNDO.
DEFINE VARIABLE xarticulo AS CHARACTER NO-UNDO.
DEFINE VARIABLE xenvase AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcolor AS CHARACTER NO-UNDO.
DEFINE VARIABLE xlote  AS CHARACTER NO-UNDO.
DEFINE VARIABLE xmercado AS CHARACTER NO-UNDO.
DEFINE VARIABLE xtipotransporte AS CHARACTER NO-UNDO.
DEFINE VARIABLE xproceso AS CHARACTER NO-UNDO.

FIND balanza_pesadas WHERE  balanza_pesadas.id_balanza = pbalanza AND
                            balanza_pesadas.id_pesada = pPesada NO-ERROR.

IF AVAILABLE balanza_pesadas THEN DO ON ERROR UNDO , RETURN ERROR:

    hFunctions = connectToSap().

    IF VALID-HANDLE(hFunctions) THEN DO:

        hFunc           = hFunctions:ADD('BAPI_ZTMM_MOV_FLETE_CREATE').

        hparametros     = hFunc:exports('LPARAMETROS').
        hOCarga         = hFunc:imports('OCARGA').
        hPesada         = hFunc:imports('OTICKET').
    
        /***** indicador acoplado  codigo de balanza **/ 
        /***** numeracion de ticket impreso: numero de pesada ***/
            
        IF balanza_pesadas.id_pesada_sap = ? THEN balanza_pesadas.id_pesada_sap = '000000000000'.
        cparametros = cparametros + string(balanza_pesadas.id_pesada_sap , '999999999999') + '|||'.

        FIND transportes_proveedor WHERE transportes_proveedor.id_transporte =  balanza_pesadas.id_transporte NO-LOCK NO-ERROR.
        FIND proveedores OF transportes_proveedor NO-LOCK NO-ERROR.
            
        IF AVAILABLE proveedores  THEN
            cparametros = cparametros + substring(proveedores.cuit,1,10) + '|'.
        ELSE
            cparametros = cparametros + '0000000000|'.
            
        IF AVAILABLE transportes_proveedor THEN cparametros = cparametros + transportes_proveedor.id_transporte_sap. 
                                           ELSE cparametros = cparametros + '9999' .
    
        FIND tipo_transporte OF transportes_proveedor NO-LOCK NO-ERROR.
    
        IF AVAILABLE tipo_transporte THEN xtipotransporte = tipo_transporte.id_tipo_transporte_sap.
                                     ELSE xtipotransporte = ''.
    
        FIND balanzas OF balanza_pesadas NO-LOCK NO-ERROR. 
    
        cparametros = cparametros + '|0|CHOFER7890123456789012345||||00000000||||0||||0|0||'. 
    
        cparametros = cparametros + string(year(produccion.balanza_pesadas.fecha_entrada),'9999') + 
                      STRING(MONTH(produccion.balanza_pesadas.fecha_entrada),'99') +
                      STRING(DAY(produccion.balanza_pesadas.fecha_entrada),'99') + '|' +
                      REPLACE(balanza_pesadas.hora_entrada,':','') + '||||' + xtipotransporte +
                      '||||||||||||||||' + trim(replace(substring(id_balanza_sap,9,4),'0','')) + '||||'.
            
        IF AVAILABLE transportes_proveedor THEN cparametros = cparametros  + transportes_proveedor.patente + '|' + transportes_proveedor.patente_acop.
                                           ELSE cparametros = cparametros  + 'SP999|SP999'.
    
        CASE balanzas.id_sucursal:
            WHEN 97  THEN xproceso = 'PF'.
            WHEN 98  THEN xproceso = 'PL'.
            WHEN 99  THEN xproceso = 'PU'.
            WHEN 95  THEN xproceso = 'IN'.
        END CASE.
        
        cparametros = cparametros + '|000000000000000|||||'  + xproceso + '|FI||000000000000000000|0000000000|||||||||' .
 
        IF AVAILABLE proveedores THEN
            cparametros = cparametros + SUBSTRING(string(cuit),1,10) + '|||||' + string(produccion.balanza_pesadas.id_pesada_sap,'999999999999')  + '||||'.
        ELSE
            cparametros = cparametros + '0000000000|||||' + string(produccion.balanza_pesadas.id_pesada_sap,'999999999999')  + '||||'.
        
        hparametros:VALUE = cparametros.

        IF hFunc:CALL() THEN DO ON ERROR UNDO , RETURN ERROR:
            RUN pesadasToSap ( balanza_pesadas.id_balanza , balanza_pesadas.id_pesada ) NO-ERROR.
           
            IF ERROR-STATUS:ERROR THEN DO:
                hFunctions:connection:logoff().
                UNDO, RETURN ERROR. 
            END.

            balanza_pesadas.orden_carga_sap = hOCarga:VALUE. 
            balanza_pesadas.id_pesada_sap = STRING(hPesada:VALUE,'999999999999').
            RUN balanzaTicketsTosap ( balanza_pesadas.id_balanza , balanza_pesadas.id_pesada , xproceso, 0) NO-ERROR.

            IF ERROR-STATUS:ERROR THEN DO:
                hFunctions:connection:logoff().
                UNDO, RETURN ERROR. 
            END.
        END.
        ELSE DO:
            hFunctions:connection:logoff().
            UNDO, RETURN ERROR. 
        END.
        hFunctions:connection:logoff().
    END.
    ELSE DO:
        UNDO , RETURN ERROR.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-procesarBalanzaTicket) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesarBalanzaTicket Procedure 
PROCEDURE procesarBalanzaTicket :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER balanza_tickets FOR balanza_tickets.
    
DEFINE VAR hProd AS HANDLE NO-UNDO.
DEF BUFFER aux_pesada FOR balanza_pesadas.
DEF BUFFER aux_tickets FOR balanza_tickets.
DEF BUFFER aux_salida FOR balanza_salidas.
DEF VAR X_peso_envases AS DECIMAL NO-UNDO.
DEF VAR v1 AS INTEGER.
DEF VAR v2 AS INTEGER.

DEFINE VAR cerror AS CHARACTER NO-UNDO INITIAL 'Error en la rutina de proceso de la balanza'.



DO TRANSACTION ON ERROR UNDO , RETURN ERROR cerror + ERROR-STATUS:GET-MESSAGE(1) :
    {persistentprocedure.i libproduccion.p hProd}
    
    CASE balanza_tickets.id_balanza:
    WHEN  2 OR  WHEN 4 THEN
    DO:
    
        RUN crearpartidas IN hprod (INPUT balanza_tickets.id_sucursal,
                                    INPUT balanza_tickets.nro_partida ).
    
        RUN crearitemsstockdebalanza IN hprod (INPUT balanza_tickets.id_balanza, INPUT balanza_tickets.id_pesada, balanza_tickets.nro_ticket).
     END.
    WHEN 7 THEN
    DO:
      FIND FIRST aux_pesada WHERE
          aux_pesada.id_balanza = balanza_tickets.id_balanza and
          aux_pesada.id_pesada  = balanza_tickets.id_pesada no-lock no-error.
    
    
      find first aux_tickets WHERE   
                 aux_tickets.id_balanza = balanza_tickets.id_balanza and
                 aux_tickets.id_pesada  = balanza_tickets.id_pesada no-lock no-error.
      if available aux_tickets then do:
          for each aux_salida of aux_pesada:
              delete aux_salida.
          end.
          for each aux_tickets of aux_pesada:
              create aux_salida.
              assign
                  aux_salida.id_balanza          = aux_tickets.id_balanza
                  aux_salida.id_pesada           = aux_tickets.id_pesada
                  aux_salida.id_envase           = aux_tickets.id_envase
                  aux_salida.cant_env_salida     = aux_tickets.cant_env_entrada
                  aux_salida.peso_envases_salida = aux_tickets.peso_envases_entrada.
    
          end.
          find aux_pesada where
            aux_pesada.id_balanza = balanza_tickets.id_balanza and
            aux_pesada.id_pesada  = balanza_tickets.id_pesada.
    
          if available aux_pesada then do:
              for each aux_tickets where
                aux_tickets.id_balanza = balanza_tickets.id_balanza and
                aux_tickets.id_pesada  = balanza_tickets.id_pesada:
                  x_peso_envases  = x_peso_envases  + aux_tickets.peso_envases_entrada.
              end.
              assign aux_pesada.peso_envases_entrada = x_peso_envases.
          END.
      END.
    END.
    END CASE.
    
    /* Creo tabla items_transporte */
    
    RUN crearitemstranspdebalanza IN hprod (    INPUT balanza_tickets.id_balanza,
                                                INPUT balanza_tickets.id_pesada).
    
    
    /* Actualizo fecha y hora salida de balanza_pesada */
    FIND FIRST aux_pesada WHERE aux_pesada.id_balanza = balanza_tickets.id_balanza AND
    aux_pesada.id_pesada = balanza_tickets.id_pesada NO-ERROR.
    IF AVAILABLE aux_pesada THEN
    DO:
    ASSIGN aux_pesada.fecha_salida = balanza_tickets.fecha_salida
           aux_pesada.hora_salida = balanza_tickets.hora_salida.
    END.
    
    IF balanza_tickets.id_balanza = 2 OR balanza_tickets.id_balanza = 4 THEN
    DO:
        /* Actualizo pesos */
        RUN pesosbalanzapesada IN hprod (INPUT balanza_tickets.id_balanza,
                                INPUT balanza_tickets.id_pesada). 
    
         /* Actualizo saldos de partida */
        RUN actsaldospartidadebalanza IN hprod (INPUT balanza_tickets.id_sucursal, INPUT balanza_tickets.nro_partida , INPUT balanza_tickets.nro_partida_serial). 
    
    /*
        /* Imprimo etiqueta */
        RUN dd_etibal.p (input balanza_tickets.nro_partida).
    */
    END.
    CATCH err AS PROGRESS.Lang.ERROR:

        UNDO, RETURN ERROR cerror + err:GetMEssage(1) .    
    END CATCH.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-procesarPallet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesarPallet Procedure 
PROCEDURE procesarPallet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER cpallet AS CHARACTER FORMAT 'x(10)'.
  DEFINE OUTPUT PARAMETE iEstado AS INTEGER NO-UNDO.

  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPallet     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hEstado     AS COM-HANDLE NO-UNDO.

  hFunctions = connectToSap().

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc         = hFunctions:ADD('BAPI_PROCESAR_PALLET').
      hPallet       = hFUnc:exports('IP_PALLET').

      hPallet:VALUE = cpallet.
      IF NOT hFunc:CALL() THEN DO:
          hFunctions:connection:logoff().
          RETURN ERROR.
      END.
      ELSE DO:
          hFunc         = hFunctions:ADD('BAPI_GET_STATUS_PALLET').
          hPallet       = hFUnc:exports('IP_PALLET').
          hEstado       = hFUnc:imports('O_STATUS').
          hPallet:VALUE = cpallet.
          IF NOT hFunc:CALL() THEN DO:
              hFunctions:connection:logoff().
              RETURN ERROR 'Error  de procesamiento'.
          END.
          iEstado = INTEGER(hEstado:VALUE).
      END.
      
      hFunctions:connection:logoff().
  END.
  ELSE MESSAGE 'ERROR Connect' VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-procesarPalletPrueba) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesarPalletPrueba Procedure 
PROCEDURE procesarPalletPrueba :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER cpallet AS CHARACTER FORMAT 'x(10)'.
  DEFINE OUTPUT PARAMETE iEstado AS INTEGER NO-UNDO.

  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPallet     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hEstado     AS COM-HANDLE NO-UNDO.

  hFunctions = connectToSapCalidad().

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc         = hFunctions:ADD('BAPI_PROCESAR_PALLET').
      hPallet       = hFUnc:exports('IP_PALLET').

      hPallet:VALUE = cpallet.
      IF NOT hFunc:CALL() THEN DO:
          hFunctions:connection:logoff().
          RETURN ERROR.
      END.
      ELSE DO:
          hFunc         = hFunctions:ADD('BAPI_GET_STATUS_PALLET').
          hPallet       = hFUnc:exports('IP_PALLET').
          hEstado       = hFUnc:imports('O_STATUS').
          hPallet:VALUE = cpallet.
          IF NOT hFunc:CALL() THEN DO:
              hFunctions:connection:logoff().
              RETURN ERROR 'Error  de procesamiento'.
          END.
          iEstado = INTEGER(hEstado:VALUE).
      END.
      
      hFunctions:connection:logoff().
  END.
  ELSE MESSAGE 'ERROR Connect' VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pruebaConexion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pruebaConexion Procedure 
PROCEDURE pruebaConexion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
      DEFINE VARIABLE   hconn AS COM-HANDLE.


      hFunctions = connectTosap().

      hconn = hFunctions:connection.
      
      MESSAGE  valid-handle( hconn) VIEW-AS ALERT-BOX.

      hFunctions:connection:logoff().

      MESSAGE  valid-handle(hConn) VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pruebaOrigen) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pruebaOrigen Procedure 
PROCEDURE pruebaOrigen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
MESSAGE 'f' SOURCE-PROCEDURE:FILE-NAME 
        'n' source-procedure:NAME 
    MATCHES '*pruebaorigen*' VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-regrabaGln) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE regrabaGln Procedure 
PROCEDURE regrabaGln :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER  iSucTrabajo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER  iPallet AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER iRetcode AS INTEGER NO-UNDO.

  
  
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.

  DEFINE VARIABLE   hDocu       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE   hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRow        AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRows       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hparametros AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR iCodigo AS INTEGER NO-UNDO.
  DEFINE VAR cCentro AS CHARACTER NO-UNDO.





  DEFINE VARIABLE inum AS INTEGER NO-UNDO.
  DEFINE VARIABLE cestado AS CHARACTER NO-UNDO.
  DEFINE VARIABLE K AS INTEGER NO-UNDO.

  DEFINE VAR par AS INTEGER INITIAL 0.

  DEFINE VARIABLE   hPalletId       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hGln            AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hRetcode        AS COM-HANDLE NO-UNDO.

  DEFINE VARIABLE   igln            AS INT64   NO-UNDO.

  DEFINE BUFFER bitems FOR items_pallets.

  iretcode = 1000.


  hFunctions = connectToSap().


  IF VALID-HANDLE(hFunctions) THEN 
  DO:
      
      hFunc         = hFunctions:ADD('ZREGRABAGLN').

      
      hpalletid      = hFunc:exports('P_NRO_PALLET_GESSI').
      hgln           = hFUnc:EXPORTS('P_GLN').
      hRETCODE       = hFUnc:imports('RETCODE').



    FIND pallets WHERE pallets.id_suc_trabajo = ISucTrabajo AND
                         pallets.id_pallet = iPallet NO-LOCK NO-ERROR.

    IF AVAILABLE pallets  THEN DO ON ERROR UNDO , RETURN ERROR "Error en proceso de pallets":
          hpalletid:VALUE = STRING(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet,'9999999999').
          igln = pallets.gln.
          hgln:VALUE      = STRING(igln, '9999999999999').
          IF hFunc:CALL() THEN  
              IRETCODE = HRETCODE:VALUE().
    END.  
  END.
    
  hFunctions:connection:logoff().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-regrabaPalletProgress) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE regrabaPalletProgress Procedure 
PROCEDURE regrabaPalletProgress :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iSuc AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iPallet AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER iEstado AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER cpallet AS CHARACTER NO-UNDO.

  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPallet     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hEstado     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPalletSap     AS COM-HANDLE NO-UNDO.

  hFunctions = connectToSap().

  IF VALID-HANDLE(hFunctions) THEN DO:
      FIND pallets WHERE pallets.id_suc_trabajo = iSuc AND
                         pallets.id_pallet = ipallet NO-ERROR.
      
      hFunc         = hFunctions:ADD('BAPI_GET_DATOS_PALLET').
      hPallet       = hFUnc:exports('IP_PALLET').
      hEstado       = hFUnc:imports('O_STATUS').
      hPalletSap    = hFUnc:imports('O_PALLET').
      hPallet:VALUE = STRING(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet,'9999999999').
      
      IF NOT hFunc:CALL THEN DO:
          hFunctions:connection:logoff().
          RETURN ERROR.
      END.
      
      iEstado = INTEGER(hEstado:VALUE).
      cpallet = hPalletSap:VALUE.
      
      hFunctions:connection:logoff().
  END.
  ELSE MESSAGE 'ERROR Connect' VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-regrabaPalletProgressPrueba) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE regrabaPalletProgressPrueba Procedure 
PROCEDURE regrabaPalletProgressPrueba :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iSuc AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iPallet AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER iEstado AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER cpallet AS CHARACTER NO-UNDO.

  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPallet     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hEstado     AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hPalletSap     AS COM-HANDLE NO-UNDO.

  hFunctions = connectToSapCalidad().

  IF VALID-HANDLE(hFunctions) THEN DO:
      FIND pallets WHERE pallets.id_suc_trabajo = iSuc AND
                         pallets.id_pallet = ipallet NO-ERROR.
      
      hFunc         = hFunctions:ADD('BAPI_GET_DATOS_PALLET').
      hPallet       = hFUnc:exports('IP_PALLET').
      hEstado       = hFUnc:imports('O_STATUS').
      hPalletSap    = hFUnc:imports('O_PALLET').
      hPallet:VALUE = STRING(pallets.id_suc_trabajo * 100000000 + pallets.id_pallet,'9999999999').
      
      IF NOT hFunc:CALL() THEN DO:
          hFunctions:connection:logoff().
          RETURN ERROR.
      END.
      
      iEstado = INTEGER(hEstado:VALUE).
      cpallet = hPalletSap:VALUE.
      
      hFunctions:connection:logoff().
  END.
  ELSE MESSAGE 'ERROR Connect' VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sapToAlmacenes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sapToAlmacenes Procedure 
PROCEDURE sapToAlmacenes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VAR        hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR cCodigo AS CHARACTER NO-UNDO.



  
  hFunctions = connectToSap().

  IF VALID-HANDLE(hFunctions) THEN DO:

      hFunc     = hFunctions:ADD('BAPI_GET_SUCURSALES').
      hTable    = hFunc:tables('SUCU').
      hNum      = hfunc:imports('O_RECNUM').

      IF hFunc:CALL() THEN DO:

        i = 0.
        FOR EACH tablas_sap WHERE tablas_sap.tabla = 'Almacenes'.
            DELETE tablas_sap.
            I = i + 1.
        END.

        j = INTEGER(hNum:VALUE).

        DO i = 1 TO j:
           cCodigo = hTable:VALUE(i,2) + hTable:VALUE(i,3).
           IF ccodigo = '' OR cCodigo = ? OR hTable:VALUE(i,4) = '' OR hTable:VALUE(i,4) = ? THEN
               NEXT.

           FIND FIRST tablas_sap WHERE tablas_sap.tabla = 'Almacenes' AND
                                       tablas_sap.codigo = cCodigo NO-ERROR.

           IF NOT AVAILABLE tablas_sap THEN DO:
               CREATE tablas_sap.
               ASSIGN tablas_sap.tabla = 'Almacenes'
                      tablas_sap.codigo = cCodigo.
           END.
           ASSIGN tablas_sap.descripcion = hTable:VALUE(i,4)
                  tablas_sap.abreviatura = hTable:VALUE(i,4).
        END.
      END.
      
      hFunctions:connection:logoff().
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sapToBalanzaPesada) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sapToBalanzaPesada Procedure 
PROCEDURE sapToBalanzaPesada :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT     PARAMETER hTablePesada      AS COM-HANDLE NO-UNDO.
DEFINE INPUT-OUTPUT     PARAMETER hTableMovFlete    AS COM-HANDLE NO-UNDO.
DEFINE PARAMETER        BUFFER balanza_pesada       FOR balanza_pesada.
DEFINE OUTPUT PARAMETER pobalanza                   AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER popesada                    AS INTEGER NO-UNDO.

DEFINE VAR hProd            AS HANDLE NO-UNDO.
DEFINE VAR vfecha           AS DATE NO-UNDO.
DEFINE VAR vfechaOperativa  AS DATE NO-UNDO.
DEFINE VAR vhora            AS DATETIME NO-UNDO.
DEFINE VAR vfechaEntrada    AS DATE NO-UNDO.
DEFINE VAR vfechaSalida     AS DATE NO-UNDO.
DEFINE VAR vHoraEntrada     AS CHARACTER NO-UNDO.
DEFINE VAR vHoraSalida      AS CHARACTER NO-UNDO.
DEFINE VAR vPesoEntrada     AS DECIMAL  NO-UNDO.
DEFINE VAR vTransporte      AS CHARACTER NO-UNDO.
DEFINE VAR vProveedor       AS INTEGER NO-UNDO.
DEFINE VAR vtipotransporte  AS INTEGER NO-UNDO.
DEFINE VAR vmarca           AS CHARACTER NO-UNDO.
DEFINE VAR vmodelo          AS INTEGER NO-UNDO.
DEFINE VAR vnromov          AS INTEGER NO-UNDO.
DEFINE VAR vpesoneto        AS DECIMAL NO-UNDO DECIMALS 3.
DEFINE VAR vpesodescarte    AS DECIMAL NO-UNDO DECIMALS 3.
DEFINE VAR vpesosalida      AS DECIMAL NO-UNDO DECIMALS 3.
DEFINE VAR vtara            AS DECIMAL NO-UNDO DECIMALS 3.
DEFINE VAR vsucursal        AS INTEGER NO-UNDO.
DEFINE VAR vsucplaya        AS INTEGER NO-UNDO.
DEFINE VAR vtipomov         AS INTEGER NO-UNDO.
DEFINE VAR vsucorigen       AS INTEGER NO-UNDO.
DEFINE VAR vpatente         AS CHARACTER NO-UNDO.
DEFINE VAR xenvase          AS CHARACTER NO-UNDO.
DEFINE VAR vpesadasap       AS INTEGER NO-UNDO.
DEFINE VAR cError           AS CHARACTER NO-UNDO.
DEFINE VAR vocarga          AS CHARACTER NO-UNDO.





DO TRANSACTION ON ERROR UNDO , RETURN cError + ERROR-STATUS:GET-MESSAGE(1).
    {persistentprocedure.i libProduccion.p hProd}.

    vocarga = hTableMovFlete:VALUE(1,2).
    vfecha = DATE (REPLACE(hTablePesada:VALUE(1,4),".","/")).
    vfechaOperativa =  dynamic-function("devuelvefechaoperativa" IN hProd, hTablePesada:VALUE(1,5)).
    
    IF vfechaOperativa = ? THEN
      RETURN "No existe Fecha Operativa habilitada".
    
    pobalanza = htablePesada:VALUE(1,13).
    popesada  = htablePesada:VALUE(1,2).
    
    
    RUN valoresfijos  IN hProd (    INPUT pobalanza,     
                                    OUTPUT vsucursal,
                                    OUTPUT vsucplaya,
                                    OUTPUT vtipomov,
                                    OUTPUT vsucorigen).

    vfechaEntrada = vfecha.
    vfechaSalida  = hTablePesada:VALUE(1,7).
    vhoraEntrada  = hTablePEsada:VALUE(1,5).
    vHoraSalida   = hTablePesada:VALUE(1,8).
    vPesoEntrada  = hTablePesada:VALUE(1,3). 
    vPesoSalida   = hTablePesada:VALUE(1,6). 
    
    vTransporte   = hTableMovFlete:VALUE(1,6).
    vProveedor    = htableMovFlete:VALUE(1,7).

    IF vTransporte <> "" THEN
    DO:
     
        FIND FIRST transportes_proveedor WHERE 
           transportes_proveedor.id_transporte_sap = vTransporte NO-LOCK NO-ERROR.
        IF NOT AVAILABLE transportes_proveedor THEN RETURN "Transporte no valido".
    
        ASSIGN      vtipotransporte = transportes_proveedor.id_tipo_transporte
                    vmarca          = transportes_proveedor.marca
                    vmodelo         = transportes_proveedor.modelo
                    vpatente        = transportes_proveedor.patente.
        
        find last mov_transp_cabecera no-lock no-error.
        if available mov_transp_cabecera then
            assign vnromov = mov_transp_cabecera.nromov + 1.
        else
            assign vnromov = 1.
    END.
    ELSE
    DO:
       FIND FIRST r_prov_activ WHERE r_prov_activ.id_proveedor = vproveedor AND
           r_prov_activ.id_actividad = 3 NO-LOCK NO-ERROR.
       IF NOT AVAILABLE r_prov_activ THEN RETURN "Debe ingresar un transportista valido".
    END.

    ASSIGN vpesoneto = hTablePesada:VALUE(1,12).
    ASSIGN vpesodescarte = hTablePesada:VALUE(1,11).

    RUN crearmovtransportedebalanza IN hProd (  INPUT pobalanza, INPUT popesada). 
    RUN crearmovsucudebalanza       IN hProd (  INPUT pobalanza, INPUT popesada,
                                                INPUT vsucplaya, INPUT vtipomov,
                                                INPUT vsucorigen). 

    xenvase = hTableMovFlete:VALUE (1,54).
    FIND FIRST envases_prod WHERE envases_prod.id_envase_sap = xenvase NO-LOCK NO-ERROR.
    vpesadasap = hTableMovFlete:VALUE(1,74).
    
    CREATE balanza_pesadas.
    ASSIGN
        balanza_pesadas.fecha_entrada       = vfechaentrada
        balanza_pesadas.fecha_operativa     = vfechaoperativa
        balanza_pesadas.fecha_salida        = vfechasalida
        balanza_pesadas.hora_entrada        = vhoraentrada
        balanza_pesadas.hora_salida         = vhorasalida
        balanza_pesadas.id_balanza          = pobalanza
        balanza_pesadas.id_pesada           = popesada
    /*    balanza_pesadas.id_pesada_ctf       = vpesadactf  */
        balanza_pesadas.id_pesada_sap       = string(vpesadasap)   
        balanza_pesadas.id_proveedor        = vproveedor
        balanza_pesadas.id_sucursal         = vsucursal
        balanza_pesadas.id_tipo_movsto      = vtipomov
        balanza_pesadas.id_tipo_transporte  = vtipotransporte
        balanza_pesadas.id_transporte       = integer(vtransporte)
        balanza_pesadas.marca               = vmarca
        balanza_pesadas.modelo              = vmodelo
    /*    balanza_pesadas.nro                 = vnro */
        balanza_pesadas.nromov              = vnromov
    /*    balanza_pesadas.nromov_0            = vnormov0 */
        balanza_pesadas.orden_carga_sap     = vocarga
        balanza_pesadas.patente             = vpatente
        balanza_pesadas.peso_descarte       = vpesodescarte
        balanza_pesadas.peso_entrada        = vpesoentrada
    /*    balanza_pesadas.peso_envases_entrada = vpesoenvasesentrada
        balanza_pesadas.peso_envases_salida  = vpesoenvasessalida */
        balanza_pesadas.peso_neto            = vpesoneto
        balanza_pesadas.peso_salida          = vpesosalida
        balanza_pesadas.tara                 = vtara.
    CATCH err AS PROGRESS.lang.ERROR.
        UNDO , RETURN ERROR cError + ERROR-STATUS:GET-MESSAGE(1).
    END CATCH.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sapToBalanzas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sapToBalanzas Procedure 
PROCEDURE sapToBalanzas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR        hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR iCodigo AS INTEGER NO-UNDO.
  DEFINE VAR cCentro AS CHARACTER NO-UNDO.


  
  hFunctions = connectToSap().

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc     = hFunctions:ADD('BAPI_GET_BALANZAS').
      hTable    = hFunc:tables('BALA').
      hNum      = hfunc:imports('O_RECNUM').

      IF hFunc:CALL() THEN DO:

        FOR EACH ZTMM_BALANZAS.
            DELETE ZTMM_BALANZAS.
        END.

        j = INTEGER(hNum:VALUE).

        DO i = 1 TO j:

           iCodigo = hTable:VALUE(i,3).
           cCentro = hTable:VALUE(i,2).

           IF iCodigo = ? OR hTable:VALUE(i,4) = '' OR hTable:VALUE(i,4) = ? THEN
               NEXT.

           FIND FIRST ZTMM_BALANZAS WHERE ZTMM_BALANZAS.centro = cCentro AND
                                       ZTMM_BALANZAS.codigo = iCodigo NO-ERROR.

           IF NOT AVAILABLE tablas_sap THEN DO:
               CREATE ZTMM_BALANZAS.
               ASSIGN ZTMM_BALANZAS.centro = cCentro
                      ZTMM_BALANZAS.codigo = iCodigo.
           END.
           ASSIGN ZTMM_BALANZAS.descripcion = hTable:VALUE(i,4).
        END.
      END.
      
      hFunctions:connection:logoff().
  END.
  ELSE DO:
      MESSAGE 'error de conexion' VIEW-AS ALERT-BOX.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sapToCalidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sapToCalidad Procedure 
PROCEDURE sapToCalidad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VAR        hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR cCodigo AS CHARACTER NO-UNDO.


  hFunctions = connectToSap().

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc     = hFunctions:ADD('BAPI_GET_CAlIDADES').
      hTable    = hFunc:tables('CALI').
      hNum      = hfunc:imports('O_RECNUM').

      IF hFunc:CALL() THEN DO:

        i = 0.
        FOR EACH tablas_sap WHERE tablas_sap.tabla = 'Calidades'.
            DELETE tablas_sap.
            I = i + 1.
        END.

        j = INTEGER(hNum:VALUE).

        DO i = 1 TO j:
           cCodigo = hTable:VALUE(i,3).
           IF ccodigo = '' OR cCodigo = ? OR hTable:VALUE(i,4) = '' OR hTable:VALUE(i,4) = ? THEN
               NEXT.

           FIND FIRST tablas_sap WHERE tablas_sap.tabla = 'Calidades' AND
                                       tablas_sap.codigo = cCodigo NO-ERROR.

           IF NOT AVAILABLE tablas_sap THEN DO:
               CREATE tablas_sap.
               ASSIGN tablas_sap.tabla = 'Calidades'
                      tablas_sap.codigo = cCodigo.
           END.
           ASSIGN tablas_sap.descripcion = hTable:VALUE(i,4)
                  tablas_sap.abreviatura = hTable:VALUE(i,4).
        END.
      END.
      
      hFunctions:connection:logoff().
  END.
  ELSE MESSAGE 'error de conexion' VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sapToCaracteristicas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sapToCaracteristicas Procedure 
PROCEDURE sapToCaracteristicas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR        hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR cCodigo AS CHARACTER NO-UNDO.


  hFunctions = connectToSap().

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc     = hFunctions:ADD('BAPI_GET_CARACTERISTICAS').
      hTable    = hFunc:tables('CARA').
      hNum      = hfunc:imports('O_RECNUM').

      IF hFunc:CALL() THEN DO:

        i = 0.
        FOR EACH tablas_sap WHERE tablas_sap.tabla = 'Caracteristicas'.
            DELETE tablas_sap.
            I = i + 1.
        END.

        j = INTEGER(hNum:VALUE).


        DO i = 1 TO j:


           cCodigo = hTable:VALUE(i,3).

           IF ccodigo = '' OR cCodigo = ? OR hTable:VALUE(i,4) = '' OR hTable:VALUE(i,4) = ? THEN
               NEXT.

           FIND FIRST tablas_sap WHERE tablas_sap.tabla = 'Caracteristicas' AND
                                       tablas_sap.codigo = cCodigo NO-ERROR.

           IF NOT AVAILABLE tablas_sap THEN
           DO:
               CREATE tablas_sap.
               ASSIGN tablas_sap.tabla = 'Caracteristicas'
                      tablas_sap.codigo = cCodigo.
           END.
           ASSIGN tablas_sap.descripcion = hTable:VALUE(i,4)
                  tablas_sap.abreviatura = hTable:VALUE(i,4).
        END.
      END.
      
      hFunctions:connection:logoff().
  END.
  ELSE MESSAGE 'error de conexion' VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sapToCentros) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sapToCentros Procedure 
PROCEDURE sapToCentros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VAR        hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR cCodigo AS CHARACTER NO-UNDO.


  hFunctions = connectToSap().

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc     = hFunctions:ADD('BAPI_GET_CENTROS').
      hTable    = hFunc:tables('CENT').
      hNum      = hfunc:imports('O_RECNUM').

      IF hFunc:CALL() THEN DO:

        i = 0.
        FOR EACH tablas_sap WHERE tablas_sap.tabla = 'Centros'.
            DELETE tablas_sap.
            I = i + 1.
        END.

        j = INTEGER(hNum:VALUE).

        DO i = 1 TO j:
           cCodigo = hTable:VALUE(i,2).
           IF ccodigo = '' OR cCodigo = ? OR hTable:VALUE(i,3) = '' OR hTable:VALUE(i,3) = ? THEN
               NEXT.

           FIND FIRST tablas_sap WHERE tablas_sap.tabla = 'Centros' AND
                                       tablas_sap.codigo = cCodigo NO-ERROR.

           IF NOT AVAILABLE tablas_sap THEN DO:
               CREATE tablas_sap.
               ASSIGN tablas_sap.tabla = 'Centros'
                      tablas_sap.codigo = cCodigo.
           END.
           ASSIGN tablas_sap.descripcion = hTable:VALUE(i,3)
                  tablas_sap.abreviatura = hTable:VALUE(i,9).
        END.
      END.
      
      hFunctions:connection:logoff().
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sapToDestinos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sapToDestinos Procedure 
PROCEDURE sapToDestinos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VAR        hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR cCodigo AS CHARACTER NO-UNDO.


  hFunctions = connectToSap().

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc     = hFunctions:ADD('BAPI_GET_DESTINOS').
      hTable    = hFunc:tables('Dest').
      hNum      = hfunc:imports('O_RECNUM').

      IF hFunc:CALL() THEN DO:

        i = 0.
        FOR EACH tablas_sap WHERE tablas_sap.tabla = 'Destinos'.
            DELETE tablas_sap.
            I = i + 1.
        END.

        j = INTEGER(hNum:VALUE).

        DO i = 1 TO j:
           cCodigo = hTable:VALUE(i,2).
           IF ccodigo = '' OR cCodigo = ? OR hTable:VALUE(i,3) = '' OR hTable:VALUE(i,3) = ? THEN
               NEXT.

           FIND FIRST tablas_sap WHERE tablas_sap.tabla = 'Destinos' AND
                                       tablas_sap.codigo = cCodigo NO-ERROR.

           IF NOT AVAILABLE tablas_sap THEN DO:
               CREATE tablas_sap.
               ASSIGN tablas_sap.tabla = 'Destinos'
                      tablas_sap.codigo = cCodigo.
           END.
           ASSIGN tablas_sap.descripcion = hTable:VALUE(i,3)
                  tablas_sap.abreviatura = hTable:VALUE(i,4).
        END.
      END.
      
      hFunctions:connection:logoff().
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sapToIngresoFrutaOld) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sapToIngresoFrutaOld Procedure 
PROCEDURE sapToIngresoFrutaOld :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pocarga  AS CHARACTER NO-UNDO.

DEFINE VARIABLE   hSAP                          AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTableMovFlete                AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTableMovFleteD               AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTablePesada                  AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTableLote                    AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTableClasificacionLote       AS COM-HANDLE NO-UNDO.
DEFINE VAR        hStatus                       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunctions                    AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc                         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc1                        AS COM-HANDLE NO-UNDO.
DEFINE VAR hoc                                  AS COM-HANDLE NO-UNDO.
DEFINE VAR nm                                   AS CHARACTER NO-UNDO.
DEFINE VAR hProd            AS HANDLE NO-UNDO.
DEFINE VAR i                AS INTEGER NO-UNDO.
DEFINE VAR vposicion        AS INTEGER NO-UNDO.
DEFINE VAR v1               AS INTEGER NO-UNDO.
DEFINE VAR v2               AS INTEGER NO-UNDO.
DEFINE VAR vtipocosecha     AS INTEGER NO-UNDO.
DEFINE VAR vunioneuropea        AS LOGICAL  NO-UNDO.
DEFINE VAR vchina               AS LOGICAL  NO-UNDO.
DEFINE VAR vsucursaletiqueta    AS INTEGER  NO-UNDO.
DEFINE VAR vorigenorigen        AS INTEGER  NO-UNDO.
DEFINE VAR vfechacosecha        AS DATE     NO-UNDO.
DEFINE VAR vfincasenasa         AS INTEGER  NO-UNDO.
DEFINE VAR vlotesenasa          AS INTEGER  NO-UNDO.
DEFINE VAR vcertificado         AS CHARACTER    NO-UNDO.
DEFINE VAR vcodigotrazabilidad  AS CHARACTER    NO-UNDO.
DEFINE VAR vpesonetoticket      AS DECIMAL  NO-UNDO DECIMALS 3.
DEFINE VAR laceptado            AS LOGICAL  NO-UNDO.
DEFINE VAR ctipocosecha         AS CHARACTER NO-UNDO.
DEFINE VAR vcertunion           AS CHARACTER NO-UNDO.
DEFINE VAR vcertchina           AS CHARACTER NO-UNDO.
DEFINE VAR ctiposervicio        AS CHARACTER NO-UNDO.
DEFINE VAR cenvase              AS CHARACTER NO-UNDO.
DEFINE VAR ccolor               AS CHARACTER NO-UNDO.
DEFINE VAR clote                AS CHARACTER NO-UNDO.
DEFINE VAR cproveedor           AS CHARACTER NO-UNDO.
DEFINE VAR cmateriaprima        AS CHARACTER NO-UNDO.
DEFINE VAR corigensap           AS CHARACTER NO-UNDO.
DEFINE VAR ccodigotrazabilidad  AS CHARACTER NO-UNDO.
DEFINE VAR vtiposervicio        AS INTEGER NO-UNDO.
DEFINE VAR venvase              AS INTEGER NO-UNDO.
DEFINE VAR cmercado             AS CHARACTER NO-UNDO.
DEFINE VAR vcolor               AS INTEGER NO-UNDO.
DEFINE VAR iarticulo            AS INTEGER NO-UNDO.
DEFINE VAR vpesadasap           AS INTEGER NO-UNDO.
DEFINE VAR vrenspa              AS CHARACTER NO-UNDO.
DEFINE VAR vordencomprasap      AS CHARACTER NO-UNDO.
DEFINE VAR vposordencomprasap   AS INTEGER NO-UNDO.
DEFINE VAR vpesoenvasesentrada  AS DECIMAL NO-UNDO.
DEFINE VAR vvariedad            AS INTEGER NO-UNDO.
DEFINE VAR vbalanza             AS INTEGER NO-UNDO.
DEFINE VAR vpesada              AS INTEGER NO-UNDO.
DEFINE VAR vsucursal            AS INTEGER NO-UNDO.
DEFINE VAR vsucplaya            AS INTEGER NO-UNDO.
DEFINE VAR vtipomov             AS INTEGER NO-UNDO.
DEFINE VAR vsucorigen           AS INTEGER NO-UNDO.
DEFINE VAR vproveedor           AS INTEGER NO-UNDO.
DEFINE VAR vpesodescarte        AS DECIMAL NO-UNDO DECIMALS 3.
DEFINE VAR vsucursalpacking     AS INTEGER NO-UNDO.
DEFINE VAR vproveedororigen     AS INTEGER NO-UNDO.
DEFINE VAR vorigen              AS INTEGER NO-UNDO.
DEFINE VAR vmateriaprima        AS INTEGER NO-UNDO.
DEFINE VAR vlote                AS CHARACTER NO-UNDO.
DEFINE VAR vetiqueta            AS INTEGER NO-UNDO.
DEFINE VAR vdetalletransporte   AS INTEGER NO-UNDO.
DEFINE VAR vdestinopacking      AS INTEGER NO-UNDO.
DEFINE VAR vdescarte            AS DECIMAL NO-UNDO.
DEFINE VAR vcalidadbalanza      AS INTEGER NO-UNDO.
DEFINE VAR vhorasalida          AS CHARACTER NO-UNDO.
DEFINE VAR vhoraentrada         AS CHARACTER NO-UNDO.
DEFINE VARIABLE vfinca          AS LOGICAL NO-UNDO.
DEFINE VAR vfechasalida         AS DATE NO-UNDO.
DEFINE VAR vfecharemito         AS DATE NO-UNDO.
DEFINE VAR vfechaoperativa      AS DATE NO-UNDO.
DEFINE VAR vfechaentrada        AS DATE NO-UNDO.
DEFINE VAR vcodbarrasap         AS CHARACTER NO-UNDO.
DEFINE VAR vcantenvasesentrada  AS DECIMAL NO-UNDO.
DEFINE VARIABLE pcode           AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE VARIABLE porden          AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE pposic          AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE perror          AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE ptexto          AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE ptipoe          AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE VARIABLE cerror          AS CHARACTER NO-UNDO INITIAL 'Error de interface de ingreso de fruta'.


DEFINE BUFFER aux_items FOR items_stock.

DO:
{persistentprocedure.i libp hProd}

TEMP-TABLE TTCLASIFICACION:CLEAR().

nm = SESSION:NUMERIC-FORMAT.
SESSION:NUMERIC-FORMAT = 'European'.
hFunctions = connectToSapCalidad ().

IF VALID-HANDLE(hFunctions) THEN DO:

    hFunc                     = hFunctions:ADD('BAPI_GET_INGRESO_FRUTA').
    hoc                       = hFunc:exports('OCARGA').
    hStatus                   = hFunc:imports('STATUS').
    hTableMovFlete            = hFunc:tables('TOMOVFLETE').
    hTableMovFleteD           = hFunc:tables('TOMOVFLETED').
    hTablePesada              = hFunc:tables('TOPESADA').

    hoc:VALUE = trim(pocarga).
  
    IF hFunc:CALL() THEN DO:
        
        IF hStatus:VALUE <> 0 THEN UNDO , RETURN ERROR 'Error en la bapi de ingreso de frutas'.
    
        RUN sapToBalanzaPesada (INPUT-OUTPUT hTablePesada , INPUT-OUTPUT hTableMovFlete , BUFFER balanza_pesadas , OUTPUT vbalanza , OUTPUT vpesada ) NO-ERROR.
        IF ERROR-STATUS:ERROR  THEN
            UNDO , RETURN ERROR 'Error de funcion de interface: BalanzaPesada - ' + ERROR-STATUS:GET-MESSAGE(1). 
        
    
        RUN valoresfijos  IN hProd (    INPUT  vbalanza,     
                                        OUTPUT vsucursal,
                                        OUTPUT vsucplaya,
                                        OUTPUT vtipomov,
                                        OUTPUT vsucorigen).
        I = 0.
        REPEAT:
            i = i + 1.
            vposicion = hTableMovFleteD:VALUE(i,3) NO-ERROR.
            IF vposicion = ? THEN LEAVE.
            /******* VERIFICO QUE EXISTA SALDO DE BINES PARA ESE REMITO ****/
            /*****
            run saldoBines in hcontainer (input rowobjupd.nro_remito , output isaldo).
            if isaldo < rowobjupd.cant_env_entrada then
               RETURN "No existe ese saldo de bines para ese remito".
            ***/
            /******* VERIFICA QUE EXISTA SALDO EN LA ESTIMACION DE COSECHA *******/
    
            FIND r_envases_prod WHERE r_envases_prod.id_envase = envases_prod.id_envase AND
                                      r_envases_prod.id_articulo = productos_terminados.id_articulo NO-LOCK NO-ERROR.
            
            RUN devuelvevaloresMovflete (htableMovFleteD , i, vsucursal ,
                                         OUTPUT cmateriaprima , 
                                         OUTPUT vcantenvasesentrada , 
                                         OUTPUT vpesoenvasesentrada , 
                                         OUTPUT ctipocosecha , 
                                         OUTPUT ctiposervicio , 
                                         OUTPUT cenvase , 
                                         OUTPUT corigensap , 
                                         OUTPUT ccodigotrazabilidad , 
                                         OUTPUT vcolor,
                                         OUTPUT cmercado ,
                                         OUTPUT vpesonetoticket , 
                                         OUTPUT vunioneuropea ,
                                         OUTPUT vchina ,
                                         OUTPUT vfinca ,
                                         OUTPUT vsucursaletiqueta,
                                         OUTPUT vsucursalpacking,
                                         OUTPUT vproveedor,
                                         OUTPUT vproveedororigen,
                                         OUTPUT vorigen,
                                         OUTPUT vorigenorigen,
                                         OUTPUT vtipocosecha,
                                         OUTPUT vcalidadbalanza,
                                         OUTPUT vtiposervicio,
                                         OUTPUT venvase,
                                         OUTPUT vlotesenasa,
                                         OUTPUT vlote,
                                         OUTPUT vfincasenasa,
                                         OUTPUT vcertunion,
                                         OUTPUT vcertchina,
                                         OUTPUT vcertificado,
                                         OUTPUT v1,
                                         OUTPUT v2,
                                         OUTPUT iarticulo).

            RUN getCaracter¡sticas (hFunc1 ,hTableMovFleteD:VALUE(i,21) , hTableMovFleteD:VALUE(i,13), cmateriaprima,  INPUT-OUTPUT TABLE ttclasificacion ) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
            DO:
                UNDO , RETURN 'Error as obtener caracter¡sticas ' + RETURN-VALUE.
            END.
    
            FIND FIRST   ttclasificacion WHERE ttclasificacion.caracteristica = "NRO_CERTIFICADO_1" NO-ERROR.
            IF AVAILABLE ttclasificacion THEN vcertificado = ttclasificacion.valor. ELSE
                                              vcertificado = "".

            FIND FIRST   ttclasificacion WHERE ttclasificacion.caracteristica = "COLOR" NO-ERROR.
            IF AVAILABLE ttclasificacion THEN ccolor = ttclasificacion.valor. ELSE ccolor = "".
            
            FIND productos_terminados WHERE productos_terminados.id_articulo = iarticulo NO-LOCK NO-ERROR.
            FIND FIRST periodo_cosecha NO-LOCK.
            
            IF  vtipocosecha = 1 and
                cmercado = 'UE' and
                vsucursaletiqueta <> v1 and
                vorigenorigen <> v2 then do:
    
                run dd_valsdoest.p
                    (input vfechacosecha,
                    input vproveedor,
                    input vfincasenasa,
                    input vlotesenasa,
                    input vcertificado,
                    input vcodigotrazabilidad,
                    input vpesonetoticket,
                    OUTPUT laceptado).
                IF NOT laceptado THEN
                    UNDO , RETURN ERROR RETURN-VALUE.
            end.
            
            
            FIND FIRST VARIEDADES OF LOTE NO-LOCK NO-ERROR.
                
             /* Asigno etiqueta */
            find last aux_items use-index etiqueta where
                aux_items.id_sucursal_etiqueta = vsucursaletiqueta no-lock no-error.
            if available aux_items then
                vetiqueta = aux_items.id_etiqueta + 1.
            else
                vetiqueta = 1.

            create balanza_ticket.
            assign 
                balanza_tickets.union_europea  = vunioneuropea
                balanza_tickets.renspa                  = origenes.renspa
                balanza_tickets.zona_up                 = origenes.zona_up
                balanza_tickets.orden_compra_sap        = vordencomprasap /*** completo con validacion ***/
                balanza_tickets.pos_orden_compra_sap    = string(vposordencomprasap)
                balanza_tickets.peso_neto_ticket        = vpesonetoticket
                balanza_tickets.peso_envases_entrada    = vpesoenvasesentrada
                balanza_tickets.peso_descarte           = vpesodescarte
                balanza_tickets.periodo_cosecha         = periodo_cosecha.periodo_cosecha
    /*            balanza_tickets.nro_ticket_ctf       = vnroticketctf */
                balanza_tickets.nro_ticket              = vpesada
                balanza_tickets.nro_remito              = hTableMovFleteD:VALUE(i,11)
                balanza_tickets.nro_partida_serial      = 1
                balanza_tickets.nro_partida             = NEXT-VALUE(nro_partida, produccion)
                balanza_tickets.nro_partida_general     = balanza_tickets.nro_partida
                balanza_tickets.id_variedad             = VARIEDADES.ID_VARIEDAD WHEN AVAILABLE VARIEDADES
                balanza_tickets.id_tipo_servicio        = vtiposervicio
                balanza_tickets.id_tipo_cosecha         = vtipocosecha
    /*            balanza_tickets.id_ticket_quincena   = vticketquincena */
                balanza_tickets.id_sucursal_packing     = vsucursalpacking
                balanza_tickets.id_sucursal_etiqueta    = vsucursaletiqueta
                balanza_tickets.id_sucursal             = vsucursal
                balanza_tickets.id_proveedor_origen     = vproveedororigen
                balanza_tickets.id_proveedor            = vproveedor
    /*            balanza_tickets.id_pesada_ctf        = vpesadactf */
                balanza_tickets.id_pesada               = vpesada
                balanza_tickets.id_origen_origen        = vorigenorigen
                balanza_tickets.id_origen               = vorigen
                balanza_tickets.id_materia_prima        = vmateriaprima
                balanza_tickets.id_lote_senasa          = IF vunioneuropea  THEN vlotesenasa ELSE ''
                balanza_tickets.id_lote                 = lote.id_lote WHEN AVAILABLE lote
                balanza_tickets.id_finca_senasa         = IF vunioneuropea THEN  vfincasenasa ELSE ''
                balanza_tickets.id_etiqueta             = vetiqueta
                balanza_tickets.id_envase               = venvase
                balanza_tickets.id_detalle_transporte   = vdetalletransporte
                balanza_tickets.id_destino_packing      = vdestinopacking
                balanza_tickets.id_descarte             = vdescarte
                balanza_tickets.id_color                = vcolor
                balanza_tickets.id_calidad_balanza      = vcalidadbalanza
                balanza_tickets.id_balanza              = vbalanza
                balanza_tickets.hora_salida             = balanza_pesada.hora_salida
                balanza_tickets.hora_entrada            = balanza_pesada.hora_entrada
                balanza_tickets.finca                   = vfinca
                balanza_tickets.fecha_salida            = balanza_pesada.fecha_salida
                balanza_tickets.fecha_remito            = DATE(STRING(htableMovfleteD:VALUE(i,35)))
                balanza_tickets.fecha_operativa         = balanza_pesada.fecha_operativa
                balanza_tickets.fecha_entrada           = balanza_pesada.fecha_entrada
                balanza_tickets.fecha_cosecha           = DATE(STRING(htableMovfleteD:VALUE(i,5)))
                balanza_tickets.cod_barra_sap           = trim(cmateriaprima) + trim(vlote)
                balanza_tickets.codigo_trazabilidad     = vcodigotrazabilidad
                balanza_tickets.china                   = vchina
                balanza_tickets.cert_china              = vcertchina
                balanza_tickets.certificado             = vcertificado
                balanza_tickets.cant_env_entrada        = vcantenvasesentrada.
                
                RUN ordenCompraSap 
                   (INPUT  balanza_tickets.id_balanza,
                    INPUT  balanza_tickets.id_pesada,
                    INPUT  balanza_tickets.nro_ticket,
                    OUTPUT balanza_tickets.orden_compra_sap,
                    OUTPUT balanza_tickets.pos_orden_compra_Sap,
                    OUTPUT perror,
                    OUTPUT ptexto,
                    OUTPUT ptipoe) NO-ERROR.
                
                RUN procesoBalanzaTicket (BUFFER balanza_tickets) NO-ERROR.

        END.
        SESSION:NUMERIC-FORMAT = nm.
    END. 
    ELSE UNDO , RETURN ERROR 'Error en funcion de interfase de ingreso de Frutas'.
END.
ELSE  UNDO , RETURN ERROR 'Error de comunicac¢n  con SAP'.
END.

RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sapToLugDes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sapToLugDes Procedure 
PROCEDURE sapToLugDes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VAR        hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR cCodigo AS CHARACTER NO-UNDO.


  hFunctions = connectToSap().

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc     = hFunctions:ADD('BAPI_GET_LUGDES').
      hTable    = hFunc:tables('LUGD').
      hNum      = hfunc:imports('O_RECNUM').

      IF hFunc:CALL() THEN DO:

        i = 0.
        FOR EACH tablas_sap WHERE tablas_sap.tabla = 'LugDes'.
            DELETE tablas_sap.
            I = i + 1.
        END.

        j = INTEGER(hNum:VALUE).

        DO i = 1 TO j:
           cCodigo = hTable:VALUE(i,2).
           IF ccodigo = '' OR cCodigo = ? OR hTable:VALUE(i,3) = '' OR hTable:VALUE(i,3) = ? THEN
               NEXT.

           FIND FIRST tablas_sap WHERE tablas_sap.tabla = 'LugDes' AND
                                       tablas_sap.codigo = cCodigo NO-ERROR.

           IF NOT AVAILABLE tablas_sap THEN
           DO:
               CREATE tablas_sap.
               ASSIGN tablas_sap.tabla = 'LugDes'
                      tablas_sap.codigo = cCodigo.
           END.
           ASSIGN tablas_sap.descripcion = hTable:VALUE(i,3)
                  tablas_sap.abreviatura = hTable:VALUE(i,3).
        END.
      END.
      
      hFunctions:connection:logoff().
  END.
  ELSE MESSAGE 'error de conexion' VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sapToMaterial) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sapToMaterial Procedure 
PROCEDURE sapToMaterial :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR        hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR cCodigo AS CHARACTER NO-UNDO.


  hFunctions = connectToSap().

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc     = hFunctions:ADD('BAPI_GET_MATERIAL').
      hTable    = hFunc:tables('MATE').
      hNum      = hfunc:imports('O_RECNUM').

      IF hFunc:CALL() THEN DO:

        FOR EACH productos_sap. DELETE productos_sap. END.

        j = INTEGER(hNum:VALUE).
        DO i = 1 TO j:
           cCodigo = hTable:VALUE(i,2).
           IF ccodigo = '' OR cCodigo = ? OR hTable:VALUE(i,4) = '' OR hTable:VALUE(i,4) = ? THEN
               NEXT.

           DO TRANSACTION ON ERROR UNDO, LEAVE:
               FIND productos_sap WHERE productos_sap.id_producto_sap = cCodigo  NO-ERROR.

               IF NOT AVAILABLE tablas_sap THEN DO:
                   CREATE productos_sap.
                   ASSIGN productos_sap.id_producto_sap = cCodigo.
               END.
               ASSIGN productos_sap.descripcion_sap = hTable:VALUE(i,4).

               IF cCodigo >= 'A' AND cCodigo <= 'Z' AND num-entries(cCodigo,'-') = 4 THEN DO:
                   productos_sap.id_especie_sap         = SUBSTRING(cCodigo,1,1).
                   productos_sap.id_Variedad_sap        = INTEGER(SUBSTRING(cCodigo,2,2)).
                   productos_sap.id_Envase_sap          = INTEGER(SUBSTRING(cCodigo,5,3)).
                   productos_sap.id_Marca_sap           = INTEGER(SUBSTRING(cCodigo,9,3)).
                   productos_sap.paletizado             = INTEGER(SUBSTRING(cCodigo,13,3)).

                   FIND FIRST productos_terminados WHERE productos_terminados.id_articulo_sap = productos_sap.id_especie_sap NO-LOCK NO-ERROR.
                   FIND FIRST variedad WHERE variedad.id_variedad_sap = string(productos_sap.id_variedad_sap) AND 
                                             variedad.id_articulo_sap = productos_sap.id_especie_sap NO-LOCK NO-ERROR.
                   FIND FIRST envases_prod WHERE envases_prod.id_envase_sap = string(productos_sap.id_envase_sap) NO-LOCK NO-ERROR.
                   FIND FIRST marcas_prod WHERE marcas_prod.id_marca_sap = string(productos_sap.id_marca_sap) NO-LOCK NO-ERROR.

                   productos_sap.id_articulo    = IF AVAILABLE productos_terminados THEN productos_terminados.id_articulo ELSE 999.
                   productos_sap.id_variedad    = IF AVAILABLE variedad THEN variedad.id_variedad ELSE 999. 
                   productos_sap.id_envase      = IF AVAILABLE envases_prod THEN envases_prod.id_envase ELSE 999.
                   productos_sap.id_marca       = IF AVAILABLE marcas_prod THEN marcas_prod.id_marca ELSE 999.
               END.
               RELEASE productos_sap.
           END.
        END.
      END.
      
      hFunctions:connection:logoff().
  END.
  ELSE MESSAGE 'error de conexion' VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sapToP) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sapToP Procedure 
PROCEDURE sapToP :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pDesde   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pHasta   AS CHARACTER NO-UNDO.


DEFINE VARIABLE   hSAP        AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTable      AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTable1      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc1       AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.
DEFINE VARIABLE iAnio AS INTEGER NO-UNDO.

define variable hD1 as com-handle NO-UNDO.
define variable hD2 as com-handle no-undo.
DEFINE VAR hOE AS COM-HANDLE NO-UNDO.
DEFINE VAR cV AS CHARACTER NO-UNDO.
DEFINE VAR cV1 AS CHARACTER NO-UNDO.

DEFINE VAR k AS INTEGER NO-UNDO.
DEFINE VAR cVar AS CHARACTER NO-UNDO FORMAT 'x(1024)'.
DEFINE VAR ccVar AS CHARACTER NO-UNDO FORMAT 'x(20)'.


DEFINE VAR cVapor AS CHARACTER NO-UNDO.
DEFINE VAR iLug AS INTEGER NO-UNDO.
DEFINE VAR iPuerto AS INTEGER NO-UNDO.
DEFINE VAR cDestino AS CHARACTER NO-UNDO.
DEFINE VAR iCliente AS INT64 NO-UNDO.
DEFINE VAR cFecha AS CHARACTER NO-UNDO.
DEFINE VAR iPedido AS INTEGER NO-UNDO.
DEFINE VAR iOrden AS INTEGER NO-UNDO.
DEFINE VAR iItem AS INTEGER NO-UNDO.


DEFINE VAR cEspecie AS CHARACTER NO-UNDO.
DEFINE VAR cVariedad AS CHARACTER NO-UNDO.
DEFINE VAR cEnvase AS CHARACTER NO-UNDO.
DEFINE VAR cMarca AS CHARACTER NO-UNDO.
DEFINE VAR iPaletizado AS INTEGER NO-UNDO.
DEFINE VAR cTipoPalet AS CHARACTER NO-UNDO.
DEFINE VAR cTipoEsquinero AS CHARACTER NO-UNDO.
DEFINE VAR cCategoria AS CHARACTER NO-UNDO.
DEFINE VAR iCalidad AS INTEGER NO-UNDO.
DEFINE VAR iTratamiento AS INTEGER NO-UNDO.

DEFINE VAR resp AS LOGICAL NO-UNDO.

DEFINE VAR fdesde AS DATE NO-UNDO.
DEFINE VAR fhasta AS DATE NO-UNDO.


fdesde = DATE(INTEGER(SUBSTRING(pdesde,5,2)) ,  INTEGER(substring(pdesde,7,2)) , INTEGER(SUBSTRING(pdesde,1,4)) ).
fhasta = DATE(INTEGER(SUBSTRING(phasta,5,2)) ,  INTEGER(substring(phasta,7,2)) , INTEGER(SUBSTRING(phasta,1,4)) ).


hFunctions = connectToSap ().

IF VALID-HANDLE(hFunctions) THEN DO:
  
  hFunc     = hFunctions:ADD('BAPI_GET_ORDEN_FABRICACION').
  hD1       = hFunc:exports('DATE_FROM').
  hD2       = hFunc:exports('DATE_TO').
  hTable    = hFunc:tables('ORDENES').

  hD1:VALUE = trim(pDesde).
  hD2:VALUE = trim(pHasta).
  
  IF hFunc:CALL() THEN DO:
      i = 0.

      REPEAT ON ERROR UNDO , LEAVE:

          i = i + 1.
          cV = hTable:VALUE(i,5) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN  LEAVE.
          IF cV = ? THEN   LEAVE.
          
          
          cVapor = hTable:VALUE(i,18).
          iLug = integer(hTable:VALUE(i,19)).
          iPuerto = integer(hTable:VALUE(i,20)).
          cDestino = hTable:VALUE(i,16).
          iOrden =  integer(hTable:value(i,2)).
          iCliente = INT64(HTABLE:VALUE(i,4)).

          FIND FIRST pedidos_packing WHERE  pedidos_packing.id_empresa = 1 AND
                                            pedidos_packing.id_punto_emisor = 1 AND
                                            pedidos_packing.id_orden = iOrden  NO-ERROR.

          IF NOT AVAILABLE pedidos_packing  THEN  NEXT.

            hFunc1    = hFunctions:ADD('BAPI_GET_CALIBRE_OE').
            hOE       = hFunc1:exports('OE').
            hTable1   = hFunc1:tables('CALIBRE').
            hOE:VALUE = string(pedidos_packing.oe,'9999999999').
        
          IF hFunc1:CALL() THEN DO:
              k = 0.

              REPEAT:
                  k = k + 1.
                  cV1 = hTable1:VALUE(k,2) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN  LEAVE.
                  IF cV1 = ? THEN   LEAVE.

                  IF INTEGER(htable1:VALUE(k , 11)) <> iOrden THEN  NEXT.
                      
                  iItem = hTable1:VALUE(k,3) + hTable1:VALUE(k,4).

                  FIND FIRST items_pedidos_packing WHERE  items_pedidos_packing.id_empresa = 1 AND
                                                          items_pedidos_packing.id_punto_emisor = 1 AND
                                                          Items_pedidos_packing.id_orden = iOrden  AND
                                                          items_pedidos_packing.ITEM = iItem NO-ERROR.

                  cEspecie =SUBSTRING(hTable:VALUE(i,7),1,1).
                  
                  icalidad = integer(hTable:VALUE(i,11)).

                  cVariedad      = SUBSTRING(hTable:VALUE(i,7),2,2).
                  cEnvase        = SUBSTRING(hTable:VALUE(i,7),5,3).
                  cMarca         = SUBSTRING(hTable:VALUE(i,7),9,3).
                  iPaletizado    = INTEGER(SUBSTRING(hTable:VALUE(i,7),13,3)).


                  ctipoPalet     = string(hTable:VALUE(i,13)).
                  cTipoEsquinero = string(hTable:VALUE(i,14)).
                  ccategoria     = string(hTable:VALUE(i,12)).
                  iTratamiento   = INTEGER(hTable:VALUE(i,10)).

                  MESSAGE  decimal(hTable1:VALUE(k,9)) * 1000 items_pedidos_packing.id_orden  VIEW-AS ALERT-BOX.
              END.
        END.
      END.
  END.
      
  hFunctions:connection:logoff().
END.

RELEASE OBJECT hFunctions.

RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sapToPEdidosPacking) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sapToPEdidosPacking Procedure 
PROCEDURE sapToPEdidosPacking :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pDesde   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pHasta   AS CHARACTER NO-UNDO.

DEFINE VARIABLE DEBUG AS LOGICAL NO-UNDO.

DEFINE VARIABLE   hSAP       AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTable     AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTable1    AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTablev    AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunctions AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc1     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFuncV     AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE i     AS INTEGER NO-UNDO.
DEFINE VARIABLE j     AS INTEGER NO-UNDO.
DEFINE VARIABLE iAnio AS INTEGER NO-UNDO.

define variable hD1     as com-handle NO-UNDO.
define variable hD2     as com-handle no-undo.
DEFINE VAR hOE          AS COM-HANDLE NO-UNDO.
DEFINE VAR hOEv          AS COM-HANDLE NO-UNDO.
DEFINE VAR cV           AS CHARACTER NO-UNDO.
DEFINE VAR cV1          AS CHARACTER NO-UNDO.

DEFINE VAR k            AS INTEGER NO-UNDO.
DEFINE VAR cVar         AS CHARACTER NO-UNDO FORMAT 'x(1024)'.
DEFINE VAR ccVar        AS CHARACTER NO-UNDO FORMAT 'x(20)'.


DEFINE VAR cVapor   AS CHARACTER NO-UNDO.
DEFINE VAR iLug     AS INTEGER NO-UNDO.
DEFINE VAR iPuerto  AS INTEGER NO-UNDO.
DEFINE VAR cDestino AS CHARACTER NO-UNDO.
DEFINE VAR iCliente AS INT64 NO-UNDO.
DEFINE VAR cFecha   AS CHARACTER NO-UNDO.
DEFINE VAR iPedido  AS INTEGER NO-UNDO.
DEFINE VAR iOrden   AS INTEGER NO-UNDO.
DEFINE VAR iItem    AS INTEGER NO-UNDO.


DEFINE VAR cEspecie     AS CHARACTER NO-UNDO.
DEFINE VAR cVariedad    AS CHARACTER NO-UNDO.
DEFINE VAR cEnvase      AS CHARACTER NO-UNDO.
DEFINE VAR cMarca       AS CHARACTER NO-UNDO.
DEFINE VAR iPaletizado  AS INTEGER NO-UNDO.
DEFINE VAR cTipoPalet       AS CHARACTER NO-UNDO.
DEFINE VAR cTipoEsquinero   AS CHARACTER NO-UNDO.
DEFINE VAR cCategoria       AS CHARACTER NO-UNDO.
DEFINE VAR iCalidad         AS INTEGER NO-UNDO.
DEFINE VAR iTratamiento     AS INTEGER NO-UNDO.


DEFINE VAR fdesde           AS DATE NO-UNDO.
DEFINE VAR fhasta           AS DATE NO-UNDO.
DEFINE VAR nm               AS CHARACTER NO-UNDO.

DEFINE VAR flag-esquinero AS LOGICAL NO-UNDO.
DEFINE VAR msgesquinero AS CHARACTER NO-UNDO.

fdesde = DATE(INTEGER(SUBSTRING(pdesde,5,2)) ,  INTEGER(substring(pdesde,7,2)) , INTEGER(SUBSTRING(pdesde,1,4)) ).
fhasta = DATE(INTEGER(SUBSTRING(phasta,5,2)) ,  INTEGER(substring(phasta,7,2)) , INTEGER(SUBSTRING(phasta,1,4)) ).

nm = SESSION:NUMERIC-FORMAT.

SESSION:NUMERIC-FORMAT = 'European'.


hFunctions = connectToSap().

IF VALID-HANDLE(hFunctions) THEN DO:
  
  FOR EACH pedidos_packing USE-INDEX fecha WHERE pedidos_packing.fecha  >= fDesde AND pedidos_packing.fecha <= fHasta AND 
      id_vapor_usa = 0.
      FIND FIRST cajas WHERE
          cajas.id_empresa = pedidos_packing.id_empresa AND
          cajas.id_punto_emisor = pedidos_packing.id_punto_emisor AND
          cajas.id_orden = pedidos_packing.id_orden NO-LOCK NO-ERROR.
      IF NOT AVAILABLE cajas THEN DO:
          FOR EACH items_pedidos_packing OF pedidos_packing.
              DELETE items_pedidos_packing.
          END.
          DELETE pedidos_packing .
      END.
  END.

  hFunc  = hFunctions:ADD('BAPI_GET_ORDEN_FABRICACION').
  hD1    = hFunc:exports('DATE_FROM').
  hD2    = hFunc:exports('DATE_TO').
  hTable = hFunc:tables('ORDENES').

  hD1:VALUE = trim(pDesde).
  hD2:VALUE = trim(pHasta).
  
  IF hFunc:CALL() THEN DO:
      i = 0.
          
      OUTPUT STREAM ped TO z:/temp/pedidos.txt.

      REPEAT ON ERROR UNDO , LEAVE:

          
          
          i = i + 1.
          cV = hTable:VALUE(i,5) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN  LEAVE.
          IF cV = ? THEN   LEAVE.

          cVapor   = hTable:VALUE(i,18).
          iLug     = integer(hTable:VALUE(i,19)).
          iPuerto  = integer(hTable:VALUE(i,20)).
          cDestino = hTable:VALUE(i,16).
          iOrden   = integer(hTable:value(i,2)).
          iCliente = INT64(HTABLE:VALUE(i,4)).    
          
          IF LENGTH(string(hTable:value(i,23))) > 4 THEN
          DO:
              MESSAGE "Contramarca para la orden "  STRING(iOrden)  SKIP  " Tiene m s de 4 caracteres. No se procesar  ." VIEW-AS ALERT-BOX ERROR.
              NEXT.
          END.

          FIND FIRST clientes_ventas WHERE clientes_ventas.id_cliente_sap = iCliente NO-LOCK NO-ERROR.
          cTipoEsquinero = string(hTable:VALUE(i,14)).                 
          FIND FIRST tipo_esquineros WHERE tipo_esquineros.id_tipo_esquinero_sap = cTipoEsquinero NO-LOCK NO-ERROR.
          IF NOT AVAILABLE clientes_ventas OR NOT AVAILABLE tipo_esquineros THEN
          DO:
            MESSAGE "Cliente o esquinero no definido para la orden "  STRING(iOrden)  SKIP  "No se procesar  ." VIEW-AS ALERT-BOX ERROR.
            NEXT.
          END.

          hFuncV    = hFunctions:ADD('BAPI_GET_CALIBRE_OE').
          hOEV       = hFuncV:exports('OE').
          hTableV   = hFuncV:tables('CALIBRE').
          hOEV:VALUE = INTEGER(hTable:VALUE(i,3)).

          flag-esquinero = FALSE.
          IF hFuncV:CALL() THEN DO:
              k = 0.
              REPEAT:
                  k = k + 1.
                  cV1 = hTableV:VALUE(k,2) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN  LEAVE.
                  IF cV1 = ? THEN   LEAVE.
                
                  IF INTEGER(htableV:VALUE(k , 11)) <> iOrden THEN  NEXT.
                      
                  FIND r_clientes_esquineros WHERE r_clientes_esquineros.id_cliente = clientes_ventas.id_cliente AND
                                                   r_clientes_esquineros.id_tipo_esquinero = tipo_esquineros.id_tipo_esquinero NO-LOCK NO-ERROR.
                  IF  AVAILABLE r_clientes_esquineros AND r_clientes_esquineros.estado THEN
                  DO:
                      MESSAGE "La orden " STRING(iOrden) " del cliente SAP " string(iCliente) " - " Clientes_ventas.razon_social SKIP
                              "tiene pedido un esquinero no autorizado. No se procesar " VIEW-AS ALERT-BOX ERROR.
                      flag-esquinero = TRUE.
                    LEAVE.
                  END.
              END.
          END.
          IF flag-esquinero = TRUE THEN NEXT.

          FIND FIRST pedidos_packing WHERE  pedidos_packing.id_empresa      = 1 AND
                                            pedidos_packing.id_punto_emisor = 1 AND
                                            pedidos_packing.id_orden        = iOrden  NO-ERROR.

          IF AVAILABLE pedidos_packing AND pedidos_packing.id_vapor_usa <> 0 THEN  NEXT.


          IF NOT AVAILABLE pedidos_packing  THEN DO:
              CREATE pedidos_packing.
              ASSIGN 
                  pedidos_packing.id_orden        = iOrden
                  pedidos_packing.id_empresa      = 1
                  pedidos_packing.id_punto_emisor = 1.
          END.

          FIND FIRST vapores WHERE vapores.id_vapor_sap = cVapor NO-LOCK NO-ERROR.
          FIND FIRST lugar_descarga WHERE integer(lugar_descarga.id_lugdes_sap) = iLug NO-LOCK NO-ERROR.
          FIND FIRST puertos WHERE puertos.id_puerto_sap = ipuerto NO-LOCK NO-ERROR.
          FIND FIRST destinos WHERE integer(destinos.id_destino_sap) = integer(cDestino) NO-LOCK NO-ERROR.
          FIND FIRST clientes_ventas WHERE clientes_ventas.id_cliente_sap = iCliente NO-LOCK NO-ERROR.
          

          ASSIGN pedidos_packing.union_europea    = IF hTable:VALUE(i,21) = '' THEN FALSE ELSE TRUE
                 pedidos_packing.id_vapor         = IF AVAILABLE vapores THEN vapores.id_vapor ELSE 999 
                 pedidos_packing.id_puerto_sal    = if available lugar_descarga then lugar_descarga.id_lugdes ELSE 999
                 pedidos_packing.id_pedido_sap    = hTable:value(i,2)
                 pedidos_packing.id_destino_final = if available destinos then destinos.id_destino ELSE 999
                 pedidos_packing.id_cliente       = if available clientes_ventas then clientes_ventas.id_cliente ELSE 999                                                            
                 pedidos_packing.id_mercado       = if available clientes_ventas THEN  clientes_ventas.mercado ELSE 999 
                 pedidos_packing.fecha            = date(string(hTable:VALUE(i,17)))  
                 pedidos_packing.c_usuario        = 'Interface Sap'
                 pedidos_packing.contramarca      = hTable:value(i,23)
                 pedidos_packing.china            = if hTable:value(i,22) = '' then FALSE ELSE TRUE 
                 pedidos_packing.ano              = YEAR(pedidos_packing.fecha)
                 pedidos_packing.oe               = INTEGER(hTable:VALUE(i,3))
                 pedidos_packing.OF_sap           = string(iOrden,'99999999999')
                 pedidos_packing.id_cliente_remito = 999996.
                 pedidos_packing.id_puerto_ent    = pedidos_packing.id_destino_final.
                 RUN semanaAno IN THIS-PROCEDURE (pedidos_packing.fecha , OUTPUT pedidos_packing.semana).
           ASSIGN  pedidos_packing.c_hora = STRING(NOW)
                   pedidos_packing.c_fecha = TODAY
                   pedidos_packing.pedido_traslado_sap = hTable:value(i,26)
                   pedidos_packing.posicion_pedido_sap = hTable:value(i,27).

            EXPORT STREAM ped pedidos_packing.

            hFunc1    = hFunctions:ADD('BAPI_GET_CALIBRE_OE').
            hOE       = hFunc1:exports('OE').
            hTable1   = hFunc1:tables('CALIBRE').
            hOE:VALUE = string(pedidos_packing.oe,'9999999999').
        
          IF hFunc1:CALL() THEN DO:
              k = 0.


              REPEAT:
                  k = k + 1.
                  cV1 = hTable1:VALUE(k,2) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN  LEAVE.
                  IF cV1 = ? THEN   LEAVE.
                
                  IF INTEGER(htable1:VALUE(k , 11)) <> iOrden THEN  NEXT.
                      
                  iItem = integer(hTable1:VALUE(k,3)) + integer(hTable1:VALUE(k,4)).
/*                  
                  MESSAGE iItem VIEW-AS ALERT-BOX.
*/
                  FIND FIRST items_pedidos_packing WHERE  items_pedidos_packing.id_empresa      = 1 AND
                                                          items_pedidos_packing.id_punto_emisor = 1 AND
                                                          Items_pedidos_packing.id_orden        = iOrden  AND
                                                          items_pedidos_packing.ITEM            = iItem NO-ERROR.

                  FIND FIRST pallets OF items_pedidos_packing NO-LOCK NO-ERROR.
                  IF AVAILABLE pallets THEN 
                  DO:
                      items_pedidos_packing.cant_pallets      = decimal(hTable1:VALUE(k,9)) * 1000 .
                      NEXT.
                  END.

                  IF NOT AVAILABLE items_pedidos_packing THEN DO:
                      CREATE    items_pedidos_packing.
                      ASSIGN    items_pedidos_packing.id_empresa      = 1 
                                items_pedidos_packing.id_punto_emisor = 1
                                items_pedidos_packing.id_orden        = iOrden
                                items_pedidos_packing.ITEM            = iItem.
                  END.
/*
MESSAGE hTable " - " SUBSTRING(hTable:VALUE(i,7),1,1) " " integer(hTable:VALUE(i,11)) VIEW-AS ALERT-BOX.
*/
                  cEspecie = SUBSTRING(hTable:VALUE(i,7),1,1).
                  icalidad = integer(hTable:VALUE(i,11)).
/*
MESSAGE cEspecie icalidad VIEW-AS ALERT-BOX.
*/
                  cVariedad      = SUBSTRING(hTable:VALUE(i,7),2,2).
                  cEnvase        = SUBSTRING(hTable:VALUE(i,7),5,3).
                  cMarca         = SUBSTRING(hTable:VALUE(i,7),9,3).
                  iPaletizado    = INTEGER(SUBSTRING(hTable:VALUE(i,7),13,3)).

                  ctipoPalet     = string(hTable:VALUE(i,13)).
                  cTipoEsquinero = string(hTable:VALUE(i,14)).
                  ccategoria     = string(hTable:VALUE(i,12)).
                  iTratamiento   = INTEGER(hTable:VALUE(i,10)).

                  FIND FIRST variedades WHERE variedades.id_articulo_sap = cEspecie AND 
                                              variedades.id_variedad_sap = cVariedad NO-LOCK NO-ERROR.
                  FIND FIRST productos_terminados WHERE productos_terminados.id_articulo_sap = cEspecie NO-LOCK NO-ERROR.
                  FIND FIRST envases_prod WHERE envases_prod.id_envase_sap = cEnvase NO-LOCK NO-ERROR.
                  FIND FIRST marcas_prod WHERE marcas_prod.id_marca_sap = cMarca NO-LOCK NO-ERROR.
                  FIND FIRST tipo_pallets WHERE tipo_pallets.id_tipo_pallet_sap = ctipoPalet NO-LOCK NO-ERROR.
                  FIND FIRST tipo_esquineros WHERE tipo_esquineros.id_tipo_esquinero_sap = cTipoEsquinero NO-LOCK NO-ERROR.
                  FIND FIRST categorias_packing WHERE categorias_packing.id_categoria_sap = cCategoria NO-LOCK NO-ERROR.
                  FIND FIRST calidades WHERE integer(calidades.id_calidad_sap) = iCalidad NO-LOCK NO-ERROR.
                  FIND FIRST caracteristicas WHERE integer(caracteristicas.id_caract_sap) = iTratamiento NO-LOCK NO-ERROR.
/*
IF AVAILABLE calidades THEN MESSAGE calidades.id_calidad VIEW-AS ALERT-BOX.
*/
                  ASSIGN 
                      items_pedidos_packing.semana            = pedidos_packing.semana
                      items_pedidos_packing.item              = iItem
                      items_pedidos_packing.id_variedad       = IF AVAILABLE variedades THEN variedades.id_variedad ELSE 999
                      items_pedidos_packing.id_vapor          = pedidos_packing.id_vapor
                      items_pedidos_packing.id_tipo_pallet    = IF AVAILABLE tipo_pallets THEN tipo_pallets.id_tipo_pallet ELSE 999
                      items_pedidos_packing.id_tipo_esquinero = IF AVAILABLE tipo_esquineros THEN tipo_esquineros.id_tipo_esquinero ELSE 999
                      items_pedidos_packing.id_punto_emisor   = pedidos_packing.id_punto_emisor
                      items_pedidos_packing.id_pedido_sap     = pedidos_packing.id_pedido_sap 
                      items_pedidos_packing.id_orden          = pedidos_packing.id_orden
                      items_pedidos_packing.id_marca          = IF AVAILABLE marcas_prod THEN marcas_prod.id_marca ELSE 999
                      items_pedidos_packing.id_envase         = IF AVAILABLE envases_prod THEN envases_prod.id_envase ELSE 999
                      items_pedidos_packing.id_empresa        = pedidos_packing.id_empresa
                      items_pedidos_packing.id_categoria      = IF AVAILABLE categorias_packing THEN categorias_packing.id_categoria ELSE 999
                      items_pedidos_packing.id_calidad        = IF AVAILABLE calidades THEN calidades.id_calidad ELSE 999
                      items_pedidos_packing.id_articulo       = IF AVAILABLE productos_terminados THEN productos_terminados.id_articulo ELSE 999 
                      items_pedidos_packing.c_usuario         = pedidos_packing.c_usuario
                      items_pedidos_packing.contramarca       = pedidos_packing.contramarca
                      items_pedidos_packing.cant_pallets      = decimal(hTable1:VALUE(k,9)) * 1000 
                      items_pedidos_packing.calibre           = replace(string(hTable1:VALUE(k,8)) , '/' , '')
                      items_pedidos_packing.bultos            = ipaletizado
                      items_pedidos_packing.anio              = pedidos_packing.ano 
                      items_pedidos_packing.posicion_oe       = htable:VALUE(i,6)
                      items_pedidos_packing.material_sap      = htable:VALUE(i,7)
                      items_pedidos_packing.id_caract         = IF AVAILABLE caracteristicas THEN caracteristicas.id_caract ELSE 999. 
                      items_pedidos_packing.c_hora            = STRING(NOW).
                      items_pedidos_packing.c_fecha           = TODAY.

                      RELEASE items_pedidos_packing.

              END.
              pedidos_packing.TOTAL_pallets = 0.
              FOR EACH items_pedidos_packing OF pedidos_packing.
                  pedidos_packing.TOTAL_pallets = pedidos_packing.total_pallets + items_pedidos_packing.cant_pallets.
              END.
            END.
      END.
      
      OUTPUT STREAM ped CLOSE.
  END.
  ELSE
      MESSAGE 'ERROR' VIEW-AS ALERT-BOX ERROR.

  hFunctions:connection:logoff().
  RELEASE OBJECT hFunctions.
END.
ELSE
    MESSAGE 'No se pudo conectar a Sap' VIEW-AS ALERT-BOX ERROR.

SESSION:NUMERIC-FORMAT = nm.

RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sapToPedidosPackingCalidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sapToPedidosPackingCalidad Procedure 
PROCEDURE sapToPedidosPackingCalidad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pDesde   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pHasta   AS CHARACTER NO-UNDO.

DEFINE VARIABLE DEBUG AS LOGICAL NO-UNDO.

DEFINE VARIABLE   hSAP       AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTable     AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTable1    AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTablev    AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunctions AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc1     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFuncV     AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE i     AS INTEGER NO-UNDO.
DEFINE VARIABLE j     AS INTEGER NO-UNDO.
DEFINE VARIABLE iAnio AS INTEGER NO-UNDO.

define variable hD1     as com-handle NO-UNDO.
define variable hD2     as com-handle no-undo.
DEFINE VAR hOE          AS COM-HANDLE NO-UNDO.
DEFINE VAR hOEv          AS COM-HANDLE NO-UNDO.
DEFINE VAR cV           AS CHARACTER NO-UNDO.
DEFINE VAR cV1          AS CHARACTER NO-UNDO.

DEFINE VAR k            AS INTEGER NO-UNDO.
DEFINE VAR cVar         AS CHARACTER NO-UNDO FORMAT 'x(1024)'.
DEFINE VAR ccVar        AS CHARACTER NO-UNDO FORMAT 'x(20)'.


DEFINE VAR cVapor   AS CHARACTER NO-UNDO.
DEFINE VAR iLug     AS INTEGER NO-UNDO.
DEFINE VAR iPuerto  AS INTEGER NO-UNDO.
DEFINE VAR cDestino AS CHARACTER NO-UNDO.
DEFINE VAR iCliente AS INT64 NO-UNDO.
DEFINE VAR cFecha   AS CHARACTER NO-UNDO.
DEFINE VAR iPedido  AS INTEGER NO-UNDO.
DEFINE VAR iOrden   AS INTEGER NO-UNDO.
DEFINE VAR iItem    AS INTEGER NO-UNDO.


DEFINE VAR cEspecie     AS CHARACTER NO-UNDO.
DEFINE VAR cVariedad    AS CHARACTER NO-UNDO.
DEFINE VAR cEnvase      AS CHARACTER NO-UNDO.
DEFINE VAR cMarca       AS CHARACTER NO-UNDO.
DEFINE VAR iPaletizado  AS INTEGER NO-UNDO.
DEFINE VAR cTipoPalet       AS CHARACTER NO-UNDO.
DEFINE VAR cTipoEsquinero   AS CHARACTER NO-UNDO.
DEFINE VAR cCategoria       AS CHARACTER NO-UNDO.
DEFINE VAR iCalidad         AS INTEGER NO-UNDO.
DEFINE VAR iTratamiento     AS INTEGER NO-UNDO.


DEFINE VAR fdesde           AS DATE NO-UNDO.
DEFINE VAR fhasta           AS DATE NO-UNDO.
DEFINE VAR nm               AS CHARACTER NO-UNDO.

DEFINE VAR flag-esquinero AS LOGICAL NO-UNDO.
DEFINE VAR msgesquinero AS CHARACTER NO-UNDO.

fdesde = DATE(INTEGER(SUBSTRING(pdesde,5,2)) ,  INTEGER(substring(pdesde,7,2)) , INTEGER(SUBSTRING(pdesde,1,4)) ).
fhasta = DATE(INTEGER(SUBSTRING(phasta,5,2)) ,  INTEGER(substring(phasta,7,2)) , INTEGER(SUBSTRING(phasta,1,4)) ).

nm = SESSION:NUMERIC-FORMAT.

SESSION:NUMERIC-FORMAT = 'American'.


hFunctions = connectToSapCalidad().

IF VALID-HANDLE(hFunctions) THEN DO:
  
  FOR EACH pedidos_packing USE-INDEX fecha WHERE pedidos_packing.fecha  >= fDesde AND pedidos_packing.fecha <= fHasta AND 
      id_vapor_usa = 0.
      FIND FIRST cajas WHERE
          cajas.id_empresa = pedidos_packing.id_empresa AND
          cajas.id_punto_emisor = pedidos_packing.id_punto_emisor AND
          cajas.id_orden = pedidos_packing.id_orden NO-LOCK NO-ERROR.
      IF NOT AVAILABLE cajas THEN DO:
          FOR EACH items_pedidos_packing OF pedidos_packing.
              DELETE items_pedidos_packing.
          END.
          DELETE pedidos_packing .
      END.
  END.

  hFunc  = hFunctions:ADD('BAPI_GET_ORDEN_FABRICACION').
  hD1    = hFunc:exports('DATE_FROM').
  hD2    = hFunc:exports('DATE_TO').
  hTable = hFunc:tables('ORDENES').

  hD1:VALUE = trim(pDesde).
  hD2:VALUE = trim(pHasta).
  
  IF hFunc:CALL() THEN DO:
      i = 0.
          

      REPEAT ON ERROR UNDO , LEAVE:

          
          
          i = i + 1.
          cV = hTable:VALUE(i,5) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN  LEAVE.
          IF cV = ? THEN   LEAVE.

          cVapor   = hTable:VALUE(i,18).
          iLug     = integer(hTable:VALUE(i,19)).
          iPuerto  = integer(hTable:VALUE(i,20)).
          cDestino = hTable:VALUE(i,16).
          iOrden   = integer(hTable:value(i,2)).
          iCliente = INT64(HTABLE:VALUE(i,4)).    
          
          IF LENGTH(string(hTable:value(i,23))) > 4 THEN
          DO:
              MESSAGE "Contramarca para la orden "  STRING(iOrden)  SKIP  " Tiene m s de 4 caracteres. No se procesar  ." VIEW-AS ALERT-BOX ERROR.
              NEXT.
          END.

          FIND FIRST clientes_ventas WHERE clientes_ventas.id_cliente_sap = iCliente NO-LOCK NO-ERROR.
          cTipoEsquinero = string(hTable:VALUE(i,14)).                 
          FIND FIRST tipo_esquineros WHERE tipo_esquineros.id_tipo_esquinero_sap = cTipoEsquinero NO-LOCK NO-ERROR.
          IF NOT AVAILABLE clientes_ventas OR NOT AVAILABLE tipo_esquineros THEN
          DO:
            MESSAGE "Cliente o esquinero no definido para la orden "  STRING(iOrden)  SKIP  "No se procesar  ." VIEW-AS ALERT-BOX ERROR.
            NEXT.
          END.

          hFuncV    = hFunctions:ADD('BAPI_GET_CALIBRE_OE').
          hOEV       = hFuncV:exports('OE').
          hTableV   = hFuncV:tables('CALIBRE').
          hOEV:VALUE = INTEGER(hTable:VALUE(i,3)).

          flag-esquinero = FALSE.
          IF hFuncV:CALL() THEN DO:
              k = 0.
              REPEAT:
                  k = k + 1.
                  cV1 = hTableV:VALUE(k,2) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN  LEAVE.
                  IF cV1 = ? THEN   LEAVE.
                
                  IF INTEGER(htableV:VALUE(k , 11)) <> iOrden THEN  NEXT.
                      
                  FIND r_clientes_esquineros WHERE r_clientes_esquineros.id_cliente = clientes_ventas.id_cliente AND
                                                   r_clientes_esquineros.id_tipo_esquinero = tipo_esquineros.id_tipo_esquinero NO-LOCK NO-ERROR.
                  IF  AVAILABLE r_clientes_esquineros AND r_clientes_esquineros.estado THEN
                  DO:
                      MESSAGE "La orden " STRING(iOrden) " del cliente SAP " string(iCliente) " - " Clientes_ventas.razon_social SKIP
                              "tiene pedido un esquinero no autorizado. No se procesar " VIEW-AS ALERT-BOX ERROR.
                      flag-esquinero = TRUE.
                    LEAVE.
                  END.
              END.
          END.
          IF flag-esquinero = TRUE THEN NEXT.

          FIND FIRST pedidos_packing WHERE  pedidos_packing.id_empresa      = 1 AND
                                            pedidos_packing.id_punto_emisor = 1 AND
                                            pedidos_packing.id_orden        = iOrden  NO-ERROR.

          IF AVAILABLE pedidos_packing AND pedidos_packing.id_vapor_usa <> 0 THEN  NEXT.


          IF NOT AVAILABLE pedidos_packing  THEN DO:
              CREATE pedidos_packing.
              ASSIGN 
                  pedidos_packing.id_orden        = iOrden
                  pedidos_packing.id_empresa      = 1
                  pedidos_packing.id_punto_emisor = 1.
          END.

          FIND FIRST vapores WHERE vapores.id_vapor_sap = cVapor NO-LOCK NO-ERROR.
          FIND FIRST lugar_descarga WHERE integer(lugar_descarga.id_lugdes_sap) = iLug NO-LOCK NO-ERROR.
          FIND FIRST puertos WHERE puertos.id_puerto_sap = ipuerto NO-LOCK NO-ERROR.
          FIND FIRST destinos WHERE integer(destinos.id_destino_sap) = integer(cDestino) NO-LOCK NO-ERROR.
          FIND FIRST clientes_ventas WHERE clientes_ventas.id_cliente_sap = iCliente NO-LOCK NO-ERROR.
          

          ASSIGN pedidos_packing.union_europea    = IF hTable:VALUE(i,21) = '' THEN FALSE ELSE TRUE
                 pedidos_packing.id_vapor         = IF AVAILABLE vapores THEN vapores.id_vapor ELSE 999 
                 pedidos_packing.id_puerto_sal    = if available lugar_descarga then lugar_descarga.id_lugdes ELSE 999
                 pedidos_packing.id_pedido_sap    = hTable:value(i,2)
                 pedidos_packing.id_destino_final = if available destinos then destinos.id_destino ELSE 999
                 pedidos_packing.id_cliente       = if available clientes_ventas then clientes_ventas.id_cliente ELSE 999                                                            
                 pedidos_packing.id_mercado       = if available clientes_ventas THEN  clientes_ventas.mercado ELSE 999 
                 pedidos_packing.fecha            = date(string(hTable:VALUE(i,17)))  
                 pedidos_packing.c_usuario        = 'Interface Sap'
                 pedidos_packing.contramarca      = hTable:value(i,23)
                 pedidos_packing.china            = if hTable:value(i,22) = '' then FALSE ELSE TRUE 
                 pedidos_packing.ano              = YEAR(pedidos_packing.fecha)
                 pedidos_packing.oe               = INTEGER(hTable:VALUE(i,3))
                 pedidos_packing.OF_sap           = string(iOrden,'99999999999')
                 pedidos_packing.id_cliente_remito = 999996.
                 pedidos_packing.id_puerto_ent    = pedidos_packing.id_destino_final.
                 RUN semanaAno IN THIS-PROCEDURE (pedidos_packing.fecha , OUTPUT pedidos_packing.semana).
           ASSIGN  pedidos_packing.c_hora = STRING(NOW)
                   pedidos_packing.c_fecha = TODAY
                   pedidos_packing.pedido_traslado_sap = hTable:value(i,26)
                   pedidos_packing.posicion_pedido_sap = hTable:value(i,27).


            hFunc1    = hFunctions:ADD('BAPI_GET_CALIBRE_OE').
            hOE       = hFunc1:exports('OE').
            hTable1   = hFunc1:tables('CALIBRE').
            hOE:VALUE = string(pedidos_packing.oe,'9999999999').
          
            IF hFunc1:CALL() THEN DO:
              k = 0.


              REPEAT:
                  k = k + 1.
                  cV1 = hTable1:VALUE(k,2) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN  LEAVE.
                  IF cV1 = ? THEN   LEAVE.
                
                  IF INTEGER(htable1:VALUE(k , 11)) <> iOrden THEN  NEXT.
                      
                  iItem = integer(hTable1:VALUE(k,3)) + integer(hTable1:VALUE(k,4)).
/*                  
                  MESSAGE iItem VIEW-AS ALERT-BOX.
*/
                  FIND FIRST items_pedidos_packing WHERE  items_pedidos_packing.id_empresa      = 1 AND
                                                          items_pedidos_packing.id_punto_emisor = 1 AND
                                                          Items_pedidos_packing.id_orden        = iOrden  AND
                                                          items_pedidos_packing.ITEM            = iItem NO-ERROR.

                  FIND FIRST pallets OF items_pedidos_packing NO-LOCK NO-ERROR.
                  IF AVAILABLE pallets THEN 
                  DO:
                      items_pedidos_packing.cant_pallets      = decimal(hTable1:VALUE(k,9)) * 1000 .
                      NEXT.
                  END.

                  IF NOT AVAILABLE items_pedidos_packing THEN DO:
                      CREATE    items_pedidos_packing.
                      ASSIGN    items_pedidos_packing.id_empresa      = 1 
                                items_pedidos_packing.id_punto_emisor = 1
                                items_pedidos_packing.id_orden        = iOrden
                                items_pedidos_packing.ITEM            = iItem.
                  END.


                  cEspecie = SUBSTRING(hTable:VALUE(i,7),1,1).
                  
                  icalidad = integer(hTable:VALUE(i,11)).

                  cVariedad      = SUBSTRING(hTable:VALUE(i,7),2,2).
                  cEnvase        = SUBSTRING(hTable:VALUE(i,7),5,3).
                  cMarca         = SUBSTRING(hTable:VALUE(i,7),9,3).
                  iPaletizado    = INTEGER(SUBSTRING(hTable:VALUE(i,7),13,3)).


                  ctipoPalet     = string(hTable:VALUE(i,13)).
                  cTipoEsquinero = string(hTable:VALUE(i,14)).
                  ccategoria     = string(hTable:VALUE(i,12)).
                  iTratamiento   = INTEGER(hTable:VALUE(i,10)).

                  FIND FIRST variedades WHERE variedades.id_articulo_sap = cEspecie AND 
                                              variedades.id_variedad_sap = cVariedad NO-LOCK NO-ERROR.
                  FIND FIRST productos_terminados WHERE productos_terminados.id_articulo_sap = cEspecie NO-LOCK NO-ERROR.
                  FIND FIRST envases_prod WHERE envases_prod.id_envase_sap = cEnvase NO-LOCK NO-ERROR.
                  FIND FIRST marcas_prod WHERE marcas_prod.id_marca_sap = cMarca NO-LOCK NO-ERROR.
                  FIND FIRST tipo_pallets WHERE tipo_pallets.id_tipo_pallet_sap = ctipoPalet NO-LOCK NO-ERROR.
                  FIND FIRST tipo_esquineros WHERE tipo_esquineros.id_tipo_esquinero_sap = cTipoEsquinero NO-LOCK NO-ERROR.
                  FIND FIRST categorias_packing WHERE categorias_packing.id_categoria_sap = cCategoria NO-LOCK NO-ERROR.
                  FIND FIRST calidades WHERE integer(calidades.id_calidad_sap) = iCalidad NO-LOCK NO-ERROR.
                  FIND FIRST caracteristicas WHERE integer(caracteristicas.id_caract_sap) = iTratamiento NO-LOCK NO-ERROR.


                  ASSIGN 
                      items_pedidos_packing.semana            = pedidos_packing.semana
                      items_pedidos_packing.item              = iItem
                      items_pedidos_packing.id_variedad       = IF AVAILABLE variedades THEN variedades.id_variedad ELSE 999
                      items_pedidos_packing.id_vapor          = pedidos_packing.id_vapor
                      items_pedidos_packing.id_tipo_pallet    = IF AVAILABLE tipo_pallets THEN tipo_pallets.id_tipo_pallet ELSE 999
                      items_pedidos_packing.id_tipo_esquinero = IF AVAILABLE tipo_esquineros THEN tipo_esquineros.id_tipo_esquinero ELSE 999
                      items_pedidos_packing.id_punto_emisor   = pedidos_packing.id_punto_emisor
                      items_pedidos_packing.id_pedido_sap     = pedidos_packing.id_pedido_sap 
                      items_pedidos_packing.id_orden          = pedidos_packing.id_orden
                      items_pedidos_packing.id_marca          = IF AVAILABLE marcas_prod THEN marcas_prod.id_marca ELSE 999
                      items_pedidos_packing.id_envase         = IF AVAILABLE envases_prod THEN envases_prod.id_envase ELSE 999
                      items_pedidos_packing.id_empresa        = pedidos_packing.id_empresa
                      items_pedidos_packing.id_categoria      = IF AVAILABLE categorias_packing THEN categorias_packing.id_categoria ELSE 999
                      items_pedidos_packing.id_calidad        = IF AVAILABLE calidades THEN calidades.id_calidad ELSE 999
                      items_pedidos_packing.id_articulo       = IF AVAILABLE productos_terminados THEN productos_terminados.id_articulo ELSE 999 
                      items_pedidos_packing.c_usuario         = pedidos_packing.c_usuario
                      items_pedidos_packing.contramarca       = pedidos_packing.contramarca
                      items_pedidos_packing.cant_pallets      = decimal(hTable1:VALUE(k,9)) * 1000 
                      items_pedidos_packing.calibre           = replace(string(hTable1:VALUE(k,8)) , '/' , '')
                      items_pedidos_packing.bultos            = ipaletizado
                      items_pedidos_packing.anio              = pedidos_packing.ano 
                      items_pedidos_packing.posicion_oe       = htable:VALUE(i,6)
                      items_pedidos_packing.material_sap      = htable:VALUE(i,7)
                      items_pedidos_packing.id_caract         = IF AVAILABLE caracteristicas THEN caracteristicas.id_caract ELSE 999. 
                      items_pedidos_packing.c_hora            = STRING(NOW).
                      items_pedidos_packing.c_fecha           = TODAY.

                      RELEASE items_pedidos_packing.

              END.
              pedidos_packing.TOTAL_pallets = 0.
              FOR EACH items_pedidos_packing OF pedidos_packing.
                  pedidos_packing.TOTAL_pallets = pedidos_packing.total_pallets + items_pedidos_packing.cant_pallets.
              END.
            END.
      END.
      
  END.
  ELSE
      MESSAGE 'ERROR' VIEW-AS ALERT-BOX ERROR.

  hFunctions:connection:logoff().
  RELEASE OBJECT hFunctions.
END.
ELSE
    MESSAGE 'No se pudo conectar a Sap' VIEW-AS ALERT-BOX ERROR.

SESSION:NUMERIC-FORMAT = nm.

RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sapToPedidosPackingPrueba) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sapToPedidosPackingPrueba Procedure 
PROCEDURE sapToPedidosPackingPrueba :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pDesde   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pHasta   AS CHARACTER NO-UNDO.

DEFINE VARIABLE DEBUG AS LOGICAL NO-UNDO.

DEFINE VARIABLE   hSAP       AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTable     AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTable1    AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunctions AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc1     AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE i     AS INTEGER NO-UNDO.
DEFINE VARIABLE j     AS INTEGER NO-UNDO.
DEFINE VARIABLE iAnio AS INTEGER NO-UNDO.

define variable hD1 as com-handle NO-UNDO.
define variable hD2 as com-handle no-undo.
DEFINE VAR hOE AS COM-HANDLE NO-UNDO.
DEFINE VAR cV AS CHARACTER NO-UNDO.
DEFINE VAR cV1 AS CHARACTER NO-UNDO.

DEFINE VAR k AS INTEGER NO-UNDO.
DEFINE VAR cVar AS CHARACTER NO-UNDO FORMAT 'x(1024)'.
DEFINE VAR ccVar AS CHARACTER NO-UNDO FORMAT 'x(20)'.


DEFINE VAR cVapor AS CHARACTER NO-UNDO.
DEFINE VAR iLug AS INTEGER NO-UNDO.
DEFINE VAR iPuerto AS INTEGER NO-UNDO.
DEFINE VAR cDestino AS CHARACTER NO-UNDO.
DEFINE VAR iCliente AS INT64 NO-UNDO.
DEFINE VAR cFecha AS CHARACTER NO-UNDO.
DEFINE VAR iPedido AS INTEGER NO-UNDO.
DEFINE VAR iOrden AS INTEGER NO-UNDO.
DEFINE VAR iItem AS INTEGER NO-UNDO.


DEFINE VAR cEspecie AS CHARACTER NO-UNDO.
DEFINE VAR cVariedad AS CHARACTER NO-UNDO.
DEFINE VAR cEnvase AS CHARACTER NO-UNDO.
DEFINE VAR cMarca AS CHARACTER NO-UNDO.
DEFINE VAR iPaletizado AS INTEGER NO-UNDO.
DEFINE VAR cTipoPalet AS CHARACTER NO-UNDO.
DEFINE VAR cTipoEsquinero AS CHARACTER NO-UNDO.
DEFINE VAR cCategoria AS CHARACTER NO-UNDO.
DEFINE VAR iCalidad AS INTEGER NO-UNDO.
DEFINE VAR iTratamiento AS INTEGER NO-UNDO.


DEFINE VAR fdesde AS DATE NO-UNDO.
DEFINE VAR fhasta AS DATE NO-UNDO.
DEFINE VAR nm AS CHARACTER NO-UNDO.

fdesde = DATE(INTEGER(SUBSTRING(pdesde,5,2)) ,  INTEGER(substring(pdesde,7,2)) , INTEGER(SUBSTRING(pdesde,1,4)) ).
fhasta = DATE(INTEGER(SUBSTRING(phasta,5,2)) ,  INTEGER(substring(phasta,7,2)) , INTEGER(SUBSTRING(phasta,1,4)) ).

nm = SESSION:NUMERIC-FORMAT.

SESSION:NUMERIC-FORMAT = 'American'.


hFunctions = connectToSapCalidad ().

IF VALID-HANDLE(hFunctions) THEN DO:
  
  FOR EACH pedidos_packing USE-INDEX fecha WHERE pedidos_packing.fecha  >= fDesde AND pedidos_packing.fecha <= fHasta AND 
      id_vapor_usa = 0.
      FIND FIRST cajas WHERE
          cajas.id_empresa = pedidos_packing.id_empresa AND
          cajas.id_punto_emisor = pedidos_packing.id_punto_emisor AND
          cajas.id_orden = pedidos_packing.id_orden NO-LOCK NO-ERROR.
      IF NOT AVAILABLE cajas THEN DO:
          FOR EACH items_pedidos_packing OF pedidos_packing.
              DELETE items_pedidos_packing.
          END.
          DELETE pedidos_packing .
      END.
  END.

  hFunc  = hFunctions:ADD('BAPI_GET_ORDEN_FABRICACION').
  hD1    = hFunc:exports('DATE_FROM').
  hD2    = hFunc:exports('DATE_TO').
  hTable = hFunc:tables('ORDENES').

  hD1:VALUE = trim(pDesde).
  hD2:VALUE = trim(pHasta).
  
  IF hFunc:CALL() THEN DO:
      i = 0.

      OUTPUT STREAM ped TO z:/temp/pedidos.txt.

      REPEAT ON ERROR UNDO , LEAVE:

          i = i + 1.
          cV = hTable:VALUE(i,5) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN  LEAVE.
          IF cV = ? THEN   LEAVE.

          cVapor   = hTable:VALUE(i,18).
          iLug     = integer(hTable:VALUE(i,19)).
          iPuerto  = integer(hTable:VALUE(i,20)).
          cDestino = hTable:VALUE(i,16).
          iOrden   = integer(hTable:value(i,2)).
          iCliente = INT64(HTABLE:VALUE(i,4)).


          FIND FIRST pedidos_packing WHERE  pedidos_packing.id_empresa      = 1 AND
                                            pedidos_packing.id_punto_emisor = 1 AND
                                            pedidos_packing.id_orden        = iOrden  NO-ERROR.

          IF AVAILABLE pedidos_packing AND pedidos_packing.id_vapor_usa <> 0 THEN  NEXT.

          IF NOT AVAILABLE pedidos_packing  THEN DO:
              CREATE pedidos_packing.
              ASSIGN 
                  pedidos_packing.id_orden        = iOrden
                  pedidos_packing.id_empresa      = 1
                  pedidos_packing.id_punto_emisor = 1.
          END.

          FIND FIRST vapores WHERE vapores.id_vapor_sap = cVapor NO-LOCK NO-ERROR.
          FIND FIRST lugar_descarga WHERE integer(lugar_descarga.id_lugdes_sap) = iLug NO-LOCK NO-ERROR.
          FIND FIRST puertos WHERE puertos.id_puerto_sap = ipuerto NO-LOCK NO-ERROR.
          FIND FIRST destinos WHERE integer(destinos.id_destino_sap) = integer(cDestino) NO-LOCK NO-ERROR.
          FIND FIRST clientes_ventas WHERE clientes_ventas.id_cliente_sap = iCliente NO-LOCK NO-ERROR.
          
          ASSIGN pedidos_packing.union_europea    = IF hTable:VALUE(i,21) = '' THEN FALSE ELSE TRUE
                 pedidos_packing.id_vapor         = IF AVAILABLE vapores THEN vapores.id_vapor ELSE 999 
                 pedidos_packing.id_puerto_sal    = if available lugar_descarga then lugar_descarga.id_lugdes ELSE 999
                 pedidos_packing.id_pedido_sap    = hTable:value(i,2)
                 pedidos_packing.id_destino_final = if available destinos then destinos.id_destino ELSE 999
                 pedidos_packing.id_cliente       = if available clientes_ventas then clientes_ventas.id_cliente ELSE 999                                                            
                 pedidos_packing.id_mercado       = if available clientes_ventas THEN  clientes_ventas.mercado ELSE 999 
                 pedidos_packing.fecha            = date(string(hTable:VALUE(i,17)))  
                 pedidos_packing.c_usuario        = 'Interface Sap'
                 pedidos_packing.contramarca      = hTable:value(i,23)
                 pedidos_packing.china            = if hTable:value(i,22) = '' then FALSE ELSE TRUE 
                 pedidos_packing.ano              = YEAR(pedidos_packing.fecha)
                 pedidos_packing.oe               = INTEGER(hTable:VALUE(i,3))
                 pedidos_packing.OF_sap           = string(iOrden,'99999999999')
                 pedidos_packing.id_cliente_remito = 999996.
                 pedidos_packing.id_puerto_ent    = pedidos_packing.id_destino_final.
                 RUN semanaAno IN THIS-PROCEDURE (pedidos_packing.fecha , OUTPUT pedidos_packing.semana).
           ASSIGN  pedidos_packing.c_hora = STRING(NOW)
                   pedidos_packing.c_fecha = TODAY
                   pedidos_packing.pedido_traslado_sap = hTable:value(i,26)
                   pedidos_packing.posicion_pedido_sap = hTable:value(i,27).

            EXPORT STREAM ped pedidos_packing.

            hFunc1    = hFunctions:ADD('BAPI_GET_CALIBRE_OE').
            hOE       = hFunc1:exports('OE').
            hTable1   = hFunc1:tables('CALIBRE').
            hOE:VALUE = string(pedidos_packing.oe,'9999999999').
        
          IF hFunc1:CALL() THEN DO:
              k = 0.

              OUTPUT STREAM ite TO z:/temp/itpedidos.txt.

              REPEAT:
                  k = k + 1.
                  cV1 = hTable1:VALUE(k,2) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN  LEAVE.
                  IF cV1 = ? THEN   LEAVE.
                
                  IF INTEGER(htable1:VALUE(k , 11)) <> iOrden THEN  NEXT.
                      
                  iItem = integer(hTable1:VALUE(k,3)) + integer(hTable1:VALUE(k,4)).
/*                  
                  MESSAGE iItem VIEW-AS ALERT-BOX.
*/
                  FIND FIRST items_pedidos_packing WHERE  items_pedidos_packing.id_empresa      = 1 AND
                                                          items_pedidos_packing.id_punto_emisor = 1 AND
                                                          Items_pedidos_packing.id_orden        = iOrden  AND
                                                          items_pedidos_packing.ITEM            = iItem NO-ERROR.

                  IF NOT AVAILABLE items_pedidos_packing THEN DO:
                      CREATE    items_pedidos_packing.
                      ASSIGN    items_pedidos_packing.id_empresa      = 1 
                                items_pedidos_packing.id_punto_emisor = 1
                                items_pedidos_packing.id_orden        = iOrden
                                items_pedidos_packing.ITEM            = iItem.
                  END.


                  cEspecie = SUBSTRING(hTable:VALUE(i,7),1,1).
                  
                  icalidad = integer(hTable:VALUE(i,11)).

                  cVariedad      = SUBSTRING(hTable:VALUE(i,7),2,2).
                  cEnvase        = SUBSTRING(hTable:VALUE(i,7),5,3).
                  cMarca         = SUBSTRING(hTable:VALUE(i,7),9,3).
                  iPaletizado    = INTEGER(SUBSTRING(hTable:VALUE(i,7),13,3)).


                  ctipoPalet     = string(hTable:VALUE(i,13)).
                  cTipoEsquinero = string(hTable:VALUE(i,14)).
                  ccategoria     = string(hTable:VALUE(i,12)).
                  iTratamiento   = INTEGER(hTable:VALUE(i,10)).

                  FIND FIRST variedades WHERE variedades.id_articulo_sap = cEspecie AND 
                                              variedades.id_variedad_sap = cVariedad NO-LOCK NO-ERROR.
                  FIND FIRST productos_terminados WHERE productos_terminados.id_articulo_sap = cEspecie NO-LOCK NO-ERROR.
                  FIND FIRST envases_prod WHERE envases_prod.id_envase_sap = cEnvase NO-LOCK NO-ERROR.
                  FIND FIRST marcas_prod WHERE marcas_prod.id_marca_sap = cMarca NO-LOCK NO-ERROR.
                  FIND FIRST tipo_pallets WHERE tipo_pallets.id_tipo_pallet_sap = ctipoPalet NO-LOCK NO-ERROR.
                  FIND FIRST tipo_esquineros WHERE tipo_esquineros.id_tipo_esquinero_sap = cTipoEsquinero NO-LOCK NO-ERROR.
                  FIND FIRST categorias_packing WHERE categorias_packing.id_categoria_sap = cCategoria NO-LOCK NO-ERROR.
                  FIND FIRST calidades WHERE integer(calidades.id_calidad_sap) = iCalidad NO-LOCK NO-ERROR.
                  FIND FIRST caracteristicas WHERE integer(caracteristicas.id_caract_sap) = iTratamiento NO-LOCK NO-ERROR.


                  ASSIGN 
                      items_pedidos_packing.semana            = pedidos_packing.semana
                      items_pedidos_packing.item              = iItem
                      items_pedidos_packing.id_variedad       = IF AVAILABLE variedades THEN variedades.id_variedad ELSE 999
                      items_pedidos_packing.id_vapor          = pedidos_packing.id_vapor
                      items_pedidos_packing.id_tipo_pallet    = IF AVAILABLE tipo_pallets THEN tipo_pallets.id_tipo_pallet ELSE 999
                      items_pedidos_packing.id_tipo_esquinero = IF AVAILABLE tipo_esquineros THEN tipo_esquineros.id_tipo_esquinero ELSE 999
                      items_pedidos_packing.id_punto_emisor   = pedidos_packing.id_punto_emisor
                      items_pedidos_packing.id_pedido_sap     = pedidos_packing.id_pedido_sap 
                      items_pedidos_packing.id_orden          = pedidos_packing.id_orden
                      items_pedidos_packing.id_marca          = IF AVAILABLE marcas_prod THEN marcas_prod.id_marca ELSE 999
                      items_pedidos_packing.id_envase         = IF AVAILABLE envases_prod THEN envases_prod.id_envase ELSE 999
                      items_pedidos_packing.id_empresa        = pedidos_packing.id_empresa
                      items_pedidos_packing.id_categoria      = IF AVAILABLE categorias_packing THEN categorias_packing.id_categoria ELSE 999
                      items_pedidos_packing.id_calidad        = IF AVAILABLE calidades THEN calidades.id_calidad ELSE 999
                      items_pedidos_packing.id_articulo       = IF AVAILABLE productos_terminados THEN productos_terminados.id_articulo ELSE 999 
                      items_pedidos_packing.c_usuario         = pedidos_packing.c_usuario
                      items_pedidos_packing.contramarca       = pedidos_packing.contramarca
                      items_pedidos_packing.cant_pallets      = decimal(hTable1:VALUE(k,9)) * 1000 
                      items_pedidos_packing.calibre           = replace(string(hTable1:VALUE(k,8)) , '/' , '')
                      items_pedidos_packing.bultos            = ipaletizado
                      items_pedidos_packing.anio              = pedidos_packing.ano 
                      items_pedidos_packing.posicion_oe       = htable:VALUE(i,6)
                      items_pedidos_packing.material_sap      = htable:VALUE(i,7)
                      items_pedidos_packing.id_caract         = IF AVAILABLE caracteristicas THEN caracteristicas.id_caract ELSE 999. 
                      items_pedidos_packing.c_hora            = STRING(NOW).
                      items_pedidos_packing.c_fecha           = TODAY.

                      EXPORT STREAM ite items_pedidos_packing.
                      
                      RELEASE items_pedidos_packing.

              END.
              OUTPUT STREAM ite CLOSE.
              pedidos_packing.TOTAL_pallets = 0.
              FOR EACH items_pedidos_packing OF pedidos_packing.
                  pedidos_packing.TOTAL_pallets = pedidos_packing.total_pallets + items_pedidos_packing.cant_pallets.
              END.
        END.
      END.
      
      OUTPUT STREAM ped CLOSE.
  END.
  ELSE
      MESSAGE 'ERROR' VIEW-AS ALERT-BOX ERROR.

  hFunctions:connection:logoff().
  RELEASE OBJECT hFunctions.
END.
ELSE
    MESSAGE 'No se pudo conectar a Sap' VIEW-AS ALERT-BOX ERROR.

SESSION:NUMERIC-FORMAT = nm.

RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sapToPEdidosPackingSEsquineros) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sapToPEdidosPackingSEsquineros Procedure 
PROCEDURE sapToPEdidosPackingSEsquineros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Not       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pDesde   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pHasta   AS CHARACTER NO-UNDO.

DEFINE VARIABLE DEBUG AS LOGICAL NO-UNDO.

DEFINE VARIABLE   hSAP       AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTable     AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTable1    AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunctions AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc1     AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE i     AS INTEGER NO-UNDO.
DEFINE VARIABLE j     AS INTEGER NO-UNDO.
DEFINE VARIABLE iAnio AS INTEGER NO-UNDO.

define variable hD1 as com-handle NO-UNDO.
define variable hD2 as com-handle no-undo.
DEFINE VAR hOE AS COM-HANDLE NO-UNDO.
DEFINE VAR cV AS CHARACTER NO-UNDO.
DEFINE VAR cV1 AS CHARACTER NO-UNDO.

DEFINE VAR k AS INTEGER NO-UNDO.
DEFINE VAR cVar AS CHARACTER NO-UNDO FORMAT 'x(1024)'.
DEFINE VAR ccVar AS CHARACTER NO-UNDO FORMAT 'x(20)'.


DEFINE VAR cVapor AS CHARACTER NO-UNDO.
DEFINE VAR iLug AS INTEGER NO-UNDO.
DEFINE VAR iPuerto AS INTEGER NO-UNDO.
DEFINE VAR cDestino AS CHARACTER NO-UNDO.
DEFINE VAR iCliente AS INT64 NO-UNDO.
DEFINE VAR cFecha AS CHARACTER NO-UNDO.
DEFINE VAR iPedido AS INTEGER NO-UNDO.
DEFINE VAR iOrden AS INTEGER NO-UNDO.
DEFINE VAR iItem AS INTEGER NO-UNDO.


DEFINE VAR cEspecie AS CHARACTER NO-UNDO.
DEFINE VAR cVariedad AS CHARACTER NO-UNDO.
DEFINE VAR cEnvase AS CHARACTER NO-UNDO.
DEFINE VAR cMarca AS CHARACTER NO-UNDO.
DEFINE VAR iPaletizado AS INTEGER NO-UNDO.
DEFINE VAR cTipoPalet AS CHARACTER NO-UNDO.
DEFINE VAR cTipoEsquinero AS CHARACTER NO-UNDO.
DEFINE VAR cCategoria AS CHARACTER NO-UNDO.
DEFINE VAR iCalidad AS INTEGER NO-UNDO.
DEFINE VAR iTratamiento AS INTEGER NO-UNDO.


DEFINE VAR fdesde AS DATE NO-UNDO.
DEFINE VAR fhasta AS DATE NO-UNDO.
DEFINE VAR nm AS CHARACTER NO-UNDO.

fdesde = DATE(INTEGER(SUBSTRING(pdesde,5,2)) ,  INTEGER(substring(pdesde,7,2)) , INTEGER(SUBSTRING(pdesde,1,4)) ).
fhasta = DATE(INTEGER(SUBSTRING(phasta,5,2)) ,  INTEGER(substring(phasta,7,2)) , INTEGER(SUBSTRING(phasta,1,4)) ).

nm = SESSION:NUMERIC-FORMAT.

SESSION:NUMERIC-FORMAT = 'American'.


hFunctions = connectToSap().

IF VALID-HANDLE(hFunctions) THEN DO:
  
  FOR EACH pedidos_packing USE-INDEX fecha WHERE pedidos_packing.fecha  >= fDesde AND pedidos_packing.fecha <= fHasta AND 
      id_vapor_usa = 0.
      FIND FIRST cajas WHERE
          cajas.id_empresa = pedidos_packing.id_empresa AND
          cajas.id_punto_emisor = pedidos_packing.id_punto_emisor AND
          cajas.id_orden = pedidos_packing.id_orden NO-LOCK NO-ERROR.
      IF NOT AVAILABLE cajas THEN DO:
          FOR EACH items_pedidos_packing OF pedidos_packing.
              DELETE items_pedidos_packing.
          END.
          DELETE pedidos_packing .
      END.
  END.

  hFunc  = hFunctions:ADD('BAPI_GET_ORDEN_FABRICACION').
  hD1    = hFunc:exports('DATE_FROM').
  hD2    = hFunc:exports('DATE_TO').
  hTable = hFunc:tables('ORDENES').

  hD1:VALUE = trim(pDesde).
  hD2:VALUE = trim(pHasta).
  
  IF hFunc:CALL() THEN DO:
      i = 0.
          
      OUTPUT STREAM ped TO z:/temp/pedidos.txt.

      REPEAT ON ERROR UNDO , LEAVE:

          
          
          i = i + 1.
          cV = hTable:VALUE(i,5) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN  LEAVE.
          IF cV = ? THEN   LEAVE.

          cVapor   = hTable:VALUE(i,18).
          iLug     = integer(hTable:VALUE(i,19)).
          iPuerto  = integer(hTable:VALUE(i,20)).
          cDestino = hTable:VALUE(i,16).
          iOrden   = integer(hTable:value(i,2)).
          iCliente = INT64(HTABLE:VALUE(i,4)).




          FIND FIRST pedidos_packing WHERE  pedidos_packing.id_empresa      = 1 AND
                                            pedidos_packing.id_punto_emisor = 1 AND
                                            pedidos_packing.id_orden        = iOrden  NO-ERROR.

          IF AVAILABLE pedidos_packing AND pedidos_packing.id_vapor_usa <> 0 THEN  NEXT.


          IF NOT AVAILABLE pedidos_packing  THEN DO:
              CREATE pedidos_packing.
              ASSIGN 
                  pedidos_packing.id_orden        = iOrden
                  pedidos_packing.id_empresa      = 1
                  pedidos_packing.id_punto_emisor = 1.
          END.

          FIND FIRST vapores WHERE vapores.id_vapor_sap = cVapor NO-LOCK NO-ERROR.
          FIND FIRST lugar_descarga WHERE integer(lugar_descarga.id_lugdes_sap) = iLug NO-LOCK NO-ERROR.
          FIND FIRST puertos WHERE puertos.id_puerto_sap = ipuerto NO-LOCK NO-ERROR.
          FIND FIRST destinos WHERE integer(destinos.id_destino_sap) = integer(cDestino) NO-LOCK NO-ERROR.
          FIND FIRST clientes_ventas WHERE clientes_ventas.id_cliente_sap = iCliente NO-LOCK NO-ERROR.
          

          ASSIGN pedidos_packing.union_europea    = IF hTable:VALUE(i,21) = '' THEN FALSE ELSE TRUE
                 pedidos_packing.id_vapor         = IF AVAILABLE vapores THEN vapores.id_vapor ELSE 999 
                 pedidos_packing.id_puerto_sal    = if available lugar_descarga then lugar_descarga.id_lugdes ELSE 999
                 pedidos_packing.id_pedido_sap    = hTable:value(i,2)
                 pedidos_packing.id_destino_final = if available destinos then destinos.id_destino ELSE 999
                 pedidos_packing.id_cliente       = if available clientes_ventas then clientes_ventas.id_cliente ELSE 999                                                            
                 pedidos_packing.id_mercado       = if available clientes_ventas THEN  clientes_ventas.mercado ELSE 999 
                 pedidos_packing.fecha            = date(string(hTable:VALUE(i,17)))  
                 pedidos_packing.c_usuario        = 'Interface Sap'
                 pedidos_packing.contramarca      = hTable:value(i,23)
                 pedidos_packing.china            = if hTable:value(i,22) = '' then FALSE ELSE TRUE 
                 pedidos_packing.ano              = YEAR(pedidos_packing.fecha)
                 pedidos_packing.oe               = INTEGER(hTable:VALUE(i,3))
                 pedidos_packing.OF_sap           = string(iOrden,'99999999999')
                 pedidos_packing.id_cliente_remito = 999996.
                 pedidos_packing.id_puerto_ent    = pedidos_packing.id_destino_final.
                 RUN semanaAno IN THIS-PROCEDURE (pedidos_packing.fecha , OUTPUT pedidos_packing.semana).
           ASSIGN  pedidos_packing.c_hora = STRING(NOW)
                   pedidos_packing.c_fecha = TODAY
                   pedidos_packing.pedido_traslado_sap = hTable:value(i,26)
                   pedidos_packing.posicion_pedido_sap = hTable:value(i,27).

            EXPORT STREAM ped pedidos_packing.

            hFunc1    = hFunctions:ADD('BAPI_GET_CALIBRE_OE').
            hOE       = hFunc1:exports('OE').
            hTable1   = hFunc1:tables('CALIBRE').
            hOE:VALUE = string(pedidos_packing.oe,'9999999999').
        
          IF hFunc1:CALL() THEN DO:
              k = 0.

              OUTPUT STREAM ite TO z:/temp/itpedidos.txt.

              REPEAT:
                  k = k + 1.
                  cV1 = hTable1:VALUE(k,2) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN  LEAVE.
                  IF cV1 = ? THEN   LEAVE.
                
                  IF INTEGER(htable1:VALUE(k , 11)) <> iOrden THEN  NEXT.
                      
                  iItem = integer(hTable1:VALUE(k,3)) + integer(hTable1:VALUE(k,4)).
/*                  
                  MESSAGE iItem VIEW-AS ALERT-BOX.
*/
                  FIND FIRST items_pedidos_packing WHERE  items_pedidos_packing.id_empresa      = 1 AND
                                                          items_pedidos_packing.id_punto_emisor = 1 AND
                                                          Items_pedidos_packing.id_orden        = iOrden  AND
                                                          items_pedidos_packing.ITEM            = iItem NO-ERROR.

                  FIND FIRST pallets OF items_pedidos_packing NO-LOCK NO-ERROR.
                  IF AVAILABLE pallets THEN 
                  DO:
                      items_pedidos_packing.cant_pallets      = decimal(hTable1:VALUE(k,9)) * 1000 .
                      NEXT.
                  END.

                  IF NOT AVAILABLE items_pedidos_packing THEN DO:
                      CREATE    items_pedidos_packing.
                      ASSIGN    items_pedidos_packing.id_empresa      = 1 
                                items_pedidos_packing.id_punto_emisor = 1
                                items_pedidos_packing.id_orden        = iOrden
                                items_pedidos_packing.ITEM            = iItem.
                  END.


                  cEspecie = SUBSTRING(hTable:VALUE(i,7),1,1).
                  
                  icalidad = integer(hTable:VALUE(i,11)).

                  cVariedad      = SUBSTRING(hTable:VALUE(i,7),2,2).
                  cEnvase        = SUBSTRING(hTable:VALUE(i,7),5,3).
                  cMarca         = SUBSTRING(hTable:VALUE(i,7),9,3).
                  iPaletizado    = INTEGER(SUBSTRING(hTable:VALUE(i,7),13,3)).


                  ctipoPalet     = string(hTable:VALUE(i,13)).
                  cTipoEsquinero = string(hTable:VALUE(i,14)).
                  ccategoria     = string(hTable:VALUE(i,12)).
                  iTratamiento   = INTEGER(hTable:VALUE(i,10)).

                  FIND FIRST variedades WHERE variedades.id_articulo_sap = cEspecie AND 
                                              variedades.id_variedad_sap = cVariedad NO-LOCK NO-ERROR.
                  FIND FIRST productos_terminados WHERE productos_terminados.id_articulo_sap = cEspecie NO-LOCK NO-ERROR.
                  FIND FIRST envases_prod WHERE envases_prod.id_envase_sap = cEnvase NO-LOCK NO-ERROR.
                  FIND FIRST marcas_prod WHERE marcas_prod.id_marca_sap = cMarca NO-LOCK NO-ERROR.
                  FIND FIRST tipo_pallets WHERE tipo_pallets.id_tipo_pallet_sap = ctipoPalet NO-LOCK NO-ERROR.
                  FIND FIRST tipo_esquineros WHERE tipo_esquineros.id_tipo_esquinero_sap = cTipoEsquinero NO-LOCK NO-ERROR.
                  FIND FIRST categorias_packing WHERE categorias_packing.id_categoria_sap = cCategoria NO-LOCK NO-ERROR.
                  FIND FIRST calidades WHERE integer(calidades.id_calidad_sap) = iCalidad NO-LOCK NO-ERROR.
                  FIND FIRST caracteristicas WHERE integer(caracteristicas.id_caract_sap) = iTratamiento NO-LOCK NO-ERROR.


                  ASSIGN 
                      items_pedidos_packing.semana            = pedidos_packing.semana
                      items_pedidos_packing.item              = iItem
                      items_pedidos_packing.id_variedad       = IF AVAILABLE variedades THEN variedades.id_variedad ELSE 999
                      items_pedidos_packing.id_vapor          = pedidos_packing.id_vapor
                      items_pedidos_packing.id_tipo_pallet    = IF AVAILABLE tipo_pallets THEN tipo_pallets.id_tipo_pallet ELSE 999
                      items_pedidos_packing.id_tipo_esquinero = IF AVAILABLE tipo_esquineros THEN tipo_esquineros.id_tipo_esquinero ELSE 999
                      items_pedidos_packing.id_punto_emisor   = pedidos_packing.id_punto_emisor
                      items_pedidos_packing.id_pedido_sap     = pedidos_packing.id_pedido_sap 
                      items_pedidos_packing.id_orden          = pedidos_packing.id_orden
                      items_pedidos_packing.id_marca          = IF AVAILABLE marcas_prod THEN marcas_prod.id_marca ELSE 999
                      items_pedidos_packing.id_envase         = IF AVAILABLE envases_prod THEN envases_prod.id_envase ELSE 999
                      items_pedidos_packing.id_empresa        = pedidos_packing.id_empresa
                      items_pedidos_packing.id_categoria      = IF AVAILABLE categorias_packing THEN categorias_packing.id_categoria ELSE 999
                      items_pedidos_packing.id_calidad        = IF AVAILABLE calidades THEN calidades.id_calidad ELSE 999
                      items_pedidos_packing.id_articulo       = IF AVAILABLE productos_terminados THEN productos_terminados.id_articulo ELSE 999 
                      items_pedidos_packing.c_usuario         = pedidos_packing.c_usuario
                      items_pedidos_packing.contramarca       = pedidos_packing.contramarca
                      items_pedidos_packing.cant_pallets      = decimal(hTable1:VALUE(k,9)) * 1000 
                      items_pedidos_packing.calibre           = replace(string(hTable1:VALUE(k,8)) , '/' , '')
                      items_pedidos_packing.bultos            = ipaletizado
                      items_pedidos_packing.anio              = pedidos_packing.ano 
                      items_pedidos_packing.posicion_oe       = htable:VALUE(i,6)
                      items_pedidos_packing.material_sap      = htable:VALUE(i,7)
                      items_pedidos_packing.id_caract         = IF AVAILABLE caracteristicas THEN caracteristicas.id_caract ELSE 999. 
                      items_pedidos_packing.c_hora            = STRING(NOW).
                      items_pedidos_packing.c_fecha           = TODAY.

                      EXPORT STREAM ite items_pedidos_packing.
                      
                      RELEASE items_pedidos_packing.

              END.
              OUTPUT STREAM ite CLOSE.
              pedidos_packing.TOTAL_pallets = 0.
              FOR EACH items_pedidos_packing OF pedidos_packing.
                  pedidos_packing.TOTAL_pallets = pedidos_packing.total_pallets + items_pedidos_packing.cant_pallets.
              END.


        END.
      END.
      
      OUTPUT STREAM ped CLOSE.
  END.
  ELSE
      MESSAGE 'ERROR' VIEW-AS ALERT-BOX ERROR.

  hFunctions:connection:logoff().
  RELEASE OBJECT hFunctions.
END.
ELSE
    MESSAGE 'No se pudo conectar a Sap' VIEW-AS ALERT-BOX ERROR.

SESSION:NUMERIC-FORMAT = nm.

RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sapToProveedores) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sapToProveedores Procedure 
PROCEDURE sapToProveedores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VAR        hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VAR cCodigo AS CHARACTER NO-UNDO.
  DEFINE VAR iCodigo AS INTEGER NO-UNDO.


  hFunctions = connectToSap().

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc     = hFunctions:ADD('BAPI_GET_PROVEEDORES').
      hTable    = hFunc:tables('PROVE').
      hNum      = hfunc:imports('O_RECNUM').

      IF hFunc:CALL() THEN DO:

        i = 0.

        j = INTEGER(hNum:VALUE).

        DO i = 1 TO j:
           cCodigo = hTable:VALUE(i,2).
           IF ccodigo = '' OR cCodigo = ? THEN NEXT.
           iCodigo = INTEGER(cCodigo).

           FIND FIRST proveedores WHERE proveedores.id_proveedor_sap = cCodigo NO-ERROR.
           
           IF NOT AVAILABLE proveedores THEN
               FIND proveedores WHERE proveedores.id_proveedor = iCodigo NO-ERROR.
           
           IF NOT AVAILABLE proveedores THEN 
               FIND proveedores WHERE substring(proveedores.cuit,1,11) = cCodigo NO-ERROR.
           
           IF NOT AVAILABLE proveedores THEN CREATE proveedores.
           
           ASSIGN proveedores.id_proveedor = iCodigo
                  proveedores.cuit = cCodigo + '0' 
                  proveedores.id_proveedor_sap = cCodigo
                  proveedores.nombre = hTable:VALUE(i,4)
                  proveedores.razon_social = proveedores.nombre
                  proveedores.pais = hTable:VALUE(i , 3)
                  proveedores.c_usuario = 'Interface Sap'.

        END.
      END.
      
      hFunctions:connection:logoff().
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saptoPuertos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saptoPuertos Procedure 
PROCEDURE saptoPuertos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE   hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i         AS INTEGER NO-UNDO.
  DEFINE VARIABLE j         AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE cCodigo   AS CHARACTER NO-UNDO.


  hFunctions = connectToSap().

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc     = hFunctions:ADD('BAPI_GET_PUERTOS').
      hTable    = hFunc:tables('VAPO').
      hNum      = hfunc:imports('O_RECNUM').

      IF hFunc:CALL() THEN DO:

        i = 0.
        FOR EACH tablas_sap WHERE tablas_sap.tabla = 'puertos'.
            DELETE tablas_sap.
            I = i + 1.
        END.

        j = INTEGER(hNum:VALUE).

        DO i = 1 TO j:
           cCodigo = hTable:VALUE(i,2).
           IF ccodigo = '' OR cCodigo = ? OR hTable:VALUE(i,3) = '' OR hTable:VALUE(i,3) = ? THEN
               NEXT.

           FIND FIRST tablas_sap WHERE tablas_sap.tabla = 'puertos' AND
                                       tablas_sap.codigo = cCodigo NO-ERROR.

           IF NOT AVAILABLE tablas_sap THEN
           DO:
               CREATE tablas_sap.
               ASSIGN tablas_sap.tabla = 'puertos'
                      tablas_sap.codigo = cCodigo.
           END.
           ASSIGN tablas_sap.descripcion = hTable:VALUE(i,3)
                  tablas_sap.abreviatura = hTable:VALUE(i,4).
        END.
      END.
      
      hFunctions:connection:logoff().
  END.
  ELSE MESSAGE 'error de conexion' VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sapToRamos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sapToRamos Procedure 
PROCEDURE sapToRamos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE   hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE cCodigo AS CHARACTER NO-UNDO.


  hFunctions = connectToSap().

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc     = hFunctions:ADD('BAPI_GET_RAMO').
      hTable    = hFunc:tables('RAMO').
      hNum      = hfunc:imports('O_RECNUM').

      IF hFunc:CALL() THEN DO:

        i = 0.
        FOR EACH tablas_sap WHERE tablas_sap.tabla = 'Ramos'.
            DELETE tablas_sap.
            I = i + 1.
        END.

        j = INTEGER(hNum:VALUE).

        DO i = 1 TO j:
           cCodigo = hTable:VALUE(i,3).
           IF ccodigo = '' OR cCodigo = ? OR hTable:VALUE(i,4) = '' OR hTable:VALUE(i,4) = ? THEN
               NEXT.

           FIND FIRST tablas_sap WHERE tablas_sap.tabla = 'Ramos' AND
                                       tablas_sap.codigo = cCodigo NO-ERROR.

           IF NOT AVAILABLE tablas_sap THEN
           DO:
               CREATE tablas_sap.
               ASSIGN tablas_sap.tabla = 'Ramos'
                      tablas_sap.codigo = cCodigo.
           END.
           ASSIGN tablas_sap.descripcion = hTable:VALUE(i,4)
                  tablas_sap.abreviatura = hTable:VALUE(i,4).
        END.
      END.
      
      hFunctions:connection:logoff().
  END.
  ELSE MESSAGE 'error de conexion' VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sapToUM) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sapToUM Procedure 
PROCEDURE sapToUM :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE OUTPUT PARAMETER iRecnum AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER cStatus AS CHARACTER NO-UNDO.

  DEFINE VARIABLE   hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE cCodigo AS CHARACTER NO-UNDO.

  DEFINE VAR iRec AS INTEGER NO-UNDO.


  hFunctions = connectToSap().

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc     = hFunctions:ADD('BAPI_GET_UNIDADES').
      hTable    = hFunc:tables('UNIDADES').
      hNum      = hfunc:imports('O_RECNUM').

      IF hFunc:CALL() THEN DO:

        i = 0.
        FOR EACH tablas_sap WHERE tablas_sap.tabla = 'UM'.
            DELETE tablas_sap.
        END.

        j = INTEGER(hNum:VALUE).


        iRecnum = 0.
        DO i = 1 TO j:
           DO TRANSACTION ON ERROR UNDO , LEAVE.
               cCodigo = hTable:VALUE(i,3).
               IF ccodigo = '' OR cCodigo = ? OR hTable:VALUE(i,7) = '' OR hTable:VALUE(i,7) = ? THEN
                   NEXT.

               FIND FIRST tablas_sap WHERE tablas_sap.tabla = 'UM' AND
                                           tablas_sap.codigo = cCodigo NO-ERROR.

               IF NOT AVAILABLE tablas_sap THEN
               DO:
                   CREATE tablas_sap.
                   ASSIGN tablas_sap.tabla = 'UM'
                          tablas_sap.codigo = cCodigo.
               END.
               ASSIGN tablas_sap.descripcion = hTable:VALUE(i,7)
                      tablas_sap.abreviatura = hTable:VALUE(i,6).
               iRecnum = iRecnum + 1.
           END.
        END.
      END.
      
      hFunctions:connection:logoff().
  END.
  ELSE cStatus = 'error de conexion'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sapToVapores) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sapToVapores Procedure 
PROCEDURE sapToVapores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE   hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE cCodigo AS CHARACTER NO-UNDO.


  hFunctions = connectToSap().

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc     = hFunctions:ADD('BAPI_GET_VAPORES').
      hTable    = hFunc:tables('VAPO').
      hNum      = hfunc:imports('O_RECNUM').

      IF hFunc:CALL() THEN DO:

        i = 0.
        FOR EACH tablas_sap WHERE tablas_sap.tabla = 'Vapores'.
            DELETE tablas_sap.
            I = i + 1.
        END.

        j = INTEGER(hNum:VALUE).

        DO i = 1 TO j:
           cCodigo = hTable:VALUE(i,2).
           IF ccodigo = '' OR cCodigo = ? OR hTable:VALUE(i,3) = '' OR hTable:VALUE(i,3) = ? THEN
               NEXT.

           FIND FIRST tablas_sap WHERE tablas_sap.tabla = 'Vapores' AND
                                       tablas_sap.codigo = cCodigo NO-ERROR.

           IF NOT AVAILABLE tablas_sap THEN DO:
               CREATE tablas_sap.
               ASSIGN tablas_sap.tabla = 'Vapores'
                      tablas_sap.codigo = cCodigo.
           END.
           ASSIGN tablas_sap.descripcion = hTable:VALUE(i,3)
                  tablas_sap.abreviatura = hTable:VALUE(i,4).
        END.
      END.
      
      hFunctions:connection:logoff().
  END.
  ELSE MESSAGE 'error de conexion' VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-satToTServicio) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE satToTServicio Procedure 
PROCEDURE satToTServicio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE OUTPUT PARAMETER iRecnum AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER cStatus AS CHARACTER NO-UNDO.

  DEFINE VARIABLE   hTable      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
  
  DEFINE VARIABLE i         AS INTEGER NO-UNDO.
  DEFINE VARIABLE j         AS INTEGER NO-UNDO.
  DEFINE VARIABLE hNum      AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE cCodigo   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE iRec      AS INTEGER NO-UNDO.


  hFunctions = connectToSap().

  IF VALID-HANDLE(hFunctions) THEN DO:
      
      hFunc     = hFunctions:ADD('BAPI_GET_UNIDADES').
      hTable    = hFunc:tables('UNIDADES').
      hNum      = hfunc:imports('O_RECNUM').

      IF hFunc:CALL() THEN DO:

        i = 0.
        FOR EACH tablas_sap WHERE tablas_sap.tabla = 'UM'.
            DELETE tablas_sap.
        END.

        j = INTEGER(hNum:VALUE).


        iRecnum = 0.
        DO i = 1 TO j:
           DO TRANSACTION ON ERROR UNDO , LEAVE.
               cCodigo = hTable:VALUE(i,3).
               IF ccodigo = '' OR cCodigo = ? OR hTable:VALUE(i,7) = '' OR hTable:VALUE(i,7) = ? THEN NEXT.

               FIND FIRST tablas_sap WHERE tablas_sap.tabla = 'UM' AND
                                           tablas_sap.codigo = cCodigo NO-ERROR.

               IF NOT AVAILABLE tablas_sap THEN DO:
                   CREATE tablas_sap.
                   ASSIGN tablas_sap.tabla = 'UM'
                          tablas_sap.codigo = cCodigo.
               END.
               ASSIGN tablas_sap.descripcion = hTable:VALUE(i,7)
                      tablas_sap.abreviatura = hTable:VALUE(i,6).
               iRecnum = iRecnum + 1.
           END.
        END.
      END.
      
      hFunctions:connection:logoff().
  END.
  ELSE cStatus = 'error de conexion'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-semanaAno) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE semanaAno Procedure 
PROCEDURE semanaAno :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pfecha AS date.
DEFINE OUTPUT PARAMETER psemana AS INTEGER.

DEFINE VAR idias AS INTEGER EXTENT 7 INITIAL [2,1,7,6,5,4,3].
DEFINE VAR fecha1 AS DATE NO-UNDO.
DEFINE VAR fecha2 AS DATE NO-UNDO.
DEFINE VAR pdia  AS INTEGER NO-UNDO.
DEFINE VAR pdia1 AS INTEGER NO-UNDO.
DEFINE VAR t AS INTEGER NO-UNDO.

DEFINE VAR bisiesto AS LOGICAL NO-UNDO.
DEFINE VAR bisiesto1 AS LOGICAL NO-UNDO.
DEFINE VAR dias AS INTEGER EXTENT 12 INITIAL [0,31,59,90,120,151,181,212,243,273,304,334].

DEFINE VAR nrodiasano AS INTEGER NO-UNDO.
DEFINE VAR nroano AS INTEGER NO-UNDO.
DEFINE VAR nrodias AS INTEGER NO-UNDO.

bisiesto = ( YEAR(pfecha) MODULO 4  = 0 AND YEAR(pfecha) MODULO 4 = 0 ) OR YEAR(pfecha) MODULO 400 = 0.
bisiesto1 = ( ( YEAR(pfecha) - 1 ) MODULO 4  = 0 AND ( YEAR(pfecha) - 1 ) MODULO 4 = 0 ) OR ( YEAR(pfecha) - 1 ) MODULO 400 = 0.

nrodiasano = dias[MONTH(pfecha)] + DAY(pfecha).

IF bisiesto AND MONTH ( pfecha ) > 2 THEN nrodiasano = nrodiasano + 1.

fecha1= DATE(1,1,YEAR(pfecha)).
pdia1 = WEEKDAY(fecha1).
pdia  = WEEKDAY(pfecha).

IF nrodiasano <= (8 - pdia1 ) AND pdia1 > 4  THEN DO:
    nroano = YEAR(pfecha) - 1.
    IF pdia1 = 5  OR ( pdia1 = 6 AND bisiesto1) THEN
        psemana = 53.
    ELSE
        psemana = 52.
END.
ELSE
   nroano = YEAR(pfecha).


IF nroano = YEAR(pfecha) THEN DO:
    IF bisiesto THEN
        nrodias = 366.
    ELSE
        nrodias = 365.
    IF nrodias - nrodiasano < ( 4 - pdia) THEN DO:
        nroano = YEAR(pfecha) + 1.
        psemana = 1.
    END.
END.

IF nroano = YEAR(pfecha) THEN DO:
    psemana = INTEGER( (nrodiasano + ( 7 - pdia ) + (pdia1 - 1)  ) / 7 ).
    IF pdia1 > 4 THEN  psemana = psemana - 1.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validaPosicionesIngreso) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validaPosicionesIngreso Procedure 
PROCEDURE validaPosicionesIngreso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pibalanza AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER piPesada  AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER piEntrega AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE centrega AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStatus  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE k AS INTEGER NO-UNDO.
    
    
    DEFINE VARIABLE hFunctions  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hFunc       AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE hEntrega    AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE htabla      AS COM-HANDLE NO-UNDO.
    
   
    DEFINE VAR cLote   AS CHARACTER NO-UNDO.
    DEFINE VAR cMaterial AS CHARACTER NO-UNDO.
    DEFINE VAR dPeso     AS DECIMAL NO-UNDO.
    DEFINE VAR viEntrega  AS INTEGER NO-UNDO.

    DEFINE VARIABLE dPesoProgress   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dPesoSap        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iProgress       AS INTEGER NO-UNDO.
    DEFINE VARIABLE iSap            AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE fitem           AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMensajeError   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE flagError          AS LOGICAL INITIAL FALSE.


    hFunctions = connectToSap().
    
    IF NOT VALID-HANDLE(hFunctions) THEN DO:
        RETURN ERROR "error de conecci¢n".
    END.

    FOR EACH ttentrega.
        DELETE ttentrega.
    END.
    
    FOR EACH ttprogress.
        DELETE ttprogress.
    END.

    hFunc       = hFunctions:ADD('BAPI_VALIDA_POS').
    hEntrega    = hFunc:EXPORTS('PIENTREGA').
    hTabla      = hFunc:tables('PT_LOTES').

    hEntrega:VALUE = piEntrega.

    IF hfunc:CALL() THEN  DO ON ERROR UNDO , LEAVE:
        k = 0.
        iSap = 0.
        REPEAT:
            k = k + 1.
            cLote = htabla:VALUE (k , 2 ) NO-ERROR.
            IF cLote = ? OR error-status:ERROR THEN  LEAVE.
            iSap = iSap + 1.
            cMaterial = htabla:VALUE(k , 6).
            dPeso     = DECIMAL ( htabla:VALUE(k , 31)) / 1000.
            viEntrega  = integer( htabla:VALUE(k , 35) ).
            dPesoSap = dPesoSap + dpeso.

            CREATE ttentrega.
            ASSIGN ttentrega.tclote = cLote
                   ttentrega.tcmaterial = cMaterial
                   ttentrega.tdpeso = dpeso.
        END.
    END.
    ELSE
    DO:
        hFunctions:connection:logoff().
        RETURN ERROR 'Error de funci¢n en SAP'.
    END.
    iProgress = 0.
    FIND FIRST balanza_pesadas WHERE  balanza_pesadas.id_balanza = pibalanza AND
                                      balanza_pesadas.id_pesada  = piPesada AND
                                      balanza_pesadas.tara       = pientrega NO-LOCK NO-ERROR.

    IF NOT AVAILABLE balanza_pesadas  THEN RETURN ERROR "Pesada inexistente para esa orden de entrega".

    FOR EACH balanza_tickets WHERE  balanza_tickets.id_balanza = balanza_pesadas.id_balanza AND
                                    balanza_tickets.id_pesada  = balanza_pesadas.id_pesada  NO-LOCK.
            dpesoProgress = dPesoProgress + balanza_tickets.peso_neto_ticket.
            iProgress = iProgress + 1.
            CREATE  ttprogress.
            ASSIGN  ttprogress.tccodbarra = balanza_tickets.cod_barra_sap
                    ttprogress.tdpeso = balanza_tickets.peso_neto_ticket.
    END.

    cMensajeError = "".
    flagError = FALSE.
    FIND FIRST ttentrega NO-ERROR.
    IF NOT AVAILABLE ttentrega  THEN RETURN ERROR 'Entrega sin items en SAP'.
    FOR EACH ttentrega.
        FIND FIRST ttprogress WHERE ttprogress.tccodbarra = ttentrega.tcmaterial + ttentrega.tclote NO-ERROR.
        IF NOT AVAILABLE ttprogress THEN
        DO:
            cMensajeError = cMensajeError + CHR(13) + "Item Lote : " + ttentrega.tclote + " no existe o no coincide en el material".
            flagError = TRUE.
        END.
        ELSE
        DO:
            IF ttentrega.tdpeso <> ttprogress.tdpeso THEN
            DO:
                cMensajeError = cMensajeError + CHR(13) + "Item Lote : " + ttentrega.tclote + " no coincide en peso ".
                flagError = TRUE.
            END.
        END.
    END.
    
    IF flagError  THEN
        RETURN ERROR cMensajeError.
    /*
    IF dPesoProgress <> dPesoSap  THEN
        RETURN ERROR "Existen diferencias de peso: " + CHR(13) +
                     "Orden Entrega Progress : " + STRING(piEntrega) + CHR(13) +
                     "Orden Entrega Sap : " + STRING(viEntrega) + CHR(13) +
                     "Items Progress : " + STRING(iProgress) + CHR(13) +
                     "Items Sap : " + STRING(iSap) + CHR(13) +
                     "Peso Progress : " + STRING(dPesoProgress, ">>>>>9.999") + CHR(13) +
                     "Peso Sap : " + STRING(dPesoSap, ">>>>>9.999").
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-verificaPedidosPacking) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE verificaPedidosPacking Procedure 
PROCEDURE verificaPedidosPacking :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-verificoPedidoSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE verificoPedidoSap Procedure 
PROCEDURE verificoPedidoSap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pDesde   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pHasta   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER PIORDEN AS INTEGER NO-UNDO.

DEFINE VARIABLE DEBUG AS LOGICAL NO-UNDO.

DEFINE VARIABLE   hSAP       AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTable     AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTable1    AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunctions AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc1     AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE i     AS INTEGER NO-UNDO.
DEFINE VARIABLE j     AS INTEGER NO-UNDO.
DEFINE VARIABLE iAnio AS INTEGER NO-UNDO.

define variable hD1 as com-handle NO-UNDO.
define variable hD2 as com-handle no-undo.
DEFINE VAR hOE AS COM-HANDLE NO-UNDO.
DEFINE VAR cV AS CHARACTER NO-UNDO.
DEFINE VAR cV1 AS CHARACTER NO-UNDO.

DEFINE VAR k AS INTEGER NO-UNDO.
DEFINE VAR cVar AS CHARACTER NO-UNDO FORMAT 'x(1024)'.
DEFINE VAR ccVar AS CHARACTER NO-UNDO FORMAT 'x(20)'.


DEFINE VAR cVapor AS CHARACTER NO-UNDO.
DEFINE VAR iLug AS INTEGER NO-UNDO.
DEFINE VAR iPuerto AS INTEGER NO-UNDO.
DEFINE VAR cDestino AS CHARACTER NO-UNDO.
DEFINE VAR iCliente AS INT64 NO-UNDO.
DEFINE VAR cFecha AS CHARACTER NO-UNDO.
DEFINE VAR iPedido AS INTEGER NO-UNDO.
DEFINE VAR iOrden AS INTEGER NO-UNDO.
DEFINE VAR iItem AS INTEGER NO-UNDO.


DEFINE VAR cEspecie AS CHARACTER NO-UNDO.
DEFINE VAR cVariedad AS CHARACTER NO-UNDO.
DEFINE VAR cEnvase AS CHARACTER NO-UNDO.
DEFINE VAR cMarca AS CHARACTER NO-UNDO.
DEFINE VAR iPaletizado AS INTEGER NO-UNDO.
DEFINE VAR cTipoPalet AS CHARACTER NO-UNDO.
DEFINE VAR cTipoEsquinero AS CHARACTER NO-UNDO.
DEFINE VAR cCategoria AS CHARACTER NO-UNDO.
DEFINE VAR iCalidad AS INTEGER NO-UNDO.
DEFINE VAR iTratamiento AS INTEGER NO-UNDO.


DEFINE VAR fdesde AS DATE NO-UNDO.
DEFINE VAR fhasta AS DATE NO-UNDO.
DEFINE VAR nm AS CHARACTER NO-UNDO.

fdesde = DATE(INTEGER(SUBSTRING(pdesde,5,2)) ,  INTEGER(substring(pdesde,7,2)) , INTEGER(SUBSTRING(pdesde,1,4)) ).
fhasta = DATE(INTEGER(SUBSTRING(phasta,5,2)) ,  INTEGER(substring(phasta,7,2)) , INTEGER(SUBSTRING(phasta,1,4)) ).

nm = SESSION:NUMERIC-FORMAT.

SESSION:NUMERIC-FORMAT = 'European'.


FOR EACH TTPED.
    DELETE TTPED.
END.
FOR EACH TTITPED.
    DELETE TTITPED.
END.

hFunctions = connectToSap().

IF VALID-HANDLE(hFunctions) THEN DO:
  

  hFunc  = hFunctions:ADD('BAPI_GET_ORDEN_FABRICACION').
  hD1    = hFunc:exports('DATE_FROM').
  hD2    = hFunc:exports('DATE_TO').
  hTable = hFunc:tables('ORDENES').

  hD1:VALUE = trim(pDesde).
  hD2:VALUE = trim(pHasta).
  
  IF hFunc:CALL() THEN DO:
      i = 0.
          

      REPEAT ON ERROR UNDO , LEAVE:

          i = i + 1.
          cV = hTable:VALUE(i,5) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN  LEAVE.
          IF cV = ? THEN   LEAVE.

          cVapor   = hTable:VALUE(i,18).
          iLug     = integer(hTable:VALUE(i,19)).
          iPuerto  = integer(hTable:VALUE(i,20)).
          cDestino = hTable:VALUE(i,16).
          iOrden   = integer(hTable:value(i,2)).
          iCliente = INT64(HTABLE:VALUE(i,4)).

          FIND FIRST ttped WHERE  ttped.id_empresa      = 1 AND
                                            ttped.id_punto_emisor = 1 AND
                                            ttped.id_orden        = iOrden  NO-ERROR.

          IF AVAILABLE ttped AND ttped.id_vapor_usa <> 0 THEN  NEXT.


          IF NOT AVAILABLE ttped  THEN DO:
              CREATE ttped.
              ASSIGN 
                  ttped.id_orden        = iOrden
                  ttped.id_empresa      = 1
                  ttped.id_punto_emisor = 1.
          END.

          FIND FIRST vapores WHERE vapores.id_vapor_sap = cVapor NO-LOCK NO-ERROR.
          FIND FIRST lugar_descarga WHERE integer(lugar_descarga.id_lugdes_sap) = iLug NO-LOCK NO-ERROR.
          FIND FIRST puertos WHERE puertos.id_puerto_sap = ipuerto NO-LOCK NO-ERROR.
          FIND FIRST destinos WHERE integer(destinos.id_destino_sap) = integer(cDestino) NO-LOCK NO-ERROR.
          FIND FIRST clientes_ventas WHERE clientes_ventas.id_cliente_sap = iCliente NO-LOCK NO-ERROR.
          

          ASSIGN ttped.union_europea    = IF hTable:VALUE(i,21) = '' THEN FALSE ELSE TRUE
                 ttped.id_vapor         = IF AVAILABLE vapores THEN vapores.id_vapor ELSE 999 
                 ttped.id_puerto_sal    = if available lugar_descarga then lugar_descarga.id_lugdes ELSE 999
                 ttped.id_pedido_sap    = hTable:value(i,2)
                 ttped.id_destino_final = if available destinos then destinos.id_destino ELSE 999
                 ttped.id_cliente       = if available clientes_ventas then clientes_ventas.id_cliente ELSE 999                                                            
                 ttped.id_mercado       = if available clientes_ventas THEN  clientes_ventas.mercado ELSE 999 
                 ttped.fecha            = date(string(hTable:VALUE(i,17)))  
                 ttped.c_usuario        = 'Interface Sap'
                 ttped.contramarca      = hTable:value(i,23)
                 ttped.china            = if hTable:value(i,22) = '' then FALSE ELSE TRUE 
                 ttped.ano              = YEAR(ttped.fecha)
                 ttped.oe               = INTEGER(hTable:VALUE(i,3))
                 ttped.OF_sap           = string(iOrden,'99999999999')
                 ttped.id_cliente_remito = 999996.
                 ttped.id_puerto_ent    = ttped.id_destino_final.
                 RUN semanaAno IN THIS-PROCEDURE (ttped.fecha , OUTPUT ttped.semana).
           ASSIGN  ttped.c_hora = STRING(NOW)
                   ttped.c_fecha = TODAY
                   ttped.pedido_traslado_sap = hTable:value(i,26)
                   ttped.posicion_pedido_sap = hTable:value(i,27).


            hFunc1    = hFunctions:ADD('BAPI_GET_CALIBRE_OE').
            hOE       = hFunc1:exports('OE').
            hTable1   = hFunc1:tables('CALIBRE').
            hOE:VALUE = string(ttped.oe,'9999999999').
            IF hFunc1:CALL() THEN DO:
              k = 0.
              REPEAT:
                  k = k + 1.
                  cV1 = hTable1:VALUE(k,2) NO-ERROR.
                  IF TTPED.ID_ORDEN = PIORDEN THEN MESSAGE "cv1" HTABLE1:VALUE(1,2) "11" INTEGER(HTABLE1:VALUE(1,11)) VIEW-AS ALERT-BOX.
                  IF ERROR-STATUS:ERROR THEN  LEAVE.
                  IF cV1 = ? THEN   LEAVE.
                  
                  IF INTEGER(htable1:VALUE(k , 11)) <> iOrden THEN  NEXT.
                      
                  iItem = integer(hTable1:VALUE(k,3)) + integer(hTable1:VALUE(k,4)).
/*                  
                  MESSAGE iItem VIEW-AS ALERT-BOX.
*/
                  FIND FIRST ttitped WHERE  ttitped.id_empresa      = 1 AND
                                                          ttitped.id_punto_emisor = 1 AND
                                                          ttitped.id_orden        = iOrden  AND
                                                          ttitped.ITEM            = iItem NO-ERROR.

                  IF NOT AVAILABLE ttitped THEN DO:
                      CREATE    ttitped.
                      ASSIGN    ttitped.id_empresa      = 1 
                                ttitped.id_punto_emisor = 1
                                ttitped.id_orden        = iOrden
                                ttitped.ITEM            = iItem.
                  END.


                  cEspecie = SUBSTRING(hTable:VALUE(i,7),1,1).
                  
                  icalidad = integer(hTable:VALUE(i,11)).

                  cVariedad      = SUBSTRING(hTable:VALUE(i,7),2,2).
                  cEnvase        = SUBSTRING(hTable:VALUE(i,7),5,3).
                  cMarca         = SUBSTRING(hTable:VALUE(i,7),9,3).
                  iPaletizado    = INTEGER(SUBSTRING(hTable:VALUE(i,7),13,3)).


                  ctipoPalet     = string(hTable:VALUE(i,13)).
                  cTipoEsquinero = string(hTable:VALUE(i,14)).
                  ccategoria     = string(hTable:VALUE(i,12)).
                  iTratamiento   = INTEGER(hTable:VALUE(i,10)).

                  FIND FIRST variedades WHERE variedades.id_articulo_sap = cEspecie AND 
                                              variedades.id_variedad_sap = cVariedad NO-LOCK NO-ERROR.
                  FIND FIRST productos_terminados WHERE productos_terminados.id_articulo_sap = cEspecie NO-LOCK NO-ERROR.
                  FIND FIRST envases_prod WHERE envases_prod.id_envase_sap = cEnvase NO-LOCK NO-ERROR.
                  FIND FIRST marcas_prod WHERE marcas_prod.id_marca_sap = cMarca NO-LOCK NO-ERROR.
                  FIND FIRST tipo_pallets WHERE tipo_pallets.id_tipo_pallet_sap = ctipoPalet NO-LOCK NO-ERROR.
                  FIND FIRST tipo_esquineros WHERE tipo_esquineros.id_tipo_esquinero_sap = cTipoEsquinero NO-LOCK NO-ERROR.
                  FIND FIRST categorias_packing WHERE categorias_packing.id_categoria_sap = cCategoria NO-LOCK NO-ERROR.
                  FIND FIRST calidades WHERE integer(calidades.id_calidad_sap) = iCalidad NO-LOCK NO-ERROR.
                  FIND FIRST caracteristicas WHERE integer(caracteristicas.id_caract_sap) = iTratamiento NO-LOCK NO-ERROR.


                  ASSIGN 
                      ttitped.semana            = ttped.semana
                      ttitped.item              = iItem
                      ttitped.id_variedad       = IF AVAILABLE variedades THEN variedades.id_variedad ELSE 999
                      ttitped.id_vapor          = ttped.id_vapor
                      ttitped.id_tipo_pallet    = IF AVAILABLE tipo_pallets THEN tipo_pallets.id_tipo_pallet ELSE 999
                      ttitped.id_tipo_esquinero = IF AVAILABLE tipo_esquineros THEN tipo_esquineros.id_tipo_esquinero ELSE 999
                      ttitped.id_punto_emisor   = ttped.id_punto_emisor
                      ttitped.id_pedido_sap     = ttped.id_pedido_sap 
                      ttitped.id_orden          = ttped.id_orden
                      ttitped.id_marca          = IF AVAILABLE marcas_prod THEN marcas_prod.id_marca ELSE 999
                      ttitped.id_envase         = IF AVAILABLE envases_prod THEN envases_prod.id_envase ELSE 999
                      ttitped.id_empresa        = ttped.id_empresa
                      ttitped.id_categoria      = IF AVAILABLE categorias_packing THEN categorias_packing.id_categoria ELSE 999
                      ttitped.id_calidad        = IF AVAILABLE calidades THEN calidades.id_calidad ELSE 999
                      ttitped.id_articulo       = IF AVAILABLE productos_terminados THEN productos_terminados.id_articulo ELSE 999 
                      ttitped.c_usuario         = ttped.c_usuario
                      ttitped.contramarca       = ttped.contramarca
                      ttitped.cant_pallets      = decimal(hTable1:VALUE(k,9)) * 1000 
                      ttitped.calibre           = replace(string(hTable1:VALUE(k,8)) , '/' , '')
                      ttitped.bultos            = ipaletizado
                      ttitped.anio              = ttped.ano 
                      ttitped.posicion_oe       = htable:VALUE(i,6)
                      ttitped.material_sap      = htable:VALUE(i,7)
                      ttitped.id_caract         = IF AVAILABLE caracteristicas THEN caracteristicas.id_caract ELSE 999. 
                      ttitped.c_hora            = STRING(NOW).
                      ttitped.c_fecha           = TODAY.

                      
                      RELEASE ttitped.

              END.
        END.
      END.
  END.
  ELSE
      MESSAGE 'ERROR' VIEW-AS ALERT-BOX ERROR.

  hFunctions:connection:logoff().
  RELEASE OBJECT hFunctions.
END.
ELSE
    MESSAGE 'No se pudo conectar a Sap' VIEW-AS ALERT-BOX ERROR.

SESSION:NUMERIC-FORMAT = nm.

FOR EACH TTPED WHERE TTPED.ID_ORDEN = PIORDEN.
    DISP TTPED WITH FRAME A.
    FOR EACH TTITPED WHERE TTITPED.ID_ORDEN = TTPED.ID_ORDEN NO-LOCK.
        DISP TTITPED WITH FRAME B.
    END.
END.

RETURN.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-verPedido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE verPedido Procedure 
PROCEDURE verPedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
----------*/

DEFINE INPUT PARAMETER pDesde   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pHasta   AS CHARACTER NO-UNDO.

DEFINE VARIABLE DEBUG AS LOGICAL NO-UNDO.

DEFINE VARIABLE   hSAP        AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTable      AS COM-HANDLE NO-UNDO.
DEFINE VAR        hTable1      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunctions  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE   hFunc1       AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.
DEFINE VARIABLE iAnio AS INTEGER NO-UNDO.

define variable hD1 as com-handle NO-UNDO.
define variable hD2 as com-handle no-undo.
DEFINE VAR hOE AS COM-HANDLE NO-UNDO.
DEFINE VAR cV AS CHARACTER NO-UNDO.
DEFINE VAR cV1 AS CHARACTER NO-UNDO.

DEFINE VAR k AS INTEGER NO-UNDO.
DEFINE VAR cVar AS CHARACTER NO-UNDO FORMAT 'x(1024)'.
DEFINE VAR ccVar AS CHARACTER NO-UNDO FORMAT 'x(20)'.


DEFINE VAR cVapor AS CHARACTER NO-UNDO.
DEFINE VAR iLug AS INTEGER NO-UNDO.
DEFINE VAR iPuerto AS INTEGER NO-UNDO.
DEFINE VAR cDestino AS CHARACTER NO-UNDO.
DEFINE VAR iCliente AS INT64 NO-UNDO.
DEFINE VAR cFecha AS CHARACTER NO-UNDO.
DEFINE VAR iPedido AS INTEGER NO-UNDO.
DEFINE VAR iOrden AS INTEGER NO-UNDO.
DEFINE VAR iItem AS INTEGER NO-UNDO.


DEFINE VAR cEspecie AS CHARACTER NO-UNDO.
DEFINE VAR cVariedad AS CHARACTER NO-UNDO.
DEFINE VAR cEnvase AS CHARACTER NO-UNDO.
DEFINE VAR cMarca AS CHARACTER NO-UNDO.
DEFINE VAR iPaletizado AS INTEGER NO-UNDO.
DEFINE VAR cTipoPalet AS CHARACTER NO-UNDO.
DEFINE VAR cTipoEsquinero AS CHARACTER NO-UNDO.
DEFINE VAR cCategoria AS CHARACTER NO-UNDO.
DEFINE VAR iCalidad AS INTEGER NO-UNDO.
DEFINE VAR iTratamiento AS INTEGER NO-UNDO.


DEFINE VAR fdesde AS DATE NO-UNDO.
DEFINE VAR fhasta AS DATE NO-UNDO.

DEFINE VAR nm AS CHARACTER NO-UNDO.


fdesde = DATE(INTEGER(SUBSTRING(pdesde,5,2)) ,  INTEGER(substring(pdesde,7,2)) , INTEGER(SUBSTRING(pdesde,1,4)) ).
fhasta = DATE(INTEGER(SUBSTRING(phasta,5,2)) ,  INTEGER(substring(phasta,7,2)) , INTEGER(SUBSTRING(phasta,1,4)) ).

nm = SESSION:NUMERIC-FORMAT.

SESSION:NUMERIC-FORMAT = 'European'.

hFunctions = connectToSap ().

IF VALID-HANDLE(hFunctions) THEN DO:
  
  hFunc     = hFunctions:ADD('BAPI_GET_ORDEN_FABRICACION').
  hD1       = hFunc:exports('DATE_FROM').
  hD2       = hFunc:exports('DATE_TO').
  hTable    = hFunc:tables('ORDENES').

  hD1:VALUE = trim(pDesde).
  hD2:VALUE = trim(pHasta).
  
  IF hFunc:CALL() THEN DO:
      i = 0.

      REPEAT ON ERROR UNDO , LEAVE:

          i = i + 1.
          cV = hTable:VALUE(i,5) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN  LEAVE.
          IF cV = ? THEN   LEAVE.

          cVapor = hTable:VALUE(i,18).
          iLug = integer(hTable:VALUE(i,19)).
          iPuerto = integer(hTable:VALUE(i,20)).
          cDestino = hTable:VALUE(i,16).
          iOrden =  integer(hTable:value(i,2)).
          iCliente = INT64(HTABLE:VALUE(i,4)).


         hFunc1    = hFunctions:ADD('BAPI_GET_CALIBRE_OE').
         hOE       = hFunc1:exports('OE').
         hTable1   = hFunc1:tables('CALIBRE').
         hOE:VALUE = hTable:VALUE(i,3).

          IF hFunc1:CALL() THEN DO:
              k = 0.

              REPEAT:
                  k = k + 1.
                  cV1 = hTable1:VALUE(k,2) NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN  LEAVE.
                  IF cV1 = ? THEN   LEAVE.

                  IF INTEGER(htable1:VALUE(k , 11)) <> iOrden THEN  NEXT.
                      DISPLAY  decimal(hTable1:VALUE(k,9)) * 1000. 
              END.
         END.
      END.
  END.
      
  hFunctions:connection:logoff().
END.

RELEASE OBJECT hFunctions.

SESSION:NUMERIC-FORMAT = nm.

RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-connectToSap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION connectToSap Procedure 
FUNCTION connectToSap RETURNS COM-HANDLE
  (  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VAR hFunctions AS COM-HANDLE NO-UNDO  .

CREATE "SAP.FUNCTIONS" hFunctions.

/***************************************************
hFunctions:connection:client            = '300'.
hFunctions:connection:system            = 'SMP'.
hFunctions:connection:systemNumber      = '00'.
hFunctions:connection:ApplicationServer = '192.168.1.230'.
hFunctions:connection:USER              = 'procesos'.
hFunctions:connection:password          = 'panasonic'.
********************************************************/

hFunctions:connection:client            = '300'.
hFunctions:connection:system            = 'SMP'.
hFunctions:connection:systemNumber      = '00'.
hFunctions:connection:ApplicationServer = '192.168.1.143'.
hFunctions:connection:USER              = 'procesos'.
hFunctions:connection:password          = 'rSA2015eli'.

IF hFunctions:connection:Logon(0 , TRUE ) THEN
    RETURN hFunctions.

RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-connectToSapCalidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION connectToSapCalidad Procedure 
FUNCTION connectToSapCalidad RETURNS COM-HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VAR hFunctions AS COM-HANDLE NO-UNDO  .

CREATE "SAP.FUNCTIONS" hFunctions.

/***************************************************
    hFunctions:connection:client            = '300'.
    hFunctions:connection:system            = 'SMQ'.
    hFunctions:connection:systemNumber      = '01'.
    hFunctions:connection:ApplicationServer = '192.168.1.236'.
    hFunctions:connection:USER              = 'marianot'.
    hFunctions:connection:password          = 'angelus5'. 
********************************************************/

hFunctions:connection:client            = '300'.
hFunctions:connection:system            = 'SMP'.
hFunctions:connection:systemNumber      = '00'.
hFunctions:connection:ApplicationServer = '192.168.1.143'.
hFunctions:connection:USER              = 'procesos'.
hFunctions:connection:password          = 'rSA2015eli'.


IF hFunctions:connection:Logon(0 , TRUE ) THEN
    RETURN hFunctions.

RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-connectToSapCapacitacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION connectToSapCapacitacion Procedure 
FUNCTION connectToSapCapacitacion RETURNS COM-HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VAR hFunctions AS COM-HANDLE NO-UNDO  .

CREATE "SAP.FUNCTIONS" hFunctions.


    hFunctions:connection:client            = '300'.
    hFunctions:connection:system            = 'SMQ'.
    hFunctions:connection:systemNumber      = '01'.
    hFunctions:connection:ApplicationServer = '192.168.1.236'.
    hFunctions:connection:USER              = 'marianot'.
    hFunctions:connection:password          = 'angelus8'.

IF hFunctions:connection:Logon(0 , TRUE ) THEN
    RETURN hFunctions.

RETURN ?.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-connectToSapDesarrollo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION connectToSapDesarrollo Procedure 
FUNCTION connectToSapDesarrollo RETURNS COM-HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VAR hFunctions AS COM-HANDLE NO-UNDO  .

CREATE "SAP.FUNCTIONS" hFunctions.

    hFunctions:connection:client            = '110'.
    hFunctions:connection:system            = 'SMD'.
    hFunctions:connection:systemNumber      = '00'.
    hFunctions:connection:ApplicationServer = '192.168.1.236'.
    hFunctions:connection:USER              = 'marianot'.
    hFunctions:connection:password          = 'angelus5'. 

IF hFunctions:connection:Logon(0 , TRUE ) THEN
    RETURN hFunctions.

RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-connectToSapInternet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION connectToSapInternet Procedure 
FUNCTION connectToSapInternet RETURNS COM-HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEFINE VAR hFunctions AS COM-HANDLE NO-UNDO  .

CREATE "SAP.FUNCTIONS" hFunctions.


    hFunctions:connection:client            = '300'.
    hFunctions:connection:system            = 'SMP'.
    hFunctions:connection:systemNumber      = '00'.
    hFunctions:connection:ApplicationServer = '/H/200.45.110.123/H/192.168.1.143'.
    hFunctions:connection:USER              = 'procesos'.
    hFunctions:connection:password          = 'rSA2015eli'.
    
IF hFunctions:connection:Logon(0 , TRUE ) THEN
    RETURN hFunctions.

RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-connectToSapTipo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION connectToSapTipo Procedure 
FUNCTION connectToSapTipo RETURNS COM-HANDLE
  ( cTipo AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VAR hFunctions AS COM-HANDLE NO-UNDO  .

CREATE "SAP.FUNCTIONS" hFunctions.


CASE ctipo:
    WHEN 'produccion' THEN
    DO:
        RETURN connectToSap().
    END.
    WHEN 'calidad' THEN
    DO:
        RETURN connectToSapCalidad().
    END.
    OTHERWISE 
    DO:
        RETURN ?.
    END.
END CASE.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-disconnectFromSAP) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION disconnectFromSAP Procedure 
FUNCTION disconnectFromSAP RETURNS LOGICAL
    (phConn AS COM-HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    phConn:connection:logoff().

    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

