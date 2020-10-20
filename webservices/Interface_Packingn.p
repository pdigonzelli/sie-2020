/****************************************************************************/
/* NOMBRE PROGRAMA......:   Interface_Packing.p                             */
/****************************************************************************/
/* Genera los Movimientos de Packing en SAP a trav‚s de una Interfaz        */
/****************************************************************************/
/* PROGRAMADOR..........:   Gabriel Navarro                                 */
/****************************************************************************/
/*
1000      Ex Fabrica Lav
1001      Dep Central Lav
1002      Dep Central Fam
1003      Ex biciclet Lav
1004      Barone
1100      RECEP FRUTA LAV
1101      CAMARA 1-FF LAV
1102      CAMARA 2-FF LAV
1103      CAMARA 3-FF LAV
1104      CAMARA 4-FF LAV
1105      CAMARA 5-FF LAV
1106      CAMARA 12-FF LAV
1107      PROD TERM-FF LAV
1108      CAMARA TESTIGO
1109      PIE MAQ-FF LAV
1110      BALANZA LAV
1200      RECEP FRUTA FAM
1201      CAMARA 3-FF FAM
1202      PIE MAQ-FF FAM
1203      BALANZA FAM
1300      SAGITARIO
1301      LATIN LEMON
1302      SIGSTAD
1303      PADILLA PABLO
1304      LA ASTURIANA 
*/    

ROUTINE-LEVEL ON ERROR UNDO, THROW.

FUNCTION CAMARA  RETURNS INTEGER (ICAMARA AS INTEGER , ISUCURSAL AS INTEGER).

    IF ISucursal <> 106 THEN RETURN 1201.
    /*       IF ICAMARA < 5 THEN
                RETURN (1100 + ICAMARA).
           ELSE
                RETURN 1106.*/
    RETURN 1102.      
END FUNCTION.

FUNCTION WLOG RETURNS LOGICAL
    ( INPUT ARCHIVO AS CHARACTER, INPUT TEXTO AS CHARACTER)  FORWARD.

FUNCTION devuelvefechaoperativa RETURNS DATE
    ( INPUT pfecha AS DATE, INPUT phora AS CHARACTER)  FORWARD.


/* Recibe Par metros de items_stock */
DEFINE INPUT PARAMETER  xSucursal   AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER  xTipoMov    AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER  xSucEnvio   AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER  xNro        AS INT64   NO-UNDO.
DEFINE INPUT PARAMETER  xTransfer   AS LOGICAL NO-UNDO.





/* Define Variables del Programa */
DEFINE VARIABLE Hlib          AS HANDLE    NO-UNDO.
DEFINE VARIABLE cCentro       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAlmacenO     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iAlmacenD     AS INTEGER   NO-UNDO.
DEFINE VARIABLE pError        AS CHARACTER NO-UNDO.
DEFINE VARIABLE pNumDoc       AS CHARACTER NO-UNDO FORMAT 'x(60)'.
DEFINE VARIABLE xArticulo     AS CHARACTER NO-UNDO.
DEFINE VARIABLE xUnidadMedida AS CHARACTER NO-UNDO.
DEFINE VARIABLE xCodBarra     AS CHARACTER NO-UNDO FORMAT 'x(30)'.
DEFINE VARIABLE xCodMovim     AS INTEGER   NO-UNDO INITIAL 2.
DEFINE VARIABLE xFecha        AS DATE.

DEFINE VAR      PISUCURSAL    AS CHARACTER NO-UNDO.
DEFINE VAR      CPARAMETROS   AS CHARACTER NO-UNDO.
DEFINE VAR      PIALMACEN     AS INTEGER   NO-UNDO.
DEFINE VAR      PIPESO        AS DECIMAL   NO-UNDO.
DEFINE VAR      PIUM          AS CHARACTER NO-UNDO.
DEFINE VAR      PIMATERIALSAP AS CHARACTER NO-UNDO FORMAT 'X(35)'.
DEFINE VAR      PILOTESAP     AS CHARACTER NO-UNDO.
DEFINE VAR      operacion     AS CHARACTER NO-UNDO.
DEFINE VAR      RES           AS CHARACTER NO-UNDO.
DEFINE VAR      XUNIDPROD     AS CHARACTER NO-UNDO FORMAT 'X(22)'.
DEFINE VAR      DFECHA        AS CHARACTER NO-UNDO FORMAT 'X(8)'.
DEFINE VAR      XMERCADO      AS CHARACTER NO-UNDO.
DEFINE VAR      DCANTIDAD     AS DECIMAL NO-UNDO.
DEFINE VAR      ICANTIDAD     AS INTEGER NO-UNDO.


DEFINE VAR      ARCHIVO       AS CHARACTER NO-UNDO.
DEFINE VAR      VTEXTO        AS CHARACTER NO-UNDO.

DEFINE VAR CPLANTAEMPAQUE AS CHARACTER NO-UNDO.
DEFINE VAR CLOTECUADRO    AS CHARACTER NO-UNDO.
DEFINE VAR CFECHAPRODUCCION AS CHARACTER NO-UNDO.
DEFINE VAR CTURNOPRODUCCION AS CHARACTER NO-UNDO.
DEFINE VAR CUNIONEUROPEA AS CHARACTER NO-UNDO.
DEFINE VAR CUSA AS CHARACTER NO-UNDO.
DEFINE VAR CCHINA AS CHARACTER NO-UNDO.
DEFINE VAR CMERCADOEXPINTIND AS CHARACTER NO-UNDO.
DEFINE VAR CFECHAVENCIMIENTO AS CHARACTER NO-UNDO.
DEFINE VAR DIR AS CHARACTER NO-UNDO.
DEFINE VAR FACTOR AS DECIMAL NO-UNDO.
DEFINE VAR VPESO AS DECIMAL NO-UNDO.
DEFINE VAR VESPECIE AS CHARACTER NO-UNDO.
DEFINE VAR VVARIEDAD AS CHARACTER NO-UNDO.
DEFINE VAR H AS INTEGER NO-UNDO.
DEFINE VARIABLE PORDEN       AS CHARACTER NO-UNDO FORMAT 'x(60)'.

DEFINE BUFFER BITEMS FOR items_stock.

/* Ejecuta la Librer¡a de Interfaces */
/*RUN LibInterfaces.p PERSISTENT SET Hlib.*/


FILE-INFO:FILE-NAME = ".".
DIR = FILE-INFO:FULL-PATHNAME.


ARCHIVO = SESSION:TEMP-DIRECTORY + STRING(XNRO).
VTEXTO = "NRO: "  + STRING(XNRO).
WLOG(ARCHIVO,VTEXTO).

VTEXTO = 'EJECUTANDO INTERFACE_PACKINGAP'.
WLOG(ARCHIVO,VTEXTO).





FIND items_stock WHERE
    items_stock.id_sucursal     = xSucursal AND
    items_stock.id_tipo_movsto  = xTipoMov  AND
    items_stock.id_suc_envio    = xSucEnvio AND
    items_stock.nro             = xNro.

WLOG(ARCHIVO,'2').


/*
IF NOT AVAILABLE items_stock THEN 
DO:
    RETURN ERROR 'ITEM_STOCK INEXISTENTE'.
END.
*/

/*****************************/
/*****************************/
/* Carga la Fecha de Proceso */
/*****************************/
/*****************************/

xFecha  = DATE(items_stock.fecha).

FIND balanza_tickets WHERE
    balanza_tickets.nro_partida = items_stock.nro_partida AND
    balanza_tickets.nro_partida_serial = 1
    NO-LOCK NO-ERROR.



/***************************/
/***************************/
/* Vaciado Playa y Camaras */
/***************************/
/***************************/

IF xTipoMov = 70 THEN 
DO:     

    
    WLOG(ARCHIVO,'3').

    IF xSucursal = 102 OR
        xSucursal = 103 OR
        xSucursal = 112 THEN xCodMovim = 9.         /* Movimiento Volcado Preseleccion */

    IF xSucursal = 104 OR
        xSucursal = 105 OR
        xSucursal = 113 THEN xCodMovim = 1.         /* Movimiento Volcado Produccion */


    IF AVAILABLE balanza_tickets THEN 
    DO:
        ASSIGN 
            PISUCURSAL    = IF balanza_tickets.id_balanza = 2 THEN 'SPL' ELSE 'SPF'     /* Centro Lavalle o Famailla */
            PIMATERIALSAP = SUBSTRING(balanza_tickets.cod_barra_sap,1,11)
            PILOTESAP     = SUBSTRING(balanza_tickets.cod_barra_sap,12).
        IF items_stock.estado_fruta THEN
            PIALMACEN   = IF balanza_tickets.id_balanza = 2 THEN 1102 ELSE 1201.         /* Camaras Lavalle o Famailla */
        ELSE    
            PIALMACEN   = IF balanza_tickets.id_balanza = 2 THEN 1101 ELSE 1200.         /* Playa Lavalle o Famailla */
        FACTOR = balanza_tickets.peso_neto_ticket / balanza_tickets.cant_env_entrada / 400.
    END.

    /*************************/
    /* Vaciado desde Camaras */
    /*************************/

    WLOG(ARCHIVO,'4').
    

    XTRANSFER = FALSE.
    IF items_stock.estado_fruta THEN  
    DO:
        
       WLOG(ARCHIVO, "LOTESAP0-" + STRING(INTEGER(items_stock.orden_entrega_sap),'9999999999')).


        FIND LAST BITEMS WHERE  BITEMS.NRO_PARTIDA = items_stock.NRO_PARTIDA AND 
                                BITEMS.orden_entrega_sap <= items_stock.orden_entrega_sap AND
                                BITEMS.ID_TIPO_MOVSTO = 78 NO-LOCK NO-ERROR.

        IF AVAILABLE BITEMS THEN
        DO:
            PILOTESAP = STRING(INTEGER(items_stock.orden_entrega_sap),'9999999999').
            XTRANSFER = TRUE.
            WLOG(ARCHIVO, "LOTESAP1-" + PILOTESAP).
        END.
        ELSE
        DO:
            FIND FIRST BITEMS WHERE  BITEMS.NRO_PARTIDA = items_stock.NRO_PARTIDA AND 
                                     BITEMS.nro_partida_serial = items_stock.nro_partida_serial AND
                                         (BITEMS.ID_TIPO_MOVSTO =  5  OR  BITEMS.ID_TIPO_MOVSTO =  71 OR BITEMS.ID_TIPO_MOVSTO =  72 OR BITEMS.ID_TIPO_MOVSTO =  77)
                                     NO-LOCK NO-ERROR.
            IF AVAILABLE BITEMS THEN
                PILOTESAP = STRING(INTEGER(BITEMS.orden_entrega_sap),'9999999999').
            ELSE
                PILOTESAP = ''. 
        END.    
        ASSIGN PIMATERIALSAP = REPLACE(SUBSTRING(balanza_tickets.cod_barra_sap,1,11), SUBSTRING(balanza_tickets.cod_barra_sap,1,6) , '303000').
    END.
    
    PIPESO = items_stock.peso * FACTOR.
    PIUM   = 'KG'.

    IF XTRANSFER THEN ASSIGN PISUCURSAL = 'SPL' PIALMACEN = 1102. 
    
    CPARAMETROS = PISUCURSAL + ',' + STRING(items_stock.NRO_PARTIDA) + ',' + TRIM(PIMATERIALSAP) + ',' + /*''*/ PILOTESAP + ',' + STRING(items_stock.PESO) + ',KG,' + STRING(PIALMACEN).
    FIND CURRENT items_stock NO-LOCK.
  
     WLOG(ARCHIVO,'5').


    OPERACION = 'pp141ingn.py '.

END.                        


/*********************************/
/*********************************/
/* Ingresos y Retornos a Camaras */
/*********************************/
/*********************************/


IF xTipoMov = 5 OR
    xTipoMov = 71 OR
    xTipoMov = 72 OR
    xTipoMov = 77 THEN 
DO:

/*    IF items_stock.estado_fruta THEN  
        /*IF xSucEnvio = 106 OR  xSucEnvio = 114 THEN DO: /*** tengo dudas respecto a esto . no todo lo de camara tiene porque estar procesado . usar items_stock.id_estado fruta ***/ */   
        ASSIGN PIMATERIALSAP = REPLACE(SUBSTRING(balanza_tickets.cod_barra_sap,1,11), SUBSTRING(balanza_tickets.cod_barra_sap,1,3) , '303'). */ 

    ASSIGN 
        PISUCURSAL    = IF xSucursal = 106 THEN 'SPL' ELSE 'SPF'     /* Centro Lavalle o Famailla */
        PILOTESAP     = SUBSTRING(balanza_tickets.cod_barra_sap,12)
        xCodMovim     = 4.
    PIMATERIALSAP = REPLACE(SUBSTRING(balanza_tickets.cod_barra_sap,1,11), SUBSTRING(balanza_tickets.cod_barra_sap,1,6) , '303000'). 
    PIALMACEN   = camara(items_stock.camara , XSUCURSAL).         /* Camara Lavalle o Famailla */

    FIND SUCURSALES WHERE SUCURSALES.ID_SUCURSAL = ITEMS_STOCK.ID_SUC_ENVIO NO-LOCK NO-ERROR.

    FIND productos_terminados OF items_stock.
    FIND variedades OF items_stock .

    FIND LOTES_PLANTACION WHERE lotes_plantacion.codigo_trazabilidad = items_stock.codigo_trazabilidad .
    FIND FIRST   lote        OF lotes_plantacion  NO-LOCK NO-ERROR.
    FIND FIRST   origenes    OF lote NO-LOCK NO-ERROR.       
    
    XUNIDPROD = "UP-" + trim(origenes.zona_up)+ "-" + string(origenes.id_finca_senasa,"9999") + '-' + string(lote.id_lote_senasa,'999') /* +  '  NC.' */.
    DFECHA = STRING(YEAR(items_stock.fecha_cosecha),'9999') + STRING(MONTH(items_stock.FECHA_COSECHA), '99') + STRING(DAY(items_stock.FECHA_COSECHA),'99').
    
    XMERCADO = IF items_stock.union_europea THEN 'UE' ELSE 'NOUE'.
    XMERCADO = IF items_stock.CHINA THEN 'CHINA' ELSE 'NOUE'.
    
    DCANTIDAD = items_stock.CANTIDAD / 20.
    
    IF DCANTIDAD = INTEGER(ROUND(TRUNCATE(DCANTIDAD , 0),0))  THEN
        ICANTIDAD = INTEGER(ROUND(TRUNCATE(DCANTIDAD , 0),0)).
    ELSE
        ICANTIDAD =  INTEGER(ROUND(TRUNCATE(DCANTIDAD , 0),0)) + 1.
    
    
    FIND FIRST colores  WHERE colores.id_color = items_stock.id_color.

    CFECHAPRODUCCION = STRING(YEAR(devuelvefechaoperativa( TODAY , STRING(TIME,'HH:MM:SS'))),'9999') + STRING(MONTH(devuelvefechaoperativa( TODAY , STRING(TIME,'HH:MM:SS'))), '99') + STRING(DAY(devuelvefechaoperativa( TODAY , STRING(TIME,'HH:MM:SS'))),'99').
    CUNIONEUROPEA   = IF balanza_tickets.union_europea THEN 'UE' ELSE 'NOUE'.
    CUSA            = IF balanza_tickets.usa THEN 'USA' ELSE 'NOUSA'.
    CCHINA          = IF balanza_tickets.china THEN 'CHINA' ELSE 'NOCHINA'.

    FIND productos_terminados WHERE productos_terminados.id_articulo = balanza_tickets.id_materia_prima NO-LOCK NO-ERROR.
    FIND variedades OF balanza_tickets NO-LOCK NO-ERROR.
    VESPECIE = productos_terminados.descripcion.
    VVARIEDAD = variedades.descripcion.

    FIND movsucu OF items_stock NO-LOCK NO-ERROR.

    CASE movsucu.id_suc_envio .
        WHEN 102 OR WHEN 104 THEN
            CPLANTAEMPAQUE = 'PC'.
        WHEN 103 OR WHEN 105 THEN
            CPLANTAEMPAQUE = 'PG'.
        WHEN 114 THEN
            CPLANTAEMPAQUE = 'PF'.
    END CASE.
    
    
    H = INTEGER(SUBSTRING(items_stock.hora,1,2)).
    IF H >= 6 AND H < 14 THEN CTURNOPRODUCCION = 'TM'.
    ELSE IF H >= 14 AND H < 22 THEN CTURNOPRODUCCION = 'TT'.
    ELSE IF  H >= 22 THEN CTURNOPRODUCCION ='TN'.
    ELSE IF H >= 0 AND H < 6 THEN 
        CTURNOPRODUCCION = 'TN'. 
    
    PILOTESAP = ''.
    CPARAMETROS =   PISUCURSAL + ',' + STRING(items_stock.NRO_PARTIDA) + ',' + TRIM(PIMATERIALSAP) + ',' + /*PILOTESAP*/ '' + ',' + STRING(ITEMS_STOCK.PESO) + ',KG,' + STRING(PIALMACEN) 
        + ',' + CPLANTAEMPAQUE     
        + ',' + VESPECIE + ',' + VVARIEDAD + ',' + items_stock.codigo_trazabilidad  
        + ',' +  /*origenes.id_origen_sap*/ balanza_tickets.FINCASAP + ',' + balanza_tickets.loteagricolaSap + ',' + XUNIDPROD + ',' + colores.ABREVIATURA + ',' + DFECHA + ',' + STRING(ICANTIDAD)
        + ',' +  CFECHAPRODUCCION + ',' + CTURNOPRODUCCION + ',' + CUNIONEUROPEA + ',' + CUSA + ',' + CCHINA + ',' + CMERCADOEXPINTIND + ',' + CFECHAVENCIMIENTO.

/*SPL,424968,50111010101,0000000100,20,KG,1109,ORDEN,01,01,MERCADO,25F0101,A3003230,LOTE SAP,UP-TU-0172-013  NC.,2,20160121,1*/
/*    OPERACION = 'python ' +  dir + '\pp140ingn.py ' + '"' + cparametros + '"'. */
    OPERACION = 'pp140ingn.py '.
    FIND CURRENT items_stock NO-LOCK.


END.                                               

VTEXTO = "ENTRO OPERACION " + OPERACION.
WLOG(ARCHIVO,VTEXTO).

VTEXTO = "PARAMETROS: "  + CPARAMETROS.
WLOG(ARCHIVO,VTEXTO).

DEFINE VAR hAppSrv AS HANDLE NO-UNDO.
DEFINE VAR RET AS LOGICAL NO-UNDO.

CREATE SERVER hAppSrv.


ret = hAppSrv:CONNECT("-H samiprogdesa -S 5162 -AppService sap2").

IF NOT ret THEN
DO:
  WLOG(ARCHIVO,'FALLO AL CONECTAR EL APP SERVER').   
  RETURN ERROR "Fallo al conectar AppServer".
END.

RUN OPERACION.P  ON SERVER hAppSrv TRANSACTION DISTINCT (INPUT operacion , INPUT CPARAMETROS, OUTPUT  RES) NO-ERROR.
IF ERROR-STATUS:ERROR THEN
    UNDO , RETURN ERROR NEW Progress.Lang.AppError(RETURN-VALUE,550).

VTEXTO = "RESULTADO OPERACION: " + RES.
WLOG(ARCHIVO,VTEXTO).

VTEXTO = "SALIO OPERACION".
WLOG(ARCHIVO,VTEXTO).

PNUMDOC = RES.  
IF items_stock.id_tipo_movsto = 70 THEN
DO:
    PORDEN = ENTRY(3,PNUMDOC,'&') NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        UNDO , RETURN ERROR NEW Progress.Lang.AppError('ERROR: NO DEVUELVE ORDENMTS EN VOLCADO ',550).
    END.
END. 

IF xTipoMov = 5 OR
    xTipoMov = 71 OR
    xTipoMov = 72 OR
    xTipoMov = 77 THEN
DO:
    PORDEN = ENTRY(2,PNUMDOC,'&') NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        UNDO , RETURN ERROR NEW Progress.Lang.AppError('ERROR: NO DEVUELVE LOTE EN INGRESO ',550).
    END.
END.     

IF pNumDoc <> '' THEN
        ASSIGN
            items_stock.orden_entrega_sap = PORDEN
            items_stock.documento_sap = IF ( items_stock.documento_sap <> "" ) THEN
            items_stock.documento_sap + ";" + trim(string(xCodMovim)) + "|" + TRIM(pNumDoc)
        ELSE
            trim(string(xCodMovim)) + "|" + TRIM(pNumDoc).
ELSE
DO:
    UNDO , RETURN ERROR NEW Progress.Lang.AppError('ERROR: MOVIMIENTO INCORRECTO. NO DEVUELVE DOCUMENTO DE MATERIAL',550).
END.


RETURN.                       

CATCH E AS Progress.Lang.Error.
    VTEXTO = "ERROR: "  + E:GetMessage(1).
    WLOG(ARCHIVO,VTEXTO).
    UNDO, THROW E .
END CATCH.

FINALLY.
    VTEXTO = "SALIO".
    WLOG(ARCHIVO,VTEXTO).
    IF hAppSrv:CONNECTED () THEN
        ret = hAppSrv:DISCONNECT().
    DELETE OBJECT hAppSrv.

    RELEASE items_stock.
END FINALLY.

    
/******  
    
    
    


    








/**********************************/
/**********************************/
/* Movimientos a Camaras o Playas */
/**********************************/
/**********************************/

IF xTipoMov = 79 THEN DO:

    xCodMovim   = 3.                                                            /* Movimiento a Camaras o Playa */
    
    /***********************/
    /* Movimiento a Playas */
    /***********************/
    IF xSucursal = 101 OR
        xSucursal = 111 THEN DO:

        IF items_stock.estado_fruta THEN DO:
            ASSIGN 
                xCodBarra   = IF xSucursal = 101 THEN
                    (xCodBarra + 'PROCESADOS') ELSE (xCodBarra + 'PROCES-FAM')  /* Estado de la Fruta */
                cCentro     = IF xSucursal = 101 THEN 'A100' ELSE 'A200'        /* Centro Lavalle o Famailla */
                iAlmacenO   = IF xSucursal = 101 THEN 1800 ELSE 2800            /* Camaras Lavalle o Famailla */
                iAlmacenD   = IF xSucursal = 101 THEN 1010 ELSE 2210.           /* Playa Lavalle o Famailla */
        END.
        ELSE DO:
            FIND balanza_tickets WHERE
                balanza_tickets.nro_partida = items_stock.nro_partida AND
                balanza_tickets.nro_partida_serial = 1
                NO-LOCK NO-ERROR.
            
            IF AVAILABLE balanza_tickets THEN DO:
                ASSIGN 
                    xCodBarra   = TRIM(balanza_tickets.cod_barra_sap)           /* Estado de la Fruta */
                    cCentro     = IF xSucursal = 101 THEN 'A100' ELSE 'A200'    /* Centro Lavalle o Famailla */
                    iAlmacenO   = IF xSucursal = 101 THEN 1800 ELSE 2800        /* Camaras Lavalle o Famailla */
                    iAlmacenD   = IF xSucursal = 101 THEN 1010 ELSE 2210.       /* Playa Lavalle o Famailla */
            END.
        END.
    END.
    
    /************************/
    /* Movimiento a Camaras */
    /************************/
    ELSE DO:
        FIND balanza_tickets WHERE
            balanza_tickets.nro_partida = items_stock.nro_partida AND
            balanza_tickets.nro_partida_serial = 1
            NO-LOCK NO-ERROR.

        IF AVAILABLE balanza_tickets THEN DO:
            ASSIGN 
                xCodBarra   = TRIM(balanza_tickets.cod_barra_sap)               /* Estado de la Fruta */
                cCentro     = IF xSucursal = 106 THEN 'A100' ELSE 'A200'        /* Centro Lavalle o Famailla */
                iAlmacenO   = IF xSucursal = 106 THEN 1010 ELSE 2210            /* Playa Lavalle o Famailla */
                iAlmacenD   = IF xSucursal = 106 THEN 1800 ELSE 2800.            /* Camaras Lavalle o Famailla */
        END.
    END.


    RUN movinternos IN Hlib (xCodBarra , xFecha ,
                            cCentro , iAlmacenO , iAlmacenD ,
                            items_stock.peso , 'KG' , xCodMovim , '' , '' , 
                            OUTPUT pError , OUTPUT pNumDoc) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
/*        
        ASSIGN items_stock.c_usuario = "MOV".
*/        
        MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.

    IF pNumDoc <> '' THEN
        ASSIGN
            items_stock.documento_sap =
            IF ( items_stock.documento_sap <> "" ) THEN
                items_stock.documento_sap + ";" + trim(string(xCodMovim)) + "|" + SUBSTR(pNumDoc,1,10)
            ELSE
                trim(string(xCodMovim)) + "|" + SUBSTR(pNumDoc,1,10).
END.                                               



/**********************************/
/**********************************/
/* Descarte desde Playa y Camaras */
/**********************************/
/**********************************/
IF xTipoMov = 73 THEN DO:
    
    xCodMovim   = 1.                                                                /* Movimiento Descarte a Industria */

    /********************/
    /* Descarte Camaras */
    /********************/
    IF xSucEnvio = 106 OR
        xSucEnvio = 114 THEN DO:

        IF items_stock.estado_fruta THEN DO:
            ASSIGN 
                xCodBarra   = IF xSucEnvio = 106 THEN
                    (xCodBarra + 'PROCESADOS') ELSE (xCodBarra + 'PROCES-FAM')      /* Estado de la Fruta */
                cCentro     = IF xSucEnvio = 106 THEN 'A100' ELSE 'A200'            /* Centro Lavalle o Famailla */
                iAlmacenO   = IF xSucEnvio = 106 THEN 1800 ELSE 2800                /* Camaras Lavalle o Famailla */
                iAlmacenD   = IF xSucEnvio = 106 THEN 1900 ELSE 2900.               /* Silos Descarte Lavalle o Famailla */
        END.
        ELSE DO:
            FIND balanza_tickets WHERE
                balanza_tickets.nro_partida = items_stock.nro_partida AND
                balanza_tickets.nro_partida_serial = 1
                NO-LOCK NO-ERROR.

            IF AVAILABLE balanza_tickets THEN DO:
                ASSIGN 
                    xCodBarra   = TRIM(balanza_tickets.cod_barra_sap)                           /* Estado de la Fruta */
                    cCentro     = IF balanza_tickets.id_balanza = 2 THEN 'A100' ELSE 'A200'     /* Centro Lavalle o Famailla */
                    iAlmacenO   = IF balanza_tickets.id_balanza = 2 THEN 1800 ELSE 2800         /* Camaras Lavalle o Famailla */
                    iAlmacenD   = IF balanza_tickets.id_balanza = 2 THEN 1900 ELSE 2900.        /* Silos Descarte Lavalle o Famailla */
            END.
        END.
    END.

    /*******************/
    /* Descarte Playas */
    /*******************/
    ELSE DO:
        FIND balanza_tickets WHERE
            balanza_tickets.nro_partida = items_stock.nro_partida AND
            balanza_tickets.nro_partida_serial = 1
            NO-LOCK NO-ERROR.

        IF AVAILABLE balanza_tickets THEN DO:
            ASSIGN 
                xCodBarra   = TRIM(balanza_tickets.cod_barra_sap)                           /* Estado de la Fruta */
                cCentro     = IF balanza_tickets.id_balanza = 2 THEN 'A100' ELSE 'A200'     /* Centro Lavalle o Famailla */
                iAlmacenO   = IF balanza_tickets.id_balanza = 2 THEN 1010 ELSE 2210         /* Playa Lavalle o Famailla */
                iAlmacenD   = IF balanza_tickets.id_balanza = 2 THEN 1900 ELSE 2900.        /* Silos Descarte Lavalle o Famailla */
        END.
    END.

/*    
    RUN movinternos IN Hlib (xCodBarra , xFecha ,
                            cCentro , iAlmacenO , iAlmacenD ,
                            items_stock.peso , 'KG' , xCodMovim , '' , '' , 
                            OUTPUT pError , OUTPUT pNumDoc) NO-ERROR.
*/

    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN items_stock.c_usuario = "MOV".
        MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.

    IF pNumDoc <> '' THEN
        ASSIGN
            items_stock.documento_sap =
            IF ( items_stock.documento_sap <> "" ) THEN
                items_stock.documento_sap + ";" + trim(string(xCodMovim)) + "|" + SUBSTR(pNumDoc,1,10)
            ELSE
                trim(string(xCodMovim)) + "|" + SUBSTR(pNumDoc,1,10).
END.



/*****************************/
/*****************************/
/* Enviados y Ventas Camaras */
/*****************************/
/*****************************/

IF xTipoMov = 78 OR
    xTipoMov = 80 THEN DO:
    
    xCodMovim   = 5.                                                                /* Movimientos Despachos y Ventas */

    IF xSucEnvio = 106 OR
        xSucEnvio = 114 THEN DO:

        IF items_stock.estado_fruta THEN DO:
            ASSIGN 
                xCodBarra   = IF xSucEnvio = 106 THEN
                    (xCodBarra + 'PROCESADOS') ELSE (xCodBarra + 'PROCES-FAM')      /* Estado de la Fruta */
                cCentro     = IF xSucEnvio = 106 THEN 'A100' ELSE 'A200'            /* Centro Lavalle o Famailla */
                iAlmacenO   = IF xSucEnvio = 106 THEN 1800 ELSE 2800                /* Camaras Lavalle o Famailla */
                iAlmacenD   = IF xSucEnvio = 106 THEN 1010 ELSE 2210.               /* Playa Lavalle o Famailla */
        END.


        RUN movinternos IN Hlib (xCodBarra , xFecha ,
                                cCentro , iAlmacenO , iAlmacenD ,
                                items_stock.peso , 'KG' , xCodMovim , '' , '' , 
                                OUTPUT pError , OUTPUT pNumDoc) NO-ERROR.
    
        IF ERROR-STATUS:ERROR THEN DO:
/*        
            IF xTipoMov = 78 THEN
                ASSIGN items_stock.c_usuario = "ENV".
            IF xTipoMov = 80 THEN
                ASSIGN items_stock.c_usuario = "VTA".
*/        
            MESSAGE RETURN-VALUE ERROR-STATUS VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END.
    
        IF pNumDoc <> '' THEN
            ASSIGN
                items_stock.documento_sap =
                IF ( items_stock.documento_sap <> "" ) THEN
                    items_stock.documento_sap + ";" + trim(string(xCodMovim)) + "|" + SUBSTR(pNumDoc,1,10)
                ELSE
                    trim(string(xCodMovim)) + "|" + SUBSTR(pNumDoc,1,10).

    END.
END.



/*****************************/
/*****************************/
/* Podridos y Mermas Camaras */
/*****************************/
/*****************************/

IF xTipoMov = 75 OR
    xTipoMov = 76 THEN DO:
    
    IF xSucursal <> 109 AND
        xSucursal <> 119 THEN LEAVE.
    
    xCodMovim   = 11.                                                           /* Movimientos Podridos y Mermas */
    
    IF items_stock.estado_fruta THEN DO:
        ASSIGN 
            xCodBarra   = IF xSucursal = 109 THEN
                (xCodBarra + 'PROCESADOS') ELSE (xCodBarra + 'PROCES-FAM')      /* Estado de la Fruta */
            cCentro     = IF xSucursal = 109 THEN 'A100' ELSE 'A200'            /* Centro Lavalle o Famailla */
            iAlmacenO   = IF xSucursal = 109 THEN 1800 ELSE 2800                /* Camaras Lavalle o Famailla */
            iAlmacenD   = IF xSucursal = 109 THEN 1800 ELSE 2800.               /* Camaras Lavalle o Famailla */
    END.
    ELSE DO:
        FIND balanza_tickets WHERE
            balanza_tickets.nro_partida = items_stock.nro_partida AND
            balanza_tickets.nro_partida_serial = 1
            NO-LOCK NO-ERROR.

        IF AVAILABLE balanza_tickets THEN DO:
            ASSIGN 
                xCodBarra   = TRIM(balanza_tickets.cod_barra_sap)                           /* Estado de la Fruta */
                cCentro     = IF balanza_tickets.id_balanza = 2 THEN 'A100' ELSE 'A200'     /* Centro Lavalle o Famailla */
                iAlmacenO   = IF balanza_tickets.id_balanza = 2 THEN 1800 ELSE 2800         /* Camaras Lavalle o Famailla */
                iAlmacenD   = IF balanza_tickets.id_balanza = 2 THEN 1800 ELSE 2800.        /* Camaras Lavalle o Famailla */
        END.
    END.

    
    RUN movinternos IN Hlib (xCodBarra , xFecha ,
                            cCentro , iAlmacenO , iAlmacenD ,
                            items_stock.peso , 'KG' , xCodMovim , '' , '' , 
                            OUTPUT pError , OUTPUT pNumDoc) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
/*        
        IF xTipoMov = 75 THEN
            ASSIGN items_stock.c_usuario = "POD".
        IF xTipoMov = 75 THEN
            ASSIGN items_stock.c_usuario = "MER".
*/        
        MESSAGE RETURN-VALUE ERROR-STATUS VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.

    IF pNumDoc <> '' THEN
        ASSIGN
            items_stock.documento_sap =
            IF ( items_stock.documento_sap <> "" ) THEN
                items_stock.documento_sap + ";" + trim(string(xCodMovim)) + "|" + SUBSTR(pNumDoc,1,10)
            ELSE
                trim(string(xCodMovim)) + "|" + SUBSTR(pNumDoc,1,10).
END.

IF AVAILABLE items_stock THEN DO:
    {logmovinternos.i}
END.
*/

/* DELETE PROCEDURE Hlib. */

FUNCTION WLOG RETURNS LOGICAL (INPUT ARCHIVO AS CHARACTER, INPUT TEXTO AS CHARACTER):
    OUTPUT TO VALUE(ARCHIVO) APPEND.
    EXPORT DELIMITER "|" STRING(NOW)  TEXTO.
    OUTPUT CLOSE.
    RETURN TRUE.
END FUNCTION.

FUNCTION devuelvefechaoperativa RETURNS DATE
    ( INPUT pfecha AS DATE, INPUT phora AS CHARACTER) :
    /*------------------------------------------------------------------------------
      Purpose:   
        Notes:  
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE vfechaop AS DATE.
  
    FIND FIRST fechas_operativas WHERE
        (fechas_operativas.fecha_inicio = pfecha AND phora >= fechas_operativas.hora_inicio) OR
        (fechas_operativas.fecha_fin = pfecha AND phora <= fechas_operativas.hora_fin)  NO-LOCK NO-ERROR.
    IF  AVAILABLE fechas_operativas THEN 
        vfechaop = fechas_operativas.fecha_inicio.



    RETURN vfechaop.
END FUNCTION.     
