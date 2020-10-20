/****************************************************************************/
/*  NOMBRE PROGRAMA......:   dd_cierra.p                                    */
/****************************************************************************/
/*  Programa para cierre de volcado con lectura de codigo de barras         */
/****************************************************************************/
/*  PROGRAMADOR..........:   Gabriel Navarro                                */
/****************************************************************************/

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER x_destino    LIKE movsucu.id_sucursal.
DEFINE INPUT PARAMETER x_partida    LIKE volcado_packing.nro_partida.
DEFINE INPUT PARAMETER xPartida_s   LIKE volcado_packing.nro_partida_serial.

/*
DEFINE VARIABLE x_destino   LIKE movsucu.id_sucursal.
DEFINE VARIABLE x_partida   LIKE volcado_packing.nro_partida.
DEFINE VARIABLE xPartida_s  LIKE volcado_packing.nro_partida_serial.

x_destino   = 104.
x_partida   = 424426.
xPartida_s  = 2.
*/

SESSION:TIME-SOURCE = "produccion".

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE x_fec_oper  LIKE movsucu.fecha_operativa    NO-UNDO.
DEFINE VARIABLE x_movsto    LIKE movsucu.id_tipo_movsto     NO-UNDO.
DEFINE VARIABLE x_origen    LIKE movsucu.id_suc_envio       NO-UNDO.
DEFINE VARIABLE x_envase    LIKE items_stock.id_envase      NO-UNDO.
DEFINE VARIABLE x_partunica LIKE items_stock.nro_partida    NO-UNDO.

DEFINE VARIABLE x_cant      LIKE saldos_packing.saldo       NO-UNDO.
DEFINE VARIABLE x_numero    LIKE movsucu.nro                NO-UNDO.
DEFINE VARIABLE x_num_envio LIKE movsucu.nro                NO-UNDO.
DEFINE VARIABLE x_stock     AS LOGICAL INITIAL FALSE        NO-UNDO.
DEFINE VARIABLE x_uenoue    AS CHARACTER                    NO-UNDO.
DEFINE VARIABLE xSucursal   AS INTEGER                      NO-UNDO.
DEFINE VARIABLE xTipoMov    LIKE items_stock.id_tipo_movsto NO-UNDO.

DEFINE BUFFER aux_movsucu   FOR movsucu.
DEFINE BUFFER aux_items     FOR items_stock.
DEFINE BUFFER aux_items_1   FOR items_stock.
DEFINE BUFFER aux_volcado   FOR volcado_packing.

IF SUBSTR(STRING(TIME,"hh:mm:ss"),1,2) + "00" < "0600" THEN
    x_fec_oper = TODAY - 1.
ELSE
    x_fec_oper = TODAY.

x_movsto    = 70.
x_envase    = 10.


/***********************************************************/
/*   Graba Datos de Cierre de Volcado de saldos_packing    */
/***********************************************************/

FIND saldos_packing WHERE
    saldos_packing.nro_partida = x_partida AND
    saldos_packing.nro_partida_serial = xPartida_s
    NO-ERROR.

IF AVAILABLE saldos_packing AND saldos_packing.abierto THEN DO
    TRANSACTION ON ERROR UNDO , RETURN ERROR:
    
    x_cant      = saldos_packing.saldo.
    xSucursal   = saldos_packing.id_sucursal.

    FIND LAST volcado_packing USE-INDEX volcado_packing WHERE
        volcado_packing.nro_partida = x_partida AND
        volcado_packing.nro_partida_serial = xPartida_s AND
        volcado_packing.abierto
        NO-ERROR.

    IF AVAILABLE volcado_packing THEN DO:
        CREATE proceso_volcado.
        ASSIGN
            proceso_volcado.nro_partida         = volcado_packing.nro_partida
            proceso_volcado.nro_partida_serial  = volcado_packing.nro_partida_serial
            proceso_volcado.nro_volcado         = volcado_packing.nro_volcado
            proceso_volcado.id_sucursal         = volcado_packing.id_sucursal
            proceso_volcado.id_tipo_movsto      = x_movsto
            proceso_volcado.nro                 = 0
            proceso_volcado.id_suc_envio        = volcado_packing.id_sucursal_etiqueta
            proceso_volcado.cantidad            = x_cant
            proceso_volcado.fecha_operativa     = x_fec_oper
            proceso_volcado.fecha               = TODAY
            proceso_volcado.hora                = SUBSTR(STRING(TIME,"hh:mm:ss"),1,2) + SUBSTR(STRING(TIME,"hh:mm:ss"),4,2)
            proceso_volcado.hora_cierre         = STRING(TIME,"hh:mm:ss")
            proceso_volcado.inicio_volcado      = volcado_packing.inicio_ultimo_proceso
            proceso_volcado.fin_volcado         = NOW.

        ASSIGN
            saldos_packing.cantidad_volcada     = saldos_packing.cantidad_volcada + proceso_volcado.cantidad
            saldos_packing.saldo                = saldos_packing.saldo - proceso_volcado.cantidad.

        IF xSucursal = 106 OR xSucursal = 114 THEN
            ASSIGN
                saldos_packing.cantidad_salida  = saldos_packing.cantidad_salida + proceso_volcado.cantidad
                saldos_packing.saldo_camara     = saldos_packing.saldo_camara - proceso_volcado.cantidad.

        ASSIGN
            volcado_packing.cantidad            = volcado_packing.cantidad + proceso_volcado.cantidad
            volcado_packing.fin_ultimo_proceso  = NOW
            volcado_packing.abierto             = FALSE.
        
        x_uenoue = volcado_packing.letra_color.

        FOR EACH aux_volcado WHERE
            aux_volcado.nro_partida = x_partida AND
            aux_volcado.nro_partida_serial = xPartida_s AND
            aux_volcado.abierto:
        
            ASSIGN aux_volcado.abierto  = FALSE.
        END.
        
        /****************/
        RUN graba_volcado NO-ERROR.
        /****************/
        
        IF ERROR-STATUS:ERROR THEN
            UNDO , RETURN.

        IF saldos_packing.saldo = 0 THEN
            ASSIGN saldos_packing.abierto   = FALSE.
    END.
END.

RELEASE movsucu.
RELEASE items_stock.
RELEASE tipo_numero.
RELEASE aux_movsucu.
RELEASE aux_items.
RELEASE saldos_packing.
RELEASE volcado_packing.
RELEASE proceso_volcado.


/***************************************************************/
/***************************************************************/
/***************************************************************/
/*   Graba Datos de Cierre de Volcado de Packing en movsucu    */
/***************************************************************/
/***************************************************************/
/***************************************************************/

PROCEDURE graba_volcado.


/******** Crea Movimiento ********/

    FIND r_cendis_suc WHERE
        r_cendis_suc.id_cendis > 1 AND
        r_cendis_suc.id_cendis < 4 AND
        r_cendis_suc.id_sucursal = proceso_volcado.id_suc_envio
        NO-LOCK NO-ERROR.
    
    IF AVAILABLE r_cendis_suc THEN DO:
        IF r_cendis_suc.llevar_stock THEN
            x_stock = TRUE.
        ELSE
            x_stock = FALSE.
    END.

    CREATE movsucu.
    CREATE items_stock.


/**********  movsucu **********/
    ASSIGN
         movsucu.id_sucursal     = x_destino
         movsucu.id_tipo_movsto  = x_movsto
         movsucu.id_suc_origen   = x_destino
         movsucu.id_suc_envio    = proceso_volcado.id_suc_envio
         movsucu.fecha           = proceso_volcado.fecha
         movsucu.hora            = proceso_volcado.hora
         movsucu.fecha_operativa = proceso_volcado.fecha_operativa
         movsucu.fecha_proceso   = ?
         movsucu.c_fecha         = proceso_volcado.fecha
         movsucu.c_hora          = proceso_volcado.hora_cierre.


/******* Graba Ingreso *******/
    FIND tipo_numero WHERE
        tipo_numero.id_sucursal     = movsucu.id_sucursal AND
        tipo_numero.id_tipo_movsto  = movsucu.id_tipo_movsto
        NO-ERROR.

    IF AVAILABLE tipo_numero THEN
        x_numero = tipo_numero.nro + 1.
    ELSE
        x_numero = 1.
    
    ASSIGN
        tipo_numero.nro     = x_numero
        movsucu.nro         = x_numero
        proceso_volcado.nro = x_numero.


    IF x_stock THEN DO:
        /******* Graba Egreso *******/
        FIND tipo_numero WHERE
            tipo_numero.id_sucursal = movsucu.id_suc_envio AND
            tipo_numero.id_tipo_movsto = 4
            NO-ERROR.

        IF AVAILABLE tipo_numero THEN
            x_num_envio = tipo_numero.nro + 1.
        ELSE
            x_num_envio = 1.
    
        ASSIGN movsucu.nro_envio = x_num_envio.
    
        CREATE aux_movsucu.
        ASSIGN
            aux_movsucu.id_sucursal     = movsucu.id_suc_envio
            aux_movsucu.id_tipo_movsto  = 4
            aux_movsucu.nro             = x_num_envio
            aux_movsucu.id_suc_origen   = movsucu.id_suc_envio
            aux_movsucu.id_suc_envio    = movsucu.id_sucursal
            aux_movsucu.nro_envio       = movsucu.nro
            aux_movsucu.fecha           = movsucu.fecha
            aux_movsucu.hora            = movsucu.hora
            aux_movsucu.fecha_operativa = movsucu.fecha_operativa
            aux_movsucu.fecha_proceso   = ?
            aux_movsucu.c_usuario       = movsucu.c_usuario
            aux_movsucu.c_fecha         = movsucu.c_fecha
            aux_movsucu.c_hora          = movsucu.c_hora.
                
        ASSIGN tipo_numero.nro  = x_num_envio.
    END.


/******* Controla carga de items_stock *******/
    FIND FIRST aux_items_1 WHERE
        aux_items_1.nro_partida         = x_partida AND
        aux_items_1.Nro_partida_serial  = xPartida_s
        NO-LOCK NO-ERROR.
    xTipoMov    = aux_items_1.id_tipo_movsto.

    ASSIGN
        items_stock.id_sucursal             = movsucu.id_sucursal
        items_stock.id_tipo_movsto          = movsucu.id_tipo_movsto
        items_stock.id_suc_envio            = movsucu.id_suc_envio
        items_stock.nro                     = movsucu.nro
        items_stock.item                    = 1
        items_stock.fecha                   = movsucu.fecha
        items_stock.hora                    = movsucu.hora
        items_stock.fecha_operativa         = movsucu.fecha_operativa
        items_stock.dia                     = WEEKDAY( movsucu.fecha)
        items_stock.id_sucursal_etiqueta    = aux_items_1.id_sucursal_etiqueta
        items_stock.id_etiqueta             = aux_items_1.id_etiqueta
        items_stock.nro_partida             = aux_items_1.nro_partida
        items_stock.nro_partida_serial      = aux_items_1.nro_partida_serial
        items_stock.nro_partida_origen      = aux_items_1.nro_partida_origen
        items_stock.codigo_stock            = "I"
        items_stock.id_color                = aux_items_1.id_color
        items_stock.nro_comprobante         = aux_items_1.nro_comprobante
        items_stock.id_articulo             = aux_items_1.id_articulo
        items_stock.id_variedad             = aux_items_1.id_variedad
        items_stock.fecha_cosecha           = aux_items_1.fecha_cosecha
        items_stock.id_tipo_cosecha         = aux_items_1.id_tipo_cosecha
        items_stock.estado_fruta            = aux_items_1.estado_fruta
        items_stock.periodo_cosecha         = aux_items_1.periodo_cosecha
        items_stock.codigo_trazabilidad     = aux_items_1.codigo_trazabilidad
        items_stock.id_proveedor            = aux_items_1.id_proveedor
        items_stock.id_origen               = aux_items_1.id_origen
        items_stock.id_lote                 = aux_items_1.id_lote
        items_stock.union_europea           = aux_items_1.union_europea
        items_stock.china                   = aux_items_1.china
        items_stock.certificado             = aux_items_1.certificado
        items_stock.cert_china              = aux_items_1.cert_china
        items_stock.id_finca_senasa         = aux_items_1.id_finca_senasa
        items_stock.id_lote_senasa          = aux_items_1.id_lote_senasa
        items_stock.camara                  = aux_items_1.camara
        items_stock.id_envase               = x_envase
        items_stock.cantidad                = x_cant
        items_stock.cantidad1               = x_cant
        items_stock.c_usuario               = aux_items_1.c_usuario
        items_stock.c_fecha                 = aux_items_1.c_fecha
        items_stock.c_hora                  = aux_items_1.c_hora.

    IF x_uenoue = "N" THEN
        ASSIGN items_stock.consumido        = TRUE.

    FIND r_envases_prod WHERE
        r_envases_prod.id_envase    = items_stock.id_envase AND
        r_envases_prod.id_articulo  = items_stock.id_articulo
        NO-LOCK NO-ERROR.
    
    ASSIGN items_stock.peso = r_envases_prod.kilos * items_stock.cantidad.

    IF items_stock.peso <> 0 THEN
        ASSIGN items_stock.cantidad1    = items_stock.peso / 20.

    IF items_stock.id_serial <> 0 THEN
        ASSIGN items_stock.id_serial    = NEXT-VALUE(stock, general).


    /***********************/
    RUN graba_partida_remito.
    /***********************/

/******* Busca registro de  movsucu y asigna el peso *******/
    FIND movsucu WHERE
        movsucu.id_sucursal     = items_stock.id_sucursal AND
        movsucu.id_tipo_movsto  = items_stock.id_tipo_movsto AND
        movsucu.id_suc_envio    = items_stock.id_suc_envio AND
        movsucu.nro             = items_stock.nro
        NO-ERROR.

    ASSIGN movsucu.peso = items_stock.peso.

    IF x_stock THEN DO:
        /******* Contrapartida  items_stock *******/
        CREATE aux_items.

        /***************/
        RUN graba_egreso.
        /***************/

        /******* Contrapartida  movsucu y asignacion del peso total *******/
        FIND aux_movsucu WHERE
            aux_movsucu.id_sucursal     = aux_items.id_sucursal AND
            aux_movsucu.id_tipo_movsto  = aux_items.id_tipo_movsto AND
            aux_movsucu.id_suc_envio    = aux_items.id_suc_envio AND
            aux_movsucu.nro             = aux_items.nro
            NO-ERROR.
        
        ASSIGN
            aux_movsucu.fecha   = aux_items.fecha
            aux_movsucu.hora    = aux_items.hora
            aux_movsucu.peso    = aux_items.peso.
    END.

    x_origen    = movsucu.id_suc_envio.


/*****************************/
/******* Interface SAP *******/
/*****************************/
/*    IF xTipoMov = 79 AND 
        x_movsto = 70 THEN
        RUN Interface_Packingn.p
            (INPUT x_destino,
            INPUT x_movsto,
            INPUT x_origen,
            INPUT x_numero) NO-ERROR.
    ELSE */
    RUN Interface_Packing.p
       (INPUT x_destino,
        INPUT x_movsto,
        INPUT x_origen,
        INPUT x_numero) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
        UNDO ,  RETURN ERROR.

END PROCEDURE.


/******************************************************/
/******************************************************/
/*************** Graba Partida y Remito ***************/
/******************************************************/
/******************************************************/
PROCEDURE graba_partida_remito:

    IF items_stock.nro_comprobante <> "" AND 
        items_stock.nro_comprobante <> ? AND
        items_stock.id_tipo_movsto = 70 THEN DO:

        FIND FIRST partidas_remitos WHERE
            partidas_remitos.fecha_operativa        = items_stock.fecha_operativa       AND
            partidas_remitos.id_sucursal            = items_stock.id_sucursal           AND
            partidas_remitos.id_tipo_movsto         = items_stock.id_tipo_movsto        AND
            partidas_remitos.id_suc_envio           = items_stock.id_suc_envio          AND
            partidas_remitos.nro_partida            = items_stock.nro_partida           AND
            partidas_remitos.nro_comprobante        = items_stock.nro_comprobante       AND
            partidas_remitos.codigo_trazabilidad    = items_stock.codigo_trazabilidad   AND
            partidas_remitos.union_europea          = items_stock.union_europea
            NO-ERROR.

        IF AVAILABLE partidas_remitos THEN DO:
            ASSIGN
                partidas_remitos.cantidad       = partidas_remitos.cantidad + items_stock.cantidad1
                partidas_remitos.saldo          = partidas_remitos.saldo + items_stock.cantidad1
                items_stock.nro_partida_general = partidas_remitos.nro_partida_unica.
        END.
        ELSE DO:
            x_partunica = NEXT-VALUE(nro_partida_general, produccion).
        
            CREATE partidas_remitos.
            
            ASSIGN
                partidas_remitos.id_sucursal            = items_stock.id_sucursal
                partidas_remitos.id_tipo_movsto         = items_stock.id_tipo_movsto
                partidas_remitos.id_suc_envio           = items_stock.id_suc_envio
                partidas_remitos.nro_partida_unica      = x_partunica
                partidas_remitos.nro_partida            = items_stock.nro_partida
                partidas_remitos.nro_partida_origen     = items_stock.nro_partida_origen
                partidas_remitos.nro_comprobante        = items_stock.nro_comprobante
                partidas_remitos.codigo_trazabilidad    = items_stock.codigo_trazabilidad
                partidas_remitos.union_europea          = items_stock.union_europea
                partidas_remitos.fecha                  = items_stock.fecha
                partidas_remitos.hora                   = items_stock.hora
                partidas_remitos.fecha_operativa        = items_stock.fecha_operativa
                partidas_remitos.cantidad               = items_stock.cantidad1
                partidas_remitos.saldo                  = items_stock.cantidad1.
                
                ASSIGN items_stock.nro_partida_general = x_partunica.
        END.
        
        RELEASE partidas_remitos.
    END.

END PROCEDURE.


/******************************************************/
/******************************************************/
/******************************************************/
/*   Graba contrapartida en items_stock por volcado   */
/******************************************************/
/******************************************************/
/******************************************************/

PROCEDURE graba_egreso.

    ASSIGN
        aux_items.id_sucursal           = proceso_volcado.id_suc_envio
        aux_items.id_tipo_movsto        = 4
        aux_items.id_suc_envio          = x_destino
        aux_items.nro                   = x_num_envio
        aux_items.item                  = items_stock.item
        aux_items.id_serial             = NEXT-VALUE(stock, general)
        aux_items.fecha                 = items_stock.fecha
        aux_items.hora                  = items_stock.hora
        aux_items.fecha_operativa       = items_stock.fecha_operativa
        aux_items.dia                   = WEEKDAY( items_stock.fecha)
        aux_items.id_sucursal_etiqueta  = items_stock.id_sucursal_etiqueta
        aux_items.id_etiqueta           = items_stock.id_etiqueta
        aux_items.nro_partida           = items_stock.nro_partida
        aux_items.nro_partida_serial    = items_stock.nro_partida_serial
        aux_items.nro_partida_origen    = items_stock.nro_partida_origen
        aux_items.codigo_stock          = "E"
        aux_items.id_color              = items_stock.id_color
        aux_items.nro_comprobante       = items_stock.nro_comprobante
        aux_items.id_articulo           = items_stock.id_articulo
        aux_items.id_variedad           = items_stock.id_variedad
        aux_items.fecha_cosecha         = items_stock.fecha_cosecha
        aux_items.id_tipo_cosecha       = items_stock.id_tipo_cosecha
        aux_items.estado_fruta          = items_stock.estado_fruta
        aux_items.periodo_cosecha       = items_stock.periodo_cosecha
        aux_items.codigo_trazabilidad   = items_stock.codigo_trazabilidad
        aux_items.id_proveedor          = items_stock.id_proveedor
        aux_items.id_origen             = items_stock.id_origen
        aux_items.id_lote               = items_stock.id_lote
        aux_items.union_europea         = items_stock.union_europea
        aux_items.china                 = items_stock.china
        aux_items.certificado           = items_stock.certificado
        aux_items.cert_china            = items_stock.cert_china
        aux_items.id_finca_senasa       = items_stock.id_finca_senasa
        aux_items.id_lote_senasa        = items_stock.id_lote_senasa
        aux_items.camara                = items_stock.camara
        aux_items.id_envase             = items_stock.id_envase
        aux_items.cantidad              = items_stock.cantidad
        aux_items.cantidad1             = items_stock.cantidad1
        aux_items.peso                  = items_stock.peso
        aux_items.consumido             = items_stock.consumido
        aux_items.c_usuario             = items_stock.c_usuario
        aux_items.c_fecha               = items_stock.c_fecha
        aux_items.c_hora                = items_stock.c_hora.

END PROCEDURE.
