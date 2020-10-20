/****************************************************************************/
/*  NOMBRE PROGRAMA......:   dArmadoPallets.p                               */
/****************************************************************************/
/*  Genera los pallets en base a la lectura de cajas con la PDA             */
/****************************************************************************/

SESSION:TIME-SOURCE = "produccion". 

DEFINE TEMP-TABLE temp_pedidos
    FIELD id_empresa        LIKE items_pedidos_packing.id_empresa
    FIELD id_punto_emisor   LIKE items_pedidos_packing.id_punto_emisor
    FIELD id_orden          LIKE items_pedidos_packing.id_orden
    FIELD ITEM              LIKE items_pedidos_packing.ITEM
    FIELD id_cajonera       LIKE cajas.id_cajonera
    FIELD letra_color       LIKE cajas.letra_color
    FIELD calibre           LIKE items_pedidos_packing.calibre
    FIELD bultos            AS INTEGER
    FIELD cantidad          AS INTEGER
    FIELD elegido           AS LOGICAL INITIAL FALSE.

DEFINE INPUT PARAMETER  xSucTrabajo AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER  xPacking    AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER  xLector     AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER xPallet     AS INTEGER NO-UNDO. 
DEFINE OUTPUT PARAMETER xMensaje    AS CHARACTER NO-UNDO. 

/*
DEFINE VAR xSucTrabajo  AS INTEGER NO-UNDO.
DEFINE VAR xPacking     AS INTEGER NO-UNDO.
DEFINE VAR xLector      AS INTEGER NO-UNDO.
DEFINE VAR xPallet      AS INTEGER NO-UNDO.
DEFINE VAR xMensaje     AS CHARACTER NO-UNDO. 

xSucTrabajo = 98.
xPacking    = 1.
xLector     = 5.
*/

DEFINE VARIABLE xNuePal     AS CHARACTER NO-UNDO.
DEFINE VARIABLE xDatos      AS CHARACTER NO-UNDO.
DEFINE VARIABLE xNroMov     AS INTEGER NO-UNDO.
DEFINE VARIABLE xCantItem   AS INTEGER NO-UNDO.
DEFINE VARIABLE xCantDif    AS INTEGER NO-UNDO.
DEFINE VARIABLE xDifItem    AS INTEGER NO-UNDO.
DEFINE VARIABLE xCantCaj    AS INTEGER NO-UNDO.
DEFINE VARIABLE xKilos      AS INTEGER NO-UNDO.
DEFINE VARIABLE xPalItem    AS INTEGER NO-UNDO.
DEFINE VARIABLE xHora       AS CHARACTER NO-UNDO.
DEFINE VARIABLE xFecOper    AS DATE NO-UNDO.
DEFINE VARIABLE xErroneo    AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE xTipoEsq    AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE xTipoPal    AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE xContram    AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE xChep       AS INTEGER NO-UNDO.
DEFINE VARIABLE xArticulo   AS INTEGER NO-UNDO.
DEFINE VARIABLE xVariedad   AS INTEGER NO-UNDO.
DEFINE VARIABLE xEnvase     AS INTEGER NO-UNDO.
DEFINE VARIABLE xMarca      AS INTEGER NO-UNDO.
DEFINE VARIABLE xCalidad    AS INTEGER NO-UNDO.
DEFINE VARIABLE xCaract     AS INTEGER NO-UNDO.
DEFINE VARIABLE xCateg      AS INTEGER NO-UNDO.
DEFINE VARIABLE xUnion      AS LOGICAL NO-UNDO.
DEFINE VARIABLE xChina      AS LOGICAL NO-UNDO.
DEFINE VARIABLE xUEChina    AS LOGICAL NO-UNDO.
DEFINE VARIABLE xCalibre    AS CHARACTER NO-UNDO.
DEFINE VARIABLE xTotCaj     AS INTEGER NO-UNDO.
DEFINE VARIABLE xBultos     AS INTEGER NO-UNDO.
DEFINE VARIABLE xIdColor    AS INTEGER NO-UNDO.
DEFINE VARIABLE xPorcent    AS DECIMAL NO-UNDO.
DEFINE VARIABLE xTrazab     AS CHARACTER NO-UNDO.
DEFINE VARIABLE xNewOrden   AS INTEGER NO-UNDO.
DEFINE VARIABLE xNewItem    AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE xUsuario    AS CHARACTER NO-UNDO.
DEFINE VARIABLE xTotPal     AS INTEGER NO-UNDO.
DEFINE VARIABLE xMercado    AS INTEGER NO-UNDO.
DEFINE VARIABLE xItemPal    AS INTEGER NO-UNDO.
DEFINE VARIABLE xImpGLN     AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE xTestigo    AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE xTrazTest   AS CHARACTER NO-UNDO.


DEFINE VARIABLE dCaja       LIKE cajas.etiqueta     NO-UNDO.
DEFINE VARIABLE dTestigo    LIKE cajas.id_testigo   NO-UNDO.


DEFINE TEMP-TABLE temp_itemspal LIKE items_pallets
    FIELD union_europea     LIKE cajas.union_europea
    FIELD china             LIKE cajas.china
    FIELD id_cajonera       LIKE cajas.id_cajonera
    FIELD letra_color       LIKE cajas.letra_color
    FIELD bultos_tot        AS INTEGER
    FIELD cajas_ciegas      AS LOGICAL INITIAL FALSE
    FIELD id_testigo        LIKE cajas.id_testigo
    FIELD id_caja           LIKE cajas.id_caja.

DEFINE TEMP-TABLE temp_items    LIKE items_pallets.

DEFINE TEMP-TABLE temp_cajas
    FIELD id_suc_trabajo    LIKE cajas.id_suc_trabajo
    FIELD id_caja           LIKE cajas.id_caja.


DEFINE BUFFER aux_pallets   FOR pallets.
DEFINE BUFFER aux_itemspal  FOR temp_itemspal.
DEFINE BUFFER tpallets      FOR pallets.

DEFINE VARIABLE hLib        AS HANDLE NO-UNDO.

{persistentprocedure.i libinterfaces.p hLib}.

IF NOT VALID-HANDLE(hlib) THEN DO:
    MESSAGE 'Error en librer¡a de intefases SAP. Avise a sistemas !!!' VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.



/********************************/
/* Controla Fecha de Produccion */
/********************************/

xErroneo    = FALSE.

xHora   = substr(string(time,"hh:mm:ss"),1,2) +
          substr(string(time,"hh:mm:ss"),4,2) +
          substr(string(time,"hh:mm:ss"),7,2).
if xHora < "060000" then
    xFecOper = today - 1.
else
    xFecOper = today.

FIND fechas_produccion WHERE
    fechas_produccion.fecha = xFecOper NO-LOCK NO-ERROR.

IF NOT AVAILABLE fechas_produccion THEN DO:
    xErroneo    = TRUE.
    xMensaje    = "ERROR - FALTA FECHA PRODUCCION".
END.

IF xErroneo THEN RETURN.


/*********************************/
/* Controla Lectura Caja Testigo */
/*********************************/

xErroneo    = FALSE.
dCaja       = 0.
dTestigo    = 0.

RUN wSelTestigos.w
    (INPUT xSucTrabajo,
     INPUT xPacking,
     INPUT xLector,
     INPUT-OUTPUT dCaja,
     INPUT-OUTPUT dTestigo,
     INPUT-OUTPUT xErroneo).

IF xErroneo THEN DO:
    RUN Elimina_Testigo.
    RETURN.
END.
ELSE DO:
    xErroneo    = FALSE.
    
    /******* Valida Existencia de Caja *******/
    IF dCaja <> 0 THEN DO:
        FIND cajas WHERE cajas.etiqueta = dCaja NO-LOCK NO-ERROR.

        IF NOT AVAILABLE cajas THEN DO:
            xMensaje    = "CAJA INEXISTENTE".
            xErroneo    = TRUE.
        END.
    END.
    
    IF dCaja <> 0 AND dTestigo <> 0 AND xErroneo = FALSE THEN DO:
        FIND cajas WHERE cajas.etiqueta = dCaja NO-LOCK NO-ERROR.

        /******* Valida Caja Testigo *******/
        IF AVAILABLE cajas AND
            cajas.id_pallet <> 0 OR
            cajas.id_lector_pallet <> 0 THEN DO:

            IF cajas.id_testigo <> 0 THEN
                xMensaje    = "CAJA CON ETIQUETA TESTIGO".
            ELSE
                xMensaje    = "CAJA LEIDA" + CHR(13) + "INEXISTENTE O ASIGNADA".
            xErroneo    = TRUE.
        END.
    
        /******* Valida Etiqueta Testigo *******/
        IF xErroneo = FALSE THEN DO:
            FIND cajas WHERE cajas.id_testigo = dTestigo NO-LOCK NO-ERROR.

            IF AVAILABLE cajas THEN DO:
                xMensaje    = "ETIQUETA TESTIGO EN OTRA CAJA".
                xErroneo    = TRUE.
            END.
        END.
    END.
    
    IF dCaja <> 0 AND dTestigo = 0 AND xErroneo = FALSE THEN DO:
        xMensaje    = "CAJA LEIDA - FALTA LEER TESTIGO".
        xErroneo    = TRUE.
    END.
    
    IF dCaja = 0 AND dTestigo <> 0 AND xErroneo = FALSE THEN DO:
        xMensaje    = "TESTIGO LEIDO - FALTA LEER CAJA".
        xErroneo    = TRUE.
    END.
END.

IF xErroneo THEN DO:
    RUN Elimina_Testigo.
    RETURN.
END.
ELSE DO:
    IF dCaja <> 0 AND dTestigo <> 0 THEN DO:
        FIND cajas WHERE cajas.etiqueta = dCaja NO-ERROR.
        ASSIGN 
            cajas.id_suc_trabajo_lector = xSucTrabajo
            cajas.id_packing_lector     = xPacking
            cajas.id_lector_pallet      = xLector
            cajas.hora_lectura_pallet   = NOW
            cajas.id_testigo            = dTestigo
            cajas.testigo               = TRUE.
    
        FIND cajas WHERE cajas.etiqueta = dCaja NO-LOCK NO-ERROR.
        RELEASE cajas.
    END.
END.



/*********************************************/
/* Lee Cajas y Carga Pallets e Items Pallets */
/*********************************************/

xCantCaj    = 0.
xNroMov     = TIME.
xPalItem    = 0.
xTestigo    = FALSE.


FOR EACH cajas WHERE
    cajas.id_suc_trabajo_lector = xSucTrabajo   AND
    cajas.id_packing_lector     = xPacking      AND
    cajas.id_lector_pallet      = xLector       AND
    cajas.id_suc_trabajo        = xSucTrabajo   AND
    cajas.id_pallet             = 0 NO-LOCK:

    IF cajas.anulada THEN NEXT.
    IF cajas.id_testigo <> 0 THEN DO:
        xTestigo    = TRUE.
        xTrazab     = cajas.codigo_trazabilidad.
    END.

    /****************************************/
    /* Controla la cantidad de cajas leidas */
    /****************************************/
    xCantCaj    = xCantCaj + 1.
    
    FIND FIRST colores WHERE
        colores.letra_color = cajas.letra_color NO-LOCK NO-ERROR.

    FIND FIRST lote WHERE
        lote.codigo_trazabilidad = cajas.codigo_trazabilidad NO-LOCK NO-ERROR.
    IF AVAILABLE lote THEN DO:
        FIND FIRST proveedores OF lote NO-LOCK NO-ERROR.
        FIND FIRST origenes OF lote NO-LOCK NO-ERROR.
    END.
        
    FIND FIRST caracteristicas OF cajas NO-LOCK NO-ERROR.
    IF AVAILABLE caracteristicas AND caracteristicas.estado THEN
        xDatos  = SUBSTR(caracteristicas.abreviatura,1,5).
    ELSE
        xDatos  = "".
        
    xPalItem    = xPalItem + 1.
        
    CREATE temp_itemspal.
    ASSIGN
        temp_itemspal.id_suc_trabajo        = xSucTrabajo
        temp_itemspal.id_pallet             = xNroMov
        temp_itemspal.item_pallet           = xPalItem
        temp_itemspal.id_turno_packing      = cajas.id_turno_packing 
        temp_itemspal.id_packing            = cajas.id_packing 
        temp_itemspal.fecha_operativa       = xFecOper
        temp_itemspal.codigo_trazabilidad   = cajas.codigo_trazabilidad
        temp_itemspal.id_proveedor          = proveedores.id_proveedor
        temp_itemspal.id_origen             = origenes.id_origen
        temp_itemspal.id_lote               = lote.id_lote
        temp_itemspal.renspa                = origenes.renspa
        temp_itemspal.zona_up               = origenes.zona_up
        temp_itemspal.cod_trazabilidad      = STRING(cajas.codigo_trazabilidad,"xxxxxxx") + xDatos
        temp_itemspal.bultos                = 1
        temp_itemspal.cod_prod              = fechas_produccion.fecha_senasa
        temp_itemspal.id_color              = colores.id_color
        temp_itemspal.tipo_proceso          = cajas.id_tipo_proceso
        temp_itemspal.id_empresa            = cajas.id_empresa
        temp_itemspal.id_punto_emisor       = cajas.id_punto_emisor
        temp_itemspal.id_orden              = cajas.id_orden
        temp_itemspal.ITEM                  = cajas.ITEM
        temp_itemspal.id_marca              = cajas.id_marca
        temp_itemspal.id_articulo           = cajas.id_articulo
        temp_itemspal.id_variedad           = cajas.id_variedad
        temp_itemspal.id_envase             = cajas.id_envase
        temp_itemspal.id_calidad            = cajas.id_calidad
        temp_itemspal.id_caract             = cajas.id_caract
        temp_itemspal.id_categoria          = cajas.id_categoria
        temp_itemspal.calibre               = cajas.calibre
        temp_itemspal.contramarca           = cajas.contramarca
        temp_itemspal.nro_partida_general   = cajas.kilos
        temp_itemspal.union_europea         = cajas.union_europea
        temp_itemspal.china                 = cajas.china
        temp_itemspal.id_cajonera           = cajas.id_cajonera
        temp_itemspal.letra_color           = cajas.letra_color
        temp_itemspal.bultos_tot            = cajas.bultos
        temp_itemspal.id_testigo            = cajas.id_testigo
        temp_itemspal.id_caja               = cajas.id_caja.

    
    IF cajas.union_europea = FALSE AND
        cajas.china = FALSE THEN
        ASSIGN
            temp_itemspal.id_finca_senasa   = 0
            temp_itemspal.id_lote_senasa    = 0
            temp_itemspal.certificado       = ""
            temp_itemspal.cert_china        = ""
            temp_itemspal.union_europea     = FALSE
            temp_itemspal.china             = FALSE.
        

    IF cajas.union_europea THEN DO:
        IF lote.habilitada AND lote.certificado <> "" THEN
            ASSIGN
                temp_itemspal.id_finca_senasa   = origenes.id_finca_senasa
                temp_itemspal.id_lote_senasa    = lote.id_lote_senasa
                temp_itemspal.certificado       = lote.certificado
                temp_itemspal.cert_china        = lote.cert_china
                xArticulo                       = lote.id_articulo.
        ELSE
            ASSIGN
                temp_itemspal.id_finca_senasa   = 0
                temp_itemspal.id_lote_senasa    = 0
                temp_itemspal.certificado       = ""
                temp_itemspal.cert_china        = ""
                temp_itemspal.union_europea     = FALSE
                temp_itemspal.china             = FALSE.
    END.


    IF cajas.china THEN DO:
        IF lote.habilitada AND lote.cert_china <> "" THEN
            ASSIGN
                temp_itemspal.id_finca_senasa   = origenes.id_finca_senasa
                temp_itemspal.id_lote_senasa    = lote.id_lote_senasa
                temp_itemspal.certificado       = lote.cert_china
                temp_itemspal.cert_china        = lote.certificado
                xArticulo                       = lote.id_articulo.
        ELSE
            ASSIGN
                temp_itemspal.id_finca_senasa   = 0
                temp_itemspal.id_lote_senasa    = 0
                temp_itemspal.certificado       = ""
                temp_itemspal.cert_china        = ""
                temp_itemspal.union_europea     = FALSE
                temp_itemspal.china             = FALSE.
    END.
END.

IF xCantCaj = 0 THEN DO:
    xErroneo    = TRUE.
    xMensaje    = "NO EXISTEN CAJAS LEIDAS".
END.

IF xErroneo THEN DO:
    RUN Elimina_Testigo.
    RETURN.
END.



/*****************************************/
/* Controla Trazabilidad de Caja Testigo */
/*****************************************/

xErroneo    = FALSE.
xCantCaj    = 0.

IF xTestigo THEN DO:
    FIND FIRST temp_itemspal WHERE
        temp_itemspal.id_testigo <> 0 NO-LOCK NO-ERROR.

    IF AVAILABLE temp_itemspal THEN DO:
        xTrazab     = temp_itemspal.codigo_trazabilidad.
        xTrazTest   = temp_itemspal.codigo_trazabilidad.
    END.

    FOR EACH temp_itemspal NO-LOCK WHERE
        temp_itemspal.codigo_trazabilidad = xTrazab AND
        temp_itemspal.id_testigo    = 0:

        xCantCaj    = xCantCaj + 1.
    END.
END.

IF xTestigo AND xCantCaj = 0 THEN DO:
    xErroneo    = TRUE.
    xMensaje    = "NO EXISTEN CAJAS LEIDAS" + CHR(13) + "CON TRAZABILIDAD DEL TESTIGO".
END.

IF xErroneo THEN DO:
    RUN Elimina_Testigo.
    RETURN.
END.



/**********************************************************/
/* Controla Errores por Diferencias de Especie y Variedad */
/**********************************************************/

xTrazab     = "".
xErroneo    = FALSE.
xCantCaj    = 0.

FOR EACH temp_itemspal
    BREAK
    BY temp_itemspal.id_articulo
    BY temp_itemspal.id_variedad:

    xCantCaj    = xCantCaj + temp_itemspal.bultos.
    
    IF LAST-OF (temp_itemspal.id_variedad) AND
        xCantCaj <> xPalItem THEN DO:
            xPallet     = 0.
            xErroneo    = TRUE.
            xMensaje    = "DIFERENTES ESPECIES " + STRING(xCantCaj,"zzz9").
        LEAVE.
    END.
END.

IF xErroneo THEN DO:
    RUN Elimina_Testigo.
    RETURN.
END.



/**********************************************/
/* Controla Errores por Diferencias de Envase */
/**********************************************/

xErroneo    = FALSE.
xCantCaj    = 0.

FOR EACH temp_itemspal
    BREAK
    BY temp_itemspal.id_envase:

    xCantCaj    = xCantCaj + temp_itemspal.bultos.
    
    IF LAST-OF (temp_itemspal.id_envase) AND
        xCantCaj <> xPalItem THEN DO:
            xPallet     = 0.
            xErroneo    = TRUE.
            xMensaje    = "DIFERENTES ENVASES " + STRING(xCantCaj,"zzz9").
        LEAVE.
    END.
END.

IF xErroneo THEN DO:
    RUN Elimina_Testigo.
    RETURN.
END.



/**********************************************/
/* Controla Errores por Diferencias de Marcas */
/**********************************************/

xErroneo    = FALSE.
xCantCaj    = 0.

FOR EACH temp_itemspal
    BREAK
    BY temp_itemspal.id_marca:

    xCantCaj    = xCantCaj + temp_itemspal.bultos.
    
    IF LAST-OF (temp_itemspal.id_marca) AND
        xCantCaj <> xPalItem THEN DO:
            xPallet     = 0.
            xErroneo    = TRUE.
            xMensaje    = "DIFERENTES MARCAS " + STRING(xCantCaj,"zzz9").
        LEAVE.
    END.
END.

IF xErroneo THEN DO:
    RUN Elimina_Testigo.
    RETURN.
END.



/*******************************************************/
/* Controla Errores por Diferencias de Caracteristicas */
/*******************************************************/

xErroneo    = FALSE.
xCantCaj    = 0.

FOR EACH temp_itemspal
    BREAK
    BY temp_itemspal.id_caract:

    xCantCaj    = xCantCaj + temp_itemspal.bultos.
    
    IF LAST-OF (temp_itemspal.id_caract) AND
        xCantCaj <> xPalItem THEN DO:
            xPallet     = 0.
            xErroneo    = TRUE.
            xMensaje    = "DIFERENTES CARACTERISTICAS " + STRING(xCantCaj,"zzz9").
        LEAVE.
    END.
END.

IF xErroneo THEN DO:
    RUN Elimina_Testigo.
    RETURN.
END.



/**************************************************/
/* Controla Errores por Diferencias de Categor¡as */
/**************************************************/

xErroneo    = FALSE.
xCantCaj    = 0.

FOR EACH temp_itemspal
    BREAK
    BY temp_itemspal.id_categoria:

    xCantCaj    = xCantCaj + temp_itemspal.bultos.
    
    IF LAST-OF (temp_itemspal.id_categoria) AND
        xCantCaj <> xPalItem THEN DO:
            xPallet     = 0.
            xErroneo    = TRUE.
            xMensaje    = "DIFERENTES CATEGORIAS " + STRING(xCantCaj,"zzz9").
        LEAVE.
    END.
END.

IF xErroneo THEN DO:
    RUN Elimina_Testigo.
    RETURN.
END.



/*************************************************/
/* Controla Errores por Diferencias de Calidades */
/*************************************************/

xErroneo    = FALSE.
xCantCaj    = 0.

FOR EACH temp_itemspal
    BREAK
    BY temp_itemspal.id_calidad:

    xCantCaj    = xCantCaj + temp_itemspal.bultos.
    
    IF LAST-OF (temp_itemspal.id_calidad) AND
        xCantCaj <> xPalItem THEN DO:
            xPallet     = 0.
            xErroneo    = TRUE.
            xMensaje    = "DIFERENTES CALIDADES " + STRING(xCantCaj,"zzz9").
        LEAVE.
    END.
END.

IF xErroneo THEN DO:
    xUsuario    = "".
    RUN wSelConfecciones.w (INPUT-OUTPUT xErroneo, INPUT-OUTPUT xUsuario).
END.

IF xErroneo THEN DO:
    RUN Elimina_Testigo.
    RETURN.
END.



/******************************************************/
/* Controla Cajas de Diferentes Pedidos de Produccion */
/******************************************************/
xErroneo    = FALSE.
xCantCaj    = 0.

FOR EACH temp_itemspal
    BREAK
    BY temp_itemspal.id_empresa
    BY temp_itemspal.id_punto_emisor
    BY temp_itemspal.id_orden
    BY temp_itemspal.ITEM:
    
    IF temp_itemspal.id_testigo = 0 THEN DO:
        xCantCaj    = xCantCaj + temp_itemspal.bultos.
        xTotCaj     = xTotCaj + temp_itemspal.bultos.
    END.

    IF LAST-OF (temp_itemspal.ITEM) THEN DO:
        CREATE temp_pedidos.
        ASSIGN
            temp_pedidos.id_empresa         = temp_itemspal.id_empresa
            temp_pedidos.id_punto_emisor    = temp_itemspal.id_punto_emisor
            temp_pedidos.id_orden           = temp_itemspal.id_orden
            temp_pedidos.ITEM               = temp_itemspal.ITEM
            temp_pedidos.bultos             = temp_itemspal.bultos_tot
            temp_pedidos.cantidad           = xCantCaj
            temp_pedidos.letra_color        = temp_itemspal.letra_color
            temp_pedidos.id_cajonera        = temp_itemspal.id_cajonera
            temp_pedidos.calibre            = temp_itemspal.calibre.

        xCantCaj    = 0.
    END.
END.
               



/**********************************/
/* Selecciona el Pedido a Aplicar */
/**********************************/

RUN wSelItemPedido.w
    (INPUT-OUTPUT TABLE temp_pedidos).

FIND FIRST temp_pedidos WHERE temp_pedidos.elegido NO-ERROR.

IF NOT AVAILABLE temp_pedidos THEN DO:
    xErroneo    = TRUE.
    xMensaje    = "NO SELECCIONO PEDIDO".
    RETURN.
END.
ELSE DO:
    
/********************************/
/* Controla Cantidad de Pallets */
/********************************/
    FIND FIRST pedidos_packing WHERE
        pedidos_packing.id_empresa      = temp_pedidos.id_empresa AND
        pedidos_packing.id_punto_emisor = temp_pedidos.id_punto_emisor AND
        pedidos_packing.id_orden        = temp_pedidos.id_orden
        NO-LOCK NO-ERROR.
    
    xTotPal = 0.
    FOR EACH pallets OF pedidos_packing NO-LOCK WHERE
        pallets.estado AND
        pallets.merma = FALSE:

        xTotPal = xTotPal + 1.
    end.
    RELEASE pallets.

    IF xTotPal >= pedidos_packing.total_pallets THEN DO:
        xErroneo    = TRUE.
        xMensaje    = "PEDIDO " + STRING(pedidos_packing.id_orden,"zzzzzzz9") + " COMPLETO".
        RETURN.
    END.

/************************************/
/* Selecciona el Color de Esquinero */
/************************************/
    RUN wSelEsquinero.w
        (INPUT-OUTPUT xTipoEsq).
    FIND FIRST tipo_esquineros WHERE
        tipo_esquineros.id_tipo_esquinero = xTipoEsq NO-LOCK NO-ERROR.
    
    IF NOT AVAILABLE tipo_esquineros THEN DO:
        xErroneo    = TRUE.
        xMensaje    = "ESQUINERO INEXISTENTE".
        RETURN.
    END.
    ELSE DO:
        FIND FIRST r_clientes_esquineros WHERE
            r_clientes_esquineros.id_cliente = pedidos_packing.id_cliente AND
            r_clientes_esquineros.id_tipo_esquinero = xTipoEsq AND
            r_clientes_esquineros.estado NO-LOCK NO-ERROR.
        IF AVAILABLE r_clientes_esquineros THEN DO:
            xErroneo    = TRUE.
            xMensaje    = "ESQUINERO INHABILITADO P/PEDIDO " + STRING(pedidos_packing.id_orden,"zzzzzzz9").
            RETURN.
        END.
        
        FOR EACH temp_itemspal:
            IF (temp_itemspal.id_finca_senasa = 0 OR 
                temp_itemspal.union_europea = FALSE) AND
                pedidos_packing.union_europea THEN DO:
                xErroneo    = TRUE.
                xMensaje    = "PEDIDO PARA UNION EUROPEA".
                RETURN.
            END.

            IF (temp_itemspal.id_finca_senasa = 0 OR 
                temp_itemspal.china = FALSE) AND
                pedidos_packing.china THEN DO:
                xErroneo    = TRUE.
                xMensaje    = "PEDIDO PARA CHINA".
                RETURN.
            END.

            IF temp_itemspal.id_finca_senasa <> 0 AND 
                temp_itemspal.id_articulo <> xArticulo THEN DO:
                xErroneo    = TRUE.
                xMensaje    = "DIFERENTES ESPECIES POR UP".
                RETURN.
            END.

            ASSIGN
                temp_itemspal.id_empresa        = temp_pedidos.id_empresa
                temp_itemspal.id_punto_emisor   = temp_pedidos.id_punto_emisor
                temp_itemspal.id_orden          = temp_pedidos.id_orden
                temp_itemspal.ITEM              = temp_pedidos.ITEM.
            
            IF temp_pedidos.id_cajonera = 431 OR
                temp_pedidos.id_cajonera = 432 OR
                temp_pedidos.id_cajonera = 433 THEN
                ASSIGN temp_itemspal.tipo_proceso = "A".
        END.
    
        FIND FIRST items_pedidos_packing WHERE
            items_pedidos_packing.id_empresa        = temp_pedidos.id_empresa AND
            items_pedidos_packing.id_punto_emisor   = temp_pedidos.id_punto_emisor AND
            items_pedidos_packing.id_orden          = temp_pedidos.id_orden AND
            items_pedidos_packing.ITEM              = temp_pedidos.ITEM
            NO-LOCK NO-ERROR.
        
        FIND FIRST tipo_pallets OF items_pedidos_packing NO-LOCK NO-ERROR.
        FIND FIRST colores WHERE
            colores.letra_color = temp_pedidos.letra_color NO-LOCK NO-ERROR.
        
        IF AVAILABLE colores THEN
            xIdColor    = colores.id_color.
        ELSE
            xIdColor    = 3.

        xUnion      = pedidos_packing.union_europea.
        xChina      = pedidos_packing.china.
        xMercado    = pedidos_packing.id_mercado.
        xTipoPal    = items_pedidos_packing.id_tipo_pallet.
        xArticulo   = items_pedidos_packing.id_articulo.
        xVariedad   = items_pedidos_packing.id_variedad.
        xEnvase     = items_pedidos_packing.id_envase.
        xMarca      = items_pedidos_packing.id_marca.
        xCalidad    = items_pedidos_packing.id_calidad.
        xCaract     = items_pedidos_packing.id_caract.
        xCateg      = items_pedidos_packing.id_categoria.
        xChep       = IF tipo_pallets.chep THEN 1 ELSE 0.
        xContram    = items_pedidos_packing.contramarca.
        xCalibre    = items_pedidos_packing.calibre.
        xBultos     = items_pedidos_packing.bultos.
    END.
END.



/***********************************/
/* Genera Registro de Cajas Ciegas */
/***********************************/

xErroneo    = FALSE.

IF xTotCaj < xBultos THEN DO:
    xCantItem = 0.
    xCantCaj  = 0.
    xDifItem  = xBultos - xTotCaj.

/***************************************************/
/* Controla Cantidad de Cajas que deben ser leidas */
/***************************************************/
    
    FIND FIRST r_pallets_envases WHERE
        r_pallets_envases.id_tipo_pallet    = xTipoPal  AND
        r_pallets_envases.id_envase         = xEnvase   AND
        r_pallets_envases.pallets           = xBultos   NO-LOCK NO-ERROR.
    IF AVAILABLE r_pallets_envases AND 
        r_pallets_envases.cajas_leidas <> 0 THEN DO:
        IF xTotCaj < r_pallets_envases.cajas_leidas THEN DO:
            xErroneo    = TRUE.
            xMensaje    = "ERROR CAJAS - LEIDAS = " + STRING(xTotCaj,"zz9") +
                          " PEDIDAS = " + STRING(r_pallets_envases.cajas_leidas,"zz9").
            RETURN.
        END.
    END.
    ELSE DO:
        xErroneo    = TRUE.
        xMensaje    = "FALTA RELACION ENVASE/PALLET".
        RETURN.
    END.

    FOR EACH temp_itemspal NO-LOCK WHERE
        temp_itemspal.id_empresa            = temp_pedidos.id_empresa AND
        temp_itemspal.id_punto_emisor       = temp_pedidos.id_punto_emisor AND
        temp_itemspal.id_orden              = temp_pedidos.id_orden AND
        temp_itemspal.ITEM                  = temp_pedidos.ITEM AND
        temp_itemspal.id_testigo            = 0
        BREAK
        BY temp_itemspal.codigo_trazabilidad:

        IF FIRST-OF (temp_itemspal.codigo_trazabilidad) THEN DO:
            xCantItem = xCantItem + 1.
        END.
    END.

    xCantCaj = xDifItem / xCantItem.

    xCantDif  = xCantItem.
    xCantItem = 0.
    xNewItem  = FALSE.

    FOR EACH temp_itemspal NO-LOCK WHERE
        temp_itemspal.id_empresa            = temp_pedidos.id_empresa AND
        temp_itemspal.id_punto_emisor       = temp_pedidos.id_punto_emisor AND
        temp_itemspal.id_orden              = temp_pedidos.id_orden AND
        temp_itemspal.ITEM                  = temp_pedidos.ITEM AND
        temp_itemspal.id_testigo            = 0
        BREAK
        BY temp_itemspal.codigo_trazabilidad:

        IF FIRST-OF (temp_itemspal.codigo_trazabilidad) THEN DO:
            xCantDif  = xCantDif - 1.
            IF xCantDif = 0 THEN
                xCantCaj  = xDifItem - xCantItem.
            ELSE
                xCantItem = xCantItem + xCantCaj.

            IF xCantItem > xDifItem THEN DO:
                xCantItem = xCantItem - xCantCaj.
                xCantCaj  = xDifItem - xCantItem.
                xNewItem  = TRUE.
            END.

            IF xCantCaj > 0 THEN DO:
                xPalItem  = xPalItem + 1.

                CREATE aux_itemspal.
                BUFFER-COPY temp_itemspal
                    EXCEPT
                    temp_itemspal.ITEM_pallet
                    temp_itemspal.nro_partida_general
                    TO aux_itemspal.

                ASSIGN
                    aux_itemspal.ITEM_pallet            = xPalItem
                    aux_itemspal.bultos                 = xCantCaj
                    aux_itemspal.nro_partida_general    = xCantCaj * temp_itemspal.nro_partida_general
                    aux_itemspal.cajas_ciegas           = TRUE.
            END.
        END.
        IF xNewItem THEN LEAVE.
    END.
END.

IF xTotCaj > xBultos THEN DO:
    xErroneo    = TRUE.
    xMensaje    = "ERROR CAJAS - LEIDAS = " + STRING(xTotCaj,"zz9") + "PEDIDAS = " + STRING(xBultos,"zz9").
    RETURN.
END.
xNewItem = FALSE.



/*******************************************************/
/* Controla Porcentaje de Rendimiento por Trazabilidad */
/*******************************************************/

IF xUnion OR xChina THEN
    xUEChina    = TRUE.
ELSE
    xUEchina    = FALSE.
            

xErroneo    = FALSE.
xPorcent    = 0.
xTrazab     = "".

FOR EACH temp_itemspal
    BREAK BY temp_itemspal.codigo_trazabilidad:

    IF FIRST-OF (temp_itemspal.codigo_trazabilidad) THEN DO:
        /* Controla Porcentaje Global */
        FIND FIRST saldos_fincas WHERE
            saldos_fincas.id_sucursal           = temp_itemspal.id_suc_trabajo AND
            saldos_fincas.id_proveedor          = temp_itemspal.id_proveedor AND
            saldos_fincas.id_origen             = temp_itemspal.id_origen AND
            saldos_fincas.union_europea         = FALSE AND
            saldos_fincas.codigo_trazabilidad   = temp_itemspal.codigo_trazabilidad
            NO-LOCK NO-ERROR.

        IF AVAILABLE saldos_fincas THEN DO:
            IF saldos_fincas.porcentaje > 88 OR
                saldos_fincas.porcentaje = ? THEN DO:

                xPorcent    = saldos_fincas.porcentaje.
                xTrazab     = temp_itemspal.codigo_trazabilidad.
                xErroneo    = TRUE.
                xMensaje    = "ERROR RENDIM.TRAZAB. " + TRIM(xTrazab).
            END.
        END.
        ELSE DO:
            xPorcent    = 0.
            xTrazab     = temp_itemspal.codigo_trazabilidad.
            xErroneo    = TRUE.
            xMensaje    = "ERROR TRAZAB. " + TRIM(xTrazab) + " S/SALDO".
        END.
        
        /* Controla Porcentaje para UE */
        IF xUEChina THEN DO:
            FIND FIRST saldos_fincas WHERE
                saldos_fincas.id_sucursal           = temp_itemspal.id_suc_trabajo AND
                saldos_fincas.id_proveedor          = temp_itemspal.id_proveedor AND
                saldos_fincas.id_origen             = temp_itemspal.id_origen AND
                saldos_fincas.union_europea         = TRUE AND
                saldos_fincas.codigo_trazabilidad   = temp_itemspal.codigo_trazabilidad
                NO-LOCK NO-ERROR.

            IF AVAILABLE saldos_fincas THEN DO:
                IF saldos_fincas.porcentaje > 80 OR
                    saldos_fincas.porcentaje = ? THEN DO:

                    xPorcent    = saldos_fincas.porcentaje.
                    xTrazab     = temp_itemspal.codigo_trazabilidad.
                    xErroneo    = TRUE.
                    xMensaje    = "ERROR RENDIM.TRAZAB.UE " + TRIM(xTrazab).
                END.
            END.
            ELSE DO:
                xPorcent    = 0.
                xTrazab     = temp_itemspal.codigo_trazabilidad.
                xErroneo    = TRUE.
                xMensaje    = "ERROR TRAZAB.UE " + TRIM(xTrazab) + " S/SALDO".
            END.
        END.

        IF xErroneo THEN DO:
            xUsuario    = "".
            RUN wSelRendimiento.w (INPUT-OUTPUT xErroneo, INPUT-OUTPUT xUsuario, INPUT xPorcent, INPUT xTrazab).
        END.

        IF xErroneo THEN LEAVE.
    END.
END.


IF xErroneo THEN DO:
    RUN Elimina_Testigo.
    RETURN.
END.



/***********************************/
/* Obtiene Ultimo Numero de Pallet */
/***********************************/

FIND LAST pallets WHERE
    pallets.id_suc_trabajo = xSucTrabajo NO-LOCK NO-ERROR .

IF AVAILABLE pallets THEN DO:
    xNuePal = STRING(pallets.id_pallet, "99999999").
    
    IF INTEGER(SUBSTR(xNuePal,1,2)) = INTEGER(SUBSTR(STRING(YEAR(TODAY),"9999"),3,2)) THEN
        ASSIGN
            xNuePal = STRING(INTEGER(xNuePal) + 1, "99999999")
            xNroMov = pallets.nromov + 1.
    ELSE
        ASSIGN
            xNuePal = STRING(INTEGER(SUBSTR(STRING(YEAR(TODAY),"9999"),3,2)),"99") + STRING(1, "999999")
            xNroMov = INTEGER(xNuePal).
    
    RELEASE pallets.
END.
ELSE DO:
    ASSIGN
        xNuePal = STRING(INTEGER(SUBSTR(STRING(YEAR(TODAY),"9999"),3,2)),"99") + STRING(1, "999999")
        xNroMov = INTEGER(xNuePal).
END.


xPallet = INTEGER(xNuePal).



/***********************************************/
/* Llama al procedimiento para crear el Pallet */
/***********************************************/

RUN creaNuevoPallet.



/****************************/
/* Declara el Pallet en SAP */
/****************************/

RUN SapNuevo(xSucTrabajo , xpallet) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE 'Error en interface SAP. ' SKIP RETURN-VALUE VIEW-AS ALERT-BOX ERROR.
    RUN anulaPallet.
    RETURN ERROR 'Error en interface de pallet: ' + RETURN-VALUE.
END.

FIND tpallets WHERE 
     tpallets.id_suc_trabajo     = xSucTrabajo AND
     tpallets.id_pallet          = xPallet NO-LOCK NO-ERROR.

MESSAGE 'El pallet se grabo con' SKIP
    'numero de Lote SAP: ' tpallets.id_pallet_sap  
    VIEW-AS ALERT-BOX INFORMATION.



/****************************************/
/* Llama a los procedimientos restantes */
/****************************************/

RUN grabaCajas.
RUN saldosfincas.
RUN pedidospacking.



RELEASE aux_pallets.
RELEASE aux_itemspal.
RELEASE tpallets.
RELEASE pallets.
RELEASE items_pallets.
RELEASE pedidos_packing.
RELEASE items_pedidos_packing.
RELEASE cajas.



/******************************/
/* Lee Etiqueta Pallet Senasa */
/******************************/
IF xMercado = 1 THEN
    RUN wSelEtiquetaSenasa.w
        (INPUT xSucTrabajo,
         INPUT xPallet).



/********************************/
/* Imprime etiquetas del Pallet */
/********************************/

RUN dEtiquetaPallet.p
    (INPUT xSucTrabajo,
     INPUT xPacking,
     INPUT xPallet,
     INPUT 4).

RETURN 'OK'.



/**********************************************/
/**********************************************/
/******* Anula Pallet por Error con SAP *******/
/**********************************************/
/**********************************************/

PROCEDURE anulaPallet:

    DEFINE VAR x_time AS INTEGER NO-UNDO.

    FIND pallets WHERE
        pallets.id_suc_trabajo = xsuctrabajo AND
        pallets.id_pallet = xpallet NO-ERROR.

    IF AVAILABLE pallets THEN DO:
        x_time = TIME.
        assign
            pallets.estado              = false
            pallets.id_estado_pallet    = 15 /* error sap */
            pallets.fecha_anul          = today
            pallets.hora_anul           = string(x_time,"hh:mm:ss")
            pallets.usuario_anul        = userid("userdb")
            pallets.pallet_senasa       = "".
    
        if x_time < 21600 then
            assign pallets.fecha_anulacion  = today - 1.
        else
            assign pallets.fecha_anulacion  = today.
    END.

    IF AVAILABLE pallets THEN
        FIND CURRENT pallets NO-LOCK.
    
    RELEASE pallets.

END PROCEDURE.



/**************************************************/
/**************************************************/
/******* Graba Pallet y Pedido en las cajas *******/
/**************************************************/
/**************************************************/

PROCEDURE grabaCajas.

    FOR EACH cajas WHERE
        cajas.id_suc_trabajo_lector = xSucTrabajo   AND
        cajas.id_packing_lector     = xPacking      AND
        cajas.id_lector_pallet      = xLector       AND
        cajas.id_suc_trabajo        = xSucTrabajo   AND
        cajas.id_pallet             = 0:

        ASSIGN
            cajas.id_pallet         = xPallet
            cajas.id_empresa        = temp_pedidos.id_empresa
            cajas.id_punto_emisor   = temp_pedidos.id_punto_emisor
            cajas.id_orden          = temp_pedidos.id_orden
            cajas.ITEM              = temp_pedidos.ITEM
            cajas.bultos            = xBultos
            cajas.id_tipo_pallet    = xTipoPal
            cajas.contramarca       = xContram
            cajas.id_tipo_esquinero = xTipoEsq.
    END.

    RELEASE cajas.

END PROCEDURE.



/***************************************************/
/***************************************************/
/******* Actualiza Tabla de Saldos de Fincas *******/
/***************************************************/
/***************************************************/

PROCEDURE saldosFincas:
    
    FOR EACH items_pallets NO-LOCK WHERE
        items_pallets.id_suc_trabajo    = xsuctrabajo AND
        items_pallets.id_pallet         = xPallet:
        
        RUN dd_sdofca.p
            (INPUT items_pallets.id_suc_trabajo,
            INPUT items_pallets.id_proveedor,
            INPUT items_pallets.id_origen,
            INPUT items_pallets.id_lote,
            INPUT xUEChina,
            INPUT items_pallets.codigo_trazabilidad,
            INPUT items_pallets.nro_partida_general,
            INPUT "desp").
    END.

    IF xTestigo THEN DO:
        FIND temp_itemspal WHERE
            temp_itemspal.id_testigo <> 0 NO-LOCK NO-ERROR.

        IF AVAILABLE temp_itemspal THEN
            RUN dd_sdofca.p
                (INPUT temp_itemspal.id_suc_trabajo,
                INPUT temp_itemspal.id_proveedor,
                INPUT temp_itemspal.id_origen,
                INPUT temp_itemspal.id_lote,
                INPUT xUEChina,
                INPUT temp_itemspal.codigo_trazabilidad,
                INPUT temp_itemspal.nro_partida_general,
                INPUT "desp").
    END.

END PROCEDURE.



/********************************************************************************/
/********************************************************************************/
/******* Graba Nuevo Item de Pedido por Cambio de Item y Error de Calibre *******/
/********************************************************************************/
/********************************************************************************/

PROCEDURE pedidosPacking.

    FIND tpallets WHERE
        tpallets.id_suc_trabajo = xsuctrabajo AND
        tpallets.id_pallet = xPallet NO-LOCK NO-ERROR.

    IF xNewItem THEN DO:
        FIND FIRST pedidos_packing where
            pedidos_packing.id_empresa          = 1 AND
            pedidos_packing.id_punto_emisor     = 1 AND
            pedidos_packing.id_orden            = xNewOrden NO-LOCK NO-ERROR.

        IF AVAILABLE pedidos_packing THEN DO:
            FIND FIRST items_pedidos_packing OF pedidos_packing WHERE
                items_pedidos_packing.id_articulo       = tPallets.id_articulo       AND
                items_pedidos_packing.id_variedad       = tPallets.id_variedad       AND
                items_pedidos_packing.id_marca          = tPallets.id_marca          AND
                items_pedidos_packing.id_caract         = tPallets.id_caract         AND
                items_pedidos_packing.id_tipo_esquinero = tPallets.id_tipo_esquinero AND
                items_pedidos_packing.id_tipo_pallet    = tPallets.id_tipo_pallet    AND
                items_pedidos_packing.calibre           = tPallets.calibre           AND
                items_pedidos_packing.bultos            = tPallets.bultos            NO-LOCK NO-ERROR.

            IF AVAILABLE items_pedidos_packing THEN DO:
                FIND CURRENT tpallets EXCLUSIVE-LOCK.
                ASSIGN tPallets.ITEM     = items_pedidos_packing.ITEM.
                FIND CURRENT tpallets NO-LOCK.
                FOR EACH items_pallets OF tPallets:
                    ASSIGN items_pallets.ITEM   = items_pedidos_packing.ITEM.
                END.
            END.
        END.
        RELEASE items_pallets.
    END.
    
    RELEASE tpallets.

END PROCEDURE.



/************************************/
/************************************/
/******* Crea el Nuevo Pallet *******/
/************************************/
/************************************/

PROCEDURE creaNuevopallet.

/******************************************/
/* Crea los registros de items del Pallet */
/******************************************/

    xPalItem    = 100.
    xCantCaj    = 0.
    xKilos      = 0.

    FOR EACH temp_itemspal WHERE
        temp_itemspal.id_testigo = 0
        BREAK
        BY temp_itemspal.id_empresa
        BY temp_itemspal.id_punto_emisor
        BY temp_itemspal.id_orden
        BY temp_itemspal.ITEM
        BY temp_itemspal.id_packing
        BY temp_itemspal.codigo_trazabilidad
        BY temp_itemspal.calibre
        BY temp_itemspal.id_color:

        IF FIRST-OF(temp_itemspal.id_color) THEN
            xPalItem    = xPalItem + 1.

        xCantCaj    = xCantCaj + temp_itemspal.bultos.
        xKilos      = xKilos + temp_itemspal.nro_partida_general.

        IF LAST-OF (temp_itemspal.id_color) THEN DO:
            CREATE items_pallets.
            BUFFER-COPY temp_itemspal EXCEPT item_pallet TO items_pallets.

            ASSIGN
                items_pallets.id_pallet             = xPallet
                items_pallets.item_pallet           = xPalItem
                items_pallets.bultos                = xCantCaj
                items_pallets.contramarca           = xContram
                items_pallets.nro_partida_general   = xKilos.
            
            xCantCaj    = 0.
            xKilos      = 0.
        END.
    END.


    /****************************************************/
    /* Ordena en forma descendente los items del Pallet */
    /****************************************************/

    xPalItem    = 0.

    FOR EACH items_pallets WHERE
        items_pallets.id_suc_trabajo    = xSucTrabajo AND
        items_pallets.id_pallet         = xPallet AND
        items_pallets.calibre           = xCalibre
        BY items_pallets.bultos DESC:

        xPalItem    = xPalItem + 1.
        
        ASSIGN items_pallets.item_pallet    = xPalItem.
    END.


    /*******************************************/
    /* Controla Relacion de Items con Calibres */
    /*******************************************/

    xErroneo    = FALSE.
    xNewItem    = FALSE.
    xPalItem    = 0.

    FOR EACH items_pallets NO-LOCK WHERE
        items_pallets.id_suc_trabajo    = xSucTrabajo AND
        items_pallets.id_pallet         = xPallet AND
        items_pallets.calibre           = xCalibre
        BY items_pallets.bultos DESC:
        xPalItem = xPalItem + 1.
        LEAVE.
    END.
    IF xPalItem = 0 THEN DO:
        FOR EACH items_pallets NO-LOCK WHERE
            items_pallets.id_suc_trabajo    = xSucTrabajo AND
            items_pallets.id_pallet         = xPallet
            BY items_pallets.bultos DESC:
            xCalibre    = items_pallets.calibre.
            xNewOrden   = items_pallets.id_orden.
            xNewItem    = TRUE.
            xPalItem = xPalItem + 1.
            LEAVE.
        END.
    END.
    

    /**********************************/
    /* Controla Items para Global Gap */
    /**********************************/

    xImpGLN = TRUE.
    FOR EACH items_pallets NO-LOCK WHERE
        items_pallets.id_suc_trabajo    = xSucTrabajo AND
        items_pallets.id_pallet         = xPallet:
        
        FIND FIRST origenes OF items_pallets NO-LOCK NO-ERROR.
        IF AVAILABLE origenes AND origenes.certificada = FALSE THEN DO:
            xImpGLN = FALSE.
            LEAVE.
        END.
    END.
    
    
    /**************************************************/
    /* Reasigna la numeraci¢n de los items del pallet */
    /**************************************************/

    xPalItem    = 200.

    FOR EACH items_pallets WHERE
        items_pallets.id_suc_trabajo    = xSucTrabajo AND
        items_pallets.id_pallet         = xPallet
        BY items_pallets.item_pallet:

        xPalItem    = xPalItem + 1.
        
        ASSIGN items_pallets.item_pallet    = xPalItem.
    END.

    xPalItem    = 0.

    FOR EACH items_pallets WHERE
        items_pallets.id_suc_trabajo    = xSucTrabajo AND
        items_pallets.id_pallet         = xPallet
        BY items_pallets.item_pallet:

        xPalItem    = xPalItem + 1.
        
        ASSIGN items_pallets.item_pallet    = xPalItem.

        IF items_pallets.codigo_trazabilidad = xTrazTest THEN
            ASSIGN items_pallets.con_testigo    = TRUE.
    END.

    RELEASE items_pallets.


    /********************************************************/
    /******* Obtiene Item con Mayor Cantidad de Cajas *******/
    /********************************************************/

    FIND FIRST items_pallets WHERE
        items_pallets.id_suc_trabajo    = xSucTrabajo AND
        items_pallets.id_pallet         = xPallet NO-LOCK NO-ERROR.

    IF NOT AVAILABLE items_pallets THEN DO:
        xErroneo    = TRUE.
        xMensaje    = "ITEM PARA CREAR PALLET ERRONEO".
        RETURN.
    END.

    IF xImpGLN THEN
        FIND FIRST origenes OF items_pallets NO-LOCK NO-ERROR.
    

    /********************************/
    /* Carga el registro del Pallet */
    /********************************/

    CREATE tpallets.
    ASSIGN
        tpallets.id_suc_trabajo         = xSucTrabajo
        tpallets.id_pallet              = xPallet
        tpallets.fecha_operativa        = items_pallets.fecha_operativa
        tpallets.nromov                 = xNroMov
        tpallets.id_empresa             = items_pallets.id_empresa
        tpallets.id_punto_emisor        = items_pallets.id_punto_emisor
        tpallets.id_orden               = items_pallets.id_orden
        tpallets.item                   = items_pallets.item
        tpallets.cod_prod               = items_pallets.cod_prod
        tpallets.fecha_prod             = TODAY
        tpallets.hora_prod              = STRING(TIME,"hh:mm:ss")
        tpallets.id_turno_packing       = items_pallets.id_turno_packing 
        tpallets.id_packing             = items_pallets.id_packing 
        tpallets.codigo_trazabilidad    = items_pallets.codigo_trazabilidad
        tpallets.id_proveedor           = items_pallets.id_proveedor
        tpallets.id_origen              = items_pallets.id_origen
        tpallets.id_lote                = items_pallets.id_lote
        tpallets.id_finca_senasa        = items_pallets.id_finca_senasa
        tpallets.id_lote_senasa         = items_pallets.id_lote_senasa
        tpallets.certificado            = items_pallets.certificado
        tpallets.cert_china             = items_pallets.cert_china
        tpallets.union_europea          = xUnion
        tpallets.china                  = xChina
        tpallets.gln                    = IF AVAILABLE origenes AND xImpGLN THEN origenes.gln ELSE 0
        tpallets.tipo_proceso           = items_pallets.tipo_proceso
        tpallets.renspa                 = items_pallets.renspa
        tpallets.zona_up                = items_pallets.zona_up
        tpallets.cod_trazabilidad       = items_pallets.cod_trazabilidad
        tpallets.id_marca               = xMarca
        tpallets.id_articulo            = xArticulo
        tpallets.id_variedad            = xVariedad
        tpallets.id_color               = xIdColor
        tpallets.id_envase              = xEnvase
        tpallets.id_calidad             = xCalidad
        tpallets.id_caract              = xCaract
        tpallets.id_categoria           = xCateg
        tpallets.bultos                 = xBultos
        tpallets.calibre                = items_pallets.calibre
        tpallets.contramarca            = xContram
        tpallets.id_tipo_pallet         = xTipoPal
        tpallets.pallet_chep            = xChep
        tpallets.id_tipo_esquinero      = xTipoEsq
        tpallets.id_lector_pallet       = xLector
        tpallets.id_proveedor_caja      = 0
        tpallets.testigo                = xTestigo
        tpallets.estado                 = TRUE
        tpallets.merma                  = FALSE
        tpallets.c_usuario              = xUsuario
        tpallets.c_fecha                = TODAY
        tpallets.c_hora                 = STRING(TIME,"hh:mm:ss").
    
    RELEASE tpallets.

END PROCEDURE.



/**********************************************/
/**********************************************/
/******* Elimina Caja Testigo por Error *******/
/**********************************************/
/**********************************************/

PROCEDURE Elimina_Testigo:
    
    IF dCaja <> 0 THEN DO:
        FIND cajas WHERE cajas.etiqueta = dCaja NO-ERROR.
        
        IF AVAILABLE cajas THEN DO:
            ASSIGN 
                cajas.id_suc_trabajo_lector = 0
                cajas.id_packing_lector     = 0
                cajas.id_lector_pallet      = 0
                cajas.hora_lectura_pallet   = ?
                cajas.id_testigo            = 0
                cajas.testigo               = FALSE.
        END.

        FIND cajas WHERE cajas.etiqueta = dCaja NO-LOCK NO-ERROR.
    END.

    IF dTestigo <> 0 THEN DO:
        FIND cajas WHERE cajas.id_testigo = dTestigo NO-ERROR.

        IF AVAILABLE cajas THEN DO:
            ASSIGN 
                cajas.id_suc_trabajo_lector = 0
                cajas.id_packing_lector     = 0
                cajas.id_lector_pallet      = 0
                cajas.hora_lectura_pallet   = ?
                cajas.id_testigo            = 0
                cajas.testigo               = FALSE.
        END.

        FIND cajas WHERE cajas.id_testigo = dTestigo NO-LOCK NO-ERROR.
    END.

    RELEASE cajas.

END PROCEDURE.



/*************************************/
/*************************************/
/******* Declara Pallet en SAP *******/
/*************************************/
/*************************************/

{declaraPallet.i}
