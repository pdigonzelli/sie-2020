if x_sum_caj <> 0 then
    apply "END-ERROR".

x_time = time.

assign
    pallets.fecha_prod          = today
    pallets.hora_prod           = string(x_time,"hh:mm:ss").

if x_time < 21600 then
    assign pallets.fecha_operativa = today - 1.
else
    assign pallets.fecha_operativa = today.


/******* Obtiene el peso del pallet *******/
find first r_envases_prod of pallets
    no-lock no-error.
x_kilos     = 0.
x_pal_item  = 0.

/******* Graba todos los datos en items_pallets *******/
for each temp_items
    by temp_items.bultos desc:

    x_pal_item  = x_pal_item + 1.

    create items_pallets.
    assign
        items_pallets.id_sucursal           = temp_items.id_sucursal
        items_pallets.id_suc_trabajo        = temp_items.id_suc_trabajo
        items_pallets.id_pallet             = temp_items.id_pallet
        items_pallets.item_pallet           = x_pal_item
        items_pallets.id_turno_packing      = temp_items.id_turno_packing 
        items_pallets.id_packing            = temp_items.id_packing 
        items_pallets.fecha_operativa       = pallets.fecha_operativa
        items_pallets.codigo_trazabilidad   = temp_items.codigo_trazabilidad
        items_pallets.id_proveedor          = temp_items.id_proveedor
        items_pallets.id_origen             = temp_items.id_origen
        items_pallets.id_lote               = temp_items.id_lote
        items_pallets.id_finca_senasa       = temp_items.id_finca_senasa
        items_pallets.id_lote_senasa        = temp_items.id_lote_senasa
        items_pallets.certificado           = temp_items.certificado
        items_pallets.cert_china            = temp_items.cert_china
        items_pallets.renspa                = temp_items.renspa
        items_pallets.zona_up               = temp_items.zona_up
        items_pallets.cod_trazabilidad      = temp_items.cod_trazabilidad
        items_pallets.bultos                = temp_items.bultos.

        x_kilos = temp_items.bultos * r_envases_prod.kilos.

    assign
        items_pallets.cod_prod              = pallets.cod_prod
        items_pallets.id_color              = pallets.id_color
        items_pallets.tipo_proceso          = pallets.tipo_proceso
        items_pallets.id_empresa            = pallets.id_empresa
        items_pallets.id_punto_emisor       = pallets.id_punto_emisor
        items_pallets.id_orden              = pallets.id_orden
        items_pallets.item                  = pallets.item
        items_pallets.id_marca              = pallets.id_marca
        items_pallets.id_articulo           = pallets.id_articulo
        items_pallets.id_variedad           = pallets.id_variedad
        items_pallets.id_envase             = pallets.id_envase
        items_pallets.id_calidad            = pallets.id_calidad
        items_pallets.id_caract             = pallets.id_caract
        items_pallets.id_categoria          = pallets.id_categoria
        items_pallets.calibre               = pallets.calibre
        items_pallets.contramarca           = pallets.contramarca
        items_pallets.nro_partida_general   = x_kilos.
end.

/******* Controla Items para Global Gap *******/
xImpGLN = TRUE.
for each temp_items:
    FIND FIRST origenes OF temp_items NO-LOCK NO-ERROR.
    if AVAILABLE origenes and origenes.certificada = FALSE then do:
        xImpGLN = FALSE.
        leave.
    end.
end.

if xImpGLN then
    assign pallets.gln  = origenes.gln.

/******* Graba la primera trazabilidad en pallets *******/
for each temp_items
    by temp_items.bultos desc:
    assign
        pallets.id_turno_packing    = temp_items.id_turno_packing 
        pallets.id_packing          = temp_items.id_packing 
        pallets.codigo_trazabilidad = temp_items.codigo_trazabilidad
        pallets.id_proveedor        = temp_items.id_proveedor
        pallets.id_origen           = temp_items.id_origen
        pallets.id_lote             = temp_items.id_lote
        pallets.id_finca_senasa     = temp_items.id_finca_senasa
        pallets.id_lote_senasa      = temp_items.id_lote_senasa
        pallets.renspa              = temp_items.renspa
        pallets.zona_up             = temp_items.zona_up
        pallets.cod_trazabilidad    = temp_items.cod_trazabilidad.
    leave.
end.

/******* Obtiene los ordenes disponibles *******/
x_valor     = 0.
x_ubicacion = 0.
x_cant_pal  = 1.

for each aux_pal_ord use-index nro_orden no-lock where
    aux_pal_ord.id_empresa      = items_pedidos_packing.id_empresa and
    aux_pal_ord.id_punto_emisor = items_pedidos_packing.id_punto_emisor and
    aux_pal_ord.id_orden        = items_pedidos_packing.id_orden and
    aux_pal_ord.id_suc_trabajo  = x_sucursal and
    aux_pal_ord.nro_orden       <> 0:
    
    x_valor = x_valor + 1.

    if aux_pal_ord.nro_orden > x_valor then do:
        x_ubicacion = x_valor.
        leave.
    end.
end.

/******* Graba el orden del pallet *******/
if x_ubicacion <> 0 then do:
    assign pallets.nro_orden = x_ubicacion.
end.
else do:
    find last aux_pallets use-index nro_orden where
        aux_pallets.id_empresa      = items_pedidos_packing.id_empresa and
        aux_pallets.id_punto_emisor = items_pedidos_packing.id_punto_emisor and
        aux_pallets.id_orden        = items_pedidos_packing.id_orden and
        aux_pallets.id_suc_trabajo  = x_sucursal
        no-lock no-error.

    if available aux_pallets then
        assign pallets.nro_orden = aux_pallets.nro_orden + 1.
    else
        assign pallets.nro_orden = 1.
end.

/******* Imprime Etiquetas de Pallet *******/
x_pal_ini   = pallets.id_pallet.
x_packing   = pallets.id_packing.

DO:
    DEFINE VAR hAppSrv AS HANDLE NO-UNDO.
    DEFINE VAR RET AS LOGICAL NO-UNDO.
    
    CREATE SERVER hAppSrv.
    
    ret = hAppSrv:CONNECT("-H samiprogdesa -S 5162 -AppService sap3").
        
    FIND PALLETS WHERE PALLETS.ID_SUC_TRABAJO = 98 AND PALLETS.ID_PALLET = 16000032 NO-LOCK.
    
    RUN  PP159INGP1.P  ON hAppSrv  TRANSACTION DISTINCT (pallets.id_suc_trabajo, pallets.id_pallet, OUTPUT CSTATUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX TITLE 'ERROR'.
        UNDO _SALE , LEAVE _SALE.
    END.
    
    FINALLY.
        IF hAppSrv:CONNECTED () THEN
            ret = hAppSrv:DISCONNECT().
        DELETE OBJECT hAppSrv.
    END FINALLY.
END.
run d_etipal.p
    (input x_sucursal,
    input x_pal_ini).
