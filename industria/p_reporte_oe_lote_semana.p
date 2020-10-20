define input parameter p_semana as integer.
define var v_lotes as char.
define var v_cantidad as integer.
define var v_anio as integer.

for each oe_lote_semana.
    delete oe_lote_semana.    
end.

for each orden_entrega where orden_entrega.semana_embarque = p_semana no-lock.
    find vapores of orden_entrega no-lock no-error.
    find destinos of orden_entrega no-lock no-error.
    find clientes of orden_entrega no-lock no-error.
    find calidades of orden_entrega no-lock no-error.
    find productos_terminados of orden_entrega no-lock no-error.
    find contratos of orden_entrega no-lock no-error.
    find items_contratos of orden_entrega no-lock no-error.
    find lugar_descarga of orden_entrega no-lock no-error.
    
    for each tambores_industria of orden_entrega break by id_lote.
        
        v_cantidad = v_cantidad + 1.
        if last-of(id_lote) then
            do:
                if v_lotes = "" then
                  do:
                    v_lotes = string(tambores_industria.id_lote) + "/" + 
                              SUBSTRING(STRING(tambores_industria.anio),3,2) + " (" +
                              string(v_cantidad) + ")". 
                    v_cantidad = 0.
                  end.
                else
                  do:
                    v_lotes = v_lotes + " - " +
                              string(tambores_industria.id_lote) + "/" + 
                              SUBSTRING(STRING(tambores_industria.anio),3,2) + " (" +
                              string(v_cantidad) + ")".
                    v_cantidad = 0.
                  end.
            end.
    end.
    
    create oe_lote_semana.
    assign oe_lote_semana.id_orden_entrega  = orden_entrega.id_orden_entrega
           oe_lote_semana.semana            = orden_entrega.semana_embarque
           oe_lote_semana.fecha             = orden_entrega.fecha
           oe_lote_semana.id_vapor          = orden_entrega.id_vapor
           oe_lote_semana.id_destino        = orden_entrega.id_destino
           oe_lote_semana.id_cliente        = orden_entrega.id_cliente
           oe_lote_semana.id_contrato       = orden_entrega.id_contrato
           oe_lote_semana.id_tipo_contrato  = orden_entrega.id_tipo_contrato
           oe_lote_semana.anio              = orden_entrega.anio
           oe_lote_semana.item              = orden_entrega.item
           oe_lote_semana.id_calidad        = orden_entrega.id_calidad
           oe_lote_semana.id_articulo       = orden_entrega.id_articulo
           oe_lote_semana.id_lugar_descarga = orden_entrega.id_lugdes
           oe_lote_semana.lotes             = v_lotes
           oe_lote_semana.tambores          = orden_entrega.cantidad_tambores.
    
    if available vapores then assign oe_lote_semana.vapor = vapores.descripcion. else assign oe_lote_semana.vapor = "null".
    if available destinos then assign oe_lote_semana.destino = destinos.descripcion. else assign oe_lote_semana.destino = "null".
    if available clientes then assign oe_lote_semana.cliente = clientes.nombre. else assign oe_lote_semana.cliente = "null".
    if available calidades then assign oe_lote_semana.calidad = calidades.abreviatura. else assign oe_lote_semana.calidad = "null".
    if available productos_terminados then assign oe_lote_semana.articulo = productos_terminados.abreviatura. else assign oe_lote_semana.articulo = "null".
    if available lugar_descarga then assign oe_lote_semana.lugar_descarga = lugar_descarga.descripcion. else assign oe_lote_semana.lugar_descarga = "null".
    if available contratos then assign oe_lote_semana.orden_fabricacion = string(contratos.orden_fabricacion). else assign oe_lote_semana.orden_fabricacion = "null".
    if available items_contratos then
        do:
            assign oe_lote_semana.observaciones = items_contratos.observaciones
                   oe_lote_semana.marca_tambores = items_contratos.marca_tambores
                   oe_lote_semana.release_nro       = items_contratos.numero_release[1]
                   oe_lote_semana.po_cliente        = items_contratos.id_po_cliente[1]
                   oe_lote_semana.cod_prod_cliente  = items_contratos.id_articulo_cliente[1].
            
        end.
    else
        do:
            assign oe_lote_semana.observaciones = "null"
                   oe_lote_semana.marca_tambores = "null".
            
        end.
    v_lotes = "".
    v_cantidad = 0.
end.