/**********  movsucu **********/
create movsucu.
assign movsucu.id_sucursal  = x_destino
    movsucu.id_tipo_movsto  = x_movsto
    movsucu.id_suc_origen   = x_destino
    movsucu.id_suc_envio    = x_origen
    movsucu.fecha_proceso   = ?.

if items_stock.hora < "0600" then
    assign movsucu.fecha = x_fecha_has.
else
    assign movsucu.fecha = x_fecha_des.

assign movsucu.hora = items_stock.hora.

assign movsucu.fecha_operativa = x_fecha_des.


/******* Graba Ingreso *******/
find tipo_numero where
    tipo_numero.id_sucursal = movsucu.id_sucursal and
    tipo_numero.id_tipo_movsto = movsucu.id_tipo_movsto
    no-error.
    
if available tipo_numero then
    x_numero = tipo_numero.nro + 1.
else
    x_numero = 1.

assign movsucu.nro      = x_numero.
assign tipo_numero.nro  = x_numero.


/******* Graba Egreso *******/
find r_cendis_suc where
    r_cendis_suc.id_cendis > 1 and
    r_cendis_suc.id_cendis < 4 and
    r_cendis_suc.id_sucursal = movsucu.id_suc_envio
    no-lock no-error.
    
if available r_cendis_suc then do:
    if r_cendis_suc.llevar_stock = true then do:
        x_lleva_stock = true.
        
        find tipo_numero where
            tipo_numero.id_sucursal = movsucu.id_suc_envio and
            tipo_numero.id_tipo_movsto = 4
            no-error.
            
        if available tipo_numero then
            x_num_envio = tipo_numero.nro + 1.
        else
            x_num_envio = 1.

        assign movsucu.nro_envio = x_num_envio.
        
        create aux_movsucu.
        
        assign aux_movsucu.id_sucursal  = movsucu.id_suc_envio
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

        assign tipo_numero.nro = x_num_envio.
    end.
    else do:
        x_lleva_stock = false.
    end.
end.


/******* Controla carga de  items_stock *******/
assign items_stock.id_sucursal   = movsucu.id_sucursal
    items_stock.id_tipo_movsto   = movsucu.id_tipo_movsto
    items_stock.id_suc_envio     = movsucu.id_suc_envio
    items_stock.nro              = movsucu.nro
    items_stock.fecha            = movsucu.fecha
    items_stock.fecha_operativa  = movsucu.fecha_operativa
    items_stock.dia              = weekday( movsucu.fecha)
    items_stock.codigo_stock     = "I".

if items_stock.cantidad > g_resto_bdjas then
    assign items_stock.cantidad = g_resto_bdjas.

find r_envases_prod where
    r_envases_prod.id_envase     = items_stock.id_envase and
    r_envases_prod.id_articulo   = items_stock.id_articulo.

assign
    items_stock.peso       = r_envases_prod.kilos * items_stock.cantidad
    items_stock.cantidad1  = items_stock.cantidad.


/******* Graba saldos packing ******/
run graba_saldos_packing.

assign
    items_stock.nro_partida            = x_partida
    items_stock.nro_partida_serial     = x_partida_s
    items_stock.nro_partida_origen     = x_partida_o
    items_stock.id_sucursal_etiqueta   = x_suc_etiq
    items_stock.id_etiqueta            = x_etiqueta.

if items_stock.id_serial <> 0 then
    assign items_stock.id_serial = next-value(stock, general).

if x_lleva_stock = true then do:
    create aux_items.
    run graba_aux_camaras.    
end.

x_peso_fruta = x_peso_fruta + items_stock.peso.
if x_peso_fruta <> 0 then
    x_total_bdjas = x_peso_fruta / 20.


/******* Busca registro de movsucu y asigna el peso *******/
find movsucu where
    movsucu.id_sucursal       = items_stock.id_sucursal    and
    movsucu.id_tipo_movsto    = items_stock.id_tipo_movsto and
    movsucu.id_suc_envio      = items_stock.id_suc_envio   and
    movsucu.nro               = items_stock.nro.

assign movsucu.peso = items_stock.peso.


/******* Contrapartida items_stock *******/
if x_lleva_stock = true then do:
    find aux_items where
        aux_items.id_sucursal       = movsucu.id_suc_envio  and
        aux_items.id_tipo_movsto    = 4 and
        aux_items.id_suc_envio      = movsucu.id_sucursal   and
        aux_items.nro               = movsucu.nro_envio     and
        aux_items.item              = items_stock.item  no-error.

    x_num_envio = aux_items.nro.
    
    run graba_aux_camaras.
end.


/******* Contrapartida  movsucu y asignacion del peso total *******/
if x_lleva_stock = true then do:
    find aux_movsucu where
        aux_movsucu.id_sucursal     = aux_items.id_sucursal     and
        aux_movsucu.id_tipo_movsto  = aux_items.id_tipo_movsto  and
        aux_movsucu.id_suc_envio    = aux_items.id_suc_envio    and
        aux_movsucu.nro             = aux_items.nro.

    assign
        aux_movsucu.fecha   = aux_items.fecha
        aux_movsucu.hora    = aux_items.hora
        aux_movsucu.peso    = aux_items.peso.
end.



release movsucu.
release aux_movsucu.
release items_stock.
release aux_items.
release tipo_numero.


/*****************************/
/******* Interface SAP *******/
/*****************************/
RUN Interface_Packingn.p
   (INPUT x_destino,
   INPUT x_movsto,
   INPUT x_origen,
   INPUT x_numero) no-error.
   
IF error-status:error then
    undo _sale , leave _sale.
   

g_resto_bdjas = 0.

display
    x_peso_fruta  @ x_peso_fruta
    x_total_bdjas @ x_total_bdjas
    with frame f-base.


/******* Llama al programa para imprimir etiquetas de balanza *******/
run dd_etipla.p (input x_partida, input x_partida_s).
