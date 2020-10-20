/* Desde remitos */
for each stock_histo where
            stock_histo.id_empresa    = 1  and
            stock_histo.id_sucursal   = 96 and
            stock_histo.id_tipotambor = 3 and
            stock_histo.nromov        = 23701
                        no-lock:
        displa 
        stock_historico_tambores.id_articulo 
        stock_historico_tambores.fecha
        stock_historico_tambores.id_lote 
        stock_historico_tambores.anio 
        stock_historico_tambores.id_serial format ">99"
        stock_historico_tambores.datos_adicionales  
        stock_historico_tambores.id_suc_origen 
        stock_historico_tambores.id_suc_des         
        stock_historico_tambores.signo column-label "S"  
        stock_historico_tambores.tambor_desde column-label "Desde"
        stock_historico_tambores.tambor_hasta column-label "Hasta"
        stock_historico_tambores.id_tipo_movimiento  column-label "Tipo"
        stock_historico_tambores.id_empresa 
        stock_historico_tambores.id_sucursal 
        stock_historico_tambores.id_tipotambor  
        stock_historico_tambores.nromov
        with scrollable. 
end.    
wait-for f4 of current-window.

for each stock_histo where
            stock_histo.id_empresa    = 1  and
            stock_histo.id_sucursal   = 95 and
            stock_histo.id_tipotambor = 3 and
            stock_histo.nromov        = 22936
                        no-lock:
        displa 
        stock_historico_tambores.id_articulo 
        stock_historico_tambores.fecha
        stock_historico_tambores.id_lote 
        stock_historico_tambores.anio 
        stock_historico_tambores.id_serial format ">99"
        stock_historico_tambores.datos_adicionales  
        stock_historico_tambores.id_suc_origen 
        stock_historico_tambores.id_suc_des         
        stock_historico_tambores.signo column-label "S"  
        stock_historico_tambores.tambor_desde column-label "Desde"
        stock_historico_tambores.tambor_hasta column-label "Hasta"
        stock_historico_tambores.id_tipo_movimiento  column-label "Tipo"
        stock_historico_tambores.id_empresa 
        stock_historico_tambores.id_sucursal 
        stock_historico_tambores.id_tipotambor  
        stock_historico_tambores.nromov
        with scrollable. 
end.
wait-for f4 of current-window.
for each movimientos_tambores no-lock.
    display movimientos_tambores.
    for each stock_histo where
            stock_histo.id_empresa    = movimientos_tambores.id_empresa  and
            stock_histo.id_sucursal   = movimientos_tambores.id_sucursal and
            stock_histo.id_tipotambor = movimientos_tambores.id_tipotambor and
            stock_histo.nromov        = movimientos_tambores.nromov_mov
            no-lock:
        displa 
        stock_historico_tambores.id_articulo 
        stock_historico_tambores.fecha
        stock_historico_tambores.id_lote 
        stock_historico_tambores.anio 
        stock_historico_tambores.id_serial format ">99"
/*        stock_historico_tambores.datos_adicionales  */
        stock_historico_tambores.id_suc_origen 
        stock_historico_tambores.id_suc_des         
        stock_historico_tambores.signo column-label "S"  
        stock_historico_tambores.tambor_desde column-label "Desde"
        stock_historico_tambores.tambor_hasta column-label "Hasta"
        stock_historico_tambores.id_tipo_movimiento  column-label "Tipo"
        stock_historico_tambores.id_empresa 
        stock_historico_tambores.id_sucursal 
        stock_historico_tambores.id_tipotambor  
        stock_historico_tambores.nromov
        with scrollable. 
    end.
end.