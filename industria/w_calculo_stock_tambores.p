define input parameter p_suc as integer.
define var v_tambores as integer.
define var v_kilos as integer.
define var h_con as handle.
define var v_total_tambores_of as integer.
define var v_total_kilos_of as decimal.
define var v_articulo as integer.
define var v_envase as integer.

for each stock_tambores.
    delete stock_tambores.
end.

find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.

for each tambores_industria no-lock where id_sucursal_ubicacion = p_suc
                              and id_locacion_ubicacion = 4
                              and (tambores_industria.id_tipotambor = 3
                                    or tambores_industria.id_tipotambor = 6)
                              and (tambores_industria.id_articulo <> 952
                                and tambores_industria.id_articulo <> 953
                                and tambores_industria.id_articulo <> 42
                                and tambores_industria.id_articulo <> 43) 
                               break by tambores_industria.id_lote
                                     by tambores_industria.id_articulo
                                     by year(tambores_industria.fecha).

        accumulate tambores_industria.id_lote (count by tambores_industria.id_lote
                                                     by tambores_industria.id_articulo
                                                     by year(tambores_industria.fecha)) .
        v_tambores = v_tambores + 1.
        accumulate tambores_industria.kilos_tambor (total by tambores_industria.id_lote
                                                          by tambores_industria.id_articulo
                                                          by year(tambores_industria.fecha)).
        
    if last-of(year(tambores_industria.fecha)) then                               
        do:
            
            find envases_prod of tambores_industria no-lock no-error.
            find productos_terminados of tambores_industria no-lock no-error.
            find calidades of tambores_industria no-lock no-error.
            find contratos where contratos.id_contrato      = tambores_industria.id_contrato_of
                             and contratos.id_tipo_contrato = tambores_industria.id_tipocontrato_of
                             and contratos.anio             = tambores_industria.anio_of no-lock no-error.
            
            v_total_tambores_of = 0.
            v_total_kilos_of = 0.
            for each items_contratos of contratos no-lock.
                v_total_tambores_of = v_total_tambores_of + items_contratos.cantidad.
            end.
            
            find clientes of contratos no-lock no-error.
            find r_productos_calidad where r_productos_calidad.id_articulo = tambores_industria.id_articulo
                                       and r_productos_calidad.id_calidad = tambores_industria.id_calidad
                                       no-lock no-error. 
            
            if available productos_terminados then
                do:
                    create stock_tambores.
                    assign stock_tambores.id_empresa     = tambores_industria.id_empresa
                           stock_tambores.id_sucursal    = tambores_industria.id_sucursal
                           stock_tambores.sucursal       = sucursales.abreviatura
                           stock_tambores.id_sucursal_ubicacion    = tambores_industria.id_sucursal_ubicacion 
                           stock_tambores.id_lote        = tambores_industria.id_lote 
                           stock_tambores.id_tipotambor  = tambores_industria.id_tipotambor
                           stock_tambores.anio_lote      = INTEGER(SUBSTRING(STRING(tambores_industria.anio),3,2)) 
                           stock_tambores.fecha          = tambores_industria.fecha
                           stock_tambores.id_envase      = tambores_industria.id_envase
                           stock_tambores.envase         = envases_prod.abreviatura
                           stock_tambores.id_articulo    = productos_terminados.id_articulo                   
                           stock_tambores.articulo       = productos_terminados.abreviatura.
                           if available calidades then
                            do:
                                assign stock_tambores.calidad     = calidades.abreviatura
                                       stock_tambores.id_calidad  = calidades.id_calidad.
                            end.                  
                           
                           if available contratos then
                            do:
                                 assign stock_tambores.orden_fabricacion   = string(contratos.orden_fabricacion) 
                                        stock_tambores.id_contrato         = contratos.id_contrato
                                        stock_tambores.anio                = contratos.anio
                                        stock_tambores.id_cliente          = contratos.id_cliente
                                        stock_tambores.id_tipo_contrato    = contratos.id_tipo_contrato
                                        stock_tambores.cantidad_total_of   = v_total_tambores_of
                                        stock_tambores.kilos_total_of      = v_total_tambores_of * tambores_industria.kilos_tambor
                                        stock_tambores.anio_contrato       = integer(substring(string(year(contrato.fecha)),3,2)).
        
                                if available clientes then
                                    assign stock_tambores.cliente             = clientes.nombre.
                                else
                                    assign stock_tambores.cliente             = "SIN CLIENTE ASIGNADO".
        
                            end.
                    assign stock_tambores.tambores    = (accum count by year(tambores_industria.fecha) tambores_industria.id_lote)
                           stock_tambores.kilos       = (accum total by year(tambores_industria.fecha) tambores_industria.kilos_tambor).
                           if available r_productos_calidad then
                           assign stock_tambores.kilos_400   = (accum total by year(tambores_industria.fecha) tambores_industria.kilos_tambor) * 
                                                  r_productos_calidad.coeficiente.
                    
                    v_tambores = 0.
                end.
             else message "Hay tambores con el siguiente articulo cargado " tambores_industria.id_articulo
                            view-as alert-box.
        end.
end.    


/*********************** CALCULO LOS TAMBORES DE MATERIA PRIMA DE ACEITE  ***********************/
run p_calculo_stock_mat_prima_aceite.p (input p_suc).
/************************************************************************************************/

/*********************** CALCULO LOS TAMBORES DE PRODUCTOS DE TERCEROS    ***********************/
run p_calculo_stock_tercero.p (input p_suc).
/************************************************************************************************/

/******************************* TAMBORES PRODUCCION JUGO ******************************************/
run p_calculo_produccion_jugo.p (input p_suc).
/***************************************************************************************************/ 

/******************************* TAMBORES DESPACHADOS **********************************************/
run p_calculo_tambores_despachados.p.
/***************************************************************************************************/ 