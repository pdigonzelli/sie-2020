define input parameter p_suc as integer.
define input parameter p_articulo as integer.
define input parameter p_orden as integer.

define var v_tambores as integer.
define var v_tam_pedidos as integer.
define var v_kilos_pedidos as decimal format ">>>,>>>,>>9.99".
define var v_kilos_prod as decimal format ">>>,>>>,>>9.99".
define var v_kilos_despachados as decimal format ">>>,>>>,>>9.99".
define var v_kilos_stock as decimal format ">>>,>>>,>>9.99".
define var v_kilos_faltantes as decimal format ">>>,>>>,>>9.99".
define var v_kilos as integer.
define var h_con as handle.
define var v_total_tambores_of as integer.
define var v_total_kilos_of as decimal.
define var v_envase as integer.
define buffer b_tam for tambores_industria.

find comercial.sucursales where comercial.sucursales.id_sucursal = p_suc no-lock no-error.

/******************** TAMBORES DE LOTE JUGO CON OF **********************************/
for each tambores_industria no-lock where id_sucursal_ubicacion = p_suc
                              and tambores_industria.id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 3
                              and tambores_industria.id_articulo = p_articulo
                              and tambores_industria.id_contrato_of <> ""
                              and tambores_industria.id_tipocontrato_of <> 0
                              and tambores_industria.anio_of <> 0,
                              first lotes_jugo of tambores_industria /* where lotes_jugo.estado_lote >= 2 */
                              break by tambores_industria.id_contrato_of
                                    by tambores_industria.id_lote
                                    by year(tambores_industria.fecha).

        accumulate tambores_industria.id_lote (count by tambores_industria.id_lote
                                                     by year(tambores_industria.fecha)) .
        v_tambores = v_tambores + 1.
        v_tam_pedidos = v_tam_pedidos + 1.
        v_kilos_pedidos = v_kilos_pedidos + tambores_industria.kilos_tambor.
        
        accumulate tambores_industria.kilos_tambor (total by tambores_industria.id_lote
                                                          by year(tambores_industria.fecha)).
    
                
    if last-of(year(tambores_industria.fecha)) then                               
        do:
            
            find envases_prod of tambores_industria no-lock no-error.
            find estados_lotes where estados_lotes.id_estado_lote = lotes_jugo.estado_lote no-lock no-error.
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
                           stock_tambores.id_articulo    = IF AVAILABLE productos_terminados THEN productos_terminados.id_articulo ELSE 0
                           stock_tambores.articulo       = IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE "NONE"
                           stock_tambores.orden_reporte  = p_orden
                           stock_tambores.id_estado      = lotes_jugo.estado_lote
                           stock_tambores.estado         = estados_lotes.descripcion when available estados_lotes.
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
        /*                                stock_tambores.item                = contratos.item
                                        stock_tambores.cantidad_pedido     = items_contratos.cantidad
                                        stock_tambores.semana_desde        = items_contrato.semana_entrega
                                        stock_tambores.semana_hasta        = items_contrato.semana_entrega_hasta
                                        stock_tambores.anio_semana         = items_contrato.anio_semana */
                                        stock_tambores.id_tipo_contrato    = contratos.id_tipo_contrato
                                        stock_tambores.cantidad_total_of   = v_total_tambores_of
                                        stock_tambores.kilos_total_of      = v_total_tambores_of * tambores_industria.kilos_tambor
                                        stock_tambores.anio_contrato       = integer(substring(string(year(contrato.fecha)),3,2)).

                                 v_kilos_stock = v_kilos_stock + (v_total_tambores_of * tambores_industria.kilos_tambor).
        
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
             
             if last-of(tambores_industria.id_contrato_of) then
                do:
                    
                    for each b_tam where b_tam.id_contrato_of = contratos.id_contrato
                                     and b_tam.id_tipocontrato_of = contratos.id_tipo_contrato
                                     and b_tam.anio_of = contratos.anio no-lock.
                        v_kilos_prod = v_kilos_prod + b_tam.kilos_tambor.
                        if b_tam.id_tipo_movsto > 0 and 
                           b_tam.nro_remito > 0 then 
                            v_kilos_despachados = v_kilos_despachados + b_tam.kilos_tambor.
                    end.
                    
                    v_kilos_faltantes = v_kilos_pedidos - v_kilos_prod.
                    
                    create info_contrato.
                    assign info_contrato.orden_fabricacion  = string(contratos.orden_fabricacion)
                           info_contrato.anio_corto         = integer(substring(string(contratos.anio),3,2))
                           info_contrato.id_contrato        = contratos.id_contrato
                           info_contrato.id_tipo_contrato   = contratos.id_tipo_contrato
                           info_contrato.anio               = INTEGER(SUBSTRING(STRING(contratos.anio),3,2))
                           info_contrato.id_cliente         = clientes.id_cliente
                           info_contrato.cliente            = contratos.nombre
                           info_contrato.tambores_pedidos   = v_tam_pedidos
                           info_contrato.kilos_pedidos      = v_kilos_pedidos
                           info_contrato.kilos_producidos   = v_kilos_prod
                           info_contrato.kilos_despachados  = v_kilos_despachados
                           info_contrato.kilos_stock        = v_kilos_stock
                           info_contrato.kilos_faltante    = v_kilos_faltantes.
                           
                    v_tam_pedidos = 0.
                    v_kilos_pedidos = 0.
                    v_kilos_prod = 0.
                    v_kilos_despachados = 0.
                    v_kilos_stock = 0.
                    v_kilos_faltantes = 0.
                end.

        end.
end.    

/******************** TAMBORES DE LOTE JUGO SIN OF **********************************/
for each tambores_industria no-lock where id_sucursal_ubicacion = p_suc
                              and tambores_industria.id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 3
                              and tambores_industria.id_articulo = p_articulo 
                              and tambores_industria.id_contrato_of = ""
                              and tambores_industria.id_tipocontrato_of = 0
                              and tambores_industria.anio_of = 0,
                              first lotes_jugo of tambores_industria /* where lotes_jugo.estado_lote >= 2 */
                              break by tambores_industria.id_lote
                                     by year(tambores_industria.fecha).

        accumulate tambores_industria.id_lote (count by tambores_industria.id_lote
                                                     by year(tambores_industria.fecha)) .
        v_tambores = v_tambores + 1.
        accumulate tambores_industria.kilos_tambor (total by tambores_industria.id_lote
                                                          by year(tambores_industria.fecha)).
        
    if last-of(year(tambores_industria.fecha)) then                               
        do:
            
            find envases_prod of tambores_industria no-lock no-error.
            find estados_lotes where estados_lotes.id_estado_lote = lotes_jugo.estado_lote no-lock no-error.            
            find productos_terminados of tambores_industria no-lock no-error.
            find calidades of tambores_industria no-lock no-error.
                      
            v_total_tambores_of = 0.
            v_total_kilos_of = 0.
                        
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
                           stock_tambores.id_articulo    = IF AVAILABLE productos_terminados THEN productos_terminados.id_articulo ELSE 0                  
                           stock_tambores.articulo       = IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE "NONE"
                           stock_tambores.orden_reporte  = p_orden + 1
                           stock_tambores.id_estado      = lotes_jugo.estado_lote
                           stock_tambores.estado         = estados_lotes.descripcion when available estados_lotes.
                           if available calidades then
                            do:
                                assign stock_tambores.calidad     = calidades.abreviatura
                                       stock_tambores.id_calidad  = calidades.id_calidad.
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



/********* LOTES DE ACEITE*********************************************************************************/

/******************** TAMBORES DE LOTE ACEITE CON OF **********************************/
for each tambores_industria no-lock where id_sucursal_ubicacion = p_suc
                              and tambores_industria.id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 6
                              and tambores_industria.id_articulo = p_articulo
                              and tambores_industria.id_contrato_of <> ""
                              and tambores_industria.id_tipocontrato_of <> 0
                              and tambores_industria.anio_of <> 0,
                              first lotes_aceite of tambores_industria /* where lotes_aceite.estado_lote >= 2 */
                              break by tambores_industria.id_contrato_of
                                    by tambores_industria.id_lote
                                    by year(tambores_industria.fecha).

        accumulate tambores_industria.id_lote (count by tambores_industria.id_lote
                                                     by year(tambores_industria.fecha)) .
        v_tambores = v_tambores + 1.
        accumulate tambores_industria.kilos_tambor (total by tambores_industria.id_lote
                                                          by year(tambores_industria.fecha)).
    
    if last-of(tambores_industria.id_contrato_of) then
        do:
        
        end.
        
    if last-of(year(tambores_industria.fecha)) then                               
        do:
            
            find envases_prod of tambores_industria no-lock no-error.
            find estados_lotes where estados_lotes.id_estado_lote = lotes_aceite.estado_lote no-lock no-error.
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
                           stock_tambores.id_articulo    = IF AVAILABLE productos_terminados THEN productos_terminados.id_articulo ELSE 0                   
                           stock_tambores.articulo       = IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE "NONE"
                           stock_tambores.orden_reporte  = p_orden
                           stock_tambores.id_estado      = lotes_aceite.estado_lote
                           stock_tambores.estado         = estados_lotes.descripcion when available estados_lotes.
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
        /*                                stock_tambores.item                = contratos.item
                                        stock_tambores.cantidad_pedido     = items_contratos.cantidad
                                        stock_tambores.semana_desde        = items_contrato.semana_entrega
                                        stock_tambores.semana_hasta        = items_contrato.semana_entrega_hasta
                                        stock_tambores.anio_semana         = items_contrato.anio_semana */
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

/******************** TAMBORES DE LOTE ACEITE SIN OF **********************************/
for each tambores_industria no-lock where id_sucursal_ubicacion = p_suc
                              and tambores_industria.id_locacion_ubicacion = 4
                              and tambores_industria.id_tipotambor = 6
                              and tambores_industria.id_articulo = p_articulo 
                              and tambores_industria.id_contrato_of = ""
                              and tambores_industria.id_tipocontrato_of = 0
                              and tambores_industria.anio_of = 0,
                              first lotes_aceite of tambores_industria /* where lotes_aceite.estado_lote >= 2 */
                              break by tambores_industria.id_lote
                                     by year(tambores_industria.fecha).

        accumulate tambores_industria.id_lote (count by tambores_industria.id_lote
                                                     by year(tambores_industria.fecha)) .
        v_tambores = v_tambores + 1.
        accumulate tambores_industria.kilos_tambor (total by tambores_industria.id_lote
                                                          by year(tambores_industria.fecha)).
        
    if last-of(year(tambores_industria.fecha)) then                               
        do:
            
            find envases_prod of tambores_industria no-lock no-error.
            find estados_lotes where estados_lotes.id_estado_lote = lotes_aceite.estado_lote no-lock no-error.
            find productos_terminados of tambores_industria no-lock no-error.
            find calidades of tambores_industria no-lock no-error.
                      
            v_total_tambores_of = 0.
            v_total_kilos_of = 0.
                        
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
                           stock_tambores.id_articulo    = IF AVAILABLE productos_terminados THEN productos_terminados.id_articulo ELSE 0                
                           stock_tambores.articulo       = IF AVAILABLE productos_terminados THEN productos_terminados.abreviatura ELSE "NONE"
                           stock_tambores.orden_reporte  = p_orden + 1
                           stock_tambores.id_estado      = lotes_aceite.estado_lote
                           stock_tambores.estado         = estados_lotes.descripcion when available estados_lotes.
                           if available calidades then
                            do:
                                assign stock_tambores.calidad     = calidades.abreviatura
                                       stock_tambores.id_calidad  = calidades.id_calidad.
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
