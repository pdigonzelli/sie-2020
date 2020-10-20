define input parameter p_suc as integer.
define input parameter p_fecha as date.
define var v_tambores as integer.
define var h_con as handle.

define temp-table tt_tambores
    field id_empresa as integer
    field id_sucursal as integer
    field id_sucursal_ubicacion as integer
    field id_lote as integer
    field id_tipotambor as integer
    field id_articulo as integer
    field id_calidad as integer
    field id_envase as integer
    field id_contrato_of as char
    field id_tipocontrato_of as integer
    field anio_of as integer
    field item_of as integer
    field kilos_tambor as decimal
    field id_etiqueta as integer
    field fecha as date
    index etiqueta id_etiqueta.

    
run p_stock_a_fecha_BsAs.p (input p_suc, input p_fecha,output table tt_tambores). 

for each stock_tambores_a_fecha.
    delete stock_tambores_a_fecha.
end.

find sucursales where sucursales.id_sucursal = p_suc no-lock no-error.

for each tt_tambores break by tt_tambores.id_sucursal
                           by tt_tambores.id_tipotambor
                           by tt_tambores.id_lote
                           by tt_tambores.id_articulo
                           by tt_tambores.id_calidad
                           by year(tt_tambores.fecha).
    
            accumulate tt_tambores.id_lote (count by tt_tambores.id_sucursal
                                                  by tt_tambores.id_tipotambor
                                                  by tt_tambores.id_lote
                                                  by tt_tambores.id_articulo
                                                  by tt_tambores.id_calidad
                                                  by year(tt_tambores.fecha)) .
            v_tambores = v_tambores + 1.
            accumulate tt_tambores.kilos_tambor (total by tt_tambores.id_sucursal
                                                       by tt_tambores.id_tipotambor
                                                       by tt_tambores.id_lote
                                                       by tt_tambores.id_articulo
                                                       by tt_tambores.id_calidad
                                                       by year(tt_tambores.fecha)).
            
        if last-of(year(tt_tambores.fecha)) then                               
            do:
                
                find envases_prod of tt_tambores no-lock no-error.
                find productos_terminados of tt_tambores no-lock no-error.
                find calidades of tt_tambores no-lock no-error.
                find tipostambor of tt_tambores no-lock no-error.
                find contratos where contratos.id_contrato      = tt_tambores.id_contrato_of
                                 and contratos.id_tipo_contrato = tt_tambores.id_tipocontrato_of
                                 and contratos.anio             = tt_tambores.anio_of no-lock no-error.
    
                find clientes of contratos no-lock no-error.
                find r_productos_calidad where r_productos_calidad.id_articulo = tt_tambores.id_articulo
                                           and r_productos_calidad.id_calidad = tt_tambores.id_calidad
                                           no-lock no-error. 
                                             
                create stock_tambores_a_fecha.
                assign stock_tambores_a_fecha.id_empresa     = tt_tambores.id_empresa
                       stock_tambores_a_fecha.id_sucursal    = tt_tambores.id_sucursal
                       stock_tambores_a_fecha.sucursal       = sucursales.abreviatura
                       stock_tambores_a_fecha.id_sucursal_ubicacion    = tt_tambores.id_sucursal_ubicacion 
                       stock_tambores_a_fecha.id_lote        = tt_tambores.id_lote 
                       stock_tambores_a_fecha.id_tipotambor  = tt_tambores.id_tipotambor
                       stock_tambores_a_fecha.anio_lote      = INTEGER(SUBSTRING(STRING(tt_tambores.anio),3,2)) 
                       stock_tambores_a_fecha.fecha          = tt_tambores.fecha
                       stock_tambores_a_fecha.id_envase      = tt_tambores.id_envase
                       stock_tambores_a_fecha.envase         = envases_prod.abreviatura
                       stock_tambores_a_fecha.id_articulo    = productos_terminados.id_articulo                   
                       stock_tambores_a_fecha.articulo       = productos_terminados.abreviatura.
                       
                       if available tipostambor then
                        do:
                            assign stock_tambores_a_fecha.tipotambor  = tipostambor.descripcion.
                        end.
                        
                       if available calidades then
                        do:
                            assign stock_tambores_a_fecha.calidad     = calidades.abreviatura
                                   stock_tambores_a_fecha.id_calidad  = calidades.id_calidad.
                        end.                  
                       
                       if available contratos then
                        do:
                             assign stock_tambores_a_fecha.orden_fabricacion   = string(contratos.orden_fabricacion) 
                                    stock_tambores_a_fecha.id_contrato         = contratos.id_contrato
                                    stock_tambores_a_fecha.id_cliente          = contratos.id_cliente
                                    stock_tambores_a_fecha.cliente             = clientes.nombre.
                        end.
                        
                assign stock_tambores_a_fecha.tambores    = (accum count by year(tt_tambores.fecha) tt_tambores.id_lote)
                       stock_tambores_a_fecha.kilos       = (accum total by year(tt_tambores.fecha) tt_tambores.kilos_tambor).
                       if available r_productos_calidad then
                           assign stock_tambores_a_fecha.kilos_400   = (accum total by year(tt_tambores.fecha) tt_tambores.kilos_tambor) * 
                                                  r_productos_calidad.coeficiente.
                       else do:
                        if tt_tambores.id_tipotambor = 1 or
                           tt_tambores.id_articulo   = 42 or 
                           tt_tambores.id_articulo   = 43 then
                        assign stock_tambores_a_fecha.kilos_400   = (accum total by year(tt_tambores.fecha) tt_tambores.kilos_tambor).
                       end.
                
                v_tambores = 0.
            end.
 
end.