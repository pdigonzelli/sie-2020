define input parameter p_suc as integer.
define input parameter p_fecha as date.

define var v_lote as integer format ">>>9".
define var v_anio_lote as integer format "9999".
define var v_suc as integer.
define var v_cliente as integer.
define buffer b_items_fac for items_factura.
define buffer b_remitos for remitos.
define var v_estabien_crealo as logical initial false.

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
    
define output parameter table for tt_tambores.
/**************************** BUSCO EL STOCK ACTUAL QUE FUE CREADO ANTES DE LA FECHA ELEGIDA EN LA MISMA
                                SUCURSAL****/

for each tambores_industria no-lock where tambores_industria.id_sucursal_ubicacion = p_suc
                                      and tambores_industria.id_sucursal = p_suc
                                      and tambores_industria.id_locacion_ubicacion = 4
                                      and tambores_industria.fecha <= p_fecha
                                      and (tambores_industria.id_tipotambor = 1
                                        or tambores_industria.id_tipotambor = 3
                                        or tambores_industria.id_tipotambor = 4
                                        or tambores_industria.id_tipotambor = 5
                                        or tambores_industria.id_tipotambor = 9).

    create tt_tambores.
    assign tt_tambores.id_empresa               = tambores_industria.id_empresa
           tt_tambores.id_sucursal              = tambores_industria.id_sucursal
           tt_tambores.id_sucursal_ubicacion    = tambores_industria.id_sucursal_ubicacion   
           tt_tambores.id_lote                  = tambores_industria.id_lote
           tt_tambores.id_tipotambor            = tambores_industria.id_tipotambor
           tt_tambores.id_articulo              = tambores_industria.id_articulo
           tt_tambores.id_calidad               = tambores_industria.id_calidad
           tt_tambores.id_envase                = tambores_industria.id_envase
           tt_tambores.id_contrato_of           = tambores_industria.id_contrato_of
           tt_tambores.id_tipocontrato_of       = tambores_industria.id_tipocontrato_of
           tt_tambores.anio_of                  = tambores_industria.anio_of
           tt_tambores.item_of                  = tambores_industria.item_of
           tt_tambores.kilos_tambor             = tambores_industria.kilos_tambor
           tt_tambores.id_etiqueta              = tambores_industria.id_etiqueta
           tt_tambores.fecha                    = tambores_industria.fecha.

end.    


/*****************************************************************************************************/

/************** BUSCO LOS LOTES CREADOS ANTES DE LA FECHA Y DESPACHADOS DESPUES ***********************/

for each remitos no-lock where remitos.fecha > p_fecha
                           and remitos.fecha <= today
                           and remitos.id_sucursal = p_suc
                           and remitos.estado = true
                           .
                           
    for each items_factura of remitos no-lock.
        v_lote = integer(substring(items_factura.nro_lote,1,4)).
        v_anio_lote = integer(substring(items_factura.nro_lote,6,2)).
        v_anio_lote = v_anio_lote + 2000.
        
        if p_suc = 95 then v_suc = 96. else v_suc = 95.
        find first b_items_fac where b_items_fac.nro_lote = items_factura.nro_lote
                                 and b_items_fac.c_fecha > p_fecha 
                                 and b_items_fac.id_sucursal = v_suc no-lock no-error.
        if not available b_items_fac then do:
            
            for each tambores_industria where tambores_industria.id_tambor >= items_factura.desde_lote
                                          and tambores_industria.id_tambor <= items_factura.hasta_lote
                                          and tambores_industria.id_lote = v_lote
                                          and year(tambores_industria.fecha) = v_anio_lote
                                          and tambores_industria.id_articulo = items_factura.id_articulo
                                          and tambores_industria.fecha <= p_fecha
                                          and (tambores_industria.id_tipotambor = 1
                                            or tambores_industria.id_tipotambor = 3
                                            or tambores_industria.id_tipotambor = 4
                                            or tambores_industria.id_tipotambor = 5
                                            or tambores_industria.id_tipotambor = 9) no-lock.
                                          
            
                find tt_tambores where tt_tambores.id_etiqueta = tambores_industria.id_etiqueta no-lock no-error.
                if not available tt_tambores then
                    do:
                        create tt_tambores.
                        assign tt_tambores.id_empresa               = tambores_industria.id_empresa
                               tt_tambores.id_sucursal              = tambores_industria.id_sucursal
                               tt_tambores.id_sucursal_ubicacion    = tambores_industria.id_sucursal_ubicacion   
                               tt_tambores.id_lote                  = tambores_industria.id_lote
                               tt_tambores.id_tipotambor            = tambores_industria.id_tipotambor
                               tt_tambores.id_articulo              = tambores_industria.id_articulo
                               tt_tambores.id_calidad               = tambores_industria.id_calidad
                               tt_tambores.id_envase                = tambores_industria.id_envase
                               tt_tambores.id_contrato_of           = tambores_industria.id_contrato_of
                               tt_tambores.id_tipocontrato_of       = tambores_industria.id_tipocontrato_of
                               tt_tambores.anio_of                  = tambores_industria.anio_of
                               tt_tambores.item_of                  = tambores_industria.item_of
                               tt_tambores.kilos_tambor             = tambores_industria.kilos_tambor
                               tt_tambores.id_etiqueta              = tambores_industria.id_etiqueta
                               tt_tambores.fecha                    = tambores_industria.fecha.
                  end.
                                                                
           end.
       end.
    end.
end. 

/******************************************************************************************************/

/************** BUSCO LOS LOTES CREADOS EN LA "OTRA" SUCURSAL ANTES DE LA FECHA Y 
                DESPACHADOS HACIA MI SUCURSAL ANTES DE LA FECHA ***********************/

if p_suc = 95 then 
    do:
        v_suc = 96.
        for each remitos no-lock where remitos.fecha > date("01/01/2002")
                           and remitos.fecha <= p_fecha
                           and remitos.id_sucursal = v_suc
                           and remitos.estado = true
                           and (remitos.id_cliente = 6256 or remitos.id_cliente = 1).
                           
            {i_stock_a_fecha3.i}
        end.
    end.
else 
    do:
        v_suc = 95.
        for each remitos no-lock where remitos.fecha > date("01/01/2002")
                           and remitos.fecha <= p_fecha
                           and remitos.id_sucursal = v_suc
                           and remitos.estado = true
                           and (remitos.id_cliente = 1 or remitos.id_cliente = 59999).
                           
            {i_stock_a_fecha3.i}
        end.
    end.


 

/******************************************************************************************************/

/********************* BUSCO LOS LOTES QUE USARON COMO MATERIA PRIMA EN LOTES ****************************/
for each lotes_jugo where lotes_jugo.fecha > p_fecha
                      and lotes_jugo.id_sucursal = p_suc no-lock.
    for each tambores_industria where tambores_industria.id_empresa_destino      = lotes_jugo.id_empresa
                                  and tambores_industria.id_sucursal_destino     = lotes_jugo.id_sucursal
                                  and tambores_industria.id_tipotambor_destino   = lotes_jugo.id_tipotambor
                                  and tambores_industria.nromov_destino          = lotes_jugo.nromov
                                  and tambores_industria.fecha <= p_fecha 
                                  and (tambores_industria.id_tipotambor = 1
                                        or tambores_industria.id_tipotambor = 3
                                        or tambores_industria.id_tipotambor = 4
                                        or tambores_industria.id_tipotambor = 5
                                        or tambores_industria.id_tipotambor = 9)
                                        no-lock.
        
        find tt_tambores where tt_tambores.id_etiqueta = tambores_industria.id_etiqueta no-lock no-error.
        if not available tt_tambores then
            do:
                    create tt_tambores.
                    assign tt_tambores.id_empresa               = tambores_industria.id_empresa
                           tt_tambores.id_sucursal              = tambores_industria.id_sucursal
                           tt_tambores.id_sucursal_ubicacion    = tambores_industria.id_sucursal_ubicacion   
                           tt_tambores.id_lote                  = tambores_industria.id_lote
                           tt_tambores.id_tipotambor            = tambores_industria.id_tipotambor
                           tt_tambores.id_articulo              = tambores_industria.id_articulo
                           tt_tambores.id_calidad               = tambores_industria.id_calidad
                           tt_tambores.id_envase                = tambores_industria.id_envase
                           tt_tambores.id_contrato_of           = tambores_industria.id_contrato_of
                           tt_tambores.id_tipocontrato_of       = tambores_industria.id_tipocontrato_of
                           tt_tambores.anio_of                  = tambores_industria.anio_of
                           tt_tambores.item_of                  = tambores_industria.item_of
                           tt_tambores.kilos_tambor             = tambores_industria.kilos_tambor
                           tt_tambores.id_etiqueta              = tambores_industria.id_etiqueta
                           tt_tambores.fecha                    = tambores_industria.fecha.
            end.
    end.                                  
end.
/*****************************************************************************************************/

/********************* BUSCO LOS LOTES QUE USARON COMO MATERIA PRIMA EN PRODUCCION *********************/
for each produccion_jugo where produccion_jugo.fecha > p_fecha
                           and produccion_jugo.id_sucursal = p_suc no-lock.
    for each tambores_industria where tambores_industria.id_empresa_destino      = produccion_jugo.id_empresa
                                  and tambores_industria.id_sucursal_destino     = produccion_jugo.id_sucursal
                                  and tambores_industria.id_tipotambor_destino   = produccion_jugo.id_tipotambor
                                  and tambores_industria.nromov_destino          = produccion_jugo.nromov
                                  and tambores_industria.fecha <= p_fecha 
                                  and (tambores_industria.id_tipotambor = 1
                                        or tambores_industria.id_tipotambor = 3
                                        or tambores_industria.id_tipotambor = 4
                                        or tambores_industria.id_tipotambor = 5
                                        or tambores_industria.id_tipotambor = 9) 
                                        no-lock.
        
        find tt_tambores where tt_tambores.id_etiqueta = tambores_industria.id_etiqueta no-lock no-error.
        if not available tt_tambores then
            do:
                    create tt_tambores.
                    assign tt_tambores.id_empresa               = tambores_industria.id_empresa
                           tt_tambores.id_sucursal              = tambores_industria.id_sucursal
                           tt_tambores.id_sucursal_ubicacion    = tambores_industria.id_sucursal_ubicacion   
                           tt_tambores.id_lote                  = tambores_industria.id_lote
                           tt_tambores.id_tipotambor            = tambores_industria.id_tipotambor
                           tt_tambores.id_articulo              = tambores_industria.id_articulo
                           tt_tambores.id_calidad               = tambores_industria.id_calidad
                           tt_tambores.id_envase                = tambores_industria.id_envase
                           tt_tambores.id_contrato_of           = tambores_industria.id_contrato_of
                           tt_tambores.id_tipocontrato_of       = tambores_industria.id_tipocontrato_of
                           tt_tambores.anio_of                  = tambores_industria.anio_of
                           tt_tambores.item_of                  = tambores_industria.item_of
                           tt_tambores.kilos_tambor             = tambores_industria.kilos_tambor
                           tt_tambores.id_etiqueta              = tambores_industria.id_etiqueta
                           tt_tambores.fecha                    = tambores_industria.fecha.
            end.
    end.                                  
end.
/*****************************************************************************************************/

/********************* BUSCO LOS LOTES QUE USARON COMO MATERIA PRIMA EN CARGAS *********************/
for each cargas where cargas.fecha > p_fecha
                  and cargas.id_sucursal = p_suc no-lock.
    for each tambores_industria where tambores_industria.id_empresa_destino      = cargas.id_empresa
                                  and tambores_industria.id_sucursal_destino     = cargas.id_sucursal
                                  and tambores_industria.id_tipotambor_destino   = cargas.id_tipotambor
                                  and tambores_industria.nromov_destino          = cargas.nromov 
                                  and tambores_industria.fecha <= p_fecha
                                  and (tambores_industria.id_tipotambor = 1
                                        or tambores_industria.id_tipotambor = 3
                                        or tambores_industria.id_tipotambor = 4
                                        or tambores_industria.id_tipotambor = 5
                                        or tambores_industria.id_tipotambor = 9) no-lock.
        
        find tt_tambores where tt_tambores.id_etiqueta = tambores_industria.id_etiqueta no-lock no-error.
        if not available tt_tambores then
            do:
                    create tt_tambores.
                    assign tt_tambores.id_empresa               = tambores_industria.id_empresa
                           tt_tambores.id_sucursal              = tambores_industria.id_sucursal
                           tt_tambores.id_sucursal_ubicacion    = tambores_industria.id_sucursal_ubicacion   
                           tt_tambores.id_lote                  = tambores_industria.id_lote
                           tt_tambores.id_tipotambor            = tambores_industria.id_tipotambor
                           tt_tambores.id_articulo              = tambores_industria.id_articulo
                           tt_tambores.id_calidad               = tambores_industria.id_calidad
                           tt_tambores.id_envase                = tambores_industria.id_envase
                           tt_tambores.id_contrato_of           = tambores_industria.id_contrato_of
                           tt_tambores.id_tipocontrato_of       = tambores_industria.id_tipocontrato_of
                           tt_tambores.anio_of                  = tambores_industria.anio_of
                           tt_tambores.item_of                  = tambores_industria.item_of
                           tt_tambores.kilos_tambor             = tambores_industria.kilos_tambor
                           tt_tambores.id_etiqueta              = tambores_industria.id_etiqueta
                           tt_tambores.fecha                    = tambores_industria.fecha.
            end.
    end.                                  
end.
/*****************************************************************************************************/
