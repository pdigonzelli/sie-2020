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

FIND FIRST sucursales WHERE sucursales.id_sucursal = p_suc no-lock no-error.

/********* LOTES DE ACEITE*********************************************************************************/

/******************** TAMBORES DE LOTE ACEITE CON OF **********************************/
FOR EACH tambores_industria NO-LOCK WHERE tambores_industria.id_sucursal_ubicacion  = p_suc
                                      AND tambores_industria.id_locacion_ubicacion  = 4
                                      AND (tambores_industria.id_tipotambor = 6 OR
                                           tambores_industria.id_tipotambor = 7 )
                                      AND tambores_industria.id_articulo            = p_articulo
                                      AND tambores_industria.id_contrato_of         <> ""
                                      AND tambores_industria.id_tipocontrato_of     <> 0
                                      AND tambores_industria.anio_of                <> 0,
                                    FIRST lotes_aceite of tambores_industria where lotes_aceite.estado_lote >= 2
                                    BREAK BY tambores_industria.id_contrato_of
                                          BY tambores_industria.id_lote
                                          BY tambores_industria.anio.

    ACCUMULATE tambores_industria.id_lote (count by tambores_industria.id_lote
                                                 by tambores_industria.anio) .
    v_tambores = v_tambores + 1.
    ACCUMULATE tambores_industria.kilos_tambor (total by tambores_industria.id_lote
                                                      by tambores_industria.anio).
    
    IF LAST-OF(tambores_industria.anio) THEN DO:
        FIND FIRST estados_lotes where estados_lotes.id_estado_lote = lotes_aceite.estado_lote no-lock no-error.      
        {i_calculo_lotes_jugo.i}
        
        /****************************** SECTOR DE DATOS DE ACEITE **************************************/            
        FIND FIRST contratos WHERE contratos.id_contrato      = tambores_industria.id_contrato_of
                               AND contratos.id_tipo_contrato = tambores_industria.id_tipocontrato_of
                               AND contratos.anio             = tambores_industria.anio_of no-lock no-error.
            
        IF AVAILABLE contratos THEN DO:
            v_total_tambores_of = 0.
            v_total_kilos_of = 0.
            FOR EACH items_contratos of contratos no-lock.
                v_total_tambores_of = v_total_tambores_of + items_contratos.cantidad.
            END.
            
            FIND FIRST clientes OF contratos no-lock no-error.
        
            ASSIGN stock_tambores.orden_fabricacion   = string(contratos.orden_fabricacion) 
                   stock_tambores.id_contrato         = contratos.id_contrato
                   stock_tambores.anio                = contratos.anio
                   stock_tambores.id_cliente          = contratos.id_cliente
                   stock_tambores.id_tipo_contrato    = contratos.id_tipo_contrato
                   stock_tambores.cantidad_total_of   = v_total_tambores_of
                   stock_tambores.kilos_total_of      = v_total_tambores_of * tambores_industria.kilos_tambor
                   stock_tambores.anio_contrato       = integer(substring(string(year(contrato.fecha)),3,2))
                   stock_tambores.cliente             = IF AVAILABLE clientes THEN clientes.nombre 
                                                                              ELSE "SIN CLIENTE ASIGNADO".
        
        END.
        /***********************************************************************************************/       
        v_tambores = 0.
    END.
END.    

/******************** TAMBORES DE LOTE ACEITE SIN OF **********************************/
FOR EACH tambores_industria NO-LOCK WHERE tambores_industria.id_sucursal_ubicacion  = p_suc
                                      AND tambores_industria.id_locacion_ubicacion  = 4
                                      AND (tambores_industria.id_tipotambor = 6 OR
                                           tambores_industria.id_tipotambor = 7 )
                                      AND tambores_industria.id_articulo            = p_articulo 
                                      AND tambores_industria.id_contrato_of         = ""
                                      AND tambores_industria.id_tipocontrato_of     = 0
                                      AND tambores_industria.anio_of                = 0,
                                    FIRST lotes_aceite of tambores_industria where lotes_aceite.estado_lote >= 2
                                    BREAK BY tambores_industria.id_lote
                                          BY tambores_industria.anio.

    ACCUMULATE tambores_industria.id_lote (count by tambores_industria.id_lote
                                                 by tambores_industria.anio) .
    v_tambores = v_tambores + 1.
    ACCUMULATE tambores_industria.kilos_tambor (total by tambores_industria.id_lote
                                                      by tambores_industria.anio).
        
    IF LAST-OF(tambores_industria.anio) THEN DO:
        FIND FIRST estados_lotes where estados_lotes.id_estado_lote = lotes_aceite.estado_lote no-lock no-error.      
        {i_calculo_lotes_jugo.i}
        v_tambores = 0.
    END.
END.    