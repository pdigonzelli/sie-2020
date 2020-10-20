/**************** SECTOR DE DATOS DE CONTRATOS *********************************************/
        FIND FIRST contratos where contratos.id_contrato      = tambores_industria.id_contrato_of
                               and contratos.id_tipo_contrato = tambores_industria.id_tipocontrato_of
                               and contratos.anio             = tambores_industria.anio_of no-lock no-error.
            
        v_total_tambores_of = 0.
        v_total_kilos_of = 0.
        
        FOR EACH items_contratos of contratos no-lock.
            v_total_tambores_of = v_total_tambores_of + items_contratos.cantidad.
        END.
            
        FIND FIRST clientes of contratos no-lock no-error.

        IF AVAILABLE contratos THEN DO:
            ASSIGN stock_tambores.orden_fabricacion   = string(contratos.orden_fabricacion) 
                   stock_tambores.id_contrato         = contratos.id_contrato
                   stock_tambores.anio                = contratos.anio
                   stock_tambores.id_cliente          = contratos.id_cliente
                   stock_tambores.id_tipo_contrato    = contratos.id_tipo_contrato
                   stock_tambores.cantidad_total_of   = v_total_tambores_of
                   stock_tambores.kilos_total_of      = v_total_tambores_of * tambores_industria.kilos_tambor
                   stock_tambores.anio_contrato       = integer(substring(string(year(contrato.fecha)),3,2)).

            v_kilos_stock = v_kilos_stock + (v_total_tambores_of * tambores_industria.kilos_tambor).
        
            IF AVAILABLE clientes THEN ASSIGN stock_tambores.cliente             = clientes.nombre.
            ELSE ASSIGN stock_tambores.cliente             = "SIN CLIENTE ASIGNADO".
        
        END.
