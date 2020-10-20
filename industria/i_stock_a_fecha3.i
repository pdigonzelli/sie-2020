    for each items_factura of remitos no-lock.
        v_lote = integer(substring(items_factura.nro_lote,1,4)).
        v_anio_lote = integer(substring(items_factura.nro_lote,6,2)).
        v_anio_lote = v_anio_lote + 2000.

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
                                        or tambores_industria.id_tipotambor = 9)
                                      and tambores_industria.id_locacion_ubicacion = 4
                                      and tambores_industria.id_sucursal_ubicacion = p_suc no-lock.
                                      
        
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
