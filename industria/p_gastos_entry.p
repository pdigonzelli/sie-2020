define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
define input parameter p_id_gasto like gastos_venta.id_gasto.
define var p_entry as decimal.
define var v_galones as decimal no-undo.

find orden_entrega where orden_entrega.id_orden_entrega = p_id_orden_entrega no-lock no-error.

if available orden_entrega then
    do:
        
        if orden_entrega.id_articulo = 52 or
           orden_entrega.id_articulo = 53 then
           do: 
            v_galones = orden_entrega.total_galones.
            find destinos where destinos.id_destino = orden_entrega.id_destino no-lock no-error.
            if available destinos then
             do:
                if destinos.id_destino_grupo = 25 then
                    do:
                    find first tambores_industria where tambores_industria.id_orden_entrega = orden_entrega.id_orden_entrega        
                                                  no-lock no-error.
            
                         if available tambores_industria then
                           do:
                              find last inspecciones_lote where inspecciones_lote.id_empresa = tambores_industria.id_empresa
                                                            and inspecciones_lote.id_sucursal = tambores_industria.id_sucursal
                                                            and inspecciones_lote.id_tipotambor = tambores_industria.id_tipotambor
                                                            and inspecciones_lote.nromov = tambores_industria.nromov
                                                            no-lock no-error.
            
                              if available inspecciones_lote then
                                do:
                                     find tabla_entry where tabla_entry.Brix_desde <= orden_entrega.grados_brix
                                                        and tabla_entry.Brix_hasta >= orden_entrega.grados_brix
                                                        and tabla_entry.id_articulo = orden_entrega.id_articulo
                                                        no-lock no-error.
            
                                     if available tabla_entry then
                                        do:
                                             p_entry = p_entry + v_galones * 3.785.
                                             p_entry = p_entry * tabla_entry.grados.
                                             p_entry = p_entry * 0.079.
            
                                            create gastos_orden_entrega.
                                            assign gastos_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
                                                   gastos_orden_entrega.id_gasto            = p_id_gasto
                                                   gastos_orden_entrega.importe             = p_entry.
            
                                        end.
                                     else
                                        do:
                                            message "No se encontro valores cargados para los brix " orden_entrega.grados_brix
                                                                    view-as alert-box.
                                            return.
                                        end.
                                     
                                end.
                           end.
                        end.
                   end.
              end.
        /*      
        if orden_entrega.id_articulo = 71 then
            do:
                p_entry = orden_entrega.total_factura * 0.068.
    
                create gastos_orden_entrega.
                assign gastos_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
                       gastos_orden_entrega.id_gasto            = p_id_gasto
                       gastos_orden_entrega.importe             = p_entry.

            end.
        */    
        if orden_entrega.id_articulo = 51 or
           orden_entrega.id_articulo = 57 or
           orden_entrega.id_articulo = 58 then
            do:
                p_entry = orden_entrega.total_factura * 0.038.
    
                create gastos_orden_entrega.
                assign gastos_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
                       gastos_orden_entrega.id_gasto            = p_id_gasto
                       gastos_orden_entrega.importe             = p_entry.
            end.    
    end.
