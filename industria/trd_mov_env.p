TRIGGER PROCEDURE FOR DELETE OF movimientos_envases.
    find saldos_envases where 
        saldos_envases.id_proveedor = movimientos_envases.id_proveedor_destino and
        saldos_envases.id_origen    = movimientos_envases.id_origen_destino and 
        saldos_envases.id_envase    = movimientos_envases.id_envase no-error.
    
    
    if not available saldos_envases then do:
        create saldos_envases.
        assign
            saldos_envases.id_proveedor = movimientos_envases.id_proveedor_destino
            saldos_envases.id_origen    = movimientos_envases.id_origen_destino  
            saldos_envases.id_envase    = movimientos_envases.id_envase.
    end.
    
    saldos_envases.cant_envases = saldos_envases.cant_envases - movimientos_envases.cant_envases.
        
    find saldos_envases where 
        saldos_envases.id_proveedor = movimientos_envases.id_proveedor and
        saldos_envases.id_origen    = movimientos_envases.id_origen and 
        saldos_envases.id_envase    = movimientos_envases.id_envase
        exclusive-lock no-error.
    
    
    if not available saldos_envases then do:
        create saldos_envases.
        assign
            saldos_envases.id_proveedor = movimientos_envases.id_proveedor
            saldos_envases.id_origen    = movimientos_envases.id_origen  
            saldos_envases.id_envase    = movimientos_envases.id_envase.
    end.

    saldos_envases.cant_envases = saldos_envases.cant_envases + movimientos_envases.cant_envases.

    release saldos_envases.
