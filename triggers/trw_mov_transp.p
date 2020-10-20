TRIGGER PROCEDURE FOR WRITE OF items_mov_transp.
/*
    find movimientos_envases where 
        movimientos_envases.nromov  = items_mov_transp.nromov and
        movimientos_envases.item    = items_mov_transp.item
        exclusive-lock no-error.

    if not available movimientos_envases then
        create movimientos_envases.

    assign
        movimientos_envases.nromov                = items_mov_transp.nromov
        movimientos_envases.item                  = items_mov_transp.item
        movimientos_envases.id_proveedor          = items_mov_transp.id_proveedor
        movimientos_envases.id_origen             = items_mov_transp.id_origen
        movimientos_envases.id_proveedor_destino  = items_mov_transp.id_proveedor_destino
        movimientos_envases.id_origen_destino     = items_mov_transp.id_origen_destino  
        movimientos_envases.id_envase             = items_mov_transp.id_envase
        movimientos_envases.cant_envases          = items_mov_transp.cant_envases
        movimientos_envases.fecha_operativa       = items_mov_transp.fecha_operativa
        movimientos_envases.fecha_entrada         = items_mov_transp.fecha_entrada
        movimientos_envases.hora_entrada          = items_mov_transp.hora_entrada
        movimientos_envases.fecha_salida          = items_mov_transp.fecha_salida
        movimientos_envases.hora_salida           = items_mov_transp.hora_salida.

    release movimientos_envases.
*/
