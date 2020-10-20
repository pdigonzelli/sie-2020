TRIGGER PROCEDURE FOR replication-write OF cant_pedidos .
/*
CREATE novedadeso.
RAW-TRANSFER cant_pedidos TO novedadeso.registro.
ASSIGN  
     novedadeso.id_estado          = 3   /* modifi cacion */ 
     novedadeso.nombre_tabla       = "cant_pedidos"
     novedadeso.c_hora             = cant_pedidos.c_hora
     novedadeso.c_fecha            = cant_pedidos.c_fecha
     novedadeso.c_usuario          = cant_pedidos.c_usuario 
     novedadeso.id_transaccion     = DBTASKID(LDBNAME(BUFFER  cant_pedidos)) 
     novedadeso.fecha_novedad      = TODAY
     novedadeso.hora_novedad       = STRING(TIME,"HH:MM:SS")
     novedadeso.estado_transmision = 0
     novedadeso.sucursales         = ?    
     novedadeso.segundos_novedad   = TIME
     novedadeso.secuencia_novedad  = NEXT-VALUE(next-transfer).
     
*/     
