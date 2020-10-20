TRIGGER PROCEDURE FOR REPLICATION-WRITE OF items_stock .
/*
CREATE novedadeso.
RAW-TRANSFER items_stock TO novedadeso.registro.
ASSIGN  
     novedadeso.id_estado          = 3   /* modifi cacion */ 
     novedadeso.nombre_tabla       = "items_stock"
     novedadeso.c_hora             = items_stock.c_hora
     novedadeso.c_fecha            = items_stock.c_fecha
     novedadeso.c_usuario          = items_stock.c_usuario 
     novedadeso.id_transaccion     = DBTASKID(LDBNAME(BUFFER  items_stock)) 
     novedadeso.fecha_novedad      = TODAY
     novedadeso.hora_novedad       = STRING(TIME,"HH:MM:SS")
     novedadeso.estado_transmision = 0
     novedadeso.sucursales         = ?    
     novedadeso.segundos_novedad   = TIME
     novedadeso.secuencia_novedad  = NEXT-VALUE(next-transfer).
*/     
