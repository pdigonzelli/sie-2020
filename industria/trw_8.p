TRIGGER PROCEDURE FOR REPLICATION-WRITE OF provart .
/*
CREATE novedadeso.
RAW-TRANSFER provart TO novedadeso.registro.
ASSIGN  
     novedadeso.id_estado          = 3   /* modifi cacion */ 
     novedadeso.nombre_tabla       = "provart"
     novedadeso.c_hora             = provart.c_hora
     novedadeso.c_fecha            = provart.c_fecha
     novedadeso.c_usuario          = provart.c_usuario 
     novedadeso.id_transaccion     = DBTASKID(LDBNAME(BUFFER  provart)) 
     novedadeso.fecha_novedad      = TODAY
     novedadeso.hora_novedad       = STRING(TIME,"HH:MM:SS")
     novedadeso.estado_transmision = 0
     novedadeso.sucursales         = ?    
     novedadeso.segundos_novedad   = TIME
     novedadeso.secuencia_novedad  = NEXT-VALUE(next-transfer).
*/     
