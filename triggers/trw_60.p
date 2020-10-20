TRIGGER PROCEDURE FOR REPLICATION-WRITE OF recepciones.
/*
CREATE novedadeso.
RAW-TRANSFER recepciones TO novedadeso.registro.
ASSIGN  
     novedadeso.id_estado          = 3   /* modifi cacion */ 
     novedadeso.nombre_tabla       = "recepciones"
     novedadeso.c_hora             = recepciones.c_hora
     novedadeso.c_fecha            = recepciones.c_fecha
     novedadeso.c_usuario          = recepciones.c_usuario 
     novedadeso.id_transaccion     = DBTASKID(LDBNAME(BUFFER  recepciones)) 
     novedadeso.fecha_novedad      = TODAY
     novedadeso.hora_novedad       = STRING(TIME,"HH:MM:SS")
     novedadeso.estado_transmision = 0
     novedadeso.sucursales         = ?    
     novedadeso.segundos_novedad   = TIME
     novedadeso.secuencia_novedad  = NEXT-VALUE(next-transfer).
*/
