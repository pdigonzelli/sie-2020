TRIGGER PROCEDURE FOR replication-delete OF items_stock .
find first registros_actualizados_publica where registros_actualizados_publica.host = "tucuman1" and registros_actualizados_publica.servicio = "stocksm" and registros_actualizados_publica.protocolo = "tcp" and registros_actualizados_publica.nombre = "items_stock" and registros_actualizados_publica.rowid_registro = string(rowid(items_stock)) no-lock no-error.
if not available registros_actualizados_publica Then 
do:
CREATE registros_actualizados_publica.
RAW-TRANSFER items_stock TO registros_actualizados_publica.registro.
ASSIGN  
   registros_actualizados_publica.host = "tucuman1"
   registros_actualizados_publica.servicio  = "stocksm"
   registros_actualizados_publica.protocolo = "tcp"
   registros_actualizados_publica.nombre  = "items_stock"
   registros_actualizados_publica.rowid_registro  = string(rowid(items_stock))
   registros_actualizados_publica.serial_de_publicacion = 0
   registros_actualizados_publica.condicion = "B"
   registros_actualizados_publica.fecha_de_actualizacion = today.
   registros_actualizados_publica.hora_de_actualizacion = string(time,"HH:MM:SS").
   registros_actualizados_publica.usuario_de_actualizacion = items_stock.c_usuario. 
end. 
Else 
do: 
find current registros_actualizados_publica exclusive-lock. 
RAW-TRANSFER items_stock TO registros_actualizados_publica.registro.
ASSIGN  
   registros_actualizados_publica.serial_de_publicacion = 0
   registros_actualizados_publica.condicion = "B"
   registros_actualizados_publica.fecha_de_actualizacion = today
   registros_actualizados_publica.hora_de_actualizacion = string(time,"HH:MM:SS").
   registros_actualizados_publica.usuario_de_actualizacion = items_stock.c_usuario. 
find current registros_actualizados_publica no-lock. 
end. 