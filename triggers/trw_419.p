TRIGGER PROCEDURE FOR replication-delete OF lotes_jugo .
find first registros_actualizados_publica where registros_actualizados_publica.host = "tucuman1" and registros_actualizados_publica.servicio = "industria" and registros_actualizados_publica.protocolo = "TCP" and registros_actualizados_publica.nombre = "lotes_jugo" and registros_actualizados_publica.rowid_registro = string(rowid(lotes_jugo)) no-lock no-error.
if not available registros_actualizados_publica Then 
do:
CREATE registros_actualizados_publica.
RAW-TRANSFER industria.lotes_jugo TO registros_actualizados_publica.registro.
ASSIGN  
   registros_actualizados_publica.host = "tucuman1"
   registros_actualizados_publica.servicio  = "industria"
   registros_actualizados_publica.protocolo = "TCP"
   registros_actualizados_publica.nombre  = "lotes_jugo"
   registros_actualizados_publica.rowid_registro  = string(rowid(lotes_jugo))
   registros_actualizados_publica.serial_de_publicacion = 0
   registros_actualizados_publica.condicion = "B"
   registros_actualizados_publica.fecha_de_actualizacion = today.
   registros_actualizados_publica.hora_de_actualizacion = string(time,"HH:MM:SS").
   registros_actualizados_publica.usuario_de_actualizacion = "v_tranfer".
end. 
Else 
do: 
find current registros_actualizados_publica exclusive-lock. 
RAW-TRANSFER lotes_jugo TO registros_actualizados_publica.registro.
ASSIGN  
   registros_actualizados_publica.serial_de_publicacion = 0
   registros_actualizados_publica.condicion = "B"
   registros_actualizados_publica.fecha_de_actualizacion = today
   registros_actualizados_publica.hora_de_actualizacion = string(time,"HH:MM:SS").
   registros_actualizados_publica.usuario_de_actualizacion = "v_tranfer".
find current registros_actualizados_publica no-lock. 
end. 
