TRIGGER PROCEDURE FOR replication-write OF control_tareas .
find first registros_actualizados_publica where registros_actualizados_publica.host = "tucuman1" and registros_actualizados_publica.servicio = "agricosm" and registros_actualizados_publica.protocolo = "TCP" and registros_actualizados_publica.nombre = "control_tareas" and registros_actualizados_publica.rowid_registro = string(rowid(control_tareas)) no-lock no-error.
if not available registros_actualizados_publica Then 
do:
CREATE registros_actualizados_publica.
RAW-TRANSFER agricola.control_tareas TO registros_actualizados_publica.registro.
ASSIGN  
   registros_actualizados_publica.host = "tucuman1"
   registros_actualizados_publica.servicio  = "agricosm"
   registros_actualizados_publica.protocolo = "TCP"
   registros_actualizados_publica.nombre  = "control_tareas"
   registros_actualizados_publica.rowid_registro  = string(rowid(control_tareas))
   registros_actualizados_publica.serial_de_publicacion = 0
   registros_actualizados_publica.condicion = "M"
   registros_actualizados_publica.fecha_de_actualizacion = today.
   registros_actualizados_publica.hora_de_actualizacion = string(time,"HH:MM:SS").
   registros_actualizados_publica.usuario_de_actualizacion = "v_tranfer".
end. 
Else 
do: 
find current registros_actualizados_publica exclusive-lock. 
RAW-TRANSFER control_tareas TO registros_actualizados_publica.registro.
ASSIGN  
   registros_actualizados_publica.serial_de_publicacion = 0
   registros_actualizados_publica.condicion = "M"
   registros_actualizados_publica.fecha_de_actualizacion = today
   registros_actualizados_publica.hora_de_actualizacion = string(time,"HH:MM:SS").
   registros_actualizados_publica.usuario_de_actualizacion = "v_tranfer".
find current registros_actualizados_publica no-lock. 
end. 