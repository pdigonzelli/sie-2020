TRIGGER PROCEDURE FOR replication-write OF destinos_grupo .
find first registros_actualizados_publica where registros_actualizados_publica.host = "BUENOS_AIRES1" and registros_actualizados_publica.servicio = "bacomercsm" and registros_actualizados_publica.protocolo = "tcp" and registros_actualizados_publica.nombre = "destinos_grupo" and registros_actualizados_publica.rowid_registro = string(rowid(destinos_grupo)) no-lock no-error.
if not available registros_actualizados_publica Then 
do:
CREATE registros_actualizados_publica.
RAW-TRANSFER comercial.destinos_grupo TO registros_actualizados_publica.registro.
ASSIGN  
   registros_actualizados_publica.host = "BUENOS_AIRES1"
   registros_actualizados_publica.servicio  = "bacomercsm"
   registros_actualizados_publica.protocolo = "tcp"
   registros_actualizados_publica.nombre  = "destinos_grupo"
   registros_actualizados_publica.rowid_registro  = string(rowid(destinos_grupo))
   registros_actualizados_publica.serial_de_publicacion = 0
   registros_actualizados_publica.condicion = "M"
   registros_actualizados_publica.fecha_de_actualizacion = today.
   registros_actualizados_publica.hora_de_actualizacion = string(time,"HH:MM:SS").
   registros_actualizados_publica.usuario_de_actualizacion = "v_tranfer".
end. 
Else 
do: 
find current registros_actualizados_publica exclusive-lock. 
RAW-TRANSFER destinos_grupo TO registros_actualizados_publica.registro.
ASSIGN  
   registros_actualizados_publica.serial_de_publicacion = 0
   registros_actualizados_publica.condicion = "M"
   registros_actualizados_publica.fecha_de_actualizacion = today
   registros_actualizados_publica.hora_de_actualizacion = string(time,"HH:MM:SS").
   registros_actualizados_publica.usuario_de_actualizacion = "v_tranfer".
find current registros_actualizados_publica no-lock. 
end. 