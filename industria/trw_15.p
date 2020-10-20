TRIGGER PROCEDURE FOR replication-delete OF subd_gastos .
find first registros_actualizados_publica where registros_actualizados_publica.host = "BUENOS_AIRES1" and registros_actualizados_publica.servicio = "bacomercsm" and registros_actualizados_publica.protocolo = "tcp" and registros_actualizados_publica.nombre = "subd_gastos" and registros_actualizados_publica.rowid_registro = string(rowid(subd_gastos)) no-lock no-error.
if not available registros_actualizados_publica Then 
do:
CREATE registros_actualizados_publica.
RAW-TRANSFER comercial.subd_gastos TO registros_actualizados_publica.registro.
ASSIGN  
   registros_actualizados_publica.host = "BUENOS_AIRES1"
   registros_actualizados_publica.servicio  = "bacomercsm"
   registros_actualizados_publica.protocolo = "tcp"
   registros_actualizados_publica.nombre  = "subd_gastos"
   registros_actualizados_publica.rowid_registro  = string(rowid(subd_gastos))
   registros_actualizados_publica.serial_de_publicacion = 0
   registros_actualizados_publica.condicion = "B"
   registros_actualizados_publica.fecha_de_actualizacion = today.
   registros_actualizados_publica.hora_de_actualizacion = string(time,"HH:MM:SS").
   registros_actualizados_publica.usuario_de_actualizacion = "v_tranfer".
end. 
Else 
do: 
find current registros_actualizados_publica exclusive-lock. 
RAW-TRANSFER subd_gastos TO registros_actualizados_publica.registro.
ASSIGN  
   registros_actualizados_publica.serial_de_publicacion = 0
   registros_actualizados_publica.condicion = "B"
   registros_actualizados_publica.fecha_de_actualizacion = today
   registros_actualizados_publica.hora_de_actualizacion = string(time,"HH:MM:SS").
   registros_actualizados_publica.usuario_de_actualizacion = "v_tranfer".
find current registros_actualizados_publica no-lock. 
end. 