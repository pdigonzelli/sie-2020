TRIGGER PROCEDURE FOR replication-write OF tipo_interno_articulos .
find first registros_actualizados_publica where registros_actualizados_publica.host = "tucuman1" and registros_actualizados_publica.servicio = "comercsm" and registros_actualizados_publica.protocolo = "tcp" and registros_actualizados_publica.nombre = "tipo_interno_articulos" and registros_actualizados_publica.rowid_registro = string(rowid(tipo_interno_articulos)) no-lock no-error.
if not available registros_actualizados_publica Then 
do:
CREATE registros_actualizados_publica.
RAW-TRANSFER tipo_interno_articulos TO registros_actualizados_publica.registro.
ASSIGN  
   registros_actualizados_publica.host = "tucuman1"
   registros_actualizados_publica.servicio  = "comercsm"
   registros_actualizados_publica.protocolo = "tcp"
   registros_actualizados_publica.nombre  = "tipo_interno_articulos"
   registros_actualizados_publica.rowid_registro  = string(rowid(tipo_interno_articulos))
   registros_actualizados_publica.serial_de_publicacion = 0
   registros_actualizados_publica.condicion = "M"
   registros_actualizados_publica.fecha_de_actualizacion = today.
   registros_actualizados_publica.hora_de_actualizacion = string(time,"HH:MM:SS").
/***
   registros_actualizados_publica.usuario_de_actualizacion = tipo_interno_articulos.c_usuario. ***/
end. 
Else 
do: 
find current registros_actualizados_publica exclusive-lock. 
RAW-TRANSFER tipo_interno_articulos TO registros_actualizados_publica.registro.
ASSIGN  
   registros_actualizados_publica.serial_de_publicacion = 0
   registros_actualizados_publica.condicion = "M"
   registros_actualizados_publica.fecha_de_actualizacion = today
   registros_actualizados_publica.hora_de_actualizacion = string(time,"HH:MM:SS").
/***
   registros_actualizados_publica.usuario_de_actualizacion = tipo_interno_articulos.c_usuario. ***/
find current registros_actualizados_publica no-lock. 
end. 