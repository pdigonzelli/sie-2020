TRIGGER PROCEDURE FOR REPLICATION-WRITE OF registros_publicados.

/*************TRIGGERS WRITE de registros_publicados ******************/
/***************CREA registros_actualizados_suscrip********************/ 

{v_devuelve_hora.i}


for each suscripciones where 
       suscripciones.host = registros_publicados.host and
       suscripciones.servicio = registros_publicados.servicio and
       suscripciones.protocolo = registros_publicados.protocolo and
       suscripciones.nombre = registros_publicados.nombre 
       no-lock :
  find first tablas_suscriptas where 
       tablas_suscriptas.host = suscripciones.host_suscriptor and
       tablas_suscriptas.servicio = suscripciones.servicio_suscriptor and
       tablas_suscriptas.protocolo = suscripciones.protocolo_suscriptor and
       tablas_suscriptas.nombre = suscripciones.nombre_suscriptor 
       no-lock no-error.
   if available tablas_suscriptas Then
      do:             
        find first registros_actualizados_suscrip where 
           registros_actualizados_suscrip.host = tablas_suscriptas.host and 
           registros_actualizados_suscrip.servicio = tablas_suscriptas.servicio and 
           registros_actualizados_suscrip.protocolo = tablas_suscriptas.protocolo and 
           registros_actualizados_suscrip.nombre = tablas_suscriptas.nombre and 
           registros_actualizados_suscrip.rowid_registro = registros_publicados.rowid_registro no-lock no-error.
        if not available registros_actualizados_suscrip Then 
           do:
             CREATE registros_actualizados_suscrip.
             BUFFER-COPY registros_publicados except
               registros_publicados.host 
               registros_publicados.servicio 
               registros_publicados.protocolo 
               registros_publicados.nombre to registros_actualizados_suscrip  .
             ASSIGN
               registros_actualizados_suscrip.host = suscripciones.host_suscriptor                                  
               registros_actualizados_suscrip.servicio = suscripciones.servicio_suscriptor 
               registros_actualizados_suscrip.protocolo = suscripciones.protocolo_suscriptor 
               registros_actualizados_suscrip.nombre = suscripciones.nombre_suscriptor 
               registros_actualizados_suscrip.serial_de_suscripcion = 0 
               registros_actualizados_suscrip.fecha_de_actualizacion = registros_publicados.fecha_de_publicacion
               registros_actualizados_suscrip.hora_de_actualizacion = registros_publicados.hora_de_publicacion
               registros_actualizados_suscrip.usuario_de_actualizacion = registros_publicados.usuario_de_publicacion. 
            end.
          Else
            do:
              find current registros_actualizados_suscrip exclusive-lock. 
               ASSIGN
                  registros_actualizados_suscrip.registro = registros_publicados.registro
                  registros_actualizados_suscrip.serial_de_suscripcion = 0
                  registros_actualizados_suscrip.condicion = registros_publicados.condicion
                  registros_actualizados_suscrip.fecha_de_actualizacion = registros_publicados.fecha_de_publicacion
                  registros_actualizados_suscrip.hora_de_actualizacion = registros_publicados.hora_de_publicacion
                  registros_actualizados_suscrip.usuario_de_actualizacion = registros_publicados.usuario_de_publicacion. 
              find current registros_actualizados_suscrip no-lock. 
            end.
      end.
end.       


