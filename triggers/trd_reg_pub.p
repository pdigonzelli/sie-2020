TRIGGER PROCEDURE FOR REPLICATION-DELETE OF registros_publicados.

/*************TRIGGERS DELETE de registros_publicados ******************/
/***************BORRA registros_actualizados_suscrip********************/ 

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
        if available registros_actualizados_suscrip Then 
           DELETE registros_actualizados_suscrip.
      end.
end.       


