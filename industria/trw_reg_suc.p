TRIGGER PROCEDURE FOR REPLICATION-WRITE OF transfer.registros_suscriptos.

/*************TRIGGERS WRITE de registros_suscriptos ******************/
/**************  ACTUALIZA bases locales ********************/ 

define buffer aux_tabpub for transfer.tablas_publicadas.

find first aux_tabpub where
   aux_tabpub.host = transfer.registros_suscriptos.host and
   aux_tabpub.servicio = transfer.registros_suscriptos.servicio and
   aux_tabpub.protocolo = transfer.registros_suscriptos.protocolo and
   aux_tabpub.nombre = transfer.registros_suscriptos.nombre no-lock no-error.

if available aux_tabpub Then
  do:
     run v_actualiza_base.p (input rowid(transfer.registros_suscriptos)) 
     transfer.registros_suscriptos.nombre
     aux_tabpub.clausula_where.
  end.
