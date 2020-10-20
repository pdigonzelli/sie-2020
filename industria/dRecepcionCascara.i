  FIELD id_suc_origen LIKE recepcion_cascara_externa.id_suc_origen VALIDATE ~
  FIELD Origen AS CHARACTER FORMAT "x(50)" LABEL "Origen"~
  FIELD id_suc_destino LIKE recepcion_cascara_externa.id_suc_destino VALIDATE ~
  FIELD Destino AS CHARACTER FORMAT "x(50)" LABEL "Destino"~
  FIELD cantidad LIKE recepcion_cascara_externa.cantidad VALIDATE ~
  FIELD kilos LIKE recepcion_cascara_externa.kilos VALIDATE ~
  FIELD Transporte AS CHARACTER FORMAT "x(50)" LABEL "Transporte"~
  FIELD id_transporte LIKE recepcion_cascara_externa.id_transporte VALIDATE ~
  FIELD nombre_chofer LIKE recepcion_cascara_externa.nombre_chofer VALIDATE ~
  FIELD nro_patente LIKE recepcion_cascara_externa.nro_patente VALIDATE ~
  FIELD observaciones LIKE recepcion_cascara_externa.observaciones VALIDATE ~
  FIELD aux_1 LIKE recepcion_cascara_externa.aux_1 VALIDATE  LABEL "Fecha"~
  FIELD aux_2 LIKE recepcion_cascara_externa.aux_2 VALIDATE ~
  FIELD c_usuario LIKE recepcion_cascara_externa.c_usuario VALIDATE ~
  FIELD c_fecha LIKE recepcion_cascara_externa.c_fecha VALIDATE ~
  FIELD c_hora LIKE recepcion_cascara_externa.c_hora VALIDATE ~
  FIELD fecha LIKE recepcion_cascara_externa.fecha VALIDATE ~
  FIELD aux_3 LIKE recepcion_cascara_externa.aux_3 VALIDATE 
