  FIELD id_envio LIKE contenedores.id_envio VALIDATE ~
  FIELD nro_contenedor LIKE contenedores.nro_contenedor VALIDATE  FORMAT "X(20)"~
  FIELD cont AS CHARACTER FORMAT "x(20)"~
  FIELD fecha_entrega LIKE contenedores.fecha_entrega VALIDATE ~
  FIELD fecha_recepcion_cliente LIKE contenedores.fecha_recepcion_cliente VALIDATE ~
  FIELD fecha_pickup LIKE contenedores.fecha_pickup VALIDATE ~
  FIELD observaciones LIKE contenedores.observaciones VALIDATE 
