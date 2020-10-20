  FIELD id_envio LIKE contenedores.id_envio VALIDATE ~
  FIELD nro_contenedor LIKE contenedores.nro_contenedor VALIDATE ~
  FIELD observaciones LIKE contenedores.observaciones VALIDATE ~
  FIELD nro_lote LIKE items_packing_list.nro_lote VALIDATE ~
  FIELD fecha_entrega LIKE contenedores.fecha_entrega VALIDATE ~
  FIELD fecha_recepcion_cliente LIKE contenedores.fecha_recepcion_cliente VALIDATE  FORMAT "99/99/9999"
