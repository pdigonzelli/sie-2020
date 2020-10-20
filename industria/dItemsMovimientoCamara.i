  FIELD cantidad LIKE items_movimiento_camara.cantidad VALIDATE ~
  FIELD c_fecha LIKE items_movimiento_camara.c_fecha VALIDATE ~
  FIELD c_hora LIKE items_movimiento_camara.c_hora VALIDATE ~
  FIELD c_usuario LIKE items_movimiento_camara.c_usuario VALIDATE ~
  FIELD destino LIKE items_movimiento_camara.destino VALIDATE ~
  FIELD fecha LIKE items_movimiento_camara.fecha VALIDATE ~
  FIELD id_camara LIKE items_movimiento_camara.id_camara VALIDATE ~
  FIELD id_movimiento_camara LIKE items_movimiento_camara.id_movimiento_camara VALIDATE ~
  FIELD id_origen LIKE items_movimiento_camara.id_origen VALIDATE ~
  FIELD id_sucursal LIKE items_movimiento_camara.id_sucursal VALIDATE ~
  FIELD id_tipo_movimiento LIKE items_movimiento_camara.id_tipo_movimiento VALIDATE ~
  FIELD id_turno LIKE items_movimiento_camara.id_turno VALIDATE ~
  FIELD item LIKE items_movimiento_camara.item VALIDATE ~
  FIELD nro_columna LIKE items_movimiento_camara.nro_columna VALIDATE ~
  FIELD nro_columna_origen LIKE items_movimiento_camara.nro_columna_origen VALIDATE ~
  FIELD nro_fila LIKE items_movimiento_camara.nro_fila VALIDATE ~
  FIELD nro_fila_origen LIKE items_movimiento_camara.nro_fila_origen VALIDATE ~
  FIELD observaciones LIKE items_movimiento_camara.observaciones VALIDATE ~
  FIELD Origen AS CHARACTER FORMAT "x(20)" LABEL "Origen"~
  FIELD TipoMov AS CHARACTER FORMAT "x(20)" LABEL "TipoMov"~
  FIELD Camara AS CHARACTER FORMAT "x(25)" LABEL "Camara"
