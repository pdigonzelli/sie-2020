  FIELD id_lote LIKE stock_historico_tambores.id_lote VALIDATE ~
  FIELD anio LIKE stock_historico_tambores.anio VALIDATE ~
  FIELD c_fecha LIKE stock_historico_tambores.c_fecha VALIDATE ~
  FIELD c_usuario LIKE stock_historico_tambores.c_usuario VALIDATE ~
  FIELD id_suc_origen LIKE stock_historico_tambores.id_suc_origen VALIDATE ~
  FIELD id_suc_des LIKE stock_historico_tambores.id_suc_des VALIDATE ~
  FIELD tambor_desde LIKE stock_historico_tambores.tambor_desde VALIDATE ~
  FIELD tambor_hasta LIKE stock_historico_tambores.tambor_hasta VALIDATE ~
  FIELD cantidad_tambores AS CHARACTER FORMAT "x(8)" LABEL "Cantidad"~
  FIELD nromov LIKE stock_historico_tambores.nromov VALIDATE ~
  FIELD id_serial LIKE stock_historico_tambores.id_serial VALIDATE ~
  FIELD id_tipo_movimiento LIKE stock_historico_tambores.id_tipo_movimiento VALIDATE ~
  FIELD id_articulo LIKE stock_historico_tambores.id_articulo VALIDATE ~
  FIELD fecha LIKE stock_historico_tambores.fecha VALIDATE ~
  FIELD descripcion LIKE tipos_movimientos.descripcion VALIDATE 
