  FIELD anio LIKE movimientos_tambores.anio VALIDATE ~
  FIELD articulo AS CHARACTER FORMAT "x(25)"~
  FIELD Lote AS CHARACTER FORMAT "x(15)"~
  FIELD c_fecha LIKE movimientos_tambores.c_fecha VALIDATE ~
  FIELD c_hora LIKE movimientos_tambores.c_hora VALIDATE ~
  FIELD c_usuario LIKE movimientos_tambores.c_usuario VALIDATE ~
  FIELD datos_adicionales LIKE movimientos_tambores.datos_adicionales VALIDATE ~
  FIELD fecha LIKE movimientos_tambores.fecha VALIDATE ~
  FIELD id_articulo LIKE movimientos_tambores.id_articulo VALIDATE ~
  FIELD id_empresa LIKE movimientos_tambores.id_empresa VALIDATE ~
  FIELD id_lote LIKE movimientos_tambores.id_lote VALIDATE ~
  FIELD id_sucursal LIKE movimientos_tambores.id_sucursal VALIDATE ~
  FIELD id_suc_des LIKE movimientos_tambores.id_suc_des VALIDATE ~
  FIELD id_suc_origen LIKE movimientos_tambores.id_suc_origen VALIDATE ~
  FIELD id_tipotambor LIKE movimientos_tambores.id_tipotambor VALIDATE ~
  FIELD id_tipo_movimiento LIKE movimientos_tambores.id_tipo_movimiento VALIDATE ~
  FIELD nromov LIKE movimientos_tambores.nromov VALIDATE ~
  FIELD nromov_mov LIKE movimientos_tambores.nromov_mov VALIDATE ~
  FIELD tambor_desde LIKE movimientos_tambores.tambor_desde VALIDATE ~
  FIELD tambor_hasta LIKE movimientos_tambores.tambor_hasta VALIDATE ~
  FIELD cantidad LIKE movimientos_tambores.cantidad VALIDATE ~
  FIELD nro_movimiento LIKE movimientos_tambores.nro_movimiento VALIDATE ~
  FIELD destino AS CHARACTER FORMAT "x(40)" LABEL "Destino"~
  FIELD movimiento AS CHARACTER FORMAT "x(40)" LABEL "Movimiento"~
  FIELD origen AS CHARACTER FORMAT "x(40)" LABEL "Origen"
