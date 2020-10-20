  FIELD fecha LIKE movimiento_camara.fecha VALIDATE ~
  FIELD id_movimiento_camara LIKE movimiento_camara.id_movimiento_camara VALIDATE ~
  FIELD id_sucursal LIKE movimiento_camara.id_sucursal VALIDATE ~
  FIELD id_turno LIKE movimiento_camara.id_turno VALIDATE ~
  FIELD nro_planilla LIKE movimiento_camara.nro_planilla VALIDATE ~
  FIELD observaciones LIKE movimiento_camara.observaciones VALIDATE ~
  FIELD operador LIKE movimiento_camara.operador VALIDATE ~
  FIELD c_usuario LIKE movimiento_camara.c_usuario VALIDATE ~
  FIELD c_fecha LIKE movimiento_camara.c_fecha VALIDATE ~
  FIELD c_hora LIKE movimiento_camara.c_hora VALIDATE ~
  FIELD Sucursal AS CHARACTER FORMAT "x(20)" LABEL "Sucursal"~
  FIELD Turno AS CHARACTER FORMAT "x(20)" LABEL "Turno"
