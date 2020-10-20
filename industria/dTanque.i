  FIELD abreviatura LIKE tanque.abreviatura VALIDATE ~
  FIELD capacidad LIKE tanque.capacidad VALIDATE ~
  FIELD c_fecha LIKE tanque.c_fecha VALIDATE ~
  FIELD c_hora LIKE tanque.c_hora VALIDATE ~
  FIELD c_usuario LIKE tanque.c_usuario VALIDATE ~
  FIELD descripcion LIKE tanque.descripcion VALIDATE  FORMAT "X(30)"~
  FIELD id_sucursal LIKE tanque.id_sucursal VALIDATE ~
  FIELD id_tanque LIKE tanque.id_tanque VALIDATE ~
  FIELD observaciones LIKE tanque.observaciones VALIDATE ~
  FIELD volumen LIKE tanque.volumen VALIDATE ~
  FIELD Tanque AS CHARACTER FORMAT "x(25)" LABEL "Tanque"
