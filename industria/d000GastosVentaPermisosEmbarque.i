  FIELD anio LIKE r_gastos_permiso_embarque.anio VALIDATE ~
  FIELD c_fecha LIKE r_gastos_permiso_embarque.c_fecha VALIDATE ~
  FIELD c_hora LIKE r_gastos_permiso_embarque.c_hora VALIDATE ~
  FIELD c_usuario LIKE r_gastos_permiso_embarque.c_usuario VALIDATE ~
  FIELD fecha LIKE r_gastos_permiso_embarque.fecha VALIDATE ~
  FIELD id_aduana LIKE r_gastos_permiso_embarque.id_aduana VALIDATE ~
  FIELD id_gasto LIKE r_gastos_permiso_embarque.id_gasto VALIDATE ~
  FIELD id_permiso_embarque LIKE r_gastos_permiso_embarque.id_permiso_embarque VALIDATE ~
  FIELD importe LIKE r_gastos_permiso_embarque.importe VALIDATE  FORMAT "->>>>>9.99"~
  FIELD importe_old LIKE r_gastos_permiso_embarque.importe_old VALIDATE ~
  FIELD abreviatura LIKE gastos_venta.abreviatura VALIDATE ~
  FIELD descripcion LIKE gastos_venta.descripcion VALIDATE ~
  FIELD id_clausula LIKE gastos_venta.id_clausula VALIDATE ~
  FIELD id_gasto-gastos_venta LIKE gastos_venta.id_gasto VALIDATE ~
  FIELD nombre_programa LIKE gastos_venta.nombre_programa VALIDATE 
