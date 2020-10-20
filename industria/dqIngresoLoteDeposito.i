  FIELD id_empresa LIKE item_ingreso_lote_ubicacion.id_empresa VALIDATE ~
  FIELD Calidad AS CHARACTER FORMAT "x(25)" LABEL "Calidad"~
  FIELD Lote AS CHARACTER FORMAT "x(8)" LABEL "Lote"~
  FIELD Saldo AS INTEGER FORMAT ">>>>99" LABEL "Saldo"~
  FIELD id_sucursal LIKE item_ingreso_lote_ubicacion.id_sucursal VALIDATE ~
  FIELD id_tipotambor LIKE item_ingreso_lote_ubicacion.id_tipotambor VALIDATE ~
  FIELD nromov LIKE item_ingreso_lote_ubicacion.nromov VALIDATE ~
  FIELD id_sucursal_ubicacion LIKE item_ingreso_lote_ubicacion.id_sucursal_ubicacion VALIDATE ~
  FIELD id_lote_deposito LIKE item_ingreso_lote_ubicacion.id_lote_deposito VALIDATE ~
  FIELD cantidad LIKE item_ingreso_lote_ubicacion.cantidad VALIDATE ~
  FIELD nromov_ingreso LIKE item_ingreso_lote_ubicacion.nromov_ingreso VALIDATE ~
  FIELD nro_ingreso LIKE item_ingreso_lote_ubicacion.nro_ingreso VALIDATE ~
  FIELD id_envio LIKE item_ingreso_lote_ubicacion.id_envio VALIDATE 
