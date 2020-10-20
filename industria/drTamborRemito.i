  FIELD c_fecha LIKE r_tambor_remito.c_fecha VALIDATE ~
  FIELD c_hora LIKE r_tambor_remito.c_hora VALIDATE ~
  FIELD c_usuario LIKE r_tambor_remito.c_usuario VALIDATE ~
  FIELD fecha LIKE r_tambor_remito.fecha VALIDATE ~
  FIELD id_empresa LIKE r_tambor_remito.id_empresa VALIDATE ~
  FIELD id_sucursal LIKE r_tambor_remito.id_sucursal VALIDATE ~
  FIELD id_sucursal_remito LIKE r_tambor_remito.id_sucursal_remito VALIDATE ~
  FIELD id_tambor LIKE r_tambor_remito.id_tambor VALIDATE ~
  FIELD id_tipotambor LIKE r_tambor_remito.id_tipotambor VALIDATE ~
  FIELD id_tipo_movsto LIKE r_tambor_remito.id_tipo_movsto VALIDATE ~
  FIELD item_factura LIKE r_tambor_remito.item_factura VALIDATE ~
  FIELD nromov LIKE r_tambor_remito.nromov VALIDATE ~
  FIELD nro_remito LIKE r_tambor_remito.nro_remito VALIDATE ~
  FIELD Lote AS INTEGER FORMAT ">>>9" LABEL "Lote"~
  FIELD Anio AS INTEGER FORMAT ">>>9" LABEL "Anio"~
  FIELD Kilos AS DECIMAL FORMAT "->>,>>>,>>9.99" LABEL "Kilos"~
  FIELD Ubicacion AS CHARACTER FORMAT "x(15)" LABEL "Ubicacion"
