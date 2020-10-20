  FIELD id_articulo_insumo LIKE insumos_lote.id_articulo_insumo VALIDATE ~
  FIELD nro_lote_proveedor LIKE insumos_lote.nro_lote_proveedor VALIDATE ~
  FIELD id_empresa LIKE insumos_lote.id_empresa VALIDATE ~
  FIELD id_sucursal LIKE insumos_lote.id_sucursal VALIDATE ~
  FIELD id_tipotambor LIKE insumos_lote.id_tipotambor VALIDATE ~
  FIELD nromov LIKE insumos_lote.nromov VALIDATE ~
  FIELD Insumo AS CHARACTER FORMAT "x(30)" LABEL "Insumo"~
  FIELD c_fecha LIKE insumos_lote.c_fecha VALIDATE ~
  FIELD c_hora LIKE insumos_lote.c_hora VALIDATE ~
  FIELD c_usuario LIKE insumos_lote.c_usuario VALIDATE ~
  FIELD id_envase LIKE insumos_lote.id_envase VALIDATE ~
  FIELD cantidad LIKE insumos_lote.cantidad VALIDATE 
