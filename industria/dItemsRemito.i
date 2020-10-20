  FIELD id_sucursal LIKE items_factura.id_sucursal VALIDATE ~
  FIELD id_tipo_movsto LIKE items_factura.id_tipo_movsto VALIDATE ~
  FIELD nro LIKE items_factura.nro VALIDATE  FORMAT ">>>,>>>,>>9"~
  FIELD item LIKE items_factura.item VALIDATE ~
  FIELD nro_lote LIKE items_factura.nro_lote VALIDATE  FORMAT "x(15)"~
  FIELD desde_lote LIKE items_factura.desde_lote VALIDATE ~
  FIELD hasta_lote LIKE items_factura.hasta_lote VALIDATE ~
  FIELD kilos LIKE items_factura.kilos VALIDATE ~
  FIELD id_articulo LIKE items_factura.id_articulo VALIDATE ~
  FIELD id_calidad LIKE items_factura.id_calidad VALIDATE ~
  FIELD id_envase LIKE items_factura.id_envase VALIDATE ~
  FIELD id_tipotambor LIKE items_factura.id_tipotambor VALIDATE ~
  FIELD Articulo AS CHARACTER FORMAT "x(20)" LABEL "Articulo"~
  FIELD c_fecha LIKE items_factura.c_fecha VALIDATE ~
  FIELD c_hora LIKE items_factura.c_hora VALIDATE ~
  FIELD c_usuario LIKE items_factura.c_usuario VALIDATE ~
  FIELD cantidad LIKE items_factura.cantidad VALIDATE ~
  FIELD Calidad AS CHARACTER FORMAT "x(20)" LABEL "Calidad"~
  FIELD peso LIKE items_factura.peso VALIDATE ~
  FIELD descripcion LIKE items_factura.descripcion VALIDATE ~
  FIELD Envase AS CHARACTER FORMAT "x(20)" LABEL "Envase"~
  FIELD TipoTambor AS CHARACTER FORMAT "x(15)" LABEL "TipoTambor"
