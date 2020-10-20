  FIELD cantidad_transportes LIKE subd_despachos_industria.cantidad_transportes VALIDATE ~
  FIELD c_fecha LIKE subd_despachos_industria.c_fecha VALIDATE ~
  FIELD c_hora LIKE subd_despachos_industria.c_hora VALIDATE ~
  FIELD c_usuario LIKE subd_despachos_industria.c_usuario VALIDATE ~
  FIELD fecha_arribo LIKE subd_despachos_industria.fecha_arribo VALIDATE ~
  FIELD fecha_despacho LIKE subd_despachos_industria.fecha_despacho VALIDATE ~
  FIELD id_envase LIKE subd_despachos_industria.id_envase VALIDATE ~
  FIELD id_estado LIKE subd_despachos_industria.id_estado VALIDATE ~
  FIELD id_naviera LIKE subd_despachos_industria.id_naviera VALIDATE ~
  FIELD id_orden_entrega LIKE subd_despachos_industria.id_orden_entrega VALIDATE ~
  FIELD id_proveedor LIKE subd_despachos_industria.id_proveedor VALIDATE ~
  FIELD id_subd_despachos LIKE subd_despachos_industria.id_subd_despachos VALIDATE ~
  FIELD id_sucursal_remito LIKE subd_despachos_industria.id_sucursal_remito VALIDATE ~
  FIELD id_tipo_movsto LIKE subd_despachos_industria.id_tipo_movsto VALIDATE ~
  FIELD id_tipo_trasporte LIKE subd_despachos_industria.id_tipo_trasporte VALIDATE ~
  FIELD item_factura LIKE subd_despachos_industria.item_factura VALIDATE ~
  FIELD item_oe LIKE subd_despachos_industria.item_oe VALIDATE ~
  FIELD nromov LIKE subd_despachos_industria.nromov VALIDATE ~
  FIELD nro_despacho LIKE subd_despachos_industria.nro_despacho VALIDATE ~
  FIELD nro_remito LIKE subd_despachos_industria.nro_remito VALIDATE ~
  FIELD semana LIKE subd_despachos_industria.semana VALIDATE ~
  FIELD tara LIKE subd_despachos_industria.tara VALIDATE ~
  FIELD Cliente AS CHARACTER FORMAT "x(30)" LABEL "Cliente"~
  FIELD anio_semana LIKE subd_despachos_industria.anio_semana VALIDATE ~
  FIELD Producto AS CHARACTER FORMAT "x(20)" LABEL "Producto"~
  FIELD Lote AS CHARACTER FORMAT "x(15)" LABEL "Lote"~
  FIELD Naviera AS CHARACTER FORMAT "x(25)" LABEL "Naviera"~
  FIELD TipoTrans AS CHARACTER FORMAT "x(25)" LABEL "TipoTrans"~
  FIELD OrdenFab AS INTEGER FORMAT ">>>>>9" LABEL "OrdenFab"~
  FIELD Envase AS CHARACTER FORMAT "x(20)" LABEL "Envase"~
  FIELD Tambores AS INTEGER FORMAT ">>>9" LABEL "Tambores"
