  FIELD id_empresa LIKE r_lote_cascara_remito.id_empresa VALIDATE ~
  FIELD FechaRemito AS DATE FORMAT "99/99/9999" LABEL "FechaRemitos"~
  FIELD Sucursal AS CHARACTER FORMAT "x(30)" LABEL "Sucursal"~
  FIELD LugDesc AS CHARACTER FORMAT "x(30)" LABEL "LugDesc"~
  FIELD SucDestino AS CHARACTER FORMAT "x(30)" LABEL "SucDestino"~
  FIELD Cantidad AS INTEGER FORMAT ">>>>>" LABEL "Cantidad"~
  FIELD NroComp AS CHARACTER FORMAT "XXXX-XXXXXXXX" LABEL "NroComp"~
  FIELD id_sucursal LIKE r_lote_cascara_remito.id_sucursal VALIDATE ~
  FIELD id_tipotambor LIKE r_lote_cascara_remito.id_tipotambor VALIDATE ~
  FIELD nromov LIKE r_lote_cascara_remito.nromov VALIDATE ~
  FIELD id_sucursal_remito LIKE r_lote_cascara_remito.id_sucursal_remito VALIDATE ~
  FIELD id_tipo_movsto LIKE r_lote_cascara_remito.id_tipo_movsto VALIDATE ~
  FIELD nro_remito LIKE r_lote_cascara_remito.nro_remito VALIDATE ~
  FIELD item_factura LIKE r_lote_cascara_remito.item_factura VALIDATE 
