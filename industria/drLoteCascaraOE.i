  FIELD id_empresa LIKE r_lote_cascara_oe.id_empresa VALIDATE ~
  FIELD id_sucursal LIKE r_lote_cascara_oe.id_sucursal VALIDATE ~
  FIELD id_tipotambor LIKE r_lote_cascara_oe.id_tipotambor VALIDATE ~
  FIELD nromov LIKE r_lote_cascara_oe.nromov VALIDATE ~
  FIELD id_orden_entrega LIKE r_lote_cascara_oe.id_orden_entrega VALIDATE ~
  FIELD item_oe LIKE r_lote_cascara_oe.id_lote VALIDATE  LABEL "item_oe"~
  FIELD cantidad LIKE r_lote_cascara_oe.cantidad VALIDATE ~
  FIELD Lote AS INTEGER FORMAT ">>>>9" LABEL "Lote"~
  FIELD Anio AS INTEGER FORMAT ">>>9" LABEL "Anio"~
  FIELD LoteCliente AS INTEGER FORMAT ">>>>>9" LABEL "LoteCliente"
