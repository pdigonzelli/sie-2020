  FIELD anio_contrato LIKE aux_items_venta.anio_contrato VALIDATE ~
  FIELD bultos_nominales LIKE aux_items_venta.bultos_nominales VALIDATE ~
  FIELD bultos_standard LIKE aux_items_venta.bultos_standard VALIDATE ~
  FIELD cantidad_bolsas LIKE aux_items_venta.cantidad_bolsas VALIDATE ~
  FIELD c_fecha LIKE aux_items_venta.c_fecha VALIDATE ~
  FIELD c_hora LIKE aux_items_venta.c_hora VALIDATE ~
  FIELD c_usuario LIKE aux_items_venta.c_usuario VALIDATE ~
  FIELD descripcion LIKE aux_items_venta.descripcion VALIDATE ~
  FIELD descripcion-1 LIKE aux_items_venta.descripcion-1 VALIDATE ~
  FIELD descripcion-2 LIKE aux_items_venta.descripcion-2 VALIDATE ~
  FIELD desde_lote LIKE aux_items_venta.desde_lote VALIDATE ~
  FIELD hasta_lote LIKE aux_items_venta.hasta_lote VALIDATE ~
  FIELD id_contrato LIKE aux_items_venta.id_contrato VALIDATE ~
  FIELD id_punto_venta LIKE aux_items_venta.id_punto_venta VALIDATE ~
  FIELD id_tipo_contrato LIKE aux_items_venta.id_tipo_contrato VALIDATE ~
  FIELD item LIKE aux_items_venta.item VALIDATE ~
  FIELD item_contrato LIKE aux_items_venta.item_contrato VALIDATE ~
  FIELD kilos_item LIKE aux_items_venta.kilos_item VALIDATE ~
  FIELD kilos_nominales LIKE aux_items_venta.kilos_nominales VALIDATE ~
  FIELD kilos_por_bolsa LIKE aux_items_venta.kilos_por_bolsa VALIDATE ~
  FIELD kilos_standard LIKE aux_items_venta.kilos_standard VALIDATE ~
  FIELD merma LIKE aux_items_venta.merma VALIDATE ~
  FIELD nromov LIKE aux_items_venta.nromov VALIDATE ~
  FIELD nro_contenedor LIKE aux_items_venta.nro_contenedor VALIDATE ~
  FIELD nro_lote LIKE aux_items_venta.nro_lote VALIDATE ~
  FIELD nro_PO_cliente LIKE aux_items_venta.nro_PO_cliente VALIDATE ~
  FIELD nro_prod_cliente LIKE aux_items_venta.nro_prod_cliente VALIDATE ~
  FIELD nro_release LIKE aux_items_venta.nro_release VALIDATE ~
  FIELD nroFactura AS INTEGER FORMAT ">>>,>>>,>>9" LABEL "nroFactura"~
  FIELD Articulo AS CHARACTER FORMAT "x(20)" LABEL "Articulo"~
  FIELD Calidad AS CHARACTER FORMAT "x(20)" LABEL "Calidad"~
  FIELD FechaFact AS DATE FORMAT "99/99/9999" LABEL "FechaFact"~
  FIELD TipoComp AS CHARACTER FORMAT "x(8)" LABEL "TipoComp"~
  FIELD impreso AS LOGICAL FORMAT "definitiva/provisoria" LABEL "Impreso"
