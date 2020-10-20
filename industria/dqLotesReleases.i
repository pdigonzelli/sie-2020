  FIELD id_empresa LIKE items_release_delivery.id_empresa VALIDATE ~
  FIELD id_sucursal LIKE items_release_delivery.id_sucursal VALIDATE ~
  FIELD id_tipotambor LIKE items_release_delivery.id_tipotambor VALIDATE ~
  FIELD nromov LIKE items_release_delivery.nromov VALIDATE ~
  FIELD id_lote_deposito LIKE items_release_delivery.id_lote_deposito VALIDATE ~
  FIELD id_sucursal_ubicacion LIKE items_release_delivery.id_sucursal_ubicacion VALIDATE ~
  FIELD id_release_delivery LIKE items_release_delivery.id_release_delivery VALIDATE  LABEL "Cod.Release"~
  FIELD item_release_delivery LIKE items_release_delivery.item_release_delivery VALIDATE  LABEL "item"~
  FIELD numero_release LIKE release_delivery.numero_release VALIDATE ~
  FIELD tambores LIKE items_release_delivery.tambores VALIDATE ~
  FIELD nombre LIKE sucursales.nombre VALIDATE ~
  FIELD id_lote LIKE items_release_delivery.id_lote VALIDATE 
