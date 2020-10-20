  FIELD cantidad LIKE items_release_delivery.cantidad VALIDATE ~
  FIELD Calidad AS CHARACTER FORMAT "x(15)"~
  FIELD contenedor LIKE items_release_delivery.contenedor VALIDATE ~
  FIELD id_envase LIKE items_release_delivery.id_envase VALIDATE ~
  FIELD id_lote LIKE items_release_delivery.id_lote VALIDATE ~
  FIELD id_lote_deposito LIKE items_release_delivery.id_lote_deposito VALIDATE  FORMAT "x(40)"~
  FIELD id_release_delivery LIKE items_release_delivery.id_release_delivery VALIDATE ~
  FIELD id_sucursal LIKE items_release_delivery.id_sucursal VALIDATE ~
  FIELD id_unidad_medida LIKE items_release_delivery.id_unidad_medida VALIDATE ~
  FIELD id_vapor LIKE items_release_delivery.id_vapor VALIDATE ~
  FIELD item LIKE items_release_delivery.item_release_delivery VALIDATE  LABEL "item"~
  FIELD tambores LIKE items_release_delivery.tambores VALIDATE ~
  FIELD id_sucursal_ubicacion LIKE items_release_delivery.id_sucursal_ubicacion VALIDATE ~
  FIELD id_empresa LIKE items_release_delivery.id_empresa VALIDATE ~
  FIELD id_tipotambor LIKE items_release_delivery.id_tipotambor VALIDATE ~
  FIELD nromov LIKE items_release_delivery.nromov VALIDATE ~
  FIELD Vapor LIKE vapores.descripcion VALIDATE  LABEL "Vapor"~
  FIELD id_contrato LIKE items_release_delivery.id_contrato VALIDATE ~
  FIELD Peso AS DECIMAL FORMAT "zzz,zz9.99" LABEL "Peso"~
  FIELD anio_contrato LIKE items_release_delivery.anio_contrato VALIDATE ~
  FIELD id_tipo_contrato LIKE items_release_delivery.id_tipo_contrato VALIDATE ~
  FIELD item_contrato LIKE items_release_delivery.item_contrato VALIDATE ~
  FIELD c_usuario LIKE items_release_delivery.c_usuario VALIDATE ~
  FIELD c_fecha LIKE items_release_delivery.c_fecha VALIDATE ~
  FIELD Envase AS CHARACTER FORMAT "x(20)"~
  FIELD c_hora LIKE items_release_delivery.c_hora VALIDATE ~
  FIELD Galones AS DECIMAL FORMAT "zzz,zz9.99" LABEL "Galones"
