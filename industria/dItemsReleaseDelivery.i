  FIELD anio_contrato LIKE items_release_delivery.anio_contrato VALIDATE ~
  FIELD UsuarioConf AS CHARACTER FORMAT "x(25)" LABEL "UsuarioConf"~
  FIELD cantidad LIKE items_release_delivery.cantidad VALIDATE ~
  FIELD FechaConfirmacion AS DATE FORMAT "99/99/9999" LABEL "FechaConfirmacion"~
  FIELD contenedor LIKE items_release_delivery.contenedor VALIDATE ~
  FIELD NroRelease AS INTEGER FORMAT "zzzzz999" LABEL "NroRelease"~
  FIELD c_fecha LIKE items_release_delivery.c_fecha VALIDATE ~
  FIELD Calidad AS CHARACTER FORMAT "x(15)" LABEL "Calidad"~
  FIELD c_hora LIKE items_release_delivery.c_hora VALIDATE ~
  FIELD Envase AS CHARACTER FORMAT "x(15)" LABEL "Envase"~
  FIELD c_usuario LIKE items_release_delivery.c_usuario VALIDATE ~
  FIELD Peso AS DECIMAL FORMAT "zzzzz9,99" LABEL "Peso"~
  FIELD id_contrato LIKE items_release_delivery.id_contrato VALIDATE ~
  FIELD Galones AS DECIMAL FORMAT "zzzzz9,99" LABEL "Galones"~
  FIELD id_empresa LIKE items_release_delivery.id_empresa VALIDATE ~
  FIELD id_envase LIKE items_release_delivery.id_envase VALIDATE ~
  FIELD id_lote LIKE items_release_delivery.id_lote VALIDATE ~
  FIELD id_lote_deposito LIKE items_release_delivery.id_lote_deposito VALIDATE ~
  FIELD id_release_delivery LIKE items_release_delivery.id_release_delivery VALIDATE ~
  FIELD id_sucursal LIKE items_release_delivery.id_sucursal VALIDATE ~
  FIELD id_sucursal_ubicacion LIKE items_release_delivery.id_sucursal_ubicacion VALIDATE ~
  FIELD id_tipotambor LIKE items_release_delivery.id_tipotambor VALIDATE ~
  FIELD id_tipo_contrato LIKE items_release_delivery.id_tipo_contrato VALIDATE ~
  FIELD id_unidad_medida LIKE items_release_delivery.id_unidad_medida VALIDATE ~
  FIELD id_vapor LIKE items_release_delivery.id_vapor VALIDATE ~
  FIELD item_contrato LIKE items_release_delivery.item_contrato VALIDATE ~
  FIELD item_release_delivery LIKE items_release_delivery.item_release_delivery VALIDATE ~
  FIELD nromov LIKE items_release_delivery.nromov VALIDATE ~
  FIELD tambores LIKE items_release_delivery.tambores VALIDATE ~
  FIELD Vapor LIKE vapores.descripcion VALIDATE  LABEL "Vapor"~
  FIELD kilos_brutos LIKE items_release_delivery.kilos_brutos VALIDATE ~
  FIELD kilos_netos LIKE items_release_delivery.kilos_netos VALIDATE ~
  FIELD observaciones LIKE items_release_delivery.observaciones VALIDATE ~
  FIELD Cliente AS CHARACTER FORMAT "x(25)" LABEL "Cliente"~
  FIELD Broker AS CHARACTER FORMAT "x(20)" LABEL "Broker"~
  FIELD Producto AS CHARACTER FORMAT "x(15)" LABEL "Producto"~
  FIELD Destino AS CHARACTER FORMAT "x(20)" LABEL "Destino"~
  FIELD CondicionVta AS CHARACTER FORMAT "x(15)" LABEL "CondicionVta"~
  FIELD Plazos AS CHARACTER FORMAT "x(15)" LABEL "Plazos"~
  FIELD Moneda AS CHARACTER FORMAT "x(10)" LABEL "Moneda"~
  FIELD UnidadVta AS CHARACTER FORMAT "x(10)" LABEL "UnidadVta"
