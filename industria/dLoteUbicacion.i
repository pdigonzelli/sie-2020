  FIELD cantidad LIKE lotes_ubicacion.cantidad VALIDATE ~
  FIELD LoteDeposito AS CHARACTER FORMAT "x(8)" LABEL "LoteDeposito"~
  FIELD id_empresa LIKE lotes_ubicacion.id_empresa VALIDATE ~
  FIELD id_empresa_ubicacion LIKE lotes_ubicacion.id_empresa_ubicacion VALIDATE ~
  FIELD FechaProceso AS DATE FORMAT "99/99/9999"~
  FIELD id_locacion_ubicacion LIKE lotes_ubicacion.id_locacion_ubicacion VALIDATE ~
  FIELD id_lote LIKE lotes_ubicacion.id_lote VALIDATE  FORMAT ">,>>>,>>9"~
  FIELD id_lote_deposito LIKE lotes_ubicacion.id_lote_deposito VALIDATE ~
  FIELD id_posicion_ubicacion LIKE lotes_ubicacion.id_posicion_ubicacion VALIDATE ~
  FIELD contenedor AS CHARACTER FORMAT "x(25)"~
  FIELD id_sucursal LIKE lotes_ubicacion.id_sucursal VALIDATE ~
  FIELD id_sucursal_ubicacion LIKE lotes_ubicacion.id_sucursal_ubicacion VALIDATE ~
  FIELD id_tipotambor LIKE lotes_ubicacion.id_tipotambor VALIDATE ~
  FIELD nromov LIKE lotes_ubicacion.nromov VALIDATE ~
  FIELD vapor AS CHARACTER FORMAT "x(25)"~
  FIELD calidad LIKE lotes_ubicacion.calidad VALIDATE ~
  FIELD envase LIKE lotes_ubicacion.envase VALIDATE ~
  FIELD lote LIKE lotes_ubicacion.lote VALIDATE ~
  FIELD cantidad_comprometida LIKE lotes_ubicacion.cantidad_comprometida VALIDATE ~
  FIELD saldo AS INTEGER FORMAT ">>>9" LABEL "Cantidad"~
  FIELD id_tipo_sucursal LIKE sucursales.id_tipo_sucursal VALIDATE ~
  FIELD Deposito LIKE sucursales.nombre VALIDATE  LABEL "Deposito"~
  FIELD id_articulo LIKE lotes_ubicacion.id_articulo VALIDATE ~
  FIELD id_contrato LIKE lotes_ubicacion.id_contrato VALIDATE ~
  FIELD id_tipo_contrato LIKE lotes_ubicacion.id_tipo_contrato VALIDATE ~
  FIELD anio_contrato LIKE lotes_ubicacion.anio_contrato VALIDATE ~
  FIELD galones AS DECIMAL FORMAT "zzz,zz9.99"~
  FIELD item_contrato LIKE lotes_ubicacion.item_contrato VALIDATE ~
  FIELD c_usuario LIKE lotes_ubicacion.c_usuario VALIDATE ~
  FIELD c_hora LIKE lotes_ubicacion.c_hora VALIDATE ~
  FIELD c_fecha LIKE lotes_ubicacion.c_fecha VALIDATE ~
  FIELD codigo_lote LIKE lotes_ubicacion.codigo_lote VALIDATE ~
  FIELD abreviatura LIKE sucursales.abreviatura VALIDATE ~
  FIELD peso AS DECIMAL FORMAT "zzz,zz9.99"~
  FIELD Producto AS CHARACTER FORMAT "x(30)" LABEL "Producto"
