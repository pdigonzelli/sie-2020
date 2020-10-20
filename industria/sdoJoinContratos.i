  FIELD id_empresa LIKE tambores_industria.id_empresa VALIDATE ~
  FIELD id_sucursal LIKE tambores_industria.id_sucursal VALIDATE ~
  FIELD id_tipotambor LIKE tambores_industria.id_tipotambor VALIDATE ~
  FIELD nromov LIKE tambores_industria.nromov VALIDATE ~
  FIELD id_protocolo LIKE protocolos.id_protocolo VALIDATE ~
  FIELD anio LIKE protocolos.anio VALIDATE ~
  FIELD id_articulo LIKE protocolos.id_articulo VALIDATE ~
  FIELD item_muestra LIKE r_muestras_protocolos.item_muestra VALIDATE ~
  FIELD id_muestra LIKE muestras.id_muestra VALIDATE ~
  FIELD anio_muestra LIKE muestras.anio_muestra VALIDATE ~
  FIELD id_contrato LIKE contratos.id_contrato VALIDATE ~
  FIELD id_tipo_contrato LIKE contratos.id_tipo_contrato VALIDATE ~
  FIELD anio_of LIKE tambores_industria.anio_of VALIDATE ~
  FIELD orden_fabricacion LIKE contratos.orden_fabricacion VALIDATE 
