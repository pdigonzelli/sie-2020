  FIELD cantidad LIKE items_registro_exportacion.cantidad VALIDATE ~
  FIELD Fecha LIKE items_registro_exportacion.Fecha VALIDATE ~
  FIELD id_destino LIKE items_registro_exportacion.id_destino VALIDATE ~
  FIELD id_empresa_exportadora LIKE items_registro_exportacion.id_empresa_exportadora VALIDATE ~
  FIELD id_origen LIKE items_registro_exportacion.id_origen VALIDATE ~
  FIELD id_producto LIKE items_registro_exportacion.id_producto VALIDATE ~
  FIELD id_puerto_destino LIKE items_registro_exportacion.id_puerto_destino VALIDATE ~
  FIELD id_puerto_origen LIKE items_registro_exportacion.id_puerto_origen VALIDATE ~
  FIELD id_vapor LIKE items_registro_exportacion.id_vapor VALIDATE ~
  FIELD item LIKE items_registro_exportacion.item VALIDATE ~
  FIELD observaciones LIKE items_registro_exportacion.observaciones VALIDATE ~
  FIELD semana LIKE items_registro_exportacion.semana VALIDATE ~
  FIELD Compania LIKE empresa_exportadora.nombre VALIDATE ~
  FIELD Vapor LIKE vapores.descripcion VALIDATE  LABEL "Vapor"~
  FIELD Destino LIKE lugar_descarga.descripcion VALIDATE  LABEL "Destino"~
  FIELD anio LIKE items_registro_exportacion.anio VALIDATE 
