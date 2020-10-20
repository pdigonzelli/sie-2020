  FIELD id_pedido LIKE pedidos.id_pedido VALIDATE ~
  FIELD id_proveedor LIKE pedidos.id_proveedor VALIDATE ~
  FIELD nombre LIKE proveedores.nombre VALIDATE ~
  FIELD recibido LIKE pedidos.recibido VALIDATE ~
  FIELD estado LIKE pedidos.estado VALIDATE ~
  FIELD bultos LIKE cant_pedidos.bultos VALIDATE ~
  FIELD cantidad LIKE cant_pedidos.cantidad VALIDATE ~
  FIELD cant_recibida LIKE cant_pedidos.cant_recibida VALIDATE ~
  FIELD id_articulo LIKE cant_pedidos.id_articulo VALIDATE ~
  FIELD id_suc_origen LIKE cant_pedidos.id_suc_origen VALIDATE ~
  FIELD peso LIKE cant_pedidos.peso VALIDATE ~
  FIELD unidades LIKE cant_pedidos.unidades VALIDATE 
