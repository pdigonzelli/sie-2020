  FIELD nombre LIKE clientes.nombre VALIDATE  LABEL "Cliente"~
  FIELD articulo LIKE productos_terminados.descripcion VALIDATE  LABEL "Articulo"~
  FIELD envase LIKE envases_prod.descripcion VALIDATE  LABEL "Envase"~
  FIELD id_articulo LIKE r_clientes_envases_precios.id_articulo VALIDATE ~
  FIELD id_cliente LIKE r_clientes_envases_precios.id_cliente VALIDATE ~
  FIELD id_condicion_venta LIKE r_clientes_envases_precios.id_condicion_venta VALIDATE ~
  FIELD id_envase LIKE r_clientes_envases_precios.id_envase VALIDATE ~
  FIELD id_moneda LIKE r_clientes_envases_precios.id_moneda VALIDATE ~
  FIELD precio LIKE r_clientes_envases_precios.precio VALIDATE ~
  FIELD calibre LIKE r_clientes_envases_precios.calibre VALIDATE ~
  FIELD id_lista_precio LIKE r_clientes_envases_precios.id_lista_precio VALIDATE 
