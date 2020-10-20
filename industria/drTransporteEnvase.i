  FIELD id_tipo_transporte LIKE r_transporte_envase.id_tipo_transporte VALIDATE ~
  FIELD id_evnase LIKE r_transporte_envase.id_evnase VALIDATE ~
  FIELD id_proveedor LIKE r_transporte_envase.id_proveedor VALIDATE ~
  FIELD capacidad LIKE r_transporte_envase.capacidad VALIDATE ~
  FIELD id_articulo LIKE r_transporte_envase.id_articulo VALIDATE ~
  FIELD c_usuario LIKE r_transporte_envase.c_usuario VALIDATE ~
  FIELD c_fecha LIKE r_transporte_envase.c_fecha VALIDATE ~
  FIELD c_hora LIKE r_transporte_envase.c_hora VALIDATE ~
  FIELD Proveedor AS CHARACTER FORMAT "x(20)" LABEL "Proveedor"~
  FIELD Envase AS CHARACTER FORMAT "x(25)" LABEL "Envase"~
  FIELD Tipo AS CHARACTER FORMAT "x(25)" LABEL "Tipo"~
  FIELD Producto AS CHARACTER FORMAT "x(25)" LABEL "Producto"
