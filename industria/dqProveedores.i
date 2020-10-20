  FIELD id_proveedor LIKE proveedores.id_proveedor VALIDATE ~
  FIELD Proveedor AS CHARACTER FORMAT "x(35)" LABEL "Proveedor"~
  FIELD nombre LIKE proveedores.nombre VALIDATE ~
  FIELD razon_social LIKE proveedores.razon_social VALIDATE ~
  FIELD TipoTrans AS CHARACTER FORMAT "x(15)" LABEL "TipoTrans"
