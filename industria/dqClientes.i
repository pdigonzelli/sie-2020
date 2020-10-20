  FIELD id_cliente LIKE clientes.id_cliente VALIDATE ~
  FIELD ClienteCod AS CHARACTER FORMAT "x(25)" LABEL "ClienteCod"~
  FIELD nombre LIKE clientes.nombre VALIDATE ~
  FIELD razon_social LIKE clientes.razon_social VALIDATE 
