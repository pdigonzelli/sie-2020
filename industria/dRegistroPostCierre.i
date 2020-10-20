  FIELD c_fecha LIKE registro_post_cierre.c_fecha VALIDATE ~
  FIELD c_hora LIKE registro_post_cierre.c_hora VALIDATE ~
  FIELD c_usuario LIKE registro_post_cierre.c_usuario VALIDATE ~
  FIELD fecha LIKE registro_post_cierre.fecha VALIDATE ~
  FIELD id_empresa LIKE registro_post_cierre.id_empresa VALIDATE ~
  FIELD id_sucursal LIKE registro_post_cierre.id_sucursal VALIDATE ~
  FIELD id_tipotambor LIKE registro_post_cierre.id_tipotambor VALIDATE ~
  FIELD motivos LIKE registro_post_cierre.motivos VALIDATE ~
  FIELD nromov LIKE registro_post_cierre.nromov VALIDATE ~
  FIELD observaciones LIKE registro_post_cierre.observaciones VALIDATE ~
  FIELD serial_control LIKE registro_post_cierre.serial_control VALIDATE ~
  FIELD Lote_Proc AS CHARACTER FORMAT "x(10)" LABEL "Lote_Proc"~
  FIELD anio-2 LIKE registro_post_cierre.anio VALIDATE ~
  FIELD Anio AS INTEGER FORMAT ">>>9" LABEL "Anio"~
  FIELD id_registro LIKE registro_post_cierre.id_registro VALIDATE ~
  FIELD Producto AS CHARACTER FORMAT "x(25)" LABEL "Producto"~
  FIELD FechaInicio AS DATE FORMAT "99/99/9999" LABEL "FechaInicio"~
  FIELD FechaCierre AS DATE FORMAT "99/99/9999" LABEL "FechaCierre"
