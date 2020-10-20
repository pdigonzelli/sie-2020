  FIELD id_origen LIKE registro_exportacion.id_origen VALIDATE ~
  FIELD id_puerto_origen LIKE registro_exportacion.id_puerto_origen VALIDATE ~
  FIELD semana LIKE registro_exportacion.semana VALIDATE ~
  FIELD Cantidad AS INTEGER FORMAT ">>>>>>9"~
  FIELD anio LIKE registro_exportacion.anio VALIDATE ~
  FIELD Origen AS CHARACTER FORMAT "x(50)" LABEL "Origen"
