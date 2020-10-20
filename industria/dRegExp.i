  FIELD semana LIKE registro_exportacion.semana VALIDATE ~
  FIELD Origen AS CHARACTER FORMAT "x(20)" LABEL "Origen"~
  FIELD Cantidad AS INTEGER FORMAT ">>>>>>>>>9" LABEL "Cantidad"~
  FIELD anio LIKE registro_exportacion.anio VALIDATE ~
  FIELD id_origen LIKE registro_exportacion.id_origen VALIDATE ~
  FIELD id_puerto_origen LIKE registro_exportacion.id_puerto_origen VALIDATE 
