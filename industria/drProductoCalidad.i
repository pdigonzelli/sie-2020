  FIELD coeficiente LIKE r_productos_calidad.coeficiente VALIDATE ~
  FIELD c_fecha LIKE r_productos_calidad.c_fecha VALIDATE ~
  FIELD c_hora LIKE r_productos_calidad.c_hora VALIDATE ~
  FIELD c_usuario LIKE r_productos_calidad.c_usuario VALIDATE ~
  FIELD id_articulo LIKE r_productos_calidad.id_articulo VALIDATE ~
  FIELD id_calidad LIKE r_productos_calidad.id_calidad VALIDATE ~
  FIELD rnpa LIKE r_productos_calidad.rnpa VALIDATE ~
  FIELD Articulo AS CHARACTER FORMAT "x(25)" LABEL "Articulo"~
  FIELD Calidad AS CHARACTER FORMAT "x(20)" LABEL "Calidad"
