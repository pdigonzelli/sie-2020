  FIELD id_empresa LIKE produccion_cascara.id_empresa VALIDATE ~
  FIELD bolsas_disponibles AS INTEGER FORMAT ">,>>9" LABEL "Disponibles"~
  FIELD id_sucursal LIKE produccion_cascara.id_sucursal VALIDATE ~
  FIELD id_sucursal_ubicacion LIKE produccion_cascara.id_sucursal_ubicacion VALIDATE ~
  FIELD id_turno LIKE produccion_cascara.id_turno VALIDATE ~
  FIELD id_produccion LIKE produccion_cascara.id_produccion VALIDATE ~
  FIELD id_posicion_ubicacion LIKE produccion_cascara.id_posicion_ubicacion VALIDATE ~
  FIELD id_locacion_ubicacion LIKE produccion_cascara.id_locacion_ubicacion VALIDATE ~
  FIELD id_etiqueta LIKE produccion_cascara.id_etiqueta VALIDATE ~
  FIELD id_empresa_ubicacion LIKE produccion_cascara.id_empresa_ubicacion VALIDATE ~
  FIELD id_supervisor LIKE produccion_cascara.id_supervisor VALIDATE ~
  FIELD id_capataz LIKE produccion_cascara.id_capataz VALIDATE ~
  FIELD id_articulo LIKE produccion_cascara.id_articulo VALIDATE ~
  FIELD Fecha LIKE produccion_cascara.Fecha VALIDATE ~
  FIELD cantidad_bolsas LIKE produccion_cascara.cantidad_bolsas VALIDATE  FORMAT ">>,>>9" LABEL "Producidas"~
  FIELD observaciones LIKE produccion_cascara.observaciones VALIDATE ~
  FIELD c_usuario LIKE produccion_cascara.c_usuario VALIDATE ~
  FIELD c_fecha LIKE produccion_cascara.c_fecha VALIDATE ~
  FIELD c_hora LIKE produccion_cascara.c_hora VALIDATE ~
  FIELD id_tipotambor LIKE produccion_cascara.id_tipotambor VALIDATE ~
  FIELD nromov LIKE produccion_cascara.nromov VALIDATE ~
  FIELD anio LIKE produccion_cascara.anio VALIDATE ~
  FIELD codigo_prod LIKE produccion_cascara.codigo_prod VALIDATE ~
  FIELD desde_bolsa LIKE produccion_cascara.desde_bolsa VALIDATE ~
  FIELD hasta_bolsa LIKE produccion_cascara.hasta_bolsa VALIDATE ~
  FIELD nro_prensa LIKE produccion_cascara.nro_prensa VALIDATE ~
  FIELD responsable LIKE produccion_cascara.responsable VALIDATE ~
  FIELD supervisor LIKE produccion_cascara.supervisor VALIDATE ~
  FIELD tipo LIKE produccion_cascara.tipo VALIDATE 
