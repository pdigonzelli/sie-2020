  FIELD cantidad LIKE composicion_carga.cantidad VALIDATE ~
  FIELD c_fecha LIKE composicion_carga.c_fecha VALIDATE ~
  FIELD c_hora LIKE composicion_carga.c_hora VALIDATE ~
  FIELD c_usuario LIKE composicion_carga.c_usuario VALIDATE ~
  FIELD id_articulo LIKE composicion_carga.id_articulo VALIDATE ~
  FIELD id_empresa LIKE composicion_carga.id_empresa VALIDATE ~
  FIELD id_envase LIKE composicion_carga.id_envase VALIDATE ~
  FIELD id_sucursal LIKE composicion_carga.id_sucursal VALIDATE ~
  FIELD id_tipotambor LIKE composicion_carga.id_tipotambor VALIDATE ~
  FIELD id_unidad_quimica LIKE composicion_carga.id_unidad_quimica VALIDATE ~
  FIELD nromov LIKE composicion_carga.nromov VALIDATE ~
  FIELD observaciones LIKE composicion_carga.observaciones VALIDATE ~
  FIELD Articulo AS CHARACTER FORMAT "x(30)" LABEL "Articulo"~
  FIELD Acidez_w_v LIKE composicion_carga.Acidez_w_v VALIDATE ~
  FIELD Envase AS CHARACTER FORMAT "x(20)" LABEL "Envase"~
  FIELD Acidez_w_w LIKE composicion_carga.Acidez_w_w VALIDATE ~
  FIELD Unidad AS CHARACTER FORMAT "x(15)" LABEL "Unidad"~
  FIELD Bx_20_20 LIKE composicion_carga.Bx_20_20 VALIDATE ~
  FIELD Bx_correg LIKE composicion_carga.Bx_correg VALIDATE 
