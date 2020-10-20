  FIELD c_fecha LIKE r_insumos_envase_calidad.c_fecha VALIDATE ~
  FIELD c_hora LIKE r_insumos_envase_calidad.c_hora VALIDATE ~
  FIELD c_usuario LIKE r_insumos_envase_calidad.c_usuario VALIDATE ~
  FIELD id_articulo_insumo LIKE r_insumos_envase_calidad.id_articulo_insumo VALIDATE ~
  FIELD id_calidad LIKE r_insumos_envase_calidad.id_calidad VALIDATE ~
  FIELD id_confeccion LIKE r_insumos_envase_calidad.id_confeccion VALIDATE ~
  FIELD id_envase LIKE r_insumos_envase_calidad.id_envase VALIDATE ~
  FIELD orden LIKE r_insumos_envase_calidad.orden VALIDATE ~
  FIELD Calidad LIKE calidades.descripcion VALIDATE  LABEL "Calidad"~
  FIELD Envase LIKE envases_prod.descripcion VALIDATE  LABEL "Envase"~
  FIELD Articulo LIKE articulos.descripcion VALIDATE  LABEL "Articulo"~
  FIELD Confeccion LIKE confecciones_insumos.descripcion VALIDATE  LABEL "Confeccion"
