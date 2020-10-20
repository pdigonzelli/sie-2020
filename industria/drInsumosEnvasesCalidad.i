  FIELD id_articulo_insumo LIKE r_insumos_envase_calidad.id_articulo_insumo VALIDATE ~
  FIELD Confeccion AS CHARACTER FORMAT "x(30)" LABEL "Confeccion"~
  FIELD id_calidad LIKE r_insumos_envase_calidad.id_calidad VALIDATE ~
  FIELD Insumo AS CHARACTER FORMAT "x(30)" LABEL "Insumo"~
  FIELD id_envase LIKE r_insumos_envase_calidad.id_envase VALIDATE ~
  FIELD Envase AS CHARACTER FORMAT "x(30)" LABEL "Envase"~
  FIELD id_confeccion LIKE r_insumos_envase_calidad.id_confeccion VALIDATE ~
  FIELD Calidad AS CHARACTER FORMAT "x(30)" LABEL "Calidad"~
  FIELD orden LIKE r_insumos_envase_calidad.orden VALIDATE ~
  FIELD c_fecha LIKE r_insumos_envase_calidad.c_fecha VALIDATE ~
  FIELD c_hora LIKE r_insumos_envase_calidad.c_hora VALIDATE ~
  FIELD c_usuario LIKE r_insumos_envase_calidad.c_usuario VALIDATE 