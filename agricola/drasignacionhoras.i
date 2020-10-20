  FIELD horas_trabajadas LIKE r_asignacion_horas.horas_trabajadas VALIDATE ~
  FIELD hs_asig LIKE r_asignacion_horas.hs_asig VALIDATE ~
  FIELD hs_norm LIKE r_asignacion_horas.hs_norm VALIDATE ~
  FIELD id_tipo_asignacion LIKE r_asignacion_horas.id_tipo_asignacion VALIDATE  LABEL "Tipo"~
  FIELD descripcion LIKE tipo_asignaciones.descripcion VALIDATE  FORMAT "x(40)"
