  FIELD abreviatura LIKE liq_tareas.abreviatura VALIDATE ~
  FIELD desc-lote AS CHARACTER FORMAT "x(15)" LABEL "Lote" COLUMN-LABEL "Lote"~
  FIELD apellido_nombre LIKE liq_legajos.apellido_nombre VALIDATE ~
  FIELD ajuste_categoria LIKE liq_items_tarjas.ajuste_categoria VALIDATE ~
  FIELD cantidad LIKE liq_items_tarjas.cantidad VALIDATE ~
  FIELD cantidad_adicional LIKE liq_items_tarjas.cantidad_adicional VALIDATE ~
  FIELD cantidad_adicional-1 LIKE liq_items_tarjas.cantidad_adicional-1 VALIDATE ~
  FIELD cant_horas LIKE liq_items_tarjas.cant_horas VALIDATE ~
  FIELD cant_hs_compensa LIKE liq_items_tarjas.cant_hs_compensa VALIDATE ~
  FIELD cant_hs_extras LIKE liq_items_tarjas.cant_hs_extras VALIDATE ~
  FIELD cant_hs_norm LIKE liq_items_tarjas.cant_hs_norm VALIDATE ~
  FIELD cant_jornal LIKE liq_items_tarjas.cant_jornal VALIDATE ~
  FIELD cant_jornal_norm LIKE liq_items_tarjas.cant_jornal_norm VALIDATE ~
  FIELD compensa_hs LIKE liq_items_tarjas.compensa_hs VALIDATE ~
  FIELD c_fecha LIKE liq_items_tarjas.c_fecha VALIDATE ~
  FIELD c_hora LIKE liq_items_tarjas.c_hora VALIDATE ~
  FIELD c_usuario LIKE liq_items_tarjas.c_usuario VALIDATE ~
  FIELD dni_cuil LIKE liq_items_tarjas.dni_cuil VALIDATE ~
  FIELD fecha LIKE liq_items_tarjas.fecha VALIDATE ~
  FIELD hora_fin LIKE liq_items_tarjas.hora_fin VALIDATE ~
  FIELD hora_fin-1 LIKE liq_items_tarjas.hora_fin-1 VALIDATE ~
  FIELD hora_inicio LIKE liq_items_tarjas.hora_inicio VALIDATE ~
  FIELD hora_inicio-1 LIKE liq_items_tarjas.hora_inicio-1 VALIDATE ~
  FIELD id_centro_costo LIKE liq_items_tarjas.id_centro_costo VALIDATE ~
  FIELD id_diferencial LIKE liq_items_tarjas.id_diferencial VALIDATE ~
  FIELD id_empresa LIKE liq_items_tarjas.id_empresa VALIDATE ~
  FIELD id_grupo LIKE liq_items_tarjas.id_grupo VALIDATE ~
  FIELD id_lote LIKE liq_items_tarjas.id_lote VALIDATE ~
  FIELD id_origen LIKE liq_items_tarjas.id_origen VALIDATE ~
  FIELD id_proveedor LIKE liq_items_tarjas.id_proveedor VALIDATE ~
  FIELD id_reserva LIKE liq_items_tarjas.id_reserva VALIDATE ~
  FIELD id_sector LIKE liq_items_tarjas.id_sector VALIDATE ~
  FIELD id_sucursal LIKE liq_items_tarjas.id_sucursal VALIDATE ~
  FIELD id_tarea LIKE liq_items_tarjas.id_tarea VALIDATE ~
  FIELD id_tipo_planilla LIKE liq_items_tarjas.id_tipo_planilla VALIDATE ~
  FIELD id_unidad_adicional LIKE liq_items_tarjas.id_unidad_adicional VALIDATE ~
  FIELD id_unidad_liquidacion LIKE liq_items_tarjas.id_unidad_liquidacion VALIDATE ~
  FIELD legajo LIKE liq_items_tarjas.legajo VALIDATE  FORMAT ">>>>>>>>9"~
  FIELD nombre LIKE liq_items_tarjas.nombre VALIDATE ~
  FIELD nro_maquina LIKE liq_items_tarjas.nro_maquina VALIDATE ~
  FIELD nro_tractor LIKE liq_items_tarjas.nro_tractor VALIDATE ~
  FIELD tipo_turno LIKE liq_items_tarjas.tipo_turno VALIDATE ~
  FIELD total_horas LIKE liq_items_tarjas.total_horas VALIDATE ~
  FIELD id_turno LIKE liq_items_tarjas.id_turno VALIDATE ~
  FIELD hs_adicionales LIKE liq_items_tarjas.hs_adicionales VALIDATE ~
  FIELD id_codigo_abacus LIKE liq_items_tarjas.id_codigo_abacus VALIDATE  FORMAT ">>>9"~
  FIELD id_codigo_abacus_diferencial LIKE liq_items_tarjas.id_codigo_abacus_diferencial VALIDATE  FORMAT ">>>9"~
  FIELD id_convenio LIKE liq_legajos.id_convenio VALIDATE ~
  FIELD id_categoria LIKE liq_legajos.id_categoria VALIDATE ~
  FIELD id_codigo_abacus_adicional LIKE liq_items_tarjas.id_codigo_abacus_adicional VALIDATE ~
  FIELD id_codigo_abacus_cantidad LIKE liq_items_tarjas.id_codigo_abacus_cantidad VALIDATE ~
  FIELD fecha_hs_compensada LIKE liq_items_tarjas.fecha_hs_compensada VALIDATE ~
  FIELD hs_acond_finca LIKE liq_items_tarjas.hs_acond_finca VALIDATE ~
  FIELD hs_adicionales_tareas_trabajadas LIKE liq_items_tarjas.hs_adicionales_tareas_trabajadas VALIDATE ~
  FIELD hs_compensadas LIKE liq_items_tarjas.hs_compensadas VALIDATE ~
  FIELD hs_plus_tareas_automatico LIKE liq_items_tarjas.hs_plus_tareas_automatico VALIDATE ~
  FIELD hs_plus_tareas_trabajadas LIKE liq_items_tarjas.hs_plus_tareas_trabajadas VALIDATE ~
  FIELD descripcion LIKE liq_tareas.descripcion VALIDATE ~
  FIELD autoriza_hs LIKE liq_items_tarjas.autoriza_hs VALIDATE 
