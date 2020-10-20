  FIELD anio LIKE lotes_jugo.anio VALIDATE ~
  FIELD anio_of LIKE lotes_jugo.anio_of VALIDATE ~
  FIELD Balanza_usada LIKE lotes_jugo.Balanza_usada VALIDATE ~
  FIELD Calibracion LIKE lotes_jugo.Calibracion VALIDATE ~
  FIELD composicion_lote LIKE lotes_jugo.composicion_lote VALIDATE ~
  FIELD control_calidad LIKE lotes_jugo.control_calidad VALIDATE ~
  FIELD Control_pesas LIKE lotes_jugo.Control_pesas VALIDATE ~
  FIELD c_fecha LIKE lotes_jugo.c_fecha VALIDATE ~
  FIELD c_hora LIKE lotes_jugo.c_hora VALIDATE ~
  FIELD c_usuario LIKE lotes_jugo.c_usuario VALIDATE ~
  FIELD estado LIKE lotes_jugo.estado VALIDATE ~
  FIELD estado_lote LIKE lotes_jugo.estado_lote VALIDATE ~
  FIELD Fecha LIKE lotes_jugo.Fecha VALIDATE ~
  FIELD Fecha_comienzo LIKE lotes_jugo.Fecha_comienzo VALIDATE ~
  FIELD Fecha_comienzo_envase LIKE lotes_jugo.Fecha_comienzo_envase VALIDATE ~
  FIELD Fecha_finalizacion LIKE lotes_jugo.Fecha_finalizacion VALIDATE ~
  FIELD Fecha_finalizacion_envase LIKE lotes_jugo.Fecha_finalizacion_envase VALIDATE ~
  FIELD Hora_comienzo LIKE lotes_jugo.Hora_comienzo VALIDATE ~
  FIELD Hora_comienzo_envase LIKE lotes_jugo.Hora_comienzo_envase VALIDATE ~
  FIELD Hora_finalizacion LIKE lotes_jugo.Hora_finalizacion VALIDATE ~
  FIELD Hora_fin_envase LIKE lotes_jugo.Hora_fin_envase VALIDATE ~
  FIELD id_articulo LIKE lotes_jugo.id_articulo VALIDATE ~
  FIELD id_calidad LIKE lotes_jugo.id_calidad VALIDATE ~
  FIELD id_contrato_of LIKE lotes_jugo.id_contrato_of VALIDATE ~
  FIELD id_empresa LIKE lotes_jugo.id_empresa VALIDATE ~
  FIELD id_envase LIKE lotes_jugo.id_envase VALIDATE ~
  FIELD id_envio_of LIKE lotes_jugo.id_envio_of VALIDATE ~
  FIELD id_legajo_capataz LIKE lotes_jugo.id_legajo_capataz VALIDATE ~
  FIELD id_lote LIKE lotes_jugo.id_lote VALIDATE ~
  FIELD id_lote_nuevo LIKE lotes_jugo.id_lote_nuevo VALIDATE ~
  FIELD id_orden_entrega LIKE lotes_jugo.id_orden_entrega VALIDATE ~
  FIELD id_sucursal LIKE lotes_jugo.id_sucursal VALIDATE ~
  FIELD id_tanque LIKE lotes_jugo.id_tanque VALIDATE ~
  FIELD id_tipocontrato_of LIKE lotes_jugo.id_tipocontrato_of VALIDATE ~
  FIELD id_tipolimon LIKE lotes_jugo.id_tipolimon VALIDATE ~
  FIELD id_tipotambor LIKE lotes_jugo.id_tipotambor VALIDATE ~
  FIELD item_oe LIKE lotes_jugo.item_oe VALIDATE ~
  FIELD item_of LIKE lotes_jugo.item_of VALIDATE ~
  FIELD Jugo_pomelo LIKE lotes_jugo.Jugo_pomelo VALIDATE ~
  FIELD microbiologia LIKE lotes_jugo.microbiologia VALIDATE ~
  FIELD nromov LIKE lotes_jugo.nromov VALIDATE ~
  FIELD observaciones LIKE lotes_jugo.observaciones VALIDATE ~
  FIELD Peso_neto LIKE lotes_jugo.Peso_neto VALIDATE ~
  FIELD Pulpa LIKE lotes_jugo.Pulpa VALIDATE ~
  FIELD quimico_control_calidad LIKE lotes_jugo.quimico_control_calidad VALIDATE ~
  FIELD quimico_microbiologia LIKE lotes_jugo.quimico_microbiologia VALIDATE ~
  FIELD Articulo AS CHARACTER FORMAT "x(15)" LABEL "Articulo"~
  FIELD cantidad_envases_nuevo LIKE lotes_jugo.cantidad_envases_nuevo VALIDATE ~
  FIELD cantidad_tambores_recup LIKE lotes_jugo.cantidad_tambores_recup VALIDATE ~
  FIELD id_condicion_origen LIKE lotes_jugo.id_condicion_origen VALIDATE ~
  FIELD kilos_jugo_linea LIKE lotes_jugo.kilos_jugo_linea VALIDATE ~
  FIELD capataz LIKE lotes_jugo.capataz VALIDATE ~
  FIELD envasadores LIKE lotes_jugo.envasadores VALIDATE ~
  FIELD etiqueta_adicional LIKE lotes_jugo.etiqueta_adicional VALIDATE ~
  FIELD Calidad AS CHARACTER FORMAT "x(15)" LABEL "Calidad"~
  FIELD etiq_adicional_descripcion LIKE lotes_jugo.etiq_adicional_descripcion VALIDATE ~
  FIELD hora_aprobacion LIKE lotes_jugo.hora_aprobacion VALIDATE ~
  FIELD id_camara LIKE lotes_jugo.id_camara VALIDATE ~
  FIELD id_tanque_sobrante LIKE lotes_jugo.id_tanque_sobrante VALIDATE ~
  FIELD concentracion_mesh LIKE lotes_jugo.concentracion_mesh VALIDATE ~
  FIELD Envase AS CHARACTER FORMAT "x(15)" LABEL "Envase"~
  FIELD codigo_lote LIKE lotes_jugo.codigo_lote VALIDATE ~
  FIELD fecha_control_calidad LIKE lotes_jugo.fecha_control_calidad VALIDATE ~
  FIELD fecha_microbiologia LIKE lotes_jugo.fecha_microbiologia VALIDATE ~
  FIELD fecha_pesticida LIKE lotes_jugo.fecha_pesticida VALIDATE ~
  FIELD Sucursal AS CHARACTER FORMAT "x(15)" LABEL "Sucursal"~
  FIELD fecha_sensorial LIKE lotes_jugo.fecha_sensorial VALIDATE ~
  FIELD Tambores AS INTEGER FORMAT ">>>>9" LABEL "Tambores"~
  FIELD pesticida LIKE lotes_jugo.pesticida VALIDATE ~
  FIELD ControlCalidad AS CHARACTER FORMAT "x(20)" LABEL "ControlCalidad"~
  FIELD sensorial LIKE lotes_jugo.sensorial VALIDATE ~
  FIELD Quimico AS CHARACTER FORMAT "x(20)" LABEL "Quimico"~
  FIELD ControlMicro AS CHARACTER FORMAT "x(20)" LABEL "ControlMicro"~
  FIELD QuimicoMicro AS CHARACTER FORMAT "x(20)" LABEL "QuimicoMicro"~
  FIELD Kilos AS DECIMAL FORMAT ">>>,>>9.99" LABEL "Kilos"~
  FIELD Kilos400 AS DECIMAL FORMAT ">>>,>>>,>>9.99" LABEL "Kilos400"~
  FIELD Litros AS DECIMAL FORMAT ">>>,>>9.99" LABEL "Litros"~
  FIELD Contrato AS CHARACTER FORMAT "x(15)" LABEL "Contrato"~
  FIELD OrdenFabricacion AS INTEGER FORMAT ">>>9" LABEL "OrdenFabricacion"~
  FIELD NroCtrlAjuste AS CHARACTER FORMAT "x(20)" LABEL "NroCtrlAjuste"~
  FIELD Galones AS DECIMAL FORMAT ">>>,>>>,>>9.999999" LABEL "Galontes"
