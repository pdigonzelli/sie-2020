  FIELD Acidez_w_v LIKE cargas.Acidez_w_v VALIDATE ~
  FIELD Acidez_w_w LIKE cargas.Acidez_w_w VALIDATE ~
  FIELD anio LIKE cargas.anio VALIDATE ~
  FIELD Bx_20_20 LIKE cargas.Bx_20_20 VALIDATE ~
  FIELD Bx_correg LIKE cargas.Bx_correg VALIDATE ~
  FIELD cantidad_envases_nuevo LIKE cargas.cantidad_envases_nuevo VALIDATE ~
  FIELD cantidad_envases_recup LIKE cargas.cantidad_envases_recup VALIDATE ~
  FIELD cantidad_enzima LIKE cargas.cantidad_enzima VALIDATE ~
  FIELD c_fecha LIKE cargas.c_fecha VALIDATE ~
  FIELD c_hora LIKE cargas.c_hora VALIDATE ~
  FIELD c_usuario LIKE cargas.c_usuario VALIDATE ~
  FIELD encargado LIKE cargas.encargado VALIDATE ~
  FIELD Fecha LIKE cargas.Fecha VALIDATE ~
  FIELD Hora_comienzo LIKE cargas.Hora_comienzo VALIDATE ~
  FIELD Hora_finalizacion LIKE cargas.Hora_finalizacion VALIDATE ~
  FIELD id_articulo LIKE cargas.id_articulo VALIDATE ~
  FIELD id_calidad LIKE cargas.id_calidad VALIDATE ~
  FIELD id_carga LIKE cargas.id_carga VALIDATE ~
  FIELD id_empresa LIKE cargas.id_empresa VALIDATE ~
  FIELD id_empresa_proceso LIKE cargas.id_empresa_proceso VALIDATE ~
  FIELD id_proceso LIKE cargas.id_proceso VALIDATE ~
  FIELD id_quimico LIKE cargas.id_quimico VALIDATE ~
  FIELD id_sucursal LIKE cargas.id_sucursal VALIDATE ~
  FIELD id_sucursal_proceso LIKE cargas.id_sucursal_proceso VALIDATE ~
  FIELD id_tanque LIKE cargas.id_tanque VALIDATE ~
  FIELD id_tipotambor LIKE cargas.id_tipotambor VALIDATE ~
  FIELD id_tipotambor_proceso LIKE cargas.id_tipotambor_proceso VALIDATE ~
  FIELD litros LIKE cargas.litros VALIDATE ~
  FIELD nromov LIKE cargas.nromov VALIDATE ~
  FIELD nromov_proceso LIKE cargas.nromov_proceso VALIDATE ~
  FIELD observaciones LIKE cargas.observaciones VALIDATE ~
  FIELD operario_enzima LIKE cargas.operario_enzima VALIDATE ~
  FIELD Pulpa LIKE cargas.Pulpa VALIDATE ~
  FIELD Sodio LIKE cargas.Sodio VALIDATE ~
  FIELD temperatura_carga LIKE cargas.temperatura_carga VALIDATE ~
  FIELD tiempo_actividad_enzima LIKE cargas.tiempo_actividad_enzima VALIDATE ~
  FIELD tipo_enzima LIKE cargas.tipo_enzima VALIDATE ~
  FIELD volumen_final LIKE cargas.volumen_final VALIDATE ~
  FIELD Sucursal AS CHARACTER FORMAT "x(15)" LABEL "Sucursal"~
  FIELD Articulo AS CHARACTER FORMAT "x(20)" LABEL "Articulo"~
  FIELD Tanque AS CHARACTER FORMAT "x(10)" LABEL "Tanque"~
  FIELD Quimico AS CHARACTER FORMAT "x(25)" LABEL "Quimico"~
  FIELD SolSolubles AS DECIMAL FORMAT "->>,>>>,>>9.99" LABEL "SolSolubles"~
  FIELD Coef AS DECIMAL FORMAT ">,>>9.99" LABEL "Coef"~
  FIELD Kilos AS DECIMAL FORMAT ">>>,>>>,>>9.99" LABEL "Kilos"~
  FIELD Kilos400 AS DECIMAL FORMAT ">>>,>>>,>>9.99" LABEL "Kilos400"
