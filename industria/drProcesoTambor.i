  FIELD c_fecha LIKE r_proceso_tambor.c_fecha VALIDATE ~
  FIELD c_hora LIKE r_proceso_tambor.c_hora VALIDATE ~
  FIELD c_usuario LIKE r_proceso_tambor.c_usuario VALIDATE ~
  FIELD id_empresa LIKE r_proceso_tambor.id_empresa VALIDATE ~
  FIELD id_empresa_proceso LIKE r_proceso_tambor.id_empresa_proceso VALIDATE ~
  FIELD id_sucursal LIKE r_proceso_tambor.id_sucursal VALIDATE ~
  FIELD id_sucursal_proceso LIKE r_proceso_tambor.id_sucursal_proceso VALIDATE ~
  FIELD id_tambor LIKE r_proceso_tambor.id_tambor VALIDATE ~
  FIELD id_tipotambor LIKE r_proceso_tambor.id_tipotambor VALIDATE ~
  FIELD id_tipotambor_proceso LIKE r_proceso_tambor.id_tipotambor_proceso VALIDATE ~
  FIELD nromov LIKE r_proceso_tambor.nromov VALIDATE ~
  FIELD nromov_proceso LIKE r_proceso_tambor.nromov_proceso VALIDATE ~
  FIELD observaciones LIKE r_proceso_tambor.observaciones VALIDATE ~
  FIELD Lote AS INTEGER FORMAT ">>>>>9" LABEL "Lote"~
  FIELD Anio AS INTEGER FORMAT ">>>9" LABEL "Anio"~
  FIELD Articulo AS CHARACTER FORMAT "x(15)" LABEL "Articulo"~
  FIELD Calidad AS CHARACTER FORMAT "x(15)" LABEL "Calidad"~
  FIELD Envase AS CHARACTER FORMAT "x(15)" LABEL "Envase"~
  FIELD Kilos AS DECIMAL FORMAT ">,>>9.99" LABEL "Kilos"~
  FIELD Litros AS DECIMAL FORMAT ">,>>9.99" LABEL "Litros"
