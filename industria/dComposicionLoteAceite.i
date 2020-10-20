  FIELD id_empresa LIKE composicion_lote_aceite.id_empresa VALIDATE ~
  FIELD id_sucursal LIKE composicion_lote_aceite.id_sucursal VALIDATE ~
  FIELD id_tipotambor LIKE composicion_lote_aceite.id_tipotambor VALIDATE ~
  FIELD nromov LIKE composicion_lote_aceite.nromov VALIDATE ~
  FIELD Numeracion_desde LIKE composicion_lote_aceite.Numeracion_desde VALIDATE  FORMAT ">>>9" LABEL "Desde"~
  FIELD Numeracion_hasta LIKE composicion_lote_aceite.Numeracion_hasta VALIDATE  FORMAT ">>>9" LABEL "Hasta"~
  FIELD tara LIKE composicion_lote_aceite.tara VALIDATE  FORMAT "->>>9.99"~
  FIELD cantidad_tambores LIKE composicion_lote_aceite.cantidad_tambores VALIDATE  FORMAT ">>>9" LABEL "Tambores"~
  FIELD id_lote LIKE composicion_lote_aceite.id_lote VALIDATE  FORMAT ">>>9"~
  FIELD fecha LIKE composicion_lote_aceite.fecha VALIDATE ~
  FIELD id_composicion LIKE composicion_lote_aceite.id_composicion VALIDATE ~
  FIELD kilos_tambor LIKE composicion_lote_aceite.kilos_tambor VALIDATE ~
  FIELD c_usuario LIKE composicion_lote_aceite.c_usuario VALIDATE ~
  FIELD c_hora LIKE composicion_lote_aceite.c_hora VALIDATE ~
  FIELD c_fecha LIKE composicion_lote_aceite.c_fecha VALIDATE 
