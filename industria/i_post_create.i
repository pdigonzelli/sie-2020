general.orden_entrega.anio             = integer(orden_entrega.anio:screen-value in frame F-Main).
general.orden_entrega.item             = integer(orden_entrega.item:screen-value in frame F-Main).
general.orden_entrega.id_tipo_contrato = integer(orden_entrega.id_tipo_contrato:screen-value in frame F-Main).
general.orden_entrega.semana_entrega   = integer(orden_entrega.semana_entrega:screen-value in frame F-Main).

assign orden_entrega.c_usuario = userid("userdb")
       orden_entrega.c_fecha   = today
       orden_entrega.c_hora    = string(time,"HH:MM:SS")
       orden_entrega.id_tipo_orden_entrega = 1.

v_anio = 0.
v_item = 0.
v_id_tipo_contrato = 0.
v_semana_entrega = 0.

v_alta = 1.
