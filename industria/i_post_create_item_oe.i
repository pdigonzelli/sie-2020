DEFINE VAR h-con AS HANDLE.
DEFINE VAR v_oe AS INTEGER.
DEFINE VAR v_tipo_oe AS INTEGER.


RUN get-container (OUTPUT h-con).
RUN get-oe IN h-con (OUTPUT v_oe).
RUN get-tipo-oe IN h-con (OUTPUT v_tipo_oe).

general.items_orden_entrega.anio             = integer(items_orden_entrega.anio:screen-value in frame F-Main).
general.items_orden_entrega.item             = integer(items_orden_entrega.item:screen-value in frame F-Main).
general.items_orden_entrega.id_tipo_contrato = integer(items_orden_entrega.id_tipo_contrato:screen-value in frame F-Main).
general.items_orden_entrega.semana_entrega   = integer(items_orden_entrega.semana_entrega:screen-value in frame F-Main).

assign items_orden_entrega.c_usuario                = userid("userdb")
       items_orden_entrega.c_fecha                  = today
       items_orden_entrega.c_hora                   = string(time,"HH:MM:SS")
       items_orden_entrega.id_orden_entrega         = v_oe
       items_orden_entrega.id_tipo_orden_entrega    = v_tipo_oe
       items_orden_entrega.id_moneda                = v_moneda
       items_orden_entrega.id_tipo_unidad_venta     = v_tipo_unidad
       items_orden_entrega.importe_origen           = v_precio
       items_orden_entrega.id_tipo_venta            = v_tipo_venta
       items_orden_entrega.pendiente                = FALSE.

v_anio = 0.
v_item = 0.
v_id_tipo_contrato = 0.
v_semana_entrega = 0.
v_alta = 1.
