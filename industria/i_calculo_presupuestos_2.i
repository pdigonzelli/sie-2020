    CREATE reporte_presupuestos.
    IF AVAILABLE orden_entrega THEN
    DO:
        ASSIGN reporte_presupuestos.id_orden_entrega     = orden_entrega.id_orden_entrega
               reporte_presupuestos.anio_envio           = year(orden_entrega.fecha)
               reporte_presupuestos.semana_oe            = orden_entrega.semana_entrega.
    END.
    IF AVAILABLE contactos_industria THEN DO:
        ASSIGN reporte_presupuestos.cliente_final        = contactos_industria.razon_social
               reporte_presupuestos.id_cliente_final     = contratos.id_cliente_final. 
    END.
    IF AVAILABLE tipos_plazo THEN DO:
        ASSIGN reporte_presupuestos.id_tipo_plazo        = tipos_plazo.id_tipo_plazo
               reporte_presupuestos.tipo_plazo           = tipos_plazo.descripcion
               reporte_presupuestos.plazo                = contratos.plazo.
               
    END.
    IF AVAILABLE instrumentos_pagos THEN DO:
        ASSIGN reporte_presupuestos.id_instrumento_pago  = instrumentos_pagos.id_instrumento_pago
               reporte_presupuestos.instrumento_pago     = instrumentos_pagos.descripcion.
    END.
        ASSIGN reporte_presupuestos.id_cliente           = contratos.id_cliente
               reporte_presupuestos.cliente              = clientes.razon_social
               reporte_presupuestos.id_contrato          = items_contratos.id_contrato
               reporte_presupuestos.id_tipo_contrato     = items_contratos.id_tipo_contrato
               reporte_presupuestos.anio                 = items_contratos.anio_semana_entrega
               reporte_presupuestos.ITEM                 = items_contratos.ITEM
               reporte_presupuestos.id_articulo          = items_contrato.id_articulo
               reporte_presupuestos.articulo             = productos_terminados.descripcion
               reporte_presupuestos.id_calidad           = items_contratos.id_calidad
               reporte_presupuestos.calidad              = calidades.descripcion
               reporte_presupuestos.semana_desde         = items_contratos.semana_entrega
               reporte_presupuestos.semana_hasta         = items_contratos.semana_entrega_hasta
               reporte_presupuestos.anio_item_contrato   = items_contratos.anio_semana_entrega
               reporte_presupuestos.precio_vta_contrato  = v_importe_contrato
               reporte_presupuestos.precio_fob_contrato  = v_importe_fob_contrato   
               reporte_presupuestos.precio_com_contrato  = (v_importe_comision * (v_porc_comision / 100))

               reporte_presupuestos.precio_vta_pl        = v_importe_item_fac
               reporte_presupuestos.precio_fob_pl        = v_importe_fob_pl
               reporte_presupuestos.cantidad_kilos_contrato = v_cantidad_contratos

               reporte_presupuestos.cantidad_kilos_pl    = v_cantidad_pl
               reporte_presupuestos.id_presupuesto       = 1.
