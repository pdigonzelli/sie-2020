    CREATE contratos_prod_fac_cons.
    IF AVAILABLE orden_entrega THEN
    DO:
        ASSIGN contratos_prod_fac_cons.id_orden_entrega = orden_entrega.id_orden_entrega.
    END.
        ASSIGN contratos_prod_fac_cons.id_contrato      = items_contratos.id_contrato
           contratos_prod_fac_cons.id_tipo_contrato     = items_contratos.id_tipo_contrato
           contratos_prod_fac_cons.anio                 = items_contratos.anio
           contratos_prod_fac_cons.ITEM                 = items_contratos.ITEM
           contratos_prod_fac_cons.anio_envio           = items_contratos.anio_semana_entrega
           contratos_prod_fac_cons.semana_envio_desde   = items_contratos.semana_entrega
           contratos_prod_fac_cons.semana_envio_hasta   = items_contratos.semana_entrega_hasta
           contratos_prod_fac_cons.tambores_contrato    = items_contrato.cantidad
           contratos_prod_fac_cons.id_cliente           = contrato.id_cliente
           contratos_prod_fac_cons.cliente              = clientes.razon_social
           contratos_prod_fac_cons.tambores_factura     = v_bultos_fac
           contratos_prod_fac_cons.tambores_prod        = v_tambores
           contratos_prod_fac_cons.kilos_tambores_prod  = v_kilos_tambores
           contratos_prod_fac_cons.tambores_desp        = v_tam_despachados
           contratos_prod_fac_cons.kilos_tambores_desp  = v_kilos_tam_despachados
           contratos_prod_fac_cons.tambores_stock       = v_tam_stock
           contratos_prod_fac_cons.kilos_tambores_stock = v_kilos_tam_stock
           contratos_prod_fac_cons.importe_contrato     = v_importe_oe
           contratos_prod_fac_cons.importe_factura      = v_importe_fac
           contratos_prod_fac_cons.importe_cobranza     = v_importe_cob
           contratos_prod_fac_cons.factura              = v_factura
           contratos_prod_fac_cons.ITEM_factura         = v_item_fac 
           contratos_prod_fac_cons.nro_pack_list         = v_nro_pack_list.
