    CREATE prog_produccion.
    ASSIGN prog_produccion.orden_fabricacion        = string(contratos.orden_fabricacion)
           prog_produccion.id_contrato              = items_contratos.id_contrato
           prog_produccion.id_tipo_contrato         = items_contratos.id_tipo_contrato
           prog_produccion.anio                     = items_contratos.anio_semana_entrega
           prog_produccion.ITEM                     = items_contratos.ITEM
           prog_produccion.id_calidad               = items_contratos.id_calidad
           prog_produccion.calidad                  = calidades.descripcion
           prog_produccion.total_kgs_contrato       = v_total_kgs_contratos
           
           prog_produccion.kgs_parte                = v_kgs_items
           prog_produccion.semana_desde             = items_contratos.semana_entrega
           prog_produccion.semana_hasta             = items_contratos.semana_entrega_hasta
           prog_produccion.anio_embarque            = items_contratos.anio_semana_entrega
           
           prog_produccion.lote                     = v_lotes
           prog_produccion.kgs_asignados            = v_kgs_asignados
           prog_produccion.kgs_acum_asignados       = v_kgs_acum_asignados
           prog_produccion.kgs_pendientes           = v_kgs_items - v_kgs_asignados
           
           prog_produccion.lote_of                  = v_lotes_of
           prog_produccion.kgs_asignados_of         = v_kgs_asignados_of
           prog_produccion.kgs_acum_asignados_of    = v_kgs_acum_asignados_of
           prog_produccion.kgs_pendientes_of        = v_kgs_items - v_kgs_asignados_of.

    IF AVAILABLE items_orden_entrega THEN
    DO:
        ASSIGN prog_produccion.id_orden_entrega     = items_orden_entrega.id_orden_entrega
               prog_produccion.ITEM_oe              = items_orden_entrega.ITEM_oe
               prog_produccion.semana_oe            = items_orden_entrega.semana_entrega.
    END.
