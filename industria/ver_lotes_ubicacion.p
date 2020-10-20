FOR EACH lotes_ubicacion WHERE id_lote = 151 AND id_articulo = 53 AND anio = 2006 NO-LOCK:
    FIND FIRST contratos WHERE contratos.id_tipo_contrato = lotes_ubicacion.id_tipo_contrato and
                               contratos.id_contrato = lotes_ubicacion.id_contrato NO-LOCK NO-ERROR.
    FIND FIRST items_contratos OF contratos WHERE
        items_contratos.ITEM = lotes_ubicacion.ITEM_contrato NO-LOCK .
    DISPLAY  nromov id_sucursal_ubicacion lotes_ubicacion.id_contrato ITEM_contrato 
             contratos.id_cliente lotes_ubicacion.ITEM_contrato.
END.
