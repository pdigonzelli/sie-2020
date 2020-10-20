FOR EACH lotes_ubicacion.
    DELETE lotes_ubicacion.
END.

FOR EACH movimientos_lote_ubicacion.
    DELETE movimientos_lote_ubicacion.
END.

FOR EACH ingreso_lote_ubicacion.
    DELETE ingreso_lote_ubicacion.
END.

FOR EACH item_ingreso_lote_ubicacion.
    DELETE item_ingreso_lote_ubicacion.
END.

FOR EACH stock_historico WHERE c_fecha = TODAY.
    DELETE stock_historico.
END.

FOR EACH tambores_industria WHERE id_sucursal_ubicacion = 86 .
    id_sucursal_ubicacion = 91.
END.
