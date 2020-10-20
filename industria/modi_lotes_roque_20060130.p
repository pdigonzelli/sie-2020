FOR EACH lotes_aceite WHERE anio = 2006 AND nromov = 70361:
    DISP lotes_aceite.activo codigo_lote control_calidad estado_lote id_articulo id_contrato_of id_envase id_lote id_lote_nuevo id_orden id_orden_entrega id_sucursal id_tipocontrato_of id_tipotambor item_oe item_of nromov nro_partida Peso_neto quimico_control_calidad tanque.
    FOR EACH tambores_industria OF lotes_aceite:
        DISP tambores_industria.
    END.
    FOR EACH lotes_ubicacion OF lotes_aceite:
        DISP lotes_ubicacion.
    END.
END.
