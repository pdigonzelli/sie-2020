IF subd_vtas.nro_proforma <> 0 THEN v_factura = STRING(subd_vtas.nro_proforma,">>>9999").
ELSE v_factura = string(subd_vtas.id_punto_venta,"9999") + STRING(subd_vtas.nro_comp,"99999999").
v_importe_fac = subd_vtas.importe_origen.
v_fecha_fac = subd_vtas.fecha_comp.
v_fecha_vto = subd_vtas.vencimiento.
/*
FOR EACH movimientos_venta OF subd_vtas NO-LOCK,
                            FIRST items_operacion OF movimientos_venta 
                                WHERE items_operacion.id_concepto = 1 NO-LOCK.
    v_valor_fob = movimientos_venta.importe.
END.
*/
