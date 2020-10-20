
FOR EACH liq_items_control_tareas WHERE id_empresa = 101 AND fecha >= DATE("01/10/15") NO-LOCK:

FIND FIRST liq_legajos WHERE
    liq_legajos.id_empresa = liq_items_control_tareas.id_empresa AND
    liq_legajos.legajo = liq_items_control_tareas.legajo  NO-LOCK NO-ERROR.
IF AVAILABLE liq_legajos THEN
DO:
    FIND FIRST liq_centros_costos WHERE liq_centros_costos.id_centro_costo = liq_legajos.id_centro_costo NO-LOCK NO-ERROR.
    IF NOT AVAILABLE liq_centros_costos THEN
        DISPLAY liq_legajos.id_empresa liq_legajos.legajo 
        liq_legajos.apellido_nombre liq_legajos.id_centro_costo.
END.
END.
