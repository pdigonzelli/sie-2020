/*FOR EACH liq_control_tareas WHERE fecha >= DATE("01/10/15") BREAK BY id_proveedor BY id_origen:
    IF LAST-OF(id_origen) THEN
    DO:
        DISPLAY id_proveedor FORMAT ">>>>>>>>>>" id_origen.
        FIND FIRST origenes OF liq_control_tareas NO-LOCK NO-ERROR.
        IF AVAILABLE origenes THEN 
            DISPLAY origenes.descripcion.
    END.

END.*/


/*
OUTPUT TO z:\temp\ver-finca-planilla.txt.
FOR EACH liq_items_control_tareas WHERE fecha >= DATE("01/10/2015") AND id_proveedor > 1 NO-LOCK BREAK BY id_proveedor BY id_origen:
    IF LAST-OF(id_origen) THEN
    DO:
        FIND FIRST origenes OF liq_items_control_tareas NO-LOCK NO-ERROR.
        
        FIND FIRST liq_control_tareas OF liq_items_control_tareas .
        EXPORT DELIMITER ";" 
              liq_control_tareas.id_proveedor FORMAT ">>>>>>>>>" 
              liq_control_tareas.id_origen 
              origenes.nombre.
    END.

END.
OUTPUT CLOSE.
*/


FOR EACH liq_items_control_tareas WHERE fecha >= DATE("01/10/2015") AND id_empresa = 4948 AND legajo = 9 AND id_proveedor > 1 NO-LOCK BREAK BY id_proveedor BY id_origen:
        FIND FIRST liq_control_tareas OF liq_items_control_tareas .
        DISPLAY liq_control_tareas.id_tipo_planilla 
              liq_control_tareas.id_proveedor FORMAT ">>>>>>>>>" 
              liq_control_tareas.id_origen 
              origenes.nombre.
END.
