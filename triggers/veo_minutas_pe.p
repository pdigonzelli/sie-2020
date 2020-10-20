CURRENT-WINDOW:WIDTH=150.

FOR EACH r_ingresos_pe 
                WHERE r_ingresos_pe.nro_minuta = "09066" AND
                      r_ingresos_pe.id_minuta  = 6 .
    DISP r_ingresos_pe.id_permiso_embarque
         r_ingresos_pe.importe WITH WIDTH 140.
    FIND permisos_embarque 
            WHERE permisos_embarque.id_aduana           = r_ingresos_pe.id_aduana AND
                  permisos_embarque.anio                = r_ingresos_pe.anio      AND
                  permisos_embarque.id_permiso_embarque = r_ingresos_pe.id_permiso_embarque NO-LOCK NO-ERROR.
    IF AVAILABLE permisos_embarque THEN
       DISP fecha_oficializacion
            permisos_embarque.importe WITH WIDTH 140.
END.


