DEFINE INPUT PARAMETER v_semana_desde AS INTEGER.
DEFINE INPUT PARAMETER v_anio_desde AS INTEGER.
DEFINE INPUT PARAMETER v_semana_hasta AS INTEGER.
DEFINE INPUT PARAMETER v_anio_hasta AS INTEGER.
DEFINE VAR v_importe_pe AS DECIMAL.
DEFINE VAR v_cobrado AS DECIMAL.
DEFINE VAR v_ingresado AS DECIMAL.
DEFINE VAR v_semana_of AS INTEGER.
DEFINE VAR v_semana_fin AS INTEGER.
define var v_semana_cob as integer.
define var v_semana_ing as integer.
DEFINE VAR v_facturas AS CHAR.

{..\industria\semana.i}


FOR EACH control_permisos_embarque.
    DELETE control_permisos_embarque.
END.
FOR EACH items_control_permisos_embarque.
    DELETE items_control_permisos_embarque.
END.

FOR EACH permisos_embarque BY fecha_oficializacion.
    
    v_semana_of = semana(permisos_embarque.fecha_oficializacion).
    v_semana_fin = semana(permisos_embarque.fecha_oficializacion + 120).
    IF v_anio_desde = v_anio_hasta THEN
    DO:
        IF (v_semana_fin >= v_semana_desde AND YEAR(permisos_embarque.fecha_oficializacion + 120) = v_anio_desde)
            AND 
           (v_semana_fin <= v_semana_hasta AND YEAR(permisos_embarque.fecha_oficializacion + 120) = v_anio_desde) THEN
        DO:
            {..\industria\i_create_control_pe.i}
        END.
    END.
    ELSE
    DO:
        IF v_anio_desde < v_anio_hasta THEN
        DO:
            IF ((v_semana_fin >= v_semana_desde AND YEAR(permisos_embarque.fecha_oficializacion + 120) = v_anio_desde)
                    AND v_semana_fin < 53 ) 
                OR
               ((v_semana_fin <= v_semana_hasta AND YEAR(permisos_embarque.fecha_oficializacion + 120) = v_anio_desde)) THEN
            DO:
                {..\industria\i_create_control_pe.i}
            END.
        END.
        ELSE
            MESSAGE "Por favor ingrese un año 'DESDE' menor al año 'HASTA'." VIEW-AS ALERT-BOX.
    END.
    
END.
