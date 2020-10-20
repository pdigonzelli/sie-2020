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

    {..\industria\i_create_control_pe.i}
END.
