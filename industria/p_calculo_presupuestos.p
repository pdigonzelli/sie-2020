DEFINE INPUT PARAMETER p_cliente AS INTEGER.
DEFINE INPUT PARAMETER p_fecha AS DATE.

DEFINE VAR v_importe_fac AS DECIMAL.
DEFINE VAR v_importe_item_fac AS DECIMAL.
DEFINE VAR v_cantidad_contratos AS DECIMAL.
DEFINE VAR v_importe_comision AS DECIMAL.
DEFINE VAR v_importe_contrato AS DECIMAL.
DEFINE VAR v_importe_fob_contrato AS DECIMAL.
DEFINE VAR v_importe_fob_pl AS DECIMAL.
DEFINE VAR v_porc_comision AS DECIMAL.
DEFINE VAR v_cantidad_pl AS DECIMAL.
DEFINE VAR v_factura AS CHAR.
DEFINE VAR v_item_fac AS INTEGER.
DEFINE VAR v_lotes AS CHAR.
DEFINE VAR v_nro_pack_list AS CHAR.
DEFINE VAR v_fecha_fac AS DATE.
DEFINE VAR v_fecha_vto AS DATE.
DEFINE VAR gall         AS DECIMAL.
DEFINE VAR gallx        AS DECIMAL.
define var v_semana_desde as integer.
define var v_anio_desde as integer.
define var v_semana_hasta as integer.
define var v_anio_hasta as integer.
DEFINE VAR v_kilos_envase AS DECIMAL.

DEFINE VAR tiene-oe AS LOGICAL INITIAL FALSE.

FOR EACH reporte_presupuestos.
    DELETE reporte_presupuestos.
END.

run wc_sel_rango_semana.w (output v_semana_desde,
                           output v_anio_desde,
                           output v_semana_hasta,
                           output v_anio_hasta).

if (v_semana_desde > 0 and v_semana_desde < 53) and
   (v_semana_hasta > 0 and v_semana_hasta < 53) then
    do:
        if v_anio_desde = v_anio_hasta then
            do:
                for each items_contratos NO-LOCK 
                                           where items_contratos.semana_entrega >= v_semana_desde
                                             and items_contratos.semana_entrega <= v_semana_hasta
                                             and items_contratos.anio_semana_entrega = v_anio_desde
                                             AND items_contratos.id_tipo_contrato < 100
                                             AND items_contratos.c_fecha >= p_fecha
                                              BY items_contratos.anio_semana_entrega
                                              BY items_contratos.semana_entrega
                                              BY items_contratos.semana_entrega_hasta.
                    {..\industria\i_calculo_presupuestos_1.i}
                                
                end.
            end.
        else
            do:
                for each items_contratos NO-LOCK  
                                           where ((items_contratos.semana_entrega >= v_semana_desde
                                                    and items_contratos.anio_semana_entrega = v_anio_desde) 
                                              or (items_contratos.semana_entrega <= v_semana_hasta
                                                    and items_contratos.anio_semana_entrega = v_anio_hasta))
                                             AND items_contratos.id_tipo_contrato < 100
                                             AND items_contratos.c_fecha >= p_fecha
                                              BY items_contratos.anio_semana_entrega
                                              BY items_contratos.semana_entrega
                                              BY items_contratos.semana_entrega_hasta.
            
                        {..\industria\i_calculo_presupuestos_1.i}
                end.
            end.    
    end.
else
    DO:
        FOR EACH items_contratos NO-LOCK WHERE items_contratos.id_tipo_contrato < 100
                                 /*  AND id_contrato = "TW0150" 
                                   AND ITEM = 4 */
                                   AND items_contratos.c_fecha >= p_fecha
                         BY items_contratos.anio_semana_entrega
                         BY items_contratos.semana_entrega
                         BY items_contratos.semana_entrega_hasta.
    
            {..\industria\i_calculo_presupuestos_1.i}
        END.
    END.
