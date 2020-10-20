define input parameter p_cliente like clientes.id_cliente.
define input parameter p_broker like contratos.id_broker.
DEFINE VAR v_tam_des AS INTEGER.
DEFINE VAR tiene-oe AS LOGICAL INITIAL FALSE.
define var v_semana_desde as integer.
define var v_anio_desde as integer.
define var v_semana_hasta as integer.
define var v_anio_hasta as integer.

run wc_sel_rango_semana.w (output v_semana_desde,
                           output v_anio_desde,
                           output v_semana_hasta,
                           output v_anio_hasta).



FOR EACH contratos WHERE IF p_broker  <> 0  THEN contratos.id_broker = p_broker ELSE TRUE
                     AND IF p_cliente <> 0  THEN contratos.id_cliente = p_cliente ELSE TRUE.

                     /*    {..\industria\i_calculo_oe_packing_contratos_lotes.i}  */
                     
                     find clientes of contratos no-lock no-error.
                     find contactos_industria where contactos_industria.id_contacto =
                                                    contratos.id_broker no-lock no-error.
                     
                     if (v_semana_desde > 0 and v_semana_desde < 53) and
                        (v_semana_hasta > 0 and v_semana_hasta < 53) then
                     DO:
                         RUN genero-por-semana.
                     END.
                     ELSE
                     DO:
                         RUN genero-todos.
                     END.

END.

PROCEDURE genero-por-semana.
        
    for each items_contratos of contratos 
             WHERE estado no-lock.

    /**** verifico por semana y a�o ******/
        IF v_anio_desde = v_anio_hasta THEN
                IF    items_contratos.semana_entrega >= v_semana_desde AND
                      items_contratos.semana_entrega <= v_semana_hasta
                THEN RUN genero-temporal.
        ELSE    
                IF  ( items_contratos.semana_entrega >= v_semana_desde
                  and items_contratos.anio_semana_entrega = v_anio_desde) 
                 OR 
                    (items_contratos.semana_entrega <= v_semana_hasta
                  and items_contratos.anio_semana_entrega = v_anio_hasta)
                THEN RUN genero-temporal.
    end.


END.

PROCEDURE GENERO-TODOS.
    for each items_contratos of contratos WHERE estado.
        RUN genero-temporal.
    END.

END.

PROCEDURE genero-temporal.
    {..\industria\i_cal_oe_pack_cont_lotes_item_con.i}.
END.
