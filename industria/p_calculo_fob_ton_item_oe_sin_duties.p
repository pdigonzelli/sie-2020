define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
DEFINE INPUT PARAMETER p_item_oe AS INTEGER.
define output parameter p_fob_ton as decimal.
define output parameter p_fob_unitario as decimal.
DEFINE OUTPUT PARAMETER p_comision AS DECIMAL.
DEFINE OUTPUT PARAMETER p_gastos AS DECIMAL.

define var total_kilos as integer.
define var v_fob_unitario as decimal.
DEFINE VAR v_porc_comision AS DECIMAL.
DEFINE VAR v_flete AS DECIMAL.
DEFINE BUFFER bb_items FOR items_orden_entrega.
DEFINE VAR v_contenedores AS DECIMAL.
DEFINE VAR v_bun_con AS DECIMAL.
DEFINE VAR v_bunker_por_parte AS DECIMAL.
DEFINE VAR v_total_factura AS DECIMAL.

find items_orden_entrega where items_orden_entrega.id_orden_entrega = p_id_orden_entrega 
                           AND items_orden_entrega.ITEM_oe          = p_item_oe
                           no-error.

FOR EACH r_clausulas_gastos_item_oe WHERE r_clausulas_gastos_item_oe.id_clausula = items_orden_entrega.id_condicion_venta NO-LOCK.
    /* BUSCO TODOS LOS GASTOS SEGUN LA CONDICION DE VENTA QUE ME DETERMINARA QUE TENGO QUE RESTAR
       AL PRECIO FINAL PARA OBTENER EL FOB */
    FIND FIRST gastos_items_orden_entrega OF items_orden_entrega
                                          WHERE gastos_items_orden_entrega.id_gasto = r_clausulas_gastos_item_oe.id_gasto
                                          NO-LOCK NO-ERROR.
    IF AVAILABLE gastos_items_orden_entrega THEN DO:
        /* ENTRO A BUSCAR PRIMERO LOS GASTOS DE LAS PARTES DE LA OE */
        IF gastos_items_orden_entrega.id_gasto = 5 /* FLETE */ THEN v_flete = gastos_items_orden_entrega.importe.
        /* EXCLUYO LA SUMA DEL GASTO IKS PORQUE SE RESTA DEL PRECIO DE LA FACTURA DIRECTAMENTE */
        IF gastos_items_orden_entrega.id_gasto <> 13 THEN p_gastos = p_gastos + gastos_items_orden_entrega.importe.
    END.
    ELSE DO:
        /* SI NO ENCUENTRO EN LOS GASTOS DE LAS PARTES DE OE, BUSCO EN LOS GASTOS DE LA CABECERA 
           SE PUEDE DAR EL CASO QUE ES UN GASTO GENERAL PARA TODA LA OE ENTONCES VA EN LA
           CABECERA, PERO LO TENGO QUE RESTAR PARA EL FOB */
        v_contenedores = 0.
        /* BUSCO TODOS LOS CONTENEDORES QUE SE USAN EN CADA OE*/
        FOR EACH bb_items WHERE bb_items.id_orden_entrega = p_id_orden_entrega.
            v_contenedores = v_contenedores + bb_items.contenedores.
        END.
        /* BUSCO EL GASTO ENTRE LOS GASTOS DE LA CABECERA */
        FIND FIRST gastos_orden_entrega WHERE gastos_orden_entrega.id_orden_entrega = p_id_orden_entrega
                                          AND gastos_orden_entrega.id_gasto = r_clausulas_gastos_item_oe.id_gasto
                                          NO-LOCK NO-ERROR.
        IF AVAILABLE gastos_orden_entrega THEN DO:
            IF gastos_orden_entrega.id_gasto = 17 /* BUNKER */ THEN DO:
                /* PRIMERO CALCULO CUANTO SERIA EL PRECIO DE BUNKER POR CONTENEDOR */
                v_bun_con = gastos_orden_entrega.importe / v_contenedores.
                /* AHORA CALCULO EL BUNKER QUE LE CORRESPONDE A CADA PARTE PARA RESTAR PARA EL FOB */
                v_bunker_por_parte = v_bun_con * items_orden_entrega.contenedores.
                p_gastos = p_gastos + v_bunker_por_parte.
            END.
        END.

    END.
END.

/* A CONTINUACION CALCULO LOS VALORES DEL FOB */
IF items_orden_entrega.id_tipo_orden_entrega = 2 THEN DO:

END.
ELSE DO:
    /* TODOS ESTOS SON CALCULOS SOLO PARA OEs DE INDUSTRIA */
    RUN i_get_total_factura.i (INPUT ROWID(items_orden_entrega),
                               OUTPUT v_total_factura).
                          
    assign items_orden_entrega.fob_ton = v_total_factura - p_gastos.
    p_fob_ton = v_total_factura - p_gastos.
    
    CASE items_orden_entrega.id_tipo_unidad_venta:
        WHEN 1 THEN /* TONELADAS */ DO:
            v_fob_unitario = p_fob_ton / (items_orden_entrega.kgs_netos_tambores / 1000).
        END.
        WHEN 2 THEN /* KILOS */ DO:
            v_fob_unitario = p_fob_ton / items_orden_entrega.kgs_netos_tambores.
        END.
        WHEN 3 THEN /* GALONES PERO ES IGUAL QUE TONELADAS */ DO:
            v_fob_unitario = p_fob_ton / (items_orden_entrega.kgs_netos_tambores / 1000).
        END.
        WHEN 4 THEN /* LIBRAS PERO ES IGUAL QUE TONELADAS */ DO:
            v_fob_unitario = p_fob_ton / (items_orden_entrega.kgs_netos_tambores / 1000).
        END.
    END CASE.
    
    assign items_orden_entrega.fob_unitario = v_fob_unitario.
    p_fob_unitario = v_fob_unitario.
    
    FIND FIRST items_contratos OF items_orden_entrega NO-LOCK NO-ERROR.
    IF AVAILABLE items_contratos THEN DO:
        IF items_contratos.comision_broker = 0 THEN DO:
            v_porc_comision = 100.
        END.
        ELSE DO:
            v_porc_comision = items_contratos.comision_broker.
        END.
        CASE items_contratos.id_tipo_unidad_venta:
            WHEN 1 THEN /* TONELADAS */ DO:
                p_comision = (items_contratos.precio_comision * (items_orden_entrega.kgs_netos_tambores / 1000)) * (v_porc_comision / 100).
            END.
            WHEN 2 THEN /* KILOS */ DO:
                p_comision = (items_contratos.precio_comision * items_orden_entrega.kgs_netos_tambores) * (v_porc_comision / 100).
            END.
            WHEN 3 THEN /* GALONES */ DO:
                p_comision = (items_contratos.precio_comision * items_orden_entrega.total_galones) * (v_porc_comision / 100).
            END.
            WHEN 4 THEN /* LIBRAS PERO ES IGUAL QUE TONELADAS */ DO:
                p_comision = (items_contratos.precio_comision * (items_orden_entrega.kgs_netos_tambores * 2.20462)) * (v_porc_comision / 100).
            END.
        END CASE.
        ASSIGN items_orden_entrega.importe_comision =  p_comision.
    END. /* if available items_contratos*/

    /***************** END COMISIONES  **********************************************************/
    IF items_orden_entrega.id_tipo_venta = 3 THEN DO:
        CASE items_orden_entrega.id_tipo_unidad_venta:
            WHEN 1 THEN /* TONELADAS */ DO:
                v_fob_unitario = (p_fob_ton - p_comision) / (items_orden_entrega.kgs_netos_tambores / 1000).
            END.
            WHEN 2 THEN /* KILOS */ DO:
                v_fob_unitario = (p_fob_ton - p_comision) / items_orden_entrega.kgs_netos_tambores.
            END.
            WHEN 3 THEN /* GALONES PERO ES IGUAL QUE TONELADAS */ DO:
                v_fob_unitario = (p_fob_ton - p_comision) / (items_orden_entrega.kgs_netos_tambores / 1000).
            END.
            WHEN 4 THEN /* LIBRAS PERO ES IGUAL QUE TONELADAS */ DO:
                v_fob_unitario = (p_fob_ton - p_comision) / (items_orden_entrega.kgs_netos_tambores / 1000).
            END.
        END CASE.
            assign items_orden_entrega.fob_unitario = v_fob_unitario.
            p_fob_unitario = v_fob_unitario.
        
    END.
END.
