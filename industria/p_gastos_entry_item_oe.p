
DEFINE INPUT PARAMETER p_id_orden_entrega LIKE orden_entrega.id_orden_entrega.
DEFINE INPUT PARAMETER p_item_oe AS INTEGER.
DEFINE INPUT PARAMETER p_id_gasto LIKE gastos_venta.id_gasto.
DEFINE VAR p_entry AS DECIMAL.
DEFINE VAR v_entry_total AS DECIMAL.
DEFINE VAR v_entry_parcial AS DECIMAL.
DEFINE VAR v_galones AS DECIMAL NO-UNDO.
DEFINE VAR v_kilos as decimal.
DEFINE VAR v_kilos_brutos as decimal.
DEFINE VAR v_cantidad_tambores as decimal.
DEFINE VAR v_gall as decimal.
DEFINE VAR v_gall_brix as decimal.
DEFINE VAR v_total_factura AS DECIMAL.
DEFINE VAR v_total_gastos AS DECIMAL.
DEFINE VAR v_contenedores AS DECIMAL.
DEFINE BUFFER bb_items FOR items_orden_entrega.
DEFINE VAR v_bun_con AS DECIMAL.
DEFINE VAR v_bunker_por_parte AS DECIMAL.

FIND FIRST orden_entrega where orden_entrega.id_orden_entrega = p_id_orden_entrega NO-LOCK NO-ERROR.
FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega   = p_id_orden_entrega
                                 AND items_orden_entrega.ITEM_oe            = p_item_oe
                                NO-LOCK NO-ERROR.


IF AVAILABLE orden_entrega THEN DO:
    FIND destinos WHERE destinos.id_destino = orden_entrega.id_destino NO-LOCK NO-ERROR.
    IF AVAILABLE destinos THEN DO:
        IF destinos.id_destino_grupo = 25 THEN DO:
            /* EL ENTRY(DUTY DDP) SOLO SE CALCULA SI VA A UnitedStatesAmerica(USA) */
            FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = p_id_orden_entrega
                                             AND items_orden_entrega.ITEM_oe          = p_item_oe
                                           NO-LOCK NO-ERROR.
                IF AVAILABLE items_orden_entrega THEN DO:
                        
                    IF items_orden_entrega.id_articulo = 52 OR /* JUGO TURBIO */
                       items_orden_entrega.id_articulo = 53 /* JUGO CLARO */ THEN DO: 
                    
                        
                        FOR EACH tambores_industria where tambores_industria.id_orden_entrega = items_orden_entrega.id_orden_entrega
                                                      AND tambores_industria.ITEM_oe          = items_orden_entrega.ITEM_oe
                                                      NO-LOCK
                                                      BREAK BY tambores_industria.nromov.
                    
                            IF FIRST-OF(tambores_industria.nromov) THEN DO:
                               /* MESSAGE "Lote " tambores_industria.id_lote VIEW-AS ALERT-BOX.  */

                                /* TENGO QUE CALCULAR LOS GALONES Y LOS IMPORTE SEGUN LOS
                                   DATOS DE CADA LOTE. UNA FORMA FACIL DE AGRUPAR LOS TAMBORES
                                   DE UN LOTE ES POR EL NROMOV */      
                                
                                /* ESTA RUTINA ME DEVUELVE VARIOS VALORES, PERO EN ESTE CASO SOLO ME
                                   SIRVE LA CANTIDAD DE GALONES */

                              
                                run p_calcular_galones_lote_entry.p (input tambores_industria.nromov,
                                                                     INPUT items_orden_entrega.id_orden_entrega,
                                                                     INPUT items_orden_entrega.ITEM_oe,
                                                                     output v_kilos,
                                                                     output v_kilos_brutos,
                                                              output v_cantidad_tambores,
                                                              output v_gall,
                                                              output v_gall_brix).
                                v_galones = v_gall.
                                /*
                                MESSAGE "Kilos " v_kilos VIEW-AS ALERT-BOX.
                                MESSAGE "Tambores " v_cantidad_tambores VIEW-AS ALERT-BOX.
                                MESSAGE "GAll " v_gall  VIEW-AS ALERT-BOX.*/
                               /* MESSAGE "Brix " v_gall_brix VIEW-AS ALERT-BOX.*/
                                
                                FIND LAST inspecciones_lote WHERE inspecciones_lote.id_empresa = tambores_industria.id_empresa
                                                              AND inspecciones_lote.id_sucursal = tambores_industria.id_sucursal
                                                              AND inspecciones_lote.id_tipotambor = tambores_industria.id_tipotambor
                                                              AND inspecciones_lote.nromov = tambores_industria.nromov
                                                              NO-LOCK NO-ERROR.
                    
                                IF AVAILABLE inspecciones_lote THEN DO:
                                    /* VOY A BUSCAR LAS INSPECCIONES DE LOS LOTES PARA OBTENER 
                                       LOS GRADOS BRIX DE CADA LOTE */



                                    find tabla_entry where tabla_entry.Brix_desde <= inspecciones_lote.bx_correg
                                                       and tabla_entry.Brix_hasta >= inspecciones_lote.bx_correg
                                                       and tabla_entry.id_articulo = tambores_industria.id_articulo
                                                       no-lock no-error.
                    
                                    IF AVAILABLE tabla_entry THEN DO:
                                        /* CALCULO EL IMPORTE DEL ENTRY PARA CADA LOTE
                                           SEGUN LOS DATOS DEL MISMO */
                                       /* MESSAGE "GAlones"  v_galones VIEW-AS ALERT-BOX.*/
                                        v_entry_parcial = v_galones * 3.785.
                                        /* MESSAGE "Galones x 3.785 " v_entry_parcial VIEW-AS ALERT-BOX. */
                                        v_entry_parcial = v_entry_parcial * tabla_entry.grados.
                                        /* MESSAGE "(Galones x 3.785) x " tabla_entry.grados " = " v_entry_parcial VIEW-AS ALERT-BOX. */
                                        v_entry_parcial = v_entry_parcial * 0.079.
                                        /* MESSAGE "((Galones x 3.785) x " tabla_entry.grados " ) x 0.079 = " v_entry_parcial VIEW-AS ALERT-BOX. */
                                        /* ACUMULO LOS IMPORTE ENTRY DE TODOS LOS LOTES */
                                        v_entry_total = v_entry_total + v_entry_parcial.
                                        v_entry_parcial = 0.
                                    END.
                                    ELSE DO:
                                        MESSAGE "No se encontro valores cargados para los brix " items_orden_entrega.grados_brix
                                                            view-as alert-box.
                                        RETURN.
                                    END.
                                             
                                END. /* DEL AVAILABLE INSPECCIONES_LOTE */
                            END. /* DEL FIRST-OF(tambores_industria.nromov) */
                        END. /* DEL FOR EACH TAMBORES_INDUSTRIA */

                        /* CON EL ENTRY TOTAL DE LOS LOTES CREO EL GASTO RESPECTIVO */
                        CREATE gastos_items_orden_entrega.
                        ASSIGN gastos_items_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
                               gastos_items_orden_entrega.ITEM_oe             = p_item_oe
                               gastos_items_orden_entrega.id_gasto            = p_id_gasto
                               gastos_items_orden_entrega.importe             = v_entry_total.
                                            
                        v_entry_total = 0.
                        /* EN LA RUTINA p_gastos_oe_desde_items_oe.p ACUMULO LOS GASTOS
                           DE TODAS LAS PARTES DE OE Y GRABO ESE TOTAL COMO GASTO DE OE */
                        RUN p_gastos_oe_desde_items_oe.p (INPUT p_id_orden_entrega,
                                                          INPUT p_item_oe,
                                                          INPUT p_id_gasto).
                    END. /* DEL IF items_orden_entrega.id_articulo = 52 OR items_orden_entrega.id_articulo = 53 */
                
                 /*  POR AHORA NO SE TIENE QUE CALCULAR PARA LA CASCARA    
                if orden_entrega.id_articulo = 71 /* SI ES CASCARA */ then
                    do:
                        p_entry = orden_entrega.total_factura * 0.068.
            
                        create gastos_orden_entrega.
                        assign gastos_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
                               gastos_orden_entrega.id_gasto            = p_id_gasto
                               gastos_orden_entrega.importe             = p_entry.
        
                    end.
                */    

                    IF items_orden_entrega.id_articulo = 51 OR /* SI ES ACEITE */
                       items_orden_entrega.id_articulo = 57    /* O OIL PHASE */ THEN DO:
                        
                        p_entry = items_orden_entrega.total_factura * 0.038.
                        
                        create gastos_items_orden_entrega.
                        assign gastos_items_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
                               gastos_items_orden_entrega.ITEM_oe             = p_item_oe
                               gastos_items_orden_entrega.id_gasto            = p_id_gasto
                               gastos_items_orden_entrega.importe             = p_entry.
                        
                        RUN p_gastos_oe_desde_items_oe.p (INPUT p_id_orden_entrega,
                                                          INPUT p_item_oe,
                                                          INPUT p_id_gasto).

                    END. /* del items_orden_entrega.id_articulo = 51 OR
                                items_orden_entrega.id_articulo = 57 */

                    IF items_orden_entrega.id_articulo = 58 /* O WATER PHASE */ THEN DO:
                        
                        v_total_factura = items_orden_entrega.total_factura.

/********************************************************************************************/
/* CALCULO TODOS LOS GASTOS DE UNA ITEM OE EXCEPTO EL DUTTY */
v_total_gastos = 0.
FOR EACH r_clausulas_gastos_item_oe WHERE r_clausulas_gastos_item_oe.id_clausula = items_orden_entrega.id_condicion_venta NO-LOCK.
    /* BUSCO TODOS LOS GASTOS SEGUN LA CONDICION DE VENTA QUE ME DETERMINARA QUE TENGO QUE RESTAR
       AL PRECIO FINAL PARA OBTENER EL FOB */
    FIND FIRST gastos_items_orden_entrega OF items_orden_entrega
                                          WHERE gastos_items_orden_entrega.id_gasto = r_clausulas_gastos_item_oe.id_gasto
                                          NO-LOCK NO-ERROR.
    IF AVAILABLE gastos_items_orden_entrega THEN DO:
        IF gastos_items_orden_entrega.id_gasto <> p_id_gasto THEN DO:
            v_total_gastos = v_total_gastos + gastos_items_orden_entrega.importe.
        END.
        
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
                
                v_total_gastos = v_total_gastos + v_bunker_por_parte.
            END.
        END.

    END.
END.

/********************************************************************************************/
                        
                        p_entry = (v_total_factura - v_total_gastos) * 0.00333881.
                        
                        create gastos_items_orden_entrega.
                        assign gastos_items_orden_entrega.id_orden_entrega    = orden_entrega.id_orden_entrega
                               gastos_items_orden_entrega.ITEM_oe             = p_item_oe
                               gastos_items_orden_entrega.id_gasto            = p_id_gasto
                               gastos_items_orden_entrega.importe             = p_entry.
                        
                        RUN p_gastos_oe_desde_items_oe.p (INPUT p_id_orden_entrega,
                                                          INPUT p_item_oe,
                                                          INPUT p_id_gasto).

                    END. /* del items_orden_entrega.id_articulo = 58 */
            END. /* DEL AVAILABLE ITEMS_ORDEN_ENTREGA */
        END. /* DEL IF destinos.id_destino_grupo = 25 */
    END. /* DEL IF AVAILABLE destinos */
END. /* DEL AVAILABLE ORDEN_ENTREGA */
