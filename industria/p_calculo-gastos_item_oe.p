define input parameter v_orden_entrega as integer.
DEFINE INPUT PARAMETER v_item_oe AS INTEGER.
define input parameter v_cond_venta as integer.
define input parameter v_contenedores as decimal.
DEFINE BUFFER b_gastos_items FOR general.gastos_items_orden_entrega.
DEFINE BUFFER b_gastos_oe FOR general.gastos_orden_entrega.
DEFINE BUFFER b_items_oe FOR general.items_orden_entrega.
DEFINE VAR v_todos_contenedores AS DECIMAL.

    /* ACA RECORRO LOS GASTOS DE AGENCIA QUE A POSTERIOR CARGO AUTOMATICAMENTE,
       HAGO ESTO ANTES PORQUE NECESITO MANTENER LOS GASTOS QUE NO SON CARGADOS
       AUTOMATICAMENTE Y A POSTERIOR PIERDO EL CONTROL DE ELLOS*/
/*
/*interrupcion para debugger*/

DEFINE VAR dbg AS LOGICAL.
dbg = DEBUGGER:INITIATE().
dbg = DEBUGGER:SET-BREAK().
*/

 
FIND orden_entrega where orden_entrega.id_orden_entrega = v_orden_entrega no-lock no-error.
IF AVAILABLE orden_entrega THEN DO:
    FOR EACH gastos_agencias where gastos_agencias.id_agencia = orden_entrega.id_agencia.
        /************************************************************/
        /* BORRO SOLO LOS GASTOS QUE ESTOY POR CREAR AUTOMATICAMENTE SEGUN LA AGENCIA */
        FIND FIRST b_gastos_items WHERE b_gastos_items.id_orden_entrega = v_orden_entrega
                                    AND b_gastos_items.ITEM_oe = v_item_oe
                                    AND b_gastos_items.id_gasto = gastos_agencias.id_gasto
                                  NO-ERROR.
        IF AVAILABLE b_gastos_items THEN DO:

            FIND FIRST r_clausulas_gastos_item_oe WHERE r_clausulas_gastos_item_oe.id_clausula = v_cond_venta
                                                    AND r_clausulas_gastos_item_oe.id_gasto    = gastos_agencias.id_gasto
                                                    NO-LOCK NO-ERROR.
            IF NOT AVAILABLE r_clausulas_gastos_item_oe THEN DO:
                /* PRIMERO RECORRO LA TABLA r_clausulas_gastos_item_oe PARA QUE NO ME BORRE
                   LOS GASTOS QUE A POSTERIOR BORRO AUTOMATICAMENTE Y QUE NECESITO PARA
                   EL CALCULO DEL DUTIE DE WATER. ESTO NO TIENE QUE MODIFICAR EL RESTO
                   DEL FUNCIONAMIENTO */
                DELETE b_gastos_items.
            END.                      
        END.
        FIND FIRST b_gastos_oe WHERE b_gastos_oe.id_orden_entrega = v_orden_entrega
                                 AND b_gastos_oe.id_gasto = gastos_agencias.id_gasto
                               NO-ERROR.
        IF AVAILABLE b_gastos_oe THEN DO:
            FIND FIRST r_clausulas_gastos_item_oe WHERE r_clausulas_gastos_item_oe.id_clausula = v_cond_venta
                                                    AND r_clausulas_gastos_item_oe.id_gasto    = gastos_agencias.id_gasto
                                                    NO-LOCK NO-ERROR.
            IF NOT AVAILABLE r_clausulas_gastos_item_oe THEN DO:
                /* PRIMERO RECORRO LA TABLA r_clausulas_gastos_item_oe PARA QUE NO ME BORRE
                   LOS GASTOS QUE A POSTERIOR BORRO AUTOMATICAMENTE Y QUE NECESITO PARA
                   EL CALCULO DEL DUTIE DE WATER. ESTO NO TIENE QUE MODIFICAR EL RESTO
                   DEL FUNCIONAMIENTO */
                DELETE b_gastos_oe.
            END.
        END.
        /**************************************************************/
    END.
END.

/* ACA SE CALCULAR LOS GASTOS DEFINIDOS EN LOS PROGRAMAS PARTICULARES SEGUN EL GASTO */

FOR EACH r_clausulas_gastos_item_oe WHERE r_clausulas_gastos_item_oe.id_clausula = v_cond_venta
                                      /*  
                                      AND r_clausulas_gastos_item_oe.programa <> "" /* AGREGUE ESTA LINEA PARA QUE NO BORRE
                                                                                       EL GASTO VARIOS QUE SE CARGO A MANO */ 
                                      AL FINAL COMENTO ESTA LINEA Y PONGO ABAJO QUE SOLO SE PUEDEN BORRAR
                                      LOS GASTOS = 0. LOS DEMAS SE LOS DEJA COMO ESTA                                                 
                                      */ 
                                    NO-LOCK.
    IF r_clausulas_gastos_item_oe.programa <> "" THEN DO:
        /************************************************************/
        /* BORRO SOLO LOS GASTOS QUE ESTOY POR CREAR AUTOMATICAMENTE 
           DE ESTA FORMA PERMITO QUE SE CARGUEN GASTOS EXTRAS Y NO SE ME BORREN */
        FIND FIRST b_gastos_items WHERE b_gastos_items.id_orden_entrega = v_orden_entrega
                                    AND b_gastos_items.ITEM_oe = v_item_oe
                                    AND b_gastos_items.id_gasto = r_clausulas_gastos_item_oe.id_gasto
                                  NO-ERROR.
        IF AVAILABLE b_gastos_items THEN DO:
            DELETE b_gastos_items.
        END.
        FIND FIRST b_gastos_oe WHERE b_gastos_oe.id_orden_entrega = v_orden_entrega
                                 AND b_gastos_oe.id_gasto = r_clausulas_gastos_item_oe.id_gasto
                               NO-ERROR.
        IF AVAILABLE b_gastos_oe THEN DO:
            DELETE b_gastos_oe.
        END.
        /**************************************************************/
        RUN VALUE(r_clausulas_gastos_item_oe.programa) (INPUT v_orden_entrega, 
                                                        INPUT v_item_oe, 
                                                        INPUT r_clausulas_gastos_item_oe.id_gasto).

    END.        
    ELSE DO:
        /************************************************************/
        /* BORRO SOLO LOS GASTOS QUE ESTOY POR CREAR AUTOMATICAMENTE 
           DE ESTA FORMA PERMITO QUE SE CARGUEN GASTOS EXTRAS Y NO SE ME BORREN */
        FIND FIRST b_gastos_items WHERE b_gastos_items.id_orden_entrega = v_orden_entrega
                                    AND b_gastos_items.ITEM_oe          = v_item_oe
                                    AND b_gastos_items.id_gasto         = r_clausulas_gastos_item_oe.id_gasto
                                    AND b_gastos_items.importe          <> 0
                                  NO-ERROR.
        IF AVAILABLE b_gastos_items THEN DO:
            FIND FIRST b_gastos_oe WHERE b_gastos_oe.id_orden_entrega   = v_orden_entrega
                                     AND b_gastos_oe.id_gasto           = r_clausulas_gastos_item_oe.id_gasto
                                   NO-ERROR.
            IF AVAILABLE b_gastos_oe THEN DO:
                DELETE b_gastos_oe.
            END.

            RUN p_gastos_oe_desde_items_oe.p (INPUT v_orden_entrega,
                                              INPUT v_item_oe,
                                              INPUT r_clausulas_gastos_item_oe.id_gasto).
           
        END.
        ELSE DO:
            IF NOT AVAILABLE b_gastos_items THEN DO:
                CREATE gastos_items_orden_entrega.
                ASSIGN gastos_items_orden_entrega.id_orden_entrega    = v_orden_entrega
                       gastos_items_orden_entrega.item_oe             = v_item_oe
                       gastos_items_orden_entrega.id_gasto            = r_clausulas_gastos_item_oe.id_gasto
                       gastos_items_orden_entrega.importe             = 0.
            END.
        END.
    END.        
END.


/* ESTE CODIGO SERIA EL FOB ENTRE COMILLAS */
DEFINE BUFFER b_gastos FOR gastos_orden_entrega.
/* ACA SE CARGAN LOS DEMAS GASTOS DEFINIDOS SEGUN LOS GASTOS CARGADOS POR CADA AGENCIA MARITIMA */

IF AVAILABLE orden_entrega THEN DO:
    FOR EACH gastos_agencias where gastos_agencias.id_agencia = orden_entrega.id_agencia.
             /* RECORRO TODOS LOS GASTOS DE LAS AGENCIAS */

    FIND FIRST b_gastos WHERE b_gastos.id_gasto         = gastos_agencias.id_gasto
                          AND b_gastos.id_orden_entrega = orden_entrega.id_orden_entrega NO-LOCK NO-ERROR.
                                          
        IF NOT AVAILABLE b_gastos THEN DO:
            /* SI NO LO ENCUENTRO LO BUSCO EN LA TABLA DE EXCEPCIONES r_gastos_agencias_clausulas*/
            /* ESTO ES PARA PODER ELEJIR SI NO QUIERO QUE SE CARGUE ALGUN GASTO SEGUN LA CLAUSULA */
            FIND FIRST r_gastos_agencias_clausulas WHERE r_gastos_agencias_clausulas.id_agencia = orden_entrega.id_agencia
                                                     AND r_gastos_agencias_clausulas.id_gasto = gastos_agencias.id_gasto
                                                     AND r_gastos_agencias_clausulas.id_clausula = v_cond_venta
                                                     NO-LOCK NO-ERROR.
            IF NOT AVAILABLE r_gastos_agencias_clausulas THEN DO:
                CASE gastos_agencias.id_gasto.
                    WHEN 19 THEN DO:
                         /* ESTE ES EL GASTOS "BL" QUE ES EL UNICO QUE NO SE CALCULAR POR CONTENEDOR*/
                        CREATE gastos_orden_entrega.
                        ASSIGN gastos_orden_entrega.id_orden_entrega    = v_orden_entrega
                               gastos_orden_entrega.id_gasto            = gastos_agencias.id_gasto
                               gastos_orden_entrega.importe             = gastos_agencias.importe.
                    END.
                    WHEN 29 THEN DO:
                        /* ESTE ES EL GASTOS "AGP" SE CALCULA POR TONELADA*/
                        FIND FIRST b_items_oe WHERE b_items_oe.id_orden_entrega = v_orden_entrega
                                                AND b_items_oe.ITEM_oe          = v_item_oe
                                              NO-LOCK NO-ERROR.
                        IF AVAILABLE b_items_oe THEN DO:
                            CREATE gastos_orden_entrega.
                            ASSIGN gastos_orden_entrega.id_orden_entrega    = v_orden_entrega
                                   gastos_orden_entrega.id_gasto            = gastos_agencias.id_gasto
                                   gastos_orden_entrega.importe             = gastos_agencias.importe *
                                                                              (b_items_oe.kgs_bruto / 1000).
                        END.
                    END.
                    WHEN 32 THEN DO:
                       /* ESTE ES EL GASTOS "AMC" QUE SE CALCULA UNA SOLA VEZ, NO POR CONTENEDORES */
                        CREATE gastos_orden_entrega.
                        ASSIGN gastos_orden_entrega.id_orden_entrega    = v_orden_entrega
                               gastos_orden_entrega.id_gasto            = gastos_agencias.id_gasto
                               gastos_orden_entrega.importe             = gastos_agencias.importe.
                    END.
                    OTHERWISE DO:
                        v_todos_contenedores = 0.
                        FOR EACH items_orden_entrega OF orden_entrega NO-LOCK.
                            v_todos_contenedores = v_todos_contenedores + items_orden_entrega.contenedores.
                        END.
                        CREATE gastos_orden_entrega.
                        ASSIGN gastos_orden_entrega.id_orden_entrega    = v_orden_entrega
                               gastos_orden_entrega.id_gasto            = gastos_agencias.id_gasto
                               gastos_orden_entrega.importe             = gastos_agencias.importe * v_todos_contenedores.
                    END.
                END CASE.
            END.
        END.
    END.
END.


        /******************************************/
