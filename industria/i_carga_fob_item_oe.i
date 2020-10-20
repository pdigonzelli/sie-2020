define input parameter v_orden_entrega as integer no-undo.
DEFINE INPUT PARAMETER v_item_oe AS INTEGER NO-UNDO.

define var v_fob_ton as decimal no-undo.
define var v_fob_unitario as decimal no-undo.
DEFINE VAR v_comision AS DECIMAL.
DEFINE VAR v_coef AS DECIMAL.
DEFINE VAR v_derecho_aduana AS DECIMAL.
DEFINE VAR v_reintegro_aduana AS DECIMAL.
DEFINE VAR v_derecho AS DECIMAL.
DEFINE VAR v_reintegro AS DECIMAL.
DEFINE VAR v_cal AS DECIMAL.
DEFINE VAR v_tipo_cambio AS DECIMAL.
DEFINE VAR v_cantidad_pallets AS DECIMAL.
DEFINE VAR v_id_programa_despacho AS INTEGER.
DEFINE VAR v_id_cliente AS INTEGER.
DEFINE VAR v_cliente AS CHAR.
DEFINE VAR v_id_articulo AS INTEGER.
DEFINE VAR v_articulo AS CHAR.
DEFINE VAR v_id_marca AS INTEGER.
DEFINE VAR v_marca AS CHAR.
DEFINE VAR v_id_calidad AS INTEGER.
DEFINE VAR v_calidad AS CHAR.
DEFINE VAR v_id_envase AS INTEGER.
DEFINE VAR v_envase AS CHAR.
DEFINE VAR v_pallets AS DECIMAL.
DEFINE VAR v_cajas_por_pallets AS DECIMAL.
DEFINE VAR v_kilos_por_cajas AS DECIMAL.
DEFINE VAR v_importe_final AS DECIMAL.
DEFINE VAR v_id_moneda AS INTEGER.
DEFINE VAR v_id_tipo_unidad_venta AS INTEGER.
DEFINE VAR v_id_tipo_pallet AS INTEGER.
DEFINE VAR v_total_cajas AS INTEGER.
DEFINE VAR v_kilos AS DECIMAL.
DEFINE VAR v_kilos_brutos AS DECIMAL.
DEFINE VAR v_precio_total AS DECIMAL.
DEFINE VAR v_gastos AS DECIMAL.
DEFINE VAR v_tara AS DECIMAL.



/* MESSAGE "Estoy en i_carga_fob_item_oe.i Tipo OE" orden_entrega.id_tipo_orden_entrega VIEW-AS ALERT-BOX. */
IF orden_entrega.id_tipo_orden_entrega = 1 THEN DO:
    /* HACE ESTE CALCULO PARA LOS DE INDUSTRIA */
    RUN p_calculo_fob_ton_item_oe.p (INPUT v_orden_entrega, 
                                     INPUT v_item_oe, 
                                     OUTPUT v_fob_ton, 
                                     OUTPUT v_fob_unitario,
                                     OUTPUT v_comision,
                                     OUTPUT v_gastos).

    IF items_orden_entrega.id_tipo_venta = 3 /* CONSIGNACION */ THEN DO:
        items_orden_entrega.total_factura:SCREEN-VALUE IN FRAME F-Main = STRING(ROUND(v_fob_ton - v_comision,0)).
        items_orden_entrega.fob_ton:SCREEN-VALUE IN FRAME F-Main = STRING(ROUND(v_fob_ton - v_comision,0)).
        items_orden_entrega.fob_unitario:SCREEN-VALUE IN FRAME F-Main = STRING(ROUND(v_fob_unitario,0)).
        ASSIGN items_orden_entrega.TOTAL_factura = ROUND(v_fob_ton - v_comision,0)
               items_orden_entrega.fob_ton       = ROUND(v_fob_ton - v_comision,0).
    END.
    ELSE DO:  
        items_orden_entrega.fob_ton:screen-value in frame F-Main = string(v_fob_ton).
        items_orden_entrega.fob_unitario:screen-value in frame F-Main = string(v_fob_unitario).
    END.

    {i_calculo_derechos_reintegros.i}

END.
ELSE DO: /*
    /* HAGO LOS CACULOS PARA OEs DE FRUTA FRESCA */
    v_pallets               = INTEGER(general.items_orden_entrega.cantidad_pallets:screen-value in frame F-Main).
    v_id_envase             = INTEGER(general.items_orden_entrega.id_envase:screen-value in frame F-Main).
    v_id_tipo_pallet        = INTEGER(general.items_orden_entrega.id_tipo_pallet:screen-value in frame F-Main).
    v_id_articulo           = INTEGER(general.items_orden_entrega.id_articulo:screen-value in frame F-Main).
    v_importe_final         = INTEGER(general.items_orden_entrega.precio_x_caja:screen-value in frame F-Main).
    v_comision              = 0.

    FIND FIRST r_pallets_envase WHERE r_pallets_envase.id_envase         = v_id_envase
                                  AND r_pallets_envase.id_tipo_pallet    = v_id_tipo_pallet
                                NO-LOCK NO-ERROR.
    IF AVAILABLE r_pallets_envase THEN DO: /* SI ENCUENTRO LA INFO, NO TENDRIA QUE CARGAR A MANO */
        v_cajas_por_pallets     = r_pallets_envases.pallets. /* ESTE CAMPO ME DICE LA CANTIDAD DE CAJAS EN UN
                                                                PALLET ESPECIFICO */
        v_total_cajas           = v_pallets * v_cajas_por_pallets.
        items_orden_entrega.cajas_x_pallets:SCREEN-VALUE IN FRAME F-Main = STRING(v_cajas_por_pallets).
        items_orden_entrega.total_cajas:SCREEN-VALUE IN FRAME F-Main = STRING(v_total_cajas).
        ASSIGN items_orden_entrega.cajas_x_pallets      = v_cajas_por_pallets
               items_orden_entrega.TOTAL_cajas          = v_total_cajas.
    END.
    ELSE DO:
        /* NO ENCUENTRO LA INFO, POR ENDE DEJO LA CARGADA A MANO */
        MESSAGE "No se encontro la cantidad de cajas por pallets " VIEW-AS ALERT-BOX.
        v_cajas_por_pallets     = INTEGER(general.items_orden_entrega.cajas_x_pallets:screen-value in frame F-Main).
        v_total_cajas           = v_cajas_por_pallets * v_pallets.                           
        items_orden_entrega.total_cajas:SCREEN-VALUE IN FRAME F-Main = STRING(v_total_cajas).
    END.
    
    
    RUN p_calculo_fob_ton_item_oe.p (INPUT v_orden_entrega, 
                                     INPUT v_item_oe, 
                                     OUTPUT v_fob_ton, 
                                     OUTPUT v_fob_unitario,
                                     OUTPUT v_comision,
                                     OUTPUT v_gastos).
      
    FIND FIRST r_envases_prod WHERE r_envases_prod.id_envase = v_id_envase NO-LOCK NO-ERROR.
    IF AVAILABLE r_envases_prod THEN DO:
        v_kilos = v_total_cajas * r_envases_prod.Kilos_nominal.
        /* MESSAGE "Kilos " v_kilos 
                " cajas " v_total_cajas
                " kilos del envase " r_envases_prod.Kilos_nominal VIEW-AS ALERT-BOX. */
        FIND FIRST envases_prod WHERE envases_prod.id_envase = v_id_envase NO-LOCK NO-ERROR.
        general.items_orden_entrega.kgs_netos_tambores:screen-value in frame F-Main     = string(v_kilos).
        v_tara  = (IF AVAILABLE envases_prod THEN envases_prod.Tara ELSE 0) * v_total_cajas.
        v_kilos_brutos = v_kilos + v_tara.
        general.items_orden_entrega.kgs_brutos_tambores:screen-value in frame F-Main    = string(v_kilos_brutos).
        v_precio_total = v_total_cajas * IF v_importe_final > 0 THEN v_importe_final ELSE 5. 
        general.items_orden_entrega.total_factura:screen-value in frame F-Main  = string(v_precio_total).
        v_fob_ton = v_precio_total - v_gastos.
        items_orden_entrega.fob_ton:screen-value in frame F-Main = string(v_fob_ton).
        items_orden_entrega.fob_unitario:screen-value in frame F-Main = string(v_fob_ton / v_total_cajas).
        ASSIGN items_orden_entrega.kgs_netos_tambores   = v_kilos
               items_orden_entrega.kgs_brutos_tambores  = v_kilos_brutos
               items_orden_entrega.TOTAL_factura        = v_precio_total
               items_orden_entrega.fob_ton              = v_fob_ton
               items_orden_entrega.fob_unitario         = v_fob_ton / (IF v_total_cajas > 0 THEN v_total_cajas ELSE 1)
               .
    END.
    ELSE MESSAGE "No se encontro el envase " v_id_envase VIEW-AS ALERT-BOX.

    {i_calculo_derechos_reintegros.i}
    */
END.
