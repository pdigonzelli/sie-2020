define var r as rowid no-undo.
DEFINE VAR r_semana AS ROWID.
define var gall as decimal no-undo.
define var v_gall_tambor as decimal no-undo.
define var v_kilos as decimal no-undo.
define var v_kilos_brutos as decimal no-undo.
define var v_peso as decimal no-undo.
define var v_precio_total as decimal no-undo.
define var v_cantidad_tambores as integer no-undo.
define var v_gall_brix like inspecciones_lote.bx_correg no-undo.
define var v_id_orden_entrega like orden_entrega.id_orden_entrega no-undo.
DEFINE VAR v_item_oe AS INTEGER.
DEFINE VAR h_con AS HANDLE.
DEFINE VAR v_r_oe AS ROWID.
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


RUN get-container (OUTPUT h_con).
RUN get-oe IN h_con (OUTPUT v_id_orden_entrega).
v_item_oe = INTEGER(items_orden_entrega.ITEM_oe:SCREEN-VALUE IN FRAME F-Main).

RUN get-rowid-oe IN h_con (OUTPUT v_r_oe).

FIND FIRST orden_entrega WHERE ROWID(orden_entrega) = v_r_oe NO-LOCK NO-ERROR.
IF AVAILABLE orden_entrega THEN DO:
    CASE orden_entrega.id_tipo_orden_entrega:
        WHEN 1 THEN DO: /* INDUSTRIA */
            enable  general.items_orden_entrega.anio
                    general.items_orden_entrega.item
                    general.items_orden_entrega.id_tipo_contrato
                    general.items_orden_entrega.semana_entrega with frame F-Main.
            run wc_items_contratos.w(output r).
            find items_contratos where rowid(items_contratos) = r no-lock no-error.
            if available items_contratos then
                do:
                    find contratos of items_contratos no-lock no-error.
                    if available contratos then 
                      do:
                        general.items_orden_entrega.id_contrato:screen-value in frame F-Main      = string(contratos.id_contrato).
                        general.items_orden_entrega.anio:screen-value in frame F-Main             = string(contratos.anio).
                        v_anio                                                = contratos.anio.
                        general.items_orden_entrega.item:screen-value in frame F-Main             = string(items_contratos.item).
                        v_item                                                = items_contratos.item.
                        general.items_orden_entrega.id_tipo_contrato:screen-value in frame F-Main = string(contratos.id_tipo_contrato).
                        v_id_tipo_contrato                                    = contratos.id_tipo_contrato.
                        general.items_orden_entrega.semana_entrega:screen-value in frame F-Main   = string(items_contratos.semana_entrega).
                        v_semana_entrega                                      = items_contratos.semana_entrega.
                        general.items_orden_entrega.id_condicion_venta:screen-value in frame F-Main    = string(items_contratos.id_clausula).
                        v_id_clausula = items_contratos.id_clausula.
                        general.items_orden_entrega.id_cliente:screen-value in frame F-Main       = string(contratos.id_cliente).
                        general.items_orden_entrega.id_calidad:screen-value in frame F-Main       = string(items_contratos.id_calidad).
                        general.items_orden_entrega.id_articulo:screen-value in frame F-Main      = string(items_contratos.id_articulo).
                        general.items_orden_entrega.cantidad_tambores:screen-value in frame F-Main         = string(items_contratos.cantidad).
                        general.items_orden_entrega.tambores_pedidos:screen-value in frame F-Main         = string(items_contratos.cantidad).
                        general.items_orden_entrega.plazo:screen-value in frame F-Main         = string(contratos.plazo).
                        general.items_orden_entrega.id_tipo_plazo:screen-value in frame F-Main         = string(contratos.id_tipo_plazo).
                        general.items_orden_entrega.id_instrumento_pago:screen-value in frame F-Main         = string(contratos.id_instrumento_pago).
                        general.items_orden_entrega.cert_fito:SCREEN-VALUE IN FRAME F-Main            = STRING(items_contratos.cert_fito).
            
                        run p_calcular-brix_item_oe.p (input v_id_orden_entrega, 
                                                       INPUT v_item_oe,
                                                       output v_kilos,
                                                       output v_kilos_brutos,
                                                       output v_cantidad_tambores,
                                                       output gall,
                                                       output v_gall_brix).
                                                     
                        general.items_orden_entrega.grados_brix:screen-value in frame F-Main = string(v_gall_brix).
            
                        general.items_orden_entrega.kgs_netos_tambores:screen-value in frame F-Main    = string(v_kilos).
                        general.items_orden_entrega.kgs_brutos_tambores:screen-value in frame F-Main    = string(v_kilos_brutos).
                        general.items_orden_entrega.total_galones:screen-value in frame F-Main         = string(gall).
                        
                        case items_contratos.id_tipo_unidad_venta:
                            when 1 then /* TONELADAS */
                                do:
                                    v_precio_total = ((v_kilos / 1000) * items_contrato.precio_origen).
                                end.
                            when 2 then /* KILOS */
                                do:
                                    v_precio_total = (v_kilos * items_contrato.precio_origen).                    
                                end.
                            when 3 then /* GALONES */
                                do:
                                    v_precio_total = (gall * items_contrato.precio_origen).
                                end.
                            when 4 then /* LIBRAS */
                                do:
                                    v_precio_total = ((v_kilos * 2.20462) * items_contrato.precio_origen).
                                end.
                       end case. 
                               
                       general.items_orden_entrega.total_factura:screen-value in frame F-Main  = string(v_precio_total).
                       
                       v_moneda         = items_contratos.id_moneda_origen.
                       v_tipo_unidad    = items_contratos.id_tipo_unidad_venta_origen.
                       v_precio         = items_contratos.precio_origen.
                       v_tipo_venta     = items_contratos.id_tipo_venta.
                        
                      end.
                    
                end.
                disable general.items_orden_entrega.anio
                        general.items_orden_entrega.item
                        general.items_orden_entrega.id_tipo_contrato
                        general.items_orden_entrega.semana_entrega with frame F-Main.
        END.
        WHEN 2 THEN DO: /* FRUTA FRESCA */
            run wc_programa_despacho.w(OUTPUT v_id_programa_despacho,
                                       OUTPUT v_id_cliente, OUTPUT v_cliente ,
                                       OUTPUT v_id_articulo, OUTPUT v_articulo ,
                                       OUTPUT v_id_marca , OUTPUT v_marca ,
                                       OUTPUT v_id_calidad , OUTPUT v_calidad ,
                                       OUTPUT v_id_envase , OUTPUT v_envase ,
                                       OUTPUT v_pallets ,
                                       OUTPUT v_cajas_por_pallets ,
                                       OUTPUT v_kilos_por_cajas ,
                                       OUTPUT v_importe_final ,
                                       OUTPUT v_id_moneda ,
                                       OUTPUT v_id_tipo_unidad_venta).

            general.items_orden_entrega.id_condicion_venta:screen-value in frame F-Main    = "1".
            general.items_orden_entrega.id_cliente:screen-value in frame F-Main            = STRING(v_id_cliente).
            general.items_orden_entrega.id_calidad:screen-value in frame F-Main            = STRING(v_id_calidad).
            general.items_orden_entrega.id_articulo:screen-value in frame F-Main           = STRING(v_id_articulo).
            general.items_orden_entrega.id_marca:screen-value in frame F-Main              = STRING(v_id_marca).
            general.items_orden_entrega.cantidad_pallets:screen-value in frame F-Main      = STRING(v_pallets).
            general.items_orden_entrega.cajas_x_pallets:screen-value in frame F-Main       = STRING(v_cajas_por_pallets).
            v_kilos = v_pallets * v_cajas_por_pallets * v_kilos_por_cajas.
            general.items_orden_entrega.kgs_netos_tambores:screen-value in frame F-Main     = string(v_kilos).
            general.items_orden_entrega.kgs_brutos_tambores:screen-value in frame F-Main    = string(v_kilos).
            general.items_orden_entrega.cert_fito:SCREEN-VALUE IN FRAME F-Main              = "SI".
            general.items_orden_entrega.id_programa_despacho:screen-value in frame F-Main   = string(v_id_programa_despacho).
            general.items_orden_entrega.id_envase:screen-value in frame F-Main              = string(v_id_envase).
            v_precio_total = v_cantidad_pallets *  v_cajas_por_pallets * v_importe_final. 
            general.items_orden_entrega.total_factura:screen-value in frame F-Main  = string(v_precio_total). 
        END.
    END CASE.
END.


   
