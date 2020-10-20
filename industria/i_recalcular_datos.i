define var r as rowid no-undo.
define var gall as decimal no-undo.
define var v_kilos as decimal no-undo.
define var v_kilos_brutos as decimal no-undo.
define var v_precio_total as decimal no-undo.
define var v_cantidad_tambores as integer no-undo.
define var v_gall_brix like inspecciones_lote.bx_correg no-undo.
define var v_id_orden_entrega like orden_entrega.id_orden_entrega no-undo.
define var h_con as handle.
define var r_orden as rowid.
DEFINE VAR v_comision AS DECIMAL.
DEFINE VAR v_importe_comision AS DECIMAL.
DEFINE VAR v_gastos_cif AS DECIMAL.

run get-container (output h_con).
run dame_datos_orden in h_con (output r_orden).
find orden_entrega where rowid(orden_entrega) = r_orden no-error.
IF AVAILABLE orden_entrega THEN DO:
    find items_contratos where items_contratos.id_contrato      = orden_entrega.id_contrato
                           and items_contratos.id_tipo_contrato = orden_entrega.id_tipo_contrato
                           and items_contratos.anio             = orden_entrega.anio
                           and items_contratos.item             = orden_entrega.item
                           no-lock no-error.
    if available items_contratos then
        do:
           find contratos of items_contratos no-lock no-error.
           if available contratos then 
            do:
               IF orden_entrega.modo_actualizacion THEN DO:
                   
                  v_id_orden_entrega = orden_entrega.id_orden_entrega.
                  run p_calcular-brix.p (input v_id_orden_entrega, 
                                    output v_kilos,
                                    output v_kilos_brutos,
                                    output v_cantidad_tambores,
                                    output gall,
                                    output v_gall_brix).
    
                    if v_cantidad_tambores <> 0 then   
                    general.orden_entrega.cantidad:screen-value in frame F-Main               = string(v_cantidad_tambores).
                    general.orden_entrega.kgs_netos_tambores:screen-value in frame F-Main     = string(v_kilos).
                    general.orden_entrega.kgs_brutos_tambores:screen-value in frame F-Main    = string(v_kilos_brutos).
    
                       case items_contratos.id_tipo_unidad_venta_origen:
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
                                   if items_contrato.id_articulo = 52 or
                                      items_contrato.id_articulo = 53 then
                                       do:       
    
                                        general.orden_entrega.grados_brix:screen-value in frame F-Main = string(v_gall_brix).                               
                                        general.orden_entrega.total_galones:screen-value in frame F-Main          = string(gall).
    
                                       end.
    
                                   v_precio_total = (gall * items_contrato.precio_origen).
                               end.
                           when 4 then /* LIBRAS */
                               do:
                                   v_precio_total = ((v_kilos * 2.20462) * items_contrato.precio_origen).
                               end.
                      end case. 
                     
                      general.orden_entrega.total_factura:screen-value in frame F-Main  = string(v_precio_total).
               END.
               ELSE DO:
                   v_id_orden_entrega = orden_entrega.id_orden_entrega.
                  run p_calcular-brix.p (input v_id_orden_entrega, 
                                    output v_kilos,
                                    output v_kilos_brutos,
                                    output v_cantidad_tambores,
                                    output gall,
                                    output v_gall_brix).
    
                    v_cantidad_tambores = INTEGER(general.orden_entrega.cantidad:screen-value in frame F-Main).
                    v_kilos = decimal(general.orden_entrega.kgs_netos_tambores:screen-value in frame F-Main).
                    v_kilos_brutos = DECIMAL(general.orden_entrega.kgs_brutos_tambores:screen-value in frame F-Main).
    
                       case items_contratos.id_tipo_unidad_venta_origen:
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
                                   v_gall_brix = DECIMAL(general.orden_entrega.grados_brix:screen-value in frame F-Main).                               
                                   
                                   find last brix where brix.brix <= round(v_gall_brix,1) no-lock no-error.
                                   if available brix then 
                                    do:
                                       gall = round((v_kilos / brix.pe) / 3.785 , 2).
                                       general.orden_entrega.total_galones:screen-value in frame F-Main = STRING(gall).
                                       v_precio_total = (gall * items_contrato.precio_origen).
                                    end.
                               end.
                           when 4 then /* LIBRAS */
                               do:
                                   v_precio_total = ((v_kilos * 2.20462) * items_contrato.precio_origen).
                               end.
                      end case. 
    
                      general.orden_entrega.total_factura:screen-value in frame F-Main  = string(v_precio_total).
               END.
            end.
            else message "No se encontro el contratos " view-as alert-box.
            v_comision = items_contratos.comision_broker.  /*************************************************************************/
        end.
        else message "No se encontro el item de contrato " view-as alert-box.

    /* SE CALCULA SIEMPRE LOS GASTOS SIN IMPORTAR EL MODO DE ACTUALIZACION*/
    run calculo-gastos (input orden_entrega.id_orden_entrega, input integer(orden_entrega.id_condicion_venta:screen-value in frame F-Main)).
    
    /* CALCULO LAS COMISIONES **************************************************************************************************/
    CASE orden_entrega.id_condicion_venta:
        WHEN 1 THEN /* FOB */
        DO:
           v_importe_comision = ((v_precio_total * v_comision) / 100).
        END.
        WHEN 2 THEN /* CFR */
        DO:
             FIND gastos_orden_entrega WHERE gastos_orden_entrega.id_orden_entrega = orden_entrega.id_orden_entrega
                                         AND gastos_orden_entrega.id_gasto = 5
                                         NO-LOCK NO-ERROR.
             IF AVAILABLE gastos_orden_entrega THEN DO:
                v_importe_comision = (((v_precio_total + gastos_orden_entrega.importe) * v_comision) / 100).
             END.
        END.
        OTHERWISE /* CIF Y OTROS */
        DO:       /* FLETE = 5       SEGURO = 11 */
            FOR EACH gastos_orden_entrega WHERE gastos_orden_entrega.id_orden_entrega = orden_entrega.id_orden_entrega
                                         AND (gastos_orden_entrega.id_gasto = 5 OR gastos_orden_entrega.id_gasto = 11)
                                         NO-LOCK.
                v_gastos_cif = v_gastos_cif + gastos_orden_entrega.importe.
            END.
            v_importe_comision = (((v_precio_total + v_gastos_cif) * v_comision) / 100).
            v_gastos_cif = 0.
        END.
    END CASE.
    orden_entrega.importe_comisiones:SCREEN-VALUE IN FRAME F-Main = STRING(v_importe_comision).
    /****************************************************************************************************************************/
    message "Se actualizaron los datos satisfactoriamente!" view-as alert-box.

END.

