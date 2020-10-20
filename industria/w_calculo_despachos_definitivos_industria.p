define input parameter fecha_desde as date.
define input parameter fecha_hasta as date.
define input parameter p_lugdes as integer.
DEFINE INPUT PARAMETER p_suc_remito AS INTEGER.


define var v_lote as integer format ">>>9".
define var v_anio_lote as integer format "9999".
define var total_tambores as integer.
define var total_kilos as decimal.
DEFINE VAR v_con LIKE contratos.id_contrato.
DEFINE VAR v_tip LIKE contratos.id_tipo_contrato.
DEFINE VAR v_ani LIKE contratos.anio.

DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
DEFINE VARIABLE dKi4 AS DECIMAL    NO-UNDO.

RUN libReportes.p PERSISTENT SET hLib.


for each despachos_industria.
    delete despachos_industria.
end.

if fecha_hasta = ? then
    fecha_hasta = fecha_desde.



for each remitos no-lock where remitos.fecha >= fecha_desde
                           and remitos.fecha <= fecha_hasta
                           and (if p_lugdes <> 0 then remitos.id_lugdes = p_lugdes else true)
                           and (if p_suc_remito <> 0 then remitos.id_sucursal = p_suc_remito else (remitos.id_sucursal = 95 or remitos.id_sucursal = 96))
                           and remitos.estado = TRUE.
                           
    for each items_factura of remitos WHERE items_factura.id_tipotambor <> 11 AND id_tipotambor <> 0 no-lock.
        v_lote = integer(substring(items_factura.nro_lote,1,4)).
        v_anio_lote = integer(substring(items_factura.nro_lote,6,2)).
        v_anio_lote = v_anio_lote + 2000.
        
        total_tambores = 0.
        total_kilos = 0.
        
        FIND FIRST clientes OF remitos NO-LOCK NO-ERROR .
        FIND FIRST destinos OF remitos NO-LOCK NO-ERROR .
        FIND FIRST proveedores OF remitos NO-LOCK NO-ERROR .
        /* by facundo 05/09/05
        FIND FIRST tambores_industria WHERE tambores_industria.id_sucursal_remito = items_factura.id_sucursal
                                        AND tambores_industria.id_tipo_movsto     = items_factura.id_tipo_movsto
                                        AND tambores_industria.nro_remito         = items_factura.nro
                                        AND tambores_industria.id_lote            = v_lote
                                        NO-LOCK NO-ERROR.
        IF NOT AVAILABLE tambores_industria THEN DO:
            FIND FIRST tambores_industria WHERE tambores_industria.id_tambor = items_factura.desde_lote
                                            AND tambores_industria.id_lote = v_lote
                                            AND tambores_industria.anio = v_anio_lote
                                            AND tambores_industria.id_articulo = items_factura.id_articulo
                                            NO-LOCK NO-ERROR.
        END.
        */

        FIND FIRST r_tambor_remito WHERE r_tambor_remito.id_sucursal_remito = items_factura.id_sucursal
                                     AND r_tambor_remito.id_tipo_movsto     = items_factura.id_tipo_movsto
                                     AND r_tambor_remito.nro_remito         = items_factura.nro
                                     AND r_tambor_remito.ITEM_factura       = items_factura.ITEM
                                   NO-LOCK NO-ERROR.
        IF AVAILABLE r_tambor_remito THEN DO:
          FIND FIRST tambores_industria WHERE tambores_industria.id_tambor  = r_tambor_remito.id_tambor
                                          AND tambores_industria.nromov     = r_tambor_remito.nromov
                                        NO-LOCK NO-ERROR.
            IF AVAILABLE tambores_industria THEN DO:
              v_con = tambores_industria.id_contrato_of.
              v_tip = tambores_industria.id_tipocontrato_of.
              v_ani = tambores_industria.anio_of.
              v_con = TRIM(v_con,"").
        
        
        FIND FIRST contratos where contratos.id_contrato      = v_con
                               and contratos.id_tipo_contrato = v_tip
                               and contratos.anio             = v_ani
                                no-lock no-error.
        /*                     
        FIND FIRST r_productos_calidad where r_productos_calidad.id_articulo = items_factura.id_articulo
                                         and r_productos_calidad.id_calidad = items_factura.id_calidad
                                        no-lock no-error.*/

        find comercial.sucursales of remitos no-lock no-error.
        find envases_prod of items_factura no-lock no-error.
        find productos_terminados of items_factura no-lock no-error.
        find calidades of items_factura no-lock no-error.
        FIND FIRST lugar_descarga OF remitos NO-LOCK NO-ERROR.

        IF AVAILABLE lugar_descarga THEN DO:
            IF lugar_descarga.id_sucursal <> 95 AND
               lugar_descarga.id_sucursal <> 96 THEN DO:
                
                total_kilos = (items_factura.cantidad * items_factura.peso).
                dKi4        = DYNAMIC-FUNCTION('getKilos400' IN  hLib, items_factura.id_tipotambor, items_factura.id_articulo, tambores_industria.id_calidad, TOTAL_kilos).
                create despachos_industria.
                assign despachos_industria.id_sucursal       = remitos.id_sucursal
                       despachos_industria.sucursal          = comercial.sucursales.abreviatura
                       despachos_industria.fecha             = remitos.fecha
                       despachos_industria.nro_comprobante   = remitos.nro_comprobante
                       despachos_industria.id_cliente        = remitos.id_cliente
                       despachos_industria.cliente           = clientes.nombre
                       despachos_industria.id_destino        = remitos.id_destino
                       despachos_industria.destino           = destinos.abreviatura
                       despachos_industria.id_proveedor      = remitos.id_proveedor
                       despachos_industria.proveedor         = proveedores.nombre
                       despachos_industria.chofer            = remitos.chofer
                       despachos_industria.chasis            = remitos.pat_chasis
                       despachos_industria.acoplado          = remitos.pat_acopla
                       despachos_industria.id_envase         = items_factura.id_envase
                       despachos_industria.envase            = envases_prod.abreviatura
                       despachos_industria.id_articulo       = items_factura.id_articulo
                       despachos_industria.articulo          = productos_terminados.abreviatura
                       despachos_industria.id_lote           = v_lote
                       despachos_industria.anio_lote         = integer(substring(string(v_anio_lote),3,2))
                       /*despachos_industria.tambores          = /* IF (total_tambores <> 0) OR (total_tambores >= items_factura.cantidad ) THEN total_tambores ELSE */ items_factura.cantidad
                       despachos_industria.kilos             = IF total_kilos <> 0 THEN total_kilos ELSE (items_factura.cantidad * items_factura.peso)*/
                       despachos_industria.tambores          = items_factura.cantidad
                       despachos_industria.kilos             = total_kilos /*(items_factura.cantidad * items_factura.peso) */
                       despachos_industria.kilos_400         = dKi4
                       despachos_industria.nro_per_embarque  = remitos.nro_per_embarque
                       despachos_industria.id_lugdes         = remitos.id_lugdes
                       despachos_industria.lugdes            = lugar_descarga.descripcion
                       despachos_industria.id_orden_entrega  = remitos.id_orden_entrega.
                       
                       if available calidades then
                            assign despachos_industria.id_calidad = items_factura.id_calidad
                                   despachos_industria.calidad = calidades.abreviatura.
                       
                       if available contratos then
                            assign despachos_general.orden_fabricacion = string(contratos.orden_fabricacion)
                                   despachos_industria.id_contrato = contratos.id_contrato.
         
                       /*if available r_productos_calidad then
                            assign despachos_industria.kilos_400 = total_kilos * r_productos_calidad.coeficiente.*/
            END.
        END.
    END.
        END.
    END.
END.                           
