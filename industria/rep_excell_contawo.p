DEFINE INPUT PARAMETER p_cliente AS INTEGER.
DEFINE INPUT PARAMETER p_fecha AS DATE.


DEFINE VAR v_importe_fac_fob AS DECIMAL.
DEFINE VAR v_importe_fac_todos AS DECIMAL.
DEFINE VAR v_importe_item_fac_fob AS DECIMAL.
DEFINE VAR v_importe_item_fac_todos AS DECIMAL.
DEFINE VAR v_cantidad_contratos AS DECIMAL.
DEFINE VAR v_importe_comision AS DECIMAL.
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
DEFINE VAR v_kilos_envase AS DECIMAL.
define var v_semana_desde as integer.
define var v_anio_desde as integer.
define var v_semana_hasta as integer.
define var v_anio_hasta as integer.
DEFINE VAR v_nro_contenedor AS CHAR.
DEFINE VAR v_cobranzas AS CHAR.
DEFINE VAR v_fechas_cobranzas AS CHAR.
DEFINE VAR v_total_cobranzas AS DECIMAL.
DEFINE VAR i AS INTEGER.
DEFINE VAR v_consignacion AS LOGICAL FORMAT "SI/NO" INITIAL "NO".
DEFINE VAR v_tiene_lote_anterior AS LOGICAL INITIAL FALSE.
DEFINE VAR v_importe_contrato AS DECIMAL.
DEFINE VAR v_lotes_contrato AS CHAR.
DEFINE VAR v_importe_contrato_fob AS DECIMAL.
DEFINE VAR v_importe_contrato_kilos AS DECIMAL.
DEFINE VAR v_importe_contrato_kilos_400 AS DECIMAL.
DEFINE VAR v_importe_contrato_kilos_fob AS DECIMAL.
DEFINE VAR v_importe_contrato_kilos_fob_400 AS DECIMAL.
DEFINE VAR v_gastos AS DECIMAL.
DEFINE VAR v_coef AS DECIMAL.
DEFINE VAR v_precio_fob AS DECIMAL.
DEFINE VAR v_precio     AS DECIMAL.
DEFINE VAR v_fob_ton AS DECIMAL.
DEFINE VAR v_pesoref AS INTEGER.
DEFINE VAR v_orden_entrega AS CHAR.
DEFINE VAR v_cantidad_pl_400 AS DECIMAL.
DEFINE VAR v_cantidad_contratos_400 AS DECIMAL.
DEFINE VAR v_vapor AS INTEGER.
DEFINE VAR v_destino AS INTEGER.
DEFINE VAR v_anio_envio AS INTEGER.
DEFINE VARIABLE v_cantidad AS DECIMAL    NO-UNDO.
          
DEFINE TEMP-TABLE tt-contrato LIKE re_cial_completo
  FIELD coef AS DECIMAL.
          /*
   FIELD vapor AS CHAR
   FIELD cliente_final AS CHAR
   FIELD moneda_venta AS CHAR
   FIELD condicion_venta AS CHAR
   FIELD articulo AS CHAR
   FIELD unidad_venta  AS CHAR
   FIELD id_contrato LIKE items_contratos.id_contrato
   FIELD id_tipo_contrato LIKE items_contratos.id_tipo_contrato
   FIELD anio LIKE items_contratos.anio_semana_entrega
   FIELD ITEM LIKE items_contratos.ITEM
   FIELD orden_fabricacion LIKE contratos.orden_fabricacion
   FIELD id_vapor LIKE vapores.id_vapor
   FIELD anio_envio AS INTEGER
   FIELD id_articulo LIKE items_contrato.id_articulo
   FIELD id_calidad  LIKE items_contratos.id_calidad
   FIELD calidad LIKE calidades.descripcion
   FIELD semana_desde LIKE items_contratos.semana_entrega
   FIELD semana_hasta           like items_contratos.semana_entrega_hasta
   FIELD anio_item_contrato     like items_contratos.anio_semana_entrega
   FIELD precio_venta           like items_contratos.precio_origen
   FIELD id_condicion_venta     like items_contratos.id_clausula
   FIELD fecha_contrato         like contrato.fecha
   FIELD importe_contrato       AS DECIMAL
   FIELD precio_unitario_x_kilo AS DECIMAL
   FIELD id_cliente             like contratos.id_cliente
   FIELD cliente                like clientes.razon_social
   FIELD id_cliente_final       like contratos.id_cliente_final
   FIELD orden_entrega          AS CHAR
   FIELD cantidad_contratos     AS INTEGER
   FIELD porc_comision          AS DECIMAL
   FIELD importe_comision       AS DECIMAL
   FIELD lote                           AS CHAR
   FIELD importe_contrato_fob           AS DECIMAL
   FIELD importe_contrato_kilos_400     AS DECIMAL
   FIELD importe_contrato_kilos_fob     AS DECIMAL
   FIELD importe_contrato_kilos_fob_400 AS DECIMAL
   FIELD gastos                         AS DECIMAL
   FIELD lotes_anio_anterior AS LOGICAL.
            */
DEFINE VAR tiene-oe AS LOGICAL INITIAL FALSE.



/*----------- Principal -------------*/

FOR EACH re_cial_completo.
    DELETE re_cial_completo.
END.

FOR EACH tt-contrato.
    DELETE tt-contrato.
END.

run wc_sel_rango_semana.w (output v_semana_desde,
                           output v_anio_desde,
                           output v_semana_hasta,
                           output v_anio_hasta).

RUN genero-items-contrato (INPUT p_fecha,
                           INPUT p_cliente,
                           INPUT v_semana_desde,
                           INPUT v_semana_hasta,
                           INPUT v_anio_desde,
                           INPUT v_anio_hasta,
                           INPUT-OUTPUT TABLE tt-contrato).

RUN contratos-con-factura (INPUT-OUTPUT TABLE tt-contrato).
RUN facturas-sin-contrato.

/*----------- Principal -------------*/



/*----------- Procedimientos -------------*/

PROCEDURE incluido:
     {..\industria\i_cal_re_cial_completo2_incwo.i}
END.


PROCEDURE genero-items-contrato .
 DEFINE INPUT  PARAMETER p-fecha   AS DATE    NO-UNDO.
 DEFINE INPUT  PARAMETER p-cliente AS INTEGER    NO-UNDO.
 DEFINE INPUT  PARAMETER p-semana-desde AS INTEGER    NO-UNDO.
 DEFINE INPUT  PARAMETER p-semana-hasta AS INTEGER    NO-UNDO.
 DEFINE INPUT  PARAMETER p-anio-desde AS INTEGER    NO-UNDO.
 DEFINE INPUT  PARAMETER p-anio-hasta AS INTEGER    NO-UNDO.
 DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-contrato .


IF p-semana-desde = 1 AND p-semana-hasta = 52 AND p-anio-desde = p-anio-hasta THEN DO:
     /* COMO NECESITO VER TODA LA INFO DE LOS CONTRATOS DE UN AÑO EN PARTICULAR
        ES NECESARIO HACER EL REPORTE ESPECIAL */
     RUN ..\industria\rep_excell_conta_1_anio.p (INPUT p-cliente,
                                                 INPUT p-fecha,
                                                 INPUT p-semana-desde,
                                                 INPUT p-anio-desde,
                                                 INPUT p-semana-hasta,
                                                 INPUT p-anio-hasta).
 END.

 ELSE DO:
     /* SI NO ES NECESARIO VER LA INFO DE UN SOLO AÑO USO EL PROGRAMA GENERICO */
     if (p-semana-desde > 0 and p-semana-desde < 53) and
        (p-semana-hasta > 0 and p-semana-hasta < 53) then
         do:
             if p-anio-desde = p-anio-hasta then
                 do:
                     for each items_contratos NO-LOCK 
                                                where items_contratos.semana_entrega >= p-semana-desde
                                                  and items_contratos.semana_entrega <= p-semana-hasta
                                                  and items_contratos.anio_semana_entrega = p-anio-desde
                                                  AND items_contratos.id_tipo_contrato < 100
                                                  AND items_contratos.c_fecha >= p-fecha.
                                          /*         BY items_contratos.anio_semana_entrega
                                                   BY items_contratos.semana_entrega
                                                   BY items_contratos.semana_entrega_hasta. */

                         RUN incluido.

                     end.
                 end.
             else
                 do:
                     for each items_contratos NO-LOCK  
                                                where ((items_contratos.semana_entrega >= p-semana-desde
                                                         and items_contratos.anio_semana_entrega = p-anio-desde) 
                                                   or (items_contratos.semana_entrega <= p-semana-hasta
                                                         and items_contratos.anio_semana_entrega = p-anio-hasta))
                                                  AND items_contratos.id_tipo_contrato < 100
                                                  AND items_contratos.c_fecha >= p-fecha.
                                              /*     BY items_contratos.anio_semana_entrega
                                                   BY items_contratos.semana_entrega
                                                   BY items_contratos.semana_entrega_hasta.*/

                             RUN incluido.
                     end.
                 end.    
         end.
     else
         DO:
             FOR EACH items_contratos NO-LOCK WHERE items_contratos.id_tipo_contrato < 100
                                      /*  AND id_contrato = "TW0150" 
                                        AND ITEM = 4 */
                                        AND items_contratos.c_fecha >= p-fecha.
                           /*   BY items_contratos.anio_semana_entrega
                              BY items_contratos.semana_entrega
                              BY items_contratos.semana_entrega_hasta. */

                 RUN incluido.
             END.
         END.
 END.


 /*{..\industria\outputTTContrato.i}*/

END.



PROCEDURE contratos-con-factura.
 DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-contrato.


   FOR EACH tt-contrato ,
                EACH packing_list WHERE packing_list.id_contrato      = tt-contrato.id_contrato
                                    AND packing_list.id_tipo_contrato = tt-contrato.id_tipo_contrato
                                    AND packing_list.anio             = tt-contrato.anio
                                    AND packing_list.item_contrato    = tt-contrato.ITEM NO-LOCK ,
                     EACH items_packing_list OF packing_list NO-LOCK 
                     BREAK BY tt-contrato.id_contrato 
                           BY packing_list.ITEM_contrato:
                           
        FIND vapores OF packing_list NO-LOCK NO-ERROR.
        
        CREATE re_cial_completo.
        BUFFER-COPY tt-contrato TO re_cial_completo.
        ASSIGN 
            re_cial_completo.nro_pack_list    = packing_list.nro_pack_list 
            re_cial_completo.lotes_contrato   = items_packing_list.nro_lote
            re_cial_completo.nro_contenedor   = items_packing_list.nro_contenedor
            re_cial_completo.cantidad_pl      = items_packing_list.kilos
            re_cial_completo.cantidad_pl_400  = re_cial_completo.cantidad_pl * tt-contrato.coef
            re_cial_completo.id_vapor         = vapores.id_vapor
            re_cial_completo.vapor            = vapores.descripcion.



        FIND FIRST r_items_venta_pack_list WHERE 
                      r_items_venta_pack_list.id_sucursal     = items_packing_list.id_sucursal AND
                      r_items_venta_pack_list.id_packing_list = items_packing_list.id_packing_list AND
                      r_items_venta_pack_list.ITEM_pack       = items_packing_list.ITEM NO-LOCK NO-ERROR.
        IF AVAILABLE r_items_venta_pack_list THEN DO:

            FIND FIRST ITEMs_venta OF r_items_venta_pack_list NO-LOCK.
            FIND FIRST subd_vtas OF items_venta NO-LOCK NO-ERROR.
            
            {..\industria\i_cal_re_packing_factura.i}
        END.
        
   END.

END.

PROCEDURE facturas-sin-contrato.
 DEFINE VAR vflag         AS LOGICAL INITIAL FALSE.
 DEFINE VARIABLE vcoef    AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE vpesoref AS DECIMAL NO-UNDO.

     FOR EACH subd_vtas WHERE subd_vtas.id_operacion = 311 AND 
                              subd_vtas.id_tipocomp  = 24 AND
                              impreso NO-LOCK.
         FIND aux_subd_ventas OF subd_vtas NO-LOCK NO-ERROR.

         /*  COMENTADO POR QUE EXISTIAN FACTURAS QUE POSEEIAN AUX_SUBD_VENTAS.NRO_CONTRATO 
             PERO NO ASI LA VINCULACION CON EL PACKING LIST
         
         FIND FIRST contratos WHERE contratos.id_contrato = aux_subd_ventas.nro_contrato NO-LOCK NO-ERROR.
         IF NOT AVAILABLE contratos THEN DO:
          */
                
                RUN p_fob_fac.p (INPUT ROWID(subd_vtas), 
                                 OUTPUT v_importe_fac_fob,
                                 OUTPUT v_importe_fac_todos ).
                  
                RUN getCobranzasIndust.p (INPUT ROWID(subd_vtas), 
                                          OUTPUT v_fechas_cobranzas,
                                          OUTPUT v_cobranzas).
                                                                                
               FOR EACH items_venta OF subd_vtas NO-LOCK:


                    vflag = FALSE.
                   
                    IF items_venta.id_articulo = 52 OR
                       items_venta.id_articulo = 53 THEN DO:
                        FIND r_productos_calidad WHERE r_productos_calidad.id_articulo = items_venta.id_articulo 
                                                   AND r_productos_calidad.id_calidad = items_venta.id_calidad 
                                                 NO-LOCK NO-ERROR.  
                        vcoef = IF AVAILABLE r_productos_calidad THEN r_productos_calidad.coeficiente ELSE 1.
                    
                    END.
                    ELSE
                        vcoef = 1.
                    
                    
                    IF vcoef >= 1.25 THEN
                        vpesoref = 260.
                    ELSE
                        vpesoref = 250.


                    FOR EACH r_items_venta_pack_list 
                        WHERE r_items_venta_pack_list.id_punto_venta = items_venta.id_punto_venta AND
                              r_items_venta_pack_list.nromov         = items_venta.nromov AND
                              r_items_venta_pack_list.ITEM           = items_venta.ITEM NO-LOCK:
    
                         FIND FIRST items_packing_list WHERE
                                  r_items_venta_pack_list.id_sucursal     =  items_packing_list.id_sucursal AND
                                  r_items_venta_pack_list.id_packing_list = items_packing_list.id_packing_list AND
                                  r_items_venta_pack_list.ITEM_pack       = items_packing_list.ITEM NO-LOCK NO-ERROR.
                         
                         FIND packing_list OF items_packing_list NO-LOCK NO-ERROR.
                         IF packing_list.id_contrato <> "" AND packing_list.id_contrato <> ?  THEN DO:
                              vflag = TRUE.
                         END.

                         IF AVAILABLE items_packing_list AND NOT vflag THEN  DO:

                           CREATE re_cial_completo.                       
                           ASSIGN
                              re_cial_completo.id_punto_venta     = subd_vtas.id_punto_venta
                              re_cial_completo.nromov             = subd_vtas.nromov
                              re_cial_completo.factura            = string(subd_vtas.id_punto_venta,"9999") + STRING(subd_vtas.nro_comp,"99999999")
                              re_cial_completo.fecha_factura      = subd_vtas.fecha_comp
                              re_cial_completo.vto_factura        = subd_vtas.vencimiento
                              re_cial_completo.importe_factura    = v_importe_fac_fob
                              re_cial_completo.importe_fac_todos  = v_importe_fac_todos.

                             case items_venta.tipo_unidad:
                                when "T" then /* TONELADAS SE MANEJA IGUAL QUE KILOS */
                                    do:
                                      ASSIGN
                                       re_cial_completo.importe_item_factura = items_venta.precio_origen  / 1000
                                       re_cial_completo.importe_item_fac_todos = (items_venta.precio_origen + items_venta.gastos) / 1000.
                                    end.
                                when "K" then /* KILOS */
                                    do:
                                     ASSIGN
                                       re_cial_completo.importe_item_factura = items_venta.precio_origen 
                                       re_cial_completo.importe_item_fac_todos = (items_venta.precio_origen + items_venta.gastos).
                                    end.
                                when "G" then /* GALONES */                    
                                    do:
                                     ASSIGN
                                       re_cial_completo.importe_item_factura = (items_venta.precio_origen  * 53.6) / vpesoref
                                       re_cial_completo.importe_item_fac_todos = (items_venta.precio_origen + items_venta.gastos) * 53.6  / vpesoref.
                                    end.
                                when "L" then /* LIBRAS */
                                    do:
                                     ASSIGN
                                       re_cial_completo.importe_item_factura = items_venta.precio_origen  * 2.20462
                                       re_cial_completo.importe_item_fac_todos = (items_venta.precio_origen + items_venta.gastos) * 2.20462.
                                    end.
                              END case.
                        
                            ASSIGN
                                re_cial_completo.importe_item_factura   = re_cial_completo.importe_item_factura 
                                * IF ( items_packing_list.id_articulo = 54 OR items_packing_list.id_articulo = 55 ) THEN
                                    items_packing_list.kilos_contenedor ELSE items_packing_list.kilos

                                re_cial_completo.importe_item_fac_todos = re_cial_completo.importe_item_fac_todos
                                        * IF ( items_packing_list.id_articulo = 54 OR items_packing_list.id_articulo = 55 ) THEN
                                            items_packing_list.kilos_contenedor ELSE items_packing_list.kilos.

                         END. /*FIND FIRST items_packing_list*/
                         vflag = TRUE.
                    END. /* end for each r_items_venta_pack_list */
                    
                    IF NOT vflag THEN DO:
                        CREATE re_cial_completo.                       
                        ASSIGN
                          re_cial_completo.id_punto_venta     = subd_vtas.id_punto_venta
                          re_cial_completo.nromov             = subd_vtas.nromov
                          re_cial_completo.factura            = string(subd_vtas.id_punto_venta,"9999") + STRING(subd_vtas.nro_comp,"99999999")
                          re_cial_completo.fecha_factura      = subd_vtas.fecha_comp
                          re_cial_completo.vto_factura        = subd_vtas.vencimiento
                          re_cial_completo.importe_factura    = v_importe_fac_fob
                          re_cial_completo.importe_fac_todos  = v_importe_fac_todos.

                        ASSIGN
                          re_cial_completo.importe_item_factura   = (items_venta.precio_origen * items_venta.cantidad)
                          re_cial_completo.importe_item_fac_todos = ((items_venta.precio_origen + items_venta.gastos) * items_venta.cantidad).

                    END. /* IF NOT vflag */

               END. /* FOR EACH items_venta */

         /*END. IF NOT AVAILABLE contratos*/
         
     END. /*FOR EACH subd_vtas*/

END.

/*----------- Procedimientos -------------*/
