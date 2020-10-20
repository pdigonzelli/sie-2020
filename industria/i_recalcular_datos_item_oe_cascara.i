
DEFINE INPUT PARAMETER piOE     AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER piItemOE AS INTEGER NO-UNDO.

DEFINE OUTPUT PARAMETER piKilos           AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER piKilosBrutos     AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER pdPrecioTotal     AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER pdFobDolar        AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER pdCoef            AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER pdDerechosAduana  AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER pdReintegroAduana AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER pdTotalDerechos   AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER pdTotalReintegros AS DECIMAL NO-UNDO.

FIND FIRST items_orden_entrega WHERE items_orden_entrega.id_orden_entrega = piOE
                                 AND items_orden_entrega.ITEM_oe          = piItemOE
                               NO-ERROR.
IF AVAILABLE items_orden_entrega THEN DO:  
  FIND items_contratos WHERE items_contratos.id_contrato      = items_orden_entrega.id_contrato
                         AND items_contratos.id_tipo_contrato = items_orden_entrega.id_tipo_contrato
                         AND items_contratos.anio             = items_orden_entrega.anio
                         AND items_contratos.item             = items_orden_entrega.item
                       NO-LOCK NO-ERROR.
  IF AVAILABLE items_contratos THEN DO:
    FIND contratos OF items_contratos NO-LOCK NO-ERROR.
    IF AVAILABLE contratos THEN DO:
      IF items_orden_entrega.modo_actualizacion THEN DO:
        /* MODO AUTOMATICO, ES DECIR TOMA INFORMACION DE TAMBORES RELACIONADOS */
        FIND FIRST envases_prod WHERE envases_prod.id_envase = 14 NO-LOCK NO-ERROR. /*revisar aqui porque no tiene cargado este campo en items_orden_etnrega*/
        piKilos       = items_orden_entrega.tambores_pedidos * 50.
        piKilosBrutos = piKilos + (items_orden_entrega.tambores_pedidos * envases_prod.tara).
        CASE items_contratos.id_tipo_unidad_venta_origen:
          WHEN 1 then /* TONELADAS */ DO:
            pdPrecioTotal = ((piKilos / 1000) * items_contrato.precio_origen).
          END.
          WHEN 2 then /* KILOS */ DO:
            pdPrecioTotal = (piKilos * items_contrato.precio_origen).
          END.
          WHEN 3 then /* GALONES */ DO: 
            /*
            IF items_contrato.id_articulo = 52 OR
               items_contrato.id_articulo = 53 OR 
               items_contrato.id_articulo = 71 THEN DO:       
                general.items_orden_entrega.grados_brix:screen-value in frame F-Main = string(v_gall_brix).                               
                general.items_orden_entrega.total_galones:screen-value in frame F-Main          = string(gall).
            END.
            v_precio_total = (gall * items_contrato.precio_origen).
            */
          END.
          WHEN 4 THEN /* LIBRAS */ DO:
            pdPrecioTotal = ((piKilos * 2.20462) * items_contrato.precio_origen).
          END.
        END CASE. 
      END.
    END.
  END.

  /*calculo de totales*/
  pdFobDolar        = pdPrecioTotal.
  pdCoef            = 0.95238.
  pdDerechosAduana  = pdPrecioTotal * pdCoef.
  pdReintegroAduana = pdPrecioTotal.
  pdTotalDerechos   = pdDerechosAduana * 0.05.
  pdTotalReintegros = pdPrecioTotal * 0.0205.


  
END.
