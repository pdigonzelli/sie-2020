DEFINE INPUT PARAMETER pdDesde     AS DATE.
DEFINE INPUT PARAMETER pdHasta     AS DATE.
DEFINE INPUT PARAMETER piLugDes    AS INTEGER.
DEFINE INPUT PARAMETER piSucRemito AS INTEGER.

DEFINE VARIABLE iLote        AS INTEGER FORMAT ">>>9".
DEFINE VARIABLE iAnioLote    AS INTEGER FORMAT "9999".
DEFINE VARIABLE iTotTambores AS INTEGER.
DEFINE VARIABLE dTotKilos    AS DECIMAL.

DEFINE VAR cCon LIKE contratos.id_contrato.
DEFINE VAR iTip LIKE contratos.id_tipo_contrato.
DEFINE VAR iAni LIKE contratos.anio.

FOR EACH despachos_industria.
    DELETE despachos_industria.
END.

IF pdDesde = ? THEN
    pdHasta = pdDesde.


FOR EACH remitos WHERE remitos.fecha       >= pdDesde
                   AND remitos.fecha       <= pdHasta
                   AND YEAR(remitos.fecha) >= 2004
                   AND remitos.estado       = TRUE
                   AND (IF piLugDes <> 0 THEN remitos.id_lugdes = piLugDes ELSE TRUE)
                   AND (IF piSucRemito <> 0 THEN remitos.id_sucursal = piSucRemito ELSE (remitos.id_sucursal = 95 OR remitos.id_sucursal = 96))
                 NO-LOCK.
  FIND FIRST clientes OF remitos NO-LOCK NO-ERROR .
  FIND FIRST destinos OF remitos NO-LOCK NO-ERROR .
  FIND FIRST proveedores OF remitos NO-LOCK NO-ERROR .
  FIND comercial.sucursales OF remitos NO-LOCK NO-ERROR.
  FIND FIRST lugar_descarga OF remitos NO-LOCK NO-ERROR.
                                                                                        
  FOR EACH items_factura OF remitos WHERE items_factura.id_tipotambor = 11 
                                      AND items_factura.id_articulo   = 54 
                                    NO-LOCK.
    iTotTambores = items_factura.cantidad.
    dTotKilos    = items_factura.cantidad * 50.
    FIND envases_prod OF items_factura NO-LOCK NO-ERROR.
    FIND productos_terminados OF items_factura NO-LOCK NO-ERROR.
    FIND calidades OF items_factura NO-LOCK NO-ERROR.
    FIND FIRST r_lote_cascara_remito WHERE items_factura.id_sucursal    = r_lote_cascara_remito.id_sucursal_remito
                                       AND items_factura.id_tipo_movsto = r_lote_cascara_remito.id_tipo_movsto
                                       AND items_factura.nro            = r_lote_cascara_remito.nro_remito
                                       AND items_factura.ITEM           = r_lote_cascara_remito.ITEM_factura
                                     NO-LOCK NO-ERROR.
    IF AVAILABLE r_lote_cascara_remito THEN DO:    
      FIND lotes_cascara WHERE r_lote_cascara_remito.nromov = lotes_cascara.nromov.
      ASSIGN iLote     = lotes_cascara.id_lote
             iAnioLote = lotes_cascara.anio.
      FIND FIRST r_lote_cascara_contrato WHERE r_lote_cascara_remito.nromov = r_lote_cascara_contrato.nromov NO-LOCK.
      FIND FIRST contratos WHERE r_lote_cascara_contrato.id_contrato = contratos.id_contrato NO-LOCK NO-ERROR.

      CREATE despachos_industria.
      ASSIGN despachos_industria.id_sucursal       = remitos.id_sucursal
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
             despachos_industria.id_lote           = iLote
             despachos_industria.anio_lote         = iAnioLote - 2000
             despachos_industria.tambores          = items_factura.cantidad
             despachos_industria.kilos             = items_factura.cantidad * 50 
             despachos_industria.nro_per_embarque  = remitos.nro_per_embarque
             despachos_industria.id_lugdes         = remitos.id_lugdes
             despachos_industria.lugdes            = lugar_descarga.descripcion
             despachos_industria.id_orden_entrega  = remitos.id_orden_entrega.
             
             IF AVAILABLE calidades THEN DO:
               ASSIGN despachos_industria.id_calidad = items_factura.id_calidad
                      despachos_industria.calidad    = calidades.abreviatura.
             END.
             
             IF AVAILABLE contratos THEN DO:
               ASSIGN despachos_general.orden_fabricacion = STRING(contratos.orden_fabricacion)
                      despachos_industria.id_contrato       = contratos.id_contrato.
             END.
  
    END.

  END.
END.


