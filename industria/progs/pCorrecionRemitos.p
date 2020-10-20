DEFINE VARIABLE iLot AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAno AS INTEGER    NO-UNDO.
DEFINE VARIABLE cLot AS CHARACTER  NO-UNDO.

FOR EACH items_factura
    WHERE id_sucursal = 159
      AND nro = 673.
  cLot = string(items_factura.nro_lote).
  iLot = integer(ENTRY(1, cLot, "/")).
  iAno = integer(ENTRY(2, cLot, "/")) + 2000.

  DISP items_factura.ITEM items_factura.cantidad. 
  FOR FIRST lotes_cascara
      WHERE lotes_cascara.id_lote = iLot
        AND lotes_cascara.anio    = iAno.
    
    FIND FIRST r_lote_cascara_remito WHERE r_lote_cascara_remito.nromov = lotes_cascara.nromov AND r_lote_cascara_remito.nro_remito = items_factura.nro AND r_lote_cascara_remito.ITEM_factura = items_factura.ITEM NO-ERROR.
    IF NOT AVAILABLE r_lote_cascara_remito THEN DO:
      CREATE r_lote_cascara_remito.
      ASSIGN r_lote_cascara_remito.id_empresa = lotes_cascara.id_empresa
             r_lote_cascara_remito.id_sucursal = lotes_cascara.id_sucursal
             r_lote_cascara_remito.id_tipotambor = lotes_cascara.id_tipotambor
             r_lote_cascara_remito.nromov = lotes_cascara.nromov
             r_lote_cascara_remito.id_sucursal_remito = items_factura.id_sucursal
             r_lote_cascara_remito.id_tipo_movsto = items_factura.id_tipo_movsto 
             r_lote_cascara_remito.nro_remito = items_factura.nro
             r_lote_cascara_remito.ITEM_factura = items_factura.ITEM
             .
    END.
  END.
END.


