DEFINE TEMP-TABLE ttLotesAbiertos
  FIELD id_sucursal AS INTEGER
  FIELD id_lote AS INTEGER
  FIELD anio    AS INTEGER
  FIELD id_articulo AS INTEGER
  FIELD articulo AS CHARACTER
  FIELD calidad AS CHARACTER
  FIELD envase AS CHARACTER
  FIELD fecha_comienzo AS CHARACTER
  FIELD nromov AS INTEGER
  .

  
FOR EACH lotes_jugo WHERE fecha >= DATE("01/01/2005") 
                      AND fecha_finalizacion = ?.
  FIND FIRST productos_terminados OF lotes_jugo NO-LOCK NO-ERROR.
  FIND FIRST calidades OF lotes_jugo NO-LOCK NO-ERROR.
  FIND FIRST envases_prod OF lotes_jugo NO-LOCK NO-ERROR.
  CREATE ttLotesAbiertos.
  ASSIGN ttLotesAbiertos.id_sucursal = lotes_jugo.id_sucursal
    ttLotesAbiertos.id_lote = lotes_jugo.id_lote
    ttLotesAbiertos.anio = lotes_jugo.anio
    ttLotesAbiertos.fecha_comienzo = string(lotes_jugo.fecha_comienzo)
    ttLotesAbiertos.nromov = lotes_jugo.nromov
    ttLotesAbiertos.id_articulo = lotes_jugo.id_articulo
    ttLotesAbiertos.articulo = productos_terminados.descripcion
    ttLotesAbiertos.calidad = calidades.descripcion
    ttLotesAbiertos.envase = envases_prod.descripcion
    .
  
END.


RUN generateExcel.p (INPUT TABLE ttLotesAbiertos,
                        INPUT " Lotes Abiertos",
                        INPUT " Fecha Desde: 01/01/2005"  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Century Gothic",
                        INPUT 7).
