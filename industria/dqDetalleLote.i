  FIELD id_lote LIKE lotes_ubicacion.id_lote VALIDATE ~
  FIELD LoteDeposito AS CHARACTER FORMAT "x(20)"~
  FIELD lote LIKE lotes_ubicacion.lote VALIDATE ~
  FIELD id_sucursal LIKE sucursales.id_sucursal VALIDATE ~
  FIELD nombre LIKE sucursales.nombre VALIDATE ~
  FIELD nromov LIKE lotes_ubicacion.nromov VALIDATE ~
  FIELD calidad LIKE lotes_ubicacion.calidad VALIDATE ~
  FIELD envase LIKE lotes_ubicacion.envase VALIDATE ~
  FIELD cantidad LIKE lotes_ubicacion.cantidad VALIDATE ~
  FIELD cantidad_comprometida LIKE lotes_ubicacion.cantidad_comprometida VALIDATE 
