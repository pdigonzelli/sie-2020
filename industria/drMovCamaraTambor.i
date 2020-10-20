  FIELD fecha LIKE r_mov_camara_tambor.fecha VALIDATE ~
  FIELD id_empresa LIKE r_mov_camara_tambor.id_empresa VALIDATE ~
  FIELD id_movimiento_camara LIKE r_mov_camara_tambor.id_movimiento_camara VALIDATE ~
  FIELD id_sucursal LIKE r_mov_camara_tambor.id_sucursal VALIDATE ~
  FIELD id_sucursal_movimiento LIKE r_mov_camara_tambor.id_sucursal_movimiento VALIDATE ~
  FIELD id_tambor LIKE r_mov_camara_tambor.id_tambor VALIDATE ~
  FIELD id_tipotambor LIKE r_mov_camara_tambor.id_tipotambor VALIDATE ~
  FIELD id_turno LIKE r_mov_camara_tambor.id_turno VALIDATE ~
  FIELD item LIKE r_mov_camara_tambor.item VALIDATE ~
  FIELD nromov LIKE r_mov_camara_tambor.nromov VALIDATE ~
  FIELD Lote AS INTEGER FORMAT ">>>9" LABEL "Lote"~
  FIELD Anio AS INTEGER FORMAT ">>>9" LABEL "Anio"~
  FIELD Articulo AS CHARACTER FORMAT "x(20)" LABEL "Articulo"~
  FIELD Calidad AS CHARACTER FORMAT "x(20)" LABEL "Calidad"~
  FIELD Envase AS CHARACTER FORMAT "x(20)" LABEL "Envase"~
  FIELD Kilos AS DECIMAL FORMAT ">>>>>9.99" LABEL "Kilos"~
  FIELD FilaOrigen AS CHARACTER FORMAT "x(8)" LABEL "FilaOrigen"~
  FIELD ColumnaOrigen AS CHARACTER FORMAT "x(8)" LABEL "ColumnaOrigen"~
  FIELD FilaDestino AS CHARACTER FORMAT "x(8)" LABEL "FilaDestino"~
  FIELD ColumnaDestino AS CHARACTER FORMAT "x(8)" LABEL "ColumnaDestino"
