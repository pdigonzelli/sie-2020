DEFINE INPUT PARAMETER piNroMov AS INTEGER.

DEFINE VARIABLE i AS INTEGER    NO-UNDO.
DEFINE VARIABLE j AS INTEGER    NO-UNDO.
DEFINE VARIABLE k AS INTEGER    NO-UNDO.
DEFINE VARIABLE p AS INTEGER    NO-UNDO.
DEFINE BUFFER bu FOR lotes_aceite.
DEFINE BUFFER co FOR composicion_lote_aceite.


FIND LAST co NO-LOCK NO-ERROR.
p = co.id_composicion + 1.

FOR EACH bu WHERE bu.nromov = piNroMov.
  FIND FIRST composicion_lote_aceite WHERE bu.id_empresa    = composicion_lote_aceite.id_empresa
                                       AND bu.id_sucursal   = composicion_lote_aceite.id_sucursal
                                       AND bu.id_tipotambor = composicion_lote_aceite.id_tipotambor
                                       AND bu.nromov        = composicion_lote_aceite.nromov
                                     NO-LOCK NO-ERROR.
  IF AVAILABLE composicion_lote_aceite THEN DO:
    
  END.
  ELSE DO:
    FIND FIRST tambores_industria OF bu NO-LOCK NO-ERROR.
    IF AVAILABLE tambores_industria  THEN DO: /*si no tiene composicion_lote y si tiene tambores ==> se borro el registro de composicion vuelvo a crearlo*/
      i = i + 1.
      FOR EACH tambores_industria OF bu NO-LOCK.
        j = j + 1.
        k = tambores_industria.kilos_tambor.
      END.    
      CREATE composicion_lote_aceite. 
      ASSIGN composicion_lote_aceite.id_empresa        = bu.id_empresa
             composicion_lote_aceite.id_sucursal       = bu.id_sucursal
             composicion_lote_aceite.id_tipotambor     = bu.id_tipotambor
             composicion_lote_aceite.nromov            = bu.nromov
             composicion_lote_aceite.cantidad_tambores = j
             composicion_lote_aceite.numeracion_desde  = 1
             composicion_lote_aceite.numeracion_hasta  = j
             composicion_lote_aceite.kilos_tambor      = k
             composicion_lote_aceite.id_lote           = bu.id_lote
             composicion_lote_aceite.id_composicion    = p
             composicion_lote_aceite.fecha             = TODAY
             composicion_lote_aceite.c_usuario         = "facundo"
             composicion_lote_aceite.c_fecha           = TODAY
             composicion_lote_aceite.c_hora            = STRING(TIME, "HH:MM:SS").
      j = 0.
      p = p + 1.
      
      /*DISP bu.id_lote anio id_articulo bu.id_sucursal.*/
    END.
  END.
  
END.

