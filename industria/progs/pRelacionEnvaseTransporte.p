DEFINE TEMP-TABLE ttEnvases
  FIELD id_envase AS INTEGER
  FIELD id_articulo AS INTEGER.

FOR EACH ttEnvases.
  DELETE ttEnvases.
END.

FOR EACH r_transporte_envase.
    DELETE r_transporte_envase.
END.

/* envases de lotes de jugos */
FOR EACH tambores_industria 
    WHERE fecha >= DATE('01/01/2006')
      AND id_tipotambor = 3
      AND id_orden_entrega <> 0
      AND id_articulo <> 0
    BREAK BY (string(id_articulo) + string(id_envase)).
  IF LAST-OF(string(id_articulo) + string(id_envase)) THEN DO:
    CREATE ttEnvases.
    ASSIGN ttEnvases.id_envase = tambores_industria.id_envase
           ttEnvases.id_articulo = tambores_industria.id_articulo.
  END.
END.

/* envases de lotes de aceites */
FOR EACH tambores_industria 
    WHERE fecha >= DATE('01/01/2006')
      AND id_tipotambor = 6
      AND id_orden_entrega <> 0
      AND id_articulo <> 0
    BREAK BY (string(id_articulo) + string(id_envase)).
  IF LAST-OF(string(id_articulo) + string(id_envase)) THEN DO:
    CREATE ttEnvases.
    ASSIGN ttEnvases.id_envase = tambores_industria.id_envase
           ttEnvases.id_articulo = tambores_industria.id_articulo.
  END.
END.



FOR EACH tipos_transporte 
    WHERE (id_tipo_transporte = 1 OR id_tipo_transporte = 2 OR id_tipo_transporte = 5) NO-LOCK.
  FOR EACH ttEnvases NO-LOCK.
    CREATE r_transporte_envase.
    ASSIGN r_transporte_envase.id_tipo_transporte = tipos_transporte.id_tipo_transporte
           r_transporte_envase.id_evnase = ttEnvases.id_envase
           r_transporte_envase.id_articulo = ttEnvases.id_articulo
           r_transporte_envase.id_proveedor = 0 
           r_transporte_envase.capacidad = 0
           .
  END.


END.
 
