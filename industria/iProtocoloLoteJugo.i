DEFINE VARIABLE v_o_f AS CHARACTER  NO-UNDO.

FIND FIRST protocolos WHERE id_protocolo = 13 AND anio = 4 AND id_contramarca = 204 NO-LOCK .

FIND FIRST lotes_jugo OF protocolos NO-LOCK NO-ERROR.
IF AVAILABLE lotes_jugo THEN DO:
  FOR EACH tambores_industria OF lotes_jugo WHERE tambores_industria.id_tambor = protocolos.desde_tambor
                                            NO-LOCK.
    FIND FIRST contratos WHERE contratos.id_contrato = tambores_industria.id_contrato_of 
                           AND contratos.id_tipo_contrato = tambores_industria.id_tipocontrato_of
                           AND contratos.anio = tambores_industria.anio_of
                         NO-LOCK NO-ERROR.
    IF AVAILABLE contratos THEN DO:
      v_o_f = STRING(contratos.orden_fabricacion) + "/" + SUBSTRING(STRING(YEAR(contratos.fecha)),3,2).
    END.
  END.
END.

RUN p_reportes_9.p ("protocolo_jugo",
                    "Protocolos",
                    "protocolos.id_protocolo = " + STRING(protocolos.id_protocolo) + " and " +
                    "protocolos.anio = " + STRING(protocolos.anio) + " and " +
                    "protocolos.id_articulo = " + STRING(protocolos.id_articulo) ,
                    v_o_f + ";").                    
